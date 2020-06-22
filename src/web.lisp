(in-package :cl-user)
(defpackage openid-connect.web
  (:use :cl
        :caveman2
        :openid-connect.config
        :openid-connect.view
        :openid-connect.db
        :datafly
        :sxql
        :quri
        :jose)
  (:export :*web*))
(in-package :openid-connect.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Utility functions

(defun now ()
  "NTP時刻とPOSIX時刻のオフセット(2208988800秒) を補正して現在時刻を返す"
  (- (get-universal-time) 2208988800))

(defun logging (level &rest messages)
  (format t "[~a] ~{~a~^ : ~} ~%" level messages))

(defun config-value (key)
  (cadr (assoc key (config :keycloak))))

;;
;; auth parameter

(defparameter +keycloak-client-id+ (config-value ':client-id))
(defparameter +keycloak-client-secret+ (config-value ':client-secret))
(defparameter +keycloak-auth-url+ (config-value ':auth-url))
(defparameter +keycloak-token-url+ (config-value ':token-url))
(defparameter +keycloak-token-info-url+ (config-value ':token-info-url))
(defparameter +keycloak-logout-url+ (config-value ':logout-url))
(defparameter +keycloak-redirect-uri+ (config-value ':redirect-uri))

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))


(defun get-keycloak-auth-url (state-token)
  "keycloakアカウントでの認証URLを生成"
  (render-uri
    (make-uri :defaults +keycloak-auth-url+
              :query `(("client_id" . ,+keycloak-client-id+)
                       ("redirect_uri" . ,+keycloak-redirect-uri+)
                       ("scope" . "openid profile email")
                       ("response_type" . "code")
                       ("approval_prompt" . "force")
                       ("access_type" . "offline")
                       ("state" . ,state-token)))))

(defun request-keycloak-token-using-refresh-token (refresh_token)
  "トークンを要請"
  (logging "DEBUG" "call request-keycloak-token-using-refresh-token" refresh_token)
  (dex:post +keycloak-token-url+
    :content `(("refresh_token" . ,refresh_token)
               ("client_id" . ,+keycloak-client-id+)
               ("client_secret" . ,+keycloak-client-secret+)
               ("grant_type" . "refresh_token"))))

(defun request-keycloak-token (code)
  "トークンを要請"
  (logging "DEBUG" "call request-token" code)
  (dex:post +keycloak-token-url+
    :content `(("code" . ,code)
               ("client_id" . ,+keycloak-client-id+)
               ("client_secret" . ,+keycloak-client-secret+)
               ("redirect_uri" . ,+keycloak-redirect-uri+)
               ("grant_type" . "authorization_code"))))

(defun request-keycloak-token-info (access_token)
  "トークン情報を要請"
  (logging "DEBUG" "request-keycloak-token-info" "(access_token)" access_token)
  (dex:post +keycloak-token-info-url+
    :headers `(("content-type" . "application/json")
               ("Accept" . "application/json")
               ("Authorization" . ,(concatenate 'string "Bearer " access_token)))
    :content `()))

(defun loginp ()
  "ログインしているかどうかを確認"
  (let
    ((access_token (gethash :access_token *session* nil))
     (id_token (gethash :id_token *session* nil))
     (refresh_token (gethash :refresh_token *session* nil)))
      (logging "DEBUG" "[loginp] access_token in session" access_token)
      (logging "DEBUG" "[loginp] id_token in session" id_token)
      (logging "DEBUG" "[loginp] refresh_token in session" refresh_token)
      (if (and
          (not (null id_token))
          (not (null access_token))
          (not (null refresh_token)))
          (if (token-expirep access_token)
              (refresh-token-flow refresh_token)
              t)
        nil)))

(defun token-expirep (access_token)
  "有効期限が切れている場合 t"
  (logging "DEBUG" "token expirep  call...")
  (if (not (null access_token))
    (progn
      (setf token-jwt (jose:inspect-token access_token))
      (setf exp-jwt (cdr (assoc "exp" token-jwt :test #'string=)))
       (logging "DEBUG" "access_token" token-jwt)
       (logging "DEBUG" "now time" (now))
       (logging "DEBUG" "exp time" exp-jwt)
       (and
         (not (null access_token))
         (< exp-jwt (now))))
     nil))

(defun refresh-token-flow (refresh_token)
  "リフレッシュトークンフローによるトークンの更新"
  (logging "DEBUG" "call refresh-token-flow" refresh_token)
  (if (not (null refresh_token))
    (progn
      (setf token-jwt (jose:inspect-token refresh_token))
      (setf exp-jwt (cdr (assoc "exp" token-jwt :test #'string=)))
       (logging "DEBUG" "refresh_token" token-jwt)
       (logging "DEBUG" "now time" (now))
       (logging "DEBUG" "exp time" exp-jwt)
       (if
         (and
           (not (null refresh_token))
           (> exp-jwt (now)))

	  ;; 認可コードを使用して keycloakの認証サーバーにトークンを要請
          (let ((response (jsown:parse (request-keycloak-token-using-refresh-token refresh_token))))
            ;; ログイン成功。access_tokenを取り出してセッションの:access_tokenに格納
            (setf (gethash :access_token *session*) (jsown:val response "access_token"))
            (setf (gethash :refresh_token *session*) (jsown:val response "refresh_token"))
            (setf (gethash :id_token *session*) (jsown:val response "id_token")))
     nil))))


(defun logout (refresh_token)
  "ログアウト処理"
  (logging "DEBUG" "logout api call...")
  (dex:post +keycloak-logout-url+
    :headers `(("Content-Type" . "application/x-www-form-urlencoded"))
    :content `(("client_id" . ,+keycloak-client-id+)
               ("client_secret" . ,+keycloak-client-secret+)
               ("refresh_token" . ,refresh_token ))))

(defroute "/" ()
  (if (loginp)
    (redirect "/home")
    (render #P"index.html")))

(defroute "/home" ()
  (if (loginp)
    (render #P"home.html")
    (redirect "/")))

(defroute ("/auth-keycloak" :method :POST) ()
  ;; RFC6746 Autorization Code Grant
  (let ((state-token
          (cl-base64:usb8-array-to-base64-string
            (secure-random:bytes 32 secure-random:*generator*))))
    (setf (gethash :oauth-keycloak *session*) (acons :state state-token (list)))
    (redirect (get-keycloak-auth-url state-token))))

(defroute ("/oauth/callback" :method :GET) (&key |error| |state| |code|)
  ;; エラーが発生した場合はエラーを表示してそのままルートにリダイレクト
  (unless (null |error|)
    (logging "ERROR" "Error: ~A~%" |error|)
    (redirect "/"))
  (let ((session-oauth-keycloak (gethash :oauth-keycloak *session* nil)))
    ;; セッションにステートトークンが存在するか確認
    (if (not (null (assoc :state session-oauth-keycloak)))
      ;; セッションのステートトークンがレスポンスのステートトークンと一致するか確認
      (if (string= (cdr (assoc :state session-oauth-keycloak)) |state|)
        ;; レスポンスに認可コードが存在するか確認
        (if (not (null |code|))
          ;; 認可コードを使用して keycloakの認証サーバーにトークンを要請
          (let ((response (jsown:parse (request-keycloak-token |code|))))

            ;; ログイン成功。access_tokenを取り出してセッションの:access_tokenに格納
            (setf (gethash :access_token *session*) (jsown:val response "access_token"))
            (setf (gethash :refresh_token *session*) (jsown:val response "refresh_token"))
            (setf (gethash :id_token *session*) (jsown:val response "id_token"))

            ;; トークンが有効か確認(ライフタイムが残っているか?)
            ;; さらにIDトークンの存在を確認
            (if (and (> (jsown:val response "expires_in") 0)
                     (not (null (gethash :id_token *session* nil))))
              ;; IDトークンをkeycloakに投げてユーザー情報を取得
              ;; この処理は不要かも
              (let ((api-result
                      (jsown:parse (request-keycloak-token-info (gethash :access_token *session*)))))

                (logging "DEBUG" "token-info response;json" (jsown:to-json api-result))

                ;; ユーザ名を取り出してセッションの:preferred_usernameに格納
                ;; 本来ならここでJWTをばらして、ロール等の情報をセッションに登録をする
                (let ((preferred_username (jsown:val api-result "preferred_username")))
                  (logging "DEBUG" "Signin: success keycloak OAuth" preferred_username)
                  (setf (gethash :preferred_username *session*) preferred_username)
                  (redirect "/home")))))))))
    ;; 認証に失敗した場合はHTTP 401認証エラーコードを投げる
    (throw-code 401))

(defroute "/logout" ()
  (if (loginp)
    (progn
      (logging "DEBUG" "refresh_token" (gethash :refresh_token *session*))
      (logout (gethash :refresh_token *session*))
      (setf (gethash :access_token *session*) nil)
      (setf (gethash :refresh_token *session*) nil)
      (setf (gethash :id_token *session*) nil)
      (redirect "/"))
    (render #P"index.html")))


(defroute "/public" ()
          "public message!")
(defroute "/secured" ()
          "secured message!")
(defroute "/admin" ()
          "admin message!")
