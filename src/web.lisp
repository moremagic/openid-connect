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
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))

;; auth parameter
(defparameter +keycloak-client-id+
  "lisp-app")
(defparameter +keycloak-client-secret+
  "")
(defparameter +keycloak-auth-url+
  "http://localhost:8080/auth/realms/example/protocol/openid-connect/auth")
(defparameter +keycloak-token-url+
  "http://localhost:8080/auth/realms/example/protocol/openid-connect/token")
(defparameter +keycloak-token-info-url+
  "http://localhost:8080/auth/realms/example/protocol/openid-connect/userinfo")
(defparameter +keycloak-logout-url+
  "http://localhost:8080/auth/realms/example/protocol/openid-connect/logout")
(defparameter +keycloak-redirect-uri+
  "http://localhost:5000/oauth/callback")

;;
;; Utility functions

(defun now ()
  "NTP時刻とPOSIX時刻のオフセット(2208988800秒) を補正して現在時刻を返す"
  (- (get-universal-time) 2208988800))

(defun logging (level &rest messages)
  (format t "[~a] ~{~a~^ : ~} ~%" level messages))

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
     (id_token (gethash :id_token *session* nil)))
      (logging "DEBUG" "access_token in session" access_token)
      (logging "DEBUG" "id_token in session" id_token)
      (if (not (null id_token))
          (progn
            (setf token-jwt (jose:inspect-token id_token))
            (setf exp-jwt (cdr (assoc "exp" token-jwt :test #'string=)))
            (logging "DEBUG" "id_token" token-jwt)
            (logging "DEBUG" "now time" (now))
            (logging "DEBUG" "exp time" exp-jwt)
            (and
              (not (null id_token))
              (not (null access_token))
              (> exp-jwt (now))))
          nil)))

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
