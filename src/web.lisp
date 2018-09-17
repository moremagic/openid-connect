(in-package :cl-user)
(defpackage openid-connect.web
  (:use :cl
        :caveman2
        :openid-connect.config
        :openid-connect.view
        :openid-connect.db
        :datafly
        :sxql
        :quri)
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
  "openid-connect")
(defparameter +keycloak-client-secret+
  "9bc0468f-ce9b-4c90-ac93-4837b57494ab")
(defparameter +keycloak-auth-url+
  "http://localhost:18080/auth/realms/master/protocol/openid-connect/auth")
(defparameter +keycloak-token-url+
  "http://localhost:18080/auth/realms/master/protocol/openid-connect/token")
(defparameter +keycloak-token-info-url+
  "http://localhost:18080/auth/realms/master/protocol/openid-connect/userinfo")
(defparameter +keycloak-logout-url+
  "http://localhost:18080/auth/realms/master/protocol/openid-connect/logout")
(defparameter +keycloak-redirect-uri+
  "http://localhost:5000/oauth2callback")

;;
;; Utility functions

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
  (format t "[DEBUG] call request-token ~A~%" code)
  (dex:post +keycloak-token-url+
    :content `(("code" . ,code)
               ("client_id" . ,+keycloak-client-id+)
               ("client_secret" . ,+keycloak-client-secret+)
               ("redirect_uri" . ,+keycloak-redirect-uri+)
               ("grant_type" . "authorization_code"))))

(defun request-keycloak-token-info (access_token)
  "トークン情報を要請"
  (format t "[DEBUG]===========================================~%")
  (format t "[DEBUG][request-keycloak-token-info] '~A'~%" access_token)
  (dex:post +keycloak-token-info-url+
    :headers `(("content-type" . "application/json")
               ("Accept" . "application/json")
               ("Authorization" . ,(concatenate `string "Bearer " access_token)))
    :content `()))

(defun loginp ()
  "ログインしているかどうかを確認"
  (format t "[DEBUG] session in accsess_token '~A'~%" (gethash :access_token *session* nil))
  (not (null (gethash :access_token *session* nil))))

(defun logout (refresh_token)
  "ログアウト処理"
  (format t "[DEBUG]logout api call...")
  (dex:post +keycloak-logout-url+
    :headers `(("Content-Type" . "application/x-www-form-urlencoded"))
    :content `(("client_id" . ,+keycloak-client-id+)
               ("client_secret" . ,+keycloak-client-secret+)
               ("refresh_token" . ,refresh_token )
               )))

(defroute "/" ()
  (if (loginp)
    (redirect "/home")
    (render #P"index.html")))

(defroute "/home" ()
  (if (loginp)
    (render #P"home.html")
    (redirect "/")))

(defroute ("/auth-keycloak" :method :POST) ()
  (let ((state-token
          (cl-base64:usb8-array-to-base64-string
            (secure-random:bytes 32 secure-random:*generator*))))
    (setf (gethash :oauth-keycloak *session*) (acons :state state-token (list)))
    (redirect (get-keycloak-auth-url state-token))))

(defroute ("/oauth2callback" :method :GET) (&key |error| |state| |code|)
  ;; エラーが発生した場合はエラーを表示してそのままルートにリダイレクト
  (unless (null |error|)
    (format t "Error: ~A~%" |error|)
    (redirect "/"))
  (let ((session-oauth-keycloak (gethash :oauth-keycloak *session* nil)))
    ;; セッションにステートトークンが存在するか確認
    (if (not (null (assoc :state session-oauth-keycloak)))
      ;; セッションのステートトークンがレスポンスのステートトークンと一致するか確認
      (if (string= (cdr (assoc :state session-oauth-keycloak)) |state|)
        ;; レスポンスに認証コードが存在するか確認
        (if (not (null |code|))
          ;; keycloakの認証サーバーにトークンを要請
          (let ((response (jsown:parse (request-keycloak-token |code|))))
            ;; ログイン成功。access_tokenを取り出してセッションの:access_tokenに格納
            (format t "[DEBUG] response '~A'~%" response)
            (setf (gethash :access_token *session*) (jsown:val response "access_token"))
            (setf (gethash :refresh_token *session*) (jsown:val response "refresh_token"))
            (format t "[DEBUG] refresh_token '~A'~%" (gethash :refresh_token *session*))

            ;; トークンが有効か確認(ライフタイムが残っているか?)
            ;; さらにIDトークンの存在を確認
            (if (and (> (jsown:val response "expires_in") 0)
                     (not (null (jsown:val response "id_token"))))
              ;; IDトークンをkeycloakに投げてユーザー情報を取得
              ;; この処理は不要かも
              (let ((api-result
                      (jsown:parse (request-keycloak-token-info (gethash :access_token *session*)))))

                ;; ユーザ名を取り出してセッションの:preferred_usernameに格納
                ;; 本来ならここでJWTをばらして、ロール等の情報をセッションに登録をする
                (let ((preferred_username (jsown:val api-result "preferred_username")))
                  (format t "Signin: success keycloak OAuth '~A'~%" preferred_username)
                  (setf (gethash :preferred_username *session*) preferred_username)
                  (redirect "/home")))))))))
    ;; 認証に失敗した場合はHTTP 401認証エラーコードを投げる
    (throw-code 401))

(defroute "/logout" ()
  (if (loginp)
    (progn
      (format t "[DEBUG] refresh_token '~A'~%" (gethash :refresh_token *session*))
      (logout (gethash :refresh_token *session*))
      (setf (gethash :access_token *session*) nil)
      (setf (gethash :refresh_token *session*) nil)
      (redirect "/"))
    (render #P"index.html")))

