(in-package :cl-user)
(defpackage openid-connect.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :*application-root*
           :*static-directory*
           :*template-directory*
           :appenv
           :developmentp
           :productionp))
(in-package :openid-connect.config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :openid-connect))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))

(defconfig :common
  `(:databases ((:maindb :sqlite3 :database-name ":memory:"))
    :keycloak
    ((:client-id "lisp-app")
     (:client-secret "Enter the client secret created with Keycloak here")
     (:auth-url "http://localhost:18080/auth/realms/example/protocol/openid-connect/auth")
     (:token-url "http://localhost:18080/auth/realms/example/protocol/openid-connect/token")
     (:token-info-url "http://localhost:18080/auth/realms/example/protocol/openid-connect/userinfo")
     (:logout-url "http://localhost:18080/auth/realms/example/protocol/openid-connect/logout")
     (:redirect-uri "http://localhost:5000/oauth/callback"))))

(defconfig |development|
  '())

(defconfig |production|
  '())

(defconfig |test|
  '())

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))
