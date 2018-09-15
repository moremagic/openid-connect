(defsystem "openid-connect-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("openid-connect"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "openid-connect"))))
  :description "Test system for openid-connect"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
