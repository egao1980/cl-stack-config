(defsystem "cl-stack-config"
  :version "0.3.0"
  :description "env + .env + TOML/INI config facade for cl-stack (toml-protocol), typed settings via schema-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("toml-protocol" "toml-backend-tomlet" "uiop"
               (:version "schema-protocol" "0.2.2"))

  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "dotenv")
               (:file "config")
               (:file "settings"))
  :in-order-to ((test-op (test-op "cl-stack-config/tests"))))

(defsystem "cl-stack-config/tests"
  :depends-on ("cl-stack-config" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "config-test")
               (:file "dotenv-test")
               (:file "settings-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
