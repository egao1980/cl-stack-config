(defsystem "cl-stack-config"
  :version "0.2.0"
  :description "env + TOML/INI config facade for cl-stack (toml-protocol)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("toml-protocol" "toml-backend-tomlet" "uiop")

  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "config"))
  :in-order-to ((test-op (test-op "cl-stack-config/tests"))))

(defsystem "cl-stack-config/tests"
  :depends-on ("cl-stack-config" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "config-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
