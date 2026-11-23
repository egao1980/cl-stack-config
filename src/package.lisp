(defpackage #:cl-stack-config
  (:nicknames #:stack-config)
  (:use #:cl)
  (:shadow #:load #:get)
  (:export #:config-error
           #:config-parse-error
           #:config-missing-error
           #:config-type-error
           #:config-file-error
           #:config-validation-error
           #:dotenv-syntax-warning
           #:dotenv-syntax-warning-path
           #:dotenv-syntax-warning-line
           #:config-error-message
           #:config-error-path
           #:*config*
           #:config
           #:config-p
           #:config-data
           #:config-prefix
           #:config-format
           #:parse-ini
           #:parse-dotenv
           #:read-dotenv
           #:load-dotenv
           #:config-dotenv
           #:load-into
           #:load
           #:load-config
           #:reload
           #:with-config
           #:get
           #:get-string
           #:get-integer
           #:get-boolean
           #:get-list
           #:section
           #:config-set))

(in-package #:cl-stack-config)
