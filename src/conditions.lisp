(in-package #:cl-stack-config)

(define-condition config-error (error)
  ((message :initarg :message :reader config-error-message :initform nil)
   (path :initarg :path :reader config-error-path :initform nil))
  (:report (lambda (c s)
             (format s "config error~@[ at ~a~]: ~a"
                     (config-error-path c)
                     (or (config-error-message c) c)))))

(define-condition config-parse-error (config-error) ())
(define-condition config-missing-error (config-error) ())
(define-condition config-type-error (config-error) ())
(define-condition config-file-error (config-error) ())
