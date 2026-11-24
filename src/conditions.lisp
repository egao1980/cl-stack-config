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

(define-condition config-validation-error (config-type-error
                                           schema-protocol:schema-validation-error)
  ()
  (:report (lambda (c s)
             (format s "config error~@[ at ~a~]: ~a~%~{  ~a~%~}"
                     (config-error-path c)
                     (config-error-message c)
                     (mapcar (lambda (issue)
                               (format nil "~:[$~;~:*~{~a~^.~}~]: ~a"
                                       (schema-protocol:schema-issue-path issue)
                                       (schema-protocol:schema-issue-message issue)))
                             (schema-protocol:schema-validation-error-issues c)))))
  (:documentation "LOAD-INTO found values that do not fit the settings
schema. Both a CONFIG-TYPE-ERROR and a SCHEMA-VALIDATION-ERROR: every
field issue is in SCHEMA-VALIDATION-ERROR-ISSUES."))
(define-condition config-file-error (config-error) ())
