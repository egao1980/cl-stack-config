(in-package #:cl-stack-config/tests)

;;; Issue #8 part 1: .env files (python-dotenv).
;;; Proposed API, named after PARSE-INI and python-dotenv's load_dotenv:
;;;   (cfg:parse-dotenv text &key environ)  => ((NAME . VALUE) ...) in file order
;;;   (cfg:load-dotenv path &key override)  ; fill the process environment
;;;   (cfg:load-config file ... :dotenv path) ; layer: file < .env < env < overrides
;;; Calls go through %API / APPLY so this file compiles before the API exists.

(defun %api (name)
  "The exported cl-stack-config function NAME, or NIL."
  (multiple-value-bind (sym status) (find-symbol name :cl-stack-config)
    (when (and sym (eq status :external) (fboundp sym))
      (symbol-function sym))))

(defmacro %with-api ((var name) &body body)
  `(let ((,var (%api ,name)))
     (if ,var
         (progn ,@body)
         (fail (format nil "stack-config:~(~a~) is not defined" ,name)))))

(defun %write-tmp (text type)
  (uiop:with-temporary-file (:pathname path :prefix "cl-stack-config-" :type type
                             :keep t)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string text out))
    path))

(defun %load-config* (path &rest keys)
  "LOAD-CONFIG with KEYS unknown at compile time (:dotenv is not a keyword yet)."
  (apply #'cfg:load-config path keys))

(defparameter *dotenv-text*
  "# full-line comment
export PYCL_HOST=db.internal
PYCL_PORT=5432
PYCL_URL=postgres://${PYCL_HOST}:${PYCL_PORT}/app
PYCL_GREETING=\"hello\\nworld\"
PYCL_RAW='no ${PYCL_HOST} \\n here'
PYCL_HASH=\"a#b\"
PYCL_INLINE=value # trailing comment
PYCL_EMPTY=
PYCL_FROM_ENV=${PYCL_OUTER}
")

(deftest dotenv-parse-python-dotenv-syntax
  "Comments, export, quotes, \\n in double quotes only, ${VAR} expansion
   (earlier keys and the environment; not inside single quotes)."
  (%with-api (parse "PARSE-DOTENV")
    (ok (equal `(("PYCL_HOST" . "db.internal")
                 ("PYCL_PORT" . "5432")
                 ("PYCL_URL" . "postgres://db.internal:5432/app")
                 ("PYCL_GREETING" . ,(format nil "hello~%world"))
                 ("PYCL_RAW" . "no ${PYCL_HOST} \\n here")
                 ("PYCL_HASH" . "a#b")
                 ("PYCL_INLINE" . "value")
                 ("PYCL_EMPTY" . "")
                 ("PYCL_FROM_ENV" . "outer"))
               (funcall parse *dotenv-text* :environ '(("PYCL_OUTER" . "outer")))))))

(deftest dotenv-load-fills-environment-without-override
  (%with-api (load-dotenv "LOAD-DOTENV")
    (let ((path (%write-tmp (format nil "PYCL_NEW_8D=from-file~%PYCL_KEEP_8D=from-file~%")
                            "env")))
      (setf (uiop:getenv "PYCL_KEEP_8D") "outer")
      (unwind-protect
           (progn
             (funcall load-dotenv path)
             (ok (equal "from-file" (uiop:getenv "PYCL_NEW_8D")))
             (ok (equal "outer" (uiop:getenv "PYCL_KEEP_8D"))
                 "existing variables win by default")
             (funcall load-dotenv path :override t)
             (ok (equal "from-file" (uiop:getenv "PYCL_KEEP_8D"))
                 ":override t replaces them"))
        (setf (uiop:getenv "PYCL_NEW_8D") ""
              (uiop:getenv "PYCL_KEEP_8D") "")
        (uiop:delete-file-if-exists path)))))

(deftest dotenv-layer-between-file-and-env
  "file < .env < real env < overrides; the layer does not touch the process env."
  (let ((toml (%write-tmp (format nil "debug = false~%[database]~%host = \"file-host\"~%port = 5432~%")
                          "toml"))
        (dotenv (%write-tmp (format nil "APP_DATABASE__HOST=dotenv-host~%APP_DATABASE__PORT=1111~%APP_DEBUG=true~%")
                            "env")))
    (flet ((load* (&rest keys)
             (handler-case (apply #'%load-config* toml :prefix "APP" :dotenv dotenv keys)
               (error (e)
                 (fail (format nil "load-config :dotenv: ~a" e))
                 nil))))
      (let ((c (load* :environ '(("APP_DATABASE__PORT" . "6543")))))
        (when c
          (ok (equal "dotenv-host" (cfg:get c "database.host")) ".env beats the file")
          (ok (= 6543 (cfg:get-integer c "database.port")) "env beats .env")
          (ok (eq t (cfg:get-boolean c "debug")))
          (ok (null (uiop:getenv "APP_DATABASE__HOST")) "process env untouched")))
      (let ((c (load* :environ '(("X" . "y"))
                      :overrides '(("database.host" . "explicit")))))
        (when c
          (ok (equal "explicit" (cfg:get c "database.host")) "overrides beat .env"))))
    (uiop:delete-file-if-exists toml)
    (uiop:delete-file-if-exists dotenv)))
