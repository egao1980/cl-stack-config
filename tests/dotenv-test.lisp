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

;;; Edge cases. Expected values are python-dotenv 1.1.1's dotenv_values /
;;; load_dotenv output on the same text, except the rows marked "differs".

(defun %write-utf8 (text type)
  "Like %WRITE-TMP, in UTF-8 whatever the locale."
  (uiop:with-temporary-file (:pathname path :prefix "cl-stack-config-" :type type
                             :keep t)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (write-string text out))
    path))

(defun %parse (text &rest keys)
  (apply #'cfg:parse-dotenv text :environ '(("OUTER" . "outer") ("A" . "envA")) keys))

(defun %parse-quietly (text &rest keys)
  "PARSE-DOTENV of TEXT and the lines of its DOTENV-SYNTAX-WARNINGs."
  (let ((lines '()))
    (values (handler-bind ((cfg:dotenv-syntax-warning
                            (lambda (w)
                              (push (cfg:dotenv-syntax-warning-line w) lines)
                              (muffle-warning w))))
              (apply #'%parse text keys))
            (nreverse lines))))

(defparameter *dotenv-parity-rows*
  (list
   (list "inline comments"
         (format nil "A=b #c~%B=b#c~%D=\"q\" # c~%E='s' #c~%F=#x~%")
         '(("A" . "b") ("B" . "b#c") ("D" . "q") ("E" . "s") ("F" . "#x")))
   (list "blanks and export"
         (format nil "  A  =  spaced value  ~%export   B=1~%export=2~%~%   # indented comment~%")
         '(("A" . "spaced value") ("B" . "1") ("export" . "2")))
   (list "double-quote escapes"
         "A=\"t\\tn\\nq\\\"b\\\\s\\'x\\d\\u00e9\""
         `(("A" . ,(format nil "t~Cn~%q\"b\\s'x\\d\\u00e9" #\Tab))))
   (list "single-quote escapes"
         "A='t\\tn\\nq\\'b\\\\s'"
         '(("A" . "t\\tn\\nq'b\\s")))
   (list "multi-line quoted value"
         (format nil "A=\"line1~%line2\"~%B=after~%")
         `(("A" . ,(format nil "line1~%line2")) ("B" . "after")))
   (list "bare name"
         (format nil "A~%B=1~%")
         '(("A") ("B" . "1")))
   (list "CRLF"
         (format nil "A=1~C~%B=\"x\"~C~%C=3~C~%" #\Return #\Return #\Return)
         '(("A" . "1") ("B" . "x") ("C" . "3")))
   (list "repeated name: first position, last value"
         (format nil "A=1~%B=2~%A=3~%")
         '(("A" . "3") ("B" . "2")))
   (list "empty values"
         (format nil "A=\"\"~%B=''~%C=~%")
         '(("A" . "") ("B" . "") ("C" . "")))
   (list "key characters and = in values"
         (format nil "A.B=1~%A-B=2~%A:B=3~%E=b=c~%F==~%'Q K'=q~%")
         '(("A.B" . "1") ("A-B" . "2") ("A:B" . "3") ("E" . "b=c") ("F" . "=") ("Q K" . "q")))
   (list "expansion: file first, then environ, default, empty; $A is literal"
         (format nil "A=1~%B=${A}-${MISSING}-${MISSING:-dflt}-${OUTER}~%D=\"${A}\"~%E=$A~%A=2~%F=${A}~%G=${A:B}~%")
         '(("A" . "2") ("B" . "1--dflt-outer") ("D" . "1") ("E" . "$A") ("F" . "2")
           ("G" . "${A:B}")))
   (list "differs: single quotes are literal (python-dotenv expands them)"
         (format nil "A=1~%C='${A}'~%")
         '(("A" . "1") ("C" . "${A}")))
   (list "differs: NAME= # note is empty (python-dotenv: \"# note\")"
         "C= # note"
         '(("C" . ""))))
  "(LABEL TEXT EXPECTED): python-dotenv 1.1.1 dotenv_values output, but for
   the rows marked differs.")

(deftest dotenv-python-dotenv-parity
  ;; A loop, not DEFTEST-PARAMETRIZE :ROWS: rove 0.11.0 splices :ROWS values
  ;; into code unquoted, so list values are evaluated as calls.
  (dolist (row *dotenv-parity-rows*)
    (destructuring-bind (label text expected) row
      (let ((got (%parse-quietly text)))
        (ok (equal expected got) (format nil "~a: ~s" label got))))))

(deftest dotenv-expansion-options
  (let ((text (format nil "A=1~%B=${A}~%D=\"${A}\"~%")))
    (ok (equal '(("A" . "1") ("B" . "envA") ("D" . "envA"))
               (%parse text :override nil))
        ":override nil: the environment wins, as load_dotenv")
    (ok (equal '(("A" . "1") ("B" . "${A}") ("D" . "${A}"))
               (%parse text :expand nil)))))

(deftest dotenv-bad-statements-warn-and-skip
  "As python-dotenv: a bad statement is skipped with a warning naming its
   line; a quoted value may run over lines, an unterminated one costs only
   its own line."
  (multiple-value-bind (values lines)
      (%parse-quietly (format nil "A=1~%=nokey~%B=\"spans~%C=3~%'D'=4~%E=\"x\" junk~%F=6~%"))
    (ok (equal '(("A" . "1") ("F" . "6")) values))
    (ok (equal '(2 3) lines)))
  (multiple-value-bind (values lines)
      (%parse-quietly (format nil "A=1~%B=\"open~%C=3~%"))
    (ok (equal '(("A" . "1") ("C" . "3")) values))
    (ok (equal '(2) lines)))
  (ok (signals (cfg:parse-dotenv "=x" :environ '()) 'cfg:dotenv-syntax-warning)))

(deftest dotenv-read-and-load-files
  (let ((path (%write-utf8 (format nil "~CPYCL_BOM_8D=1~%PYCL_BARE_8D~%PYCL_SET_8D=from-file~%"
                                   (code-char #xFEFF))
                           "env"))
        (missing (merge-pathnames "no-such-dir-8d/.env" (uiop:temporary-directory))))
    (unwind-protect
        (progn
          (ok (equal '(("PYCL_BOM_8D" . "1") ("PYCL_BARE_8D") ("PYCL_SET_8D" . "from-file"))
                     (cfg:read-dotenv path))
              "UTF-8 BOM dropped")
          ;; Not "": on Windows an empty value unsets the variable.
          (setf (uiop:getenv "PYCL_SET_8D") "outer")
          (ok (equal '(("PYCL_BOM_8D" . "1")) (cfg:load-dotenv path))
              "returns what it set; bare names and set variables are kept")
          (ok (equal "outer" (uiop:getenv "PYCL_SET_8D")))
          (ok (null (uiop:getenv "PYCL_BARE_8D")))
          (ok (null (cfg:load-dotenv missing :if-does-not-exist nil)))
          (ok (signals (cfg:load-dotenv missing) 'cfg:config-file-error))
          (ok (null (handler-bind ((cfg:config-file-error #'continue))
                      (cfg:load-dotenv missing)))
              "CONTINUE skips a missing file"))
      (setf (uiop:getenv "PYCL_BOM_8D") ""
            (uiop:getenv "PYCL_SET_8D") "")
      (uiop:delete-file-if-exists path))))

(deftest dotenv-layer-expansion-missing-and-reload
  (let ((toml (%write-tmp (format nil "name = \"file\"~%") "toml"))
        (dotenv (%write-tmp (format nil "APP_URL=http://${HOST}:${PORT:-80}/~%APP_NAME=dotenv~%APP_BARE~%OTHER=x~%")
                            "env"))
        (missing (merge-pathnames "no-such-dir-8d/.env" (uiop:temporary-directory))))
    (unwind-protect
        (let ((c (cfg:load-config toml :prefix "APP" :dotenv dotenv
                                       :environ '(("HOST" . "h")))))
          (ok (equal "http://h:80/" (cfg:get c "url")) "${} from the environment, then default")
          (ok (equal "dotenv" (cfg:get c "name")))
          (ok (null (cfg:get c "bare" :default nil)) "bare names are not values")
          (ok (null (cfg:get c "other" :default nil)) "only PREFIX_ names")
          (ok (equal dotenv (cfg:config-dotenv c)))
          (with-open-file (out dotenv :direction :output :if-exists :supersede)
            (format out "APP_NAME=changed~%"))
          (ok (equal "changed" (cfg:get (cfg:reload c :env nil) "name")) "reload re-reads .env")
          (ok (signals (cfg:load-config toml :prefix "APP" :env nil :dotenv missing)
                'cfg:config-file-error))
          (let ((c (handler-bind ((cfg:config-file-error #'continue))
                     (cfg:load-config toml :prefix "APP" :env nil :dotenv missing))))
            (ok (equal "file" (cfg:get c "name")) "CONTINUE skips the layer")))
      (uiop:delete-file-if-exists toml)
      (uiop:delete-file-if-exists dotenv))))
