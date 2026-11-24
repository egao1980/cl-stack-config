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

(deftest dotenv-files-read-with-universal-newlines
  "evidence (PR #9): python-dotenv opens a .env file in text mode, so CRLF
   (Windows, git autocrlf) and CR-only line ends read as LF. python-dotenv
   1.1.1 dotenv_values(path) / load_dotenv(path) on these files:
     CRLF: {A: \"line1\\nline2\", B: \"x\", C: \"s1\\ns2\"}
     CR:   {A: \"1\", B: \"2\"}
   read-dotenv keeps the CR inside multi-line quoted values (a PEM key in
   .env gets CRs) and reads a CR-only file as one line: A = \"1\\rB=2\"."
  (let ((crlf (%write-utf8 (format nil "A=\"line1~C~%line2\"~C~%B=x~C~%C='s1~C~%s2'~C~%"
                                   #\Return #\Return #\Return #\Return #\Return)
                           "env"))
        (cr (%write-utf8 (format nil "A=1~CB=2~C" #\Return #\Return) "env")))
    (flet ((codes (alist)
             (mapcar (lambda (pair)
                       (cons (car pair) (and (cdr pair) (map 'list #'char-code (cdr pair)))))
                     alist)))
      (unwind-protect
          (let ((got-crlf (cfg:read-dotenv crlf :environ '()))
                (got-cr (cfg:read-dotenv cr :environ '())))
            (ok (equal `(("A" . ,(format nil "line1~%line2")) ("B" . "x")
                         ("C" . ,(format nil "s1~%s2")))
                       got-crlf)
                (format nil "CRLF file, char codes: ~s" (codes got-crlf)))
            (ok (equal '(("A" . "1") ("B" . "2")) got-cr)
                (format nil "CR-only file, char codes: ~s" (codes got-cr))))
        (uiop:delete-file-if-exists crlf)
        (uiop:delete-file-if-exists cr)))))

(deftest dotenv-newline-edge-cases
  "python-dotenv 1.1.1 parity: a file with mixed CR, CRLF and LF ends reads
   each as LF; an escaped \\r\\n in double quotes is still CR LF; a string
   (like dotenv_values(stream=StringIO(...))) keeps its CRs."
  (let ((mixed (%write-utf8 (format nil "A=1~C~CB=\"x~Cy\"~%~CC=3~CD=\"esc\\r\\n\""
                                    #\Return #\Return #\Return #\Return #\Return)
                            "env")))
    (unwind-protect
        (ok (equal `(("A" . "1") ("B" . ,(format nil "x~%y")) ("C" . "3")
                     ("D" . ,(format nil "esc~C~C" #\Return #\Newline)))
                   (cfg:read-dotenv mixed :environ '()))
            "mixed line ends in a file")
      (uiop:delete-file-if-exists mixed)))
  (ok (equal `(("A" . ,(format nil "l1~C~%l2" #\Return)))
             (cfg:parse-dotenv (format nil "A=\"l1~C~%l2\"~C~%" #\Return #\Return)
                               :environ '()))
      "parse-dotenv of a string keeps CR inside quotes"))

(deftest dotenv-string-line-ends-and-unicode-blanks
  "evidence (PR #9, round 2): python-dotenv's own grammar, not only Python's
   text mode, ends a line at CR (_end_of_line, _unquoted_value [^\\r\\n]*) and
   counts every Unicode blank of Python's \\s (VT, NBSP, ...) as a blank. The
   README says parse-dotenv takes a string like dotenv_values(stream=...).
   python-dotenv 1.1.1 dotenv_values(stream=io.StringIO(text)):
     'A=1\\rB=2\\r'         {A: '1', B: '2'}
     \"A='x'\\rB=2\\r\"       {A: 'x', B: '2'}
     'A=\"x\"\\rB=2\\r'       {A: 'x', B: '2'}
     '# c\\rA=1\\r'         {A: '1'}
     'A=foo\\xa0\\n'        {A: 'foo'}
     'A=x\\xa0# c\\n'       {A: 'x'}
     'A=x\\x0b# c\\n'       {A: 'x'}
     'A\\xa0=1\\n'          {A: '1'}
     'export\\xa0A=1\\n'    {A: '1'}"
  (let ((cr (code-char 13))
        (nbsp (code-char #xa0))
        (vt (code-char 11)))
    (dolist (row (list (list "CR-only string, unquoted"
                             (format nil "A=1~cB=2~c" cr cr)
                             '(("A" . "1") ("B" . "2")))
                       (list "CR-only string, single quotes"
                             (format nil "A='x'~cB=2~c" cr cr)
                             '(("A" . "x") ("B" . "2")))
                       (list "CR-only string, double quotes"
                             (format nil "A=\"x\"~cB=2~c" cr cr)
                             '(("A" . "x") ("B" . "2")))
                       (list "CR-only string, comment line"
                             (format nil "# c~cA=1~c" cr cr)
                             '(("A" . "1")))
                       (list "trailing NBSP is trimmed"
                             (format nil "A=foo~c~%" nbsp)
                             '(("A" . "foo")))
                       (list "NBSP before # starts a comment"
                             (format nil "A=x~c# c~%" nbsp)
                             '(("A" . "x")))
                       (list "VT before # starts a comment"
                             (format nil "A=x~c# c~%" vt)
                             '(("A" . "x")))
                       (list "NBSP ends a key"
                             (format nil "A~c=1~%" nbsp)
                             '(("A" . "1")))
                       (list "NBSP after export"
                             (format nil "export~cA=1~%" nbsp)
                             '(("A" . "1")))))
      (destructuring-bind (label text expected) row
        (let ((got (%parse-quietly text)))
          (ok (equal expected got)
              (format nil "~a: ~s" label
                      (mapcar (lambda (pair)
                                (cons (map 'list #'char-code (car pair))
                                      (and (cdr pair)
                                           (map 'list #'char-code (cdr pair)))))
                              got))))))))

(deftest dotenv-grammar-edge-cases
  "python-dotenv 1.1.1 dotenv_values(stream=io.StringIO(text)) on the
   corners of its grammar: Unicode blanks between statements and as
   separators, CR as a line end everywhere, a quote with no closing one
   before the end (its regex backtracks to the last quote), a bare name that
   defines a name for ${NAME:-d}."
  (let ((nbsp (code-char #xa0))
        (nel (code-char #x85))
        (ls (code-char #x2028))
        (fs (code-char #x1c))
        (cr (code-char 13)))
    (dolist (row (list (list "NEL before # starts a comment"
                             (format nil "A=x~c# c~%B=2~%" nel)
                             '(("A" . "x") ("B" . "2")))
                       (list "U+2028 is a blank, not a line end"
                             (format nil "A=1~cB=2~%" ls)
                             `(("A" . ,(format nil "1~cB=2" ls))))
                       (list "U+001C before # starts a comment"
                             (format nil "A=x~c#c~%" fs)
                             '(("A" . "x")))
                       (list "blank-only lines and a leading NBSP"
                             (format nil "~c~%~cA=1~%" nbsp nbsp)
                             '(("A" . "1")))
                       (list "CR, CR CR, LF CR line ends"
                             (format nil "A=1~c~cB=2~%~cC=3" cr cr cr)
                             '(("A" . "1") ("B" . "2") ("C" . "3")))
                       (list "junk after a quoted value up to CR"
                             (format nil "A=\"x\" j~cB=2~c" cr cr)
                             '(("B" . "2")))
                       (list "single-quoted value over a CR"
                             (format nil "A='x~cy'~cB=2" cr cr)
                             `(("A" . ,(format nil "x~cy" cr)) ("B" . "2")))
                       (list "escaped quote and no other: the value ends there"
                             (format nil "A=\"x\\\"~%B=2~%")
                             '(("A" . "x\\") ("B" . "2")))
                       (list "a single-quoted key may span lines; '' is no key"
                             (format nil "'a~%b'=1~%''=2~%B=3~%")
                             `((,(format nil "a~%b") . "1") ("B" . "3")))
                       (list "quote characters inside an unquoted key"
                             (format nil "A\"B=1~%")
                             '(("A\"B" . "1")))
                       (list "export alone, export #, export ="
                             (format nil "export~%export #c~%export =1~%B=2~%")
                             '(("export") ("B" . "2")))
                       (list "a # right after a key ends a bare name"
                             (format nil "A#c~%B=2~%")
                             '(("A") ("B" . "2")))
                       (list "a BOM in a string is part of the first name"
                             (format nil "~cA=1~%" (code-char #xfeff))
                             `((,(format nil "~cA" (code-char #xfeff)) . "1")))
                       (list "a bare name expands to \"\", not to its default"
                             (format nil "N~%X=${N:-d}~%Y=${A:-d}~%")
                             '(("N") ("X" . "") ("Y" . "envA")))))
      (destructuring-bind (label text expected) row
        (let ((got (%parse-quietly text)))
          (ok (equal expected got) (format nil "~a: ~s" label got))))))
  (ok (equal '(("HOME") ("X" . ""))
             (cfg:parse-dotenv (format nil "HOME~%X=${HOME:-d}~%") :override nil
                                                                   :environ '()))
      ":override nil: a bare name the environment lacks expands to \"\"")
  (ok (equal '(("HOME") ("X" . "/h"))
             (cfg:parse-dotenv (format nil "HOME~%X=${HOME:-d}~%") :override nil
                                                                   :environ '(("HOME" . "/h"))))
      ":override nil: the environment wins over a bare name"))

(defun %dotenv-parity-cases ()
  "The (EXPAND TEXT EXPECTED) cases of tests/dotenv-parity.sexp (or of the file
   CL_STACK_CONFIG_DOTENV_PARITY names), with character codes made strings:
   a pair there is (NAME-CODES (:s VALUE-CODES...)) or (NAME-CODES NIL)."
  (let* ((env (uiop:getenv "CL_STACK_CONFIG_DOTENV_PARITY"))
         (path (if (and env (plusp (length env)))
                   (pathname env)
                   (asdf:system-relative-pathname "cl-stack-config"
                                                  "tests/dotenv-parity.sexp"))))
    (flet ((text (codes) (map 'string #'code-char codes)))
      (with-open-file (in path :external-format :utf-8)
        (with-standard-io-syntax
            (let ((*read-eval* nil))
              (loop for form = (read in nil in)
                    until (eq form in)
                    when (eq :case (first form))
                      collect (destructuring-bind (expand text pairs) (rest form)
                                (list expand (text text)
                                      (mapcar (lambda (pair)
                                                (destructuring-bind (name value) pair
                                                  (cons (text name)
                                                        (and value (text (rest value))))))
                                              pairs))))))))))

(deftest dotenv-python-dotenv-random-parity
  "Random .env texts (tests/dotenv-parity.py: LF / CR / CRLF ends, Unicode
   blanks, quotes, backslashes, comments, export, ${...}) give what
   python-dotenv 1.1.1's dotenv_values gives, with and without
   interpolation; the documented differences are left out of the set."
  (let ((cases (%dotenv-parity-cases))
        (failures '()))
    (dolist (case cases)
      (destructuring-bind (expand text expected) case
        (let ((got (%parse-quietly text :expand expand)))
          (unless (equal expected got)
            (push (list :text (map 'list #'char-code text) :expand expand
                        :got got :expected expected)
                  failures)))))
    (ok (and (plusp (length cases)) (null failures))
        (format nil "~d cases equal python-dotenv 1.1.1~@[; ~d differ, first: ~s~]"
                (length cases) (and failures (length failures))
                (car (last failures))))))

(deftest dotenv-undecodable-file-signals-config-error
  "evidence (PR #9, round 3): a .env file that is not UTF-8 (a Latin-1
   \"APP_NAME=caf\\xe9\" saved by an old editor; python-dotenv raises
   UnicodeDecodeError) leaks SBCL's internal SB-INT:STREAM-DECODING-ERROR
   from READ-DOTENV, LOAD-DOTENV and LOAD-CONFIG :DOTENV, so
   (handler-case ... (config-error ...)) does not catch it. The TOML file
   of the same LOAD-CONFIG call, with the same byte, signals
   CONFIG-PARSE-ERROR naming its path."
  (flet ((write-octets (octets type)
           (uiop:with-temporary-file (:pathname path :prefix "cl-stack-config-" :type type
                                      :keep t)
             (with-open-file (out path :direction :output :if-exists :supersede
                                       :element-type '(unsigned-byte 8))
               (write-sequence (coerce octets '(vector (unsigned-byte 8))) out))
             path))
         (condition-of (thunk)
           (handler-case (progn (funcall thunk) nil)
             (error (e) e))))
    (let* ((latin1 (append (map 'list #'char-code "APP_NAME=caf") '(#xe9 10)))
           (env (write-octets latin1 "env"))
           (toml (write-octets (append (map 'list #'char-code "[db]
host = \"caf") '(#xe9) (map 'list #'char-code "\"
")) "toml"))
           (good-toml (%write-tmp "[db]
host = \"x\"
" "toml")))
      (unwind-protect
          (progn
            (let ((c (condition-of (lambda () (cfg:load-config toml :environ '())))))
              (ok (typep c 'cfg:config-parse-error)
                  (format nil "control: Latin-1 TOML => ~s" (type-of c))))
            (dolist (call (list (cons "read-dotenv" (lambda () (cfg:read-dotenv env :environ '())))
                                (cons "load-dotenv" (lambda () (cfg:load-dotenv env)))
                                (cons "load-config :dotenv"
                                      (lambda () (cfg:load-config good-toml :dotenv env
                                                                            :environ '())))))
              (let ((c (condition-of (cdr call))))
                (ok (typep c 'cfg:config-error)
                    (format nil "~a of a Latin-1 .env => ~s" (car call) (type-of c))))))
        (mapc #'uiop:delete-file-if-exists (list env toml good-toml))))))
