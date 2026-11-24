(in-package #:cl-stack-config/tests)

;;;; PARSE-INI reads INI text as Python's configparser.RawConfigParser() reads
;;;; a file with its defaults, which is how botocore reads ~/.aws/credentials
;;;; and ~/.aws/config (cl-stack-config#10). tests/ini-parity.py writes the
;;;; cases of tests/ini-parity.sexp from configparser itself.

(defun %ini-pairs (table)
  "TABLE's (KEY VALUE) pairs, sorted by key."
  (let ((pairs '()))
    (maphash (lambda (key value) (push (list key value) pairs)) table)
    (sort pairs #'string< :key #'first)))

(defun %ini-drop-empty-default (sections)
  "SECTIONS without an empty DEFAULT: configparser has a DEFAULT section
   whether or not the file names one."
  (remove-if (lambda (section)
               (and (string= "DEFAULT" (first section)) (null (second section))))
             sections))

(defun %ini-tree (table)
  "PARSE-INI's TABLE as (ROOT-PAIRS SECTIONS), sorted: a section is
   (NAME PAIRS)."
  (let ((root '())
        (sections '()))
    (maphash (lambda (key value)
               (if (hash-table-p value)
                   (push (list key (%ini-pairs value)) sections)
                   (push (list key value) root)))
             table)
    (list (sort root #'string< :key #'first)
          (sort (%ini-drop-empty-default sections) #'string< :key #'first))))

(defun %ini-parity-cases ()
  "The (LABEL KEYWORDS TEXT EXPECTED) cases of tests/ini-parity.sexp (or of
   the file CL_STACK_CONFIG_INI_PARITY names), character codes made strings.
   EXPECTED is :ERROR or a tree as %INI-TREE gives it."
  (let* ((env (uiop:getenv "CL_STACK_CONFIG_INI_PARITY"))
         (path (if (and env (plusp (length env)))
                   (pathname env)
                   (asdf:system-relative-pathname "cl-stack-config"
                                                  "tests/ini-parity.sexp"))))
    (labels ((text (codes) (map 'string #'code-char codes))
             (pairs (rows)
               (sort (mapcar (lambda (row) (list (text (first row)) (text (second row)))) rows)
                     #'string< :key #'first))
             (tree (expected)
               (if (eq expected :error)
                   :error
                   (destructuring-bind (root sections) (rest expected)
                     (list (pairs root)
                           (sort (%ini-drop-empty-default
                                  (mapcar (lambda (section)
                                            (list (text (first section)) (pairs (second section))))
                                          sections))
                                 #'string< :key #'first))))))
      (with-open-file (in path :external-format :utf-8)
        (with-standard-io-syntax
          (let ((*read-eval* nil))
            (loop for form = (read in nil in)
                  until (eq form in)
                  when (and (consp form) (eq :case (first form)))
                    collect (destructuring-bind (label keywords codes expected) (rest form)
                              (list label keywords (text codes) (tree expected))))))))))

(deftest ini-configparser-parity
  "INI texts (tests/ini-parity.py: the README's rules by name, then random
   ones with LF / CR / CRLF ends, Unicode blanks and indentation, comment
   prefixes, quotes, %, both delimiters, continuation lines, [DEFAULT],
   repeated sections and options) give what configparser.RawConfigParser()
   reads from the same file, or a CONFIG-PARSE-ERROR where it raises. Some
   cases set inline_comment_prefixes, strict, optionxform or
   default_section, given to PARSE-INI as its keywords. The full-lowercase
   cases (İ, ẞ, final Σ) need SBCL's Unicode lowercase."
  (let ((failures '())
        (count 0))
    (dolist (case (%ini-parity-cases))
      (destructuring-bind (label keywords text expected) case
        (unless (and (not (member :sbcl *features*))
                     (eql 0 (search "full lowercase" label)))
          (incf count)
          (let ((got (handler-case (%ini-tree (apply #'cfg:parse-ini text nil keywords))
                       (cfg:config-parse-error () :error)
                       (error (e) (list :lisp-error (princ-to-string e))))))
            (unless (equal expected got)
              (push (list label keywords (map 'list #'char-code text)
                          :got got :expected expected)
                    failures))))))
    (ok (and (plusp count) (null failures))
        (format nil "~d cases equal configparser~@[; ~d differ, first: ~s~]"
                count (and failures (length failures)) (car (last failures))))))

(deftest ini-aws-credentials-as-botocore-reads-them
  "cl-stack-config#10: a secret with # and ; is read whole, and upper-case
   option names are found under their lowercase names."
  (let* ((ini (cfg:parse-ini (format nil "[default]~%AWS_ACCESS_KEY_ID = AKIDHASH~%~
                                          aws_secret_access_key = minio#Secret;2026~%~
                                          [profile Dev]~%Region = eu-west-1 # comment?~%")))
         (default (gethash "default" ini)))
    (ok (string= "minio#Secret;2026" (gethash "aws_secret_access_key" default)))
    (ok (string= "AKIDHASH" (gethash "aws_access_key_id" default)))
    (ok (string= "eu-west-1 # comment?" (gethash "region" (gethash "profile Dev" ini)))
        "section names keep their case; # after a blank is part of the value")))

(deftest ini-compatibility-keywords
  ":inline-comments t, :key-case :preserve and :strip-quotes t read files
   that rely on parse-ini 0.3.0's inline comments, case-kept names and
   unquoting."
  (let* ((text (format nil "[db]~%Host = h ; primary~%name = \"app db\"~%"))
         (old (gethash "db" (cfg:parse-ini text nil :inline-comments t :key-case :preserve
                                                    :strip-quotes t)))
         (new (gethash "db" (cfg:parse-ini text))))
    (ok (string= "h" (gethash "Host" old)))
    (ok (string= "app db" (gethash "name" old)))
    (ok (string= "h ; primary" (gethash "host" new)))
    (ok (string= "\"app db\"" (gethash "name" new)) "configparser keeps quotes")
    (ok (string= "a" (gethash "k" (gethash "s" (cfg:parse-ini (format nil "[s]~%k = a // b~%")
                                                              nil :inline-comments '("//")))))
        "a list of inline prefixes")))

(deftest ini-errors-name-line-and-path
  "A repeated option (in any case), a line without = or :, and an empty
   name signal CONFIG-PARSE-ERROR with the line and PATH; :strict nil
   merges repeats instead."
  (flet ((err (text)
           (handler-case (progn (cfg:parse-ini text #p"/x/credentials") nil)
             (cfg:config-parse-error (e) e))))
    (let ((e (err (format nil "[s]~%a = 1~%A = 2~%"))))
      (ok (typep e 'cfg:config-parse-error))
      (ok (search "line 3" (cfg:config-error-message e)) (cfg:config-error-message e))
      (ok (equal #p"/x/credentials" (cfg:config-error-path e))))
    (ok (err (format nil "[s]~%junk~%")))
    (ok (err (format nil "[s]~%= v~%")))
    (ok (err (format nil "[s]~%[s]~%")))
    (ok (string= "2" (gethash "a" (gethash "s" (cfg:parse-ini (format nil "[s]~%a = 1~%[s]~%A = 2~%")
                                                              nil :strict nil)))))))

(deftest ini-differences-from-configparser
  "Keys before the first section stay at the root (configparser:
   MissingSectionHeaderError) and do not get [DEFAULT]'s options; a
   leading BOM is dropped (configparser: no section header)."
  (let ((ini (cfg:parse-ini (format nil "~cdebug = true~%[DEFAULT]~%region = r~%[s]~%"
                                    (code-char #xFEFF)))))
    (ok (string= "true" (gethash "debug" ini)))
    (ok (null (nth-value 1 (gethash "region" ini))))
    (ok (string= "r" (gethash "region" (gethash "s" ini))))))
