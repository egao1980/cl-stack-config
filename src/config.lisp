(in-package #:cl-stack-config)

;;; Precedence: file < env < explicit overrides.
;;; Env: PREFIX_KEY / PREFIX_TABLE__KEY → dotted path (lowercase).

(defvar *config* nil
  "Default config object for accessors when CFG is NIL.")

(defstruct (config (:constructor %make-config)
                   (:conc-name config-)
                   (:predicate config-p))
  (data (make-hash-table :test #'equal) :type hash-table)
  (source nil)
  (prefix nil)
  (env-p t)
  (format :toml)
  (dotenv nil))

(defun %path-list (path)
  (cond
    ((null path) nil)
    ((listp path) (mapcar (lambda (x)
                            (if (stringp x) x (string-downcase (string x))))
                          path))
    ((stringp path)
     (if (zerop (length path))
         nil
         (uiop:split-string path :separator ".")))
    ((symbolp path) (list (string-downcase (symbol-name path))))
    (t (error 'config-type-error
              :message (format nil "bad config path ~s" path)
              :path path))))

(defun %stringish-vector-p (value)
  "Tomlet may emit character vectors instead of STRING."
  (and (vectorp value)
       (not (stringp value))
       (plusp (length value))
       (every #'characterp value)))

(defun %copy-tree (value)
  (cond
    ((hash-table-p value)
     (let ((out (make-hash-table :test #'equal)))
       (maphash (lambda (k v) (setf (gethash k out) (%copy-tree v))) value)
       out))
    ;; STRING is a VECTOR — must precede the general vector case.
    ((stringp value) value)
    ((%stringish-vector-p value)
     (coerce value 'string))
    ((vectorp value) (map 'vector #'%copy-tree value))
    ((consp value) (mapcar #'%copy-tree value))
    (t value)))

(defun %ensure-table (table key)
  (let ((child (gethash key table)))
    (unless (hash-table-p child)
      (setf child (make-hash-table :test #'equal)
            (gethash key table) child))
    child))

(defun %set-path (table path value)
  (let ((keys (%path-list path)))
    (when (null keys)
      (error 'config-type-error :message "empty path" :path path))
    (loop with node = table
          for (key . rest) on keys
          do (if rest
                 (setf node (%ensure-table node key))
                 (setf (gethash key node) value)))
    value))

(defun %get-path (table path)
  (let ((keys (%path-list path))
        (node table))
    (dolist (key keys (values node t))
      (unless (hash-table-p node)
        (return (values nil nil)))
      (multiple-value-bind (v ok) (gethash key node)
        (unless ok
          (return (values nil nil)))
        (setf node v)))))

(defun %parse-bool (s)
  (let ((x (string-downcase (string-trim '(#\Space #\Tab) s))))
    (cond
      ((member x '("true" "1" "yes" "on") :test #'string=) t)
      ((member x '("false" "0" "no" "off") :test #'string=) nil)
      (t (error 'config-type-error
                :message (format nil "not a boolean: ~s" s))))))

(defun %environ-alist (&optional environ)
  "Process environment as (NAME . VALUE) strings.
   ENVIRON when non-nil is used as-is (tests / non-SBCL injection)."
  (or environ
      #+sbcl
      (mapcar (lambda (s)
                (let ((i (position #\= s)))
                  (if i
                      (cons (subseq s 0 i) (subseq s (1+ i)))
                      (cons s ""))))
              (sb-ext:posix-environ))
      #-sbcl
      (error 'config-error
             :message "env overlay: pass :environ alist on non-SBCL (no portable environ list)")))

(defun %split-ddash (string)
  "Split STRING on literal '__' (uiop:split-string treats separator as char set)."
  (let ((parts nil)
        (start 0)
        (len (length string)))
    (loop
      (let ((pos (search "__" string :start2 start)))
        (if pos
            (progn
              (push (subseq string start pos) parts)
              (setf start (+ pos 2)))
            (return))))
    (push (subseq string start len) parts)
    (nreverse parts)))

(defun %apply-variables (table prefix alist)
  "Overlay the PREFIX_* entries of ALIST, ((NAME . VALUE) ...), onto TABLE.
   Nesting via __. Entries with a NIL value (bare .env names) are skipped."
  (when (or (null prefix) (zerop (length prefix)))
    (return-from %apply-variables table))
  (let* ((needle (concatenate 'string (string-upcase prefix) "_"))
         (nlen (length needle)))
    (dolist (pair alist)
      (destructuring-bind (name . raw) pair
        (when (and raw
                   (> (length name) nlen)
                   (string= name needle :end1 nlen))
          (let* ((rest (subseq name nlen))
                 (parts (mapcar #'string-downcase (%split-ddash rest)))
                 (path (format nil "~{~a~^.~}" parts)))
            (%set-path table path raw)))))
    table))

(defun %apply-env (table prefix &optional environ)
  "Overlay getenv keys PREFIX_* onto TABLE (ENVIRON: alist, else the process
   environment). Nesting via __."
  (%apply-variables table prefix (%environ-alist environ)))

(defun %apply-overrides (table overrides)
  (cond
    ((null overrides) table)
    ((hash-table-p overrides)
     (maphash (lambda (k v) (%set-path table k v)) overrides)
     table)
    ((listp overrides)
     (dolist (pair overrides table)
       (if (consp pair)
           (%set-path table (car pair) (cdr pair))
           (error 'config-type-error
                  :message (format nil "bad override ~s" pair)))))
    (t (error 'config-type-error
              :message (format nil "bad overrides ~s" overrides)))))

(defun %detect-format (path format)
  (ecase format
    ((:toml :ini) format)
    (:auto
     (let ((type (string-downcase (or (pathname-type path) ""))))
       (if (member type '("ini" "cfg" "conf") :test #'string=)
           :ini
           :toml)))))

(defun %ini-strip (string &optional (start 0) (end (length string)))
  "STRING from START to END without the leading and trailing whitespace of
   Python's str.strip() (%DOTENV-SPACE-P: NBSP, U+3000 and the other
   Unicode blanks included)."
  (declare (type string string))
  (let* ((from (or (position-if-not #'%dotenv-space-p string :start start :end end) end))
         (to (position-if-not #'%dotenv-space-p string :start from :end end :from-end t)))
    (if to (subseq string from (1+ to)) "")))

(defun %ini-prefix-at-p (prefix string start)
  "PREFIX occurs in STRING at START."
  (let ((end (+ start (length prefix))))
    (and (<= end (length string))
         (string= prefix string :start2 start :end2 end))))

(defun %ini-clean (line inline-prefixes)
  "configparser's _Line.clean of LINE, as (values CLEAN STRIPPED). CLEAN is
   \"\" for a comment line (STRIPPED, LINE without its blanks, starts with #
   or ;); else LINE cut at the first of INLINE-PREFIXES that starts it or
   follows whitespace, then stripped. A blank line, not a comment, has
   CLEAN = STRIPPED = \"\"."
  (declare (type string line))
  (let ((stripped (%ini-strip line)))
    (values
     (if (or (%ini-prefix-at-p "#" stripped 0) (%ini-prefix-at-p ";" stripped 0))
         ""
         (%ini-strip line 0
                     (or (and inline-prefixes
                              (loop for i from 0 below (length line)
                                    when (and (or (zerop i) (%dotenv-space-p (char line (1- i))))
                                              (some (lambda (prefix) (%ini-prefix-at-p prefix line i))
                                                    inline-prefixes))
                                      return i))
                         (length line))))
     stripped)))

(defun %ini-header (clean)
  "The section name of the header line CLEAN, else NIL. configparser's
   \\[(?P<header>.+)\\] matched at the start: the name runs to the last ]
   (at least one character) and is kept as written, case and blanks."
  (declare (type string clean))
  (when (and (plusp (length clean)) (char= #\[ (char clean 0)))
    (let ((close (position #\] clean :from-end t)))
      (when (and close (>= close 2))
        (subseq clean 1 close)))))

(defun %ini-lowercase (name)
  "Python's str.lower() of NAME, configparser's optionxform. SBCL has
   Unicode's full lowercase mapping (sb-unicode:lowercase: İ is i + U+0307,
   a final Σ is ς), which equals str.lower() on every code point; elsewhere
   STRING-DOWNCASE, the simple mapping, leaves the few letters without a
   one-to-one lowercase (İ, ẞ, the Kelvin sign, a final Σ) apart."
  (declare (type string name))
  #+sbcl (sb-unicode:lowercase name)
  #-sbcl (string-downcase name))

(defun %ini-unquote (value)
  "VALUE without one pair of enclosing double or single quotes."
  (declare (type string value))
  (let ((n (length value)))
    (if (and (>= n 2)
             (member (char value 0) '(#\" #\'))
             (char= (char value 0) (char value (1- n))))
        (subseq value 1 (1- n))
        value)))

(defun parse-ini (text &optional path
                       &key inline-comments (key-case :downcase) (strict t)
                       (default-section "DEFAULT") strip-quotes)
  "Parse INI TEXT into a nested EQUAL hash-table the way Python's
   configparser.RawConfigParser() reads a file with its defaults, as
   botocore reads ~/.aws/credentials and ~/.aws/config:

   - A line whose text starts with # or ; (after any indentation) is a
     comment. There are no inline comments: k = a#b ;c is \"a#b ;c\".
   - [NAME] starts a section. NAME runs to the last ] and is kept as
     written, case and blanks.
   - NAME = VALUE or NAME: VALUE is split at the first = or :. NAME is
     lowercased (str.lower()), both are stripped, quotes stay, nothing is
     interpolated.
   - A line indented deeper than its option continues the value, joined
     with newlines. Blank lines inside stay, trailing ones go, comment lines
     are skipped.
   - Every section gets the options of [DEFAULT] it lacks; the DEFAULT
     table is there too.
   - A repeated section or option, an empty name, or a line that is neither
     a header nor an option signals CONFIG-PARSE-ERROR.

   Lines end at LF, CRLF or CR; whitespace is Python's (NBSP, U+3000, ...).
   Unlike configparser, keys before the first section stay at the root
   (MissingSectionHeaderError there) without [DEFAULT]'s options, and a
   leading BOM is dropped. PATH names the source in errors; pass NIL before
   the keywords when there is none. The keywords are configparser's options:

   INLINE-COMMENTS  T for # and ;, or a list of prefixes: a prefix at the
                    start of a line or after whitespace starts a comment
                    (inline_comment_prefixes).
   KEY-CASE         :PRESERVE keeps option names as written (optionxform = str).
   STRICT           NIL: a repeated section merges, a repeated option's last
                    value wins.
   DEFAULT-SECTION  the section every section inherits from; NIL for none.
   STRIP-QUOTES     T drops one pair of enclosing quotes from each value, as
                    parse-ini did before 0.3.1."
  (declare (type string text))
  (check-type key-case (member :downcase :preserve))
  (check-type inline-comments (or boolean cons))
  (check-type default-section (or null string))
  (let* ((inline (if (eq inline-comments t) '("#" ";") inline-comments))
         (root (make-hash-table :test #'equal))
         (current root)
         (defaults nil)
         ;; Sections and (section . option) pairs seen, for STRICT; the root
         ;; is :ROOT.
         (seen (make-hash-table :test #'equal))
         (section :root)
         (option nil)
         (indent-level 0)
         (line-no 0))
    (labels ((fail (control &rest args)
               (error 'config-parse-error
                      :message (format nil "~? (line ~d)" control args line-no)
                      :path path))
             (enter-section (name)
               (let ((table (gethash name root)))
                 (cond ((and default-section (string= name default-section))
                        (setf current (or defaults
                                          (setf defaults (make-hash-table :test #'equal)
                                                (gethash name root) defaults))))
                       ((hash-table-p table)
                        (when strict
                          (fail "duplicate section [~a]" name))
                        (setf current table))
                       (t
                        (setf current (make-hash-table :test #'equal)
                              (gethash name root) current))))
               (setf section name
                     option nil))
             (add-option (clean)
               (let ((delimiter (position-if (lambda (c) (or (char= c #\=) (char= c #\:)))
                                             clean)))
                 (unless delimiter
                   (fail "expected name = value or name: value, got ~s" clean))
                 (let ((name (%ini-strip clean 0 delimiter)))
                   (when (zerop (length name))
                     (fail "empty name in ~s" clean))
                   (let* ((key (if (eq key-case :preserve) name (%ini-lowercase name)))
                          (id (cons section key)))
                     (when (and strict (gethash id seen))
                       (fail "duplicate option ~a~:[ before the first section~; in [~:*~a]~]"
                             key (and (stringp section) section)))
                     (setf (gethash id seen) t
                           option key
                           ;; The value's lines, last first, until the end.
                           (gethash key current) (list (%ini-strip clean (1+ delimiter))))))))
             (join (table)
               ;; '\n'.join(lines).rstrip(): a value whose first line is
               ;; empty (botocore's nested "s3 =" block) keeps its newline.
               (maphash (lambda (key lines)
                          (when (listp lines)
                            (let* ((joined (format nil "~{~a~^~%~}" (reverse lines)))
                                   (end (position-if-not #'%dotenv-space-p joined :from-end t))
                                   (value (subseq joined 0 (if end (1+ end) 0))))
                              (setf (gethash key table)
                                    (if strip-quotes (%ini-unquote value) value)))))
                        table)))
      (let ((text (if (and (plusp (length text)) (char= (char text 0) (code-char #xFEFF)))
                      (subseq text 1)
                      text)))
        (dolist (line (uiop:split-string (%universal-newlines text) :separator '(#\Newline)))
          (incf line-no)
          (multiple-value-bind (clean stripped) (%ini-clean line inline)
            (if (zerop (length clean))
                ;; A blank line (not a comment) inside a value is part of it.
                (when (and option (zerop (length stripped)))
                  (push "" (gethash option current)))
                (let ((indent (or (position-if-not #'%dotenv-space-p line) 0)))
                  (if (and option (> indent indent-level))
                      (push clean (gethash option current))
                      (let ((header (%ini-header clean)))
                        (setf indent-level indent)
                        (if header
                            (enter-section header)
                            (add-option clean)))))))))
      (join root)
      (maphash (lambda (name table)
                 (declare (ignore name))
                 (when (hash-table-p table)
                   (join table)))
               root)
      (when defaults
        (maphash (lambda (name table)
                   (declare (ignore name))
                   (when (and (hash-table-p table) (not (eq table defaults)))
                     (maphash (lambda (key value)
                                (unless (nth-value 1 (gethash key table))
                                  (setf (gethash key table) value)))
                              defaults)))
                 root)))
    root))

(defun load-config (source &key (prefix "APP") (env t) overrides environ (format :auto)
                           dotenv)
  "Parse SOURCE (pathname/string) → CONFIG.
   FORMAT is :toml, :ini, or :auto (.ini/.cfg/.conf → ini, else toml).
   DOTENV — a .env file (see PARSE-DOTENV) whose PREFIX_* names are a layer
   above the file; the process environment is not changed. A missing one
   signals CONFIG-FILE-ERROR with a CONTINUE restart that skips the layer.
   Precedence: file < DOTENV < env (when ENV) < OVERRIDES.
   ENVIRON — optional (NAME . VALUE) alist (required for env overlay off SBCL);
   ${NAME} in DOTENV looks it up first, then earlier .env names."
  (let* ((path (uiop:ensure-pathname source :want-file t
                                            :defaults *default-pathname-defaults*))
         (fmt (%detect-format path format))
         (tree
          (handler-case
              (progn
                (unless (probe-file path)
                  (error 'config-file-error
                         :message (format nil "missing file ~a" path)
                         :path (namestring path)))
                (%copy-tree
                 (ecase fmt
                   (:toml (toml-protocol:decode path))
                   (:ini (parse-ini (uiop:read-file-string path) (namestring path))))))
            (config-error (e) (error e))
            (error (e)
              (error 'config-parse-error
                     :message (format nil "~a" e)
                     :path (namestring path))))))
    (when dotenv
      (%apply-variables tree prefix
                        (apply #'read-dotenv dotenv :override nil
                               (and environ (list :environ environ)))))
    (when env
      (%apply-env tree prefix environ))
    (%apply-overrides tree overrides)
    (%make-config :data tree :source path :prefix prefix :env-p env :format fmt
                  :dotenv dotenv)))

(defun load (source &rest keys &key &allow-other-keys)
  "Alias for LOAD-CONFIG (shadows CL:LOAD in this package)."
  (apply #'load-config source keys))

(defun reload (cfg &key (env (config-env-p cfg)) overrides)
  "Re-read CFG's source file and .env layer; re-apply env/overrides."
  (load-config (config-source cfg)
               :prefix (config-prefix cfg)
               :env env
               :overrides overrides
               :format (config-format cfg)
               :dotenv (config-dotenv cfg)))

(defmacro with-config ((cfg) &body body)
  `(let ((*config* ,cfg))
     ,@body))

(defun %cfg (cfg)
  (or cfg *config*
      (error 'config-error :message "*config* unbound — pass CFG or WITH-CONFIG")))

(defun get (cfg path &key (default nil default-p))
  "Lookup PATH in CFG (or *CONFIG* when CFG is NIL).
   Signals CONFIG-MISSING-ERROR unless :DEFAULT is supplied."
  (let ((c (%cfg cfg)))
    (multiple-value-bind (val ok) (%get-path (config-data c) path)
      (cond
        (ok val)
        (default-p default)
        (t (error 'config-missing-error
                  :message "missing key"
                  :path path))))))

(defun config-set (cfg path value)
  "Mutate CFG at PATH. Returns VALUE."
  (%set-path (config-data (%cfg cfg)) path value))

(defun section (cfg path)
  "Return a CONFIG wrapping the sub-table at PATH."
  (let* ((c (%cfg cfg))
         (val (get c path)))
    (unless (hash-table-p val)
      (error 'config-type-error
             :message (format nil "not a table: ~s" path)
             :path path))
    (%make-config :data val
                  :source (config-source c)
                  :prefix (config-prefix c)
                  :env-p (config-env-p c))))

(defun get-string (cfg path &key (default nil default-p))
  (let ((v (if default-p (get cfg path :default default) (get cfg path))))
    (cond
      ((stringp v) v)
      ((and default-p (null v) (eq default nil)) nil)
      ((null v)
       (error 'config-type-error :message "nil string" :path path))
      (t (princ-to-string v)))))

(defun get-integer (cfg path &key (default nil default-p))
  (let ((v (if default-p (get cfg path :default default) (get cfg path))))
    (cond
      ((integerp v) v)
      ((stringp v) (parse-integer v :junk-allowed nil))
      ((and default-p (eq v default)) default)
      (t (error 'config-type-error
                :message (format nil "not an integer: ~s" v)
                :path path)))))

(defun get-boolean (cfg path &key (default nil default-p))
  (multiple-value-bind (v ok)
      (%get-path (config-data (%cfg cfg)) path)
    (cond
      ((not ok)
       (if default-p default
           (error 'config-missing-error :message "missing key" :path path)))
      ((eq v t) t)
      ((null v) nil)
      ((stringp v) (%parse-bool v))
      (t (error 'config-type-error
                :message (format nil "not a boolean: ~s" v)
                :path path)))))

(defun get-list (cfg path &key (default nil default-p))
  (let ((v (if default-p (get cfg path :default default) (get cfg path))))
    (cond
      ((vectorp v) (coerce v 'list))
      ((listp v) v)
      ((and default-p (eq v default)) default)
      (t (error 'config-type-error
                :message (format nil "not a list/vector: ~s" v)
                :path path)))))
