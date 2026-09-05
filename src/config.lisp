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
  (env-p t))

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

(defun %apply-env (table prefix &optional environ)
  "Overlay getenv keys PREFIX_* onto TABLE. Nesting via __."
  (when (or (null prefix) (zerop (length prefix)))
    (return-from %apply-env table))
  (let* ((needle (concatenate 'string (string-upcase prefix) "_"))
         (nlen (length needle)))
    (dolist (pair (%environ-alist environ))
      (destructuring-bind (name . raw) pair
        (when (and (> (length name) nlen)
                   (string= name needle :end1 nlen))
          (let* ((rest (subseq name nlen))
                 (parts (mapcar #'string-downcase (%split-ddash rest)))
                 (path (format nil "~{~a~^.~}" parts)))
            (%set-path table path raw)))))
    table))

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

(defun load-config (source &key (prefix "APP") (env t) overrides environ)
  "Parse TOML SOURCE (pathname/string) → CONFIG.
   Precedence: file < env (when ENV) < OVERRIDES.
   ENVIRON — optional (NAME . VALUE) alist (required for env overlay off SBCL)."
  (let* ((path (uiop:ensure-pathname source :want-file t
                                     :defaults *default-pathname-defaults*))
         (tree
          (handler-case
              (progn
                (unless (probe-file path)
                  (error 'config-file-error
                         :message (format nil "missing file ~a" path)
                         :path (namestring path)))
                (%copy-tree (toml-protocol:decode path)))
            (config-error (e) (error e))
            (error (e)
              (error 'config-parse-error
                     :message (format nil "~a" e)
                     :path (namestring path))))))
    (when env
      (%apply-env tree prefix environ))
    (%apply-overrides tree overrides)
    (%make-config :data tree :source path :prefix prefix :env-p env)))

(defun load (source &rest keys &key &allow-other-keys)
  "Alias for LOAD-CONFIG (shadows CL:LOAD in this package)."
  (apply #'load-config source keys))

(defun reload (cfg &key (env (config-env-p cfg)) overrides)
  "Re-read CFG's source file; re-apply env/overrides."
  (load-config (config-source cfg)
               :prefix (config-prefix cfg)
               :env env
               :overrides overrides))

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
