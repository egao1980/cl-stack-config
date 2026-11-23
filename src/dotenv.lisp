(in-package #:cl-stack-config)

;;; .env files (python-dotenv syntax), cl-stack-config#8.
;;;
;;;   # comment
;;;   export NAME=value          ; "export " is optional
;;;   NAME = unquoted value # trailing comment
;;;   NAME="double: \n \t \" \\ escapes, ${OTHER} and ${OTHER:-default}"
;;;   NAME='single: literal, only \' and \\ escapes'
;;;   NAME                       ; no "=": parsed with value NIL, never set
;;;
;;; Quoted values may span lines. A statement that does not parse signals
;;; DOTENV-SYNTAX-WARNING and is skipped, as python-dotenv logs and skips.
;;; Two deliberate differences from python-dotenv 1.1: single-quoted values
;;; are not expanded (as in sh and docker compose), and in "NAME= # note"
;;; the # starts a comment (python-dotenv returns "# note").

(define-condition dotenv-syntax-warning (warning)
  ((path :initarg :path :reader dotenv-syntax-warning-path :initform nil)
   (line :initarg :line :reader dotenv-syntax-warning-line :initform nil))
  (:report (lambda (c s)
             (format s "could not parse .env statement~@[ in ~a~]~@[ at line ~d~]; skipped"
                     (dotenv-syntax-warning-path c)
                     (dotenv-syntax-warning-line c))))
  (:documentation "A .env statement that does not parse. The statement is
skipped; MUFFLE-WARNING silences it, a handler may also signal an error."))

(defun %dotenv-hspace-p (c)
  (and c (member c '(#\Space #\Tab #\Return #\Page)) t))

(defun %dotenv-unescape (raw kind)
  "Decode the escapes of a quoted value: KIND :double knows \\\\ \\' \\\" \\a
   \\b \\f \\n \\r \\t \\v, :single only \\\\ and \\'. Other backslashes stay."
  (if (eq kind :unquoted)
      raw
      (with-output-to-string (out)
        (let ((i 0)
              (n (length raw)))
          (loop while (< i n)
                do (let ((c (char raw i))
                         (next (and (< (1+ i) n) (char raw (1+ i)))))
                     (let ((decoded
                             (and (char= c #\\) next
                                  (if (eq kind :single)
                                      (and (member next '(#\\ #\')) next)
                                      (case next
                                        ((#\\ #\' #\") next)
                                        (#\a (code-char 7))
                                        (#\b (code-char 8))
                                        (#\f (code-char 12))
                                        (#\n #\Newline)
                                        (#\r (code-char 13))
                                        (#\t #\Tab)
                                        (#\v (code-char 11))
                                        (t nil))))))
                       (if decoded
                           (progn (write-char decoded out) (incf i 2))
                           (progn (write-char c out) (incf i))))))))))

(defun %dotenv-statements (text path)
  "Statements of TEXT as (NAME VALUE KIND) in file order. VALUE is decoded
   but not expanded (NIL for a bare NAME); KIND is :single, :double,
   :unquoted or NIL."
  (let ((i 0)
        (line 1)
        (n (length text))
        (out '()))
    (labels ((peek (&optional (k 0))
               (let ((j (+ i k)))
                 (and (< j n) (char text j))))
             (advance ()
               (when (char= #\Newline (char text i))
                 (incf line))
               (incf i))
             (skip-hspace ()
               (loop while (%dotenv-hspace-p (peek)) do (advance)))
             (skip-to-eol ()
               (loop while (and (peek) (char/= #\Newline (peek))) do (advance)))
             (end-of-statement ()
               "Blanks, an optional comment, then a newline or the end: consumed, T."
               (skip-hspace)
               (when (eql #\# (peek))
                 (skip-to-eol))
               (cond ((null (peek)) t)
                     ((char= #\Newline (peek)) (advance) t)
                     (t nil)))
             (skip-bad (start-line)
               (warn 'dotenv-syntax-warning :path path :line start-line)
               (skip-to-eol)
               (when (peek) (advance)))
             (read-quoted (quote)
               "At QUOTE: the raw text up to the closing quote (consumed), or NIL."
               (advance)
               (let ((start i))
                 (loop
                   (let ((c (peek)))
                     (cond ((null c) (return nil))
                           ((and (char= c #\\) (peek 1)) (advance) (advance))
                           ((char= c quote)
                            (return (prog1 (subseq text start i) (advance))))
                           (t (advance)))))))
             (read-key ()
               (if (eql #\' (peek))
                   (read-quoted #\')
                   (let ((start i))
                     (loop while (and (peek)
                                      (not (member (peek) '(#\= #\# #\Newline)))
                                      (not (%dotenv-hspace-p (peek))))
                           do (advance))
                     (and (> i start) (subseq text start i)))))
             (read-unquoted (after-space)
               "Up to the end of the line; a # after whitespace (AFTER-SPACE: there was
                some between = and the value) starts a comment."
               (let ((start i))
                 (loop for c = (peek)
                       until (or (null c) (char= c #\Newline)
                                 (and (char= c #\#) after-space))
                       do (setf after-space (%dotenv-hspace-p c))
                          (advance))
                 (prog1 (string-right-trim '(#\Space #\Tab #\Return #\Page)
                                           (subseq text start i))
                   (skip-to-eol)
                   (when (peek) (advance))))))
      (loop
        (loop (skip-hspace)
              (cond ((eql #\Newline (peek)) (advance))
                    ((eql #\# (peek)) (skip-to-eol))
                    (t (return))))
        (unless (peek)
          (return))
        (let ((start-line line))
          (when (and (< (+ i 6) n)
                     (string= "export" text :start2 i :end2 (+ i 6))
                     (%dotenv-hspace-p (peek 6)))
            (incf i 6)
            (skip-hspace))
          (let ((name (read-key)))
            (skip-hspace)
            (cond
              ((or (null name) (zerop (length name)))
               (skip-bad start-line))
              ((eql #\= (peek))
               (advance)
               (let ((spaced (%dotenv-hspace-p (peek))))
                 (skip-hspace)
                 (let* ((value-start i)
                        (value-line line)
                        (kind (case (peek) (#\' :single) (#\" :double) (t :unquoted)))
                        (raw (if (eq kind :unquoted)
                                 (read-unquoted spaced)
                                 (read-quoted (peek)))))
                   (cond
                    ((eq kind :unquoted)
                     (push (list name raw kind) out))
                    ((and raw (end-of-statement))
                     (push (list name (%dotenv-unescape raw kind) kind) out))
                    (raw (skip-bad start-line))
                    (t
                     ;; Unterminated quote: skip this line only, not the file.
                     (setf i value-start
                           line value-line)
                     (skip-bad start-line))))))
              ((end-of-statement)
               (push (list name nil nil) out))
              (t (skip-bad start-line)))))))
    (nreverse out)))

(defun %dotenv-expand (value lookup)
  "Replace ${NAME} and ${NAME:-DEFAULT} in VALUE: LOOKUP's value for NAME,
   else DEFAULT, else \"\". A bare $NAME is left alone (python-dotenv)."
  (with-output-to-string (out)
    (let ((i 0)
          (n (length value)))
      (loop while (< i n)
            do (let ((close (and (char= #\$ (char value i))
                                 (< (1+ i) n)
                                 (char= #\{ (char value (1+ i)))
                                 (position #\} value :start (+ i 2)))))
                 (if close
                     (let* ((body (subseq value (+ i 2) close))
                            (sep (search ":-" body))
                            (name (if sep (subseq body 0 sep) body)))
                       (if (find #\: name)
                           (progn (write-char #\$ out) (incf i))
                           (progn
                             (write-string (or (funcall lookup name)
                                               (and sep (subseq body (+ sep 2)))
                                               "")
                                           out)
                             (setf i (1+ close)))))
                     (progn (write-char (char value i) out) (incf i))))))))

(defun parse-dotenv (text &key (environ nil environ-p) (expand t) (override t) path)
  "Parse .env TEXT into ((NAME . VALUE) ...), in first-occurrence order with
   the last value of a repeated name. VALUE is NIL for a bare NAME line.
   EXPAND (default T): ${NAME} / ${NAME:-DEFAULT} in unquoted and
   double-quoted values, looked up in the names parsed so far and in ENVIRON
   (an alist; default the process environment). OVERRIDE T (default, as
   python-dotenv's dotenv_values): earlier names win over ENVIRON; NIL (as
   load_dotenv): ENVIRON wins. PATH only names the source in warnings."
  (let ((values '()))
    (flet ((env (name)
             (if environ-p
                 (cdr (assoc name environ :test #'string=))
                 (uiop:getenv name)))
           (file (name)
             (cdr (assoc name values :test #'string=))))
      (dolist (statement (%dotenv-statements text path))
        (destructuring-bind (name value kind) statement
          (let ((value (if (and expand value (not (eq kind :single)))
                           (%dotenv-expand value
                                           (lambda (ref)
                                             (if override
                                                 (or (file ref) (env ref))
                                                 (or (env ref) (file ref)))))
                           value))
                (cell (assoc name values :test #'string=)))
            (if cell
                (setf (cdr cell) value)
                (push (cons name value) values))))))
    (nreverse values)))

(defun %read-dotenv-file (path if-does-not-exist)
  "Text of the .env file PATH (UTF-8, BOM dropped). A missing file signals
   CONFIG-FILE-ERROR with a CONTINUE restart (=> NIL) when IF-DOES-NOT-EXIST
   is :ERROR; NIL returns NIL."
  (let ((file (probe-file path)))
    (cond
      (file
       (let ((text (uiop:read-file-string file :external-format :utf-8)))
         (if (and (plusp (length text)) (char= (char text 0) (code-char #xFEFF)))
             (subseq text 1)
             text)))
      ((null if-does-not-exist) nil)
      (t
       (restart-case
           (error 'config-file-error
                  :message (format nil "missing .env file ~a" path)
                  :path (namestring path))
         (continue ()
           :report "Skip the missing .env file"
           nil))))))

(defun read-dotenv (path &rest keys &key environ expand override (if-does-not-exist :error))
  "PARSE-DOTENV of the file PATH; NIL when it is missing and
   IF-DOES-NOT-EXIST is NIL (or CONTINUE was invoked)."
  (declare (ignore environ expand override))
  (let ((text (%read-dotenv-file path if-does-not-exist)))
    (when text
      (let ((keys (copy-list keys)))
        (remf keys :if-does-not-exist)
        (apply #'parse-dotenv text :path (namestring path) keys)))))

(defun load-dotenv (&optional (path #p".env") &key override (expand t)
                                                    (if-does-not-exist :error))
  "Set the process environment from the .env file PATH, like python-dotenv's
   load_dotenv. A variable that is already set (even to \"\") is kept unless
   OVERRIDE; ${NAME} expansion then also prefers the environment. Bare NAME
   lines set nothing. Returns the ((NAME . VALUE) ...) that were set."
  (let ((set '()))
    (dolist (pair (read-dotenv path :expand expand :override override
                                    :if-does-not-exist if-does-not-exist))
      (destructuring-bind (name . value) pair
        (when (and value (or override (null (uiop:getenv name))))
          (setf (uiop:getenv name) value)
          (push pair set))))
    (nreverse set)))
