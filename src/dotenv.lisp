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
;;; The grammar is python-dotenv 1.1's (dotenv/parser.py): a line ends at
;;; LF, CR or CRLF, and a blank is any whitespace of Python's \s, NBSP and
;;; the other Unicode blanks included. Quoted values may span lines. A
;;; statement that does not parse signals DOTENV-SYNTAX-WARNING and is
;;; skipped, as python-dotenv logs and skips. Deliberate differences from
;;; python-dotenv 1.1 (README): single-quoted values are not expanded (as in
;;; sh and docker compose); in "NAME= # note" the # starts a comment
;;; (python-dotenv: "# note"); \\" ends a quoted value (python-dotenv reads
;;; an escaped quote and runs on to the next quote); a file's UTF-8 BOM is
;;; dropped (python-dotenv keeps it in the first name).

(define-condition dotenv-syntax-warning (warning)
  ((path :initarg :path :reader dotenv-syntax-warning-path :initform nil)
   (line :initarg :line :reader dotenv-syntax-warning-line :initform nil))
  (:report (lambda (c s)
             (format s "could not parse .env statement~@[ in ~a~]~@[ at line ~d~]; skipped"
                     (dotenv-syntax-warning-path c)
                     (dotenv-syntax-warning-line c))))
  (:documentation "A .env statement that does not parse. The statement is
skipped; MUFFLE-WARNING silences it, a handler may also signal an error."))

(defun %dotenv-space-p (c)
  "C is whitespace in Python's sense (re's \\s, str.isspace()), which
   python-dotenv's grammar uses: ASCII TAB LF VT FF CR, the separators
   #x1C-#x1F, SPACE, NEL, NBSP and the other Unicode blanks (U+1680,
   U+2000-U+200A, U+2028, U+2029, U+202F, U+205F, U+3000)."
  (and c
       (let ((code (char-code c)))
         (or (<= 9 code 13)
             (<= #x1c code #x20)
             (= code #x85)
             (= code #xa0)
             (= code #x1680)
             (<= #x2000 code #x200a)
             (= code #x2028)
             (= code #x2029)
             (= code #x202f)
             (= code #x205f)
             (= code #x3000)))))

(defun %dotenv-eol-p (c)
  "C ends a line in python-dotenv's grammar: LF or CR (CRLF is one end)."
  (and c (member (char-code c) '(10 13)) t))

(defun %dotenv-blank-p (c)
  "C is whitespace that does not end a line: python-dotenv's [^\\S\\r\\n]."
  (and (%dotenv-space-p c) (not (%dotenv-eol-p c))))

(defun %dotenv-unquoted-value (part spaced)
  "python-dotenv's parse_unquoted_value of PART (the rest of the line after
   = and its blanks): cut at the first # that follows whitespace
   (re.sub(r\"\\s+#.*\", \"\", part)), then trailing whitespace stripped
   (rstrip()). SPACED: blanks came between = and PART; then a # at its start
   is a comment too, so NAME= # note is \"\" (python-dotenv: \"# note\")."
  (declare (type string part))
  (when (and spaced (plusp (length part)) (char= #\# (char part 0)))
    (return-from %dotenv-unquoted-value ""))
  (let ((end (length part))
        (run nil))
    (loop for k from 0 below (length part)
          for c = (char part k)
          do (cond ((%dotenv-space-p c) (unless run (setf run k)))
                   ((and run (char= c #\#)) (setf end run) (return))
                   (t (setf run nil))))
    (let ((last (position-if-not #'%dotenv-space-p part :end end :from-end t)))
      (if last (subseq part 0 (1+ last)) ""))))

(defun %dotenv-closing-quote (text open)
  "Index in TEXT of the quote that closes the value opened at OPEN, or NIL.
   A backslash pairs with the next character, so \\\" does not close it and
   \\\\\" does (python-dotenv's regex takes that \\\" as an escaped quote and
   reads on to the next quote: the documented difference). With no closing
   quote before the end, the last quote after OPEN closes it, where
   python-dotenv's regex backtracks to, so A=\"x\\\" is x\\."
  (let ((quote (char text open))
        (n (length text))
        (j (1+ open)))
    (loop while (< j n)
          do (let ((c (char text j)))
               (cond ((and (char= c #\\) (< (1+ j) n)) (incf j 2))
                     ((char= c quote) (return-from %dotenv-closing-quote j))
                     (t (incf j)))))
    (position quote text :start (1+ open) :from-end t)))

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
  "Statements of TEXT as (NAME VALUE KIND) in file order, by python-dotenv
   1.1's grammar (dotenv/parser.py): a line ends at LF, CR or CRLF, and
   whitespace (%DOTENV-SPACE-P) is Python's, Unicode blanks included. VALUE
   is decoded but not expanded (NIL for a bare NAME); KIND is :single,
   :double, :unquoted or NIL. A statement that does not parse warns and is
   skipped to the end of the line where it failed."
  (let ((i 0)
        (line 1)
        (n (length text))
        (out '()))
    (labels ((peek ()
               (and (< i n) (char text i)))
             (advance ()
               ;; Lines as python-dotenv counts them: CRLF, LF and CR each end one.
               (let ((c (char text i)))
                 (incf i)
                 (when (case (char-code c)
                         (10 t)
                         (13 (let ((next (peek)))
                               (not (and next (= 10 (char-code next))))))
                         (t nil))
                   (incf line))))
             (skip-while (predicate)
               (loop while (funcall predicate (peek)) do (advance)))
             (skip-to-eol ()
               (skip-while (lambda (c) (and c (not (%dotenv-eol-p c))))))
             (skip-line ()
               "_rest_of_line: to the end of this line and past it."
               (skip-to-eol)
               (when (peek) (advance)))
             (end-of-line ()
               "_comment and _end_of_line: blanks, an optional # comment, then a line
                end or the end of TEXT, consumed: T; else NIL."
               (skip-while #'%dotenv-blank-p)
               (when (eql #\# (peek))
                 (skip-to-eol))
               (let ((c (peek)))
                 (cond ((null c) t)
                       ((%dotenv-eol-p c)
                        (advance)
                        (let ((next (peek)))
                          (when (and next (= 13 (char-code c)) (= 10 (char-code next)))
                            (advance)))
                        t)
                       (t nil))))
             (read-key ()
               "_single_quoted_key / _unquoted_key: the key (consumed), :COMMENT at a
                #, or NIL when there is none."
               (let ((c (peek)))
                 (cond ((eql c #\#) :comment)
                       ((eql c #\')
                        (let ((close (position #\' text :start (1+ i))))
                          (when (and close (> close (1+ i)))
                            (prog1 (subseq text (1+ i) close)
                              (loop while (<= i close) do (advance))))))
                       (t
                        (let ((start i))
                          (skip-while (lambda (c)
                                        (and c (char/= c #\=) (char/= c #\#)
                                             (not (%dotenv-space-p c)))))
                          (and (> i start) (subseq text start i)))))))
             (read-value ()
               "After = and its blanks: (VALUE KIND), or NIL for an unterminated quote
                (nothing consumed)."
               (let ((c (peek)))
                 (cond ((or (null c) (%dotenv-eol-p c))
                        (list "" :unquoted))
                       ((or (char= c #\') (char= c #\"))
                        (let ((close (%dotenv-closing-quote text i))
                              (kind (if (char= c #\') :single :double)))
                          (when close
                            (prog1 (list (%dotenv-unescape (subseq text (1+ i) close) kind)
                                         kind)
                              (loop while (<= i close) do (advance))))))
                       (t
                        (let ((start i))
                          (skip-to-eol)
                          (list (subseq text start i) :unquoted)))))))
      (loop
        (skip-while #'%dotenv-space-p)
        (unless (peek)
          (return))
        (let ((start-line line))
          (block statement
            (flet ((bad ()
                     (warn 'dotenv-syntax-warning :path path :line start-line)
                     (skip-line)
                     (return-from statement)))
              (when (and (< (+ i 6) n)
                         (string= "export" text :start2 i :end2 (+ i 6))
                         (%dotenv-blank-p (char text (+ i 6))))
                (incf i 6)
                (skip-while #'%dotenv-blank-p))
              (let ((name (read-key)))
                (cond
                 ((null name) (bad))
                 ((eq name :comment) (end-of-line))
                 (t
                  (skip-while #'%dotenv-blank-p)
                  (let ((statement
                         (if (eql #\= (peek))
                             (progn
                               (advance)
                               (let ((spaced (%dotenv-blank-p (peek))))
                                 (skip-while #'%dotenv-blank-p)
                                 (destructuring-bind (&optional value kind) (read-value)
                                   (unless kind (bad))
                                   (list name
                                         (if (eq kind :unquoted)
                                             (%dotenv-unquoted-value value spaced)
                                             value)
                                         kind))))
                             (list name nil nil))))
                    (if (end-of-line)
                        (push statement out)
                        (bad)))))))))))
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
   load_dotenv): ENVIRON wins. A name parsed so far is defined even when it
   is a bare NAME: it expands to \"\", not to its default (python-dotenv's
   None). PATH only names the source in warnings."
  (let ((values '()))
    (flet ((env (name)
             (if environ-p
                 (cdr (assoc name environ :test #'string=))
                 (uiop:getenv name)))
           (file (name)
             (let ((cell (assoc name values :test #'string=)))
               (and cell (or (cdr cell) "")))))
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

(defun %universal-newlines (text)
  "TEXT with CRLF and lone CR line ends turned into LF, as Python's text
   mode reads a file: python-dotenv opens a .env path that way, so a CRLF
   checkout gives the same values (a multi-line quoted value has no CR) and
   a CR-only file has one statement per line. TEXT itself when it has no
   CR."
  (declare (type string text))
  (if (not (find #\Return text))
      text
      (let ((n (length text))
            (i 0))
        (with-output-to-string (out)
          (loop while (< i n)
                do (let ((c (char text i)))
                     (incf i)
                     (cond ((char/= c #\Return) (write-char c out))
                           (t (write-char #\Newline out)
                              (when (and (< i n) (char= (char text i) #\Newline))
                                (incf i))))))))))

(defun %read-dotenv-file (path if-does-not-exist)
  "Text of the .env file PATH (UTF-8, BOM dropped, line ends read as by
   %UNIVERSAL-NEWLINES). A missing file signals CONFIG-FILE-ERROR with a
   CONTINUE restart (=> NIL) when IF-DOES-NOT-EXIST is :ERROR; NIL returns
   NIL. A file that cannot be read signals CONFIG-FILE-ERROR, and one that is
   not UTF-8 (python-dotenv: UnicodeDecodeError) CONFIG-PARSE-ERROR, both
   naming PATH, so the implementation's stream condition does not leak."
  (let ((file (probe-file path)))
    (cond
     (file
      (let ((text (handler-case (uiop:read-file-string file :external-format :utf-8)
                    (file-error (e)
                      (error 'config-file-error
                             :message (format nil "cannot read .env file: ~a" e)
                             :path (namestring path)))
                    (error (e)
                      (error 'config-parse-error
                             :message (format nil ".env file is not UTF-8: ~a" e)
                             :path (namestring path))))))
        (%universal-newlines
         (if (and (plusp (length text)) (char= (char text 0) (code-char #xFEFF)))
             (subseq text 1)
             text))))
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
   load_dotenv. A variable that is already set (on POSIX even to \"\"; on
   Windows an empty variable does not exist) is kept unless OVERRIDE; without
   OVERRIDE, ${NAME} expansion also prefers the environment. Bare NAME lines
   set nothing. Returns the ((NAME . VALUE) ...) that were set."
  (let ((set '()))
    (dolist (pair (read-dotenv path :expand expand :override override
                                    :if-does-not-exist if-does-not-exist))
      (destructuring-bind (name . value) pair
        (when (and value (or override (null (uiop:getenv name))))
          (setf (uiop:getenv name) value)
          (push pair set))))
    (nreverse set)))
