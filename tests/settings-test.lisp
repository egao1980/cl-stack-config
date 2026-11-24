(in-package #:cl-stack-config/tests)

;;; Issue #8 part 2: typed settings (pydantic-settings BaseSettings) through
;;; schema-protocol: LOAD-INTO.

(stack-schema:defschema %db-settings ()
  (host string)
  (port (integer 1 65535))
  (:key-style :snake))

(stack-schema:defschema %app-settings ()
  (verbose boolean :default nil)
  (database %db-settings)
  (max-connections (integer 1) :default 4)
  (tags (list string) :default nil)
  (:key-style :snake)
  (:extra :ignore))

(defun %settings-files ()
  (values (%write-tmp (format nil "tags = [\"a\", \"b\"]~%[database]~%host = \"file-host\"~%port = 5432~%")
                      "toml")
          (%write-tmp (format nil "APP_VERBOSE=true~%APP_DATABASE__PORT=1111~%") "env")))

(deftest settings-load-into-layers-and-coercion
  "File < .env < environment, env strings coerced to the slot types,
   defaults for what no layer sets."
  (multiple-value-bind (toml dotenv) (%settings-files)
    (unwind-protect
         (let* ((c (cfg:load-config toml :prefix "APP" :dotenv dotenv
                                         :environ '(("APP_DATABASE__PORT" . "6543"))))
                (s (cfg:load-into '%app-settings c)))
           (ok (typep s '%app-settings))
           (ok (eq t (slot-value s 'verbose)) "\"true\" from .env")
           (ok (equal "file-host" (slot-value (slot-value s 'database) 'host)))
           (ok (= 6543 (slot-value (slot-value s 'database) 'port)) "env beats .env")
           (ok (= 4 (slot-value s 'max-connections)) "default")
           (ok (equal '("a" "b") (slot-value s 'tags)))
           (let ((db (cfg:load-into '%db-settings c :path "database")))
             (ok (typep db '%db-settings) ":path sub-table"))
           (cfg:with-config (c)
             (ok (= 6543 (slot-value (slot-value (cfg:load-into '%app-settings nil) 'database)
                                     'port))
                 "NIL means *config*")))
      (uiop:delete-file-if-exists toml)
      (uiop:delete-file-if-exists dotenv))))

(deftest settings-load-into-reports-every-field
  (multiple-value-bind (toml dotenv) (%settings-files)
    (unwind-protect
         (let* ((c (cfg:load-config toml :prefix "APP" :dotenv dotenv
                                         :environ '(("APP_DATABASE__PORT" . "not-a-port")
                                                    ("APP_VERBOSE" . "maybe")
                                                    ("APP_MAX_CONNECTIONS" . "0"))))
                (e (handler-case (progn (cfg:load-into '%app-settings c) nil)
                     (cfg:config-validation-error (e) e))))
           (ok e "config-validation-error")
           (when e
             (ok (typep e 'cfg:config-type-error))
             (ok (typep e 'stack-schema:schema-validation-error))
             (let ((paths (mapcar (lambda (i)
                                    (format nil "~{~a~^.~}" (stack-schema:schema-issue-path i)))
                                  (stack-schema:schema-validation-error-issues e))))
               (ok (every (lambda (p) (member p paths :test #'string=))
                          '("verbose" "database.port" "max_connections"))
                   (format nil "all three fields reported: ~s" paths)))
             (ok (search "database.port" (princ-to-string e)) (princ-to-string e))))
      (uiop:delete-file-if-exists toml)
      (uiop:delete-file-if-exists dotenv))))

(deftest settings-load-into-errors
  (multiple-value-bind (toml dotenv) (%settings-files)
    (unwind-protect
         (let ((c (cfg:load-config toml :prefix "APP" :env nil)))
           (ok (signals (cfg:load-into '%db-settings c :path "tags") 'cfg:config-type-error)
               "PATH must name a table")
           (ok (signals (cfg:load-into '%db-settings c :path "nope") 'cfg:config-missing-error))
           (ok (signals (cfg:load-into '%app-settings c :coerce nil) 'cfg:config-validation-error)
               ":coerce nil keeps strings strings")
           (ok (signals (cfg:load-into '%db-settings
                                       (cfg:load-config toml :prefix "APP" :env nil
                                                             :overrides '(("database.extra" . "x")))
                                       :path "database")
                        'cfg:config-validation-error)
               "unknown keys follow the schema's :extra policy (default :forbid)"))
      (uiop:delete-file-if-exists toml)
      (uiop:delete-file-if-exists dotenv))))
