(in-package #:cl-stack-config)

;;; Typed settings (pydantic-settings' BaseSettings), cl-stack-config#8:
;;; a schema-protocol DEFSCHEMA filled from a CONFIG, whose values came from
;;; the file, a .env layer and the environment. schema-protocol does the
;;; coercion ("6543" => 6543, "true" => T) and collects every field issue.
;;; Env names become snake_case keys (APP_MAX_CONNECTIONS => max_connections),
;;; so settings schemas use (:key-style :snake) or :aliases.

(defun load-into (schema cfg &key path (coerce t) (extra nil extra-p))
  "Instance of the schema-protocol SCHEMA (a name or class) filled from CFG
   (NIL: *CONFIG*), or from its sub-table at PATH. COERCE (default T) turns
   env strings into the slot types; EXTRA overrides the schema's policy for
   unknown keys. Values that do not fit signal CONFIG-VALIDATION-ERROR with
   every field issue at once."
  (let* ((c (%cfg cfg))
         (data (if path (get c path) (config-data c))))
    (unless (hash-table-p data)
      (error 'config-type-error
             :message (format nil "not a table: ~s" path)
             :path path))
    (handler-case
        (apply #'schema-protocol:parse schema data :coerce coerce
               (and extra-p (list :extra extra)))
      (schema-protocol:schema-validation-error (e)
        (error 'config-validation-error
               :message (format nil "settings do not match ~s" schema)
               :path path
               :issues (schema-protocol:schema-validation-error-issues e))))))
