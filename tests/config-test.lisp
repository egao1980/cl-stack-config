(in-package #:cl-stack-config/tests)

(defun %write-tmp-toml (text)
  (uiop:with-temporary-file (:pathname path :prefix "cl-stack-config-" :type "toml"
                             :keep t)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string text out))
    path))

(deftest load-and-get
  (let* ((path (%write-tmp-toml "
features = [\"auth\", \"cache\"]

[database]
host = \"localhost\"
port = 5432
enabled = true
"))
         (c (cfg:load-config path :env nil)))
    (ok (string= "localhost" (cfg:get c "database.host")))
    (ok (= 5432 (cfg:get-integer c "database.port")))
    (ok (eq t (cfg:get-boolean c "database.enabled")))
    (ok (equal '("auth" "cache") (cfg:get-list c "features")))
    (ok (string= "localhost" (cfg:get (cfg:section c "database") "host")))
    (ok (signals (cfg:get c "missing") 'cfg:config-missing-error))
    (ok (string= "x" (cfg:get c "missing" :default "x")))))

(deftest env-overlay-wins-over-file
  (let* ((path (%write-tmp-toml "
[database]
host = \"file-host\"
port = 1
"))
         (c (cfg:load-config path :prefix "APP" :env t
                             :environ '(("APP_DATABASE__HOST" . "env-host")
                                        ("APP_DATABASE__PORT" . "99")))))
    (ok (string= "env-host" (cfg:get c "database.host")))
    (ok (= 99 (cfg:get-integer c "database.port")))))

(deftest overrides-win-over-env
  (let* ((path (%write-tmp-toml "debug = false
"))
         (c (cfg:load-config path :prefix "APP" :env t
                             :environ '(("APP_DEBUG" . "true"))
                             :overrides '(("debug" . nil)))))
    (ok (null (cfg:get-boolean c "debug")))))

(deftest with-config-dynamic
  (let* ((path (%write-tmp-toml "x = 1
"))
         (c (cfg:load-config path :env nil)))
    (cfg:with-config (c)
      (ok (= 1 (cfg:get nil "x"))))))
