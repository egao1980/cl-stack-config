# cl-stack-config

Env + `.env` + **TOML** / **INI** config facade for [cl-stack](https://github.com/egao1980/cl-stack), with typed settings through [`schema-protocol`](https://github.com/egao1980/schema-protocol).

| Piece | Choice |
|-------|--------|
| File format | TOML via [`toml-protocol`](https://github.com/egao1980/toml-protocol) / `toml-backend-tomlet`; INI via `parse-ini` (`:format :ini` / `:auto`) |
| `.env` | `parse-dotenv` / `read-dotenv` / `load-dotenv` (python-dotenv syntax); `load-config :dotenv` layer |
| Precedence | file < `.env` < env < explicit overrides |
| Env nesting | `APP_DATABASE__HOST` → `database.host` |
| Typed settings | `load-into` a `defschema` (pydantic-settings `BaseSettings`) |

Brief: [`cl-stack/docs/capabilities/config.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/config.md).

```lisp
(asdf:load-system "cl-stack-config")
(defvar *cfg* (stack-config:load #p"config.toml" :prefix "APP"))
(stack-config:get-integer *cfg* "database.port")
```

## `.env` files

python-dotenv's syntax:

```sh
# comment
export APP_HOST=db.internal          # "export " is optional
APP_PORT = 5432                      # blanks around = are fine; # after a blank starts a comment
APP_URL=postgres://${APP_HOST}:${APP_PORT:-5432}/app
APP_GREETING="hello\nworld"          # double quotes: \n \t \r \" \\ \' \a \b \f \v; may span lines
APP_RAW='no ${expansion} \n here'    # single quotes: literal, only \' and \\
APP_FLAG                             # no "=": parsed with value NIL, never set
```

```lisp
(stack-config:parse-dotenv text)        ; => (("APP_HOST" . "db.internal") ...), file order
(stack-config:read-dotenv #p".env")     ; the same for a file (UTF-8, BOM dropped, CRLF / CR read as LF)
(stack-config:load-dotenv)              ; load_dotenv(): set unset variables from ./.env
(stack-config:load-dotenv #p".env" :override t)

;; Or as a layer, without touching the process environment:
(stack-config:load-config #p"config.toml" :prefix "APP" :dotenv #p".env")
```

- `${NAME}` and `${NAME:-default}` expand in unquoted and double-quoted values; `$NAME` does not. A missing name gives the default, else `""`. `parse-dotenv` (like `dotenv_values`) looks names up in the file first, then the environment (`:environ` alist, default the process). `load-dotenv` without `:override` and the `load-config` layer look in the environment first (like `load_dotenv`). `:expand nil` turns expansion off.
- A repeated name keeps its first position and its last value.
- A file is read like Python's text mode: CRLF and lone CR line ends become LF, so a multi-line quoted value (a PEM key) from a Windows checkout has no CR. `parse-dotenv` takes the string as it is, like `dotenv_values(stream=...)`.
- `load-dotenv` keeps variables that are already set (on POSIX even to `""`; Windows has no empty variables) unless `:override t`. It returns the `(name . value)` pairs it set.
- A statement that does not parse signals `dotenv-syntax-warning` (with `dotenv-syntax-warning-line`) and is skipped, as python-dotenv logs and skips. `muffle-warning` silences it; a handler can turn it into an error.
- A missing file signals `config-file-error` with a `continue` restart that skips it. `load-dotenv` / `read-dotenv` take `:if-does-not-exist nil` to return NIL instead, as `load_dotenv()` does.

Two deliberate differences from python-dotenv 1.1:

| Line | python-dotenv | here |
|------|---------------|------|
| `B='${A}'` | expands `${A}` | literal `${A}`, as in sh and docker compose |
| `C= # note` | `"# note"` | `""` (the `#` follows a blank, so it starts a comment) |

## Typed settings

`load-into` fills a [`schema-protocol`](https://github.com/egao1980/schema-protocol) schema from a config. Env strings are coerced to the slot types (`"6543"` → 6543, `"true"` → T), defaults apply, and a mismatch reports every field at once:

```lisp
(stack-schema:defschema db-settings ()
  (host string)
  (port (integer 1 65535))
  (:key-style :snake))

(stack-schema:defschema app-settings ()
  (verbose boolean :default nil)
  (database db-settings)
  (max-connections (integer 1) :default 4)
  (:key-style :snake)
  (:extra :ignore))

(stack-config:load-into 'app-settings
                        (stack-config:load-config #p"config.toml" :prefix "APP" :dotenv #p".env"))
;; APP_MAX_CONNECTIONS=0 APP_DATABASE__PORT=x =>
;; CONFIG-VALIDATION-ERROR: config error: settings do not match APP-SETTINGS
;;   database.port: …
;;   max_connections: …
```

- Env names become snake_case keys (`APP_MAX_CONNECTIONS` → `max_connections`). Give settings schemas `(:key-style :snake)` or `:aliases`.
- `:path "database"` fills from a sub-table. A NIL config means `*config*`. `:coerce nil` turns coercion off; `:extra` overrides the schema's policy for unknown keys (`:forbid` by default; env overlays usually want `:ignore`).
- `config-validation-error` is both a `config-type-error` and a `stack-schema:schema-validation-error`, so `schema-validation-error-issues` lists the fields.

## License

MIT — see [LICENSE](LICENSE).
