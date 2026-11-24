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
- The grammar is python-dotenv's own: a line ends at LF, CR or CRLF, and a blank is any whitespace of Python's `\s` (tab, VT, FF, NBSP, U+2000-U+200A, U+3000, ...), so `A=x\u00a0# c` is `x`, `A\u00a0=1` names `A` and trailing Unicode blanks are trimmed. A bare `NAME` line defines `NAME`: `${NAME:-d}` after it is `""`, as in python-dotenv.
- A file is read like Python's text mode: CRLF and lone CR line ends become LF, so a multi-line quoted value (a PEM key) from a Windows checkout has no CR. `parse-dotenv` takes the string as it is, like `dotenv_values(stream=...)`: line ends are the same, but a CR inside a quoted value stays.
- `load-dotenv` keeps variables that are already set (on POSIX even to `""`; Windows has no empty variables) unless `:override t`. It returns the `(name . value)` pairs it set.
- A statement that does not parse signals `dotenv-syntax-warning` (with `dotenv-syntax-warning-line`) and is skipped, as python-dotenv logs and skips. `muffle-warning` silences it; a handler can turn it into an error.
- A missing file signals `config-file-error` with a `continue` restart that skips it. `load-dotenv` / `read-dotenv` take `:if-does-not-exist nil` to return NIL instead, as `load_dotenv()` does.
- A file that is not UTF-8 signals `config-parse-error` naming the path (python-dotenv: `UnicodeDecodeError`); one that cannot be read signals `config-file-error`.

Deliberate differences from python-dotenv 1.1:

| Line | python-dotenv | here |
|------|---------------|------|
| `B='${A}'` | expands `${A}` | literal `${A}`, as in sh and docker compose |
| `C= # note` | `"# note"` | `""` (the `#` follows a blank, so it starts a comment) |
| `P="C:\\temp\\"` then a line with a quote | reads `\\"` as `\` and an escaped quote, so the value runs on to the next quote in the file; both lines are dropped | `C:\temp\`: `\\` is one backslash, the quote closes the value |
| a file starting with a UTF-8 BOM | the BOM is part of the first name (`"\ufeffNAME"`) | dropped |

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
