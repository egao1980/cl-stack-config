# cl-stack-config

Env + **TOML** / **INI** config facade for [cl-stack](https://github.com/egao1980/cl-stack).

| Piece | Choice |
|-------|--------|
| File format | TOML via [`toml-protocol`](https://github.com/egao1980/toml-protocol) / `toml-backend-tomlet`; INI via `parse-ini` (`:format :ini` / `:auto`) |
| Precedence | file < env < explicit overrides |
| Env nesting | `APP_DATABASE__HOST` → `database.host` |

Brief: [`cl-stack/docs/capabilities/config.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/config.md).

```lisp
(asdf:load-system "cl-stack-config")
(defvar *cfg* (stack-config:load #p"config.toml" :prefix "APP"))
(stack-config:get-integer *cfg* "database.port")
```

## License

MIT — see [LICENSE](LICENSE).
