# elixir-ts-extras

Extra utilities for Elixir development with `elixir-ts-mode`.

## Features

- **Smart test running** with context detection (test/describe/file)
- **Transient menus** for test flags and mix commands
- **Custom compilation mode** with ANSI color support
- **Error regexp matching** for ExUnit output

## Requirements

- Emacs 30.1+
- `elixir-ts-mode` (built-in to Emacs 30.1)

## Installation

### From source

```elisp
(use-package elixir-ts-extras
  :load-path "/path/to/elixir-ts-extras"
  :after elixir-ts-mode
  :bind (:map elixir-ts-mode-map
              ("C-c , t" . elixir-ts-extras-test-menu)
              ("C-c , s" . elixir-ts-extras-test)
              ("C-c , v" . elixir-ts-extras-test-file)
              ("C-c , a" . elixir-ts-extras-test-all)
              ("C-c , r" . elixir-ts-extras-test-rerun)
              ("C-c , k" . elixir-ts-extras-test-stop)
              ("C-c , x" . elixir-ts-extras-mix-menu)))
```

## Commands

| Command                        | Description                |
|--------------------------------|----------------------------|
| `elixir-ts-extras-test-menu`   | Open test menu             |
| `elixir-ts-extras-test`        | Run test at point          |
| `elixir-ts-extras-test-file`   | Run tests in current file  |
| `elixir-ts-extras-test-all`    | Run all project tests      |
| `elixir-ts-extras-test-rerun`  | Rerun last test            |
| `elixir-ts-extras-test-stop`   | Stop running test          |
| `elixir-ts-extras-mix-menu`    | Open mix command menu      |

## Test Menu

The test menu (`elixir-ts-extras-test-menu`) provides:

- **Filters**: `--failed`, `--stale`
- **Flags**: `--trace`
- **Value flags**: `--slowest-modules`, `--repeat-until-failure`, `--seed`

Use `C-u` prefix with any test command to ignore transient flags.

## Mix Menu

The mix menu (`elixir-ts-extras-mix-menu`) provides shortcuts for common mix commands:

- `s` - `mix phx.server`
- `S` - `mix setup`
- `e` - `mix ecto.reset`
- `x` - Run any mix command with completion
- `E` - Set `MIX_ENV`

## Customization

```elisp
;; Scroll compilation output (default: t)
(setq elixir-ts-extras-compilation-scroll-output t)

;; Default MIX_ENV for mix commands (default: nil)
(setq elixir-ts-extras-mix-env "dev")
```

## License

GPL-3.0-or-later
