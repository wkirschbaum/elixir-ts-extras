# elixir-ts-extras

Extra utilities for Elixir development with `elixir-ts-mode`.

> **Note**: This package is experimental. The API may change without notice.

This package is designed exclusively for `elixir-ts-mode` and requires Emacs 30.1+ with tree-sitter support. It does not work with the legacy `elixir-mode`.

## Features

- **Smart test running** with context detection (test/describe/file)
- **Transient menus** for test flags and mix commands
- **Mix help at point** (context-aware documentation lookup)
- **Custom compilation mode** with ANSI color support
- **Error regexp matching** for ExUnit output

## Requirements

- Emacs 30.1+ compiled with tree-sitter support
- `elixir-ts-mode` (built-in to Emacs 30.1)
- `transient` (built-in to Emacs 29+)
- Elixir tree-sitter grammar installed

## Installation

```elisp
(use-package elixir-ts-extras
  :vc (:url "https://github.com/wkirschbaum/elixir-ts-extras")
  :after elixir-ts-mode
  :custom
  (elixir-ts-extras-compilation-scroll-output t)
  :bind (;; Global bindings (work from any buffer in an Elixir project)
         ("C-c , t" . elixir-ts-extras-test-menu)
         ("C-c , s" . elixir-ts-extras-test)
         ("C-c , v" . elixir-ts-extras-test-file)
         ("C-c , a" . elixir-ts-extras-test-all)
         ("C-c , r" . elixir-ts-extras-test-rerun)
         ("C-c , k" . elixir-ts-extras-test-stop)
         ("C-c , x" . elixir-ts-extras-mix-menu)
         ;; Mode-specific bindings
         :map elixir-ts-mode-map
         ("C-c , j" . elixir-ts-extras-test-jump)
         ("C-c , h" . elixir-ts-extras-mix-help-at-point)))
```

## Commands

| Command                           | Description                |
|-----------------------------------|----------------------------|
| `elixir-ts-extras-test-menu`      | Open test menu             |
| `elixir-ts-extras-test`           | Run test at point          |
| `elixir-ts-extras-test-file`      | Run tests in current file  |
| `elixir-ts-extras-test-all`       | Run all project tests      |
| `elixir-ts-extras-test-rerun`     | Rerun last test            |
| `elixir-ts-extras-test-stop`      | Stop running test          |
| `elixir-ts-extras-test-jump`      | Jump between source and test file |
| `elixir-ts-extras-mix-menu`       | Open mix command menu      |
| `elixir-ts-extras-mix-help-at-point` | Show mix help for symbol at point |

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

## Mix Help at Point

The `elixir-ts-extras-mix-help-at-point` command (`C-c , h`) shows mix help for the symbol at point:

- On a function call (`Enum.map`): shows `mix help Enum.map`
- On a module (`Enum`): shows `mix help Enum`
- On an atom (`:my_app`): shows `mix help app:my_app`
- On a Mix task module (`Mix.Tasks.Compile`): shows `mix help compile`

## License

GPL-3.0-or-later
