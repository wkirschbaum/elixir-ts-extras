;;; elixir-ts-extras.el --- Extra utilities for elixir-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Wilhelm H Kirschbaum

;; Author: Wilhelm H Kirschbaum
;; Version: 0.1
;; URL: https://github.com/wkirschbaum/elixir-ts-extras
;; Package-Requires: ((emacs "30.1"))
;; Created: December 2025
;; Keywords: elixir languages tree-sitter

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides extra utilities for Elixir development with
;; `elixir-ts-mode', including:
;;
;; * Test running with smart context detection (test/describe/file)
;; * Transient menus for test flags and mix commands
;; * Custom compilation mode with ANSI color support
;; * Error regexp matching for ExUnit output

;;; Code:

(require 'treesit)
(require 'compile)
(require 'ansi-color)
(require 'project)
(require 'transient)

;;; Customization

(defgroup elixir-ts-extras nil
  "Extra utilities for `elixir-ts-mode'."
  :group 'elixir
  :prefix "elixir-ts-extras-")

(defcustom elixir-ts-extras-test-command "mix test"
  "Command to run Elixir tests."
  :type 'string
  :group 'elixir-ts-extras)

(defcustom elixir-ts-extras-compilation-scroll-output t
  "Whether to scroll compilation output automatically."
  :type 'boolean
  :group 'elixir-ts-extras)

;;; Internal Variables

(defvar elixir-ts-extras--mix-env nil
  "MIX_ENV value to use for mix run commands.
When nil, no MIX_ENV is set (uses mix default).")

(defvar elixir-ts-extras--compile-buffer-name nil
  "Display name for current compilation buffer.")

(defvar elixir-ts-extras--last-test-command nil
  "Last test command that was run (without flags).")

(defvar elixir-ts-extras-mix-history nil
  "History of mix commands.")

(defvar elixir-ts-extras--mix-tasks-cache nil
  "Cache of mix tasks per project.  Alist of (project-root . tasks).")

(defvar elixir-ts-extras-compilation--current-dep nil
  "Current dependency being compiled, used to resolve relative paths.")

;;; Compilation Buffer Naming

(defun elixir-ts-extras--compilation-buffer-name (_mode)
  "Generate compilation buffer name from mix command."
  (if elixir-ts-extras--compile-buffer-name
      (format "*elixir[%s]*" elixir-ts-extras--compile-buffer-name)
    "*elixir*"))

;;; Tree-sitter Helpers

(defun elixir-ts-extras--call-match-p (node name)
  "Return non-nil if NODE is a call with target NAME."
  (and node
       (equal (treesit-node-type node) "call")
       (when-let* ((target (treesit-node-child-by-field-name node "target")))
         (equal (treesit-node-text target) name))))

(defun elixir-ts-extras--find-enclosing-call (name)
  "Find enclosing call node with target NAME containing point.
Use end of current line to ensure calls starting on this line are found.
Return the node or nil if not found."
  (let ((node (treesit-node-on (line-end-position) (line-end-position))))
    (while (and node (not (elixir-ts-extras--call-match-p node name)))
      (setq node (treesit-node-parent node)))
    node))

(defun elixir-ts-extras--test-context ()
  "Determine the test context at point.
Return a cons cell (TYPE . LINE) where TYPE is one of:
  - `test' - inside a test block, LINE is the test line
  - `describe' - inside describe but not in test, LINE is describe line
  - `file' - outside any test structure, LINE is nil"
  (let ((test-node (elixir-ts-extras--find-enclosing-call "test"))
        (describe-node (elixir-ts-extras--find-enclosing-call "describe")))
    (cond
     (test-node
      (cons 'test (line-number-at-pos (treesit-node-start test-node))))
     (describe-node
      (cons 'describe (line-number-at-pos (treesit-node-start describe-node))))
     (t
      (cons 'file nil)))))

;;; MIX_ENV Management

;;;###autoload
(defun elixir-ts-extras-set-mix-env (env)
  "Set MIX_ENV for mix run commands to ENV."
  (interactive
   (list (completing-read
          (format "MIX_ENV%s: "
                  (if elixir-ts-extras--mix-env
                      (format " (current: %s)" elixir-ts-extras--mix-env)
                    ""))
          '("dev" "test" "prod") nil nil)))
  (setq elixir-ts-extras--mix-env (if (string-empty-p env) nil env))
  (message "Mix MIX_ENV %s"
           (if elixir-ts-extras--mix-env
               (format "set to '%s'" elixir-ts-extras--mix-env)
             "cleared")))

;;; Mix Task Completion

(defun elixir-ts-extras--get-mix-tasks ()
  "Get available mix tasks for current project, with caching."
  (when-let* ((project (project-current))
              (project-root (project-root project)))
    (let ((cached (assoc project-root elixir-ts-extras--mix-tasks-cache)))
      (if cached
          (cdr cached)
        (let* ((default-directory project-root)
               (tasks (split-string
                       (shell-command-to-string "mix help --names 2>/dev/null")
                       "\n" t)))
          (when tasks
            (push (cons project-root tasks) elixir-ts-extras--mix-tasks-cache))
          tasks)))))

(defun elixir-ts-extras--read-mix-command ()
  "Read a mix command with completion."
  (completing-read "Mix: " (elixir-ts-extras--get-mix-tasks)
                   nil nil nil 'elixir-ts-extras-mix-history))

;;; Mix Command Running

(defun elixir-ts-extras--run-mix (command)
  "Run mix COMMAND using `elixir-ts-extras--mix-env'."
  (let* ((default-directory (project-root (project-current t)))
         (full-command (if elixir-ts-extras--mix-env
                           (format "MIX_ENV=%s mix %s" elixir-ts-extras--mix-env command)
                         (concat "mix " command)))
         (elixir-ts-extras--compile-buffer-name (car (split-string command)))
         (compilation-buffer-name-function #'elixir-ts-extras--compilation-buffer-name))
    (compile full-command #'elixir-ts-extras-compilation-mode)))

;;;###autoload
(defun elixir-ts-extras-mix-run (command)
  "Run a mix COMMAND in the project root."
  (interactive (list (elixir-ts-extras--read-mix-command)))
  (elixir-ts-extras--run-mix command))

;;; Mix Menu

;;;###autoload (autoload 'elixir-ts-extras-mix-menu "elixir-ts-extras" nil t)
(transient-define-prefix elixir-ts-extras-mix-menu ()
  "Transient menu for running mix commands."
  [["Run"
    ("s" "server" (lambda () (interactive) (elixir-ts-extras--run-mix "phx.server")))
    ("S" "setup" (lambda () (interactive) (elixir-ts-extras--run-mix "setup")))
    ("x" "other..." elixir-ts-extras-mix-run)]
   ["Ecto"
    ("e" "ecto.reset" (lambda () (interactive) (elixir-ts-extras--run-mix "ecto.reset")))]
   ["Settings"
    ("E" elixir-ts-extras-set-mix-env
     :description (lambda ()
                    (if elixir-ts-extras--mix-env
                        (format "MIX_ENV: %s" elixir-ts-extras--mix-env)
                      "MIX_ENV"))
     :transient t)]])

;;; Test Running

(defun elixir-ts-extras--run-test-command (command &optional project-directory)
  "Run mix test COMMAND in PROJECT-DIRECTORY or current project root.
Always uses MIX_ENV=test to ensure consistent test environment."
  (let* ((default-directory (or project-directory
                                (project-root (project-current t))))
         (full-command (concat "MIX_ENV=test mix " command))
         (elixir-ts-extras--compile-buffer-name "test")
         (compilation-buffer-name-function #'elixir-ts-extras--compilation-buffer-name))
    (compile full-command #'elixir-ts-extras-compilation-mode)))

(defun elixir-ts-extras--test-flags (&optional ignore-flags)
  "Build test flags string from transient arguments.
If IGNORE-FLAGS is non-nil, return empty string."
  (if ignore-flags
      ""
    (string-join (transient-args 'elixir-ts-extras-test-menu) " ")))

(defun elixir-ts-extras--run-test (command &optional ignore-flags)
  "Run test COMMAND with current transient flags.
If IGNORE-FLAGS is non-nil, run without flags."
  (setq elixir-ts-extras--last-test-command command)
  (let ((flags (elixir-ts-extras--test-flags ignore-flags)))
    (elixir-ts-extras--run-test-command
     (if (string-empty-p flags)
         command
       (concat command " " flags)))))

;;;###autoload
(defun elixir-ts-extras-test-rerun (arg)
  "Rerun the last test command.
With prefix ARG, ignore transient flags."
  (interactive "P")
  (if elixir-ts-extras--last-test-command
      (elixir-ts-extras--run-test elixir-ts-extras--last-test-command arg)
    (user-error "No previous test command to rerun")))

;;;###autoload
(defun elixir-ts-extras-test (arg)
  "Run test at point with smart context detection.
Inside a test block: run that single test.
Inside a describe block (not in test): run all tests in describe.
Outside both: run all tests in the file.
With prefix ARG, ignore transient flags."
  (interactive "P")
  (let* ((context (elixir-ts-extras--test-context))
         (context-type (car context))
         (context-line (cdr context))
         (default-directory (project-root (project-current t)))
         (file-relative (when buffer-file-name
                          (file-relative-name buffer-file-name)))
         (command
          (pcase context-type
            ('test (format "test %s:%d" file-relative context-line))
            ('describe (format "test %s:%d" file-relative context-line))
            ('file (format "test %s" file-relative)))))
    (elixir-ts-extras--run-test command arg)))

;;;###autoload
(defun elixir-ts-extras-test-file (arg)
  "Run all tests in the current file.
With prefix ARG, ignore transient flags."
  (interactive "P")
  (let* ((default-directory (project-root (project-current t)))
         (file-relative (file-relative-name buffer-file-name)))
    (elixir-ts-extras--run-test (format "test %s" file-relative) arg)))

;;;###autoload
(defun elixir-ts-extras-test-all (arg)
  "Run all tests in the project.
With prefix ARG, ignore transient flags."
  (interactive "P")
  (elixir-ts-extras--run-test "test" arg))

;;;###autoload
(defun elixir-ts-extras-test-stop ()
  "Stop the currently running test.
Sends interrupt signal followed by \\='a\\=' to abort the Erlang break menu."
  (interactive)
  (when-let* ((buf (get-buffer "*elixir[test]*"))
              (proc (get-buffer-process buf)))
    (interrupt-process proc)
    (process-send-string proc "a\n")))

;;; Test Menu

;;;###autoload (autoload 'elixir-ts-extras-test-menu "elixir-ts-extras" nil t)
(transient-define-prefix elixir-ts-extras-test-menu ()
  "Transient menu for Elixir test commands."
  :incompatible '(("--failed" "--stale"))
  ["Filter"
   ("-f" "Failed (rerun failed tests)" "--failed")
   ("-s" "Stale (only changed modules)" "--stale")]
  ["Flags"
   ("-t" "Trace (detailed output)" "--trace")]
  ["Flags with values"
   ("-m" "Slowest modules" "--slowest-modules=" :reader transient-read-number-N+)
   ("-r" "Repeat until failure" "--repeat-until-failure=" :reader transient-read-number-N+)
   ("-S" "Seed" "--seed=" :reader transient-read-number-N0)]
  ["Run Tests (C-u to ignore flags)"
   ("s" "at point" elixir-ts-extras-test)
   ("v" "current file" elixir-ts-extras-test-file)
   ("a" "all project" elixir-ts-extras-test-all)
   ("r" elixir-ts-extras-test-rerun
    :description (lambda ()
                   (format "rerun: %s" elixir-ts-extras--last-test-command))
    :if (lambda () elixir-ts-extras--last-test-command))
   ("k" "stop test" elixir-ts-extras-test-stop)]
  ["Settings"
   ("C-x C-s" "Save flags as default" transient-save)
   ("C-x C-r" "Reset to saved" transient-reset)]
  (interactive)
  (transient-setup 'elixir-ts-extras-test-menu nil nil
                   :value (transient-get-value)))

;;; Compilation Mode

(defun elixir-ts-extras--colorize-compilation-buffer ()
  "Apply ANSI color codes in the compilation buffer."
  (when (eq major-mode 'elixir-ts-extras-compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(defun elixir-ts-extras--track-current-dep ()
  "Track the current dependency from ==> lines in compilation output."
  (save-excursion
    (goto-char compilation-filter-start)
    (while (re-search-forward "^==> \\([a-z_]+\\)$" nil t)
      (setq elixir-ts-extras-compilation--current-dep (match-string 1)))))

(defun elixir-ts-extras--resolve-dep-file ()
  "Resolve file path, checking deps directory if needed.
Return nil if file cannot be found."
  (let ((file (match-string 1)))
    (cond
     ((file-exists-p file) file)
     (elixir-ts-extras-compilation--current-dep
      (let ((dep-file (concat "deps/" elixir-ts-extras-compilation--current-dep "/" file)))
        (when (file-exists-p dep-file)
          dep-file)))
     (t nil))))

(defun elixir-ts-extras--resolve-c-dep-file ()
  "Resolve C file path, checking deps directory if needed.
Return nil if file cannot be found."
  (let ((file (match-string 1)))
    (cond
     ((file-exists-p file) file)
     (elixir-ts-extras-compilation--current-dep
      (let ((dep-file (concat "deps/" elixir-ts-extras-compilation--current-dep "/" file)))
        (when (file-exists-p dep-file)
          dep-file)))
     (t nil))))

(defvar elixir-ts-extras-compilation-error-regexp-alist-alist
  '((exunit-error
     "\\[error\\] \\([^[:space:]]+\\):\\([[:digit:]]+\\)" 1 2)
    (exunit-warning
     "warning: .*\n[[:space:]]*\\([^[:space:]]+\\.exs?\\):\\([[:digit:]]+\\)" 1 2 nil 1)
    (exunit-dep-warning
     "└─ \\([^:]+\\.exs?\\):\\([0-9]+\\):\\([0-9]+\\)"
     elixir-ts-extras--resolve-dep-file 2 3 1)
    (exunit-c-file
     "\\([^ \t\n]+\\.c\\)" elixir-ts-extras--resolve-c-dep-file nil nil 0)
    (erlang-warning
     "\\([^ \t\n]+\\.erl\\):\\([0-9]+\\):\\([0-9]+\\): Warning:"
     elixir-ts-extras--resolve-dep-file 2 3 1)
    (elixir-from-comment
     "# from: \\([^:]+\\.exs?\\):\\([0-9]+\\):\\([0-9]+\\)"
     elixir-ts-extras--resolve-dep-file 2 3 1)
    (exunit-file-line
     "\\([^ \t\n]+\\.exs?\\):\\([0-9]+\\)" 1 2))
  "Alist of error regexp for Elixir compilation output.")

(defvar elixir-ts-extras-compilation-error-regexp-alist
  '(exunit-error exunit-warning exunit-dep-warning exunit-c-file
    erlang-warning elixir-from-comment exunit-file-line)
  "List of active error matchers for Elixir compilation.")

(defvar-keymap elixir-ts-extras-compilation-mode-map
  :doc "Keymap for `elixir-ts-extras-compilation-mode'."
  :parent compilation-mode-map)

(define-compilation-mode elixir-ts-extras-compilation-mode "Elixir"
  "Compilation mode for Elixir mix command output with ANSI color support."
  (add-hook 'compilation-filter-hook
            #'elixir-ts-extras--colorize-compilation-buffer nil t)
  (add-hook 'compilation-filter-hook
            #'elixir-ts-extras--track-current-dep nil t)
  (setq-local compilation-error-regexp-alist-alist
              elixir-ts-extras-compilation-error-regexp-alist-alist)
  (setq-local compilation-error-regexp-alist
              elixir-ts-extras-compilation-error-regexp-alist)
  (setq-local compilation-scroll-output elixir-ts-extras-compilation-scroll-output)
  (setq-local compilation-max-output-line-length nil)
  (setq-local elixir-ts-extras-compilation--current-dep nil))

(provide 'elixir-ts-extras)
;;; elixir-ts-extras.el ends here
