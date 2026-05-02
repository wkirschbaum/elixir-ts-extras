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
;; * Mix help at point (context-aware documentation lookup)
;; * Custom compilation mode with ANSI color support
;; * Error regexp matching for ExUnit output
;;
;; Suggested keybindings:
;;
;;   (keymap-global-set "C-c , t" #'elixir-ts-extras-test-menu)
;;   (keymap-global-set "C-c , s" #'elixir-ts-extras-test)
;;   (keymap-global-set "C-c , v" #'elixir-ts-extras-test-file)
;;   (keymap-global-set "C-c , a" #'elixir-ts-extras-test-all)
;;   (keymap-global-set "C-c , r" #'elixir-ts-extras-test-rerun)
;;   (keymap-global-set "C-c , k" #'elixir-ts-extras-test-stop)
;;   (keymap-global-set "C-c , x" #'elixir-ts-extras-mix-menu)
;;   (keymap-set elixir-ts-mode-map "C-c , j" #'elixir-ts-extras-test-jump)
;;   (keymap-set elixir-ts-mode-map "C-c , h" #'elixir-ts-extras-mix-help-at-point)

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
  "Command prefix for running Elixir tests.
Args (file paths, `:LINE' suffixes, transient flags) are appended
to this string when the test commands run.  Override to use
alternatives like \"mix coveralls\" or \"mix test.interactive\";
MIX_ENV=test is always set on top regardless of value."
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

(defvar elixir-ts-extras--last-test-commands nil
  "Per-project record of the most recent test invocation.
Alist of (PROJECT-ROOT . (COMMAND . FLAGS)) where COMMAND is the
mix test invocation without flags and FLAGS is the list of flag
strings that were active at run time.")

(defvar elixir-ts-extras--invocation-flags nil
  "Test flags for the current menu-initiated invocation.
Let-bound by the test menu suffixes from `transient-args' before
they call into the test commands.  Direct keybindings leave this
nil so they run without flags — without this scoping, any flag
saved via `transient-save' would silently apply to every direct
invocation.")

(defvar elixir-ts-extras-mix-history nil
  "History of mix commands.")

(defvar elixir-ts-extras--mix-tasks-cache nil
  "Cache of mix tasks per project.
Alist of (PROJECT-ROOT . (MIX-LOCK-MTIME . TASKS)).  MIX-LOCK-MTIME
is the mtime of mix.lock at the time the tasks were captured (or
nil if the project has no mix.lock yet); the entry is refreshed
when the recorded mtime no longer matches the file on disk.")

(defvar elixir-ts-extras--prewarm-in-flight nil
  "List of project roots whose mix-tasks prewarm is currently running.
Guards `elixir-ts-extras--prewarm-mix-tasks' against spawning
duplicate `mix help --names' processes when many `elixir-ts-mode'
buffers in the same project initialise simultaneously (e.g. on
session restore).")

(defvar elixir-ts-extras--test-processes nil
  "Alist of (PROJECT-ROOT . PROCESS) for currently running test runs.
Tracked so `elixir-ts-extras-test-stop' targets the right process
even after the user has switched buffers, and entries are cleaned
up automatically when the process exits.")

(defvar elixir-ts-extras-compilation--current-dep nil
  "Current dependency being compiled, used to resolve relative paths.")

;;; Project Detection

(defun elixir-ts-extras--elixir-project-p ()
  "Return non-nil if current project is an Elixir project.
Checks for mix.exs in the project root."
  (when-let* ((project (project-current))
              (root (project-root project)))
    (file-exists-p (expand-file-name "mix.exs" root))))

(defun elixir-ts-extras--ensure-elixir-project ()
  "Signal an error if not in an Elixir project."
  (unless (elixir-ts-extras--elixir-project-p)
    (user-error "Not in an Elixir project (no mix.exs found)")))

;;; Compilation Buffer Naming

(defun elixir-ts-extras--compilation-buffer-name (_mode)
  "Generate compilation buffer name from mix command."
  (let ((project-name (when-let* ((project (project-current))
                                  (root (project-root project)))
                        (file-name-nondirectory (directory-file-name root)))))
    (cond
     ((and project-name elixir-ts-extras--compile-buffer-name)
      (format "*elixir[%s/%s]*" project-name elixir-ts-extras--compile-buffer-name))
     (project-name
      (format "*elixir[%s]*" project-name))
     (elixir-ts-extras--compile-buffer-name
      (format "*elixir[%s]*" elixir-ts-extras--compile-buffer-name))
     (t "*elixir*"))))

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

(defun elixir-ts-extras--mix-lock-mtime (root)
  "Return the mtime of mix.lock under ROOT, or nil if absent."
  (let ((lock (expand-file-name "mix.lock" root)))
    (when (file-exists-p lock)
      (file-attribute-modification-time (file-attributes lock)))))

(defun elixir-ts-extras--mix-tasks-cached (root)
  "Return cached tasks for ROOT if still valid, else nil.
The cache is keyed by mix.lock mtime — adding or removing deps
invalidates the entry."
  (when-let* ((entry (alist-get root elixir-ts-extras--mix-tasks-cache
                                nil nil #'equal))
              ((equal (car entry) (elixir-ts-extras--mix-lock-mtime root))))
    (cdr entry)))

(defun elixir-ts-extras--store-mix-tasks (root tasks)
  "Record TASKS for ROOT in the cache, stamped with current mix.lock mtime."
  (when tasks
    (setf (alist-get root elixir-ts-extras--mix-tasks-cache nil nil #'equal)
          (cons (elixir-ts-extras--mix-lock-mtime root) tasks))))

(defun elixir-ts-extras--get-mix-tasks ()
  "Return available mix tasks for the current project.
Uses the cache when valid; falls back to a synchronous `mix help'
otherwise.  `elixir-ts-extras--prewarm-mix-tasks' on
`elixir-ts-mode-hook' avoids the synchronous fallback in the
common case."
  (when-let* ((project (project-current))
              (root (project-root project)))
    (or (elixir-ts-extras--mix-tasks-cached root)
        (let* ((default-directory root)
               (tasks (split-string
                       (shell-command-to-string "mix help --names 2>/dev/null")
                       "\n" t)))
          (elixir-ts-extras--store-mix-tasks root tasks)
          tasks))))

(defun elixir-ts-extras--prewarm-mix-tasks ()
  "Populate the mix-tasks cache for the current project asynchronously.
No-op when the cache is already valid, the directory is not an
Elixir project, `mix' is not on PATH, or another prewarm for the
same project root is already in flight.  Designed for
`elixir-ts-mode-hook' so that opening the mix menu does not block
on the first invocation."
  (when-let* (((executable-find "mix"))
              (project (project-current))
              (root (project-root project))
              ((file-exists-p (expand-file-name "mix.exs" root)))
              ((not (elixir-ts-extras--mix-tasks-cached root)))
              ((not (member root elixir-ts-extras--prewarm-in-flight))))
    (let* ((default-directory root)
           (out-buf (generate-new-buffer " *elixir-ts-extras-mix-tasks*"))
           (err-buf (generate-new-buffer
                     " *elixir-ts-extras-mix-tasks-stderr*")))
      (push root elixir-ts-extras--prewarm-in-flight)
      (make-process
       :name "elixir-ts-extras-mix-tasks"
       :buffer out-buf
       :stderr err-buf
       :command '("mix" "help" "--names")
       :noquery t
       :sentinel
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (unwind-protect
               (when (zerop (process-exit-status proc))
                 (with-current-buffer (process-buffer proc)
                   (elixir-ts-extras--store-mix-tasks
                    root
                    (split-string (buffer-string) "\n" t))))
             (setq elixir-ts-extras--prewarm-in-flight
                   (delete root elixir-ts-extras--prewarm-in-flight))
             (when (buffer-live-p (process-buffer proc))
               (kill-buffer (process-buffer proc)))
             (when (buffer-live-p err-buf)
               (kill-buffer err-buf)))))))))

(defun elixir-ts-extras--read-mix-command ()
  "Read a mix command with completion.
First prompts for a task with completion, then allows adding arguments."
  (let* ((task (completing-read "Mix task: " (elixir-ts-extras--get-mix-tasks)
                                nil nil nil 'elixir-ts-extras-mix-history))
         (args (read-string (format "mix %s args: " task))))
    (if (string-empty-p args)
        task
      (concat task " " args))))

;;; Mix Help

(defun elixir-ts-extras--help-target-at-point ()
  "Get mix help target at point.
Returns a string suitable for `mix help':
- MODULE.FUNCTION when on a remote function call (e.g., Enum.map)
- MODULE when on a module reference
- app:APP when on an atom (e.g., :my_app)
- Mix task name when on Mix.Tasks.* module"
  (when-let* ((node (treesit-node-at (point))))
    (let* ((node-type (treesit-node-type node))
           (parent (treesit-node-parent node))
           (parent-type (and parent (treesit-node-type parent))))
      (cond
       ;; On identifier/function name in a dot call like Enum.map
       ((and (equal node-type "identifier")
             (equal parent-type "dot"))
        (let ((module (treesit-node-text
                       (treesit-node-child-by-field-name parent "left"))))
          (format "%s.%s" module (treesit-node-text node))))
       ;; On module part of a dot call - get full module.function
       ((and (equal node-type "alias")
             (equal parent-type "dot"))
        (treesit-node-text parent))
       ;; On a standalone alias (module reference)
       ((equal node-type "alias")
        (let ((text (treesit-node-text node)))
          ;; Convert Mix.Tasks.Foo.Bar to task name foo.bar
          (if (string-match "^Mix\\.Tasks\\.\\(.+\\)$" text)
              (downcase (match-string 1 text))
            text)))
       ;; On an atom like :my_app -> app:my_app
       ((equal node-type "atom")
        (let ((text (treesit-node-text node)))
          (when (string-match "^:\\(.+\\)$" text)
            (format "app:%s" (match-string 1 text)))))
       ;; Bare identifier — pass it through as a possible local function
       ;; or task name.  Other node types (operators, punctuation,
       ;; whitespace tokens) have no useful help target, so return nil
       ;; and let the caller surface a proper user-error.
       ((equal node-type "identifier")
        (treesit-node-text node))))))

(defun elixir-ts-extras--show-mix-help (task)
  "Display mix help for TASK in a help window.
TASK is shell-quoted because it can come from `treesit-node-text'
on user content — e.g. quoted atoms like :\"weird name\" expand
to text containing whitespace and double quotes."
  (let* ((default-directory (project-root (project-current t)))
         (output (shell-command-to-string
                  (format "mix help %s 2>&1" (shell-quote-argument task)))))
    (with-help-window "*mix help*"
      (princ output))))

;;;###autoload
(defun elixir-ts-extras-mix-help-at-point ()
  "Show mix help for the symbol at point.
Detects context to determine the appropriate help target:
- On a function call (Enum.map): shows MODULE.FUNCTION help
- On a module (Enum): shows MODULE help
- On an atom (:my_app): shows app:APP help
- On Mix.Tasks.*: shows task help"
  (interactive)
  (if-let* ((target (elixir-ts-extras--help-target-at-point)))
      (elixir-ts-extras--show-mix-help target)
    (user-error "No help target at point")))

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
  [["Build"
    ("c" "compile" (lambda () (interactive) (elixir-ts-extras--run-mix "compile")))
    ("d" "deps.get" (lambda () (interactive) (elixir-ts-extras--run-mix "deps.get")))
    ("f" "format" (lambda () (interactive) (elixir-ts-extras--run-mix "format")))]
   ["Run"
    ("s" "phx.server" (lambda () (interactive) (elixir-ts-extras--run-mix "phx.server")))
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
     :transient t)]]
  (interactive)
  (elixir-ts-extras--ensure-elixir-project)
  (transient-setup 'elixir-ts-extras-mix-menu))

;;; Test File Navigation

(defun elixir-ts-extras--test-file-p (file)
  "Return non-nil if FILE is a test file."
  (and file (string-match-p "^test/.*_test\\.exs$" file)))

(defun elixir-ts-extras--test-file-for (file)
  "Return the test file path for FILE.
Converts lib/foo/bar.ex to test/foo/bar_test.exs."
  (when (and file (string-match "^lib/\\(.+\\)\\.ex$" file))
    (format "test/%s_test.exs" (match-string 1 file))))

(defun elixir-ts-extras--resolve-test-file (file-relative)
  "Resolve FILE-RELATIVE to a test file path.
If FILE-RELATIVE is already a test file, return it.
If it's a source file, return the corresponding test file.
Signal `user-error' if no test file can be determined or doesn't exist."
  (let* ((project-root (project-root (project-current t)))
         (test-file (if (elixir-ts-extras--test-file-p file-relative)
                        file-relative
                      (elixir-ts-extras--test-file-for file-relative))))
    (cond
     ((null test-file)
      (user-error "No test file found for %s" (or file-relative "current buffer")))
     ((not (file-exists-p (expand-file-name test-file project-root)))
      (user-error "Test file %s does not exist" test-file))
     (t test-file))))

(defun elixir-ts-extras--source-file-for (file)
  "Return the source file path for test FILE.
Converts test/foo/bar_test.exs to lib/foo/bar.ex."
  (when (and file (string-match "^test/\\(.+\\)_test\\.exs$" file))
    (format "lib/%s.ex" (match-string 1 file))))

;;;###autoload
(defun elixir-ts-extras-test-jump ()
  "Jump between source and test file.
If in a source file (lib/*.ex), jump to its test file (test/*_test.exs).
If in a test file, jump to its source file.
If the target file doesn't exist, prompt to create it."
  (interactive)
  (elixir-ts-extras--ensure-elixir-project)
  (let* ((project-root (project-root (project-current t)))
         (file-relative (when buffer-file-name
                          (file-relative-name buffer-file-name project-root)))
         (target-relative (or (elixir-ts-extras--test-file-for file-relative)
                              (elixir-ts-extras--source-file-for file-relative)))
         (target-absolute (when target-relative
                            (expand-file-name target-relative project-root))))
    (cond
     ((null target-relative)
      (user-error "Not in a lib/*.ex or test/*_test.exs file"))
     ((file-exists-p target-absolute)
      (find-file target-absolute))
     ((y-or-n-p (format "Create %s? " target-relative))
      (find-file target-absolute))
     (t
      (message "Aborted")))))

;;; Test Running

(defun elixir-ts-extras--test-process-cleanup (proc _event)
  "Remove PROC from `elixir-ts-extras--test-processes' on exit.
Looks up the project root via the `elixir-ts-extras-project-root'
process property stamped at launch time."
  (when (memq (process-status proc) '(exit signal))
    (when-let* ((root (process-get proc 'elixir-ts-extras-project-root)))
      (setq elixir-ts-extras--test-processes
            (assoc-delete-all root elixir-ts-extras--test-processes #'equal)))))

(defun elixir-ts-extras--run-test-command (args &optional project-directory)
  "Run the configured test command with ARGS in the project root.
ARGS is appended to `elixir-ts-extras-test-command' (so callers
pass just the file path, `:LINE' suffix, and any flag string).
PROJECT-DIRECTORY overrides the project root.  MIX_ENV=test is
always set on top.  The launched process is registered under its
project root for `elixir-ts-extras-test-stop'."
  (let* ((default-directory (or project-directory
                                (project-root (project-current t))))
         (root (expand-file-name default-directory))
         (full-command (concat "MIX_ENV=test "
                               elixir-ts-extras-test-command
                               (if (string-empty-p args)
                                   ""
                                 (concat " " args))))
         (elixir-ts-extras--compile-buffer-name "test")
         (compilation-buffer-name-function
          #'elixir-ts-extras--compilation-buffer-name)
         (buf (compile full-command #'elixir-ts-extras-compilation-mode)))
    (when-let* (((bufferp buf))
                (proc (get-buffer-process buf)))
      (process-put proc 'elixir-ts-extras-project-root root)
      (setf (alist-get root elixir-ts-extras--test-processes
                       nil nil #'equal)
            proc)
      (let ((existing (process-sentinel proc)))
        (set-process-sentinel
         proc
         (lambda (p e)
           (when existing (funcall existing p e))
           (elixir-ts-extras--test-process-cleanup p e)))))
    buf))

(defun elixir-ts-extras--last-test-entry-for-project ()
  "Return the (COMMAND . FLAGS) entry for the current project, or nil."
  (when-let* ((project (project-current))
              (root (project-root project)))
    (alist-get root elixir-ts-extras--last-test-commands nil nil #'equal)))

(defun elixir-ts-extras--combine-args (args flags)
  "Join ARGS string and FLAGS list into a single arg string."
  (let ((flag-str (and flags (string-join flags " "))))
    (cond
     ((and (string-empty-p args) (not flag-str)) "")
     ((string-empty-p args) flag-str)
     ((not flag-str) args)
     (t (concat args " " flag-str)))))

(defun elixir-ts-extras--run-test (args &optional ignore-flags)
  "Run a test invocation with ARGS in the current project.
ARGS are appended to `elixir-ts-extras-test-command' (the empty
string runs the whole suite).  `elixir-ts-extras--invocation-flags'
are appended unless IGNORE-FLAGS is non-nil.  Records the args
and flags under the current project so
`elixir-ts-extras-test-rerun' reproduces the exact invocation
regardless of project switches."
  (let* ((flags (and (not ignore-flags) elixir-ts-extras--invocation-flags))
         (root (project-root (project-current t))))
    (setf (alist-get root elixir-ts-extras--last-test-commands
                     nil nil #'equal)
          (cons args flags))
    (elixir-ts-extras--run-test-command
     (elixir-ts-extras--combine-args args flags))))

;;;###autoload
(defun elixir-ts-extras-test-rerun (arg)
  "Rerun the last test command for the current project.
With prefix ARG, drop the saved flags for this rerun only — the
recorded entry is left intact so the next plain rerun still uses
the original flags."
  (interactive "P")
  (elixir-ts-extras--ensure-elixir-project)
  (let ((entry (elixir-ts-extras--last-test-entry-for-project)))
    (unless entry
      (user-error "No previous test command for this project"))
    (elixir-ts-extras--run-test-command
     (elixir-ts-extras--combine-args (car entry)
                                     (and (not arg) (cdr entry))))))

;;;###autoload
(defun elixir-ts-extras-test (arg)
  "Run test at point with smart context detection.
Inside a test block: run that single test.
Inside a describe block (not in test): run all tests in describe.
Outside both: run all tests in the file.
If not in a test file, run the corresponding test file instead.
With prefix ARG, drop the menu's saved flags for this invocation."
  (interactive "P")
  (elixir-ts-extras--ensure-elixir-project)
  (let* ((default-directory (project-root (project-current t)))
         (file-relative (when buffer-file-name
                          (file-relative-name buffer-file-name))))
    (if (elixir-ts-extras--test-file-p file-relative)
        ;; In a test file - use context detection
        (let* ((context (elixir-ts-extras--test-context))
               (context-type (car context))
               (context-line (cdr context))
               (args
                (pcase context-type
                  ('test (format "%s:%d" file-relative context-line))
                  ('describe (format "%s:%d" file-relative context-line))
                  ('file file-relative))))
          (elixir-ts-extras--run-test args arg))
      ;; Not in a test file - run corresponding test file
      (let ((test-file (elixir-ts-extras--resolve-test-file file-relative)))
        (elixir-ts-extras--run-test test-file arg)))))

;;;###autoload
(defun elixir-ts-extras-test-file (arg)
  "Run all tests in the current file.
If not in a test file, run the corresponding test file instead.
With prefix ARG, drop the menu's saved flags for this invocation."
  (interactive "P")
  (elixir-ts-extras--ensure-elixir-project)
  (let* ((default-directory (project-root (project-current t)))
         (file-relative (when buffer-file-name
                          (file-relative-name buffer-file-name)))
         (test-file (elixir-ts-extras--resolve-test-file file-relative)))
    (elixir-ts-extras--run-test test-file arg)))

;;;###autoload
(defun elixir-ts-extras-test-all (arg)
  "Run all tests in the project.
With prefix ARG, drop the menu's saved flags for this invocation."
  (interactive "P")
  (elixir-ts-extras--ensure-elixir-project)
  (elixir-ts-extras--run-test "" arg))

;;;###autoload
(defun elixir-ts-extras-test-stop ()
  "Stop the currently running test in this project.
Looks the process up in `elixir-ts-extras--test-processes' (keyed
by project root) so it works regardless of which buffer is
current.  Sends an interrupt signal followed by \\='a\\=' to
abort the Erlang break menu."
  (interactive)
  (elixir-ts-extras--ensure-elixir-project)
  (let* ((root (project-root (project-current t)))
         (proc (alist-get root elixir-ts-extras--test-processes
                          nil nil #'equal)))
    (cond
     ((null proc)
      (user-error "No test process running for this project"))
     ((not (process-live-p proc))
      (setq elixir-ts-extras--test-processes
            (assoc-delete-all root elixir-ts-extras--test-processes #'equal))
      (user-error "Last test process for this project has already exited"))
     (t
      (interrupt-process proc)
      (process-send-string proc "a\n")))))

;;; Test Menu

(defmacro elixir-ts-extras--with-menu-flags (&rest body)
  "Run BODY with `elixir-ts-extras--invocation-flags' bound from the menu.
Suffixes use this so the menu's flag selections reach the test
commands; direct keybindings, which never enter the menu, leave
the variable nil and run without flags."
  (declare (indent 0))
  `(let ((elixir-ts-extras--invocation-flags
          (transient-args 'elixir-ts-extras-test-menu)))
     ,@body))

(transient-define-suffix elixir-ts-extras--test-at-point-suffix (arg)
  "Run test at point with the menu's current flags."
  :description "at point"
  (interactive "P")
  (transient-set)
  (elixir-ts-extras--with-menu-flags
    (elixir-ts-extras-test arg)))

(transient-define-suffix elixir-ts-extras--test-file-suffix (arg)
  "Run tests in current file with the menu's current flags."
  :description "current file"
  (interactive "P")
  (transient-set)
  (elixir-ts-extras--with-menu-flags
    (elixir-ts-extras-test-file arg)))

(transient-define-suffix elixir-ts-extras--test-all-suffix (arg)
  "Run all tests with the menu's current flags."
  :description "all project"
  (interactive "P")
  (transient-set)
  (elixir-ts-extras--with-menu-flags
    (elixir-ts-extras-test-all arg)))

(transient-define-suffix elixir-ts-extras--test-rerun-suffix (arg)
  "Rerun the last test for this project."
  :description (lambda ()
                 (let ((args (car (elixir-ts-extras--last-test-entry-for-project))))
                   (format "rerun: %s"
                           (if (or (null args) (string-empty-p args))
                               "all tests"
                             args))))
  :if #'elixir-ts-extras--last-test-entry-for-project
  (interactive "P")
  (transient-set)
  (elixir-ts-extras-test-rerun arg))

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
   ("s" elixir-ts-extras--test-at-point-suffix)
   ("v" elixir-ts-extras--test-file-suffix)
   ("a" elixir-ts-extras--test-all-suffix)
   ("r" elixir-ts-extras--test-rerun-suffix)
   ("k" "stop test" elixir-ts-extras-test-stop)]
  ["Settings"
   ("C-x C-s" "Save flags permanently" transient-save)
   ("C-x C-r" "Reset to saved" transient-reset)]
  (interactive)
  (elixir-ts-extras--ensure-elixir-project)
  (transient-setup 'elixir-ts-extras-test-menu))

;;; Compilation Mode

(defun elixir-ts-extras--colorize-compilation-buffer ()
  "Apply ANSI color codes in the compilation buffer."
  (when (eq major-mode 'elixir-ts-extras-compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(defun elixir-ts-extras--track-current-dep ()
  "Track the current dependency from ==> lines in compilation output.
Hex package names start with a lowercase letter and may contain
digits and underscores (e.g. `oauth2', `phoenix_live_view'); the
trailing portion of the line is unconstrained because mix
sometimes appends suffixes like ` (compile)'."
  (save-excursion
    (goto-char compilation-filter-start)
    (while (re-search-forward "^==> \\([a-z][a-z0-9_]*\\)\\(?:$\\| \\)" nil t)
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

(defvar elixir-ts-extras-compilation-error-regexp-alist-alist
  '((exunit-error
     "\\[error\\] \\([^[:space:]]+\\):\\([[:digit:]]+\\)" 1 2)
    (exunit-warning
     "warning: .*\n[[:space:]]*\\([^[:space:]]+\\.exs?\\):\\([[:digit:]]+\\)" 1 2 nil 1)
    (exunit-dep-warning
     "└─ \\([^:]+\\.exs?\\):\\([0-9]+\\):\\([0-9]+\\)"
     elixir-ts-extras--resolve-dep-file 2 3 1)
    ;; NIF/C build errors carry a line and column — anchoring on
    ;; `:N:N' avoids matching every stray `foo.c' that wanders past in
    ;; prose or stack traces.
    (exunit-c-file
     "\\([^[:space:]]+\\.c\\):\\([0-9]+\\):\\([0-9]+\\)"
     elixir-ts-extras--resolve-dep-file 2 3)
    (erlang-warning
     "\\([^ \t\n]+\\.erl\\):\\([0-9]+\\):\\([0-9]+\\): Warning:"
     elixir-ts-extras--resolve-dep-file 2 3 1)
    (elixir-from-comment
     "# from: \\([^:]+\\.exs?\\):\\([0-9]+\\):\\([0-9]+\\)"
     elixir-ts-extras--resolve-dep-file 2 3 1)
    ;; Catch-all for `path.exs?:N' references in failure summaries and
    ;; stack frames.  Anchored to start-of-line or a leading whitespace
    ;; / `(' character so it doesn't fire inside arbitrary prose like
    ;; `see foo.ex:5'.  ExUnit prints stack frames with leading
    ;; whitespace and frames like `(my_app 0.1.0) lib/foo.ex:13', both
    ;; of which still match.
    (exunit-file-line
     "\\(?:^\\|[ \t(]\\)\\([^[:space:](:]+\\.exs?\\):\\([0-9]+\\)" 1 2))
  "Alist of error regexp for Elixir compilation output.")

(defvar elixir-ts-extras-compilation-error-regexp-alist
  '(exunit-error exunit-warning exunit-dep-warning exunit-c-file
    erlang-warning elixir-from-comment exunit-file-line)
  "List of active error matchers for Elixir compilation.")

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

(add-hook 'elixir-ts-mode-hook #'elixir-ts-extras--prewarm-mix-tasks)

(provide 'elixir-ts-extras)
;;; elixir-ts-extras.el ends here
