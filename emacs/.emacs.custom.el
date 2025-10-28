(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; use spaces for everything
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Relative line numbers
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Remove the bar below to main menu
(tool-bar-mode -1)

;; Set the font weight to 16
(set-face-attribute 'default nil :height 160)

;; Map C-g to ESC
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Bindings for copying and pasting
(global-set-key (kbd "C-S-c") 'kill-ring-save)
(global-set-key (kbd "C-S-v") 'yank)
(global-set-key (kbd "C-S-x") 'kill-region)
(global-set-key (kbd "C-S-s") 'save-buffer)

;; Undo and Redo
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-q") 'undo-redo)

;; --- Disable automatic clipboard copying on selection ---
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
Unlike `backward-kill-word', this does not save the deleted text to the kill ring."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(global-set-key (kbd "C-<backspace>") #'backward-delete-word)

(defun my-setup-minibuffer-backspace ()
  "Make C-Backspace delete word without copying in all minibuffers."
  (dolist (keymap (list minibuffer-local-map
                        minibuffer-local-ns-map
                        minibuffer-local-completion-map
                        minibuffer-local-must-match-map
                        minibuffer-local-filename-completion-map))
    (define-key keymap (kbd "C-<backspace>") #'backward-delete-word)))

(add-hook 'minibuffer-setup-hook #'my-setup-minibuffer-backspace)

;; Moving a line up and down
(defun move-line-up ()
  "Move current line up by one."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  "Move current line down by one."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; Start in fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Bindings for navigation
(global-set-key (kbd "C-w") 'kill-buffer-and-window)
(global-set-key (kbd "<f2>") 'switch-to-next-buffer)
(global-set-key (kbd "<f1>") 'switch-to-prev-buffer)

(defun select-current-line ()
  "Select the current line."
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line))

(global-set-key (kbd "C-l") 'select-current-line)
(global-set-key (kbd "C-/") 'comment-region)

(require 'multiple-cursors)
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)

;; Setting emacs to symlink the compile_commands.json file to the root directory
;; if using meson and the build directory is "builddir"
(defun my/meson-setup-compile-commands ()
  "Automatically link compile_commands.json from Meson builddir to project root."
  (let* ((root (project-root (project-current t)))
         (builddir (expand-file-name "builddir/compile_commands.json" root))
         (link (expand-file-name "compile_commands.json" root)))
    (when (and (file-exists-p builddir)
               (not (file-exists-p link)))
      (make-symbolic-link builddir link t)
      (message "Linked compile_commands.json from builddir."))))

;; Hook into opening a C/C++ file
(add-hook 'c-mode-common-hook #'my/meson-setup-compile-commands)

;; Lsp things
(require 'company)
(global-company-mode t)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(defvar my/company-default-backends
  '((company-capf company-dabbrev-code company-dabbrev company-files))
  "Default company backends including LSP (capf) and local buffer completions.")
(setq company-backends my/company-default-backends)
(setq company-dabbrev-downcase nil       ;; keep original case
      company-dabbrev-other-buffers t    ;; search other buffers too
      company-dabbrev-code-everywhere t) ;; include comments and strings

(with-eval-after-load 'company
  (define-key company-mode-map (kbd "C-SPC") #'company-complete))

(require 'dotenv-mode)
(require 'yaml-mode)
(require 'php-mode)
(require 'go-mode)
(require 'svelte-mode)
(require 'typescript-mode)
(require 'eglot)
(require 'prettier-js)
(require 'php-cs-fixer)
(require 'dotenv-mode)
(require 'eglot-java)

(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (setq-local company-backends my/company-default-backends)))

(setq read-process-output-max (* 1024 1024)) ;; 1MB, default is 4k

(defvar my-prettier-modes '(typescript-mode tsx-ts-mode js-ts-mode json-mode svelte-mode)
  "A list of major modes where Prettier should be used for formatting.")

(defvar my-php-cs-fixer-modes '(php-mode))

(defun my-eglot-or-other-format ()
  "Format using prettier-js or php-cs-fixer depending on mode, otherwise Eglot."
  (interactive)
  (when (eglot-managed-p)
    (cond
     ((memq major-mode my-prettier-modes)
      (prettier-js-prettify))
     ((memq major-mode my-php-cs-fixer-modes)
      (php-cs-fixer-fix))
     (t
      (eglot-format-buffer)))))

(global-set-key (kbd "C-f") #'my-eglot-or-other-format)

(setq eglot-workspace-configuration
      '(:java (:format (:enabled t
               :settings (:url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
                               :profile "GoogleStyle")))))

(setq eglot-server-programs
       '((typescript-mode . ("typescript-language-server" "--stdio"))
	 (go-mode . ("gopls" "serve"))
	 (java-mode . ("jdtls"
         "-data" "/home/ice/.cache/jdtls-workspace"
         "--add-modules=ALL-SYSTEM"
         "--add-opens" "java.base/java.util=ALL-UNNAMED"
         "--add-opens" "java.base/java.lang=ALL-UNNAMED"))
	 (python-mode . ("uv" "run" "pylsp"))
	 (php-mode . ("phpactor" "language-server"))
	 (tsx-mode . ("typescript-language-server" "--stdio"))
         (js-ts-mode . ("typescript-language-server" "--stdio"))))

(add-hook 'java-mode-hook #'eglot-ensure)
(add-hook 'eglot-managed-mode-hook #'eglot-java-mode)
(add-hook 'typescript-mode-hook #'eglot-ensure)
(add-hook 'tsx-ts-mode-hook #'eglot-ensure)
(add-hook 'js-ts-mode-hook #'eglot-ensure)
(add-hook 'svelte-mode-hook 'eglot-ensure)
(add-hook 'php-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)

;; git
(require 'magit)
(global-set-key (kbd "<f5>") 'magit-status)

;; autosave and stuff
(require 'no-littering)
(let ((dir (no-littering-expand-var-file-name "lock-files/")))
  (make-directory dir t)
  (setq lock-file-name-transforms `((".*" ,dir t))))
(no-littering-theme-backups)
