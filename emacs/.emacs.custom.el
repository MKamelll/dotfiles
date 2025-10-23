(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

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
(global-set-key (kbd "C-S-z") 'undo-redo)

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
(with-eval-after-load 'company
  (define-key company-mode-map (kbd "C-SPC") #'company-complete))

(require 'lsp-mode)
(require 'php-mode)
(global-set-key (kbd "C-f") #'lsp-format-buffer)
(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)
(add-hook 'python-mode-hook #'lsp-deferred)
(add-hook 'php-mode-hook #'lsp-deferred)

(setq read-process-output-max (* 1024 1024)) ;; 1MB, default is 4k
(setq lsp-idle-delay 0.500)

(setq lsp-pylsp-plugins-mypy-enabled t
      lsp-pylsp-plugins-mypy-live-mode nil
      lsp-pylsp-plugins-black-enabled t
      lsp-pylsp-plugins-flake8-enabled nil
      lsp-pylsp-plugins-pycodestyle-enabled nil
      lsp-pylsp-server-command '("uv" "run" "pylsp"))

(setq lsp-pylsp-plugins-pylsp_mypy-args '("--config-file" "pyproject.toml"))

(require 'lsp-ui)
(add-hook 'lsp-mode-hook #'lsp-ui-mode)

;; git
(require 'magit)
(global-set-key (kbd "<f5>") 'magit-status)
