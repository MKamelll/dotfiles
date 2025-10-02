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

;; Searching
(defun my-isearch-from-start ()
  "Run `isearch-forward` but always from the beginning of the buffer."
  (interactive)
  (goto-char (point-min))
  (isearch-forward))

(global-set-key (kbd "C-s") 'my-isearch-from-start)

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
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-S-<tab>") 'previous-buffer)

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
(setq lsp-keymap-prefix "C-c l")
(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook #'lsp-ui-mode)
