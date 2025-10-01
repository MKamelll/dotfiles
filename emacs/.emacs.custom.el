(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Relative line numbers
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Autocomplete
(require 'corfu)
(global-corfu-mode)
(setq corfu-auto t)        ;; enable auto popup
(setq corfu-auto-prefix 2) ;; show after typing 2 chars
(setq corfu-auto-delay 0)  ;; no delay

;; Remove the bar below to main menu
(tool-bar-mode -1)

;; Set the font weight to 16
(set-face-attribute 'default nil :height 160)

;; Map C-g to ESC
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Bindings for copying and pasting
(global-set-key (kbd "C-S-c") 'kill-ring-save)   ;; Copy
(global-set-key (kbd "C-S-v") 'yank)             ;; Paste
(global-set-key (kbd "C-S-d") 'kill-region)    ;; Cut (Shift+Ctrl+C)
(global-set-key (kbd "C-S-z") 'undo)             ;; Undo
(global-set-key (kbd "C-S-r") 'undo-redo)           ;; Redo (needs redo+ package or `undo-redo` in Emacs 28+)
(global-set-key (kbd "C-S-s") 'save-buffer)

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

;; Lsp things
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

(require 'lsp-mode)
(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook #'lsp-ui-mode)
