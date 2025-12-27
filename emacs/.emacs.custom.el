(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(require 'projectile)
(projectile-mode 1)

;; global stuff
(electric-indent-mode 1)
(delete-selection-mode t)
(ido-mode 1)
(ido-everywhere 1)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

(require 'amx)
(amx-mode 1)

;; use spaces for everything
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default svelte-basic-offset 4)
(setq-default typescript-indent-level 4)
(setq-default fsharp-indent-offset 2)
(setq-default go-ts-mode-indent-offset 4)
(setq-default lua-indent-level 4)
(setq-default python-indent-offset 4)

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

;; Delete region with Backspace (and Shift+Backspace)
(defun my-backspace-or-delete-region ()
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (call-interactively 'delete-backward-char)))

(global-set-key (kbd "<backspace>") #'my-backspace-or-delete-region)

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

;; move region or line up and down with alt and arrows
(require 'drag-stuff)
(drag-stuff-global-mode 1)
(global-set-key (kbd "M-<up>") 'drag-stuff-up)
(global-set-key (kbd "M-<down>") 'drag-stuff-down)

;; Start in fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Bindings for switching windows
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)

;; Bindings for navigation
(global-set-key (kbd "C-w") 'kill-buffer-and-window)
(global-set-key (kbd "<f2>") 'switch-to-next-buffer)
(global-set-key (kbd "<f1>") 'switch-to-prev-buffer)
(global-set-key (kbd "<f3>") 'switch-to-buffer)

(defun select-current-line ()
  "Select the current line."
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line))

(global-set-key (kbd "C-l") 'select-current-line)
(global-set-key (kbd "C-/") 'comment-region)
(global-set-key (kbd "C-S-/") 'uncomment-region)

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

;; tree-sitter
(setq treesit-font-lock-level 4)
(setq treesit-auto-install 'prompt)

;; templ golang
(require 'templ-ts-mode)
(add-to-list 'auto-mode-alist '("\\.templ\\'" . templ-ts-mode))

;; dune for ocaml
(require 'dune)

(require 'yaml-mode)
(require 'php-mode)
(require 'go-mode)
(require 'go-ts-mode)
(require 'elixir-mode)
(require 'eglot)
(setq eglot-connect-timeout 60)

;; use flycheck for c++/c
(use-package flycheck
  :hook ((c-mode c++-mode) . flycheck-mode))

(use-package flycheck-clang-tidy
  :after flycheck
  :hook (flycheck-mode . flycheck-clang-tidy-setup))

;; svelte
(require 'svelte-mode)
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-mode))

(require 'typescript-mode)
(require 'prettier-js)
(require 'php-cs-fixer)
(require 'ruby-mode)
(require 'rubocopfmt)
(require 'html-ts-mode)
(require 'fsharp-mode)
(require 'crystal-mode)
(require 'rust-mode)
(require 'lua-mode)
(require 'qt-pro-mode)
(require 'cmake-mode)
(require 'meson-mode)
(with-eval-after-load 'meson-mode
  (define-key meson-mode-map [f1] nil))

;; java
(require 'eglot-java)
(require 'groovy-mode)

;; scala-ts-mode
(require 'scala-ts-mode)
(require 'sbt-mode)

;; blazor
(add-to-list 'auto-mode-alist '("\\.cshtml?\\'" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.razor?\\'" . csharp-mode))

;; ocaml
(require 'tuareg)
(add-to-list 'load-path "/home/ice/.opam/default/share/emacs/site-lisp")
(require 'ocp-indent)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(define-derived-mode django-web-mode web-mode "django-web"
  "Web-mode for Django templates.")
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . django-web-mode))
(setq web-mode-attr-indent-offset 2)

;; use C-j to complete emmet
(require 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'django-web-mode 'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))

;; dot-env
(require 'dotenv-mode)
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))

(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (setq-local company-backends my/company-default-backends)))

(setq read-process-output-max (* 1024 1024)) ;; 1MB, default is 4k

(defvar my-prettier-modes '(typescript-mode tsx-ts-mode js-ts-mode json-mode svelte-mode)
  "A list of major modes where Prettier should be used for formatting.")

(defvar my-php-cs-fixer-modes '(php-mode))

(global-set-key (kbd "C-S-a") #'align-regexp)
(global-set-key (kbd "<f10>") 'eglot-code-action-quickfix)

(require 'reformatter)
(reformatter-define djlint-format
  :program "djlint"
  :args '("--reformat" "-")
  :lighter " DJ")

(defun my-eglot-or-other-format ()
  "Format using prettier-js or php-cs-fixer depending on mode, otherwise eglot."
  (interactive)
  (delete-trailing-whitespace)
    (cond
     ((memq major-mode my-prettier-modes) (prettier-js-prettify))
     ((memq major-mode my-php-cs-fixer-modes) (php-cs-fixer-fix))
     ((derived-mode-p 'django-web-mode) (djlint-format-buffer))
     ((memq major-mode '(ruby-mode)) (rubocopfmt))
     ((eglot-managed-p) (eglot-format-buffer))))

(global-set-key (kbd "C-f") #'my-eglot-or-other-format)

(global-set-key (kbd "<f9>") #'eldoc-doc-buffer)

(setq eglot-workspace-configuration
      '(:java (:format (:enabled t
               :settings (:url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
                               :profile "GoogleStyle")))))

(setq eglot-server-programs
      '(((typescript-mode tsx-ts-mode js-ts-mode) . ("typescript-language-server" "--stdio"))
        ((go-mode go-ts-mode) . ("gopls" "serve"))
        (svelte-mode . ("svelteserver" "--stdio"))
        ((html-mode html-ts-mode django-web-mode) . ("/home/ice/playground/slipper/slipper"))
        (templ-ts-mode . ("templ" "lsp"))
        (csharp-mode . ("OmniSharp" "-lsp"))
        ((sbt-mode scala-ts-mode) . ("metals"))
        (perl-mode . ("pls"))
        (java-mode . ("jdtls"))
        ((ruby-mode ruby-ts-mode) . ("solargraph" "stdio"))
        (crystal-mode . ("crystalline"))
        (python-mode . ("pylsp"))
        (elixir-mode . ("elixir-ls"))
        (lua-mode .    ("lua-language-server"))
        ((c-mode c++-mode) . ("clangd" "--completion-style=detailed" "--header-insertion=never"))
        ((rust-ts-mode rust-mode) . ("rust-analyzer" :initializationOptions (:check (:command "clippy"))))
        (tuareg-mode . ("ocamllsp" "--stdio"))
        (php-mode . ("phpactor" "language-server"))))

;; setup eglot for f#
(require 'eglot-fsharp)

;; compile stuff
(require 'compile)
(global-set-key (kbd "C-<return>") 'compile)
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(setq compilation-scroll-output t)
(setq compilation-max-output-line-length nil)

;; change string casing
(require 'string-inflection)
(global-set-key (kbd "<f12>") 'string-inflection-all-cycle)

;; spell checking
(global-set-key (kbd "<f11>") #'ispell-word)

;; git
(require 'magit)
(setq magit-display-buffer-noselect t)

;; autosave and stuff
(require 'no-littering)
(let ((dir (no-littering-expand-var-file-name "lock-files/")))
  (make-directory dir t)
  (setq lock-file-name-transforms `((".*" ,dir t))))
(no-littering-theme-backups)

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-d") 'mc/mark-previous-like-this)
