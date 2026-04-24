(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(defun my/backspace-or-delete-region ()
  "Delete region with Backspace (and Shift+Backspace)"
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (call-interactively 'delete-backward-char)))

(defun my/backward-kill-word ()
  "Kill word backward, stopping at line beginning."
  (interactive)
  (let ((limit (line-beginning-position)))
    (if (> (point) limit)
        (kill-region (max limit (save-excursion (backward-word) (point)))
                     (point))
      (delete-char -1))))

(defun my/setup-minibuffer-backspace ()
  "Make C-Backspace delete word without copying in all minibuffers."
  (dolist (keymap (list minibuffer-local-map
                        minibuffer-local-ns-map
                        minibuffer-local-completion-map
                        minibuffer-local-must-match-map
                        minibuffer-local-filename-completion-map))
    (define-key keymap (kbd "C-<backspace>") #'backward-delete-word)))

(defun select-current-line ()
  "Select the current line."
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line))

(use-package indent-bars
  :ensure t
  :hook
  (prog-mode . indent-bars-mode))

(use-package projectile
  :ensure t
  :config (projectile-mode 1))

(use-package emms
  :ensure t
  :defer t
  :init
  (setq-default emms-player-list '(emms-player-mpv)
                emms-player-mpv-environment '("PULSE_PROP_media.role=music")
                emms-player-mpv-parameters
                '("--quiet" "--really-quiet" "--no-audio-display" "--force-window=no" "--vo=null"))
  :config
  (emms-all)
)

(use-package smex
  :ensure t
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode 1))

(use-package amx
  :ensure t
  :config
  (amx-mode 1))

(use-package transient
  :config
  (define-key transient-map [escape] #'transient-quit-one))

(use-package magit
  :ensure t
  :init
  (setq magit-display-buffer-noselect t)
  :bind (:map magit-mode-map
              ("<escape>" . keyboard-escape-quit)
              ("C-w" . kill-buffer-and-window)))

(use-package git-commit
  :config (remove-hook 'git-commit-setup-hook #'git-commit-setup-capf))

(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-global-mode 1)
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))


(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-dabbrev-ignore-case t
        company-dabbrev-downcase nil
        completion-ignore-case t
        company-dabbrev-other-buffers t
        company-dabbrev-code-everywhere t
        company-transformers '(company-sort-prefer-same-case-prefix)
        )

  :config
  (global-company-mode 1)
  (define-key company-mode-map (kbd "C-SPC") #'company-complete)
  (define-key company-mode-map (kbd "TAB") nil)
  (define-key company-mode-map (kbd "<tab>") nil)
  (define-key company-active-map (kbd "TAB") #'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") #'company-complete-common-or-cycle)
  )

(use-package web-mode
  :ensure t
  :init
  (define-derived-mode django-web-mode web-mode "django-web"
    "Web-mode for Django templates.")

  :mode (("\\.phtml\\'"       . web-mode)
         ("\\.tpl\\.php\\'"   . web-mode)
         ("\\.[agj]sp\\'"    . web-mode)
         ("\\.as[cp]x\\'"    . web-mode)
         ("\\.erb\\'"         . web-mode)
         ("\\.mustache\\'"    . web-mode)
         ("\\.html?\\'"       . web-mode)
         ("\\.djhtml\\'" . django-web-mode))
  :config
  ;; other pref
  (setq web-mode-attr-indent-offset 2)
  (add-hook 'django-web-mode-hook
          (lambda ()
            (web-mode-set-engine "django")
            (font-lock-flush)
            (font-lock-ensure)))
  )

(use-package flycheck
  :ensure t)

(use-package lsp-mode
  :ensure t
  :init (setq lsp-headerline-breadcrumb-enable nil
              lsp-enabled-clients nil
              lsp-eldoc-enable-hover t
              lsp-enable-on-type-formatting nil
              lsp-completion-enable t
              lsp-enable-snippet nil
              lsp-signature-auto-activate t
              lsp-signature-render-documentation t
              lsp-signature-doc-lines 3
              lsp-keep-workspace-alive nil
              lsp-diagnostics-provider :flycheck
              lsp-log-io nil
              lsp-enable-suggest-server-download nil
              lsp-auto-guess-root t)
  :commands (lsp lsp-deferred)

  :hook
  (python-ts-mode . lsp-deferred)
  (go-ts-mode . lsp-deferred)
  (rust-ts-mode . lsp-deferred)
  (typescript-ts-mode . lsp-deferred)
  (django-web-mode . lsp-deferred)
  (c-ts-mode . lsp-deferred)
  (c++-ts-mode . lsp-deferred)

  :config
  (add-hook 'lsp-managed-mode-hook
            (lambda ()
              (setq-local company-backends
                          '((company-capf company-dabbrev-code company-dabbrev company-files)))
              ))

  (defun lsp-code-action-quickfix ()
  "Execute quickfix code actions."
  (interactive)
  (lsp-execute-code-action-by-kind "quickfix"))

  ;; shortcuts
  (global-set-key (kbd "<f10>") #'lsp-code-action-quickfix)
  (global-set-key (kbd "<f9>") #'lsp-describe-thing-at-point)

  (define-key lsp-mode-map (kbd "<tab>") nil)
  (define-key lsp-mode-map (kbd "TAB") nil)
  (define-key lsp-signature-mode-map (kbd "C-<tab>") #'lsp-signature-next)
  (define-key lsp-signature-mode-map (kbd "C-TAB") #'lsp-signature-next)
  (define-key lsp-signature-mode-map (kbd "C-<backtab>") #'lsp-signature-previous)

  ;; pylsp
  (setq lsp-pylsp-plugins-autopep8-enabled nil
      lsp-pylsp-plugins-yapf-enabled nil
      lsp-pylsp-plugins-black-enabled t
      lsp-pylsp-plugins-flake8-enabled t
      lsp-pylsp-plugins-mypy-enabled t
      lsp-pylsp-plugins-mypy-dmypy-enabled t
      lsp-pylsp-plugins-mypy-live-mode t
      lsp-pylsp-plugins-mypy-strict t
      lsp-pylsp-plugins-pydocstyle-enabled t
      lsp-pylsp-plugins-pydocstyle-ignore ["D100" "D101" "D102" "D103" "D104" "D105" "D106" "D107"])

  ;; django
  (add-to-list 'lsp-language-id-configuration '(django-web-mode . "html"))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("djls" "serve"))
    :major-modes '(django-web-mode)
    :add-on? t
    :server-id 'djls))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("tailwindcss-language-server" "--stdio"))
    :major-modes '(django-web-mode)
    :add-on? t
    :server-id 'tailwindcss-ls-django))

  ;; clangd
  (setq lsp-clients-clangd-args '("--header-insertion=never" "--completion-style=detailed" "--query-driver=/usr/bin/g++" "--clang-tidy"))
  )

;; templ golang
(use-package templ-ts-mode
  :ensure t
  :defer t
  :mode
  ("\\.templ\\'" . templ-ts-mode)
  )

(use-package dune
  :ensure t)

(use-package yaml-mode
  :ensure t
  :mode
  ("\\.clang-format\\'" . yaml-mode)
  )

(use-package php-mode
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package elixir-mode
  :ensure t)

(use-package svelte-mode
  :ensure t
  :defer t
  :mode ("\\.svelte\\'" . svelte-mode)
  )

(use-package typescript-mode
  :ensure t)

(use-package prettier-js
  :ensure t)

(use-package php-cs-fixer
  :ensure t)

(use-package ruby-mode
  :ensure t)

(use-package rubocopfmt
  :ensure t)

(use-package fsharp-mode
  :ensure t)

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  (treesit-font-lock-level 4)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package fsharp-mode
  :ensure t)

(use-package crystal-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package qt-pro-mode
  :ensure t
  :defer t
  :mode ("\\.pro\\'" . qt-pro-mode))

(use-package cmake-mode
  :ensure t)

(use-package meson-mode
  :ensure t
  :config
  (define-key meson-mode-map [f1] nil)
  )

(use-package groovy-mode
  :ensure t)

(use-package scala-ts-mode
  :ensure t)

(use-package sbt-mode
  :ensure t)

(use-package csharp-mode
  :ensure t
  :mode (("\\.cshtml?\\'" . csharp-mode)
         ("\\.razor?\\'" . csharp-mode))
  )

(use-package ocp-indent
  :ensure t)

(use-package tuareg
  :ensure t
  :config
  (add-to-list 'load-path "/home/ice/.opam/default/share/emacs/site-lisp")
  (require 'ocp-indent)
  (add-hook 'tuareg-mode-hook 'ocp-setup-indent))

(use-package dotenv-mode
  :ensure t
  :mode ("\\.env\\..*\\'" . dotenv-mode))

(defvar my-prettier-modes '(typescript-mode tsx-ts-mode js-ts-mode json-mode svelte-mode)
  "A list of major modes where Prettier should be used for formatting.")

(defvar my-php-cs-fixer-modes '(php-mode))

(use-package reformatter
  :ensure t
  :config
  (reformatter-define djlint-format
    :program "djlint"
    :args '("--reformat" "-")
    :lighter " DJ")
  )

(defun my/lsp-mode-or-other-format ()
  "Format using prettier-js or php-cs-fixer depending on mode, otherwise lsp-mode."
  (interactive)
  (delete-trailing-whitespace)
    (cond
     ((memq major-mode my-prettier-modes) (prettier-js-prettify))
     ((memq major-mode my-php-cs-fixer-modes) (php-cs-fixer-fix))
     ((derived-mode-p 'django-web-mode) (djlint-format-buffer))
     ((memq major-mode '(ruby-mode)) (rubocopfmt))
     ((bound-and-true-p lsp-mode) (lsp-format-buffer))))

(defun query-replace-whole-buffer (from to)
  "Query and replace something in the whole buffer"
  (interactive "sQuery replace: \nsQuery replace %s with: ")
  (save-excursion
  (goto-char (point-min))
  (query-replace from to)))

(use-package compile
  :init
  (setq compilation-scroll-output t
        compilation-max-output-line-length nil)
  :bind
  ("C-<return>" . compile))

(use-package ansi-color
  :ensure t
  :hook (compilation-filter-hook . ansi-color-compilation-filter))


(use-package string-inflection
  :ensure t
  :bind ("<f12>" . string-inflection-all-cycle))

(use-package no-littering
  :ensure t
  :config
  (let ((dir (no-littering-expand-var-file-name "lock-files/")))
    (make-directory dir t)
    (setq lock-file-name-transforms `((".*" ,dir t))))
  (no-littering-theme-backups)
  )

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-d") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-S-d") 'mc/mark-previous-like-this)
  (add-to-list 'mc/cmds-to-run-for-all 'my/backspace-or-delete-region)
  (add-to-list 'mc/cmds-to-run-for-all 'my/backward-kill-word)
  )

(use-package gruber-darker-theme
  :ensure t
  :config
  (load-theme 'gruber-darker t))

(use-package dired
  :init
  (setq dired-dwim-target t)
  )

(use-package emacs
  :init
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (setq inhibit-startup-screen t
        gc-cons-threshold 100000000
        read-process-output-max (* 1024 1024)
        display-line-numbers-type 'relative
        select-enable-clipboard nil
        select-enable-primary nil
        mouse-drag-copy-region nil)

  (setq-default indent-tabs-mode nil
                tab-width 4
                svelte-basic-offset 4
                typescript-indent-level 4
                fsharp-indent-offset 2
                lua-indent-level 4
                python-indent-offset 4
                c-basic-offset 4
                c-ts-mode-indent-offset 4
                c-ts-common-indent-offset 4)

  :hook
  (minibuffer-setup . my/setup-minibuffer-backspace)
  (before-save . my/lsp-mode-or-other-format)

  :config
  ;; ui stuff
  (set-face-attribute 'default nil :height 160)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (electric-indent-mode 1)
  (delete-selection-mode t)
  (ido-mode 1)
  (ido-everywhere 1)
  (global-display-line-numbers-mode 1)
  (column-number-mode 1)

  (add-hook 'c++-ts-mode-hook
          (lambda ()
            (define-key c++-ts-mode-map (kbd "TAB") nil)
            (define-key c++-ts-mode-map (kbd "<tab>") nil)))

  ;; flycheck
  (global-set-key (kbd "C-f") 'flycheck-list-errors)

  ;; spell checking
  (global-set-key (kbd "<f11>") #'ispell-word)

  ;; cancel things with esc
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  ;; text stuff shortcuts
  (global-set-key (kbd "C-S-c") 'clipboard-kill-ring-save)
  (global-set-key (kbd "C-S-v") 'clipboard-yank)
  (global-set-key (kbd "C-S-x") 'clipboard-kill-region)
  (global-set-key (kbd "C-S-s") 'save-buffer)
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-q") 'undo-redo)
  (global-set-key (kbd "C-/") 'comment-region)
  (global-set-key (kbd "C-S-/") 'uncomment-region)
  (global-set-key (kbd "C-l") 'select-current-line)
  (global-set-key (kbd "C-S-a") 'mark-whole-buffer)

  ;; shortcuts for user functions
  (global-set-key (kbd "<backspace>") #'my/backspace-or-delete-region)
  (global-set-key (kbd "C-<backspace>") #'my/backward-kill-word)

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
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
