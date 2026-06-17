;;; init.el --- Minimal modern CLI Emacs config -*- lexical-binding: t; -*-
;;; Commentary:
;; CLI-first (runs in `emacs -nw` inside ghostty). Targets Clojure, Python, Go, Markdown.
;; Uses built-in package.el + use-package (Emacs >= 29). No straight.el.
;;; Code:

;; ---------------------------------------------------------------------------
;; Package bootstrap
;; ---------------------------------------------------------------------------
(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))
(unless package-archive-contents (package-refresh-contents))
(require 'use-package)
(setq use-package-always-ensure t
      use-package-expand-minimally t)

;; Keep transient/customize/native-comp state out of the tracked config dir.
(setq custom-file (expand-file-name "var/custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;; ---------------------------------------------------------------------------
;; Sane defaults
;; ---------------------------------------------------------------------------
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 100)
(setq make-backup-files nil
      create-lockfiles nil
      require-final-newline t
      ring-bell-function 'ignore
      use-short-answers t
      sentence-end-double-space nil)

(global-display-line-numbers-mode 1)
(column-number-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(global-auto-revert-mode 1)
(delete-selection-mode 1)

;; Make the terminal play nice: enable mouse + clipboard where supported.
(when (eq window-system nil)
  (xterm-mouse-mode 1))

(use-package modus-themes
  :init (load-theme 'modus-vivendi :no-confirm))

;; ---------------------------------------------------------------------------
;; Minibuffer / completion UX
;; ---------------------------------------------------------------------------
(use-package vertico :init (vertico-mode 1))
(use-package orderless
  :init (setq completion-styles '(orderless basic)
              completion-category-overrides '((file (styles partial-completion)))))
(use-package marginalia :init (marginalia-mode 1))
(use-package consult
  :bind (("C-s"   . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y"   . consult-yank-pop)
         ("M-g g" . consult-goto-line)))
(use-package which-key :init (which-key-mode 1))

;; In-buffer completion (terminal-friendly).
(use-package corfu
  :init (global-corfu-mode 1)
  :custom (corfu-auto t) (corfu-auto-delay 0.2)
  :config (when (eq window-system nil)
            (use-package corfu-terminal
              :init (corfu-terminal-mode 1))))
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; ---------------------------------------------------------------------------
;; Project / VCS
;; ---------------------------------------------------------------------------
(use-package magit :bind ("C-x g" . magit-status))
(use-package eglot :ensure nil  ; built-in
  :hook ((python-mode python-ts-mode go-mode go-ts-mode) . eglot-ensure)
  :config (add-hook 'before-save-hook
                    (lambda () (when (eglot-managed-p) (eglot-format-buffer))) nil t))

;; Tree-sitter where grammars are available (Emacs 29+).
(when (treesit-available-p)
  (setq treesit-language-source-alist
        '((python "https://github.com/tree-sitter/tree-sitter-python")
          (go     "https://github.com/tree-sitter/tree-sitter-go")
          (gomod  "https://github.com/camdencheek/tree-sitter-go-mod"))))

;; ---------------------------------------------------------------------------
;; Languages
;; ---------------------------------------------------------------------------
;; Clojure
(use-package clojure-mode)
(use-package cider
  :hook (clojure-mode . cider-mode)
  :custom (cider-repl-display-help-banner nil))
(use-package paredit
  :hook ((clojure-mode emacs-lisp-mode lisp-mode) . enable-paredit-mode))

;; Python (eglot picks up pyright/pylsp; respects the active pyenv interpreter).
(use-package pyvenv :hook (python-mode . pyvenv-mode))

;; Go
(use-package go-mode
  :hook (go-mode . (lambda () (setq tab-width 4 indent-tabs-mode t))))

;; Markdown (a primary use case — comfortable long-form editing).
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :custom (markdown-fontify-code-blocks-natively t))
(use-package markdown-toc :after markdown-mode)
(use-package olivetti
  :hook ((markdown-mode . olivetti-mode)
         (markdown-mode . visual-line-mode))
  :custom (olivetti-body-width 100))

;;; init.el ends here
