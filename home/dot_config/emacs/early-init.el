;;; early-init.el --- Loaded before init.el and before the GUI is set up -*- lexical-binding: t; -*-

;; Faster startup: raise GC threshold during init, restore afterwards.
(setq gc-cons-threshold (* 256 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 32 1024 1024))))

;; Disable GUI chrome early (no-ops in a terminal, but harmless and fast).
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Keep the native-comp cache out of the config dir.
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache
   (expand-file-name "var/eln-cache/" user-emacs-directory)))

(setq inhibit-startup-screen t
      initial-scratch-message nil
      frame-inhibit-implied-resize t)

;;; early-init.el ends here
