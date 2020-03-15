;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


;; We have to load cider before org-mode to ensure ob-clojure
;; recogines that cider is available:
(require 'cider)

(require 'org-install)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "adamtait.org" user-emacs-directory))



;  Read
;  http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;  for a great explanation of emacs color themes.
;  https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;  for a more technical explanation.

;(load-theme 'solarized-dark t)
(require 'color-theme-sanityinc-tomorrow)
(color-theme-sanityinc-tomorrow--define-theme eighties)


;; Non-disabled commands

(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)



;;
;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-word-wrap-default-function nil)
 '(backup-directory-alist (quote (("." . "/tmp/emacs-backups"))))
 '(backward-delete-char-untabify-method (quote all))
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("eedfd62e0242a78ceb8d6c5426410cbbfab4c8c253682a142984693c56c4a1d0" "dcde3c9b3118fd9b2e9fbe1fe390d412216aa40f046ef5a158546616bb20f074" default)))
 '(default-directory "~/workspace" t)
 '(delete-old-versions t)
 '(dired-listing-switches "-alg")
 '(shell-file-name "/usr/local/Cellar/bash/5.0.16/bin/bash")
 '(explicit-shell-file-name "/usr/local/Cellar/bash/5.0.16/bin/bash")
 '(fringe-mode nil nil (fringe))
 '(global-auto-revert-mode t)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(image-dired-external-viewer "/usr/bin/open")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(linum-format "%3d")
 '(magit-commit-all-when-nothing-staged (quote ask-stage))
 '(magit-process-popup-time -1)
 '(magit-set-upstream-on-push t)
 '(menu-bar-mode nil)
 '(ns-pop-up-frames nil)
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode (quote regular) t)
 '(org-agenda-files nil)
 '(org-clock-idle-time 10)
 '(org-clock-into-drawer t)
 '(org-emphasis-alist
   (quote
    (("*" bold)
     ("/" italic)
     ("_" underline)
     ("=" org-code verbatim)
     ("`" org-code verbatim)
     ("~" org-verbatim verbatim)
     ("+"
      (:strike-through t)))))
 '(org-export-author-info nil)
 '(org-export-babel-evaluate nil)
 '(org-export-creator-info nil)
 '(org-export-html-validation-link "")
 '(org-export-htmlize-output-type (quote css))
 '(org-export-time-stamp-file nil)
 '(org-export-with-section-numbers nil)
 '(org-export-with-tags nil)
 '(org-export-with-toc nil)
 '(org-html-htmlize-output-type (quote css))
 '(org-return-follows-link t)
 '(org-special-ctrl-a/e t)
 '(org-startup-folded (quote showeverything))
 '(org-todo-keywords (quote ((sequence "TODO" "INPROGRESS" "DONE"))))
 '(safe-local-variable-values
   (quote
    ((eval setq org-export-htmlize-output-type
           (quote css))
     (buffer-file-coding-system . utf-8-unix)
     (org-export-html-style-include-scripts)
     (eval define-clojure-indent
           (to-data
            (quote defun))))))
 '(sentence-end-double-space nil)
 '(text-scale-mode-step 1.1)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(version-control t)
 '(visual-line-mode nil t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :family "Hack"))))
 '(highlight ((t (:foreground nil :background nil))))
 '(hl-line ((t (:background "#2d2d2d")))))
  ;; "#2d2d2d" "#ffcccc"
