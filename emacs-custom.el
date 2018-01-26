
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(explicit-shell-file-name "/usr/local/Cellar/bash/4.4.12/bin/bash")
 '(shell-file-name "/usr/local/Cellar/bash/4.4.12/bin/bash")
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
 '(org-export-docbook-xsl-fo-proc-command "/opt/local/bin/fop %i %o")
 '(org-export-docbook-xslt-proc-command
   "java -cp \"/opt/local/share/java/*\" net.sf.saxon.Transform -o %o %i %s")
 '(org-export-docbook-xslt-stylesheet "/opt/local/share/xsl/docbook-xsl/fo/docbook.xsl")
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
 '(org-tag-alist
   (quote
    (("billable" . 98)
     ("nonbillable" . 110)
     ("twentypercent" . 116)
     ("offclock" . 111)
     ("product" . 112))))
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
