
(setq user-init-file "/tmp/.emacs")
(require 'package)

;; taken from melpa.org
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  ;;(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives (cons "gnu" "https://elpa.gnu.org/packages/") t))


;; from reddit thread on "Failed to download 'gnu' archive." bug
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)



(defvar my-packages
  '(
    cider
    clojure-mode
    gh
    gist
    graphviz-dot-mode
    js2-mode
    json-mode
    magit
    markdown-mode
    paredit
    pkg-info
    protobuf-mode
    rjsx-mode
    enh-ruby-mode
    s
    smartparens
    smex
    swift-mode
    terraform-mode
    web-mode
    yaml-mode
    yasnippet
    ))

(package-refresh-contents)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
