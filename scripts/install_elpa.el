(setq user-init-file "/tmp/.emacs")
(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;(add-to-list 'package-archives
;    '("marmalade" .
;      "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
    '("melpa-stable" .
      "http://stable.melpa.org/packages/"))

(package-initialize)
(package-refresh-contents)

(defvar my-packages
  '(
    gh
    gist
    markdown-mode
    paredit
    pkg-info
    protobuf-mode
    ruby-mode
    s
    smex
    web-mode
    yaml-mode
    yasnippet
    graphviz-dot-mode
    magit
    swift-mode
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
