(setq user-init-file "/tmp/.emacs")
(require 'package)


(defvar my-packages
  '(
    cider
    clojure-mode
    gh
    gist
    graphviz-dot-mode
    json-mode
    magit
    markdown-mode
    paredit
    pkg-info
    protobuf-mode
    rjsx-mode
    ruby-mode
    s
    smex
    swift-mode
    web-mode
    yaml-mode
    yasnippet
    ))




(package-initialize)

(add-to-list 'package-archives
             '("marmalade" .
               "https://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("melpa-stable" .
               "http://stable.melpa.org/packages/"))

(package-refresh-contents)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
