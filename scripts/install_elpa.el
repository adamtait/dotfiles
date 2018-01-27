(setq user-init-file "/tmp/.emacs")
(require 'package)



(defvar my-packages
  '(
    gh
    gist
    pkg-info
    s
    smex
    yasnippet
    magit    
    paredit

    cider
    clojure-mode

    markdown-mode
    protobuf-mode
    ruby-mode
    web-mode
    yaml-mode
    graphviz-dot-mode
    swift-mode
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
