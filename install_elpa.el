(setq user-init-file "/tmp/.emacs")

;; Marmalade: http://marmalade-repo.org/
(require 'package)

(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
    '("melpa" .
      "http://melpa.milkbox.net/packages/"))

(package-initialize)

(package-refresh-contents)

(defvar my-packages
  '(
    clj-refactor
    clojure-cheatsheet
    color-theme
    dash
    gh
    gist
    markdown-mode
    multiple-cursors
    paredit
    pkg-info
    protobuf-mode
    ruby-mode
    s
    smex
    typopunct
    web-mode
    yaml-mode
    yasnippet
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
