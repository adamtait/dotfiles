(package-initialize)

(defun adamtait/all-packages-installed-p (packages)
  (cl-loop for pkg in packages
	   when (not (package-installed-p pkg)) do (cl-return nil)
	   finally (cl-return t)))

(defun adamtait/install-all-packages (packages)
  (unless (adamtait/all-packages-installed-p packages)
    (package-refresh-contents)
    (dolist (pkg packages)
      (when (not (package-installed-p pkg))
	(package-install pkg)))))

(setq adamtait/stable-packages
  '(
    abyss-theme
    cider
    clojure-mode
    clojure-mode-extra-font-locking
    ;color-theme-sanityinc-solarized
    enh-ruby-mode
    exec-path-from-shell
    find-file-in-project
    flycheck-clj-kondo
    gh
    gist
    git-commit
    gnuplot
    graphviz-dot-mode
    htmlize
    ido-vertical-mode
    inf-clojure
    js2-mode
    json-mode    
    ledger-mode
    lua-mode
    magit
    markdown-mode
    markdown-toc
    multiple-cursors
    org-plus-contrib
    org-re-reveal
    paredit
    rainbow-delimiters
    rjsx-mode
    ruby-mode
    s
    ;smartparens
    smex
    ;solarized-theme
    swift-mode
    terraform-mode
    typescript-mode
    typo
    visual-fill-column
    web-mode
    xml-rpc
    yaml-mode
    yasnippet
    ))

(setq adamtait/unstable-packages
  '(graphql-mode
    hlinum
    linum-off
    ob-restclient
    restclient
    ))

(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)

(adamtait/install-all-packages adamtait/stable-packages)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(adamtait/install-all-packages adamtait/unstable-packages)

;; (unless (adamtait/all-packages-installed-p)
;;   (message "%s" "Refreshing package database...")
;;   (package-refresh-contents)
;;   (dolist (pkg adamtait/packages)
;;     (when (not (package-installed-p pkg))
;;       (package-install pkg))))
