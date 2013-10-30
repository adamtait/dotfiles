; Marmalade: http://marmalade-repo.org/
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
    '("melpa" .
      "http://melpa.milkbox.net/packages/"))
(package-initialize)

(defun install-package (package-name)
  (unless (package-installed-p package-name)
    (package-refresh-contents)
    (package-install package-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [f2] 'start-kbd-macro)
(global-set-key [f3] 'end-kbd-macro)
(global-set-key [f4] 'call-last-kbd-macro)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq split-height-threshold nil)
(if window-system
    (scroll-bar-mode -1))
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq ereceipts-home-dir "~/workspace/eReceipts-services")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Grep ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'grep)

(defun ereceipts-read-regexp ()
  "Read regexp arg for interactive grep."
  (let ((default (grep-tag-default)))
    (read-string
     (concat "Search for"
	     (if (and default (> (length default) 0))
		 (format " (default \"%s\"): " default) ": "))
     default 'grep-regexp-history)))

(defun ereceipts-clj-grep (regexp)
  "Searches for the regexp in all clojure files under the eReceipts-services directory"
  (interactive
   (list (ereceipts-read-regexp)))
  (rgrep regexp "*.clj" ereceipts-home-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Magit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(install-package 'magit)
(require 'magit)

(global-set-key (kbd "C-x m") 'magit-status)

;;; following from https://github.com/magnars/.emacs.d/blob/master/setup-magit.el

;; full screen magit-status

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;; full screen vc-annotate

(defun vc-annotate-quit ()
  "Restores the previous window configuration and kills the vc-annotate buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

(eval-after-load "vc-annotate"
  '(progn
     (defadvice vc-annotate (around fullscreen activate)
       (window-configuration-to-register :vc-annotate-fullscreen)
       ad-do-it
       (delete-other-windows))

     (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)))

;; ignore whitespace

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;;;;;;;;;;;;;;; Magit: git commit string ;;;;;;;;;;;;;;;

(defun get-git-commit-string-from-story-ID (storyID)
  (format "[#%s] \n\nStory: https://www.pivotaltracker.com/story/show/%s" storyID storyID))

(defun get-current-git-branch-name ()
  (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))

(defun parse-out-story-id (branch-name)
  (if (> (length branch-name) 8)
      (let ((story-id (substring branch-name 0 8)))
        (if (numberp (string-to-number story-id))
            story-id
          ""))
        ""))

(defun generate-git-commit-string ()
  (let ((story-id (parse-out-story-id (get-current-git-branch-name))))
    (if (> (length story-id) 0)
        (get-git-commit-string-from-story-ID story-id)
      "")))

(defun add-git-template-on-magit-log-edit-mode-hook ()
  (when (eq major-mode 'magit-log-edit-mode)
    (let ((buf (get-buffer magit-log-edit-buffer-name)))
      (princ (generate-git-commit-string) buf)
      (previous-line)
      (previous-line)
      (move-end-of-line))))

(add-hook 'after-change-major-mode-hook
          'add-git-template-on-magit-log-edit-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Clojure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(install-package 'clojure-mode)
(install-package 'clojure-test-mode)
(install-package 'paredit)

(show-paren-mode 1)

(autoload 'clojure-mode "clojure-mode" nil t)
(autoload 'align-cljlet "align-cljlet" nil t)

(eval-after-load 'clojure-mode
  '(progn
     (require 'paredit)
     (defun clojure-paredit-hook () (paredit-mode +1))
     (add-hook 'clojure-mode-hook 'clojure-paredit-hook)

     (define-key clojure-mode-map "{" 'paredit-open-brace)
     (define-key clojure-mode-map "}" 'paredit-close-brace)
     (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-[") nil)

     ;; Custom indentation rules; see clojure-indent-function
     (define-clojure-indent
       (describe 'defun)
       (testing 'defun)
       (given 'defun)
       (using 'defun)
       (with 'defun)
       (it 'defun)
       (do-it 'defun))))


;;;;;;;;;;;;;;; Clojure file associations ;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.\\(cljs?\\|dtm\\|edn\\)$" . clojure-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; nREPL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(install-package 'nrepl)
(require 'nrepl)
(add-hook 'nrepl-interaction-mode-hook
          (lambda ()
            (nrepl-turn-on-eldoc-mode)
            (enable-paredit-mode)))

(add-hook 'nrepl-mode-hook
          (lambda ()
            (nrepl-turn-on-eldoc-mode)
            (enable-paredit-mode)
            (define-key nrepl-mode-map
              (kbd "{") 'paredit-open-curly)
            (define-key nrepl-mode-map
              (kbd "}") 'paredit-close-curly)))

(setq nrepl-popup-stacktraces-in-repl t)

(defun live-nrepl-set-print-length ()
  (nrepl-send-string-sync "(set! *print-length* 100)" "clojure.core"))

(add-hook 'nrepl-connected-hook 'live-nrepl-set-print-length)

;;;;;;;;;;;;;;;;;;;;;;;;;; Helm (find files in project) ;;;;;;;;;;;;;;;;;;;;;;;;;;

(install-package 'helm)
(install-package 'helm-ls-git)
(require 'helm-ls-git)
(setq helm-ff-transformer-show-only-basename nil
      helm-ls-git-show-abs-or-relative 'relative)
(global-set-key (kbd "C-c C-f") 'helm-ls-git-ls)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Org mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(require 'org-clock)
(require 'org-faces)

(set-face-attribute 'org-mode-line-clock nil :background "pink")

(when (fboundp 'set-word-wrap)
  (add-hook 'org-mode-hook 'set-word-wrap))

(setq daypage-path "~/Dropbox/daypage/")

(defun find-daypage (&optional date)
  "Go to the day page for the specified date,
   or toady's if none is specified."
  (interactive (list
                (org-read-date "" 'totime nil nil
                               (current-time) "")))
  (setq date (or date (current-time)))
  (let* ((file (expand-file-name
                (concat daypage-path
                        (format-time-string "daypage-%Y-%m-%d-%a" date) ".org")))
         (buffer (find-buffer-visiting file)))
    (if buffer
        (pop-to-buffer buffer)
      (find-file file))
    (when (= 0 (buffer-size))
      ;; Insert an initial for the page
      (insert (format-time-string "%Y-%m-%d %A : " date)))))

(defun todays-daypage ()
  "Go straight to today's day page without prompting for a date."
  (interactive)
  (find-daypage))

(global-set-key "\C-con" 'todays-daypage)
(global-set-key "\C-coN" 'find-daypage)

(defun my-agenda ()
  (interactive)
  (org-agenda nil "n"))

(global-set-key (kbd "C-c o a") 'my-agenda)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Color ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;; silly hack to make color-theme work in Emacs24+ ;;;;;;;;;;
(defun plist-to-alist (the-plist)
  (defun get-tuple-from-plist (the-plist)
    (when the-plist
      (cons (car the-plist) (cadr the-plist))))

  (let ((alist '()))
    (while the-plist
      (add-to-list 'alist (get-tuple-from-plist the-plist))
      (setq the-plist (cddr the-plist)))
  alist))

;;;;;;;;;;;;;;; load color-theme package (Emacs < 24) ;;;;;;;;;;;;;;;
(install-package 'color-theme)
(require 'color-theme)
(setq color-theme-is-global t)
(add-to-list 'load-path "~/.emacs.d/themes")

;;;;;;;;;;;;;;; install color themes ;;;;;;;;;;;;;;;

(require 'gentooish-theme)
(require 'color-theme-solarized)
(require 'color-theme-hihat)

;; (load-theme 'bubbleberry t))
(if window-system
  (color-theme-gentooish)
  (color-theme-hihat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace

;; from https://github.com/candera/emacs/blob/3cc572daf3148a1aebe2fc69c1c93e462dba2fee/init.el#L298

(defun detabify-buffer ()
  "Calls untabify on the current buffer"
  (interactive)
  (untabify (point-min) (point-max)))

(defvar detabify-modes '(javascript-mode emacs-lisp-mode ruby-mode clojure-mode java-mode)
  "A list of the modes that will have tabs converted to spaces before saving.")

(defun mode-aware-detabify ()
  "Calls untabify on the current buffer if the major mode is one of 'detabify-modes'"
  (interactive)
  (when (member major-mode detabify-modes)
    (detabify-buffer)))

(defvar delete-trailing-whitespace-modes detabify-modes
  "A list of the modes that will have trailing whitespace before saving.")

(defun mode-aware-trailing-whitespace-cleanup ()
  "Calls delete-trailing-whitespace-modes on the current buffer
if the major mode is one of 'delete-trailing-whitespace-modes'"
  (interactive)
  (when (member major-mode delete-trailing-whitespace-modes)
    (delete-trailing-whitespace)))

(defun clean-up-whitespace ()
  "Calls untabify and delete-trailing-whitespace on the current buffer."
  (interactive)
  (detabify-buffer)
  (delete-trailing-whitespace))

(global-set-key (kbd "C-x t") 'clean-up-whitespace)

(defun toggle-show-whitespace ()
  (interactive)
  (setq show-trailing-whitespace
        (not show-trailing-whitespace)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rotate windows

;; from http://emacswiki.org/emacs/TransposeWindows
(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond
   ((not (> (count-windows) 1))
    (message "You can't rotate a single window!"))
   (t
    (let ((i 1)
          (num-windows (count-windows)))
      (while  (< i num-windows)
        (let* ((w1 (elt (window-list) i))
               (w2 (elt (window-list) (+ (% i num-windows) 1)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)
          (setq i (1+ i))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ido ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ido)
(ido-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; smex ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(install-package 'smex)
(require 'smex)
(global-set-key [(meta x)] (lambda ()
                             (interactive)
                             (or (boundp 'smex-cache)
                                 (smex-initialize))
                             (global-set-key [(meta x)] 'smex)
                             (smex)))

(global-set-key [(shift meta x)] (lambda ()
                                   (interactive)
                                   (or (boundp 'smex-cache)
                                       (smex-initialize))
                                   (global-set-key [(shift meta x)] 'smex-major-mode-commands)
                                   (smex-major-mode-commands)))
