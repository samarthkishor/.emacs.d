(unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))

(setq use-package-verbose t)
(setq use-package-always-ensure t)

(eval-when-compile
    (require 'use-package))

(setq user-full-name "Samarth Kishor"
      user-mail-address "samarthkishor1@gmail.com")

(setq evil-want-abbrev-expand-on-insert-exit nil)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(global-set-key (kbd "M-x") 'execute-extended-command)

(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)

(evil-define-key nil evil-normal-state-map
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

(setq sentence-end-double-space nil)
(define-key evil-normal-state-map ")" 'forward-sentence)

(use-package evil-leader
  :commands (evil-leader-mode)
  :ensure evil-leader
  :demand evil-leader
  :init
  (setq evil-leader/in-all-states 1)
  (global-evil-leader-mode)
  :config
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "h" 'evil-window-left
    "n" 'evil-window-bottom
    "e" 'evil-window-up
    "i" 'evil-window-right))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(if window-system (scroll-bar-mode -1))
(tool-bar-mode 0)
(menu-bar-mode 0)

(if (eq window-system 'ns)
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

(setq custom-safe-themes t)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

(setq-default inhibit-startup-screen t)

(display-time-mode t)

;; Time format
(customize-set-variable 'display-time-string-forms
                        '((propertize (concat dayname
                                              " " 12-hours ":" minutes " " (upcase am-pm))
                                      'help-echo (format-time-string "%a, %b %e %Y" now))))

;; Update display-time-string
(display-time-update)

(use-package telephone-line
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-utf-abs-left
        telephone-line-secondary-left-separator 'telephone-line-utf-abs-hollow-left
        telephone-line-primary-right-separator 'telephone-line-utf-abs-right
        telephone-line-secondary-right-separator 'telephone-line-utf-abs-hollow-right)
  (setq telephone-line-height 24
        telephone-line-evil-use-short-tag t)

  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-airline-position-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment
                     telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (evil   . (telephone-line-vc-segment))
          (accent . (telephone-line-major-mode-segment
                     telephone-line-flycheck-segment))))

  (telephone-line-mode t))

;; ;; Remove display-time-string from global-mode-string
;; (setq global-mode-string (delq 'display-time-string global-mode-string))

;; ;; Remove battery-mode-line-string from global-mode-string
;; (setq global-mode-string (delq 'battery-mode-line-string global-mode-string))

;; (defun *-mode-line-fill (reserve)
;;   "Return empty space using FACE and leaving RESERVE space on the right."
;;   (unless reserve
;;     (setq reserve 20))
;;   (when (and window-system
;;              (eq 'right (get-scroll-bar-mode)))
;;     (setq reserve (- reserve 3)))
;;   (propertize " "
;;               'display `((space :align-to (- (+ right right-fringe right-margin), reserve)))))

;; (setq-default mode-line-format
;;               '("%e"
;;                 mode-line-front-space
;;                 mode-line-client
;;                 mode-line-remote
;;                 mode-line-mule-info
;;                 mode-line-modified
;;                 "  "
;;                 ;; Buffer name
;;                 (:propertize mode-line-buffer-identification
;;                              face font-lock-builtin-face)
;;                 "  "
;;                 ;; Position
;;                 "%p (%l,%c)"
;;                 "  "
;;                 ;; Mode, recursive editing, and narrowing information
;;                 "("
;;                 (:propertize "%["
;;                              face font-lock-warning-face)
;;                 mode-name
;;                 (:propertize "%]"
;;                              face font-lock-warning-face)
;;                 (:eval (if (buffer-narrowed-p)
;;                            (concat " "
;;                                    (propertize "Narrow"
;;                                                'face 'font-lock-warning-face))))
;;                 ")"
;;                 ;; Version control
;;                 (:eval (when vc-mode
;;                          (concat " "
;;                                  vc-mode)))
;;                 ;; Miscellaneous information
;;                 "  "
;;                 mode-line-misc-info
;;                 (:eval (*-mode-line-fill (+ (length battery-mode-line-string)
;;                                             1
;;                                             (length display-time-string))))
;;                 battery-mode-line-string
;;                 " "
;;                 display-time-string
;;                 mode-line-end-spaces))

;; (use-package all-the-icons
;;   :demand
;;   :init
;;   (progn (defun -custom-modeline-github-vc ()
;;            (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
;;              (concat
;;               (propertize (format " %s" (all-the-icons-octicon "git-branch"))
;;                           'face `(:height 1 :family ,(all-the-icons-octicon-family))
;;                           'display '(raise 0))
;;               (propertize (format " %s" branch))
;;               (propertize "  "))))

;;          (defun -custom-modeline-svn-vc ()
;;            (let ((revision (cadr (split-string vc-mode "-"))))
;;              (concat
;;               (propertize (format " %s" (all-the-icons-faicon "cloud"))
;;                           'face `(:height 1)
;;                           'display '(raise 0))
;;               (propertize (format " %s" revision) 'face `(:height 0.9)))))

;;          (defvar mode-line-my-vc
;;            '(:propertize
;;              (:eval (when vc-mode
;;                       (cond
;;                        ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
;;                        ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
;;                        (t (format "%s" vc-mode)))))
;;              face mode-line-directory)
;;            "Formats the current directory's git information in the modeline."))
;;   :config
;;   (progn
;;     (setq-default mode-line-format
;;                   (list
;;                    "("
;;                    "%02l" "," "%02c"
;;                    ") "
;;                    mode-line-front-space
;;                    " "
;;                    mode-line-mule-info
;;                    mode-line-modified
;;                    mode-line-frame-identification
;;                    mode-line-buffer-identification
;;                    " %6 "
;;                    mode-line-modes
;;                    mode-line-my-vc
;;                    '("  " battery-mode-line-string "  " display-time-string)
;;                    ))
;;     (concat evil-mode-line-tag)))

(use-package diminish
    :ensure t
    :init
    (diminish 'undo-tree-mode)
    (diminish 'auto-revert-mode)
    (diminish 'global-auto-revert-mode)
    (diminish 'flyspell-mode))

(setq display-time-default-load-average nil)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(defun my-terminal-visible-bell ()
   "A friendlier visual bell effect."
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line))

 (setq visible-bell nil
       ring-bell-function 'my-terminal-visible-bell)

(when (window-system)
  (set-frame-font "Fira Code 14" nil t))

(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(when window-system
      (global-hl-line-mode))

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(setq-default indicate-empty-lines t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(setq vc-make-backup-files t)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(define-key global-map (kbd "RET") 'newline-and-indent)

(show-paren-mode 1)

(column-number-mode t)

(setq vc-follow-symlinks t)

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (use-package evil-magit)
  (setq git-commit-summary-max-length 50)
  (add-hook 'git-commit-mode-hook 'turn-on-flyspell)
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package company
  :diminish company-mode
  :bind (:map company-active-map
              ("M-j" . company-select-next)
              ("M-k" . company-select-previous))
  :init
  (global-company-mode t))

(setq dafny-verification-backend 'server)
(setq flycheck-dafny-executable "/Users/samarth/dafny/dafny")
(setq flycheck-boogie-executable "/Users/samarth/dafny/dafny-server")
(setq flycheck-z3-smt2-executable "/Users/samarth/dafny/z3/bin/z3")
(setq flycheck-inferior-dafny-executable "/Users/samarth/dafny/dafny-server") ;; Optional
;; (setq boogie-friends-profile-analyzer-executable "PATH-TO-Z3-AXIOM-PROFILER") ;; Optional

(use-package prettier-js
  :ensure t
  :hook
  (js2-mode . prettier-js-mode))

(use-package cider
  :ensure t)

(use-package inf-clojure
  :commands (inf-clojure))

(defun cljs-node-repl ()
  (interactive)
  (run-clojure "lein trampoline run -m clojure.main repl.clj"))

(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode))

(setq org-ellipsis "⤵")

(setq org-src-fontify-natively t)

(setq org-src-tab-acts-natively t)

(setq org-src-window-setup 'current-window)

(add-hook 'org-mode-hook 'flyspell-mode)

(defun my/org-mode-hook ()
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'semi-bold :height 1.1)))

(add-hook 'org-mode-hook 'my/org-mode-hook)
(setq solarized-scale-org-headlines nil)

(setq solarized-use-variable-pitch nil)

(setq org-directory "~/Dropbox/org/")

(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-show-log t)

(setq org-log-done 'time)

(defun set-org-agenda-files ()
  "Set different org-files to be used in org-agenda"
  (setq org-agenda-files (list (concat org-directory "tasks.org")
                               (concat org-directory "refile-beorg.org"))))

(set-org-agenda-files)

(defun tasks ()
  "Open main 'org-mode' file and start 'org-agenda' for this week."
  (interactive)
  (find-file (concat org-directory "tasks.org"))
  (set-org-agenda-files)
  (org-agenda-list)
  (org-agenda-week-view)
  (shrink-window-if-larger-than-buffer)
  (other-window 1))

(use-package evil-org
  :ensure t
  :after org
  :diminish evil-org-mode
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(setq org-export-with-smart-quotes t)

(setq org-html-postamble nil)

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind
  ("C-x C-f" . 'helm-find-files)
  ("C-x C-b" . 'helm-buffers-list)
  ("M-x" . 'helm-M-x)
  :init
  (helm-mode 1)
  (add-hook 'helm-major-mode-hook
          (lambda ()
            (setq auto-composition-mode nil))))

(require 'flycheck)

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode))

(add-to-list 'flycheck-checkers 'proselint)

(add-hook 'markdown-mode-hook #'flycheck-mode)
(add-hook 'gfm-mode-hook #'flycheck-mode)
(add-hook 'text-mode-hook #'flycheck-mode)
(add-hook 'org-mode-hook #'flycheck-mode)

(diminish 'flycheck-mode)

(setq ispell-program-name "/usr/local/bin/aspell")

(defun visit-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/configuration.org"))

(global-set-key (kbd "C-c e") 'visit-emacs-config)

(defun config-reload ()
  "Reloads ~/.emacs.d/configuration.org at runtime"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/configuration.org")))

(global-set-key (kbd "C-c r") 'config-reload)

(save-place-mode t)

(global-auto-revert-mode t)

(setq-default indent-tabs-mode nil)

(add-hook 'prog-mode-hook #'hs-minor-mode)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
    (projectile-mode 1))

(setq scroll-conservatively 100)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
    (which-key-mode))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

(add-hook 'before-save-hook '(lambda ()
                              (when (not (or (derived-mode-p 'markdown-mode)
                                             (derived-mode-p 'org-mode))
                                (delete-trailing-whitespace)))))

(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)

(setq mu4e-maildir (expand-file-name "~/Maildir"))
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-change-filenames-when-moving t) ;; fix for mbsync

(setq mu4e-drafts-folder "/gmail/drafts")
(setq mu4e-sent-folder "/gmail/sent")
(setq mu4e-trash-folder "/gmail/trash")
(setq mu4e-refile-folder "/gmail/[Gmail].All Mail")

(add-hook 'mu4e-headers-mode-hook
          (lambda ()
            (setq-local auto-composition-mode nil)))

(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/local/bin/msmtp")
; tell msmtp to choose the SMTP server according to the from field in the outgoing email
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil 't)
