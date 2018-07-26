(unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))

(setq use-package-verbose t)
(setq use-package-always-ensure t)

(eval-when-compile
    (require 'use-package))

(setq user-full-name "Samarth Kishor"
      user-mail-address "samarthkishor1@gmail.com")

(use-package evil-leader
  :after evil-nerd-commenter
  :commands (evil-leader-mode global-evil-leader-mode)
  :ensure evil-leader
  :demand evil-leader
  :init
  (setq evil-leader/in-all-states 1)
  :config
  (global-evil-leader-mode t)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "h"  'evil-window-left
    "n"  'evil-window-bottom
    "e"  'evil-window-up
    "i"  'evil-window-right
    "b"  'ibuffer
    "cl" 'flycheck-list-errors
    ","  'evilnc-comment-operator
    "cc" 'evilnc-comment-or-uncomment-lines))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)
  (setq evil-want-fine-undo t)
  :config
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
  (define-key evil-normal-state-map (kbd "q") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (define-key evil-normal-state-map (kbd "M-,") nil)
  (evil-define-key nil evil-normal-state-map
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(global-set-key (kbd "M-x") 'execute-extended-command)

(setq sentence-end-double-space nil)
(define-key evil-normal-state-map ")" 'forward-sentence)

(use-package evil-nerd-commenter
  :ensure t
  :requires (evil))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-mc
  :ensure t
  :after (evil)
  :init
  ;; To avoid conflicts with other packages, only keep the g* bindings
  (setq evil-mc-key-map
        (let ((map (make-sparse-keymap))
              (keys '(("grm" . evil-mc-make-all-cursors)
                      ("gru" . evil-mc-undo-all-cursors)
                      ("grs" . evil-mc-pause-cursors)
                      ("grr" . evil-mc-resume-cursors)
                      ("grf" . evil-mc-make-and-goto-first-cursor)
                      ("grl" . evil-mc-make-and-goto-last-cursor)
                      ("grh" . evil-mc-make-cursor-here)
                      ("grj" . evil-mc-make-cursor-move-next-line)
                      ("grk" . evil-mc-make-cursor-move-prev-line)
                      ("M-n" . evil-mc-make-and-goto-next-cursor)
                      ("grN" . evil-mc-skip-and-goto-next-cursor)
                      ("grP" . evil-mc-skip-and-goto-prev-cursor)
                      ("grn" . evil-mc-skip-and-goto-next-match)
                      ("grp" . evil-mc-skip-and-goto-prev-match))))
          (dolist (key-data keys)
            (evil-define-key 'normal map (kbd (car key-data)) (cdr key-data))
            (evil-define-key 'visual map (kbd (car key-data)) (cdr key-data)))
          map))
  :config
  (setq-default evil-mc-enable-bar-cursor nil)
  ;; Use a proper face for cursors
  (setq evil-mc-cursor-current-face '(:reverse-video t))
  ;; Enable globally to make vim-like bindings (ie gr*) available
  (global-evil-mc-mode 1))

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

(use-package beacon
  :defer t
  :diminish beacon-mode
  :init
  (beacon-mode 1))

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

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(column-number-mode t)

(setq vc-follow-symlinks t)

(use-package dumb-jump
  :ensure
  :bind
  (("M-g o" . dumb-jump-go-to-other-window)
   ("M-g d" . dumb-jump-go)
   ("M-g p" . dumb-jump-back)
   ("M-g q" . dumb-jump-quick-look)
   ("M-g i" . dumb-jump-go-prompt))
  :config
  (dumb-jump-mode)
  (setq dumb-jump-selector 'helm))

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

(use-package cider
  :ensure t
  :defer t)

(use-package inf-clojure
  :commands (inf-clojure))

(defun cljs-node-repl ()
  (interactive)
  (run-clojure "lein trampoline run -m clojure.main repl.clj"))

(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))

(setq dafny-verification-backend 'server)
(setq flycheck-dafny-executable "/Users/samarth/dafny/dafny")
(setq flycheck-boogie-executable "/Users/samarth/dafny/dafny-server")
(setq flycheck-z3-smt2-executable "/Users/samarth/dafny/z3/bin/z3")
(setq flycheck-inferior-dafny-executable "/Users/samarth/dafny/dafny-server") ;; Optional
;; (setq boogie-friends-profile-analyzer-executable "PATH-TO-Z3-AXIOM-PROFILER") ;; Optional

(use-package js2-mode
  :mode ("\\.js" . js2-mode)
  :interpreter ("node" . js2-mode)
  :config
  (setq js-basic-indent 2)
  (setq-default js2-basic-indent 2
                js2-basic-offset 2
                js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t)
  (setq flycheck-javascript-eslint-executable "eslint")
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  ;; turn off all warnings in js2-mode because flycheck + eslint will handle them
  (setq js2-mode-show-parse-errors t
        js2-mode-show-strict-warnings nil
        js2-strict-missing-semi-warning nil)
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook
            (lambda ()
              (flycheck-mode)
              (flycheck-select-checker "javascript-eslint"))))

(use-package js2-refactor
  :after js2-mode
  :hook ((js2-mode . js2-refactor-mode))
  :config
  ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref
  (define-key js-mode-map (kbd "M-.") nil)
  (js2r-add-keybindings-with-prefix "C-c C-r")

  ;; xref-js2 supports things like jump to definition using ag instead of tags
  ;; (use-package xref-js2
  ;;   :ensure t
  ;;   :after js2-mode)

  ;; (add-hook 'js2-mode-hook (lambda ()
  ;;                            (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  )

(use-package tern
  :ensure t
  :after js2-mode
  :init
  (add-hook 'js2-mode-hook (lambda () (tern-mode)))
  :config
  ;; (define-key tern-mode-keymap (kbd "M-.") nil)
  ;; (define-key tern-mode-keymap (kbd "M-,") nil)
  (use-package company-tern
    :ensure t
    :init (add-to-list 'company-backends 'company-tern)))

(use-package prettier-js
  :ensure t
  :after js2-mode
  :hook ((js2-mode . prettier-js-mode)))

;; (use-package paredit
;;   :ensure t
;;   :commands (enable-paredit-mode paredit-mode)
;;   :diminish paredit-mode
;;   :init
;;   (add-hook 'clojure-mode-hook #'paredit-mode)
;;   (add-hook 'cider-mode-hook #'paredit-mode))

;; (use-package evil-paredit
;;   :ensure t
;;   :commands (evil-paredit-mode))

(use-package smartparens
  :ensure t
  :diminish
  :init
  (smartparens-global-mode 1))

(use-package evil-smartparens
  :ensure t
  :diminish
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package lsp-mode
  :ensure t
  :config
  ;; make sure we have lsp-imenu everywhere we have LSP
  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  ;; get lsp-python-enable defined
  ;; NB: use either projectile-project-root or ffip-get-project-root-directory
  ;;     or any other function that can be used to find the root directory of a project
  (lsp-define-stdio-client lsp-python "python"
                           #'projectile-project-root
                           '("pyls"))

  ;; make sure this is activated when python-mode is activated
  ;; lsp-python-enable is created by macro above
  (add-hook 'python-mode-hook
            (lambda ()
              (lsp-python-enable)))

  ;; lsp extras
  (use-package lsp-ui
    :ensure t
    :config
    (setq lsp-ui-sideline-ignore-duplicate t)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode))

  (use-package company-lsp
    :config
    (push 'company-lsp company-backends))

  ;; format file on save
  (add-hook 'before-save-hook 'lsp-format-buffer))

;; (use-package pipenv
;;   :hook (python-mode . pipenv-mode)
;;   :init
;;   (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))

(use-package pyvenv
  :ensure t
  :commands
  (pyvenv-activate pyvenv-workon))

(setq python-shell-interpreter "ipython")

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

(setq org-default-notes-file (concat org-directory "/tasks.org"))
(define-key global-map "\C-cc" 'org-capture)

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

(use-package tablist
  :ensure t)

(use-package pdf-tools
  :pin manual
  :magic ("%PDF" . pdf-view-mode)
  :init
  (pdf-tools-install)
  :config
  (setq pdf-view-display-size 'fit-width
        pdf-view-use-scaling t
        pdf-view-resize-factor 1.25)
  (setq pdf-annot-activate-created-annotations t)

  (defun my/pdf-set-last-viewed-bookmark ()
    (interactive)
    (when (eq major-mode 'pdf-view-mode)
      (bookmark-set (my/pdf-generate-bookmark-name))))

  (defun my/pdf-jump-last-viewed-bookmark ()
    (bookmark-set "fake")
    (when
        (my/pdf-has-last-viewed-bookmark)
      (bookmark-jump (my/pdf-generate-bookmark-name))))

  (defun my/pdf-has-last-viewed-bookmark ()
    (assoc
     (my/pdf-generate-bookmark-name) bookmark-alist))

  (defun my/pdf-generate-bookmark-name ()
    (concat "PDF-LAST-VIEWED: " (buffer-file-name)))

  (defun my/pdf-set-all-last-viewed-bookmarks ()
    (dolist (buf (buffer-list))
      (with-current-buffer (and (buffer-name buf) buf)
        (my/pdf-set-last-viewed-bookmark))))

  (add-hook 'kill-buffer-hook 'my/pdf-set-last-viewed-bookmark)
  (add-hook 'pdf-view-mode-hook 'my/pdf-jump-last-viewed-bookmark)
  (unless noninteractive  ; as `save-place-mode' does
    (add-hook 'kill-emacs-hook #'my/pdf-set-all-last-viewed-bookmarks)))

(use-package org-pdfview
  :ensure t
  :init
  (org-link-set-parameters "pdfview" :export #'org-pdfview-export)
  (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link))))
  (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . (lambda (file link) (org-pdfview-open link)))))

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

(use-package flycheck
  :ensure t
  :diminish
  :config
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
    (add-to-list 'flycheck-checkers 'proselint))

(add-hook 'markdown-mode-hook #'flycheck-mode)
(add-hook 'gfm-mode-hook #'flycheck-mode)
(add-hook 'text-mode-hook #'flycheck-mode)
(add-hook 'org-mode-hook #'flycheck-mode)

(setq ispell-program-name "/usr/local/bin/aspell")

(use-package synosaurus
  :ensure t
  :bind
  (("C-c C-h l" . synosaurus-lookup)
   ("C-c C-h r" . synosaurus-choose-and-replace))
  :config
  (setq synosaurus-backend 'synosaurus-backend-wordnet)
  (setq synosaurus-choose-method 'default))

(use-package typo
  :defer t
  :diminish
  :config
  (typo-global-mode 1)
  (add-hook 'text-mode-hook 'typo-mode))

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
;; Enable inline images.
(setq mu4e-view-show-images t)
(setq mu4e-view-image-max-width 800)
;; Use imagemagick, if available.
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(setq mu4e-view-show-addresses t)

(add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser) t)

(setq w3m-default-desplay-inline-images t)
(defun mu4e-action-view-in-w3m ()
  "View the body of the message in emacs w3m."
  (interactive)
  (w3m-browse-url (concat "file://"
                          (mu4e~write-body-to-html (mu4e-message-at-point t)))))

(add-hook 'mu4e-mark-execute-pre-hook
          (lambda (mark msg)
            (cond ((member mark '(refile trash)) (mu4e-action-retag-message msg "-\\Inbox"))
                  ((equal mark 'flag) (mu4e-action-retag-message msg "\\Starred"))
                  ((equal mark 'unflag) (mu4e-action-retag-message msg "-\\Starred")))))

(defun mu4e-message-maildir-matches (msg rx)
  "Determine which account context I am in based on the maildir subfolder"
  (when rx
    (if (listp rx)
        ;; If rx is a list, try each one for a match
        (or (mu4e-message-maildir-matches msg (car rx))
            (mu4e-message-maildir-matches msg (cdr rx)))
      ;; Not a list, check rx
      (string-match rx (mu4e-message-field msg :maildir)))))

(defun choose-msmtp-account ()
  "Choose account label to feed msmtp -a option based on From header
  in Message buffer; This function must be added to
  message-send-mail-hook for on-the-fly change of From address before
  sending message since message-send-mail-hook is processed right
  before sending message."
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (save-restriction
                     (message-narrow-to-headers)
                     (message-fetch-field "from")))
             (account
              (cond
               ((string-match "samarthkishor1@gmail.com" from) "gmail")
               ((string-match "sk4gz@virginia.edu" from) "uva"))))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))

(add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "gmail"
           :enter-func (lambda () (mu4e-message "Switch to the gmail context"))
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-maildir-matches msg "^/gmail")))
           :leave-func (lambda () (mu4e-clear-caches))
           :vars '((user-mail-address     . "samarthkishor1@gmail.com")
                   (user-full-name        . "Samarth Kishor")
                   (mu4e-sent-folder      . "/gmail/sent")
                   (mu4e-drafts-folder    . "/gmail/drafts")
                   (mu4e-trash-folder     . "/gmail/trash")
                   (mu4e-refile-folder    . "/gmail/[Gmail].All Mail")))
         ,(make-mu4e-context
           :name "uva"
           :enter-func (lambda () (mu4e-message "Switch to the UVA context"))
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-maildir-matches msg "^/uva")))
           :leave-func (lambda () (mu4e-clear-caches))
           :vars '((user-mail-address     . "sk4gz@virginia.edu")
                   (user-full-name        . "Samarth Kishor")
                   (mu4e-sent-folder      . "/uva/sent")
                   (mu4e-drafts-folder    . "/uva/drafts")
                   (mu4e-trash-folder     . "/uva/trash")
                   (mu4e-refile-folder    . "/uva/[Gmail].All Mail")))))

(add-hook 'mu4e-headers-mode-hook
          (lambda ()
            (setq-local auto-composition-mode nil)))

(setq mu4e-sent-messages-behavior 'delete)

(use-package visual-fill-column
  :ensure t)

(add-hook 'mu4e-view-mode-hook #'visual-line-mode)
(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (set-fill-column 80)
            (auto-fill-mode 0)
            (visual-fill-column-mode)
            (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
            (visual-line-mode)))

(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/local/bin/msmtp")
(setq user-full-name "Samarth Kishor")

; tell msmtp to choose the SMTP server according to the "from" field in the outgoing email
(setq message-sendmail-envelope-from 'header)
(add-hook 'message-send-mail-hook 'choose-msmtp-account)
;; (setq message-sendmail-f-is-evil 't)

(require 'org-mu4e)
(setq org-mu4e-link-query-in-headers-mode nil)
(setq org-mu4e-convert-to-html t)

(setq org-capture-templates
      `(("t" "TODO" entry (file+headline "~/Dropbox/org/tasks.org" "Tasks")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))
