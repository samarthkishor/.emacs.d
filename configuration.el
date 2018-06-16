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

(evil-mode 1)

(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)

(evil-define-key nil evil-normal-state-map
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

(if window-system (scroll-bar-mode -1))
(tool-bar-mode 0)
(menu-bar-mode 0)

(if (eq window-system 'ns)
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

(setq custom-safe-themes t)

(require 'telephone-line)
(setq telephone-line-lhs
      '((evil   . (telephone-line-evil-tag-segment))
        (accent . (telephone-line-vc-segment
                   telephone-line-erc-modified-channels-segment
                   telephone-line-process-segment))
        (nil    . (telephone-line-minor-mode-segment
                   telephone-line-buffer-segment))))
(setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (accent . (telephone-line-major-mode-segment
                   telephone-line-flycheck-segment))
        (evil   . (telephone-line-airline-position-segment))))
(telephone-line-mode t)

(use-package diminish
    :ensure t
    :init
    (diminish 'undo-tree-mode)
    (diminish 'flyspell-mode))

(setq display-time-default-load-average nil)

;; (use-package fancy-battery
;;   :ensure t
;;   :config
;;     (setq fancy-battery-show-percentage t)
;;     (setq battery-update-interval 15)
;;     (if window-system
;;       (fancy-battery-mode)
;;       (display-battery-mode)))

(when window-system
      (use-package pretty-mode
      :ensure t
      :config
      (global-pretty-mode t)))

(setq visible-bell t)

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

(setq make-backup-files nil)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

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
    (set-face-attribute face nil :weight 'semi-bold :height 1.2)))

(add-hook 'org-mode-hook 'my/org-mode-hook)

(setq org-directory "~/Dropbox/org")

(setq org-agenda-files (list "~/Dropbox/org/todo.org"
                             "~/Dropbox/org/beorg-local.org"))

(setq org-log-done 'time)

(setq org-export-with-smart-quotes t)

(setq org-html-postamble nil)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(setenv "BROWSER" "firefox")

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
  (helm-mode 1))

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

(setq-default indent-tabs-mode nil)

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

(add-hook 'before-save-hook 'delete-trailing-whitespace)
