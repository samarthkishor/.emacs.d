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
  :after (evil-commentary projectile-mode)
  :commands (evil-leader-mode global-evil-leader-mode)
  :ensure evil-leader
  :demand evil-leader
  :init
  (setq evil-leader/in-all-states 1)
  :config
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "h"  'evil-window-left
    "j"  'evil-window-bottom
    "k"  'evil-window-up
    "l"  'evil-window-right
    "b"  'ivy-switch-buffer
    "m"  'counsel-imenu
    "e" 'flycheck-list-errors
    "gg" 'magit-status
    "pp" 'counsel-projectile-switch-project))_

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)
  :config
  (defun prelude-shift-left-visual ()
    "Shift left and restore visual selection."
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))
  (defun prelude-shift-right-visual ()
    "Shift right and restore visual selection."
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))
  (use-package evil-numbers :ensure t)
  (setq evil-want-fine-undo t)
  (setq evil-shift-width 2)
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  (define-key evil-normal-state-map (kbd "q") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (define-key evil-normal-state-map (kbd "M-,") nil)
  (define-key evil-normal-state-map (kbd "C-A") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-S-A") 'evil-numbers/dec-at-pt)
  (define-key evil-visual-state-map (kbd ">") 'prelude-shift-right-visual)
  (define-key evil-visual-state-map (kbd "<") 'prelude-shift-left-visual)
  (evil-define-key nil evil-normal-state-map
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)
  (evil-declare-key 'normal org-mode-map
    "gk" 'outline-up-heading
    "gj" 'outline-next-visible-heading
    "H" 'org-beginning-of-line ; smarter behavior on headlines etc.
    "L" 'org-end-of-line ; smarter behavior on headlines etc.
    "t" 'org-todo ; mark a TODO item as DONE
    ",c" 'org-cycle
    (kbd "TAB") 'org-cycle
    "$" 'org-end-of-line ; smarter behavior on headlines etc.
    "^" 'org-beginning-of-line ; ditto
    "-" 'org-ctrl-c-minus ; change bullet style
    "<" 'org-metaleft ; out-dent
    ">" 'org-metaright) ; indent
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :defer t
  :config
  (evil-collection-init))

(global-set-key (kbd "M-x") 'execute-extended-command)

(setq sentence-end-double-space nil)
(define-key evil-normal-state-map ")" 'forward-sentence)

(use-package evil-commentary
  :ensure t
  :requires (evil)
  :hook (prog-mode . evil-commentary-mode))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-mc
  :ensure t
  :defer t
  :diminish
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

(use-package evil-lion
  :ensure t
  :after (evil)
  :config
  (evil-lion-mode))

(mapc (lambda (mode)
        (evil-set-initial-state mode 'emacs))
      '(dired-mode
        image-dired-mode
        image-dired-thumbnail-mode
        eww-mode))

(use-package solarized-theme
  :ensure t
  :config (load-theme 'solarized-dark t))

(if window-system (scroll-bar-mode -1))
(tool-bar-mode 0)

(if (eq window-system 'ns)
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

(setq custom-safe-themes t)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

(setq-default inhibit-startup-screen t)

;; (use-package powerline
;;   :if window-system
;;   :config (setq-default powerline-default-separator 'nil))

;; (use-package spaceline
;;   :after powerline
;;   :ensure t
;;   :config
;;   (setq spaceline-responsive nil))

;; (use-package spaceline-all-the-icons
;;   :after spaceline
;;   :config
;;   (setq spaceline-all-the-icons-icon-set-modified 'circle
;;         spaceline-all-the-icons-icon-set-flycheck-slim 'dots
;;         spaceline-all-the-icons-separator-type 'none
;;         spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
;;         spaceline-all-the-icons-flycheck-alternate t)
;;   (spaceline-all-the-icons-theme)
;;   (spaceline-toggle-all-the-icons-projectile-on)
;;   (spaceline-toggle-all-the-icons-buffer-position-on)
;;   (spaceline-helm-mode)
;;   (spaceline-toggle-all-the-icons-minor-modes-off))

(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-init)
  :config
  (setq doom-modeline-height 20)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project))

(defun remove-mode-line-box ()
  (set-face-attribute 'mode-line nil :box nil :underline nil)
  (set-face-attribute 'mode-line-inactive nil :box nil :underline nil))

(when (window-system)
  (remove-mode-line-box))

(use-package diminish
  :ensure t
  :init
  (diminish 'undo-tree-mode)
  (diminish 'auto-revert-mode)
  (diminish 'global-auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'hs-minor-mode)
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

;; (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
;;                (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
;;                (36 . ".\\(?:>\\)")
;;                (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
;;                (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
;;                (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
;;                (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
;;                (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
;;                ;; (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)") commenting this line should fix a problem with CIDER
;;                (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
;;                (48 . ".\\(?:x[a-zA-Z]\\)")
;;                (58 . ".\\(?:::\\|[:=]\\)")
;;                (59 . ".\\(?:;;\\|;\\)")
;;                (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
;;                (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
;;                (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
;;                (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
;;                (91 . ".\\(?:]\\)")
;;                (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
;;                (94 . ".\\(?:=\\)")
;;                (119 . ".\\(?:ww\\)")
;;                (123 . ".\\(?:-\\)")
;;                (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
;;                (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
;;                )
;;              ))
;;   (dolist (char-regexp alist)
;;     (set-char-table-range composition-function-table (car char-regexp)
;;                           `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; (add-hook 'after-make-frame-functions
;;           (lambda (frame) (set-fontset-font t
;;                                             '(#Xe100 . #Xe16f) "Fira Code Symbol")))
;; (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")

;; (defconst fira-code-font-lock-keywords-alist
;;   (mapcar (lambda (regex-char-pair)
;;             `(,(car regex-char-pair)
;;               (0 (prog1 ()
;;                    (compose-region (match-beginning 1)
;;                                    (match-end 1)
;;                                    ,(concat "  "
;;                                             (list
;;                                              (decode-char 'ucs
;;                                                           (cadr regex-char-pair)))))))))
;;           '(("\\(www\\)"                   #Xe100)
;;             ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
;;             ("\\(\\*\\*\\*\\)"             #Xe102)
;;             ("\\(\\*\\*/\\)"               #Xe103)
;;             ("\\(\\*>\\)"                  #Xe104)
;;             ("[^*]\\(\\*/\\)"              #Xe105)
;;             ("\\(\\\\\\\\\\)"              #Xe106)
;;             ("\\(\\\\\\\\\\\\\\)"          #Xe107)
;;             ("\\({-\\)"                    #Xe108)
;;             ("\\(:::\\)"                   #Xe10b)
;;             ("[^=]\\(:=\\)"                #Xe10c)
;;             ;; ("\\(!!\\)"                    #Xe10d)
;;             ;; this should be hooked to sml-mode only
;;             ("\\(<>\\)"                    #Xe10e)
;;             ("\\(!=\\)"                    #Xe10e)
;;             ("\\(!==\\)"                   #Xe10f)
;;             ("\\(-}\\)"                    #Xe110)
;;             ("\\(--\\)"                    #Xe111)
;;             ("\\(---\\)"                   #Xe112)
;;             ("\\(-->\\)"                   #Xe113)
;;             ("[^-]\\(->\\)"                #Xe114)
;;             ("\\(->>\\)"                   #Xe115)
;;             ("\\(-<\\)"                    #Xe116)
;;             ("\\(-<<\\)"                   #Xe117)
;;             ("\\(-~\\)"                    #Xe118)
;;             ;; ("\\(#{\\)"                    #Xe119)
;;             ("\\(#\\[\\)"                  #Xe11a)
;;             ("\\(##\\)"                    #Xe11b)
;;             ("\\(###\\)"                   #Xe11c)
;;             ("\\(####\\)"                  #Xe11d)
;;             ("\\(#(\\)"                    #Xe11e)
;;             ("\\(#\\?\\)"                  #Xe11f)
;;             ("\\(#_\\)"                    #Xe120)
;;             ("\\(#_(\\)"                   #Xe121)
;;             ("\\(\\.-\\)"                  #Xe122)
;;             ("\\(\\.=\\)"                  #Xe123)
;;             ("\\(\\.\\.\\)"                #Xe124)
;;             ("\\(\\.\\.<\\)"               #Xe125)
;;             ("\\(\\.\\.\\.\\)"             #Xe126)
;;             ("\\(\\?=\\)"                  #Xe127)
;;             ("\\(\\?\\?\\)"                #Xe128)
;;             ("\\(;;\\)"                    #Xe129)
;;             ("\\(/\\*\\)"                  #Xe12a)
;;             ("\\(/\\*\\*\\)"               #Xe12b)
;;             ("\\(/=\\)"                    #Xe12c)
;;             ("\\(/==\\)"                   #Xe12d)
;;             ("\\(/>\\)"                    #Xe12e)
;;             ("\\(//\\)"                    #Xe12f)
;;             ("\\(///\\)"                   #Xe130)
;;             ("\\(&&\\)"                    #Xe131)
;;             ("\\(||\\)"                    #Xe132)
;;             ("\\(||=\\)"                   #Xe133)
;;             ("[^|]\\(|=\\)"                #Xe134)
;;             ("\\(|>\\)"                    #Xe135)
;;             ("\\(\\^=\\)"                  #Xe136)
;;             ("\\(\\$>\\)"                  #Xe137)
;;             ("\\(\\+\\+\\)"                #Xe138)
;;             ("\\(\\+\\+\\+\\)"             #Xe139)
;;             ("\\(\\+>\\)"                  #Xe13a)
;;             ("\\(=:=\\)"                   #Xe13b)
;;             ("[^!/]\\(==\\)[^>]"           #Xe13c)
;;             ("\\(===\\)"                   #Xe13d)
;;             ("\\(==>\\)"                   #Xe13e)
;;             ("[^=]\\(=>\\)"                #Xe13f)
;;             ("\\(=>>\\)"                   #Xe140)
;;             ("\\(<=\\)"                    #Xe141)
;;             ("\\(=<<\\)"                   #Xe142)
;;             ("\\(=/=\\)"                   #Xe143)
;;             ("\\(>-\\)"                    #Xe144)
;;             ("\\(>=\\)"                    #Xe145)
;;             ("\\(>=>\\)"                   #Xe146)
;;             ("[^-=]\\(>>\\)"               #Xe147)
;;             ("\\(>>-\\)"                   #Xe148)
;;             ("\\(>>=\\)"                   #Xe149)
;;             ("\\(>>>\\)"                   #Xe14a)
;;             ("\\(<\\*\\)"                  #Xe14b)
;;             ("\\(<\\*>\\)"                 #Xe14c)
;;             ("\\(<|\\)"                    #Xe14d)
;;             ("\\(<|>\\)"                   #Xe14e)
;;             ("\\(<\\$\\)"                  #Xe14f)
;;             ("\\(<\\$>\\)"                 #Xe150)
;;             ("\\(<!--\\)"                  #Xe151)
;;             ("\\(<-\\)"                    #Xe152)
;;             ("\\(<--\\)"                   #Xe153)
;;             ("\\(<->\\)"                   #Xe154)
;;             ("\\(<\\+\\)"                  #Xe155)
;;             ("\\(<\\+>\\)"                 #Xe156)
;;             ("\\(<=\\)"                    #Xe157)
;;             ("\\(<==\\)"                   #Xe158)
;;             ("\\(<=>\\)"                   #Xe159)
;;             ("\\(<=<\\)"                   #Xe15a)
;;             ;; ("\\(<>\\)"                    #Xe15b)
;;             ("[^-=]\\(<<\\)"               #Xe15c)
;;             ("\\(<<-\\)"                   #Xe15d)
;;             ("\\(<<=\\)"                   #Xe15e)
;;             ("\\(<<<\\)"                   #Xe15f)
;;             ("\\(<~\\)"                    #Xe160)
;;             ("\\(<~~\\)"                   #Xe161)
;;             ("\\(</\\)"                    #Xe162)
;;             ("\\(</>\\)"                   #Xe163)
;;             ("\\(~@\\)"                    #Xe164)
;;             ("\\(~-\\)"                    #Xe165)
;;             ("\\(~=\\)"                    #Xe166)
;;             ("\\(~>\\)"                    #Xe167)
;;             ("[^<]\\(~~\\)"                #Xe168)
;;             ("\\(~~>\\)"                   #Xe169)
;;             ("\\(%%\\)"                    #Xe16a)
;;             ("[^:=]\\(:\\)[^:=]"           #Xe16c)
;;             ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
;;             ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

;; (defun add-fira-code-symbol-keywords ()
;;   "Add the Fira Code ligatures from Fira Code Symbol to selected keywords."
;;   (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

;; (add-hook 'prog-mode-hook #'add-fira-code-symbol-keywords)

(when window-system
  (global-hl-line-mode))

(use-package beacon
  :ensure t
  :defer t
  :diminish beacon-mode
  :init
  (beacon-mode 1))

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(setq-default indicate-empty-lines t)

(use-package writeroom-mode
  :ensure t
  :defer t)

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
  (projectile-mode 1)
  :config
  (setq projectile-enable-caching nil)
  (add-to-list 'projectile-globally-ignored-directories "*.cquery_cached_index")
  (add-to-list 'projectile-globally-ignored-directories "*node_modules"))

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
                                              (derived-mode-p 'org-mode)))
                                 (delete-trailing-whitespace))))

(defun my/normalize-buffer ()
  "Delete extra whitespace, tabs -> spaces, and indent buffer"
  (interactive)
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max)))

(use-package avy
  :ensure t
  :bind (("s-," . avy-goto-word-or-subword-1)
         ("s-." . avy-goto-char)))

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta
        mac-command-modifier 'control
        mac-control-modifier 'super
        mac-right-command-modifier 'super
        mac-right-option-modifier 'none))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("M-=" . er/contract-region)))

(defun my/insert-date ()
  "Function to insert date into buffer."
  (interactive)
  (insert (format-time-string
           "%m/%m/%Y" (current-time))))

(add-hook 'prog-mode-hook #'visual-line-mode)
(add-hook 'org-mode #'visual-line-mode)

(use-package iedit
  :ensure t
  :defer t
  :after evil-leader
  :config
  (evil-leader/set-key "r" 'iedit-mode))

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(setq vc-make-backup-files t)

(setq create-lockfiles nil)

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

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs
                                 '("~/.emacs.d/snippets/")))
  (yas-global-mode 1))

(use-package yasnippet-snippets :ensure t)

(global-eldoc-mode -1)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (use-package evil-magit)
  (setq git-commit-summary-max-length 50)
  (add-hook 'git-commit-mode-hook 'turn-on-flyspell)
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package company
  :ensure t
  :diminish company-mode
  :bind (:map company-active-map
              ("M-j" . company-select-next)
              ("M-k" . company-select-previous))
  :init
  (global-company-mode t))

(use-package eglot
  :ensure t
  :defer t
  :config
  (add-to-list 'eglot-server-programs '((c++ mode c-mode) . (eglot-cquery "cquery"))))

;; (use-package cquery
;;   :ensure t
;;   :defer t
;;   :commands (lsp-cquery-enable)
;;   :hook (c-mode-common . lsp-cquery-enable)
;;   :config
;;   (setq cquery-executable "/usr/local/bin/cquery"))

(setq-default c-basic-offset 4)
(defvar astyle-command "astyle --align-pointer=type -A2 -s4 -S")

(defun astyle-buffer (start end)
  "Run astyle on region or buffer"
  (interactive (if mark-active
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (save-restriction
    (shell-command-on-region start end
                             astyle-command
                             (current-buffer) t
                             (get-buffer-create "*Astyle Errors*") t)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (add-hook 'before-save-hook 'astyle-buffer)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (unless (file-exists-p "Makefile")
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (concat "clang++ -Wall -g -o "
                             (file-name-sans-extension file)
                             " " file))))))

(use-package cider
  :ensure t
  :defer t
  :commands (cider cider-connect cider-jack-in)
  :init
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  :config
  (setq cider-repl-use-pretty-printing t)
  (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
  (setq cider-boot-parameters "dev"))

(use-package clj-refactor
  :ensure t
  :defer t
  :diminish
  :init
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (setq cljr-warn-on-eval nil)
              (yas-minor-mode 1)
              (setq auto-composition-mode nil) ;; Fira Code causes CIDER to hang
              ;; leaves cider-macroexpand-1 unbound
              (cljr-add-keybindings-with-prefix "C-c C-m"))))

(use-package inf-clojure
  :ensure t
  :defer t
  :config
  (defun my/lumo ()
    (when (bound-and-true-p cider-mode)
      (cider-mode -1))
    (setq inf-clojure-program     "lumo -d"
          inf-clojure-generic-cmd "lumo -d"
          inf-clojure-lein-cmd    "lumo -d"))
  (add-hook 'inf-clojure-mode-hook 'my/lumo)
  (add-hook 'inf-clojure-minor-mode-hook 'my/lumo))

(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))

(use-package slime
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program (shell-cmd "which sbcl"))
  (setq slime-contribs '(slime-fancy)))

(setq dafny-verification-backend 'server)
(setq flycheck-dafny-executable "/Users/samarth/dafny/dafny")
(setq flycheck-boogie-executable "/Users/samarth/dafny/dafny-server")
(setq flycheck-z3-smt2-executable "/Users/samarth/dafny/z3/bin/z3")
(setq flycheck-inferior-dafny-executable "/Users/samarth/dafny/dafny-server") ;; Optional
;; (setq boogie-friends-profile-analyzer-executable "PATH-TO-Z3-AXIOM-PROFILER") ;; Optional

(use-package js2-mode
  :ensure t
  :defer t
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
  (js2r-add-keybindings-with-prefix "C-c C-r"))

;; xref-js2 supports things like jump to definition using ag instead of tags
;; (use-package xref-js2
;;   :ensure t
;;   :after js2-mode)

;; (add-hook 'js2-mode-hook (lambda ()
;;                            (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

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

(use-package lean-mode
  :ensure t
  :defer t
  :custom
  (lean-rootdir "~/lean-3.4.0-darwin"))

(use-package company-lean
  :ensure t
  :defer t)

(use-package helm-lean
  :ensure t
  :defer t)

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
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  :config
  (defun my-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  (setq sp-escape-quotes-after-insert nil)
  (sp-local-pair 'c++-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'c-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET"))))

(use-package evil-smartparens
  :ensure t
  :diminish
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

;; (use-package lsp-mode
;;   :ensure t
;;   :defer t
;;   :config
;;   ;; make sure we have lsp-imenu everywhere we have LSP
;;   (require 'lsp-imenu)
;;   (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
;;   ;; get lsp-python-enable defined
;;   ;; NB: use either projectile-project-root or ffip-get-project-root-directory
;;   ;;     or any other function that can be used to find the root directory of a project
;;   (lsp-define-stdio-client lsp-python "python"
;;                            #'projectile-project-root
;;                            '("pyls"))

;;   ;; make sure this is activated when python-mode is activated
;;   ;; lsp-python-enable is created by macro above
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;               (lsp-python-enable)))

;;   ;; lsp extras
;;   (use-package lsp-ui
;;     :ensure t
;;     :defer t
;;     :config
;;     (setq lsp-ui-sideline-ignore-duplicate t)
;;     (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;;   (use-package company-lsp
;;     :ensure t
;;     :defer t
;;     :config
;;     (push 'company-lsp company-backends)))

;; (use-package pipenv
;;   :hook (python-mode . pipenv-mode)
;;   :init
;;   (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))

(use-package pyvenv
  :ensure t
  :defer t
  :commands
  (pyvenv-activate pyvenv-workon))

(setq python-shell-interpreter "ipython")

(use-package hy-mode
  :ensure t
  :defer t
  :mode ("\\.hy\\'" . hy-mode)
  :config
  (define-key hy-mode-map "\C-x\C-e" 'hy-shell-eval-last-sexp)
  (setq hy-mode-inferior-lisp-command "hy"))

(defun shell-cmd (cmd)
  "Returns the stdout output of a shell command or nil if the command returned
   an error"
  (car (ignore-errors (apply 'process-lines (split-string cmd)))))

(defun reason-cmd-where (cmd)
  (let ((where (shell-cmd cmd)))
    (if (not (string-equal "unknown flag ----where" where))
        where)))

(use-package reason-mode
  :ensure t
  :config
  (let* ((refmt-bin (or (reason-cmd-where "refmt ----where")
                        (shell-cmd "which refmt")))
         (merlin-bin (or (reason-cmd-where "ocamlmerlin ----where")
                         (shell-cmd "which ocamlmerlin")))
         (merlin-base-dir (when merlin-bin
                            (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin))))
    ;; Add merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
    (when merlin-bin
      (add-to-list 'load-path (concat merlin-base-dir "share/emacs/site-lisp/"))
      (setq merlin-command merlin-bin))

    (when refmt-bin
      (setq refmt-command refmt-bin))))

(use-package merlin
  :ensure t
  :custom
  (merlin-command 'opam)
  (merlin-completion-with-doc t)
  (company-quickhelp-mode t)
  :bind (:map merlin-mode-map
              ("M-." . merlin-locate)
              ("M-," . merlin-pop-stack)
              ("C-c C-o" . merlin-occurrences)
              ("C-c C-j" . merlin-jump)
              ("C-c i" . merlin-locate-ident)
              ("C-c C-e" . merlin-iedit-occurrences))
  :hook
  (reason-mode . merlin-mode)
  (tuareg-mode . merlin-mode)
  (caml-mode-hook . merlin-mode)
  :config
  (add-hook 'reason-mode-hook (lambda ()
                                (add-hook 'before-save-hook 'refmt-before-save)
                                (merlin-mode)))
  ;; Make company aware of merlin
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'merlin-company-backend)))

;; (use-package merlin-eldoc
;;   :ensure t
;;   :after merlin
;;   :custom
;;   (eldoc-echo-area-use-multiline-p t) ; use multiple lines when necessary
;;   (merlin-eldoc-max-lines 8)          ; but not more than 8
;;   :bind (:map merlin-mode-map
;;               ("C-c m p" . merlin-eldoc-jump-to-prev-occurrence)
;;               ("C-c m n" . merlin-eldoc-jump-to-next-occurrence))
;;   :hook ((reason-mode tuareg-mode caml-mode) . merlin-eldoc-setup))

(use-package flycheck-ocaml
  :ensure t
  :config
  (add-hook 'tuareg-mode-hook
            (lambda ()
              ;; disable Merlin's own error checking
              (setq-local merlin-error-after-save nil)
              ;; enable Flycheck checker
              (flycheck-ocaml-setup))))

(use-package utop
  :config
  (defun utop-opam-utop ()
    (progn
      (setq-local utop-command "opam config exec -- utop -emacs")
      'utop-minor-mode))
  (defun reason/rtop-prompt ()
    "The rtop prompt function."
    (let ((prompt (format "rtop[%d]> " utop-command-number)))
      (add-text-properties 0 (length prompt) '(face utop-prompt) prompt)
      prompt))
  (defun utop-reason-cli-rtop ()
    (progn
      (setq-local utop-command (concat (shell-cmd "which rtop") " -emacs"))
      (setq-local utop-prompt 'reason/rtop-prompt)
      'utop-minor-mode))
  :hook
  (tuareg-mode . utop-opam-utop)
  (reason-mode . utop-reason-cli-rtop))

(use-package org-bullets
  :ensure t
  :defer t
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode))

(setq org-ellipsis "⤵")

(setq org-src-fontify-natively t)

(setq org-src-tab-acts-natively t)

(setq org-src-window-setup 'current-window)

(add-hook 'org-mode-hook 'flyspell-mode)

;; (defun my/org-mode-hook ()
;;   (dolist (face '(org-level-1
;;                   org-level-2
;;                   org-level-3
;;                   org-level-4
;;                   org-level-5))
;;     (set-face-attribute face nil :weight 'semi-bold :height 1.1)))

;; (add-hook 'org-mode-hook 'my/org-mode-hook)
(setq solarized-scale-org-headlines t)

(setq solarized-use-variable-pitch nil)

(setq org-refile-targets '((nil :maxlevel . 1)
                           (org-agenda-files :maxlevel . 1)))

(add-hook 'org-mode-hook 'visual-line-mode)

(setq org-directory "~/Dropbox/org/")

(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-show-log t)

(setq org-log-done 'time)

(defun set-org-agenda-files ()
  "Set different org-files to be used in org-agenda"
  (setq org-agenda-files (list (concat org-directory "tasks.org")
                               (concat org-directory "refile-beorg.org")
                               (concat org-directory "homework.org"))))

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

;; (setq org-latex-pdf-process
;;       '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(setq org-latex-pdf-process '("xelatex -shell-escape %f" "biber %b" "xelatex -shell-escape %f" "xelatex -shell-escape %f"))
(setq bibtex-dialect 'biblatex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(use-package tablist
  :ensure t)

(use-package pdf-tools
  :load-path (lambda () (expand-file-name "bin/pdf-tools-20180428.827/"))
  :pin manual
  :magic ("%PDF" . pdf-view-mode)
  :init
  (pdf-tools-install)
  :config
  (custom-set-variables
   '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
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
  :defer t
  :init
  (org-link-set-parameters "pdfview" :export #'org-pdfview-export)
  (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link))))
  (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . (lambda (file link) (org-pdfview-open link)))))

(use-package org-ref
  :ensure t
  :defer t
  :config
  (setq reftex-default-bibliography '("~/Documents/Second_Year/RELG3559/paper1/bibliography.bib"))
  (setq bibtex-completion-bibliography "~/Documents/Second_Year/RELG3559/paper1/bibliography.bib"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   (C . t)
   (lisp . t)
   (js . t)))

(setq org-babel-python-command "python3")

;; (use-package helm
;;   :ensure t
;;   :diminish helm-mode
;;   :bind
;;   ("C-x C-f" . 'helm-find-files)
;;   ("C-x C-b" . 'helm-buffers-list)
;;   ("C-x b"   . 'helm-mini)
;;   ("M-x"     . 'helm-M-x)
;;   :custom
;;   (helm-buffers-fuzzy-matching t)
;;   (helm-recentf-fuzzy-match t)
;;   (helm-projectile-fuzzy-match t)
;;   (helm-imenu-fuzzy-match t)
;;   :init
;;   (helm-mode 1)
;;   (add-hook 'helm-major-mode-hook
;;             (lambda ()
;;               (setq auto-composition-mode nil))))

;; (use-package swiper-helm
;;   :ensure t
;;   :bind ("C-s" . swiper-helm))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "")
  (setq ivy-initial-inputs-alist nil)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package counsel
  :ensure t)

(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1)
  :config
  '(counsel-M-x
    (:columns
     ((counsel-M-x-transformer (:width 40))  ; the original transfomer
      (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the command
    counsel-describe-function
    (:columns
     ((counsel-describe-function-transformer (:width 40))  ; the original transformer
      (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the function
    counsel-describe-variable
    (:columns
     ((counsel-describe-variable-transformer (:width 40))  ; the original transformer
      (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return the docstring of the variable
    counsel-recentf
    (:columns
     ((ivy-rich-candidate (:width 0.8)) ; return the candidate itself
      (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))))) ; return the last modified time of the file

(use-package all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setup))

(use-package hydra
  :ensure t
  :after evil-leader
  :init
  (evil-leader/set-key (kbd "s")
    (defhydra hydra-smartparens (:hint nil)
      "
      ^Nav^            ^Barf/Slurp^                 ^Depth^
      ^───^────────────^──────────^─────────────────^─────^────────────────
      _f_: forward     _→_:          slurp forward   _s_: splice
      _b_: backward    _←_:          barf forward    _R_: raise
      _u_: backward ↑  _C-<right>_:  slurp backward  _↑_: raise backward
      _d_: forward ↓   _C-<left>_:   barf backward   _↓_: raise forward
      _p_: backward ↓
      _n_: forward ↑

      ^Kill^           ^Misc^                       ^Wrap^
      ^────^───────────^────^───────────────────────^────^─────────────────
      _w_: copy        _j_: join                    _(_: wrap with ( )
      _k_: kill        _S_: split                   _{_: wrap with { }
      ^^               _t_: transpose               _'_: wrap with ' '
      ^^               _c_: convolute               _\"_: wrap with \" \"
      ^^               _i_: indent defun            _r_: rewrap
      "
      ("q" nil)
      ;; Wrapping
      ("(" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")))
      ("{" (lambda (_) (interactive "P") (sp-wrap-with-pair "{")))
      ("'" (lambda (_) (interactive "P") (sp-wrap-with-pair "'")))
      ("\"" (lambda (_) (interactive "P") (sp-wrap-with-pair "\"")))
      ("r" sp-rewrap-sexp)
      ;; Navigation
      ("f" sp-forward-sexp )
      ("b" sp-backward-sexp)
      ("u" sp-backward-up-sexp)
      ("d" sp-down-sexp)
      ("p" sp-backward-down-sexp)
      ("n" sp-up-sexp)
      ;; Kill/copy
      ("w" sp-copy-sexp)
      ("k" sp-kill-sexp)
      ;; Misc
      ("t" sp-transpose-sexp)
      ("j" sp-join-sexp)
      ("S" sp-split-sexp)
      ("c" sp-convolute-sexp)
      ("i" sp-indent-defun)
      ;; Depth changing
      ("s" sp-splice-sexp)
      ("R" sp-splice-sexp-killing-around)
      ("<up>" sp-splice-sexp-killing-backward)
      ("<down>" sp-splice-sexp-killing-forward)
      ;; Barfing/slurping
      ("<right>" sp-forward-slurp-sexp)
      ("<left>" sp-forward-barf-sexp)
      ("C-<left>" sp-backward-barf-sexp)
      ("C-<right>" sp-backward-slurp-sexp)))
  (evil-leader/set-key (kbd "f")
    (defhydra hydra-flycheck (:color blue :hint nil)
      "
      ^
      ^Flycheck^          ^Errors^            ^Checker^
      ^────────^──────────^──────^────────────^───────^─────
      _q_ quit            _<_ previous        _?_ describe
      _M_ manual          _>_ next            _d_ disable
      _v_ verify setup    _f_ check           _m_ mode
      ^^                  _l_ list            _s_ select
      ^^                  ^^                  ^^
      "
      ("q" nil)
      ("<" flycheck-previous-error :color pink)
      (">" flycheck-next-error :color pink)
      ("?" flycheck-describe-checker)
      ("M" flycheck-manual)
      ("d" flycheck-disable-checker)
      ("f" flycheck-buffer)
      ("l" flycheck-list-errors)
      ("m" flycheck-mode)
      ("s" flycheck-select-checker)
      ("v" flycheck-verify-setup))))

(use-package tex
  :ensure auctex
  :hook (LaTeX-mode . reftex-mode)
  :custom
  (TeX-PDF-mode t)
  (TeX-auto-save t)
  (TeX-byte-compile t)
  (TeX-clean-confirm nil)
  (TeX-master 'dwim)
  (TeX-parse-self t)
  (TeX-source-correlate-mode t)
  (TeX-view-program-selection '((output-pdf "open")
                                (output-html "xdg-open"))))

(use-package bibtex
  :after auctex
  :hook (bibtex-mode . my/bibtex-fill-column)
  :preface
  (defun my/bibtex-fill-column ()
    "Ensures that each entry does not exceed 120 characters."
    (setq fill-column 120)))

(use-package company-auctex
  :after (auctex company)
  :config (company-auctex-init))

(use-package company-math :after (auctex company))

(setq-default TeX-engine 'xetex)

(use-package reftex :after auctex)

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

(add-hook 'tex-mode-hook #'flyspell-mode)

(setq ispell-program-name "/usr/local/bin/aspell")

(use-package writegood-mode
  :ensure t
  :hook ((markdown-mode . writegood-mode)
         (tex-mode . writegood-mode)
         (text-mode . writegood-mode)
         (org-mode . writegood-mode)))

(use-package synosaurus
  :ensure t
  :defer t
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

(use-package poet-theme
  :ensure t
  :defer t
  :hook ((text-mode-hook . variable-pitch-mode)
         (org-mode-hook . variable-pitch-mode))
  :config
  (set-face-attribute 'default nil :family "Fira Code" :height 130)
  (set-face-attribute 'fixed-pitch nil :family "Fira Code")
  (set-face-attribute 'variable-pitch nil :family "Baskerville"))

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

(setq mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum)

(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text)
(add-hook 'mu4e-view-mode-hook
          (lambda()
            ;; try to emulate some of the eww key-bindings
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)))

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

(use-package atomic-chrome
  :ensure t
  :config
  (setq atomic-chrome-default-major-mode 'org-mode)
  (setq atomic-chrome-buffer-open-style 'frame)
  (add-hook 'atomic-chrome-edit-done-hook 'delete-frame)
  ;; Handle if there is an Emacs instance running which has the server already started
  (ignore-errors
    ;; Start the server
    (atomic-chrome-start-server)))

(use-package ledger-mode
  :mode ("\\.dat\\'"
         "\\.ledger\\'")
  :bind (:map ledger-mode-map
              ("C-x C-s" . my/ledger-save))
  :preface
  (defun my/ledger-save ()
    "Automatically clean the ledger buffer at each save."
    (interactive)
    (save-excursion
      (when (buffer-modified-p)
        (with-demoted-errors (ledger-mode-clean-buffer))
        (save-buffer))))
  :custom (ledger-clear-whole-transactions t))

(use-package flycheck-ledger
  :after ledger-mode)

(use-package helm-spotify-plus
  :ensure t)

(use-package elfeed
  :ensure t
  :after hydra
  :bind (:map elfeed-search-mode-map
              ("h" . mz/make-and-run-elfeed-hydra))
  :config
  (setq shr-max-image-proportion 0.6)
  (add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
  (add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
  (defun z/hasCap (s) ""
         (let ((case-fold-search nil))
           (string-match-p "[[:upper:]]" s)))
  (defun z/get-hydra-option-key (s)
    "Return single upper case letter (converted to lower) or first"
    (interactive)
    (let ((loc (z/hasCap s)))
      (if loc
          (downcase (substring s loc (+ loc 1)))
        (substring s 0 1))))
  (defun mz/make-elfeed-cats (tags)
    "Return a list of lists. Each one is line for the hydra configuration in the form
       (c function hint)"
    (interactive)
    (mapcar (lambda (tag)
              (let* ((tagstring (symbol-name tag))
                     (c (z/get-hydra-option-key tagstring)))
                (list c (append '(elfeed-search-set-filter) (list (format "@3-weeks-ago +%s" tagstring)))
                      tagstring)))
            tags))
  (defmacro mz/make-elfeed-hydra ()
    `(defhydra mz/hydra-elfeed ()
       "filter"
       ,@(mz/make-elfeed-cats (elfeed-db-get-all-tags))
       ("*" (elfeed-search-set-filter "@3-weeks-ago +star") "Starred")
       ("M" elfeed-toggle-star "Mark")
       ("A" (elfeed-search-set-filter "@3-weeks-ago") "All")
       ("T" (elfeed-search-set-filter "@1-day-ago") "Today")
       ("q" nil "quit" :color blue)))
  (defun mz/make-and-run-elfeed-hydra ()
    "Redefine the hydra whenever needed"
    (interactive)
    (mz/make-elfeed-hydra)
    (mz/hydra-elfeed/body)))

(use-package elfeed-goodies
  :ensure t
  :config
  (elfeed-goodies/setup))

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/feeds.org")))
