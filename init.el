(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(package-initialize)

;; Load configuration file written in org
(org-babel-load-file "~/.emacs.d/configuration.org")

;; Don't mess with customs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(display-time-mode t)
 '(org-agenda-files (quote ("~/Dropbox/org/beorg-local.org")))
 '(package-selected-packages
   (quote
    (boogie-friends evil-surround evil-leader helm telephone-line minions which-key pretty-mode smart-mode-line spaceline-all-the-icons all-the-icons diminish nlinum-hl fancy-battery spaceline exec-path-from-shell flycheck ox-pandoc evil-magit melpa-upstream-visit magit org-bullets evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
