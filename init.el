(require 'package)
(setq package-enable-at-startup nil)

;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

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
 '(display-time-string-forms
   (quote
    ((propertize
      (concat dayname " " 12-hours ":" minutes " "
              (upcase am-pm))
      (quote help-echo)
      (format-time-string "%a, %b %e %Y" now)))))
 '(helm-buffers-fuzzy-matching t)
 '(helm-imenu-fuzzy-match t t)
 '(helm-projectile-fuzzy-match t)
 '(helm-recentf-fuzzy-match t)
 '(lean-rootdir "~/lean-3.4.0-darwin" t)
 '(ledger-clear-whole-transactions t)
 '(magit-pull-arguments nil)
 '(org-agenda-files (quote ("~/Dropbox/org/beorg-local.org")))
 '(org-capture-templates
   (quote
    (("l" "Ledger")
     ("lb" "Bank" plain
      (file "~/.personal/ledger/ledger-2018.dat")
      "%(org-read-date) %^{Payee}
  Expenses:%^{Account}  $%^{Amount}
  Liabilities:DebitCard" :empty-lines 1 :immediate-finish t)
     ("lc" "Cash" plain
      (file "~/.personal/ledger/ledger-2018.dat")
      "%(org-read-date) * %^{Payee}
  Expenses:%^{Account}  $%^{Amount}
  Assets:Cash:Wallet" :empty-lines 1 :immediate-finish t))) t)
 '(package-selected-packages
   (quote
    (cquery elfeed-org elfeed-goodies expand-region evil-numbers dashboard helm-spotify-plus helm-lean company-lean lean-mode flycheck-ledger ledger-mode hy-mode helm-projectile helm-projectie clj-refactor atomic-chrome swiper-helm evil-lion evil-mc xref-js2 company-tern tern js2-refactor js2-mode pipenv elpy rainbow-delimiters typo visual-fill-column synosaurus dumb-jump evil-paredit paredit org-pdfview beacon beacon-mode evil-nerd-commenter mu4e-conversation tablist pdf-tools evil-collection evil-mu4e inf-clojure cider evil-org org-mode feebleline doom-modeline eldoc-eval shrink-path prettier-js boogie-friends evil-surround evil-leader helm telephone-line minions which-key pretty-mode smart-mode-line spaceline-all-the-icons all-the-icons diminish nlinum-hl fancy-battery spaceline exec-path-from-shell flycheck ox-pandoc evil-magit melpa-upstream-visit magit org-bullets evil-visual-mark-mode)))
 '(pdf-tools-handle-upgrades nil)
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
