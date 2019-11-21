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
 '(TeX-PDF-mode t t)
 '(TeX-auto-save t t)
 '(TeX-byte-compile t t)
 '(TeX-clean-confirm nil t)
 '(TeX-master (quote dwim) t)
 '(TeX-parse-self t t)
 '(TeX-source-correlate-mode t t)
 '(TeX-view-program-selection (quote ((output-pdf "open") (output-html "xdg-open"))) t)
 '(ansi-color-names-vector
   ["#2E3440" "#C16069" "#A2BF8A" "#ECCC87" "#80A0C2" "#B58DAE" "#86C0D1" "#ECEFF4"])
 '(company-quickhelp-mode t t)
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("70ed3a0f434c63206a23012d9cdfbe6c6d4bb4685ad64154f37f3c15c10f3b90" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(display-time-mode t)
 '(display-time-string-forms
   (quote
    ((propertize
      (concat dayname " " 12-hours ":" minutes " "
              (upcase am-pm))
      (quote help-echo)
      (format-time-string "%a, %b %e %Y" now)))))
 '(eldoc-echo-area-use-multiline-p t)
 '(fci-rule-color "#4C566A")
 '(helm-buffers-fuzzy-matching t)
 '(helm-imenu-fuzzy-match t)
 '(helm-projectile-fuzzy-match t)
 '(helm-recentf-fuzzy-match t)
 '(jdee-db-active-breakpoint-face-colors (cons "#191C25" "#80A0C2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#191C25" "#A2BF8A"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#191C25" "#434C5E"))
 '(lean-rootdir "~/lean-3.4.0-darwin" t)
 '(ledger-clear-whole-transactions t t)
 '(magit-pull-arguments nil)
 '(merlin-command (quote opam))
 '(merlin-completion-with-doc t)
 '(merlin-eldoc-max-lines 8)
 '(objed-cursor-color "#C16069")
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
    (doom-themes evil-commentary slime utop flycheck-ocaml poet-theme poet merlin-eldoc merlin reason-mode company-math company-auctex auctex all-the-icons-ivy ivy-rich writeroom-mode writegood-mode counsel-projectile counsel eglot eyebrowse iedit org-ref yasnippet-snippets cquery elfeed-org elfeed-goodies expand-region evil-numbers dashboard helm-spotify-plus helm-lean company-lean lean-mode flycheck-ledger ledger-mode hy-mode helm-projectile helm-projectie clj-refactor atomic-chrome swiper-helm evil-lion evil-mc xref-js2 company-tern tern js2-refactor js2-mode pipenv elpy rainbow-delimiters typo visual-fill-column synosaurus dumb-jump evil-paredit paredit org-pdfview beacon beacon-mode evil-nerd-commenter mu4e-conversation tablist pdf-tools evil-collection evil-mu4e inf-clojure cider evil-org org-mode feebleline doom-modeline eldoc-eval shrink-path prettier-js boogie-friends evil-surround evil-leader helm telephone-line minions which-key pretty-mode smart-mode-line spaceline-all-the-icons all-the-icons diminish nlinum-hl fancy-battery spaceline exec-path-from-shell flycheck ox-pandoc evil-magit melpa-upstream-visit magit org-bullets evil-visual-mark-mode)))
 '(pdf-tools-handle-upgrades nil)
 '(projectile-mode t nil (projectile))
 '(send-mail-function (quote smtpmail-send-it))
 '(vc-annotate-background "#2E3440")
 '(vc-annotate-color-map
   (list
    (cons 20 "#A2BF8A")
    (cons 40 "#bac389")
    (cons 60 "#d3c788")
    (cons 80 "#ECCC87")
    (cons 100 "#e3b57e")
    (cons 120 "#da9e75")
    (cons 140 "#D2876D")
    (cons 160 "#c88982")
    (cons 180 "#be8b98")
    (cons 200 "#B58DAE")
    (cons 220 "#b97e97")
    (cons 240 "#bd6f80")
    (cons 260 "#C16069")
    (cons 280 "#a15b66")
    (cons 300 "#825663")
    (cons 320 "#625160")
    (cons 340 "#4C566A")
    (cons 360 "#4C566A")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
