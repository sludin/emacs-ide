(require 'package)

;; Package handling
;;
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
			                   ("org" . "https://orgmode.org/elpa/")
			 ))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(setq package-list '(use-package))
(dolist (package package-list)
  (unless (package-installed-p package) (package-install package)))

(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs 
    projectile hydra flycheck company avy which-key helm-xref dap-mode))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))


;;
;; Quality of life improvements
;;
(setq ring-bell-function 'ignore)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq whitespace-line-column 1000)
(global-visual-line-mode 1)
(setq column-number-mode t)
(setq-default tab-width 2)
(electric-pair-mode 1)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(global-set-key "\r" 'newline-and-indent)
(defvaralias 'c-basic-offset 'tab-width)
(define-key global-map "\C-xl" 'goto-line)
(define-key global-map "\C-xa" 'compile)
(define-key global-map "\C-xm" 'man)

;;
;; Modifier key handling
;;
(setq mac-command-modifier 'none) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'super)  ; make Fn key do Hyper


;;
;; Perl mode
;;
(defalias 'perl-mode 'cperl-mode)

(defvaralias 'cperl-indent-level 'tab-width)
(setq cperl-extra-newline-before-brace t
      cperl-brace-offset               0
      cperl-merge-trailing-else        nil)

(add-hook 'cperl-mode-hook 'n-cperl-mode-hook t)
(defun n-cperl-mode-hook ()
  (setq cperl-indent-level 2)
  (setq cperl-continued-statement-offset 0)
  (setq cperl-extra-newline-before-brace t)
  (set-face-background 'cperl-array-face "white")
  (set-face-background 'cperl-hash-face "white")
  )

(define-abbrev-table 'global-abbrev-table '(
                                            ("pdbg" "use Data::Dumper qw( Dumper );\nwarn Dumper[];"     nil 1)
                                            ("phbp" "#! /usr/bin/perl -w"                                nil 1)
                                            ("pbmk" "use Benchmark qw( cmpthese );\ncmpthese -10, {};"   nil 1)
                                            ("pusc" "use Smart::Comments;\n### "                         nil 1)
                                            ("putm" "use Test::More 'no_plan';"                          nil 1)
                                             ))

;;
;; Ant for simple non-make Java compliation
;;
(setq ant-command "/opt/local/bin/ant -emacs")


;;
;; Theme handling
;;
(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-acario-light t))

(use-package heaven-and-hell
  :ensure t
  :init
  (setq heaven-and-hell-theme-type 'dark)
  (setq heaven-and-hell-themes
        '((light . doom-acario-light)
          (dark . doom-palenight)))
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
         ("<f6>" . heaven-and-hell-toggle-theme)))

(setq heaven-and-hell-theme-type 'light)

(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))

(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
	(ansi-color-apply-on-region (point-min) (point-max))))

(use-package ansi-color
  :ensure t
  :config
  (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)
  )



;;
;; Ivy / Swiper / Counsel - More powerful selection / search tools
;;
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;;
;; Treemancs - heirachical file lists / projects
;;
(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :ensure t
  :commands lsp-treemacs-errors-list
  :bind (:map lsp-mode-map
			  ("M-9" . lsp-treemacs-errors-list)))

(use-package treemacs
  :ensure t
  :commands (treemacs)
  :after (lsp-mode))

;;
;; Company - smart context completion
;;
(use-package company :ensure t)


;;
;; yasnippet - templating engine
;;
(use-package yasnippet :config (yas-global-mode))
(use-package yasnippet-snippets :ensure t)


;;
;; flycheck - on the fly syntax checking
;;
(use-package flycheck :ensure t :init (global-flycheck-mode))

(with-eval-after-load 'flycheck ; the elisp warnings are irritating
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;
;; dap-mode - debugging from emacs.  I have not gotten this
;;            to work yet
;;
(use-package dap-mode
  :ensure t
  :after (lsp-mode)
  :functions dap-hydra/nil
  :config
  (require 'dap-java)
  :bind (:map lsp-mode-map
			  ("<f5>" . dap-debug)
			  ("M-<f5>" . dap-hydra))
  :hook ((dap-mode . dap-ui-mode)
		 (dap-session-created . (lambda (&_rest) (dap-hydra)))
		 (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))))

(use-package dap-java :ensure nil)






;;
;; LSP settings - the Language Server Protocol
;;
(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :bind (:map lsp-ui-mode-map
			  ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
			  ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init (setq lsp-ui-doc-delay 1.5
			  lsp-ui-doc-position 'bottom
			  lsp-ui-doc-max-width 100
			  ))

(use-package lsp-mode
  :ensure t
  :hook (
		 (lsp-mode . lsp-enable-which-key-integration)
		 (java-mode . #'lsp-deferred)
		 )
  :init (setq 
		 lsp-keymap-prefix "C-c l"              ; this is for which-key integration documentation, need to use lsp-mode-map
		 lsp-enable-file-watchers nil
		 read-process-output-max (* 1024 1024)  ; 1 mb
		 lsp-completion-provider :capf
		 lsp-idle-delay 0.500
		 )
  :config 
  (setq lsp-intelephense-multi-root nil) ; don't scan unnecessary projects
  (with-eval-after-load 'lsp-intelephense
    (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  )

(use-package lsp-java 
  :ensure t
  :config (add-hook 'java-mode-hook 'lsp))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1 ;; clangd is fast
      ;; be more ide-ish
      lsp-headerline-breadcrumb-enable t)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#292D3E" "#ff5370" "#c3e88d" "#ffcb6b" "#82aaff" "#c792ea" "#89DDFF" "#EEFFFF"])
 '(cperl-close-paren-offset -2)
 '(cperl-continued-statement-offset 2)
 '(cperl-indent-level 2)
 '(cperl-indent-parens-as-block t)
 '(cperl-tab-always-indent t)
 '(custom-enabled-themes '(doom-acario-light))
 '(custom-safe-themes
   '("c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" default))
 '(fci-rule-color "#676E95")
 '(flycheck-disabled-checkers '(emacs-lisp-checkdoc))
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(jdee-db-active-breakpoint-face-colors (cons "#1c1f2b" "#c792ea"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1c1f2b" "#c3e88d"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1c1f2b" "#676E95"))
 '(objed-cursor-color "#ff5370")
 '(package-selected-packages
   '(git ant ivy lsp-mode yasnippet lsp-tremacs projectile company avy dap-mode lsp-java lsp-ui dap-mode flycheck yasnippet-snippets company quickrun which-key avy use-package-chords heaven-and-hell doom-themes exec-path-from-shell use-package yaml-mode json-mode js2-mode go-mode rust-mode))
 '(pdf-view-midnight-colors (cons "#EEFFFF" "#292D3E"))
 '(rustic-ansi-faces
   ["#292D3E" "#ff5370" "#c3e88d" "#ffcb6b" "#82aaff" "#c792ea" "#89DDFF" "#EEFFFF"])
 '(vc-annotate-background "#292D3E")
 '(vc-annotate-color-map
   (list
    (cons 20 "#c3e88d")
    (cons 40 "#d7de81")
    (cons 60 "#ebd476")
    (cons 80 "#ffcb6b")
    (cons 100 "#fcb66b")
    (cons 120 "#f9a16b")
    (cons 140 "#f78c6c")
    (cons 160 "#e78e96")
    (cons 180 "#d690c0")
    (cons 200 "#c792ea")
    (cons 220 "#d97dc1")
    (cons 240 "#ec6898")
    (cons 260 "#ff5370")
    (cons 280 "#d95979")
    (cons 300 "#b36082")
    (cons 320 "#8d678b")
    (cons 340 "#676E95")
    (cons 360 "#676E95")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )






;; (use-package use-package-chords
;;   :ensure t
;;   :init
;;   :config (key-chord-mode 1)
;;   (setq key-chord-two-keys-delay 0.4)
;;   (setq key-chord-one-key-delay 0.5) ; default 0.2
;;   )

;; (use-package quickrun 
;;   :ensure t
;;   :bind ("C-c r" . quickrun))

;;(which-key-mode)
;;(add-hook 'c-mode-hook 'lsp)
;;(add-hook 'cpp-mode-hook 'lsp)
