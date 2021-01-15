;; Create a variable to indicate where emacs's configuration is installed
(setq EMACS_DIR "~/.emacs.d/")

;; Avoid garbage collection at statup
;;(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
;;      gc-cons-percentage 0.6)

;; All the settings and package installation is set in configuration.org
;;(org-babel-load-file "~/.emacs.d/emacs-configuration.org")

;;(add-hook 'emacs-startup-hook
;;  (lambda ()
;;    (setq gc-cons-threshold 300000000 ; 300mb	
;;          gc-cons-percentage 0.1)))






(require 'package)

;;; Code:

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
												 ("org" . "https://orgmode.org/elpa/")
			 ))


(package-initialize)

					; Fetch the list of packages available
(unless package-archive-contents (package-refresh-contents))

					; Install use-package
(setq package-list '(use-package))
(dolist (package package-list)
  (unless (package-installed-p package) (package-install package)))

(use-package exec-path-from-shell :ensure t)
(exec-path-from-shell-initialize)

;; Load platform specific variables using specific files. E.g linux.el.
;; Make necessary changes as needed
;;(cond ((eq system-type 'windows-nt) (load (concat EMACS_DIR "windows")))
;;			((eq system-type 'gnu/linux) (load (concat EMACS_DIR "linux")))
;;			((eq system-type 'darwin) (load (concat EMACS_DIR "mac")))
;;			(t (load-library "default")))

;; Disable annoying ring-bell when backspace key is pressed in certain situations
(setq ring-bell-function 'ignore)

;; Disable scrollbar and toolbar
;;(scroll-bar-mode -1)
;;(tool-bar-mode -1)

;; Set language environment to UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Longer whitespace, otherwise syntax highlighting is limited to default column
(setq whitespace-line-column 1000)

;; Enable soft-wrap
(global-visual-line-mode 1)

;; Maintain a list of recent files opened
(recentf-mode 1)
(setq recentf-max-saved-items 50)

;; Move all the backup files to specific cache directory
;; This way you won't have annoying temporary files starting with ~(tilde) in each directory
;; Following setting will move temporary files to specific folders inside cache directory in EMACS_DIR

;;(setq user-cache-directory (concat EMACS_DIR "cache"))
;;(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-cache-directory)))
;;      url-history-file (expand-file-name "url/history" user-cache-directory)
;;      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-cache-directory)
;;      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-cache-directory))

;; Org-mode issue with src block not expanding
;; This is a fix for bug in org-mode where <s TAB does not expand SRC block
(when (version<= "9.2" (org-version))
  (require 'org-tempo))

;; Coding specific setting

;; Automatically add ending brackets and braces
(electric-pair-mode 1)

;; Make sure tab-width is 2 and not 8
(setq-default tab-width 2)

;; Highlight matching brackets and braces
(show-paren-mode 1)

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

(use-package use-package-chords
  :ensure t
  :init
  :config (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.4)
  (setq key-chord-one-key-delay 0.5) ; default 0.2
  )


(use-package avy 
  :ensure t
  :chords
  ("jc" . avy-goto-char)
  ("jw" . avy-goto-word-1)
  ("jl" . avy-goto-line))

(use-package which-key 
  :ensure t 
  :init
  (which-key-mode)
  )

(use-package quickrun 
  :ensure t
  :bind ("C-c r" . quickrun))

(use-package company :ensure t)

(use-package yasnippet :config (yas-global-mode))
(use-package yasnippet-snippets :ensure t)

(use-package flycheck :ensure t :init (global-flycheck-mode))

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

;;(use-package helm-lsp
;;:ensure t
;;:after (lsp-mode)
;;:commands (helm-lsp-workspace-symbol)
;;:init (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

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







(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
	 ["#292D3E" "#ff5370" "#c3e88d" "#ffcb6b" "#82aaff" "#c792ea" "#89DDFF" "#EEFFFF"])
 '(custom-enabled-themes '(doom-acario-light))
 '(custom-safe-themes
	 '("c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" default))
 '(fci-rule-color "#676E95")
 '(jdee-db-active-breakpoint-face-colors (cons "#1c1f2b" "#c792ea"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1c1f2b" "#c3e88d"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1c1f2b" "#676E95"))
 '(objed-cursor-color "#ff5370")
 '(package-selected-packages
	 '(lsp-java lsp-ui dap-mode flycheck yasnippet-snippets company quickrun which-key avy use-package-chords heaven-and-hell doom-themes exec-path-from-shell use-package))
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
