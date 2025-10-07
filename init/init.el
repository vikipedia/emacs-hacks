;; .emacs.d/init.el

;; == == == == == == == == == == == == ==
;; MELPA package support
;; == == == == == == == == == == == == ==
;; Enables basic packaging support
(require 'package)

;; Adds the Melpa archive to the list of available repositories
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Initializes the package infrastructure
(package-initialize)

;; If there are no archived package contents, refresh them
(when(not package-archive-contents)
  (package-refresh-contents))

;; Installs packages
;;
;; myPackages contains a list of package names
(defvar myPackages
  '(better-defaults       ;; Set up some better Emacs defaults
    elpy                  ;; Emacs Lisp Python Environment
    flycheck              ;; on the fly syntax check
    blacken               ;; Black formatting on save
    material-theme        ;; Theme
    markdown-mode         ;; markdown mode
    py-autopep8           ;;
    auth-source           ;;
    gptel                 ;;
    )
  )

;; Scans the list in myPackages
;; If the package listed is not already installed, install it
(mapc  '(lambda (package)
          (unless(package-installed-p package)
            (package-install package)))
       myPackages)

; Only evaluate this when compiling this file
(eval-when-compile
  ; For each package on the list do
  (dolist (package '(use-package diminish bind-key))
    ; Install if not yet installed
    (unless (package-installed-p package)
      (package-install package))
    ; Require package making it available on the rest of the configuration
    (require package)))

;; ===================================
;; Basic Customization
;; ===================================
(setq inhibit-startup-message t)    ;; Hide the startup message
(load-theme 'material-light t)            ;; Load material theme
(global-display-line-numbers-mode t)               ;; Enable line numbers globally
(toggle-frame-maximized)            ;; maximize window
(column-number-mode t)              ;; show column number as well
;; ====================================
;; Development Setup
;; ====================================



;; Enable elpy
(elpy-enable)
;; rpc-venv path
;;(setq elpy-rpc-python-command "python3")

(setenv "WORKON_HOME" "/home/vikrant/usr/local")
(defalias 'workon 'pyvenv-workon)
(workon "default")
(setq elpy-rpc-virtualenv-path 'current)

;; Use Python3 for REPL
;; (setq python-shell-interpreter "python3"
;;      python-shell-prompt-detect-failure-warning nil)
;;(Add-to-list 'python-shell-completion-native-disabled-interpreters
;;             "python")

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-mode) ;;I don't like autochange of my code, but t is helpful many times!



; GIT interface for Emacs
(use-package magit
  :ensure t
  :bind ("C-c m s" . magit-status))

; Auto-complete interface
(use-package company
  :ensure t
  :diminish company-mode
  :bind ("M-/" . company-complete)  
  :config
  (global-company-mode))

;; ; Project management and tools
;; (use-package projectile
;;   :ensure t
;;   :config
;;   (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;   (setq projectile-completion-system 'ivy)
;;   (projectile-mode +1))

; Sidebar navigation with extras
(use-package treemacs
  :ensure t  
  :config
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'extended)
  (treemacs-follow-mode -1)
  (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1))))

;; ; Unifies projectile and treemacs
;; (use-package treemacs-projectile
;;   :after (treemacs projectile)
;;   :ensure t)

; Makes treemacs show different colors for committed, staged and modified files
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)


;; INFO settings
(require 'info)
(setq Info-directory-list
(cons (expand-file-name "/home/vikrant/.local/share/info") ; here is where my SICP.info file is!
      Info-default-directory-list))

;; org mode settings
(setq org-todo-keywords
      '((sequence "TODO" "NEXT" "|" "DONE" "CANCELLED")))
;; Improve org mode looks
(setq org-startup-indented t
      ;; org-pretty-entities t
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300))
;; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)
(add-hook 'org-mode-hook 'visual-line-mode 'append)
(setq org-agenda-files (quote ("~/Documents/MiA2 Documents/org_agenda/"
			       "~/programming/work/github/Rumi-dev"
			       "~/programming/work/github/chitragupta")))

(setq org-default-notes-file "~/Documents/MiA2 Documents/org_agenda/unfiled.org")
;; Org-Roam basic configuration
(setq org-directory (concat (getenv "HOME") "/Documents/MiA2 Documents/markor/org_roam_dir"))
(setq org-roam-db-location
      (expand-file-name (locate-user-emacs-file "org-roam.db")))
(use-package org-roam
  :ensure t
  :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom
  (org-roam-directory (file-truename org-directory))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ;; Dailies
	 ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;; (use-package org-roam
;;   :after org
;;   :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
;;   :custom
;;   (org-roam-directory (file-truename org-directory))
;;   :config
;;   (org-roam-setup)
;;   :bind (("C-c n f" . org-roam-node-find)
;; 	 ("C-c n r" . org-roam-node-random)		    
;; 	 (:map org-mode-map
;; 	       (("C-c n i" . org-roam-node-insert)
;; 		("C-c n o" . org-id-get-create)
;; 		("C-c n t" . org-roam-tag-add)
;; 		("C-c n a" . org-roam-alias-add)
;; 		("C-c n l" . org-roam-buffer-toggle)))))



(dolist (hook '(text-mode-hook)) ;; spellcheck
  (add-hook hook (lambda () (flyspell-mode 1))))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;; yaml mode settings
(add-to-list 'load-path "~/.emacs.d/other/")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
      '(lambda ()
        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)


(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?" :empty-lines 1)
        ("w" "Work related" entry (file "~/programming/work/github/Rumi-dev/tasks.org")
         "* TODO %?" :empty-lines 1)))

(server-start)


(require 'gptel)
(require 'auth-source)
(defun get-chatgpt-api-key ()
  (let ((credentials (auth-source-search
                      :host "api.openai.com"
                      :user "vikrant.patil@gmail.com"
                      :port nil)))
    (if credentials
        (plist-get (car credentials) :secret)
      (error "API key not found!"))))
(setq gptel-api-key (get-chatgpt-api-key))
;;(setq gptel-api-key 'gptel-api-key-from-auth-source)
;; User-Defined init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(custom-safe-themes
   '("f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "d4f8fcc20d4b44bf5796196dbeabec42078c2ddb16dcb6ec145a1c610e0842f3" "afd761c9b0f52ac19764b99d7a4d871fc329f7392dfc6cd29710e8209c691477" default))
 '(elpy-test-runner 'elpy-test-pytest-runner)
 '(fci-rule-color "#37474f")
 '(flycheck-checker-error-threshold 700)
 '(hl-sexp-background-color "#1c1f26")
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(polymode gptel csv-mode quarto-mode seq racket-mode org-roam lsp-mode material-theme better-defaults))
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values '((input-method . "devanagari-itrans")))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
