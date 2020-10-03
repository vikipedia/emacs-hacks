;; .emacs.d/init.el

;; ==========================
;; MELPA package support
;; ==========================
;; Enables basic packaging support
(require 'package)

;; Adds the Melpa archive to the list of available repositories
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Initializes the package infrastructure
(package-initialize)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;; Installs packages
;;
;; myPackages contains a list of package names
(defvar myPackages
  '(better-defaults                 ;; Set up some better Emacs defaults
    blacken                         ;; Black formatting on save
    material-theme                  ;; Theme
    pyvenv                          ;; python virtual env
    )
  )

;; Scans the list in myPackages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package)
          (unless (package-installed-p package)
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
;;(setq inhibit-startup-message t)    ;; Hide the startup message
(load-theme 'material t)            ;; Load material theme
(global-linum-mode t)               ;; Enable line numbers globally

;; ====================================
;; Development Setup
;; ====================================


(setenv "PATH"
  (let ((current (getenv "PATH"))
        (new "/home/vikrant/anaconda3/bin/"))
    (if current (concat new ":" current) new)))

;; Enable elpy
(setenv "WORKON_HOME" "/home/vikrant/usr/local")
(pyvenv-activate "base")


;; Use Python3 for REPL
;;(setq python-shell-interpreter "python3"
;;      python-shell-prompt-detect-failure-warning nil)
;;(add-to-list 'python-shell-completion-native-disabled-interpreters
;;             "python3")



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

; Project management and tools
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

; Sidebar navigation with extras
(use-package treemacs
  :ensure t  
  :config
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'extended)
  (treemacs-follow-mode -1)
  (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1))))

; Unifies projectile and treemacs
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

; Makes treemacs show different colors for committed, staged and modified files
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)


;; ; LSP client interface for Emacs
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :ensure t
  :hook
   ; some examples
  (python-mode . lsp-deferred)
  :config
  (setq lsp-file-watch-ignored ; customize this to your liking :)
 	'("[/\\\\]\\.git$"
          "[/\\\\]__pycache$"
          "[/\\\\].ipynb_checkpoints$"
          )))

(use-package lsp-jedi
  :ensure t
  :config
  (setq
   lsp-clients-python-server-executable "/home/vikrant/anaconda3/bin/jedi-language-server"
   lsp-auto-guess-root t) ; very useful
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))


					; UX/UI utilities on top of the LSP client
(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t
  :after (lsp-mode)
  :config
  :init
  (setq lsp-ui-doc-enable nil ; does not work properly at the moment IMHO
        lsp-ui-doc-use-webkit t         
        lsp-ui-sideline-enable nil ; clutters UI too much for my taste
        lsp-ui-peek-enable nil) ; clutters UI too much for my taste
  :bind
  ("M-h" . lsp-ui-doc-show)  ; toogle functionality for docs
  ("s-h" . lsp-ui-doc-hide)) ; toogle functionality for docs

					; Auto-complete sources from LSP servers
(use-package company-lsp
  :commands company-lsp
  :ensure t
  :after (company lsp)
  :config
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil)
  (push 'company-lsp company-backends))

; Simple docker interface
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;; User-Defined init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(lsp-mode material-theme better-defaults))
 '(python-shell-interpreter "python"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
