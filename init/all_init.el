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
    elpy                            ;; Emacs Lisp Python Environment
    flycheck                        ;; on the fly syntax check
    py-autopep8                     ;; Run autopep8 on save
    blacken                         ;; Black formatting on save
    material-theme                  ;; Theme
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
;; Enable elpy
(elpy-enable)
;; rpc-venv path
(setq elpy-rpc-python-command "/home/vikrant/anaconda3/bin/python")

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; Use Python3 for REPL
(setq python-shell-interpreter "python3"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "python3")


(setenv "WORKON_HOME" "/home/vikrant/usr/local")

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
;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :ensure t
;;   :hook
;;   ; some examples
;;   (python-mode . lsp-deferred)
;;   :config
;;   (setq
;;    ; I will describe my Elixir setup on a next post :)
;;    lsp-clients-python-server-executable "/home/vikrant/anaconda3/bin/jedi-language-server"
;;    lsp-auto-guess-root t) ; very useful
;;   (setq lsp-file-watch-ignored ; customize this to your liking :)
;;       '("[/\\\\]\\.git$"
;;         "[/\\\\]_build$"
;;         "[/\\\\]assets$"
;;         "[/\\\\]cover$"
;;         "[/\\\\]node_modules$"
;;         "[/\\\\]submodules$"
;;         )))

(use-package lsp-jedi
  :ensure t
  :config
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
 '(python-shell-interpreter "python3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
