; package management
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(package-initialize)

; list of packages
(defvar required-packages
  '(
    color-theme
    zenburn
    markdown-mode
    fill-column-indicator
    flycheck
    org
    jedi
    ess
    jinja2-mode
    flyspell
    js2-mode
    auto-complete
    ac-js2
    yasnippet
    yasnippet-snippets
    nodejs-repl
  ) "a list of packages to ensure are installed at launch.")

;; Jedi requires the Python packages
;; - virtualenv
;; - Jedi
;; - epc

; auto-install missing packages
(require 'cl)

; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(require 'zenburn)
(zenburn)


(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(require 'fill-column-indicator)

; global flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

; Jinja2 setup
(add-hook 'jinja2-mode-hook 'fci-mode)
(add-hook 'jinja2-mode-hook (lambda() (setq fci-rule-column 80)))

; JS setup
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js2-mode-hook 'fci-mode)
(add-hook 'js2-mode-hook (lambda() (setq fci-rule-column 80)))
(add-hook 'js-mode-hook
	  (lambda ()
	    (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
            (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
            (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
            (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
            (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))

(custom-set-variables  
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(ispell-program-name "aspell")
 '(js2-basic-offset 2)  
 '(js2-bounce-indent-p t)
 '(js2-highlight-level 3)
 '(org-agenda-files (quote ("~/org/tasks.org")))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "evince %s")
     ("\\.docx\\'" . "libreoffice %s"))))
 '(split-height-threshold nil)
 '(split-width-threshold 0))
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(javascript-jshint)))
(setq flycheck-checkers '(javascript-eslint))

; Python setup
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'python-mode-hook (lambda() (setq fci-rule-column 80)))
; Jedi setup
(setq special-display-buffer-names '("*jedi:doc*"))
(setq jedi:setup-keys t)                      
(setq jedi:tooltip-method nil)
(setq jedi:complete-on-dot t)                 ; 
(add-hook 'python-mode-hook 'jedi:setup)


(require 'python)
(setq
  python-shell-interpreter "ipython"
  python-shell-interpreter-args "--simple-prompt"
  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
  python-shell-completion-setup-code
    "from IPython.core.completerlib import module_completion"
  python-shell-completion-module-string-code
    "';'.join(module_completion('''%s'''))\n"
  python-shell-completion-string-code
    "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;;activate org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;org cpature setup
(setq org-default-notes-file (concat "~/org" "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

;;markdown-mode
    (autoload 'markdown-mode "markdown-mode"
       "Major mode for editing Markdown files" t)
    (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; fonts
(defun my-set-font-to-variable-width ()
  "Change font in current window to a variable-width font."
  (interactive)
  (set-frame-font "DejaVu Sans-14" nil t)
  )

(defun my-set-font-to-fixed-width ()
  "Change font in current window to a fixed-width font."
  (interactive)
  (set-frame-font "*-Inconsolata-*-*-*-*-21-*-*-*-*-*-*-*" nil t)
  )
(my-set-font-to-fixed-width)
(add-hook 'python-mode-hook 'my-set-font-to-fixed-width)
(add-hook 'org-mode-hook 'my-set-font-to-fixed-width)
(add-hook 'markdown-mode-hook 'my-set-font-to-fixed-width)
(add-hook 'emacs-lisp-mode-hook 'my-set-font-to-fixed-width)

;; ess
(defvar ess-local-load-path "/home/hvwaldow/.emacs.d/elpa/ess-20150324.1456/lisp")
(add-to-list 'load-path ess-local-load-path)
(load "ess-site")
(setq ess-help-own-frame 'one)

;;tramp
(setq tramp-default-method "ssh")

;; css-mode
(setq css-indent-offset 2)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(ispell-program-name "aspell")
 '(org-agenda-files (quote ("~/org/tasks.org")))
 '(org-agenda-files (quote ("~/org/tasks.org")))
 '(split-height-threshold nil)
 '(split-width-threshold 0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(yas-global-mode 1)
(require 'yasnippet)

