;;; init.el --- main init file

;;; Commentary:
;; just inlines specific init files

;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "package-management.el")
; Color scheme
(require 'zenburn)
(zenburn)
; no toolbar
(tool-bar-mode -1)
; no splash screen
(setq inhibit-splash-screen t)
(require 'fill-column-indicator)





; global flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

; Python setup
(add-hook 'python-mode-hook 'fci-mode)

(add-hook 'python-mode-hook (lambda() (setq fci-rule-column 80)))
(add-hook 'python-mode-hook 'jedi:setup)
; (add-hook 'python-mode-hook 'flycheck-mode)
(setq jedi:complete-on-dot t)

(require 'python)
(setq
  python-shell-interpreter "ipython"
  ;;python-shell-interpreter-args "--pylab=tk"
  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
  python-shell-completion-setup-code
    "from IPython.core.completerlib import module_completion"
  python-shell-completion-module-string-code
    "';'.join(module_completion('''%s'''))\n"
  python-shell-completion-string-code
    "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;activate org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;org cpature setup
(setq org-default-notes-file (concat "~/org" "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

;; markdown-mode
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
(add-hook 'markdown-mode-hook 'my-set-font-to-variable-width)
(add-hook 'emacs-lisp-mode-hook 'my-set-font-to-fixed-width)

;; ess
(defvar ess-local-load-path
  (shell-command-to-string "find . -path \"*/ess*/lisp\" -type d"))
(add-to-list 'load-path ess-local-load-path)
(load "ess-site")

;;tramp
(setq tramp-default-method "ssh")



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
