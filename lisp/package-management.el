;;; package-management.el --- installs packages if missing

;;; Commentary:
;; defines package archives
;; defines required packages
;; installs missing packages

;; ATTENTION:
;; Jedi requires the Python packages
;; - virtualenv
;; - Jedi
;; - epc

;;; Code:
(eval-when-compile (require 'cl))
(require 'package)
; archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
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
  ) "A list of packages to ensure are installed at launch.")

; method to check if all packages are installed
(defun packages-installed-p () "Produces a list that contains nil for each packages that is not installed."
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

; If not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;; (provide 'package-management.el)
;;; package-management.el ends here
