;; init file etienne bazin

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; installations of all packages
(defvar eb-packages
  '(
    highlight-symbol			; highlight symbol
    magit				; git wrap-up
    ruby-mode				; ruby handler
    rinari				; rails IDE (in fact not)
    js2-mode				; javascript
    web-mode				; multi-language web
    virtualenvwrapper			; virtualenv python mode
    undo-tree				; undo-tree-mode
    projectile				; projects manager
    exec-path-from-shell		; ensure GUI emacs uses my env var
    yaml-mode				; yaml mode for emacs
    solarized-theme			; eye no hurting theme
    ) "important package to install")

;; check if packages are installed and install them if not
(dolist (package eb-packages)
  (progn
    (if (not (package-installed-p package))
	(package-install package))))

;; mini global mode to activate
(show-paren-mode 1)
(electric-pair-mode 1)

;; personal functions
(add-to-list 'load-path "~/.emacs.d/eb-functions.el")
(load-file "~/.emacs.d/eb-functions.el")
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(grep-command "egrep -nrIH --exclude-dir=\".git\"")
 '(inhibit-default-init t)
 '(inhibit-startup-screen t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; not need to enter yes in full
(fset 'yes-or-no-p 'y-or-n-p)
(put 'narrow-to-region 'disabled nil)

;; personal keystrokes
(global-set-key (kbd "C-c g") 'magit-status)

;; var environmnet initialization
(when (memq window-system '(x mac ns))
  (exec-path-from-shell-initialize))

;; theme
(load-theme 'solarized-dark)
