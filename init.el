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
    git-gutter+				; show in the buffer git diff
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
    rvm 				; rmv integration for emacs
    yasnippet				; yet anothers snippets
    helm-projectile			; helm integration for projectile
    browse-kill-ring+			; browse the kill ring nicely
    neotree				; tree mode
    comment-dwim-2			; comment line or end of line
    dockerfile-mode			; mode to edit Dockerfile
    org-bullets				; prettiers headers for org mode
    ruby-tools				; helpers for ruby
    typescript-mode			; typescript mode
    tide				; make typescript easier
    json-mode				; yes handle json is prety convenient
    vagrant				; I should start to use is
    zencoding-mode			; code html with style
    ) "important package to install")

;; check if packages are installed and install them if not
(dolist (package eb-packages)
  (print eb-packages)
  (progn
    (if (not (package-installed-p package))
	(package-install package))))

;; mini global mode to activate
(show-paren-mode 1)
(electric-pair-mode 1)

;; personal functions
(add-to-list 'load-path "~/.emacs.d/emacs-init-file/eb-functions.el")
(load-file "~/.emacs.d/emacs-init-file/eb-functions.el")
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

;; var environmnet initialization
(when (memq window-system '(x mac ns))
  (exec-path-from-shell-initialize))

;; theme
(load-theme 'solarized-dark)

;; projectile
(projectile-global-mode)
(require 'helm-projectile)
(helm-projectile-on)

;; suspend-frame binded just with "C-x C-z"
(global-unset-key (kbd "C-z"))

(global-set-key (kbd "M-\\") 'neotree-toggle)

(global-set-key (kbd "M-;") 'comment-dwim-2)
(global-set-key (kbd "M-_") 'text-scale-decrease)
(global-set-key (kbd "M-+") 'text-scale-increase)

(global-set-key (kbd "C-c o") '(lambda () "DOCSTRING" (interactive) (other-window -1)))

(add-hook 'before-save-hook '(lambda () "Delete all trailing whitespace before saving"
			       (delete-trailing-whitespace)))

;; prevent in ruby-mode to insert encoding comment while saving
(setq ruby-insert-encoding-magic-comment nil)

(yas-global-mode 1)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(global-git-gutter+-mode +1)


;;;; org mode

;; hide emphasis markers
(setq org-hide-emphasis-markers t)

;; dash are replaced by prettier bullet
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; bullets as headers
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; font headers
(let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                             ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                             ((x-list-fonts "Verdana")         '(:font "Verdana"))
                             ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))

;; git gutter
(global-set-key (kbd "C-c TAB") 'git-gutter+-show-hunk-inline-at-point)

;; hooks
(add-hook 'ruby-mode-hook (lambda () "active ruby tools for ruby mode"
			    (ruby-tools-mode 1)))


;; tide (typescript)
;; sample config
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)
            ;; company is an optional dependency. You have to
            ;; install it separately via package-install
            (company-mode-on)))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
;; see https://github.com/Microsoft/TypeScript/blob/cc58e2d7eb144f0b2ff89e6a6685fb4deaa24fde/src/server/protocol.d.ts#L421-473 for the full list available options

;; Tide can be used along with web-mode to edit tsx files
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (tide-setup)
              (flycheck-mode +1)
              (setq flycheck-check-syntax-automatically '(save mode-enabled))
              (eldoc-mode +1)
              (company-mode-on))))
