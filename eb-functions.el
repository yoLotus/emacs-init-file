(defun indent-whole-buffer ()
  (interactive)
  (save-excursion
    (save-restriction
      (progn
	(widen)
	(indent-region (point-min) (point-max))))))


(defun escape-regex-char (arg1)
  " escape all regex char in a string. ex: $http.get( -> \\\$http\.get\("

  (setq string-regex (regexp-quote arg1))

  (setq string-in (split-string string-regex "" t))
  (setq string-out "")

  ;; du to regex intern emacs, $ is annoying and post-processing must
  ;; be applied :

  (while string-in

    (setq the-char (car string-in))

    (if (string-match-p "[$]" the-char)
    	(setq string-out (concat string-out "\\\\" the-char))
      (setq string-out (concat string-out the-char)))
    (setq string-in (cdr string-in))
    )
  string-out
  )

(defun grep-symbol-at-point (dir)
  " equivalent `grep-command` (symbol-at-point) dir. Put the
point on a symbol you want to grep and call this function. All
means of search occurences are the same as the usual grep
command. if the region is active call the grep on region."

  (interactive
   (let (
	 (sy (if (region-active-p) (buffer-substring (point) (mark)) (symbol-at-point)))
	 val
	 )
     ;; (setq val (read-from-minibuffer (format "which grep dir on %s ? " sy)))
     (setq val (read-directory-name (format "directory %s?" sy) "./" "." t))
     (list (if (equal val "") "." val))
     )
   )

  (if (null dir)
      (setq dir ".")
    (if (not (file-exists-p dir))
	(message "You specified a wrong grep dir")
      (grep (concat grep-command " \"" (escape-regex-char (if (region-active-p) (buffer-substring (point) (mark)) (symbol-name (symbol-at-point)))) "\" " dir)))))


(defun copy-line ()
  (interactive)
  (save-excursion
    (let (_begin _end)
      (beginning-of-line)
      (setq _begin (point))
      (end-of-line)
      (setq _end (point))
      (kill-ring-save _begin _end)))
  )

(defun repeat-line ()
  (interactive)
  (let (
	(_col (current-column))
	)
    (copy-line)
    (end-of-line)
    (newline)
    (yank)
    (move-to-column _col))
  )

(global-set-key (kbd "M-RET") 'repeat-line)

(defun select-and-remove-tild-files ()
  (interactive)
  (progn (dired-flag-files-regexp ".*~$")
	 (dired-do-flagged-delete)))

(require 'dired)
(define-key dired-mode-map (kbd "<f1>") 'select-and-remove-tild-files)


(defun switch-to-brouillon-buffer ()
  "Switch to my personal brouillon (draft in french) buffer to
  make anything inside. I want to keep my *scratch* buffer
  exclusively for lisp interaction"
  (interactive)
  (switch-to-buffer "brouillon"))

(global-set-key (kbd "<f8>") 'switch-to-brouillon-buffer)

;; only for js-2 mode
(defun put-this-before ()
  "add this before word in js file"
  (interactive)
  (save-excursion
    (progn (backward-word)
	   (insert "this."))))

(require 'js2-mode)
(define-key js2-mode-map (kbd "C-c t") #'put-this-before)
