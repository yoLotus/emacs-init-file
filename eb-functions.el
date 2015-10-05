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
