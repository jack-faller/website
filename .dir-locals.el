((scheme-mode
  . ((eval . (put 'iterator 'scheme-indent-function #'scheme-let-indent))
	 (eval . (progn
			   (put 'iter-let 'scheme-indent-function #'scheme-let-indent)
			   (let* ((sym "[^][(){}[:space:]\n]+")
					  (space "[[:space:]\n]")
					  (regex (concat space "*\\(" sym "\\)\\(" space "+" sym "\\)?")))
				 (defun iterators-iterate-indent (state indent-point normal-indent)
				   (lisp-indent-specform
					(cond
					 ((not (looking-at regex)) 1)
					 ((match-beginning 2) 3)
					 ((match-beginning 1) 2))
					state indent-point normal-indent)))
			   (put 'iterate 'scheme-indent-function #'iterators-iterate-indent)
			   (put 'iterate* 'scheme-indent-function #'iterators-iterate-indent)))))
 (nil . ((compile-command . "make")
		 (eval . (progn
				   (defun post-template ()
					 (interactive)
					 (insert "(post\n \""
							 (string-replace "-" " " (file-name-base (buffer-file-name)))
							 "\"\n #f\n \"SUMMARY\"\n )"))
				   (defun post-date ()
					 (interactive)
					 (insert (format-time-string "\"%Y/%m/%d %H:%M %z\""))))))))
