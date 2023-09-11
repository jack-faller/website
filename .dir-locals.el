((nil . ((compile-command . "make")
		 (eval . (progn
				   (defun post-template ()
					 (interactive)
					 (insert "(post
 \""
							 (string-replace "-" " " (file-name-base (buffer-file-name)))
							 "\"
 #f
 \"SUMMARY\"
 )"))
				   (defun post-date ()
					 (interactive)
					 (insert (format-time-string "\"%Y/%m/%d %H:%M %z\""))))))))
