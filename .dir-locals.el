((nil . ((eval . (progn
				   (defun post-template ()
					 (interactive)
					 (insert "(post
 \"TITLE\"
 #f
 \"SUMMARY\"
 )"))
				   (defun post-date ()
					 (interactive)
					 (insert (format-time-string "\"%Y/%m/%d %H:%M %z\""))))))))
