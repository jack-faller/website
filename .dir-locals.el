((scheme-mode
  . ((eval . (put 'iterator 'scheme-indent-function #'scheme-let-indent))
     (eval . (progn
               (put 'let-next 'scheme-indent-function #'scheme-let-indent)
               (put 'where 'scheme-indent-function 1)
               (put 'where* 'scheme-indent-function 1)
               (put 'whererec 'scheme-indent-function 1)
               (put 'whererec* 'scheme-indent-function 1)
               (put 'iter-for 'scheme-indent-function 1)
               (put 'iter-for* 'scheme-indent-function 1)
               (put 'if-let-next 'scheme-indent-function #'scheme-let-indent)
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
               (put 'iterate 'define-syntax* #'scheme-let-indent)
               (put 'iterate 'scheme-indent-function #'iterators-iterate-indent)
               (put 'iterate* 'scheme-indent-function #'iterators-iterate-indent)))))
 (nil . ((compile-command . "make")
         (eval . (progn
                   (defun post-template (type)
                     (interactive "MType: ")
                     (let* ((name (file-name-base (buffer-file-name)))))
                     (insert
		      "(use-modules (doclisp) (make))\n"
		      "(set-reader! doclisp-reader)\n"
		      "{{" type " {version #1}}\n"
		      " {title " (string-replace "-" " " (file-name-base (buffer-file-name))) "}\n"
                      " {uuid ")
                     (forward-char (- (cadr (insert-file-contents "/proc/sys/kernel/random/uuid")) 1))
		     (insert "}\n }")
                     (backward-char))
                   (defun post-date ()
                     (interactive)
                     (insert (format-time-string "#\"%Y/%m/%d %H:%M %z\""))))))))
