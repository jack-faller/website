{just
 #"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
 {{rss {version 2.0}}
  {channel
   {title Jack Faller's Blog}
   {link https://jackfaller.xyz/}
   {description Recent blog posts on jackfaller.xyz, my general musings on life.}
   {just
	#@(map
	   (lambda (post)
		 (define (rfc-822 date) (date->string date "~a, ~d ~b ~T ~z"))
		 {item
		  {title #(post-title post)}
		  {link {join https://jackfaller.xyz/post/ #(post-name post) .html}}
		  {pubDate #(rfc-822 (post-date post))}
		  {description
		   #(let ((CD-begin "<![CDATA[") (CD-end "]]>")
				  (desc (sexp->html (post-description post))))
			  (if (string-contains desc CD-end)
				  (error "Post description contains CDATA end string: \"]]>\"")
				  (string-append CD-begin desc CD-end)))}})
	   (at-most 30 public-posts))}}}}
