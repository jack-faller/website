(let ((rfc-822 (lambda (date) (date->string date "~a, ~d ~b ~T ~z"))))
  {just
   #"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
   {{rss {version 2.0}}
	{channel
	 {title Jack Faller's Thoughts}
	 {link https://jackfaller.xyz/}
	 {description A stream of my thoughts, for anyone who wants to connect their brain to my brain.}
	 {lastBuildDate #(rfc-822 (current-date))}
	 #@(map
		(lambda (post)
		  {item
		   {title #(post-title post)}
		   {link {join https://jackfaller.xyz/thought/ #(post-name post) .html}}
		   {pubDate #(rfc-822 (post-date post))}
		   {description
			#(let ((CD-begin "<![CDATA[") (CD-end "]]>")
				   (desc (sexp->html (post-description post))))
			   (if (string-contains desc CD-end)
				   (error "Thought description contains CDATA end string:" CD-end)
				   (string-append CD-begin desc CD-end)))}})
		(at-most 30 public-thoughts))}}})
