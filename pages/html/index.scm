(home-page
 {h1 Jack Faller's Personal Site}
 (link-heading 2 "about" "About Me and This Site")
 {p 
  Hi, I'm Jack Faller. (As you may have surmised from title of this page.) I've
  been programming for a few years now, and doing some other things too. Now
  I've decided to upload some of those things to the internet, and a major
  location for that is this site. I have too many ideas to keep them all to
  myself, so I hope you can get something from the ones I put here.}
 (link-heading 2 "recent-thoughts"
			   "Recent Thoughts"
			   {join #" (" {{a {href /thoughts.html}} see all}
					 #", " {{a {href /thoughts.rss}} RSS}
					 #")"})
 recent-thoughts
 (link-heading 2 "recent-posts"
			   "Recent Blog Posts"
			   {join #" (" {{a {href /posts.html}} see all}
					 #", " {{a {href /blog.rss}} RSS}
					 #")"})
 recent-posts
 (link-heading 2 "other-places" "Other Places")
 {ul
  {li Github: {{a {href https://github.com/jack-faller}} github.com/jack-faller}}
  {li Email: {{a {href mailto:jack.t.faller@gmail.com}} jack.t.faller@gmail.com}}})
