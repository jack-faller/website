(home-page
 {h1 Here Lies That Which Is Digital of Jack Faller}
 (link-heading 2 "about" "About Me and This Site")
 {p 
  Hi, I'm Jack Faller. (As you may have surmised from title of this page.)
  I'm primarily a programmer.
  I've been doing it for some years now and I like to think I'm rather good at it.
  This is the place I will be collating and writing about the things I've done (programming or otherwise), and sharing my general musings on life.
  I have been cursed with a great many ideas and I hope they may be of some use to you.}
 (link-heading 2 "all-stuff" "All Stuff")
 {ul
  {li Thoughts: {{a {href /thoughts}} see all}, {{a {href /thoughts.rss}} RSS}}
  {li Blog: {{a {href /posts}} see all}, {{a {href /blog.rss}} RSS}}
  {li Github: {{a {href https://github.com/jack-faller}} github.com/jack-faller}}
  {li Email: {{a {href mailto:jack.t.faller@gmail.com}} jack.t.faller@gmail.com}}}
 (link-heading 2 "recent-stuff"
			   "Recent Stuff"
			   {join #" (" {{a {href /stuff}} see all}
					 #", " {{a {href /stuff.rss}} RSS}
					 #")"})
 {ul #@(map (post->li #t) (at-most 10 public-stuff))}
 (link-heading 2 "recent-blog"
			   "Recent Blog Posts"
			   {join #" (" {{a {href /posts}} see all}
					 #", " {{a {href /blog.rss}} RSS}
					 #")"})
 {ul #@(map (post->li #f) (at-most 5 public-posts))})
