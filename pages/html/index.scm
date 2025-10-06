(home-page
 {h1 Here Lies That Which Is Digital of Jack Faller}
 (link-heading 2 "about" "About Me and This Site")
 {p
  Hi, I'm Jack Faller. (As you may have surmised from title of this page.)
  I'm primarily a programmer.
  I've been doing it for some years now and I like to think I'm rather good at it.
  This is the place I will be collating and writing about the things I've done (programming or otherwise), and sharing my general musings on life.
  I have been cursed with a great many ideas and I hope they may be of some use to you.}
 {ul
  {li {b Blog}: {{a {href \#heading-recent-blog}} recent}, {{a {href ./blogs.html}} see all}, {{a {href ./rss/blogs.rss}} RSS}}
  {li {b Thoughts}: {{a {href \#heading-recent-thoughts}} recent}, {{a {href ./thoughts.html}} see all}, {{a {href ./rss/thoughts.rss}} RSS}}
  {li {b Github}: {{a {href https://github.com/jack-faller}} github.com/jack-faller}}
  {li {b Email}: {{a {href mailto:jack.t.faller@gmail.com}} jack.t.faller@gmail.com}}}
 (link-heading 2 "posts"
               "Posts"
               {join #" (" {{a {href ./atom.xml}} full feed} #")"})
 {ul #@(map (post->li #t) (at-most 10 public-posts))})
