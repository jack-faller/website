(use-modules (doclisp)
             (make)
             (utilities)
             ((utilities sinks) #:prefix sink:)
             ((utilities iterators) #:prefix iter:))
(set-reader! doclisp-reader)
{{page {version #1}}
 {wants-back-arrow? ##f}
 {published #"2025/10/07 18:20 +0100"}
 {title Here Lies That Which Is Digital of Jack Faller}
 {body
  #(link-heading 2 "about" "About Me and This Site")
  {p
   Hi, I'm Jack Faller. (As you may have surmised from title of this page.)
   I'm primarily a programmer.
   I've been doing it for some years now and I like to think I'm rather good at it.
   This is the place I will be collating and writing about the things I've done (programming or otherwise), and sharing my general musings on life.
   I have been cursed with a great many ideas and I hope they may be of some use to you.}
  {p
   Check out {{a {href /blogroll.xml}} my blogroll} to see who I follow.}
  #(link-heading 2 "where-to-find-me" "Where To Find Me")
  {ul
   {li {{a {href ./atom.xml}} My Atom feed.}}
   {li {{a {href https://github.com/jack-faller}} On Github.}}
   {li {{a {href mailto:jack.t.faller@gmail.com}} jack.t.faller@gmail.com}}}
  {h2
   {{a {href ./atom.xml} {class headinglink}}
    Activity
    {{img {src /feed-icon.svg}
          {style height: 0.75em\; position: relative\; top: 0.05em\;}
          {alt logo used to represent RSS and ATOM feeds}}}}}
  {ul
   #@(->> (iter:take 30 (iter:from-list (fluid-ref public-posts)))
          (iter:map (page->li #t))
          (iter:collect! (sink:list)))
   {li {{a {href ./atom.xml}} See more …}}}}}
