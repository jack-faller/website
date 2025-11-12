(use-modules (doclisp) (make))
(set-reader! doclisp-reader)
{{thought {version #1}}
 {title This Is Not a Web Browser}
 {uuid 8ab2ae7d-e1c4-4ac8-8c3f-304d1f3a72e7}
 {published #"2025/11/10 10:47 +0000"}
 {description
  With the {{a {href https://groups.google.com/a/chromium.org/g/blink-dev/c/CxL4gYZeSJA/m/yNs4EsD5AQAJ}} impending doom} of XSLT, I got to thinking, this really isn't a Web browser anymore.}
 {body
  {p
   The Web (capital W) is a collection of HTML documents linked together to create a database of knowledge.
   This site is a Web site as it is just a collection of static XML/HTML documents.
   Yet I've concluded it's purely incidental that modern Web browsers are capable of accessing my website.
   The Web is no longer a concern for them, people like me are second class citizens of the internet.
   The modern browser exists as an application toolkit that just happens to also display HTML documents.
   I had always looked with some confusion at other web standards such as Gopher, that promise to replace the standard HTML+HTTP paradigm, but now I see a little more sense in them.}
  {p
   Removing XSLT may not seem so significant, but I think it would essentially be the death of the Web.
   Right now, a person can create some static XML documents, upload them cheaply to a file-hosting service, and have a website.
   All the links will work and lead you to other web pages that display in the browser.
   This is possible because RSS feeds exist which allow people to subscribe to eachother's content using static feed pages.
   XSLT allows users to attach a stylesheet to the RSS feed that transforms it into an HTML document so the user doesn't get an ugly mess of XML spat out by the browser.
   It is the small addition that takes this page from something kinda clunky which mostly works to a generally pleasant viewing experience.
   It's also worth nothing that XSLT allows people to essentially write an entire website using XML documents and have all the templating done on the browser end.
   It might seem a little arcane, but the experience is actually quite good, and most static site generators do little more than that.}
  {p
   Are many people writing their own HTML these days?
   Surely not.
   (Well, I write this site in a language which is basically HTML, but I don't really count as a person.)
   But you will soon need dynamic content to reach the basic level of {q a usable website}—a set of blog posts and feed that doesn't show you strange error messages with unformatted XML if you click the wrong link.
   I struggle not to view this as a kind of end for the Web.
   The pretence of {q Web browser} is being dropped completely, and the concerns of Web users totally disregarded.}}}
