(use-modules (doclisp) (make))
(set-reader! doclisp-reader)
{{xsl {version #1}}
 {body
  {? xml version="1.0" encoding="UTF-8"}
  {{xsl:stylesheet {version 1.0}
		   {xmlns http://www.w3.org/1999/xhtml}
		   {xmlns:xsl http://www.w3.org/1999/XSL/Transform}
		   {xmlns:atom http://www.w3.org/2005/Atom}
		   {xmlns:j http://jackfaller.xyz}
		   {exclude-result-prefixes atom j}}
   ;; Not sure if setting doctype-public to "" is valid, but it's the only way to disable quirks mode.
   {{xsl:output {method html} {doctype-public} {encoding UTF-8} {indent yes}}}
   {{xsl:template {match /}}
    #(page
      {h1 Posts}
      {p
       This is an Atom feed (similar to RSS).
       Copy the link from the URL of this page into your feed reader to subscribe.
       Be sure to add it to your public {{a {href https://opml.org/blogroll.opml}} blogroll} so others can see it.}
      {{ul {class no-bullet}}
       {{xsl:for-each {select atom:feed/atom:entry}}
        {{xsl:sort {select position()} {data-type number} {order descending}}}
        {li
         {a
          {{xsl:attribute {name href}}
           {{xsl:value-of {select atom:content/@src}}}}
          {{xsl:value-of {select j:date}}}
          –
          {{xsl:value-of {select atom:category/@term}}}
          –
          {{xsl:value-of {select atom:title}}}}}}})}}}}
