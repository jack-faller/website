(use-modules (doclisp) (make))
(set-reader! doclisp-reader)
{{xsl {version #1}}
 {body
  {? xml version="1.0" encoding="UTF-8"}
  {{xsl:stylesheet {version 1.0}
		   {xmlns http://www.w3.org/1999/xhtml}
		   {xmlns:opml http://opml.org/spec2}
		   {xmlns:xsl http://www.w3.org/1999/XSL/Transform}
		   {exclude-result-prefixes opml}}
   ;; Not sure if setting doctype-public to "" is valid, but it's the only way to disable quirks mode.
   {{xsl:output {method html} {doctype-public} {encoding UTF-8} {indent yes}}}
   {{xsl:template {match /}}
    #(doclisp-template
      {{page {version #1}}
       {title Blogroll}
       {description
        Here's who I follow.
        This page is in the {{a {href https://opml.org/blogroll.opml}} blogroll} format.}
       {body
        {ul
         {{xsl:for-each {select opml:opml/opml:body/opml:outline[@title != 'Youtube']}}
          {li
           {a
            {{xsl:attribute {name href}}
             {xsl:choose
              {{xsl:when {test opml:outline/@htmlUrl}}
               {{xsl:value-of {select opml:outline/@htmlUrl}}}}
              {xsl:otherwise
               {{xsl:value-of {select opml:outline/@xmlUrl}}}}}}
            {{xsl:value-of {select opml:outline/@text}}}}}}}}})}}}}
