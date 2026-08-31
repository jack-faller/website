(use-modules (doclisp) (make))
(set-reader! doclisp-reader)
{{xsl {version #1}}
 {dummy-source feed-recent.xml}
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
   {{xsl:template {match /dummy}}
    {{xsl:apply-templates {select document("./atom.xml")}}}}
   {{xsl:template {match atom:feed}}
    {html
     {head
      {{base {target _parent}}}
      {{link {rel stylesheet} {type text/css} {href /font.css}}}}
     {{body {style margin: 0\;}}
      {ul
       {{xsl:for-each {select atom:entry}}
        {{xsl:sort {select position()} {data-type number} {order descending}}}
        {li
         {a
          {{xsl:attribute {name href}}
           {{xsl:value-of {select atom:content/@src}}}}
          {{xsl:value-of {select j:date}}}
          –
          {{xsl:value-of {select atom:category/@label}}}
          –
          {{xsl:value-of {select atom:title}}}}}}}}}}}}}
