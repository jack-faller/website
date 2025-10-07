(use-modules (doclisp) (make))
(set-reader! doclisp-reader)
{{xsl {version #1}}
 {body
  {? xml version="1.0" encoding="UTF-8"}
  {{xsl:stylesheet {version 1.0}
		   {xmlns http://opml.org/spec2}
		   {xmlns:opml http://opml.org/spec2}
		   {xmlns:xsl http://www.w3.org/1999/XSL/Transform}
		   {exclude-result-prefixes opml}}
   {{xsl:output {method xml} {version 1.0} {encoding UTF-8} {indent yes}}}
   {{xsl:template {match /}}
    {{xsl:processing-instruction {name xml-stylesheet}}
     type="text/xsl" href="./blogroll.xsl"}
    {{opml {version 2.0}}
     {head
      {title Jack Faller's Blogroll}
      {dateCreated {{xsl:value-of {select opml:opml/opml:head/opml:dateCreated}}}}
      {ownerName Jack Faller}
      {ownerEmail jack.t.faller@gmail.com}
      {ownerId https://jackfaller.xyz}
      {docs https://opml.org/spec2.opml}}
     {{xsl:copy-of {select opml:opml/opml:body}}}}}}}}
