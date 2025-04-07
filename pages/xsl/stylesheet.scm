(xsl-document
 {{xsl:template {match /}}
  #(page
	{h1
	 RSS Feed: {{xsl:value-of {select /rss/channel/title}}}}
	{{xsl:value-of {select /rss/channel/description}}}
	{ul
	 {{xsl:for-each {select /rss/channel/item}}
	  {li
	   {a
		{{xsl:attribute {name href}}
		 {{xsl:value-of {select link}}}}
		{{xsl:value-of {select title}}}}}}})})
