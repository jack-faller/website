(post
 "The Case Against Embed Links"
 "2024/03/08 23:59 +0000"
 {p
  If you use the internet a lot, you've certainly seen some kind of non-standard embed link.
  These are services such as {{a {href https://github.com/MinnDevelopment/fxreddit/tree/master}} {code fxreddit}} which essentially acts as a proxy to Reddit, but adds information that can be used to embed content into other applications.
  Essentially, a link to {code reddit.com/some/resource} is replaced with {code rxddit.com/some/resource}.
  When you click the link, it will take you to the Reddit page at {code some/resource} as any other link would.
  What this link adds is some metadata that allows a summary of the target page (for instance, the post title and an associated image) to be {q embedded} near the link in the source page.}
 {p
  These are phishing links.
  Practically, I don't think many of them of them actually steal login credentials, but they have the exact same functionality as phishing links.
  They try to appear as similar as possible as an ordinary, benign link to another website but instead take users to a slightly modified version of that website under the control of a 3rd party.
  All the providers of these links would have to do is redirect to their own version of whichever website they link to, ask users to log in, then store whatever is input and silently redirect the user to the link they wanted to go to.
  If the scammer does this correctly, you would never notice that anything has been stolen.}
 {p
  What's worse is that the owner of any established service could easily change it to implement this scam at any point without anything visibly changing to users.
  Even if they are not personally malicious, they could sell (or be tricked into giving) their URL to scammers who would then use it in the aforementioned ways.
  Worse still, if they ever want to stop providing the service and they end their lease of the domain name, a scammer could buy it and implement the scam in retroactively on all existing instances of the link.}
 {p
  While I'm not an expert in security, using these links looks like a potentially risky game to play.
  Presently, they seem to have the implicit blessing of sites they redirect to (they have yet to be sued for trademark infringement), but I can't say if any real security analysis has been conducted there.
  Even just from an abstract social sense, getting people used to the idea that links may be misspelled slightly will make it easier to trick people into phishing scams in the future.
  I know I've clicked on hundreds, maybe thousands of these links without considering the risks involved until now.})
