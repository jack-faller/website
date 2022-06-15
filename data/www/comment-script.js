function httpGetAsync(URL, callback, callbackFail) {
	let xmlHttpReq = new XMLHttpRequest()
	xmlHttpReq.onreadystatechange = function () {
		if (xmlHttpReq.readyState == 4) {
			if (xmlHttpReq.status == 200) {
				callback(xmlHttpReq.responseText)
			} else {
				callbackFail(xmlHttpReq.responseText)
			}
		}
	}
	xmlHttpReq.open("GET", URL, true) // true for asynchronous 
	xmlHttpReq.send(null)
}
function setComments(text) {
	document.getElementById("comments").innerHTML = text
}
const failText = "<h1>Post Not Found</h1> <p>I have yet to write the post.</p>"
const matches = window.location.href.match(new RegExp("^https?://jackfaller\\.xyz/post/(.*?)(.html)?$"))
document.addEventListener("DOMCommentsLoaded", function () {
	if (matches) {
		const url = "https://jackfaller.xyz/raw-posts/" + matches[1] + ".html"
		httpGetAsync(url, setComments, fail => setComments(failText))
	} else {
		setComments(failText)
	}
});
