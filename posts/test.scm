(post
 "Test Post"
 "2023/02/18 11:30 +0100"
 "A post for testing my site generator."
 {p Testing,#(note "test-1" "Test note 1")
	testing,#(note "test-2" "Test note 2.")
	123#(note-ref "test-1")}
 {p this is a keyword}
 (code-block "test/test.py")
 (code-block "test/test.lsp"))
