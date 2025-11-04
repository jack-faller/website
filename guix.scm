(use-modules (guix gexp)
	     (guix packages)
	     (guix git-download)
             (guix build-system gnu)
	     ((guix licenses) #:prefix license:)
	     (gnu packages))

(define my-website
  (package
   (name "my-website")
   (version "0.0.0")
   (source (local-file (dirname (current-filename)) #:recursive? #t))
   (build-system gnu-build-system)
   (arguments (list
	       #:phases
	       (with-imported-modules '((guix build utils))
				      #~(begin
					  (use-modules (guix build utils))
					  (modify-phases
					   %standard-phases
					   (delete 'configure)
					   (delete 'check)
					   (replace 'install
						    (lambda* (#:key outputs #:allow-other-keys)
						      (copy-recursively "servdir" (assoc-ref outputs "out")))))))))
   (native-inputs
    (map specification->package
         '("glfw" "glm" "glad" "pkg-config" "imagemagick" "gcc-toolchain@13"
           "libxslt"
           "tree-sitter" "tree-sitter-cli" "aha")))
   (inputs (map specification->package '()))
   (home-page "https://jackfaller.xyz")
   (synopsis "My website")
   (description "My website")
   (license license:gpl3)))
my-website
