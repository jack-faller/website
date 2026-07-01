(use-modules (guix gexp)
             (guix packages)
             (guix git-download)
             (guix build-system gnu)
             ((guix licenses) #:prefix license:)
             (gnu packages))

(define goaccess
  (package
   (name "goaccess")
   (version "1.10.2")
   (source
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/allinurl/goaccess")
            (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32 "1nyknlp6pg8wfb1lixnjgmfg6b0ra0fx36s06z7jhc1397g9jkwz"))))
   (build-system gnu-build-system)
   (native-inputs
    (map specification->package '("autoconf" "automake" "gettext")))
   (inputs
    (map specification->package '("ncurses" "libmaxminddb" "openssl")))
   (home-page "https://goaccess.io/")
   (synopsis "GoAccess is an open source real-time log analyzer and interactive viewer that runs in a terminal in *nix systems or through your browser.")
   (description "GoAccess is an open source, real-time web log analyzer and interactive viewer that runs in a terminal on *nix systems or directly in your browser. Designed with system administrators, DevOps engineers, and security professionals in mind, it delivers fast, actionable HTTP statistics and visual server reports on the fly. GoAccess parses your web server logs in real time and presents the data directly in the terminal or via a live HTML dashboard, making it easy to monitor traffic, detect anomalies, and troubleshoot issues instantly.")
   (license license:expat)))


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
    (cons*
     goaccess
     (map specification->package
          '("glfw" "glm" "glad" "pkg-config" "imagemagick" "gcc-toolchain@13"
            "libxslt"
            "tree-sitter" "tree-sitter-cli" "aha"))))
   (inputs (map specification->package '()))
   (home-page "https://jackfaller.xyz")
   (synopsis "My website")
   (description "My website")
   (license license:gpl3)))
my-website
