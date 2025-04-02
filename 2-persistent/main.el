(progn
  (load-file "lib.el")
  (rust-doc-connect) )

(;; This can be run multiple times.
 ;; The connection will persist across them --
 ;; which is visible both in Emacs,
 ;; and from the Rust server in the Docker container.
 ;;
 ;; The connection can even be closed here and reopened,
 ;; and the Rust server will keep running, unphased.
 rust-doc-request-file "2-persistent/data.txt")

(rust-doc-disconnect)
