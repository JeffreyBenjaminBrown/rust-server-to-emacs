(defvar rust-doc--proc nil
  "Persistent TCP connection to the Rust backend.")

(defvar rust-doc-buffer-name "*rust-doc*"
  "Buffer name for displaying contents from the Rust server.")

(defun rust-doc-handle-response (proc string)
  "Handle the JSON response from the Rust server."
  (let* ((json-object-type 'plist)
         (response (ignore-errors
		     (json-read-from-string string))))
    (when response
      (let ((contents (plist-get response :contents)))
        (with-current-buffer
	    (get-buffer-create rust-doc-buffer-name)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert contents))
          (set-visited-file-name nil) ;; ← detaches from any file
          (set-buffer-modified-p nil)
          (read-only-mode 1)
          (switch-to-buffer (current-buffer)))))))

(defun rust-doc-connect ()
  "Ensure a persistent connection to the Rust TCP server exists."
  (unless (and rust-doc--proc (process-live-p rust-doc--proc))
    (setq rust-doc--proc
          (make-network-process
           :name "rust-doc"
           :buffer "*rust-doc-raw*"
           :host "127.0.0.1"
           :service 1729
           :filter #'rust-doc-handle-response
           :coding 'utf-8
           :nowait nil)))
  rust-doc--proc)

(defun rust-doc-request-file (path)
  "Send a JSON request to the Rust server to retrieve the file PATH."
  (interactive "FPath to file: ")
  (let* ((request `(:action "get-file" :path ,path))
         (json (json-encode request))
         (proc (rust-doc-connect)))
    (process-send-string proc (concat json "\n"))))

(defun rust-doc-disconnect ()
  "Manually close the persistent Rust server connection."
  (interactive)
  (when (process-live-p rust-doc--proc)
    (delete-process rust-doc--proc)
    (setq rust-doc--proc nil)))
