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
  "Connect to the Rust TCP server and return the process."
  (make-network-process
   :name "rust-doc"
   :buffer "*rust-doc-raw*"
   :host "127.0.0.1"
   :service 1729  ;; ← updated port
   :filter #'rust-doc-handle-response
   :coding 'utf-8
   :nowait nil))

(defun rust-doc-request-file (path)
  "Send PATH to the Rust server and show the contents in a buffer."
  (interactive "FPath to file: ")
  (let ((proc (rust-doc-connect)))
    (process-send-string proc (concat path "\n"))))

(rust-doc-request-file "2-persistent/data.txt")
