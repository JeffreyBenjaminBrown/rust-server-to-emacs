(defun rust-docker-send (message)
  "Send MESSAGE to the Rust server running in Docker and print the response."
  (interactive "sMessage to send: ")
  (let ((host "127.0.0.1")
        (port 1729))
    (with-temp-buffer
      (let ((proc (open-network-stream "rust-conn" (current-buffer) host port)))
        (process-send-string proc (concat message "\n"))
        ;; Wait until the process exits
        (while (process-live-p proc)
          (accept-process-output proc 0.1))
        (message "Response: %s" (string-trim (buffer-string)))))))

;; example usage
(rust-docker-send "mork")
