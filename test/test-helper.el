;;; test-helper.el --- Helpers -*- lexical-binding: t -*-

(defmacro chunk-edit-test--with-temp-file (content &rest body)
  "Create a temporary file with CONTENT and execute BODY.
The path to the temporary file is bound to `temp-file-path` during BODY."
  `(let ((temp-file-path (make-temp-file "chunk-edit-test")))
     (unwind-protect
         (progn
           (with-temp-file temp-file-path
             (insert ,content))
           ,@body)
       (delete-file temp-file-path))))

(defmacro chunk-edit-test--with-temp-buffer (content &rest body)
  "Create a temporary buffer with CONTENT and execute BODY.
The buffer is bound to `temp-buffer` during BODY."
  `(let ((temp-buffer (generate-new-buffer " *chunk-edit-test-buffer*")))
     (unwind-protect
         (with-current-buffer temp-buffer
           (insert ,content)
           ,@body)
       (when (buffer-live-p temp-buffer)
         (kill-buffer temp-buffer)))))
