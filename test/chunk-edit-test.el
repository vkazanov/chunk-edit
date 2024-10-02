;;; chunk-edit-test.el --- Tests for chunk-edit.el -*- lexical-binding: t -*-

(require 'ert)
(require 'chunk-edit)


;;; File Backend Tests

(ert-deftest chunk-edit-file-backend-load-save-sync ()
  "Test loading, saving, and synchronization of the file backend."
  (chunk-edit-test--with-temp-file
   "Initial content of the file."
   (let* ((result (chunk-edit--file-load temp-file-path))
          (chunk (nth 0 result))
          (content (nth 1 result))
          (backend (plist-get chunk :backend)))
     (should (equal content "Initial content of the file."))
     ;; Modify the content and save
     (let ((new-content "Modified content of the file."))
       (funcall (plist-get backend :save) chunk new-content)
       ;; Verify that the file content has been updated
       (with-temp-buffer
         (insert-file-contents temp-file-path)
         (should (equal (buffer-string) new-content))))
     ;; Check synchronization
     (sleep-for 0.1)
     (should (funcall (plist-get backend :syncp) chunk))
     ;; Modify the file externally
     (delete-file temp-file-path) ;; overwrite
     (with-temp-file temp-file-path
       (insert "Externally modified content."))
     ;; Check synchronization again
     (sleep-for 0.1)
     (should-not (funcall (plist-get backend :syncp) chunk)))))

(ert-deftest chunk-edit-file-backend-revert ()
  "Test reverting a chunk from the file backend."
  (chunk-edit-test--with-temp-file
   "Original content."
   (let* ((result (chunk-edit--file-load temp-file-path))
          (chunk (nth 0 result))
          (content (nth 1 result))
          (backend (plist-get chunk :backend))
          ;; Create an overlay to simulate the chunk in buffer
          (test-buffer (generate-new-buffer " *chunk-edit-test*")))
     (unwind-protect
         (with-current-buffer test-buffer
           (insert content)
           (let ((overlay (make-overlay (point-min) (point-max))))
             (overlay-put overlay 'chunk chunk)
             (erase-buffer)
             (insert "Modified in chunk-edit buffer.")
             (move-overlay overlay (point-min) (point-max))
             ;; Revert the chunk
             (funcall (plist-get backend :revert) overlay chunk)
             ;; Check that the buffer content matches the file
             (should (equal (buffer-string) "Original content."))
             ;; Clean up overlay
             (delete-overlay overlay)))
       (kill-buffer test-buffer)))))

(ert-deftest chunk-edit-file-backend-open ()
  "Test opening the source file from the file backend."
  (chunk-edit-test--with-temp-file
   "Test content."
   (let* ((result (chunk-edit--file-load temp-file-path))
          (chunk (nth 0 result))
          (backend (plist-get chunk :backend)))
     ;; Ensure the file is not already open
     (let ((existing-buffer (find-buffer-visiting temp-file-path)))
       (when existing-buffer
         (kill-buffer existing-buffer)))
     ;; Open the chunk's source
     (funcall (plist-get backend :open) chunk)
     ;; Check that the buffer is now open and visiting the file
     (let ((buffer (find-buffer-visiting temp-file-path)))
       (should buffer)
       (with-current-buffer buffer
         (should (equal (buffer-string) "Test content."))))
     ;; Clean up
     (kill-buffer (find-buffer-visiting temp-file-path)))))

(ert-deftest chunk-edit-file-backend-kill ()
  "Test killing a file chunk (no-op for file backend)."
  (chunk-edit-test--with-temp-file
   "Some content."
   (let* ((result (chunk-edit--file-load temp-file-path))
          (chunk (nth 0 result))
          (backend (plist-get chunk :backend)))
     ;; Kill the chunk
     (funcall (plist-get backend :kill) chunk)
     ;; Since the file backend's kill is a no-op, there's nothing to check
     ;; We can ensure that no errors occur during the call
     t)))


;;; Buffer Region Backend Tests

(ert-deftest chunk-edit-region-backend-load-save-sync ()
  "Test loading, saving, and synchronization of the region backend."
  (chunk-edit-test--with-temp-buffer
   "Initial buffer content."
   (let* ((start (point-min))
          (end (point-max))
          (result (chunk-edit--region-load temp-buffer start end))
          (chunk (nth 0 result))
          (content (nth 1 result))
          (backend (plist-get chunk :backend)))
     (should (equal content "Initial buffer content."))
     ;; Modify the content and save
     (let ((new-content "Modified buffer content."))
       (funcall (plist-get backend :save) chunk new-content)
       ;; Verify that the buffer content has been updated
       (with-current-buffer temp-buffer
         (should (equal (buffer-string) new-content))))
     ;; Check synchronization
     (should (funcall (plist-get backend :syncp) chunk))
     ;; Modify the buffer externally
     (with-current-buffer temp-buffer
       (erase-buffer)
       (insert "Externally modified buffer content."))
     ;; Check synchronization again
     (should-not (funcall (plist-get backend :syncp) chunk)))))

(ert-deftest chunk-edit-region-backend-revert ()
  "Test reverting a chunk from the region backend."
  (chunk-edit-test--with-temp-buffer
   "Original buffer content."
   (let* ((start (point-min))
          (end (point-max))
          (result (chunk-edit--region-load temp-buffer start end))
          (chunk (nth 0 result))
          (content (nth 1 result))
          (backend (plist-get chunk :backend))
          ;; Create an overlay to simulate the chunk in buffer
          (test-buffer (generate-new-buffer " *chunk-edit-test*")))
     (unwind-protect
         (with-current-buffer test-buffer
           (insert content)
           (let ((overlay (make-overlay (point-min) (point-max))))
             (overlay-put overlay 'chunk chunk)
             ;; Modify the content
             (erase-buffer)
             (insert "Modified in chunk-edit buffer.")
             (move-overlay overlay (point-min) (point-max))
             ;; Revert the chunk
             (funcall (plist-get backend :revert) overlay chunk)
             ;; Check that the buffer content matches the source buffer
             (should (equal (buffer-string) "Original buffer content."))
             ;; Clean up overlay
             (delete-overlay overlay)))
       (kill-buffer test-buffer)))))

(ert-deftest chunk-edit-region-backend-open ()
  "Test opening the source buffer from the region backend."
  (chunk-edit-test--with-temp-buffer
   "Test buffer content."
   (let* ((start (point-min))
          (end (point-max))
          (result (chunk-edit--region-load temp-buffer start end))
          (chunk (nth 0 result))
          (backend (plist-get chunk :backend))
          (other-buffer (generate-new-buffer "other-buffer")))
     ;; Kill the buffer to simulate it not being current
     (switch-to-buffer other-buffer)
     (should (not (equal (buffer-string) "Test buffer content.")))
     ;; Open the chunk's source
     (funcall (plist-get backend :open) chunk)
     ;; Check that the buffer is now open
     (should (equal (buffer-string) "Test buffer content.")))))

(ert-deftest chunk-edit-region-backend-kill ()
  "Test killing a region chunk and ensuring markers are cleaned up."
  (chunk-edit-test--with-temp-buffer
   "Content to test markers."
   (let* ((start (point-min))
          (end (point-max))
          (result (chunk-edit--region-load temp-buffer start end))
          (chunk (nth 0 result))
          (backend (plist-get chunk :backend))
          (start-marker (plist-get chunk :start-marker))
          (end-marker (plist-get chunk :end-marker)))
     (should (marker-position start-marker))
     (should (marker-position end-marker))
     ;; Kill the chunk
     (funcall (plist-get backend :kill) chunk)

     (should-not (marker-position start-marker))
     (should-not (marker-position end-marker)))))

(provide 'chunk-edit-test)
