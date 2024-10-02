;;; chunk-edit.el --- Edit code chunks from multiple files in one buffer -*- lexical-binding: t -*-

;; Copyright (C) 2024  Vladimir Kazanov

;; Author: Vladimir Kazanov <vekazanov@gmail.com>
;; Version: 0.1
;; URL: https://github.com/vkazanov/chunk-edit
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, editing

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; chunk-edit-mode allows users to edit chunks of code from various
;; sources within a single buffer.  It provides syntax highlighting and
;; the ability to save changes back to the original files or buffers.
;;
;; Usage:
;;
;; - Use `M-x chunk-edit` to open the chunk-edit buffer.
;; - In any buffer, select a region and run `M-x chunk-edit-insert-region`
;;   to insert it as a chunk.
;; - Use `M-x chunk-edit-insert-file` to insert an entire file as a chunk.
;; - Within the chunk-edit buffer, use the following keybindings:
;;   - `C-c s` to save changes back to the source.
;;   - `C-c C-s` to save all changes back to sources.
;;   - `C-c r` to revert the chunk from the source.
;;   - `C-c C-r` to revert the chunk from the source.
;;   - `C-c k` to kill the chunk.
;;   - `C-c o` to open the source at the chunk's location.
;;   - `C-c q` to quit the chunk-edit buffer.
;;   - `C-c n` to move to the next chunk.
;;   - `C-c p` to move to the previous chunk.

;;; Code:


;;; Customization

(defgroup chunk-edit nil
  "Edit chunks of code from various sources."
  :group 'tools)

(defcustom chunk-edit-buffer-name "*chunk-edit*"
  "Name of the `chunk-edit' buffer."
  :type 'string
  :group 'chunk-edit)

(defface chunk-edit-modified-face
  '((t :inherit font-lock-warning-face))
  "Face used for modified chunks."
  :group 'chunk-edit)

(defface chunk-edit-unsynced-face
  '((t :inherit font-lock-warning-face :underline t))
  "Face used for unsynced chunks."
  :group 'chunk-edit)


;;; Helper functions

(defun chunk-edit--find-overlay-at-point ()
  "Find a chunk overlay at point if it exists."
  (catch 'found
    (dolist (ov (overlays-at (point)))
      (when (overlay-get ov 'chunk)
        (throw 'found ov)))))

(defun chunk-edit--find-overlays ()
  "Return a list of all chunk overlays in the current buffer."
  (let (overlays)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'chunk)
        (push ov overlays)))
    overlays))

(defun chunk-edit--get-buffer-create ()
  "Return the check-edit buffer, create one if necessary."
  (or (get-buffer chunk-edit-buffer-name)
      (let ((buf (get-buffer-create chunk-edit-buffer-name)))
        (with-current-buffer buf
          (chunk-edit-mode))
        buf)))

(defun chunk-edit--mark-modified (overlay after &rest _args)
  "Mark the chunk as modified or unmodified.
OVERLAY is the overlay being modified.  AFTER indicates whether
the change is after the fact."
  (when after
    (chunk-edit--create-or-update-overlay
     overlay (overlay-start overlay) (overlay-end overlay)
     (overlay-get overlay 'chunk)
     t
     (overlay-get overlay 'chunk-unsynced))))

(defun chunk-edit--mark-unsynced (overlay)
  "Mark the OVERLAY's chunk as unsynced."
  (chunk-edit--create-or-update-overlay
   overlay (overlay-start overlay) (overlay-end overlay)
   (overlay-get overlay 'chunk)
   (overlay-get overlay 'chunk-modified)
   t))

(defun chunk-edit--create-or-update-overlay (overlay start end chunk &optional modified unsynced)
  "Create or update an overlay for the chunk between START and END.
If OVERLAY is non-nil, update it instead of creating a new one.
If MODIFIED is non-nil, mark the chunk as modified."
  (let ((ov (or overlay (make-overlay start end)))
        (backend (plist-get chunk :backend))
        (face (cond
               ((and modified unsynced) 'chunk-edit-unsynced-face)
               (modified 'chunk-edit-modified-face)
               (unsynced 'chunk-edit-unsynced-face)
               (t 'font-lock-comment-face)))
        (mod-str (if modified " *MODIFIED*" ""))
        (unsync-str (if unsynced " *UNSYNC*" "")))
    ;; Store the chunk ID and modified state within the overlay for future reference
    (move-overlay ov start end)
    (overlay-put ov 'chunk chunk)
    (overlay-put ov 'chunk-modified modified)
    (overlay-put ov 'chunk-unsynced unsynced)
    (overlay-put ov 'before-string
                 (propertize (format "[%s MODE: %s %s%s]\n"
                                     (funcall (plist-get backend :header) chunk)
                                     (plist-get chunk :mode)
                                     mod-str
                                     unsync-str)
                             'face face))
    (overlay-put ov 'after-string
                 (propertize (format "[END]\n") 'face face))
    ;; Set the overlay to evaporate, which means it will be
    ;; deleted when the associated text is deleted
    (overlay-put ov 'evaporate t)
    ;; Add an after-change function to track modifications
    (overlay-put ov 'modification-hooks '(chunk-edit--mark-modified))
    ov))

(defun chunk-edit--insert-chunk (chunk content)
  "Insert a CHUNK's CONTENT into the buffer at current point."
  ;; Abort insertion if point is within an existing chunk overlay
  (when (chunk-edit--find-overlay-at-point)
    (user-error "Cannot insert a chunk within an existing chunk"))

  (let ((start (point))
        end)
    (insert content)
    (setq end (point))

    (chunk-edit--create-or-update-overlay nil start end chunk)))

(defun chunk-edit--check-source-modifications ()
  "Check if chunk source files or buffers have been modified.
The chunks will be marked as unsynced."
  (let (out-of-sync-chunks)

    ;; Collect chunks that are out of sync
    (dolist (overlay (chunk-edit--find-overlays))
      (let* ((chunk (overlay-get overlay 'chunk))
             (backend (plist-get chunk :backend)))
        (unless (funcall (plist-get backend :syncp) chunk)
          (push (cons overlay chunk) out-of-sync-chunks))))

    ;; Mark unsynced chunk overlays
    (dolist (pair out-of-sync-chunks)
      (let* ((overlay (car pair)))
        (chunk-edit--mark-unsynced overlay)))))

(defun chunk-edit--on-window-selection-change (_window)
  "Check if the check-edit buffer's chunks are in sync with sources."
  (when (eq (current-buffer) (chunk-edit--get-buffer-create))
    (chunk-edit--check-source-modifications)))


;;; Backends


;;;; File backend

(defconst chunk-edit--file-backend
  (list :header #'chunk-edit--file-header
        :revert #'chunk-edit--file-revert
        :save #'chunk-edit--file-save
        :open #'chunk-edit--file-open
        :kill #'chunk-edit--file-kill
        :syncp #'chunk-edit--file-syncp))

(defun chunk-edit--file-load (path)
  "Load a chunk from PATH."

  (unless (file-readable-p path)
    (user-error "Cannot read file: %s" path))

  ;; TODO: special handling for files that are already opened
  ;; TODO: remove buffers of files that were not opened previously?
  (with-current-buffer
      (find-file-noselect path)
    (let ((chunk (list :path path
                       :last-modified (file-attribute-modification-time
                                       (file-attributes path))
                       :mode major-mode
                       :backend chunk-edit--file-backend))
          (content (buffer-string)))
      (message "File chunk loaded from %s" path)
      (list chunk content))))

(defun chunk-edit--file-header (chunk)
  "Generate header string for CHUNK."
  (format "FILE PATH: %s" (plist-get chunk :path)))

(defun chunk-edit--file-save (chunk new-content)
  "Save NEW-CONTENT to the file specified in CHUNK."
  (let ((path (plist-get chunk :path)))

    (unless (file-readable-p path)
      (user-error "Cannot read file: %s" path))

    (with-temp-file path
      (insert new-content))
    (plist-put chunk :last-modified (file-attribute-modification-time
                                     (file-attributes path)))

    (message "Chunk saved to %s" path)))

(defun chunk-edit--file-revert (overlay chunk)
  "Revert OVERLAY content with the content from CHUNK's file."
  (let ((path  (plist-get chunk :path))
        new-content)
    (unless (file-readable-p path)
      (user-error "Cannot read chunk file: %s" path))
    (setq new-content (with-temp-buffer
                        (insert-file-contents path)
                        (buffer-string)))
    (save-excursion
      (goto-char (overlay-start overlay))
      (delete-region (overlay-start overlay) (overlay-end overlay))
      (chunk-edit--insert-chunk chunk new-content))
    (plist-put chunk :last-modified (file-attribute-modification-time
                                     (file-attributes path)))

    (message "Chunk reverted from %s" path)))

(defun chunk-edit--file-open (chunk)
  "Open the file specified in CHUNK."
  (let ((file-path (plist-get chunk :path)))
    (unless (file-readable-p file-path)
      (user-error "Chunk file not found: %s" file-path))
    (find-file file-path)
    (message "File chunk source %s opened" file-path)))

(defun chunk-edit--file-kill (_chunk)
  "Perform any necessary cleanup when a file CHUNK is killed.")

(defun chunk-edit--file-syncp (chunk)
  "Check if the CHUNK is in sync with the file."
  (let* ((file-path (plist-get chunk :path))
         (chunk-modified (plist-get chunk :last-modified))
         (file-modified (file-attribute-modification-time
                         (file-attributes file-path))))
    (when (time-equal-p file-modified chunk-modified)
      t)))


;;;; Buffer region backend

(defconst chunk-edit--buffer-region-backend
  (list :header #'chunk-edit--region-header
        :revert #'chunk-edit--region-revert
        :save #'chunk-edit--region-save
        :open #'chunk-edit--region-open
        :kill #'chunk-edit--region-kill
        :syncp #'chunk-edit--region-syncp))

(defun chunk-edit--region-load (buffer start end)
  "Load a chunk from BUFFER between START and END."
  (unless (buffer-live-p buffer)
    (user-error "Invalid buffer"))

  (with-current-buffer buffer
    (let* ((start-marker (copy-marker start))
           ;; tailing t for end-insertion-type
           (end-marker (copy-marker end t))
           (content (buffer-substring-no-properties start end))
           (chunk (list :buffer buffer
                        :start-marker start-marker
                        :end-marker end-marker
                        :buffer-modified-tick (buffer-chars-modified-tick)
                        :mode major-mode
                        :backend chunk-edit--buffer-region-backend)))
      (message "Region chunk loaded from buffer %s [%d, %d]" (buffer-name)
               (marker-position start-marker) (marker-position end-marker))
      (list chunk content))))

(defun chunk-edit--region-header (chunk)
  "Generate header string for CHUNK."
  (let ((buffer (plist-get chunk :buffer)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (format "BUFFER REGION: %s" (buffer-name buffer))))

(defun chunk-edit--region-save (chunk new-content)
  "Save NEW-CONTENT back to the buffer region specified in CHUNK."
  (let ((buffer (plist-get chunk :buffer))
        (start-marker (plist-get chunk :start-marker))
        (end-marker (plist-get chunk :end-marker)))
    (unless (and (buffer-live-p buffer)
                 (marker-buffer start-marker)
                 (marker-buffer end-marker))
      (user-error "Buffer or markers no longer exist"))
    (with-current-buffer buffer
      (save-excursion
        (let ((start (marker-position start-marker))
              (end (marker-position end-marker)))
          (goto-char start)
          (delete-region start end)
          (insert new-content)
          ;; Update the end marker's position
          (set-marker end-marker (point))
          ;; Update the chunk's modified tick
          (plist-put chunk :buffer-modified-tick (buffer-chars-modified-tick)))))
    (message "Chunk saved back to buffer %s" (buffer-name buffer))))

(defun chunk-edit--region-revert (overlay chunk)
  "Revert OVERLAY content with the content from CHUNK's buffer region."
  (let ((buffer (plist-get chunk :buffer))
        (start-marker (plist-get chunk :start-marker))
        (end-marker (plist-get chunk :end-marker))
        new-content)
    (unless (and (buffer-live-p buffer)
                 (marker-position start-marker)
                 (marker-position end-marker))
      (user-error "Buffer or markers no longer exist"))
    (with-current-buffer buffer
      (let ((start (marker-position start-marker))
            (end (marker-position end-marker)))
        (setq new-content (buffer-substring-no-properties start end))
        (plist-put chunk :buffer-modified-tick (buffer-chars-modified-tick))))
    ;; Replace the overlay content
    (save-excursion
      (goto-char (overlay-start overlay))
      (delete-region (overlay-start overlay) (overlay-end overlay))
      (chunk-edit--insert-chunk chunk new-content))
    (message "Chunk reverted from buffer %s" (buffer-name buffer))))

(defun chunk-edit--region-open (chunk)
  "Open the buffer specified in CHUNK and go to the region."
  (let ((buffer (plist-get chunk :buffer))
        (start-marker (plist-get chunk :start-marker)))
    (unless (and (buffer-live-p buffer)
                 (marker-position start-marker))
      (user-error "Buffer or markers no longer exist"))
    (switch-to-buffer buffer)
    (goto-char (marker-position start-marker))
    (message "Region chunk source opened in buffer %s" (buffer-name buffer))))

(defun chunk-edit--region-kill (chunk)
  "Kill the region CHUNK."
  (let ((start-marker (plist-get chunk :start-marker))
         (end-marker (plist-get chunk :end-marker)))
    (when (marker-buffer start-marker)
        (set-marker start-marker nil))
    (when (marker-buffer end-marker)
      (set-marker end-marker nil))))

(defun chunk-edit--region-syncp (chunk)
  "Check if the CHUNK is in sync with the buffer."
  (let* ((buffer (plist-get chunk :buffer))
         (chunk-modified-tick (plist-get chunk :buffer-modified-tick))
         (buffer-modified-tick (with-current-buffer buffer
                                 (buffer-chars-modified-tick))))
    (eq buffer-modified-tick chunk-modified-tick)))


;;; Fontification

(defun chunk-edit--fontify-chunk (lang-mode start end)
  "Fontify code block between START and END using LANG-MODE's syntax."
  ;; NOTE: This is inspired by the way
  ;; `org-src-font-lock-fontify-block' works. Probably has quite a lot
  ;; of hidden problems.

  (let ((modified (buffer-modified-p)))
    (remove-text-properties start end '(face nil))

    (add-text-properties
     start end '(font-lock-fontified t font-lock-multiline t))

    (when (fboundp lang-mode)
      (let ((string (buffer-substring-no-properties start end))
            (source-buffer (current-buffer)))
        (with-current-buffer
            (get-buffer-create (format " *chunk-fontification:%s*" lang-mode))
          (let ((inhibit-modification-hooks t))
            (erase-buffer)
            ;; Add string and a final space to ensure property change.
            (insert string " ")
            (unless (eq major-mode lang-mode) (funcall lang-mode))
            (font-lock-ensure)
            (let ((pos (point-min)) next)
              (while (setq next (next-property-change pos))
                (dolist (prop '(font-lock-face face))
                  (let ((new-prop (get-text-property pos prop)))
                    (when new-prop
                      (put-text-property (+ start (1- pos)) (1- (+ start next))
                                         prop new-prop source-buffer))))
                (setq pos next)))))))
    (set-buffer-modified-p modified)))

(defun chunk-edit--fontify-chunks (limit)
  "Apply syntax highlighting to chunks up to LIMIT."
  ;; TODO: would it be more efficient to maintain a list of chunk
  ;; overlays?

  (dolist (overlay (overlays-in (point) limit))
    (when-let* ((chunk (overlay-get overlay 'chunk))
                (lang-mode (plist-get chunk :mode)))
      (chunk-edit--fontify-chunk
       lang-mode (overlay-start overlay) (overlay-end overlay))))
  nil)


;;; Interface


(defvar chunk-edit--killing-buffer nil
  "Non-nil when the `chunk-edit' buffer is being killed.")

(defun chunk-edit--on-kill-buffer ()
  "Run cleanup when the `chunk-edit' buffer is killed."
  (unless chunk-edit--killing-buffer
    (chunk-edit-quit)))

(defvar-keymap chunk-edit-mode-map
  :doc "Keymap for the chunk-edit-mode."
  ;; chunk manipulation
  "C-c s" #'chunk-edit-save-chunk-at-point
  "C-c C-s" #'chunk-edit-save-all-chunks
  "C-c r" #'chunk-edit-revert-chunk-at-point
  "C-c C-r" #'chunk-edit-revert-all-chunks
  "C-c k" #'chunk-edit-kill-chunk-at-point
  "C-c o" #'chunk-edit-open-chunk-at-point
  "C-c q" #'chunk-edit-quit
  ;; navigation
  "C-c n" #'chunk-edit-next-chunk
  "C-c p" #'chunk-edit-previous-chunk)

(define-derived-mode chunk-edit-mode text-mode "Chunk-Edit"
  "Editing chunks from various sources.

The following commands are available:

\\{chunk-edit-mode-map}"
  (setq-local font-lock-defaults '(((chunk-edit--fontify-chunks)) t nil nil))

  (add-hook 'window-selection-change-functions
            #'chunk-edit--on-window-selection-change
            nil t)

  (add-hook 'kill-buffer-hook #'chunk-edit--on-kill-buffer nil t))

;;;###autoload
(defun chunk-edit ()
  "Open or create the *chunk-edit* buffer and switch to it."
  (interactive)
  (pop-to-buffer
   (chunk-edit--get-buffer-create)))

;;;###autoload
(defun chunk-edit-insert-file (file-path)
  "Insert the FILE-PATH chunk into the *chunk-edit* buffer."
  (interactive "fSelect file: ")

  (with-current-buffer (chunk-edit--get-buffer-create)
    (goto-char (point-max))
    (newline)
    (apply #'chunk-edit--insert-chunk (chunk-edit--file-load file-path))))

;;;###autoload
(defun chunk-edit-insert-region ()
  "Insert the active region as a chunk into the *chunk-edit* buffer.
If there is no active region, insert the entire buffer content."
  (interactive)

  (let* ((buf (current-buffer))
         (start (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max))))

    (with-current-buffer (chunk-edit--get-buffer-create)
      (goto-char (point-max))
      (newline)
      (apply #'chunk-edit--insert-chunk (chunk-edit--region-load buf start end)))))

(defun chunk-edit-next-chunk ()
  "Move point to the beginning of the next chunk."
  (interactive nil chunk-edit-mode)
  (if-let ((overlay (chunk-edit--find-overlay-at-point)))
      (goto-char (overlay-end overlay)))
  (goto-char (next-overlay-change (point)))
  (while (and (not (eobp))
              (not (chunk-edit--find-overlay-at-point)))
    (goto-char (next-overlay-change (point)))))

(defun chunk-edit-previous-chunk ()
  "Move point to the beginning of the previous chunk."
  (interactive nil chunk-edit-mode)
  (if-let ((overlay (chunk-edit--find-overlay-at-point)))
      (goto-char (overlay-start overlay)))
  (goto-char (previous-overlay-change (point)))
  (while (and (not (bobp))
              (not (chunk-edit--find-overlay-at-point)))
    (goto-char (previous-overlay-change (point)))))

(defun chunk-edit-save-chunk-at-point ()
  "Save the chunk at point back to its source file."
  (interactive nil chunk-edit-mode)

  (let ((overlay (chunk-edit--find-overlay-at-point)))
    (unless overlay
      (user-error "No chunk at point to save"))

    (let* ((chunk (overlay-get overlay 'chunk))
           (backend (plist-get chunk :backend))
           (new-content (buffer-substring-no-properties
                         (overlay-start overlay)
                         (overlay-end overlay))))

      (funcall (plist-get backend :save) chunk new-content)

      ;; Mark chunk as unmodified
      (chunk-edit--create-or-update-overlay
       overlay (overlay-start overlay) (overlay-end overlay) chunk)))
  (message "Chunk saved successfully."))

(defun chunk-edit-save-all-chunks ()
  "Save all modified chunks back to their sources."
  (interactive nil chunk-edit-mode)
  (let ((overlays (chunk-edit--find-overlays))
        (saved-count 0))
    (dolist (overlay overlays)
      (when (overlay-get overlay 'chunk-modified)
        (let* ((chunk (overlay-get overlay 'chunk))
               (backend (plist-get chunk :backend))
               (chunk-content (buffer-substring-no-properties
                               (overlay-start overlay)
                               (overlay-end overlay))))
          (funcall (plist-get backend :save) chunk chunk-content)
          ;; Mark chunk as unmodified
          (chunk-edit--create-or-update-overlay
           overlay (overlay-start overlay) (overlay-end overlay) chunk)
          (setq saved-count (1+ saved-count)))))
    (message "Saved %d modified chunk(s)." saved-count)))

(defun chunk-edit-revert-chunk-at-point ()
  "Revert the chunk at point from its source file."
  (interactive nil chunk-edit-mode)

  (let ((overlay (chunk-edit--find-overlay-at-point)))
    (unless overlay
      (user-error "No chunk at point to revert"))

    ;; Build a new chunk
    (let* ((chunk (overlay-get overlay 'chunk))
           (backend (plist-get chunk :backend)))
      (funcall (plist-get backend :revert) overlay chunk))))

(defun chunk-edit-revert-all-chunks ()
  "Revert all chunks from their sources.
If a chunk is modified, prompt for confirmation before reverting."
  (interactive nil chunk-edit-mode)
  (let ((overlays (chunk-edit--find-overlays))
        (reverted-count 0))
    (dolist (overlay overlays)
      (let* ((chunk (overlay-get overlay 'chunk))
             (backend (plist-get chunk :backend)))
        (when (or (not (overlay-get overlay 'chunk-modified))
                  (y-or-n-p (format "Chunk modified: %s. Revert from source? "
                                    (funcall (plist-get backend :header) chunk))))
          (funcall (plist-get backend :revert) overlay chunk)
          (setq reverted-count (1+ reverted-count)))))
    (message "Reverted %d chunk(s)." reverted-count)))

(defun chunk-edit-kill-chunk-at-point ()
  "Kill the chunk at point, suggesting to save if it is modified."
  (interactive nil chunk-edit-mode)
  ;; Find the overlay at point that represents a chunk
  (let* ((overlay (chunk-edit--find-overlay-at-point)))
    (unless overlay
      (user-error "No chunk at point to close"))

    (let* ((chunk (overlay-get overlay 'chunk))
           (backend (plist-get chunk :backend)))

      ;; Suggest saving if the chunk is modified
      (when (overlay-get overlay 'chunk-modified)
        (when (y-or-n-p "Chunk is modified.  Save before closing? ")
          (chunk-edit-save-chunk-at-point)))

      (funcall (plist-get backend :kill) chunk)

      ;; Delete the chunk's content and the overlay
      (delete-region (overlay-start overlay) (overlay-end overlay))
      (delete-overlay overlay))

    (message "Chunk closed")))

(defun chunk-edit-open-chunk-at-point ()
  "Open the source of chunk at point.
This command pushes a mark to the global mark ring and then jumps
to the source of the chunk."
  (interactive nil chunk-edit-mode)
  ;; Find the overlay at point that represents a chunk
  (let ((overlay (chunk-edit--find-overlay-at-point)))
    (unless overlay
      (user-error "No chunk at point"))

    (let* ((chunk (overlay-get overlay 'chunk))
           (backend  (plist-get chunk :backend)))
      (push-mark nil t) ;; silently push a global mark
      (funcall (plist-get backend :open) chunk))))

(defun chunk-edit-quit ()
  "Quit the chunk-edit buffer, properly killing on all chunks."
  (interactive nil chunk-edit-mode)

  ;; Prevent recursive calls
  (let ((chunk-edit--killing-buffer t)
        (overlays (chunk-edit--find-overlays)))

    (dolist (overlay overlays)
      (let* ((chunk (overlay-get overlay 'chunk))
             (backend (plist-get chunk :backend))
             (chunk-content (buffer-substring-no-properties
                             (overlay-start overlay)
                             (overlay-end overlay))))
        ;; If the chunk is modified, prompt to save
        (when (overlay-get overlay 'chunk-modified)
          (when (y-or-n-p (format "Chunk modified: %s. Save before closing? "
                                  (funcall (plist-get backend :header) chunk)))
            ;; Save the chunk
            (funcall (plist-get backend :save) chunk chunk-content)

            ;; Mark chunk as unmodified
            (chunk-edit--create-or-update-overlay
             overlay (overlay-start overlay) (overlay-end overlay) chunk)))

        ;; Call the :kill function of the backend
        (funcall (plist-get backend :kill) chunk)))

    (kill-buffer (current-buffer))))

(provide 'chunk-edit)
;;; chunk-edit.el ends here
