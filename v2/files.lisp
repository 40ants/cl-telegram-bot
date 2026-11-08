(uiop:define-package #:cl-telegram-bot2/files
  (:use #:cl)
  (:import-from #:serapeum
                #:fmt
                #:->)
  (:import-from #:dex)
  (:import-from #:alexandria
                #:read-stream-content-into-byte-vector
                #:write-byte-vector-into-file)
  (:import-from #:cl-telegram-bot2/api
                #:file-file-path
                #:get-file
                #:message
                #:message-photo
                #:message-video
                #:photo-size
                #:photo-size-file-id
                #:photo-size-file-unique-id
                #:photo-size-width
                #:video-file-id
                #:video-file-unique-id)
  (:import-from #:cl-telegram-bot2/spec
                #:*token*)
  (:documentation "Functions to download files, photos and videos from Telegram.")
  (:export #:download-file
           #:download-photo-size
           #:download-photo
           #:download-video
           #:has-photo-p
           #:has-video-p
           #:largest-photo-size))
(in-package #:cl-telegram-bot2/files)


(-> download-file (pathname string &key (:unique-id string))
    (values pathname &optional))
(defun download-file (target-dir file-id &key (unique-id file-id))
  "Download a file with the given FILE-ID from Telegram into the TARGET-DIR.

The file is saved under its UNIQUE-ID, keeping the extension
of the file path returned by the Telegram API. When UNIQUE-ID is not given,
FILE-ID is used as the file name.

When the file already exists in the TARGET-DIR, it is not downloaded again
and the existing path is returned."
  (let* ((file (get-file file-id))
         (file-path (file-file-path file))
         (target-path
           (merge-pathnames
            (make-pathname :name unique-id
                           :type (pathname-type file-path))
            target-dir))
         (url (fmt "https://api.telegram.org/file/bot~A/~A"
                   *token*
                   file-path)))

    (unless (probe-file target-path)
      (ensure-directories-exist target-path)
      (let ((stream (dex:get url :want-stream t)))
        (unwind-protect
             (write-byte-vector-into-file
              (read-stream-content-into-byte-vector stream)
              target-path
              :if-exists :supersede)
          (close stream))))

    (values target-path)))


(-> download-photo-size (pathname photo-size)
    (values pathname &optional))
(defun download-photo-size (target-dir photo-size)
  "Download the PHOTO-SIZE into the TARGET-DIR.

The file is named after the photo size's unique id."
  (download-file target-dir
                 (photo-size-file-id photo-size)
                 :unique-id (photo-size-file-unique-id photo-size)))


(-> largest-photo-size (message)
    (values (or null photo-size) &optional))
(defun largest-photo-size (message)
  "Return the widest photo size of the MESSAGE.

Returns NIL when the message has no photo."
  (first (sort (copy-list (message-photo message))
               #'>
               :key #'photo-size-width)))


(-> has-photo-p (message)
    (values boolean &optional))
(defun has-photo-p (message)
  "Return T if the MESSAGE has a photo."
  (when (message-photo message)
    t))


(-> has-video-p (message)
    (values boolean &optional))
(defun has-video-p (message)
  "Return T if the MESSAGE has a video."
  (when (message-video message)
    t))


(-> download-photo (pathname message)
    (values (or null pathname) &optional))
(defun download-photo (target-dir message)
  "Download the largest photo of the MESSAGE into the TARGET-DIR.

Returns NIL when the message has no photo."
  (let ((size (largest-photo-size message)))
    (when size
      (download-photo-size target-dir size))))


(-> download-video (pathname message)
    (values (or null pathname) &optional))
(defun download-video (target-dir message)
  "Download the video of the MESSAGE into the TARGET-DIR.

The file is named after the video's unique id.
Returns NIL when the message has no video."
  (let ((video (message-video message)))
    (when video
      (download-file target-dir
                     (video-file-id video)
                     :unique-id (video-file-unique-id video)))))
