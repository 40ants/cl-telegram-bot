(uiop:define-package #:cl-telegram-bot2-tests/files
  (:use #:cl)
  (:import-from #:rove
                #:ok
                #:deftest)
  (:import-from #:serapeum
                #:->)
  (:import-from #:cl-telegram-bot2/api
                #:message
                #:photo-size
                #:video)
  (:import-from #:cl-telegram-bot2/files
                #:has-photo-p
                #:has-video-p
                #:largest-photo-size))
(in-package #:cl-telegram-bot2-tests/files)

;; Only pure functions are tested here, because there is no network access
;; on CI. DOWNLOAD-FILE, DOWNLOAD-PHOTO-SIZE, DOWNLOAD-PHOTO and DOWNLOAD-VIDEO
;; are not covered by tests.


(-> make-photo-size (string integer)
    (values photo-size &optional))
(defun make-photo-size (unique-id width)
  (make-instance 'photo-size
                 :file-id (format nil "file-~A" unique-id)
                 :file-unique-id unique-id
                 :width width
                 :height width))


(-> make-message (&key (:photo list) (:video (or null video)))
    (values message &optional))
(defun make-message (&key photo video)
  (apply #'make-instance 'message
         (nconc (when photo
                  (list :photo photo))
                (when video
                  (list :video video)))))


(deftest test-largest-photo-size ()
  (let* ((small (make-photo-size "small" 90))
         (medium (make-photo-size "medium" 640))
         (large (make-photo-size "large" 1280))
         (message (make-message :photo (list medium small large))))
    (ok (eql large
             (largest-photo-size message))
        "The widest photo size is returned regardless of the order in the message."))
  (ok (null (largest-photo-size (make-message)))
      "NIL is returned for a message without a photo."))


(deftest test-has-photo-p ()
  (ok (has-photo-p (make-message :photo (list (make-photo-size "small" 90))))
      "T is returned for a message with a photo.")
  (ok (not (has-photo-p (make-message)))
      "NIL is returned for a message without a photo."))


(deftest test-has-video-p ()
  (ok (has-video-p (make-message :video (make-instance 'video
                                                       :file-id "file-id"
                                                       :file-unique-id "unique-id")))
      "T is returned for a message with a video.")
  (ok (not (has-video-p (make-message)))
      "NIL is returned for a message without a video."))
