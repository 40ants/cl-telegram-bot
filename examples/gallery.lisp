(uiop:define-package #:cl-telegram-bot2-examples/gallery
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/bot
                #:defbot)
  (:import-from #:cl-telegram-bot2/server
                #:stop-polling
                #:start-polling)
  (:import-from #:cl-telegram-bot2/high
                #:reply
                #:chat-state)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:cl-telegram-bot2/pipeline
                #:back-to
                #:back)
  (:import-from #:cl-telegram-bot2/api
                #:message-message-id)
  (:import-from #:cl-telegram-bot2/state-with-commands
                #:global-command
                #:command
                #:state-with-commands-mixin)
  (:import-from #:cl-telegram-bot2/generics
                #:on-result
                #:on-state-activation
                #:process)
  (:import-from #:cl-telegram-bot2/state
                #:state)
  (:import-from #:cl-telegram-bot2/term/back
                #:back-to-id)
  (:import-from #:cl-telegram-bot2/actions/send-text
                #:send-text)
  (:import-from #:str
                #:trim)
  (:import-from #:cl-telegram-bot2/actions/send-photo
                #:send-photo)
  (:import-from #:cl-telegram-bot2/actions/edit-message-media
                #:edit-message-media)
  (:import-from #:cl-telegram-bot2/states/base
                #:var)
  (:documentation "This example shows how to keep use state's vars to keep current photo's index and to edit message's media when user clicks on Prev/Next buttons."))
(in-package #:cl-telegram-bot2-examples/gallery)


(defparameter *photos*
  (directory (uiop:wilden
              (asdf:system-relative-pathname
               :cl-telegram-bot2-examples
               (make-pathname :directory '(:relative "examples" "images"))))))


(defun make-keyboard (photo-index)
  (remove nil
          (list
           (unless (zerop photo-index)
             "Prev")
           (unless (= photo-index
                      (1- (length *photos*)))
             "Next"))))


(defun show-photo ()
  (let ((photo-index (var "photo-index")))

    (unless photo-index
      (setf photo-index 0)
      (setf (var "photo-index")
            photo-index))
  
    (send-photo (elt *photos* photo-index)
                :caption (fmt "Cat ~A" (1+ photo-index))
                :inline-keyboard (make-keyboard photo-index))))


(defun show-next-photo ()
  (let ((photo-index (min (1- (length *photos*))
                          (1+ (var "photo-index")))))
    
    (setf (var "photo-index")
          photo-index)

    (edit-message-media (elt *photos* photo-index)
                        :caption (fmt "Cat ~A" (1+ photo-index))
                        :inline-keyboard (make-keyboard photo-index))))

(defun show-prev-photo ()
  (let ((photo-index (max 0
                          (1- (var "photo-index")))))
    
    (setf (var "photo-index")
          photo-index)

    (edit-message-media (elt *photos* photo-index)
                        :caption (fmt "Cat ~A" (1+ photo-index))
                        :inline-keyboard (make-keyboard photo-index))))


(defbot test-bot ()
  ()
  (:initial-state
   (state 'show-photo
          :on-update 'show-photo
          :on-callback-query
          (list (cons "Next"
                      'show-next-photo)
                (cons "Prev"
                      'show-prev-photo)))))


;; Technical part

(defvar *bot* nil)


(defun clean-threads ()
  (loop for tr in (bt:all-threads)
        when (or (str:starts-with? "message-thread" (bt:thread-name tr))
                 (str:starts-with? "timer-wheel" (bt:thread-name tr))
                 (str:starts-with? "telegram-bot" (bt:thread-name tr)))
        do (bt:destroy-thread tr)))


(defun stop ()
  (when *bot*
    (stop-polling *bot*)
    (setf *bot* nil)
    (clean-threads))
  (values))


(defun start ()
  (stop)
  
  (unless *bot*
    (setf *bot*
          (make-test-bot (uiop:getenv "TELEGRAM_TOKEN"))))
  
  (start-polling *bot* :debug t))
