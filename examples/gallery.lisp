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
  (:import-from #:cl-telegram-bot2/actions/delete-messages
                #:delete-messages)
  (:import-from #:cl-telegram-bot2/callback
                #:callback)
  (:import-from #:cl-telegram-bot-media
                #:get-path-to-dir)
  (:documentation "This example shows how to keep use state's vars to keep current photo's index and to edit message's media when user clicks on Prev/Next buttons."))
(in-package #:cl-telegram-bot2-examples/gallery)


(defparameter *photos*
  (directory (uiop:wilden
              (get-path-to-dir "images" "cats"))))


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
          :id "gallery-example"
          :on-update 'show-photo
          :on-deletion (delete-messages)
          :on-callback-query
          (list (callback "Next"
                          'show-next-photo)
                (callback "Prev"
                          'show-prev-photo)))))


;; Technical part

(defvar *bot* nil)


(defun stop ()
  (when *bot*
    (stop-polling *bot*)
    (setf *bot* nil))
  (values))


(defun start ()
  (stop)
  
  (unless *bot*
    (setf *bot*
          (make-test-bot (uiop:getenv "TELEGRAM_TOKEN"))))
  
  (start-polling *bot* :debug t))
