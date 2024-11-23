(uiop:define-package #:cl-telegram-bot2/actions/send-photo
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/action
                #:action)
  (:import-from #:cl-telegram-bot2/vars
                #:*current-chat*)
  (:import-from #:cl-telegram-bot2/api
                #:message-chat
                #:update-message
                #:update
                #:chat-id
                #:send-message)
  (:import-from #:cl-telegram-bot2/generics
                #:on-result
                #:process
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/high
                #:reply)
  (:import-from #:serapeum
                #:soft-list-of
                #:->)
  (:import-from #:cl-telegram-bot2/utils
                #:call-if-needed)
  (:export #:send-photo))
(in-package #:cl-telegram-bot2/actions/send-photo)


(defclass send-photo (action)
  ((path :initarg :path
         :type (or string
                   pathname
                   symbol)
         :reader image-path)
   (caption :initarg :caption
            :type string
            :reader caption)
   (inline-keyboard :initarg :inline-keyboard
                    :type (soft-list-of string)
                    :reader inline-keyboard)))


(-> send-photo ((or string pathname symbol)
                &key
                (:caption string)
                (:inline-keyboard (soft-list-of string)))
    (values send-photo &optional))

(defun send-photo (path-or-func-name &key caption inline-keyboard)
  (when (and (symbolp path-or-func-name)
             (not (fboundp path-or-func-name)))
    (error "SEND-PHOTO waits a path or fbound symbol. ~S is not fbound."
           path-or-func-name))
  
  (make-instance 'send-photo
                 :path path-or-func-name
                 :caption (or caption "")
                 :inline-keyboard inline-keyboard))


(defmethod print-object ((obj send-photo) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S"
            (image-path obj))))


(defun reply-with-image (action)
  (let ((path (call-if-needed
               (image-path action)))
        (caption (call-if-needed
                  (caption action)))
        (buttons (call-if-needed
                  (inline-keyboard action))))
    (cl-telegram-bot2/api:send-photo
     (chat-id *current-chat*)
     path
     :caption caption
     :reply-markup
     (make-instance 'cl-telegram-bot2/api:inline-keyboard-markup
                    :inline-keyboard
                    (list
                     (loop for button in buttons
                           collect (make-instance 'cl-telegram-bot2/api:inline-keyboard-button
                                                  :text button
                                                  :callback-data button)))))))


(defmethod on-state-activation ((action send-photo))
  (reply-with-image action)
  (values))


(defmethod process ((action send-photo) update)
  (reply-with-image action)
  (values))


(defmethod on-result ((action send-photo) result)
  (reply-with-image action)
  (values))
