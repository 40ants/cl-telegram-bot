(uiop:define-package #:cl-telegram-bot2/actions/edit-message-media
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/action
                #:action)
  (:import-from #:cl-telegram-bot2/vars
                #:*current-state*
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
  (:import-from #:cl-telegram-bot2/states/base
                #:sent-message-ids)
  (:export #:edit-message-media
           #:caption
           #:media-path
           #:inline-keyboard))
(in-package #:cl-telegram-bot2/actions/edit-message-media)


(defclass edit-message-media (action)
  ((path :initarg :path
         :type (or string
                   pathname
                   symbol)
         :reader media-path)
   (caption :initarg :caption
            :type string
            :reader caption)
   (inline-keyboard :initarg :inline-keyboard
                    :type (soft-list-of string)
                    :reader inline-keyboard)))


(-> edit-message-media ((or string pathname symbol)
                        &key
                        (:caption string)
                        (:inline-keyboard (soft-list-of string)))
    (values edit-message-media &optional))


(defun edit-message-media (path-or-func-name &key caption inline-keyboard)
  (when (and (symbolp path-or-func-name)
             (not (fboundp path-or-func-name)))
    (error "EDIT-MESSAGE-MEDIA waits a path or fbound symbol. ~S is not fbound."
           path-or-func-name))
  
  (make-instance 'edit-message-media
                 :path path-or-func-name
                 :caption (or caption "")
                 :inline-keyboard inline-keyboard))


(defmethod print-object ((obj edit-message-media) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S"
            (media-path obj))))


(defun send-reply (action)
  (let ((path (call-if-needed
               (media-path action)))
        (caption (call-if-needed
                  (caption action)))
        (buttons (call-if-needed
                  (inline-keyboard action)))
        (message-id (first (sent-message-ids *current-state*)))
        (chat-id (chat-id *current-chat*)))
    
    (cond
      (message-id
       (cl-telegram-bot2/api:edit-message-media
        (make-instance 'cl-telegram-bot2/api:input-media-photo
                       :type "photo"
                       :media path
                       ;; These options aren't supported yet
                       ;; has_spoiler
                       ;; show_caption_above_media
                       ;; parse_mode
                       ;; caption_entities
                       :caption caption)
        :chat-id chat-id
        :message-id message-id
        :reply-markup
        (make-instance 'cl-telegram-bot2/api:inline-keyboard-markup
                       :inline-keyboard
                       (list
                        (loop for button in buttons
                              collect (make-instance 'cl-telegram-bot2/api:inline-keyboard-button
                                                     :text button
                                                     :callback-data button))))))
      (t
       (log:warn "There is no message-ids to edit in the"
                 *current-state*)))))


(defmethod on-state-activation ((action edit-message-media))
  (send-reply action)
  (values))


(defmethod process ((bot t) (action edit-message-media) update)
  (send-reply action)
  (values))


(defmethod on-result ((action edit-message-media) result)
  (send-reply action)
  (values))
