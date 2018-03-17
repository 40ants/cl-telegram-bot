(defpackage :cl-telegram-bot/bot
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:dexador)
  (:import-from #:jonathan)
  (:export
   #:bot
   #:access
   #:set-webhook
   #:get-webhook-info
   #:send-message
   #:forward-message
   #:send-photo
   #:send-audio
   #:send-document
   #:send-video
   #:send-voice
   #:send-video-note
   #:send-location
   #:send-venue
   #:send-contact
   #:get-user-profile-photos
   #:get-file
   #:answer-callback-query
   #:edit-message-text
   #:edit-message-caption
   #:edit-message-reply-markup
   #:delete-message
   #:send-sticker
   #:get-sticker-set
   #:upload-sticker-file
   #:create-new-sticker-set
   #:add-sticker-to-set
   #:set-sticker-position-in-set
   #:delete-sticker-from-set
   #:answer-inline-query
   #:send-invoice
   #:answer-shipping-query
   #:answer-pre-checkout-query
   #:send-game
   #:set-game-score
   #:get-game-high-scores
   #:get-last-update-id
   #:defbot))

(in-package cl-telegram-bot/bot)


(defclass bot ()
  ((id
    :documentation "Update id"
    :initform 0
    :accessor get-last-update-id)
   (token
    :initarg :token
    :documentation "Bot token given by BotFather"
    :accessor token
    :initform nil)
   (api-uri
    :initarg  :api-uri
    :initform "https://api.telegram.org/"
    :accessor api-uri)
   (endpoint
    :initarg :endpoint
    :reader get-endpoint
    :documentation "HTTPS endpoint")
   (file-endpoint
    :initarg :file-endpoint
    :accessor file-endpoint
    :documentation "HTTPS file-endpoint"
    :initform nil)))


(defmacro defbot (name)
  `(progn
     (defclass ,name (bot)
       ())

     (defun ,(alexandria:symbolicate 'make- name) (token)
       (make-instance ',name
                      :token token))))


(defmethod initialize-instance :after ((bot bot) &key &allow-other-keys)
  (with-accessors ((token         token)
                   (file-endpoint file-endpoint)
                   (api-uri       api-uri)) bot
    (setf (slot-value bot 'endpoint)
          (concatenate 'string api-uri "bot" token "/")
          (slot-value bot 'file-endpoint)
          (concatenate 'string api-uri "file/" "bot" token "/"))))


(defmethod print-object ((bot bot) stream)
  (print-unreadable-object
      (bot stream :type t)
    (format stream
            "id=~A" (get-last-update-id bot))))


