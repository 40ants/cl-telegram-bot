(defpackage :cl-telegram-bot
  (:use #:cl)
  (:nicknames :telegram-bot :tg-bot)
  (:size 5)
  (:export
   #:bot
   #:make-bot
   #:with-package
   #:access
   #:request-error
   #:decode
   #:get-updates
   #:set-webhook
   #:send-message
   #:forward-message
   #:send-photo
   #:send-audio
   #:send-document
   #:send-sticker
   #:send-video
   #:send-voice
   #:send-location
   #:send-venue
   #:send-contact
   #:send-chat-action
   #:get-user-profile-photos
   #:get-file
   #:kick-chat-member
   #:leave-chat
   #:unban-chat-member
   #:get-chat
   #:get-chat-administrators
   #:get-chat-members-count
   #:get-chat-member
   #:answer-callback-query
   #:edit-message-text
   #:edit-message-caption
   #:edit-message-reply-markup
   #:answer-inline-query))
