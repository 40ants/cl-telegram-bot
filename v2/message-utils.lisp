(uiop:define-package #:cl-telegram-bot2/message-utils
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/api
                #:message-forward-origin
                #:message-origin-channel
                #:message-origin-channel-chat
                #:message-origin-channel-message-id
                #:message-chat
                #:message-message-id
                #:chat-id)
  (:import-from #:serapeum
                #:->)
  (:import-from #:log)
  (:documentation "Helpers to extract chat and message ids from Telegram messages.")
  (:export #:get-chat-id-and-message-id
           #:trim-channel-id))
(in-package #:cl-telegram-bot2/message-utils)


(-> get-chat-id-and-message-id (cl-telegram-bot2/api:message)
    (values integer integer (or null cl-telegram-bot2/api:message-origin) &optional))
(defun get-chat-id-and-message-id (message)
  "Return the chat id, the message id and the forward origin of the MESSAGE.

When the message was forwarded from a channel, the chat id and the message id
of the original message in the source channel are returned.  This is useful
for building a link to the original message in the channel.

For any other forward origin type (a user, a hidden user or a chat) the
MESSAGE's own chat id and message id are returned, because such origins
don't point to a message inside a channel.

The third value is the message's forward origin, or NIL when the message
was not forwarded."
  (let ((forward-origin (message-forward-origin message)))
    (cond
      ((typep forward-origin 'message-origin-channel)
       (let ((chat (message-origin-channel-chat forward-origin)))
         (values (chat-id chat)
                 (message-origin-channel-message-id forward-origin)
                 forward-origin)))
      (t
       (values (chat-id (message-chat message))
               (message-message-id message)
               forward-origin)))))


(-> trim-channel-id (integer)
    (values integer &optional))
(defun trim-channel-id (chat-id)
  "Make from -1002102092834 2102092834, cutting the -100 prefix of a channel or supergroup id.

Values above -1000000000000 are returned as is, but a warning is logged,
because such ids don't belong to a channel."
  (cond
    ((< chat-id -1000000000000)
     (- (+ chat-id
           1000000000000)))
    (t
     (log:warn "Chat id ~A is not a channel id, probably results will be wrong"
               chat-id)
     chat-id)))
