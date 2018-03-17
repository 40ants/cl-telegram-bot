(defpackage #:cl-telegram-bot/chat
  (:use #:cl)
  (:import-from #:cl-telegram-bot/network
                #:make-request)
  (:import-from #:cl-telegram-bot/telegram-call
                #:prepare-arg
                #:def-telegram-call
                #:response)
  (:import-from #:alexandria
                #:ensure-symbol)
  (:export
   #:make-chat
   #:get-raw-data
   #:get-chat-id
   #:get-username
   #:get-first-name
   #:get-last-name
   #:chat
   #:private-chat
   #:get-chat-by-id
   #:export-chat-invite-link
   #:promote-chat-member
   #:restrict-chat-member
   #:unban-chat-member
   #:kick-chat-member
   #:set-chat-title
   #:delete-chat-photo
   #:set-chat-photo
   #:set-chat-description
   #:pin-chat-message
   #:unpin-chat-message
   #:leave-chat
   #:get-chat-administrators
   #:get-chat-members-count
   #:get-chat-member
   #:send-chat-action))
(in-package cl-telegram-bot/chat)


(defclass chat ()
  ((id :initarg :id
       :reader get-chat-id)
   (raw-data :initarg :raw-data
             :reader get-raw-data)))


(defclass private-chat (chat)
  ((username :initarg :username
             :reader get-username)
   (first-name :initarg :first-name
               :reader get-first-name)
   (last-name :initarg :last-name
              :reader get-last-name)))


(defun make-chat (data)
  (unless (string-equal (getf data :|type|)
                        "private")
    (error "Only private chats are supported for now."))
  
  (make-instance 'private-chat
                 :id (getf data :|id|)
                 :username (getf data :|username|)
                 :first-name (getf data :|first_name|)
                 :last-name (getf data :|last_name|)
                 :raw-data data))


(defmethod print-object ((chat private-chat) stream)
  (print-unreadable-object
      (chat stream :type t)
    (format stream
            "id=~A username=~A"
            (get-chat-id chat)
            (get-username chat))))


(defmethod prepare-arg ((arg (eql :chat)))
  `(:|chat_id| (get-chat-id
                ,(ensure-symbol arg))))


(def-telegram-call (get-chat-by-id "getChat")
    (chat-id)
  "https://core.telegram.org/bots/api#getchat"
  (make-chat response))


(def-telegram-call kick-chat-member (chat user-id until-date)
  "https://core.telegram.org/bots/api#kickchatmember")


(def-telegram-call unban-chat-member (chat user-id)
  "https://core.telegram.org/bots/api#unbanchatmember")


(def-telegram-call restrict-chat-member (chat
                                         user-id
                                         until-date
                                         can-send-messages
                                         can-send-media-messages
                                         can-send-other-messages
                                         can-add-web-page-previews)
  "https://core.telegram.org/bots/api#restrictchatmember")


(def-telegram-call promote-chat-member (chat
                                        user-id
                                        can-change-info
                                        can-post-messages
                                        can-edit-messages
                                        can-delete-messages
                                        can-invite-users
                                        can-restrict-members
                                        can-pin-messages
                                        can-promote-members)
  "https://core.telegram.org/bots/api#promotechatmember")


(def-telegram-call export-chat-invite-link (chat)
  "https://core.telegram.org/bots/api#exportchatinvitelink")


(def-telegram-call set-chat-photo (chat photo)
  "https://core.telegram.org/bots/api#setchatphoto")


(def-telegram-call delete-chat-photo (chat)
  "https://core.telegram.org/bots/api#deletechatphoto")


(def-telegram-call set-chat-title (chat title)
  "https://core.telegram.org/bots/api#setchattitle")


(def-telegram-call set-chat-description (chat description)
  "https://core.telegram.org/bots/api#setchatdescription")


(def-telegram-call pin-chat-message (chat message-id disable-notification)
  "https://core.telegram.org/bots/api#pinchatmessage")


(def-telegram-call unpin-chat-message (chat)
  "https://core.telegram.org/bots/api#unpinchatmessage")


(def-telegram-call leave-chat (chat)
  "https://core.telegram.org/bots/api#leavechat")


(def-telegram-call get-chat-administrators (chat)
  "https://core.telegram.org/bots/api#getchatadministrators")


(def-telegram-call get-chat-members-count (chat)
  "https://core.telegram.org/bots/api#getchatmemberscount")


(def-telegram-call get-chat-member (chat user-id)
  "https://core.telegram.org/bots/api#getchatmember")


(def-telegram-call send-chat-action (chat action)
  "https://core.telegram.org/bots/api#sendchataction")
