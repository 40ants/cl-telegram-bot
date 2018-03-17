(defpackage #:cl-telegram-bot/chat
  (:use #:cl)
  (:import-from #:cl-telegram-bot/network
                #:make-request)
  (:export
   #:make-chat
   #:get-raw-data
   #:get-chat-id
   #:get-username
   #:get-first-name
   #:get-last-name
   #:chat
   #:private-chat))
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


(defun get-chat-by-id (bot chat-id)
  "https://core.telegram.org/bots/api#getchat"
  ;; TODO: test
  (let* ((options `(:|chat_id| ,chat-id))
         (response (make-request bot "getChat" options)))))


(defun kick-chat-member (bot chat user-id until-date)
  "https://core.telegram.org/bots/api#kickchatmember"
  (let ((options
          `(:|chat_id| ,(get-chat-id chat)
            :|user_id| ,user-id
            :|until_date| ,until-date)))
    (make-request bot "kickChatMember" options)))


(defun unban-chat-member (bot chat user-id)
  "https://core.telegram.org/bots/api#unbanchatmember"
  (let ((options
          `(:|chat_id| ,(get-chat-id chat)
            :|user_id| ,user-id)))
    (make-request bot "unbanChatMember" options)))


(defun restrict-chat-member (bot chat user-id until-date can-send-messages can-send-media-messages can-send-other-messages can-add-web-page-previews)
  "https://core.telegram.org/bots/api#restrictchatmember"
  (let ((options
          `(:|chat_id| ,(get-chat-id chat)
            :|user_id| ,user-id
            :|until_date| ,until-date
            :|can_send_messages| ,can-send-messages
            :|can_send_media_messages| ,can-send-media-messages
            :|can_send_other_messages| ,can-send-other-messages
            :|can_add_web_page_previews| ,can-add-web-page-previews)))
    (make-request bot "restrictChatMember" options)))


(defun promote-chat-member (bot chat user-id can-change-info can-post-messages can-edit-messages can-delete-messages can-invite-users can-restrict-members can-pin-messages can-promote-members)
  "https://core.telegram.org/bots/api#promotechatmember"
  (let ((options
          `(:|chat_id| ,(get-chat-id chat)
            :|user_id| ,user-id
            :|can_change_info| ,can-change-info
            :|can_post_messages| ,can-post-messages
            :|can_edit_messages| ,can-edit-messages
            :|can_delete_messages| ,can-delete-messages
            :|can_invite_users| ,can-invite-users
            :|can_restrict_members| ,can-restrict-members
            :|can_pin_messages| ,can-pin-messages
            :|can_promote_members| ,can-promote-members)))
    (make-request bot "promoteChatMember" options)))


(defun export-chat-invite-link (bot chat)
  "https://core.telegram.org/bots/api#exportchatinvitelink"
  (let ((options
          `(:|chat_id| ,(get-chat-id chat))))
    (make-request bot "exportChatInviteLink" options)))


(defun set-chat-photo (bot chat photo)
  "https://core.telegram.org/bots/api#setchatphoto"
  (let ((options
          `(:|chat_id| ,(get-chat-id chat)
            :|photo| ,photo)))
    (make-request bot "setChatPhoto" options)))


(defun delete-chat-photo (bot chat)
  "https://core.telegram.org/bots/api#deletechatphoto"
  (let ((options
          `(:|chat_id| ,(get-chat-id chat))))
    (make-request bot "deleteChatPhoto" options)))


(defun set-chat-title (bot chat title)
  "https://core.telegram.org/bots/api#setchattitle"
  (let ((options
          `(:|chat_id| ,(get-chat-id chat)
            :|title| ,title)))
    (make-request bot "setChatTitle" options)))


(defun set-chat-description (bot chat description)
  "https://core.telegram.org/bots/api#setchatdescription"
  (let ((options
          `(:|chat_id| ,(get-chat-id chat)
            :|description| ,description)))
    (make-request bot "setChatDescription" options)))


(defun pin-chat-message (bot chat message-id disable-notification)
  "https://core.telegram.org/bots/api#pinchatmessage"
  (let ((options
          (list
           :|chat_id| (get-chat-id chat)
           :|message_id| message-id
           :|disable_notification| disable-notification)))
    (make-request bot "pinChatMessage" options)))


(defun unpin-chat-message (bot chat)
  "https://core.telegram.org/bots/api#unpinchatmessage"
  (let ((options
          (list :|chat_id| (get-chat-id chat))))
    (make-request b "unpinChatMessage" options)))


(defun leave-chat (bot chat)
  "https://core.telegram.org/bots/api#leavechat"
  (let ((options
          (list :|chat_id| (get-chat-id chat))))
    (make-request bot "leaveChat" options)))


(defun get-chat-administrators (bot chat)
  "https://core.telegram.org/bots/api#getchatadministrators"
  (let ((options
         (list
          :|chat_id| (get-chat-id chat))))
    (make-request bot "getChatAdministrators" options)))


(defun get-chat-members-count (bot chat)
  "https://core.telegram.org/bots/api#getchatmemberscount"
  (let ((options
         (list
          :|chat_id| (get-chat-id chat))))
    (make-request bot "getChatMembersCount" options)))


(defun get-chat-member (bot chat user-id)
  "https://core.telegram.org/bots/api#getchatmember"
  (let ((options
         (list
          :|chat_id| (get-chat-id chat)
          :|user_id| user-id)))
    (make-request bot "getChatMember" options)))


(defun send-chat-action (bot chat action)
  "https://core.telegram.org/bots/api#sendchataction"
  (let ((options
         (list
          :|chat_id| (get-chat-id chat)
          :|action| action)))
    (make-request bot "sendChatAction" options)))
