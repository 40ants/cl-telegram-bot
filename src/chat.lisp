(uiop:define-package #:cl-telegram-bot/chat
  (:use #:cl)
  (:import-from #:closer-mop
                #:class-slots
                #:slot-definition-initargs)
  (:import-from #:kebab)
  (:import-from #:cl-telegram-bot/network
                #:make-request)
  (:import-from #:cl-telegram-bot/telegram-call
                #:prepare-arg
                #:def-telegram-call
                #:response)
  (:import-from #:alexandria
                #:ensure-symbol)
  (:export #:get-raw-data
           #:get-chat-id
           #:get-username
           #:get-first-name
           #:get-last-name
           #:chat
           #:private-chat
           #:group
           #:super-group
           #:channel
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
           #:send-chat-action
           #:get-has-private-forwards
           #:get-bio
           #:get-message-auto-delete-time
           #:get-has-protected-content
           #:get-sticker-set-name
           #:get-slow-mode-delay
           #:get-join-by-request
           #:get-join-to-send-messages
           #:get-can-set-sticker-set
           #:get-chat))
(in-package cl-telegram-bot/chat)


(defclass chat ()
  ((id :initarg :id
       :reader get-chat-id)
   (username :initarg :username
             :reader get-username)
   (has-protected-content :initarg :has-protected-content
                          :reader get-has-protected-content)
   (message-auto-delete-time :initarg :message-auto-delete-time
                             :reader get-message-auto-delete-time)
   (raw-data :initarg :raw-data
             :reader get-raw-data)))

(defmethod initialize-instance :after ((chat chat) &key data &allow-other-keys)
  (when data
    (let* ((slots (remove
                   'pinned-message
                   (remove
                    'raw-data
                    (mapcar #'closer-mop:slot-definition-name
                            (closer-mop:class-slots (class-of chat))))))
           (underscored-slots (mapcar (lambda (slot)
                                        (kebab:to-snake-case (intern (symbol-name slot) :keyword)))
                                      slots)))
      (mapc
       (lambda (slot underscored)
         (setf (slot-value chat slot) (getf data underscored)))
       slots underscored-slots)
      (setf (slot-value chat 'raw-data) data))))

(defclass private-chat (chat)
  ((first-name :initarg :first-name
               :reader get-first-name)
   (last-name :initarg :last-name
              :reader get-last-name)
   (bio :initarg :bio
        :reader get-bio)
   (has-private-forwards :initarg :has-private-forwards
                         :reader get-has-private-forwards)))

(defclass base-group (chat)
  ((linked-chat-id :initarg :linked-chat-id
                   :reader get-linked-chat-id)
   (invite-link :initarg :invite-link
                :reader get-invite-link)
   (pinned-message :initarg :pinned-message
                   :reader get-pinned-message)
   (title :initarg :title
          :reader get-title)
   (description :initarg :description
                :reader get-description)))

(defmethod initialize-instance :after ((chat base-group) &key data &allow-other-keys)
  (when data
    (setf (slot-value chat 'pinned-message)
          ;; FIXME: There should be make-message, but it's not available yet.
          (getf data :|pinned_message|))))

(defclass group (base-group)
  ())


(defclass super-group (base-group)
  ((join-to-send-messages :initarg :join-to-send-messages
                          :reader get-join-to-send-messages)
   (join-by-request :initarg :join-by-request
                    :reader get-join-by-request)
   (slow-mode-delay :initarg :slow-mode-delay
                    :reader get-slow-mode-delay)
   (sticker-set-name :initarg :sticker-set-name
                     :reader get-sticker-set-name)
   (can-set-sticker-set :initarg :can-set-sticker-set
                        :reader get-can-set-sticker-set)))

(defclass channel (base-group)
  ())

(defun make-chat (data)
  (when data
    (let* ((type (getf data :|type|))
           (class (closer-mop:ensure-finalized
                   (find-class
                    (cond
                      ((string-equal type "group") 'group)
                      ((string-equal type "supergroup") 'super-group)
                      ((string-equal type "channel") 'channel)
                      (t 'private-chat))))))
      (make-instance class :data data))))


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



(defgeneric get-chat (obj)
  (:documentation "Returns a chat associated with object.

                   Object could be a message, update, callback, etc. Should return an object of CHAT class or NIL.
                   Some types of updates aren't bound to a chat. In this case a method should return NIL.")
  (:method ((obj t))
    (values nil)))
