(defpackage #:cl-telegram-bot/chat
  (:use #:cl)
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
