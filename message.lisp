(defpackage #:cl-telegram-bot/message
  (:use #:cl)
  (:import-from #:cl-telegram-bot/chat
                #:make-chat)
  (:export
   #:make-message
   #:get-text
   #:get-raw-data
   #:get-chat))
(in-package cl-telegram-bot/message)


(defclass message ()
  ((text :initarg :text
         :reader get-text)
   (chat :initarg :chat
         :reader get-chat)
   (raw-data :initarg :raw-data
             :reader get-raw-data)))


(defun make-message (data)
  (make-instance 'message
                 :text (getf data :|text|)
                 :chat (make-chat (getf data :|chat|))
                 :raw-data data))


(defmethod print-object ((message message) stream)
  (print-unreadable-object
      (message stream :type t)
    (format stream
            "text=~A chat=~A"
            (get-text message)
            (get-chat message))))
