(defpackage #:cl-telegram-bot/update
  (:use #:cl)
  (:import-from #:cl-telegram-bot/message
                #:make-message)
  (:export
   #:make-update
   #:get-message
   #:get-raw-data
   #:get-update-id))
(in-package cl-telegram-bot/update)


(defclass update ()
  ((id :initarg :id
       :reader get-update-id)
   (message :initarg :message
            :reader get-message)
   (raw-data :initarg :raw-data
             :reader get-raw-data)))


(defun make-update (data)
  (let ((message-data (getf data :|message|)))
    (if message-data
        (make-instance 'update
                       :id (getf data :|update_id|)
                       :message (make-message message-data)
                       :raw-data data)
        (log:warn "Received not supported update"
                  data))))

