(uiop:define-package #:cl-telegram-bot2/block/send-text
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/generics
                #:process
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/block/base
                #:base-block)
  (:import-from #:cl-telegram-bot2/pipeline
                #:back)
  (:export #:send-text))
(in-package #:cl-telegram-bot2/block/send-text)


(defclass send-text (base-block)
  ((text :initarg :text
         :type string
         :reader text)
   (on-text :initarg :on-text
            :initform nil
            :type (or null
                      base-block
                      back)
            :reader on-text-block)))


(defun send-text (text &key on-text)
  (make-instance 'send-text
                 :text text
                 :on-text on-text))


(defmethod on-state-activation ((state send-text))
  (cl-telegram-bot2/high:reply (text state))
  (values))


(defmethod process ((state send-text) update)
  (let* ((message
           (cl-telegram-bot2/api:update-message
            update))
         (text
           (when message
             (cl-telegram-bot2/api:message-text message))))
    
    (cond
      ((and text
            (on-text-block state))
       (values (on-text-block state)))
      (t
       (values)))))
