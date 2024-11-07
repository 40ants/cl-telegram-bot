(uiop:define-package #:cl-telegram-bot2/actions/send-text
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/action
                #:action)
  (:import-from #:cl-telegram-bot2/generics
                #:process
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/high
                #:reply)
  (:export #:send-text))
(in-package #:cl-telegram-bot2/actions/send-text)


(defclass send-text (action)
  ((text :initarg :text
         :type string
         :reader text)))


(defun send-text (text)
  (make-instance 'send-text
                 :text text))


(defmethod print-object ((obj send-text) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "\"~A\""
            (text obj))))


(defmethod on-state-activation ((action send-text))
  (reply (text action))
  (values))


(defmethod process ((action send-text) update)
  (reply (text action))
  (values))
