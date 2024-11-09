(uiop:define-package #:cl-telegram-bot2/actions/send-text
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/action
                #:action)
  (:import-from #:cl-telegram-bot2/generics
                #:on-result
                #:process
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/high
                #:reply)
  (:import-from #:serapeum
                #:->)
  (:import-from #:cl-telegram-bot2/utils
                #:call-if-needed)
  (:export #:send-text))
(in-package #:cl-telegram-bot2/actions/send-text)


(defclass send-text (action)
  ((text :initarg :text
         :type (or string
                   symbol)
         :reader text)))


(-> send-text ((or string symbol))
    (values send-text &optional))

(defun send-text (text-or-func-name)
  (when (and (symbolp text-or-func-name)
             (not (fboundp text-or-func-name)))
    (error "SEND-TEXT waits a text or fbound symbol. ~S is not fbound."
           text-or-func-name))
  
  (make-instance 'send-text
                 :text text-or-func-name))


(defmethod print-object ((obj send-text) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S"
            (text obj))))


(defmethod on-state-activation ((action send-text))
  (reply (call-if-needed
          (text action)))
  (values))


(defmethod process ((action send-text) update)
  (reply (call-if-needed
          (text action)))
  (values))

(defmethod on-result ((action send-text) result)
  (reply (call-if-needed
          (text action)))
  (values))
