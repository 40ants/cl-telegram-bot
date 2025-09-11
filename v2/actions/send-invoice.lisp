(uiop:define-package #:cl-telegram-bot2/actions/send-invoice
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/action
                #:action)
  (:import-from #:cl-telegram-bot2/generics
                #:process-state
                #:on-result
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/high
                #:reply)
  (:import-from #:serapeum
                #:soft-list-of
                #:->)
  (:import-from #:cl-telegram-bot2/utils
                #:call-if-needed)
  (:import-from #:cl-telegram-bot2/workflow
                #:workflow-blocks
                #:workflow-block)
  (:import-from #:cl-telegram-bot2/states/wait-for-payment
                #:wait-for-payment)
  (:import-from #:cl-telegram-bot2/state-with-commands
                #:command)
  (:export #:send-invoice
           #:title
           #:description
           #:payload
           #:provider-token
           #:currency
           #:prices
           #:on-success
           #:commands
           #:prices-list))
(in-package #:cl-telegram-bot2/actions/send-invoice)


(deftype prices-list ()
  "Type of PRICES arguments for SEND-INVOICE class."
  '(soft-list-of hash-table))


(defclass send-invoice (action)
  ((title :initarg :title
          :type (or string
                    symbol)
          :reader title)
   (description :initarg :description
                :type (or string
                          symbol)
                :reader description)
   (payload :initarg :payload
            :type (or string
                      symbol)
            :reader payload)
   (provider-token :initarg :provider-token
                   :type (or string
                             symbol)
                   :reader provider-token)
   (currency :initarg :currency
             :type (or string
                       symbol)
             :reader currency)
   (prices :initarg :prices
           :type (or prices-list
                     symbol)
           :reader prices)
   (on-success :initarg :on-success
               :type (or workflow-block
                         workflow-blocks
                         symbol)
               :reader on-success)
   (commands :initarg :commands
             :initform nil
             :type (soft-list-of command)
             :reader commands)))


(-> send-invoice ((or string symbol)
                  (or string symbol)
                  (or string symbol)
                  (or string symbol)
                  (or string symbol)
                  (or prices-list symbol)
                  &key
                  (:on-success (or workflow-block
                                   workflow-blocks
                                   symbol))
                  (:commands (soft-list-of command)))
    (values send-invoice &optional))

(defun send-invoice (title description payload provider-token currency prices &key on-success commands)
  (make-instance 'send-invoice
                 :title title
                 :description description
                 :payload payload
                 :provider-token provider-token
                 :currency currency
                 :prices prices
                 :on-success on-success
                 :commands commands))


(defmethod print-object ((obj send-invoice) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S"
            (title obj))))


(-> perform-action (send-invoice)
    (values wait-for-payment &optional))

(defun perform-action (action)
  (cl-telegram-bot2/api::send-invoice
   (cl-telegram-bot2/api::chat-id cl-telegram-bot2/vars::*current-chat*)
   ;; title
   (call-if-needed
    (title action))
   ;; description
   (call-if-needed
    (description action))
   ;; payload
   (call-if-needed
    (payload action))
   ;; currency
   (call-if-needed
    (currency action))
   ;; prices
   (call-if-needed
    (prices action))
   
   :provider-token (call-if-needed
                    (provider-token action)))

  (wait-for-payment :on-success (on-success action)
                    :commands (commands action)))


(defmethod on-state-activation ((action send-invoice))
  (perform-action action))


(defmethod process-state ((bot t) (action send-invoice) update)
  (perform-action action))


(defmethod on-result ((action send-invoice) result)
  (perform-action action))
