(uiop:define-package #:cl-telegram-bot2/states/wait-for-payment
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/generics
                #:process
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/state
                #:base-state)
  (:import-from #:cl-telegram-bot2/states/base
                #:state-var)
  (:import-from #:cl-telegram-bot2/pipeline
                #:back)
  (:import-from #:cl-telegram-bot2/high
                #:reply)
  (:import-from #:cl-telegram-bot2/api
                #:pre-checkout-query
                #:pre-checkout-query-id
                #:answer-pre-checkout-query
                #:message-text
                #:update-message)
  (:import-from #:str
                #:trim)
  (:import-from #:serapeum
                #:soft-list-of)
  (:import-from #:cl-telegram-bot2/action
                #:action)
  (:import-from #:cl-telegram-bot2/workflow
                #:funcallable-symbol
                #:workflow-blocks)
  (:import-from #:cl-telegram-bot2/utils
                #:call-if-needed)
  (:import-from #:anaphora
                #:it
                #:aif)
  (:import-from #:cl-telegram-bot2/state-with-commands
                #:state-with-commands-mixin)
  (:export #:wait-for-payment))
(in-package #:cl-telegram-bot2/states/wait-for-payment)


(defclass wait-for-payment (state-with-commands-mixin base-state)
  ((on-success :initarg :on-success
               :initform nil
               :type workflow-blocks
               :reader on-success)))


(defun wait-for-payment (&key on-success commands)
  (make-instance 'wait-for-payment
                 :on-success (uiop:ensure-list
                              on-success)
                 :commands commands))


(defmethod process ((state wait-for-payment) update)
  (let* ((message
           (cl-telegram-bot2/api:update-message
            update))
         (successful-payment
           (cl-telegram-bot2/api:message-successful-payment message)))
    (when successful-payment
      (cond
        ((on-success state)
         (cl-telegram-bot2/action:call-if-action
          (call-if-needed (on-success state)
                          successful-payment)
          #'process
          update))
        (t
         (error "There is no ON-SUCCESS handler for ~S state."
                (type-of state)))))))


;; (defmethod cl-telegram-bot2/generics:on-pre-checkout-query ((state wait-for-payment) (query pre-checkout-query))
;;   (answer-pre-checkout-query (pre-checkout-query-id query)
;;                              (aif (on-pre-checkout-query state)
;;                                   (funcall it query)
;;                                   t)))
