(uiop:define-package #:cl-telegram-bot2/states/wait-for-payment
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/generics
                #:process-state
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/high
                #:reply)
  (:import-from #:cl-telegram-bot2/states/base
                #:base-state)
  (:import-from #:cl-telegram-bot2/api
                #:pre-checkout-query
                #:pre-checkout-query-id
                #:answer-pre-checkout-query
                #:message-text
                #:update-message)
  (:import-from #:cl-telegram-bot2/action
                #:call-if-action
                #:action)
  (:import-from #:cl-telegram-bot2/workflow
                #:funcallable-symbol
                #:workflow-blocks)
  (:import-from #:cl-telegram-bot2/utils
                #:call-if-needed)
  (:import-from #:cl-telegram-bot2/state-with-commands
                #:state-with-commands-mixin)
  (:import-from #:alexandria
                #:curry)
  (:export #:wait-for-payment
           #:on-success))
(in-package #:cl-telegram-bot2/states/wait-for-payment)


(defclass wait-for-payment (state-with-commands-mixin base-state)
  ((on-success :initarg :on-success
               :initform nil
               :type (or symbol
                         workflow-blocks)
               :reader on-success
               :documentation "On success could be an fbound symbol which function returns a list of workflow blocks or a list of workflow blocks.")))


(defun wait-for-payment (&key on-success commands)
  (make-instance 'wait-for-payment
                 :on-success (typecase on-success
                               (symbol on-success)
                               (t
                                (uiop:ensure-list on-success)))
                 :commands commands))


(defmethod process-state ((bot t) (state wait-for-payment) update)
  (let* ((message
           (cl-telegram-bot2/api:update-message
            update))
         (successful-payment
           ;; Sometimes user might click a button again and update will have no
           ;; a message at all, only callback-query.
           (when message
             (cl-telegram-bot2/api:message-successful-payment message))))

    (cond
      (successful-payment
       (cond
         ((on-success state)
          (let ((action-or-state
                  (call-if-needed (on-success state)
                                  successful-payment)))
            (call-if-action action-or-state
                            (curry #'process-state bot)
                            update)))
         (t
          (error "There is no ON-SUCCESS handler for ~S state."
                 (type-of state)))))
      (t
       ;; TODO: Probably we should show a Back button if user just enters a text
       ;; or does some callback calls while we are waiting for the payment?
       (cl-telegram-bot2/high:reply "We are still waiting for the payment.")))))


;; (defmethod cl-telegram-bot2/generics:on-pre-checkout-query ((state wait-for-payment) (query pre-checkout-query))
;;   (answer-pre-checkout-query (pre-checkout-query-id query)
;;                              (aif (on-pre-checkout-query state)
;;                                   (funcall it query)
;;                                   t)))
