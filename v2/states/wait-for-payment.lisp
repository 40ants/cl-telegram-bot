(uiop:define-package #:cl-telegram-bot2/states/wait-for-payment
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/generics
                #:process-state
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/workflow
                #:workflow-blocks
                #:workflow-block)
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
                #:state-with-commands-mixin
                #:command)
  (:import-from #:alexandria
                #:curry)
  (:import-from #:serapeum
                #:soft-list-of
                #:->)
  (:export #:wait-for-payment
           #:on-success))
(in-package #:cl-telegram-bot2/states/wait-for-payment)


(defclass wait-for-payment (state-with-commands-mixin base-state)
  ((on-success :initarg :on-success
               :initform nil
               :type (or symbol
                         workflow-blocks)
               :reader on-success
               :documentation "On success could be an fbound symbol which function returns a list of workflow blocks or a list of workflow blocks.")
   (on-cancel :initarg :on-cancel
              :type (or workflow-block
                        workflow-blocks
                        symbol)
              :reader on-cancel)))


(-> wait-for-payment (&key
                      (:on-success (or workflow-block
                                       workflow-blocks
                                       symbol))
                      (:on-cancel (or workflow-block
                                      workflow-blocks
                                      symbol)))
    (values wait-for-payment &optional))

(defun wait-for-payment (&key on-success on-cancel)
  (make-instance 'wait-for-payment
                 :on-success (typecase on-success
                               (symbol on-success)
                               (t
                                  (uiop:ensure-list on-success)))
                 :on-cancel (typecase on-cancel
                              (symbol on-cancel)
                              (t
                                 (uiop:ensure-list on-cancel)))))


(defmethod process-state ((bot t) (state wait-for-payment) update)
  (let* ((message
           (cl-telegram-bot2/api:update-message
            update))
         (successful-payment
           ;; Sometimes user might click a button again and update will have no
           ;; a message at all, only callback-query.
           (when message
             (cl-telegram-bot2/api:message-successful-payment message)))
         (callback-data
           ;; Sometimes user might click a button again and update will have no
           ;; a message at all, only callback-query.
           (cl-telegram-bot2/high/callbacks:get-callback-data update)))

    (cond
      ((and callback-data
            (string= callback-data
                     "cancel"))
       (log:info "Cancelling payment")
       
       (cond
         ((on-cancel state)
          (let* ((action-or-state
                   (call-if-needed (on-cancel state)))
                 (result (call-if-action action-or-state
                                         (curry #'process-state bot)
                                         update)))
            (log:debug "Returning result of on-cancel" result)
            (values result)))
         (t
          (error "There is no ON-CANCEL handler for ~S state."
                 (type-of state)))))
      (successful-payment
       (log:info "Successful payment")
       
       (cond
         ((on-success state)
          (let* ((action-or-state
                   (call-if-needed (on-success state)
                                   successful-payment))
                 (result (call-if-action action-or-state
                                         (curry #'process-state bot)
                                         update)))
            (log:debug "Returning result of on-success" result)
            (values result)))
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
