(uiop:define-package #:cl-telegram-bot2/sent-messages
  (:use #:cl)
  (:import-from #:serapeum
                #:fmt
                #:->
                #:pretty-print-hash-table
                #:dict
                #:soft-list-of)
  (:import-from #:cl-telegram-bot2/generics
                #:on-result
                #:on-state-activation
                #:process-state)
  (:import-from #:cl-telegram-bot2/api
                #:update
                #:update-message
                #:message-message-id)
  (:import-from #:cl-telegram-bot2/high
                #:collect-sent-messages)
  (:import-from #:cl-telegram-bot2/debug/diagram/utils
                #:obj-id
                #:render-mapslot-value-with-link)
  (:import-from #:cl-telegram-bot2/bot
                #:bot)
  (:import-from #:cl-telegram-bot2/states/base
                #:state-id
                #:sent-message-ids
                #:received-message-ids
                #:base-state)
  (:export #:save-received-message-id
           #:capture-sent-messages))
(in-package #:cl-telegram-bot2/sent-messages)


(defmacro capture-sent-messages ((state-var) &body body)
  "Use this macro to capture messages end during PROCESS-STATE generic-function handling
   in case if your state inherits from BASE-STATE but does not call CALL-NEXT-METHOD."
  `(multiple-value-bind (sent-messages result)
       (collect-sent-messages
         ,@body)
    
     (loop for message in sent-messages
           do (push (message-message-id message)
                    (sent-message-ids ,state-var)))
     (values result)))


(-> save-received-message-id (base-state update)
    (values &optional))

(defun save-received-message-id (state update)
  "If some state class processes update and don't call CALL-NEXT-METHOD,
   then it have to call this function to register received message id.

   If you don't do this, then received messages deletion will not work
   for this state."
  (let ((message (update-message update)))
    (when message
      (push (message-message-id message)
            (received-message-ids state))))
  (values))


(defmethod process-state :around ((bot bot) (state base-state) (update t))
  (save-received-message-id state update)
  
  (capture-sent-messages (state)
    (call-next-method)))


(defmethod on-state-activation :around ((state base-state))
  (capture-sent-messages (state)
    (call-next-method)))


(defmethod on-result :around ((state base-state) result)
  (capture-sent-messages (state)
    (call-next-method)))
