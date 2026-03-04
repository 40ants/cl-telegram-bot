(uiop:define-package #:cl-telegram-bot2/sent-messages
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/errors
                #:error-description
                #:telegram-error)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:log4cl-extras/context
                #:with-fields)
  (:import-from #:cl-telegram-bot2/vars
                #:*current-chat*
                #:*current-state*)
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
                #:chat-id
                #:update
                #:update-message
                #:message
                #:message-message-id)
  (:import-from #:cl-telegram-bot2/high
                #:collect-sent-messages)
  (:import-from #:cl-telegram-bot2/bot
                #:bot)
  (:import-from #:cl-telegram-bot2/states/base
                #:state-id
                #:sent-message-ids
                #:received-message-ids
                #:base-state)
  (:export #:save-received-message-id
           #:capture-sent-messages
           #:save-sent-message-id
           #:delete-messages))
(in-package #:cl-telegram-bot2/sent-messages)


(-> save-sent-message-id (base-state message)
    (values &optional))


(defun save-sent-message-id (state message)
  "Usually all sent messages are captured automatically during update processing.

   However, when messages are sent and deleted in a one workflow-blocks list, delete-messages
   action will not see these sent messages. Thus we have to call this function explicitly
   inside send-text action."
  (pushnew (message-message-id message)
           (sent-message-ids state)
           :test #'equal)
  (values))


(defmacro capture-sent-messages ((state-var) &body body)
  "Use this macro to capture messages end during PROCESS-STATE generic-function handling
   in case if your state inherits from BASE-STATE but does not call CALL-NEXT-METHOD."
  `(multiple-value-bind (sent-messages result)
       (collect-sent-messages
         ,@body)
    
     (loop for message in sent-messages
           do (when message
                (save-sent-message-id ,state-var message)))
     
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
      (pushnew (message-message-id message)
               (received-message-ids state)
               :test #'equal)))
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


(-> delete-messages (&key (:sent boolean) (:received boolean))
    (values &optional))

(defun delete-messages (&key (sent t) (received t))
  "Deletes messages of the current state."
  (let* ((state *current-state*)
         (ids (append
               (when sent
                 (sent-message-ids state))
               (when received
                 (received-message-ids state))))
         (chat-id (chat-id *current-chat*)))
    
    (log:debug "Deleting messages created in" state)
    
    (when ids
      (with-fields (:chat-id chat-id
                    :message-ids ids)
        (log:debug "These messages will be deleted" ids)
      
        (handler-case
            (with-log-unhandled ()
              (cl-telegram-bot2/api:delete-messages chat-id
                                                    ids))
          (telegram-error (e)
            (let ((desc (error-description e)))
              (when (str:containsp "message can't be deleted for everyone"
                                   desc)
                ;; Sometimes this response can be sent because user already deleted messages
                ;; probably by cleaning chat history. Just for the case, we will try to
                ;; again to delete messages one by one, maybe some of the can be deleted:
                (loop for id in ids
                      do (with-fields (:message-id id)
                           (handler-case
                               (with-log-unhandled ()
                                 (cl-telegram-bot2/api:delete-messages (chat-id *current-chat*)
                                                                       (list id)))
                             (telegram-error ()
                               ;; Just ignore subsequent errors (hovever they will be logged)
                               nil))))))))
      
        (setf (sent-message-ids *current-state*)
              nil)))
    (values)))
