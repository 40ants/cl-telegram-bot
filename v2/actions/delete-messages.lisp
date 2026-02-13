(uiop:define-package #:cl-telegram-bot2/actions/delete-messages
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/action
                #:action)
  (:import-from #:cl-telegram-bot2/vars
                #:*current-chat*
                #:*current-state*)
  (:import-from #:cl-telegram-bot2/api
                #:message-chat
                #:update-message
                #:update
                #:chat-id
                #:send-message)
  (:import-from #:cl-telegram-bot2/generics
                #:on-state-deletion
                #:on-result
                #:process-state
                #:on-state-activation)
  (:import-from #:log)
  (:import-from #:serapeum
                #:soft-list-of
                #:->)
  (:import-from #:cl-telegram-bot2/utils
                #:call-if-needed)
  (:import-from #:cl-telegram-bot2/states/base
                #:received-message-ids
                #:sent-message-ids)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:cl-telegram-bot2/errors
                #:error-description
                #:telegram-error)
  (:import-from #:log4cl-extras/context
                #:with-fields)
  (:export #:delete-messages
           #:delete-sent-messages-p
           #:delete-received-messages-p))
(in-package #:cl-telegram-bot2/actions/delete-messages)


(defclass delete-messages (action)
  ((delete-sent-messages :initarg :sent
                         :type boolean
                         :initform t
                         :reader delete-sent-messages-p)
   (delete-received-messages :initarg :received
                             :type boolean
                             :initform t
                             :reader delete-received-messages-p))
  (:documentation "Delete all messages created in the current current state."))


(-> delete-messages (&key
                     (:sent boolean)
                     (:received boolean))
    (values delete-messages &optional))


(defun delete-messages (&key
                        (sent t)
                        (received t))
  (make-instance 'delete-messages
                 :sent sent
                 :received received))


(-> delete-created-messages (delete-messages)
    (values &optional))

(defun delete-created-messages (action)
  (let* ((state *current-state*)
         (ids (append
               (when (delete-sent-messages-p action)
                 (sent-message-ids state))
               (when (delete-received-messages-p action)
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


(defmethod on-state-activation ((action delete-messages))
  (delete-created-messages action)
  (values))


(defmethod process-state ((bot t) (action delete-messages) update)
  (delete-created-messages action)
  (values))


(defmethod on-result ((action delete-messages) result)
  (delete-created-messages action)
  (values))


(defmethod on-state-deletion ((action delete-messages))
  (delete-created-messages action)
  (values))
