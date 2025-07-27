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
                #:process
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/high
                #:reply)
  (:import-from #:serapeum
                #:soft-list-of
                #:->)
  (:import-from #:cl-telegram-bot2/utils
                #:call-if-needed)
  (:import-from #:cl-telegram-bot2/states/base
                #:received-message-ids
                #:sent-message-ids)
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
                 (received-message-ids state)))))
    
    (log:debug "Deleting messages created in" state)
    
    (when ids
      (log:debug "These messages will be deleted" ids)
      
      (cl-telegram-bot2/api:delete-messages (chat-id *current-chat*)
                                            ids)
      (setf (sent-message-ids *current-state*)
            nil))
    (values)))


(defmethod on-state-activation ((action delete-messages))
  (delete-created-messages action)
  (values))


(defmethod process ((bot t) (action delete-messages) update)
  (delete-created-messages action)
  (values))


(defmethod on-result ((action delete-messages) result)
  (delete-created-messages action)
  (values))


(defmethod on-state-deletion ((action delete-messages))
  (delete-created-messages action)
  (values))
