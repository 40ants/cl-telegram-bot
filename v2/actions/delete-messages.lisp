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
                #:sent-message-ids)
  (:export #:delete-messages))
(in-package #:cl-telegram-bot2/actions/delete-messages)


(defclass delete-messages (action)
  ()
  (:documentation "Delete all messages created in the current current state."))


(-> delete-messages ()
    (values delete-messages &optional))


(defun delete-messages ()
  (make-instance 'delete-messages))


(defun delete-created-messages ()
  (when (sent-message-ids *current-state*)
    (cl-telegram-bot2/api:delete-messages (chat-id *current-chat*)
                                          (sent-message-ids *current-state*))
    (setf (sent-message-ids *current-state*)
          nil))
  (values))


(defmethod on-state-activation ((action delete-messages))
  (delete-created-messages)
  (values))


(defmethod process ((action delete-messages) update)
  (delete-created-messages)
  (values))


(defmethod on-result ((action delete-messages) result)
  (delete-created-messages)
  (values))
