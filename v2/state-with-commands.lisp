(uiop:define-package #:cl-telegram-bot2/state-with-commands
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/generics
                #:on-result
                #:process
                #:on-state-activation)
  (:import-from #:alexandria
                #:required-argument)
  (:import-from #:serapeum
                #:length<
                #:soft-list-of)
  (:import-from #:cl-telegram-bot2/api
                #:message-entity-length
                #:message-entity-offset
                #:user-id
                #:chat-id
                #:bot-command-scope-chat-member
                #:bot-command
                #:set-my-commands)
  (:import-from #:cl-telegram-bot2/vars
                #:*current-chat*
                #:*current-user*)
  (:export #:state-with-commands-mixin
           #:state-commands
           #:command))
(in-package #:cl-telegram-bot2/state-with-commands)


(defclass command ()
  ((name :initarg :name
         :initform (required-argument ":COMMAND is required argument.")
         :type string
         :documentation "A command name like \"/start\" or \"/help\"."
         :reader command-name)
   (handler :initarg :handler
            :initform (required-argument ":HANDLER is required argument.")
            :documentation "A callable object of one argument or an object to return from PROCESS generic-function."
            :reader command-handler)
   (description :initarg :description
                :initform nil
                :type (or null string)
                :documentation "A command description like \"/start\" or \"/help\"."
                :reader command-description)))

(defun command (name handler &key description)
  (make-instance 'command
                 :name name
                 :handler handler
                 :description description))


(defclass state-with-commands-mixin ()
  ((commands :initarg :commands
             :initform nil
             :type (soft-list-of command)
             :reader state-commands)))


(defun send-commands (state)
  (set-my-commands
   (loop for command in (state-commands state)
         collect (make-instance 'bot-command
                                :command (command-name command)
                                :description (or (command-description command)
                                                 (command-name command))))
   :scope (make-instance 'bot-command-scope-chat-member
                         :type "chat"
                         :chat-id (chat-id *current-chat*)
                         :user-id (user-id *current-user*))))


(defmethod on-state-activation :before ((state state-with-commands-mixin))
  (send-commands state)
  (values))


(defmethod on-result :before ((state state-with-commands-mixin) result)
  (send-commands state)
  (values))


(defun extract-command-name (update)
  (let* ((message (cl-telegram-bot2/api:update-message update)))
    (when message
      (loop with text = (cl-telegram-bot2/api:message-text message)
            for entity in (cl-telegram-bot2/api:message-entities message)
            for entity-type = (cl-telegram-bot2/api:message-entity-type entity)
            when (string= entity-type
                          "bot_command")
            do (return
                 (values
                  (subseq text
                          (message-entity-offset entity)
                          (+ (message-entity-offset entity)
                             (message-entity-length entity)))
                  (when (length<
                         (message-entity-length entity)
                         text)
                      (subseq text
                              (+ (message-entity-offset entity)
                                 (message-entity-length entity)
                                 1)))))))))


(defmethod process :around ((state state-with-commands-mixin) update)
  (multiple-value-bind (command-name rest-text)
      (extract-command-name update)
    (cond
      (command-name
       (loop for command in (state-commands state)
             when (string-equal command-name
                                (command-name command))
             do (return 
                  (let ((handler (command-handler command)))
                    (typecase handler
                      ((or symbol function)
                       (funcall handler rest-text update))
                      (t
                       handler))))
             finally (return
                       (call-next-method))))
      (t
       (call-next-method)))))
