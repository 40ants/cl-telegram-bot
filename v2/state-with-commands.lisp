(uiop:define-package #:cl-telegram-bot2/state-with-commands
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/generics
                #:on-result
                #:process
                #:on-state-activation)
  (:import-from #:alexandria
                #:flatten
                #:length=
                #:required-argument)
  (:import-from #:serapeum
                #:->
                #:length<
                #:soft-list-of)
  (:import-from #:cl-telegram-bot2/api
                #:chat-type
                #:bot-command-scope-chat
                #:message-entity-length
                #:message-entity-offset
                #:user-id
                #:chat-id
                #:bot-command-scope-chat-member
                #:bot-command
                #:set-my-commands)
  (:import-from #:cl-telegram-bot2/vars
                #:*current-bot*
                #:*current-chat*
                #:*current-user*)
  (:import-from #:cl-telegram-bot2/workflow
                #:workflow-blocks
                #:workflow-block)
  (:import-from #:sento.actor-cell
                #:*state*)
  (:import-from #:cl-telegram-bot2/bot
                #:bot-name)
  (:import-from #:cl-telegram-bot2/action
                #:call-if-action)
  (:import-from #:cl-telegram-bot2/states/base
                #:capture-sent-messages
                #:save-received-message-id)
  (:import-from #:cl-telegram-bot2/debug/diagram/generics
                #:to-text
                #:slot-name
                #:render-handlers)
  (:import-from #:cl-telegram-bot2/debug/diagram/utils
                #:obj-id
                #:render-handlers-inner)
  (:export #:state-with-commands-mixin
           #:state-commands
           #:command
           #:global-command
           #:command-name
           #:command-handler
           #:command-description
           #:base-command))
(in-package #:cl-telegram-bot2/state-with-commands)


(deftype command-handler ()
  '(or
    symbol
    workflow-block
    workflow-blocks))


(defclass base-command ()
  ((name :initarg :name
         :initform (required-argument ":COMMAND is required argument.")
         :type string
         :documentation "A command name like \"/start\" or \"/help\"."
         :reader command-name)
   (handler :initarg :handler
            :initform (required-argument ":HANDLER is required argument.")
            ;; TODO: make it a list of workflow-blocks and rename to the command-handlers
            :type command-handler
            :documentation "An fbound symbol of two arguments (command-argument update-obj) or a workflow object to return from PROCESS generic-function."
            :reader command-handler)
   (description :initarg :description
                :initform nil
                :type (or null string)
                :documentation "A command description like \"/start\" or \"/help\"."
                :reader command-description)))


(defmethod print-object ((command base-command) stream)
  (print-unreadable-object (command stream :type t)
    (format stream "~A"
            (command-name command))))


(defclass command (base-command)
  ()
  (:documentation "This type of command is available only in the state where it is defined."))


(defclass global-command (command)
  ()
  (:documentation "This command will be available during in all bot states."))


(-> command (string command-handler
             &key (:description (or null string)))
    (values command &optional))

(defun command (name handler &key description)
  (make-instance 'command
                 :name name
                 :handler handler
                 :description description))

(-> global-command (string command-handler
             &key (:description (or null string)))
    (values global-command &optional))

(defun global-command (name handler &key description)
  (make-instance 'global-command
                 :name name
                 :handler handler
                 :description description))


(defclass state-with-commands-mixin ()
  ((commands :initarg :commands
             :initform nil
             :type (soft-list-of command)
             :reader state-commands)))


(defun supports-commands-p (state)
  (typep state 'state-with-commands-mixin))


(defun state-global-commands (state)
  (loop for command in (state-commands state)
        when (typep command 'global-command)
          collect command))


(defun send-commands (state)
  (let ((chat *current-chat*)
        (user *current-user*))
    (when (and chat
               ;; Telegram does not allow to set commands for the channel.
               ;; It returns error: can't change commands in channel chats
               (not (string= (chat-type chat)
                             "channel")))
      
      (loop for command in (append (state-commands state)
                                   (flatten (mapcar #'state-global-commands
                                                    (remove-if-not #'supports-commands-p
                                                                   (rest *state*)))))
            for bot-command = (make-instance 'bot-command
                                             :command (command-name command)
                                             :description (or (command-description command)
                                                              (command-name command)))
            collect bot-command into bot-commands
            finally
               (when bot-commands
                 (set-my-commands bot-commands
                                  :scope (cond
                                           ((and chat
                                                 (string-equal
                                                  (chat-type chat)
                                                  "private"))
                                            (make-instance 'bot-command-scope-chat
                                                           :type "chat"
                                                           :chat-id (chat-id chat)))
                                           ((and chat
                                                 user)
                                            (make-instance 'bot-command-scope-chat-member
                                                           :type "chat_member"
                                                           :chat-id (chat-id chat)
                                                           :user-id (user-id user))))))))))


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
                 (let* ((full-command-name
                          (subseq text
                                  (message-entity-offset entity)
                                  (+ (message-entity-offset entity)
                                     (message-entity-length entity))))
                        (rest-text
                          (when (length<
                                 (message-entity-length entity)
                                 text)
                            (subseq text
                                    (+ (message-entity-offset entity)
                                       (message-entity-length entity)
                                       1))))
                        (splitted (str:split #\@ full-command-name
                                             :limit 2))
                        (command-name (first splitted))
                        (bot-name (when (length= 2 splitted)
                                    (second splitted))))
                 
                   (values
                    command-name
                    bot-name
                    rest-text)))))))


(defmethod process :around ((state state-with-commands-mixin) update)
  (multiple-value-bind (command-name bot-name rest-text)
      (extract-command-name update)
    (cond
      ((and command-name
            (or (null bot-name)
                (string-equal (bot-name)
                              bot-name)))

       ;; Command /start is special. If we will delete message with this
       ;; command and chat become empty, Telegram client will send another
       ;; /start message automaticall, and this could look strange, because
       ;; before this bot can send a message to and /start message in the chat
       ;; will go as a second message. Thus it is better to leave start message
       ;; in the chat :(
       (unless (string-equal command-name
                             "/start")
         (save-received-message-id state update))
       
       (capture-sent-messages (state)
         (loop for command in (append (state-commands state)
                                      ;; Here we need to search previos states
                                      ;; on a stack to support their global
                                      ;; commands in the current state:
                                      (flatten (mapcar #'state-global-commands
                                                       (rest *state*))))
               when (string-equal command-name
                                  (command-name command))
                 do (return 
                      (let ((handler (command-handler command)))
                        ;; Handler might return an action or a state
                        ;; in first case we have to apply PROCESS to an action
                        (call-if-action
                         (typecase handler
                           ((or symbol function)
                              (funcall handler rest-text update))
                           (t
                              handler))
                         #'process
                         update)))
               finally (log:warn "Command ~A cant be processed by state ~S"
                                 command-name
                                 (class-name
                                  (class-of state))))))
      (t
       (call-next-method)))))


(defmethod render-handlers ((obj command))
  (render-handlers-inner (uiop:ensure-list (command-handler obj))
                         (obj-id obj)))


(defmethod slot-name ((obj command))
  (command-name obj))


(defmethod to-text ((command command))
  (to-text
   (uiop:ensure-list
    (command-handler command))))
