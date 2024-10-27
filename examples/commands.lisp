(uiop:define-package #:cl-telegram-bot2-examples/commands
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/bot
                #:defbot)
  (:import-from #:cl-telegram-bot2/server
                #:stop-polling
                #:start-polling)
  (:import-from #:cl-telegram-bot2/high
                #:reply
                #:chat-state)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:cl-telegram-bot2/pipeline
                #:back-to
                #:back)
  (:import-from #:cl-telegram-bot2/api
                #:message-message-id))
(in-package #:cl-telegram-bot2-examples/commands)


(defclass initial-state ()
  ())


(defclass second-state ()
  ())


;; CL-TELEGRAM-BOT2-EXAMPLES/COMMANDS> (cl-telegram-bot2/api:message-entities #v62)
;; (#<CL-TELEGRAM-BOT2/API:MESSAGE-ENTITY  TYPE=bot_command OFFSET=0 LENGTH=5>)


(defmethod cl-telegram-bot2/generics:on-state-activation ((state initial-state))
  (reply "Initial state. Give /next command to go to the second state.")
  (cl-telegram-bot2/api:set-my-commands
   (list (make-instance 'cl-telegram-bot2/api:bot-command
                        :command "/next"
                        :description "Switch to the next state"))
   :scope (make-instance 'cl-telegram-bot2/api:bot-command-scope-chat
                         :type "chat"
                         :chat-id (cl-telegram-bot2/api:chat-id cl-telegram-bot2/vars::*current-chat*)))
  (values))


(defmethod cl-telegram-bot2/generics:on-result ((state initial-state) result)
  (reply "Initial state. Give /next command to go to the second state.")
  (cl-telegram-bot2/api:set-my-commands
   (list (make-instance 'cl-telegram-bot2/api:bot-command
                        :command "/next"
                        :description "Switch to the next state"))
   :scope (make-instance 'cl-telegram-bot2/api:bot-command-scope-chat
                         :type "chat"
                         :chat-id (cl-telegram-bot2/api:chat-id cl-telegram-bot2/vars::*current-chat*)))
  (values))


(defmethod cl-telegram-bot2/generics:process ((state initial-state) update)

  (let* ((message (cl-telegram-bot2/api:update-message update))
         (text (when message
                 (cl-telegram-bot2/api:message-text message))))
    (cond
      ((string-equal text "/next")
       (make-instance 'second-state))
      (t
       (reply "Give /next command to go to the second state.")
       (values)))))


(defmethod cl-telegram-bot2/generics:on-state-activation ((state second-state))
  (reply "Second state. Give /back command to go to the initial state.")
  
  (cl-telegram-bot2/api:set-my-commands
   (list (make-instance 'cl-telegram-bot2/api:bot-command
                        :command "/back"
                        :description "Switch back to the initial state"))
   :scope (make-instance 'cl-telegram-bot2/api:bot-command-scope-chat
                         :type "chat"
                         :chat-id (cl-telegram-bot2/api:chat-id cl-telegram-bot2/vars::*current-chat*)))
  
  (values))


(defmethod cl-telegram-bot2/generics:process ((state second-state) update)
  (let* ((message (cl-telegram-bot2/api:update-message update))
         (text (when message
                 (cl-telegram-bot2/api:message-text message))))
    (cond
      ((string-equal text "/back")
       (back-to 'initial-state))
      (t
       (reply "Give /back command to go to the initial state.")
       (values)))))


(defbot test-bot ()
  ()
  (:initial-state 'initial-state))


(defvar *bot* nil)


(defun stop ()
  (when *bot*
    (stop-polling *bot*)
    (setf *bot* nil)))


(defun start ()
  (stop)

  (unless *bot*
    (setf *bot*
          (make-test-bot (uiop:getenv "TELEGRAM_TOKEN"))))
  
  (start-polling *bot* :debug t))


(defun clean-threads ()
  "TODO: надо разобраться почему треды не подчищаются. Возможно это происходит когда случаются ошибки?"
  (loop for tr in (bt:all-threads)
        when (or (str:starts-with? "message-thread" (bt:thread-name tr))
                 (str:starts-with? "timer-wheel" (bt:thread-name tr))
                 (str:starts-with? "telegram-bot" (bt:thread-name tr)))
        do (bt:destroy-thread tr))
  )
