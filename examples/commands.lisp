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
                #:message-message-id)
  (:import-from #:cl-telegram-bot2/state-with-commands
                #:global-command
                #:command)
  (:import-from #:cl-telegram-bot2/generics
                #:on-result
                #:on-state-activation
                #:process)
  (:import-from #:cl-telegram-bot2/state
                #:state)
  (:import-from #:cl-telegram-bot2/term/back
                #:back-to-id)
  (:import-from #:cl-telegram-bot2/actions/send-text
                #:send-text)
  (:import-from #:str
                #:trim)
  (:import-from #:cl-telegram-bot2/actions/delete-messages
                #:delete-messages))
(in-package #:cl-telegram-bot2-examples/commands)


(defun on-help-command (arg update)
  (declare (ignore arg update))
  (reply "This bot has two states.

At the initial state only two commands are available:

/next - switches bot into the second state.
/help - shows this text.

The second state changes /next command to the /back and provides
additional command /reverse, which will reverse any given text.")
  ;; It is important to return nothing if we want switch
  ;; bot to a new state from this handler
  (values))


(defun on-reverse-command (arg update)
  (declare (ignore update))
  (let ((trimmed (trim arg)))
    (cond
      ((or (null trimmed)
           (string= trimmed ""))
       (reply "This command requires an argument."))
      (t
       (reply (reverse arg)))))
  ;; It is important to return nothing if we want switch
  ;; bot to a new state from this handler
  (values))


(defbot test-bot ()
  ()
  (:initial-state
   (state (send-text "Initial state. Give /next command to go to the second state.")
          :id "initial"
          :on-result (send-text "Welcome back! Give /next command to go to the second state.")
          :on-update (send-text "Give /next command to go to the second state.")
          :on-deletion (delete-messages)
          :commands (list
                     (command "/next"
                              (state (send-text "Second state. Give /reverse command with an argument to return it in a reversed way.

Or do /back command to go to the initial state.

Note how commands list is changed depending on current bot's state.")
                                     :on-update (send-text "Give /back command to go to the initial state.")
                                     :on-deletion (delete-messages)
                                     :commands (list
                                                (command "/back" (back-to-id "initial")
                                                         :description "Switch to the prev state")
                                                (command "/reverse" 'on-reverse-command
                                                         :description "Switch to the prev state")))
                              :description "Switch to the next state")
                     (global-command "/help" 'on-help-command
                                     :description "Show information about bot's commands.")))))


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
        do (bt:destroy-thread tr)))
