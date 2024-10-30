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
                #:command
                #:state-with-commands-mixin)
  (:import-from #:cl-telegram-bot2/generics
                #:on-result
                #:on-state-activation
                #:process))
(in-package #:cl-telegram-bot2-examples/commands)


(defclass initial-state (state-with-commands-mixin)
  ()
  (:default-initargs
   :commands (list
              (command "/next" (make-instance 'second-state)
                       :description "Switch to the next state")
              (command "/ver7" 'on-ver-command))))


(defclass second-state (state-with-commands-mixin)
  ()
  (:default-initargs
   :commands (list
              (command "/back" (back-to 'initial-state)
                       :description "Switch to the prev state")
              (command "/reverse" (lambda (arg update)
                                    (declare (ignore update))
                                    (reply
                                     (if arg
                                         (reverse arg)
                                         "This command requires an argument."))
                                    ;; It is important to return nothing here to
                                    ;; stay on the same state.
                                    (values))
                       :description "Switch to the prev state")
              (command "/ver7" 'on-ver-command))))


(defmethod on-state-activation ((state initial-state))
  (reply "Initial state. Give /next command to go to the second state.")
  (values))


(defmethod on-result ((state initial-state) result)
  (reply "Welcom back! Give /next command to go to the second state.")
  (values))


(defmethod process   ((state initial-state) update)
  (reply "Give /next command to go to the second state.")
  (values))


(defmethod on-state-activation ((state second-state))
  (reply "Second state. Give /back command to go to the initial state.")
  (values))


(defmethod process ((state second-state) update)
  (reply "Give /back command to go to the initial state.")
  (values))


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
