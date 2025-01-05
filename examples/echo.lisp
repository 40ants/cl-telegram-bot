(uiop:define-package #:cl-telegram-bot2-examples/echo
  (:use #:cl)
  (:import-from #:bordeaux-threads)
  (:import-from #:cl-telegram-bot2/state
                #:state)
  (:import-from #:cl-telegram-bot2/actions/send-text
                #:send-text)
  (:import-from #:cl-telegram-bot2/bot
                #:defbot)
  (:import-from #:cl-telegram-bot2/server
                #:stop-polling
                #:start-polling)
  (:import-from #:cl-telegram-bot2/high
                #:reply
                #:chat-state)
  (:import-from #:cl-telegram-bot2/api
                #:update-message
                #:message-text
                #:message-message-id)
  (:import-from #:40ants-logging))
(in-package #:cl-telegram-bot2-examples/echo)


(defun reply-with-same-text (update)
  (reply (message-text
          (update-message update)))
  (values))


(defbot test-bot ()
  ()
  (:initial-state
   (state (send-text "Hello, I'm the echo bot.")
          :id "echo-example"
          :on-update 'reply-with-same-text)))


(defvar *bot* nil)


(defun stop ()
  (when *bot*
    (stop-polling *bot*)
    (setf *bot* nil)

    (sleep 1)
    (bt:all-threads)))


(defun start ()
  (stop)

  (40ants-logging:setup-for-repl :level :warn)

  (unless *bot*
    (setf *bot*
          (make-test-bot (uiop:getenv "TELEGRAM_TOKEN"))))
  
  (start-polling *bot* :debug t))
