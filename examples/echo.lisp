(uiop:define-package #:cl-telegram-bot2-examples/echo
  (:use #:cl)
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
  (:import-from #:serapeum
                #:dict
                #:fmt)
  (:import-from #:cl-telegram-bot2/pipeline
                #:back-to-nth-parent
                #:back-to
                #:back)
  (:import-from #:cl-telegram-bot2/api
                #:update-message
                #:message-text
                #:message-message-id)
  (:import-from #:cl-telegram-bot2/states/ask-for-number
                #:ask-for-number)
  (:import-from #:cl-telegram-bot2/states/base
                #:var)
  (:import-from #:cl-telegram-bot2/states/ask-for-choice
                #:ask-for-choice)
  (:import-from #:40ants-logging)
  (:import-from #:cl-telegram-bot2/term/back
                #:back-to-id))
(in-package #:cl-telegram-bot2-examples/echo)


(defun reply-with-same-text (update)
  (reply (message-text
          (update-message update)))
  (values))


(defbot test-bot ()
  ()
  (:initial-state
   (state (send-text "Hello, I'm the echo bot.")
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


(defun clean-threads ()
  "TODO: надо разобраться почему треды не подчищаются. Возможно это происходит когда случаются ошибки?"
  (loop for tr in (bt:all-threads)
        when (or (str:starts-with? "message-thread" (bt:thread-name tr))
                 (str:starts-with? "timer-wheel" (bt:thread-name tr))
                 (str:starts-with? "telegram-bot" (bt:thread-name tr)))
        do (bt:destroy-thread tr)))
