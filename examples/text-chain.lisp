(uiop:define-package #:cl-telegram-bot2-examples/text-chain
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/bot
                #:defbot)
  (:import-from #:cl-telegram-bot2/server
                #:stop-polling
                #:start-polling)
  (:import-from #:cl-telegram-bot2/state
                #:state)
  (:import-from #:cl-telegram-bot2/actions/send-text
                #:send-text)
  (:import-from #:cl-telegram-bot2/actions/delete-messages
                #:delete-messages))
(in-package #:cl-telegram-bot2-examples/text-chain)



(defbot test-bot ()
  ()
  (:initial-state
   (state
    (state (send-text "Hello!")
           :on-deletion (delete-messages)
           :on-update (state (send-text "How are you doing?")
                             :on-deletion (delete-messages)
                             :on-update (state (send-text "Bye!")
                                               :on-deletion (delete-messages))))
    :id "text-chain-example")))


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
