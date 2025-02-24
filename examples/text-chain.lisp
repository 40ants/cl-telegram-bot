(uiop:define-package #:cl-telegram-bot2-examples/text-chain
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
                #:back-to-nth-parent)
  (:import-from #:cl-telegram-bot2/api
                #:answer-pre-checkout-query
                #:pre-checkout-query
                #:message-message-id)
  (:import-from #:cl-telegram-bot2/state-with-commands
                #:command
                #:state-with-commands-mixin)
  (:import-from #:cl-telegram-bot2/generics
                #:on-pre-checkout-query
                #:on-result
                #:on-state-activation
                #:process)
  (:import-from #:cl-telegram-bot2/state
                #:state)
  (:import-from #:cl-telegram-bot2/actions/send-text
                #:send-text))
(in-package #:cl-telegram-bot2-examples/text-chain)



(defbot test-bot ()
  ()
  (:initial-state
   (state nil
    :on-update (state (send-text "Hello!")
                      :on-update (state (send-text "How are you doing?")
                                        :on-update (state (list (send-text "Bye!")
                                                                (back-to-nth-parent 2))))))))


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
