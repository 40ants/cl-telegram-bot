(uiop:define-package #:cl-telegram-bot2-examples/payments
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/bot
                #:defbot)
  (:import-from #:cl-telegram-bot2/server
                #:stop-polling
                #:start-polling)
  (:import-from #:cl-telegram-bot2/high
                #:reply
                #:chat-state)
  (:import-from #:cl-telegram-bot2/actions/send-invoice
                #:send-invoice)
  (:import-from #:cl-telegram-bot2/state-with-commands
                #:global-command
                #:command
                #:state-with-commands-mixin)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:cl-telegram-bot2/pipeline
                #:back-to
                #:back)
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
                #:send-text)
  (:import-from #:cl-telegram-bot2/term/back
                #:back-to-id))
(in-package #:cl-telegram-bot2-examples/payments)


(defbot test-bot ()
  ()
  (:initial-state
   (state (send-text "Use command /pay to start payment process.")
          :id "initial"
          :on-update (send-text "Give /pay command to start payment process.")
          :on-result (send-text "Welcome back! Give /pay command to start payment process.")
          :commands (list (command "/pay"
                                   (send-invoice
                                    ;; title
                                    "Payment for the service"
                                    ;; description
                                    "This is the test service which will not be provided."
                                    ;; payload
                                    "foo-bar-payload"
                                    ;; provider token
                                    "381764678:TEST:100070"
                                    ;; currency
                                    "RUB"
                                    ;; prices
                                    (list (serapeum:dict "label" "Руб"
                                                         "amount" (* 120
                                                                     ;; Выражать цену надо в копейках
                                                                     100)))
                                    :on-success (list (send-text "Thank you for the payment!")
                                                      (back-to-id "initial"))
                                    :commands (list (command "/back"
                                                             ;; TODO: найти способ удалить invoice message
                                                             (list
                                                              (send-text "Invoice canceled!")
                                                              (back-to-id "initial"))))))))))


(defmethod on-pre-checkout-query ((bot test-bot) (query pre-checkout-query))
  (answer-pre-checkout-query (cl-telegram-bot2/api:pre-checkout-query-id query)
                             t)
  (values))


;; Technical parts:

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


(defun current-state ()
  (first
   (sento.actor-cell:state
    (first
     (sento.actor-context:all-actors
      (cl-telegram-bot2/bot::actors-system *bot*))))))
