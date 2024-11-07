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
                #:process))
(in-package #:cl-telegram-bot2-examples/payments)


(defclass initial-state (state-with-commands-mixin)
  ()
  (:default-initargs
   :commands (list
              (command "/pay" (make-instance 'send-invoice)
                       :description "Send invoice."))))


(defclass send-invoice ()
  ())


;; (defmethod on-state-activation ((state initial-state))
;;   ;; (reply "Use command /pay to start payment process.")
;;   (values))


(defmethod on-result ((state initial-state) result)
  (reply "Welcome back! Give /pay command to start pyament process.")
  (values))


(defmethod process ((state initial-state) update)
  (reply "Give /pay command to start payment process.")
  (values))


(defmethod on-state-activation ((state send-invoice))
  (cl-telegram-bot2/api::send-invoice
   (cl-telegram-bot2/api::chat-id cl-telegram-bot2/vars::*current-chat*)
   ;; title
   "Подписка на поиск судебных дел в течении месяца"
   ;; description
   "В течении месяца бот будет уведомлять вас о появлении новых судебных дел по вашим запросам."
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
                                    100))))
  (values))


(defmethod process ((state send-invoice) update)
  (let* ((message
           (cl-telegram-bot2/api:update-message
            update))
         (successful-payment
           (cl-telegram-bot2/api:message-successful-payment message)))
    (when successful-payment
      (reply "Спасибо за покупку!"))
    'initial-state))



(defbot test-bot ()
  ()
  (:initial-state 'initial-state))


(defmethod on-pre-checkout-query ((bot test-bot) (query pre-checkout-query))
  (answer-pre-checkout-query (cl-telegram-bot2/api:pre-checkout-query-id query)
                             t))


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
