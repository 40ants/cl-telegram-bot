(uiop:define-package #:cl-telegram-bot2-examples
  (:use #:cl)
  (:nicknames #:cl-telegram-bot2-examples/all)
  (:import-from #:cl-telegram-bot2/state
                #:state)
  (:import-from #:cl-telegram-bot2/server
                #:stop-polling
                #:start-polling)
  (:import-from #:cl-telegram-bot2/actions/send-text
                #:send-text)
  (:import-from #:cl-telegram-bot2/state-with-commands
                #:global-command
                #:command)
  (:import-from #:cl-telegram-bot2-examples/calc)
  (:import-from #:cl-telegram-bot2-examples/commands)
  (:import-from #:cl-telegram-bot2-examples/gallery)
  (:import-from #:cl-telegram-bot2-examples/payments)
  (:import-from #:cl-telegram-bot2-examples/mini-app)
  (:import-from #:cl-telegram-bot2-examples/echo)
  (:import-from #:cl-telegram-bot2-examples/text-chain)
  (:import-from #:cl-telegram-bot2/high/keyboard
                #:inline-keyboard
                #:call-callback)
  (:import-from #:cl-telegram-bot2/bot
                #:initial-state)
  (:import-from #:cl-telegram-bot2/term/back
                #:back-to-id)
  (:import-from #:cl-telegram-bot2/actions/delete-messages
                #:delete-messages))
(in-package #:cl-telegram-bot2-examples)


(defun show-menu-buttons ()
  (send-text "Choose an example to run:"
             :reply-markup
             (inline-keyboard
              (list
               (list
                (call-callback "Echo"
                               "open-echo")
                (call-callback "Text Chain"
                               "open-text-chain")
                (call-callback "Calc"
                               "open-calc"))
               (list
                (call-callback "Commands"
                               "open-commands")
                (call-callback "Gallery"
                               "open-gallery")
                (call-callback "Mini-app"
                               "open-mini-app"))
               (list
                (call-callback "Payments"
                               "open-payments"))))))


(defun make-mega-bot (token &rest args)
  (let* ((calc-state (initial-state
                      (cl-telegram-bot2-examples/calc::make-test-bot token)))
         (echo-state (initial-state
                      (cl-telegram-bot2-examples/echo::make-test-bot token)))
         (text-chain-state (initial-state
                            (cl-telegram-bot2-examples/text-chain::make-test-bot token)))
         (commands-state (initial-state
                          (cl-telegram-bot2-examples/commands::make-test-bot token)))
         (gallery-state (initial-state
                         (cl-telegram-bot2-examples/gallery::make-test-bot token)))
         (mini-app-state (initial-state
                          (cl-telegram-bot2-examples/mini-app::make-test-bot token)))
         (payments-state (initial-state
                          (cl-telegram-bot2-examples/payments::make-test-bot token)))
         (mega-state
           (state 'show-menu-buttons
                  :id "main-menu"
                  :commands (list (global-command "/menu"
                                                  (list (delete-messages)
                                                        (back-to-id "main-menu"))
                                                  :description "Show menu with all examples."))
                  :on-result 'show-menu-buttons
                  :on-callback-query (list (cons "open-echo"
                                                 (list (delete-messages)
                                                       echo-state))
                                           (cons "open-text-chain"
                                                 (list (delete-messages)
                                                       text-chain-state))
                                           (cons "open-calc"
                                                 (list (delete-messages)
                                                       calc-state))
                                           (cons "open-commands"
                                                 (list (delete-messages)
                                                       commands-state))
                                           (cons "open-gallery"
                                                 (list (delete-messages)
                                                       gallery-state))
                                           (cons "open-mini-app"
                                                 (list (delete-messages)
                                                       mini-app-state))
                                           (cons "open-payments"
                                                 (list (delete-messages)
                                                       payments-state))))))
    (apply #'make-instance
           'cl-telegram-bot2/bot::bot
           :token token
           :initial-state mega-state
           args)))



(defvar *bot* nil)


(defun start (&key (log-level :warn) (debug t))
  (stop)

  (40ants-logging:setup-for-repl :level log-level)

  (unless *bot*
    (setf *bot*
          (make-mega-bot (uiop:getenv "TELEGRAM_TOKEN"))))
  
  (start-polling *bot* :debug debug))


(defun stop ()
  (when *bot*
    (stop-polling *bot*)
    (setf *bot* nil)

    (sleep 1)
    (bt:all-threads)))
