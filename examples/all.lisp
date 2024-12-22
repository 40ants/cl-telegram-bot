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
               (call-callback "Calc"
                              "open-calc")
               (call-callback "Commands"
                              "open-commands")
               (call-callback "Gallery"
                              "open-gallery")))))


(defun make-mega-bot (token &rest args)
  (let* ((calc-state (initial-state
                      (cl-telegram-bot2-examples/calc::make-test-bot token)))
         (commands-state (initial-state
                          (cl-telegram-bot2-examples/commands::make-test-bot token)))
         (gallery-state (initial-state
                         (cl-telegram-bot2-examples/gallery::make-test-bot token)))
         (mega-state
           (state 'show-menu-buttons
                  :id "main-menu"
                  :commands (list (global-command "/menu"
                                                  (back-to-id "main-menu")
                                                  :description "Show menu with all examples."))
                  :on-result 'show-menu-buttons
                  :on-callback-query (list (cons "open-calc"
                                                 (list (delete-messages)
                                                       calc-state))
                                           (cons "open-commands"
                                                 (list (delete-messages)
                                                       commands-state))
                                           (cons "open-gallery"
                                                 (list (delete-messages)
                                                       gallery-state))))))
    (apply #'make-instance
           'cl-telegram-bot2/bot::bot
           :token token
           :initial-state mega-state
           args)))



(defvar *bot* nil)


(defun start ()
  (stop)

  (40ants-logging:setup-for-repl :level :warn)

  (unless *bot*
    (setf *bot*
          (make-mega-bot (uiop:getenv "TELEGRAM_TOKEN"))))
  
  (start-polling *bot* :debug t))


(defun stop ()
  (when *bot*
    (stop-polling *bot*)
    (setf *bot* nil)

    (sleep 1)
    (bt:all-threads)))

