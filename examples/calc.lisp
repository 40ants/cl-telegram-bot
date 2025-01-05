(uiop:define-package #:cl-telegram-bot2-examples/calc
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
  (:import-from #:serapeum
                #:dict
                #:fmt)
  (:import-from #:cl-telegram-bot2/states/ask-for-number
                #:ask-for-number)
  (:import-from #:cl-telegram-bot2/states/base
                #:var)
  (:import-from #:cl-telegram-bot2/states/ask-for-choice
                #:ask-for-choice)
  (:import-from #:40ants-logging)
  (:import-from #:cl-telegram-bot2/term/back
                #:back-to-id)
  (:import-from #:cl-telegram-bot2/actions/delete-messages
                #:delete-messages))
(in-package #:cl-telegram-bot2-examples/calc)



(defun calc-result ()
  (let* ((num1 (var "first-num"))
         (num2 (var "second-num"))
         (op-name (var "operation-name"))
         (op (gethash op-name
                      (dict "+" #'+
                            "-" #'-
                            "*" #'*
                            "/" #'/))))
    (funcall op num1
             num2)))

(defun send-result (result)
  (send-text
   (format nil "Result is: ~A"
           result)))


(defun make-prompt-for-op-choice ()
  (fmt "Select an operation to apply to ~A and ~A:"
       (var "first-num")
       (var "second-num")))


(defbot test-bot ()
  ()
  (:initial-state
   (state (state (list
                  (send-text "Let's calculate!")
                  (ask-for-number
                   "Enter the first number:"
                   :to "first-num"
                   :on-validation-error (send-text "Enter the number, please.")
                   :on-deletion (delete-messages)
                   :on-success (ask-for-number
                                "Enter the second number:"
                                :to "second-num"
                                :on-validation-error (send-text "Enter the number, please.")
                                :on-deletion (delete-messages)
                                :on-success (ask-for-choice
                                             'make-prompt-for-op-choice
                                             '("+" "-" "*" "/")
                                             :to "operation-name"
                                             :on-success (list ;; Here we just calculate result
                                                               ;; and return back to "start" state
                                                               ;; which will send result to the user
                                                               ;; in the :ON-RESULT handler
                                                               (back-to-id "calc-example"
                                                                           'calc-result))))))
                 :on-deletion (delete-messages))
          :id "calc-example"
          :on-result 'send-result)))


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
