(defpackage #:cl-telegram-bot/inline-keyboard
  (:use #:cl))
(in-package cl-telegram-bot/inline-keyboard)

;; TODO: refactor

(defun answer-callback-query (b callback-query-id &key text show-alert url)
  "https://core.telegram.org/bots/api#answercallbackquery"
  (let ((options
         (list
          (cons :callback_query_id callback-query-id))))
    (when text (nconc options `((:text . ,text))))
    (when show-alert (nconc options `((:show_alert . ,show-alert))))
    (when url (nconc options `((:url . ,url))))
    (apply #'make-request b "answerCallbackQuery" options)))

