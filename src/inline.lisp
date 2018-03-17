(defpackage #:cl-telegram-bot/inline
  (:use #:cl))
(in-package cl-telegram-bot/inline)


(defun answer-inline-query (b inline-query-id results &key cache-time is-personal next-offset switch-pm-text)
  "https://core.telegram.org/bots/api#answerinlinequery"
  (let ((options
         (list
          (cons :inline_query_id inline-query-id)
          (cons :results results))))
    (when cache-time (nconc options `((:cache_time . ,cache-time))))
    (when is-personal (nconc options `((:is_personal . ,is-personal))))
    (when next-offset (nconc options `((:next_offset . ,next-offset))))
    (when switch-pm-text (nconc options `((:switch_pm_text . ,switch-pm-text))))
    (make-request b "answerInlineQuery" options)))
