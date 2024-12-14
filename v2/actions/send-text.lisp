(uiop:define-package #:cl-telegram-bot2/actions/send-text
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/action
                #:action)
  (:import-from #:cl-telegram-bot2/generics
                #:on-result
                #:process
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/high
                #:reply)
  (:import-from #:serapeum
                #:soft-list-of
                #:->)
  (:import-from #:cl-telegram-bot2/utils
                #:call-if-needed)
  (:import-from #:cl-telegram-bot2/types
                #:reply-markup-type
                #:inline-keyboard-buttons)
  (:export #:send-text
           #:text
           #:reply-markup
           #:parse-mode))
(in-package #:cl-telegram-bot2/actions/send-text)


(defclass send-text (action)
  ((text :initarg :text
         :type (or symbol
                   string)
         :reader text)
   (reply-markup :initarg :reply-markup
                 :initform nil
                 :type (or null
                           symbol
                           reply-markup-type)
                 :reader reply-markup)
   (parse-mode :initarg :parse-mode
               :initform nil
               :type (or null
                         symbol
                         string)
               :documentation "Supported values are: `\"Markdown\"`, `\"MarkdownV2\"` or `\"HTML\"`. Read more about formatting options in the Telegram documentaion: https://core.telegram.org/bots/api#formatting-options"
               :reader parse-mode)))


(-> send-text ((or string symbol)
               &key
               (:reply-markup reply-markup-type)
               (:parse-mode (or null string)))
    (values send-text &optional))


(defun send-text (text-or-func-name
                  &key
                  reply-markup
                  parse-mode)
  (when (and (symbolp text-or-func-name)
             (not (fboundp text-or-func-name)))
    (error "SEND-TEXT waits a text or fbound symbol. ~S is not fbound."
           text-or-func-name))
  
  (make-instance 'send-text
                 :text text-or-func-name
                 :reply-markup reply-markup
                 :parse-mode parse-mode))


(defmethod print-object ((obj send-text) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S"
            (text obj))))


(-> do-action (send-text)
    (values &optional))

(defun do-action (action)
  (let* ((text (call-if-needed
                (text action)))
         (parse-mode (call-if-needed (parse-mode action)))
         (reply-markup (call-if-needed (reply-markup action))))
    (apply #'reply
           text 
           (append
            (when parse-mode
              (list :parse-mode parse-mode))
            (when reply-markup
              (list :reply-markup (reply-markup action))))))
  (values))


(defmethod on-state-activation ((action send-text))
  (do-action action))


(defmethod process ((action send-text) update)
  (do-action action))


(defmethod on-result ((action send-text) result)
  (do-action action))
