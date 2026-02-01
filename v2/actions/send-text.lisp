(uiop:define-package #:cl-telegram-bot2/actions/send-text
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/action
                #:action)
  (:import-from #:cl-telegram-bot2/generics
                #:on-result
                #:process-state
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/high
                #:reply)
  (:import-from #:cl-telegram-bot2/api)
  (:import-from #:serapeum
                #:fmt
                #:soft-list-of
                #:->)
  (:import-from #:cl-telegram-bot2/utils
                #:call-if-needed)
  (:import-from #:cl-telegram-bot2/types
                #:reply-markup-type
                #:inline-keyboard-buttons)
  (:import-from #:cl-telegram-bot2/debug/diagram/generics
                #:render-handler-link)
  (:import-from #:cl-telegram-bot2/debug/diagram/utils
                #:render-mapslot-value)
  (:import-from #:str
                #:shorten)
  (:import-from #:cl-telegram-bot2/debug/diagram/vars
                #:*send-text-limit*)
  (:import-from #:cl-telegram-bot2/sent-messages
                #:save-sent-message-id)
  (:export #:send-text
           #:text
           #:reply-markup
           #:parse-mode
           #:link-preview-options))
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
               :reader parse-mode)
   (link-preview-options :initarg :link-preview-options
                         :initform nil
                         :type (or null
                                   symbol
                                   (member :disable)
                                   cl-telegram-bot2/api:link-preview-options)
                         :documentation "Options object of type CL-TELEGRAM-BOT2/API:LINK-PREVIEW-OPTIONS to control if Telegram client should show a preview for some link from the message. Can be a symbol bound to a callable of one positional argument - action itself + keyword arguments `:text`, `:parse-mode` and `:reply-markup`. Shortcut :disable can be used instead of an object."
                         :reader link-preview-options)))


(-> send-text ((or string symbol)
               &key
               (:reply-markup (or null
                                  symbol
                                  reply-markup-type))
               (:parse-mode (or null
                                symbol
                                string))
               (:link-preview-options (or null
                                          symbol
                                          cl-telegram-bot2/api:link-preview-options)))
    (values send-text &optional))


(defun send-text (text-or-func-name
                  &key
                    reply-markup
                    parse-mode
                    link-preview-options)
  (when (and (symbolp text-or-func-name)
             (not (fboundp text-or-func-name)))
    (error "SEND-TEXT waits a text or fbound symbol. ~S is not fbound."
           text-or-func-name))
  
  (make-instance 'send-text
                 :text text-or-func-name
                 :reply-markup reply-markup
                 :parse-mode parse-mode
                 :link-preview-options link-preview-options))


(defmethod print-object ((obj send-text) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S"
            (text obj))))


(-> do-action (send-text)
    (values &optional))

(defun do-action (action)
  (let* ((text (call-if-needed (text action)
                               action))
         (parse-mode (call-if-needed (parse-mode action)
                                     action
                                     :text text))
         (reply-markup (call-if-needed (reply-markup action)
                                       action
                                       :text text
                                       :parse-mode parse-mode))
         (link-preview-options (call-if-needed (link-preview-options action)
                                               action
                                               :text text
                                               :parse-mode parse-mode
                                               :reply-markup reply-markup)))
    (when (and link-preview-options
               (keywordp link-preview-options))
      (unless (eql link-preview-options
                   :disable)
        (error "When link-preview-options is given as a keyword, it should be :DISABLE"))
      
      (setf link-preview-options
            (make-instance 'cl-telegram-bot2/api:link-preview-options
                           :is-disabled t)))
    (let ((message (apply #'reply
                          text 
                          (append
                           (when parse-mode
                             (list :parse-mode parse-mode))
                           (when reply-markup
                             (list :reply-markup reply-markup))
                           (when link-preview-options
                             (list :link-preview-options link-preview-options))))))
      (save-sent-message-id cl-telegram-bot2/vars::*current-state*
                            message)))
  (values))


(defmethod on-state-activation ((action send-text))
  (do-action action))


(defmethod process-state ((bot t) (action send-text) update)
  (do-action action))


(defmethod on-result ((action send-text) result)
  (do-action action))



(defmethod render-handler-link ((action send-text))
  (render-mapslot-value
   "action"
   (fmt "~A\\n~A"
        (class-name (class-of action))
        (let ((text (text action)))
          (etypecase text
            (string
               (shorten *send-text-limit*
                        text))
            (symbol
               text))))))
