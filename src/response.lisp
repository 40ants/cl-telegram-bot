(uiop:define-package #:cl-telegram-bot/response
  (:use #:cl)
  (:import-from #:cl-telegram-bot/message
                #:message
                #:get-chat
                #:send-message
                #:*current-message*
                #:*current-bot*)
  (:import-from #:cl-telegram-bot/response-processing
                #:interrupt-processing
                #:process-response)
  (:import-from #:cl-telegram-bot/callback
                #:callback-message
                #:callback)
  (:import-from #:cl-telegram-bot/inline-keyboard
                #:answer-callback-query)
  (:import-from #:cl-telegram-bot/markup
                #:to-markup)
  (:export #:response-text
           #:reply
           #:notify
           #:open-url
           #:alert
           #:response
           #:url-to-open
           #:rest-args))
(in-package #:cl-telegram-bot/response)


(defclass response ()
  ((args :initarg :args
         :type list
         :reader rest-args)))


(defclass response-with-text (response)
  ((text :initarg :text
         :reader response-text)))


(defclass reply (response-with-text)
  ())


(defclass notify (response-with-text)
  ())


(defclass alert (response-with-text)
  ())


(defclass open-url (response)
  ((url :initarg :text
        :type string
        :reader url-to-open)))



(defun reply (text
              &rest args
              &key
              ;; Set this to "markdown" to allow rich formatting
              ;; https://core.telegram.org/bots/api#formatting-options
              parse-mode
              disable-web-page-preview
              disable-notification
              reply-to-message-id
              reply-markup
              (immediately t))
  (declare (ignorable parse-mode
                      disable-web-page-preview
                      disable-notification
                      reply-to-message-id))
  "Works like a SEND-MESSAGE, but only when an incoming message is processed.
   Automatically sends reply to a chat from where current message came from."
  (unless (and (boundp '*current-bot*)
               (boundp '*current-message*))
    (error "Seems (reply ~S) was called outside of processing pipeline, because no current message is available."
           text))

  (when reply-markup
    (setf (getf args :reply-markup)
          (to-markup reply-markup)))
  
  (process-response *current-bot*
                    *current-message*
                    (make-instance 'reply
                                   :text text
                                   :args args))
  (when immediately
    (interrupt-processing)))


(defun notify (text)
  "Works like a SEND-MESSAGE, but only when an incoming message is processed.
   Automatically sends reply to a chat from where current message came from."
  (unless (and (boundp '*current-bot*)
               (boundp '*current-message*))
    (error "Seems (notify ~S) was called outside of processing pipeline, because no current message is available."
           text))

  (process-response *current-bot*
                    *current-message*
                    (make-instance 'notify
                                   :text text)))


(defun alert (text)
  "Works like a SEND-MESSAGE, but only when an incoming message is processed.
   Automatically sends reply to a chat from where current message came from."
  (unless (and (boundp '*current-bot*)
               (boundp '*current-message*))
    (error "Seems (alert ~S) was called outside of processing pipeline, because no current message is available."
           text))

  (process-response *current-bot*
                    *current-message*
                    (make-instance 'alert
                                   :text text)))


(defun open-url (url)
  "Works like a SEND-MESSAGE, but only when an incoming message is processed.
   Automatically sends reply to a chat from where current message came from."
  (unless (and (boundp '*current-bot*)
               (boundp '*current-message*))
    (error "Seems (open-url ~S) was called outside of processing pipeline, because no current message is available."
           url))

  (process-response *current-bot*
                    *current-message*
                    (make-instance 'open-url
                                   :url url)))


(defmethod process-response ((bot t) (message message) (response reply))
  (apply #'send-message
         bot
         (get-chat message)
         (response-text response)
         (rest-args response)))


(defmethod process-response ((bot t) (callback callback) (response reply))
  (apply #'send-message
         bot
         (get-chat (callback-message callback))
         (response-text response)
         (rest-args response))
  ;; And we need to send empty callback answer, just to hide loading process bar.
  (answer-callback-query bot
                         callback))


(defmethod process-response ((bot t) (message callback) (response notify))
  (answer-callback-query bot
                         message
                         :text (response-text response)))


(defmethod process-response ((bot t) (message callback) (response alert))
  (answer-callback-query bot
                         message
                         :text (response-text response)
                         :show-alert t))


(defmethod process-response ((bot t) (message callback) (response open-url))
  (answer-callback-query bot
                         message
                         :url (url-to-open response)))


