(uiop:define-package :cl-telegram-bot/bot
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:dexador)
  (:import-from #:jonathan)
  (:export #:api-uri
           #:bot
           #:debug-mode
           #:defbot
           #:file-endpoint
           #:get-endpoint
           #:get-last-update-id
           #:token
           #:sent-commands-cache
           #:bot-info))
(in-package #:cl-telegram-bot/bot)


(defclass bot ()
  ((id
    :documentation "Update id"
    :initform 0
    :accessor get-last-update-id)
   (token
    :initarg :token
    :documentation "Bot token given by BotFather"
    :accessor token
    :initform nil)
   (api-uri
    :initarg  :api-uri
    :initform "https://api.telegram.org/"
    :accessor api-uri)
   (endpoint
    :initarg :endpoint
    :reader get-endpoint
    :documentation "HTTPS endpoint")
   (file-endpoint
    :initarg :file-endpoint
    :accessor file-endpoint
    :documentation "HTTPS file-endpoint"
    :initform nil)
   (bot-info :initform nil
             :documentation "This slot will be filled with CL-TELEGRAM-BOT/USER:USER object on first access using a call to CL-TELEGRAM-BOT/USER:GET-ME function."
             :reader bot-info)
   (debug-mode
    :initform nil
    :initarg :debug-mode
    :accessor debug-mode
    :documentation "When debug mode is T, then interactive debugger will be called on each error.")
   (sent-commands-cache :initform nil
                        :documentation "Command processing code will use this cache to update commands list on the server
                                        when a new method for CL-TELEGRAM-BOT/ENTITIES/COMMAND:ON-COMMAND generic-function is defined.

                                        This slot is for internal use."
                        :accessor sent-commands-cache)))


(defmacro defbot (name &optional slots options)
  "Use this macro to define a class of your Telegram bot."
  `(progn
     (defclass ,name (bot)
       ,slots
       ,@options)

     (defun ,(alexandria:symbolicate 'make- name) (token &rest args)
       (apply 'make-instance
              ',name
              :token token
              args))))


(defmethod initialize-instance :after ((bot bot) &key &allow-other-keys)
  (with-accessors ((token         token)
                   (file-endpoint file-endpoint)
                   (api-uri       api-uri)) bot
    (setf (slot-value bot 'endpoint)
          (concatenate 'string api-uri "bot" token "/")
          (slot-value bot 'file-endpoint)
          (concatenate 'string api-uri "file/" "bot" token "/"))))


(defmethod print-object ((bot bot) stream)
  (print-unreadable-object
      (bot stream :type t)
    (format stream
            "id=~A" (get-last-update-id bot))))
