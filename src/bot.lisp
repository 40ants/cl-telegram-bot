(uiop:define-package :cl-telegram-bot/bot
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:dexador)
  (:import-from #:jonathan)
  (:export #:bot
           #:get-last-update-id
           #:defbot
           #:token
           #:file-endpoint
           #:api-uri
           #:get-endpoint))

(in-package cl-telegram-bot/bot)


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
    :initform nil)))


(defmacro defbot (name)
  `(progn
     (defclass ,name (bot)
       ())

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


