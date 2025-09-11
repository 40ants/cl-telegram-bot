(uiop:define-package #:cl-telegram-bot/entities/core
  (:use #:cl)
  (:import-from #:cl-telegram-bot/message
                #:message)
  (:import-from #:cl-telegram-bot/chat
                #:get-chat)
  (:import-from #:cl-telegram-bot/entities/generic
                #:make-entity-internal)
  (:nicknames #:cl-telegram-bot/entities))
(in-package cl-telegram-bot/entities/core)


(defclass entity ()
  ((payload :type message
            :initarg :payload
            :reader get-payload)
   (raw-data :initarg :raw-data
             :reader get-raw-data)))


(defclass unsupported-entity (entity)
  ())


(defmethod make-entity-internal (entity-type payload data)
  (declare (ignorable payload entity-type))
  (make-instance 'unsupported-entity
                 :raw-data data
                 :payload payload))


(defmethod get-chat ((command entity))
  (get-chat (get-payload command)))
