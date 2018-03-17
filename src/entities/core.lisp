(defpackage #:cl-telegram-bot/entities/core
  (:use #:cl)
  (:import-from #:cl-telegram-bot/utils
                #:make-keyword)
  (:import-from #:cl-arrows
                #:->)
  (:nicknames #:cl-telegram-bot/entities)
  (:export
   #:make-entity
   #:make-entity-internal))
(in-package cl-telegram-bot/entities/core)


(defclass entity ()
  ((raw-data :initarg :raw-data
             :reader get-raw-data)))


(defclass unsupported-entity (entity)
  ())


(defgeneric make-entity-internal (entity-type payload data)
  (:documentation "Extendable protocol to support entities of different kinds.
                   First argument is a keyword, denoting a type of the entity.
                   Payload is an object of type `message'.
                   And data is a plist with data, describing the entity."))


(defmethod make-entity-internal (entity-type payload data)
  (declare (ignorable payload entity-type))
  (make-instance 'unsupported-entity
                 :raw-data data))


(defun make-entity (payload data)
  (let ((entity-type (-> data
                         (getf :|type|)
                         (make-keyword))))
    (make-entity-internal entity-type
                          payload
                          data)))
