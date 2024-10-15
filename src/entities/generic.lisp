(uiop:define-package #:cl-telegram-bot/entities/generic
  (:use #:cl)
  (:import-from #:cl-telegram-bot/utils
                #:make-keyword)
  (:import-from #:arrows
                #:->)
  (:export #:make-entity
           #:make-entity-internal))
(in-package #:cl-telegram-bot/entities/generic)


(defgeneric make-entity-internal (entity-type payload data)
  (:documentation "Extendable protocol to support entities of different kinds.
                   First argument is a keyword, denoting a type of the entity.
                   Payload is an object of type `message'.
                   And data is a plist with data, describing the entity."))


(defun make-entity (payload data)
  (let ((entity-type (-> data
                         (getf :|type|)
                         (make-keyword))))
    (make-entity-internal entity-type
                          payload
                          data)))
