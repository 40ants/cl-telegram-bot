(uiop:define-package #:cl-telegram-bot2/debug/diagram/slot
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/debug/diagram/generics
                #:to-text
                #:render-handlers)
  (:export #:slot
           #:slot-name
           #:slot-handlers))
(in-package #:cl-telegram-bot2/debug/diagram/slot)


(defclass slot ()
  ((name :initarg :name
         :reader slot-name)
   (value :initarg :value
          :reader slot-handlers)))


(defun slot (name value)
  (when value
    (make-instance 'slot
                   :name name
                   :value value)))


(defun to-slot (obj)
  (slot (cl-telegram-bot2/debug/diagram/generics:slot-name obj)
        obj))


(defmethod render-handlers ((slot slot))
  (render-handlers (slot-handlers slot)))


(defmethod to-text ((slot slot))
  (to-text (slot-handlers slot)))
