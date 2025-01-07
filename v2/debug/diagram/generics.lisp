(uiop:define-package #:cl-telegram-bot2/debug/diagram/generics
  (:use #:cl)
  (:export #:render-handler-link
           #:render-handlers
           #:slot-name
           #:get-slots
           #:to-text))
(in-package #:cl-telegram-bot2/debug/diagram/generics)


(defgeneric render-handler-link (handler)
  (:documentation "Renders a map item for a given handler.

It should use render-mapslot-value-with-link or render-mapslot-value functions
for proper rendering of `PlantUML`."))


(defgeneric render-handlers (object)
  (:documentation "Renders a map with a list of handlers for some event."))


(defgeneric slot-name (obj)
  (:documentation "Returns a name for a state slot for such objects as callbacks or commands."))


(defgeneric get-slots (state)
  (:documentation "Returns state's slots or groups of slots as a list."))


(defgeneric to-text (obj)
  (:documentation "Renders object as an entity on `PlantUML` diagram."))

