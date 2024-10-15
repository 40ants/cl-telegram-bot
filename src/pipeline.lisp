(defpackage #:cl-telegram-bot/pipeline
  (:use #:cl)
  (:export
   #:process))
(in-package cl-telegram-bot/pipeline)


(defgeneric process (bot object)
  (:documentation "This method is called by when processing a single update.
                   It is called multiple times on different parts of an update.
                   Whole pipeline looks like that:

                   For each update we call:
                     process(update)
                     process(update.payload)
                     For each entity in payload:
                       process(entity)
                   "))


(defmethod process (bot object)
  "By default, processing does nothing"
  (declare (ignorable bot object))
  (log:warn "No PROCESS method for processing objects of ~A type."
            (type-of object))
  (values))
