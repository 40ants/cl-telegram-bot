(uiop:define-package #:cl-telegram-bot2/debug/diagram/vars
  (:use #:cl)
  (:import-from #:serapeum
                #:defvar-unbound)
  (:export
   #:*diagram-stream*))
(in-package #:cl-telegram-bot2/debug/diagram/vars)


(defvar *diagram-stream*)

(defvar *obj-to-id*)
(defvar *id-to-obj*)
(defvar *state-to-name*)
(defvar *name-to-state*)
(defvar *id-to-state*)
(defvar *current-map-id*)
(defvar *current-obj-id*)
(defvar *send-text-limit* 30)


(defvar-unbound *objects-created*
  "A list of object ids created by to-text generic-function.")


(defvar-unbound *on-after-object*
  "A hash-table to register callbacks to be called after some object was rendered.")
