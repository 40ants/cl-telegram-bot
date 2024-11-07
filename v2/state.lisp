(uiop:define-package #:cl-telegram-bot2/state
  (:use #:cl)
  (:import-from #:serapeum
                #:soft-list-of)
  (:import-from #:cl-telegram-bot2/action
                #:action)
  (:import-from #:cl-telegram-bot2/generics
                #:process
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/pipeline
                #:back)
  (:export #:state))
(in-package #:cl-telegram-bot2/state)


(defclass state ()
  ((on-activation :initarg :on-activation
                  :type (soft-list-of (or state
                                          action
                                          back))
                  :reader on-activation)
   (on-update :initarg :on-update
              :type (soft-list-of (or state
                                      action
                                      back))
              :reader on-update)))

(defmethod print-object ((state state) stream)
  (print-unreadable-object (state stream :type t)
    (when (on-activation state)
      (format stream " (~{~A~^ ~})"
              (on-activation state)))
    (when (on-update state)
      (format stream " :ON-UPDATE (~{~A~^ ~})"
              (on-update state)))))


(defun state (on-activation &key on-update)
  (make-instance 'state
                 :on-activation (uiop:ensure-list on-activation)
                 :on-update (uiop:ensure-list on-update)))


(defmethod on-state-activation ((state state))
  (loop for obj in (on-activation state)
        thereis (typecase obj
                  (action
                   (on-state-activation obj))
                  (t
                   obj))))


(defmethod process ((state state) update)
  (loop for obj in (on-update state)
        thereis (typecase obj
                  (action
                   (process obj update))
                  (t
                   obj))))
