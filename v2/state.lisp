(uiop:define-package #:cl-telegram-bot2/state
  (:use #:cl)
  (:import-from #:serapeum
                #:pretty-print-hash-table
                #:dict
                #:soft-list-of)
  (:import-from #:cl-telegram-bot2/action
                #:action)
  (:import-from #:cl-telegram-bot2/generics
                #:process
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/term/back
                #:back)
  (:import-from #:sento.actor
                #:*state*)
  (:import-from #:print-items
                #:print-items
                #:print-items-mixin)
  (:export #:state
           #:result-var
           #:state-result-var
           #:clear-state-result-vars
           #:base-state))
(in-package #:cl-telegram-bot2/state)


(defclass base-state (print-items-mixin)
  ((result-vars :initform (dict)
                :reader state-result-vars)))


(defclass state (base-state)
  ((on-activation :initarg :on-activation
                  :type (soft-list-of (or base-state
                                          action
                                          back))
                  :reader on-activation)
   (on-update :initarg :on-update
              :type (soft-list-of (or base-state
                                      action
                                      back))
              :reader on-update)))


(defmethod print-items append ((state base-state))
  (unless (zerop (hash-table-count (state-result-vars state)))
    (list (list :result-vars
                "result-vars = {~A}"
                (with-output-to-string (s)

                  (loop for key being the hash-key of (state-result-vars state)
                        using (hash-value value)
                        do (format s "~S: ~S"
                                   key value)))))))

(defmethod print-items append ((state state))
  (append
   (when (on-activation state)
     (list (list :on-activation
                 " on-activation = ~S"
                 (on-activation state))))
   (when (on-update state)
     (list (list :on-update
                 " on-update = ~S"
                 (on-update state))))))


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
  (process (on-update state)
           update))


(defmethod process ((items list) update)
  (loop for obj in items
        thereis (etypecase obj
                  (action
                   (process obj update))
                  (base-state
                   obj)
                  (back
                   obj))))


(defgeneric state-result-var (state var-name)
  (:method ((state base-state) var-name)
    (gethash var-name (state-result-vars state))))


(defgeneric clear-state-result-vars (state)
  (:method ((state base-state))
    (clrhash (state-result-vars state))))


(defgeneric (setf state-result-var) (new-value state var-name)
  (:method (new-value (state base-state) var-name)
    (setf (gethash var-name (state-result-vars state))
          new-value)))


(defun result-var (var-name)
  (loop for state in *state*
        thereis (and (typep state 'base-state)
                     (state-result-var state var-name))))
