(uiop:define-package #:cl-telegram-bot2/states/base
  (:use #:cl)
  (:import-from #:serapeum
                #:->
                #:pretty-print-hash-table
                #:dict
                #:soft-list-of)
  (:import-from #:sento.actor
                #:*state*)
  (:import-from #:print-items
                #:print-items
                #:print-items-mixin)
  (:import-from #:cl-telegram-bot2/generics
                #:process)
  (:import-from #:cl-telegram-bot2/api
                #:message-message-id)
  (:import-from #:cl-telegram-bot2/high
                #:collect-sent-messages)
  (:export #:var
           #:state-var
           #:clear-state-vars
           #:base-state
           #:state-id
           #:sent-message-ids
           #:state-vars))
(in-package #:cl-telegram-bot2/states/base)


(defclass base-state (print-items-mixin)
  ((id :initarg :id
       :initform nil
       :type (or null string)
       :reader state-id)
   (vars :initform (dict)
         :reader state-vars)
   (sent-message-ids :initform nil
                     :accessor sent-message-ids)))


(defmethod print-items append ((state base-state))
  (append
   (when (state-id state)
     (list (list :id
                 "id = ~S"
                 (state-id state))))
   (unless (zerop (hash-table-count (state-vars state)))
     (list (list :vars
                 "vars = {~A}"
                 (with-output-to-string (s)

                   (loop for key being the hash-key of (state-vars state)
                         using (hash-value value)
                         do (format s "~S: ~S"
                                    key value))))))))


(defgeneric state-var (state var-name)
  (:method ((state base-state) var-name)
    (gethash var-name (state-vars state))))


(defgeneric clear-state-vars (state)
  (:method ((state base-state))
    (clrhash (state-vars state))))


(defgeneric (setf state-var) (new-value state var-name)
  (:method (new-value (state base-state) var-name)
    (setf (gethash var-name (state-vars state))
          new-value)))


(defun var (var-name)
  (loop for state in *state*
        thereis (and (typep state 'base-state)
                     (state-var state var-name))))


(defun (setf var) (new-value var-name)
  (setf (state-var (first *state*)
                   var-name)
        new-value))


(defmethod process :around ((state base-state) (update t))
  (multiple-value-bind (sent-messages result)
      (collect-sent-messages
        (call-next-method))

    (loop for message in sent-messages
          do (push (message-message-id message)
                   (sent-message-ids state)))
    (values result)))


(defmethod cl-telegram-bot2/generics:on-state-activation :around ((state base-state))
  (call-next-method)
  ;; (multiple-value-bind (sent-messages result)
  ;;     (collect-sent-messages
  ;;       (call-next-method))
  ;;   (loop for message in sent-messages
  ;;         do (push (message-message-id message)
  ;;                  (sent-message-ids state)))
  ;;   (values result))
  )
