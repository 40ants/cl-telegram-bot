(uiop:define-package #:cl-telegram-bot2/states/base
  (:use #:cl)
  (:import-from #:serapeum
                #:->
                #:pretty-print-hash-table
                #:dict
                #:soft-list-of)
  (:import-from #:sento.actor-cell
                #:*state*)
  (:import-from #:print-items
                #:print-items
                #:print-items-mixin)
  (:import-from #:cl-telegram-bot2/generics
                #:on-result
                #:on-state-activation
                #:process)
  (:import-from #:cl-telegram-bot2/api
                #:update
                #:update-message
                #:message-message-id)
  (:import-from #:cl-telegram-bot2/high
                #:collect-sent-messages)
  (:export #:var
           #:state-var
           #:clear-state-vars
           #:base-state
           #:state-id
           #:sent-message-ids
           #:state-vars
           #:received-message-ids
           #:save-received-message-id
           #:capture-sent-messages))
(in-package #:cl-telegram-bot2/states/base)


(defclass base-state (print-items-mixin)
  ((id :initarg :id
       :initform nil
       :type (or null string)
       :reader state-id)
   (vars :initform (dict)
         :reader state-vars)
   (sent-message-ids :initform nil
                     :accessor sent-message-ids)
   (received-message-ids :initform nil
                         :accessor received-message-ids)))


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


(defmacro capture-sent-messages ((state-var) &body body)
  "Use this macro to capture messages end during PROCESS generic-function handling
   in case if your state inherits from BASE-STATE but does not call CALL-NEXT-METHOD."
  `(multiple-value-bind (sent-messages result)
       (collect-sent-messages
         ,@body)
    
     (loop for message in sent-messages
           do (push (message-message-id message)
                    (sent-message-ids ,state-var)))
     (values result)))


(-> save-received-message-id (base-state update)
    (values &optional))

(defun save-received-message-id (state update)
  "If some state class processes update and don't call CALL-NEXT-METHOD,
   then it have to call this function to register received message id.

   If you don't do this, then received messages deletion will not work
   for this state."
  (let ((message (update-message update)))
    (when message
      (push (message-message-id message)
            (received-message-ids state))))
  (values))


(defmethod process :around ((state base-state) (update t))
  (save-received-message-id state update)
  
  (capture-sent-messages (state)
    (call-next-method)))


(defmethod on-state-activation :around ((state base-state))
  (capture-sent-messages (state)
    (call-next-method)))


(defmethod on-result :around ((state base-state) result)
  (capture-sent-messages (state)
    (call-next-method)))

