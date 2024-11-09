(uiop:define-package #:cl-telegram-bot2/states/base
  (:use #:cl)
  (:import-from #:serapeum
                #:->
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
  (:import-from #:cl-telegram-bot2/state-with-commands
                #:state-with-commands-mixin)
  (:export #:state
           #:result-var
           #:state-result-var
           #:clear-state-result-vars
           #:base-state
           #:state-id))
(in-package #:cl-telegram-bot2/states/base)


(defclass base-state (print-items-mixin)
  ((id :initarg :id
       :initform nil
       :type (or null string)
       :reader state-id)
   (result-vars :initform (dict)
                :reader state-result-vars)))


(defmethod print-items append ((state base-state))
  (append
   (when (state-id state)
     (list (list :id
                 "id = ~S"
                 (state-id state))))
   (unless (zerop (hash-table-count (state-result-vars state)))
     (list (list :result-vars
                 "result-vars = {~A}"
                 (with-output-to-string (s)

                   (loop for key being the hash-key of (state-result-vars state)
                         using (hash-value value)
                         do (format s "~S: ~S"
                                    key value))))))))


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
