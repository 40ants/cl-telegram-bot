(uiop:define-package #:cl-telegram-bot2/term/back
  (:use #:cl)
  (:import-from #:alexandria
                #:required-argument)
  (:import-from #:serapeum
                #:->)
  (:import-from #:cl-telegram-bot2/generics
                #:process)
  (:export #:back
           #:back-to
           #:back-to-nth-parent
           #:result
           #:parent-number
           #:state-class
           #:back-to-id
           #:parent-id))
(in-package #:cl-telegram-bot2/term/back)


;; TODO: probably we have to define a TERMINATOR
;; class for objects which interrupt processing
;; of the actions chain
(defclass back ()
  ((result :initarg :result
           :initform nil
           :reader result)))


(-> back (&optional t)
    (values back &optional))

(defun back (&optional result)
  (make-instance 'back
                 :result result))


(defclass back-to (back)
  ((state-class :initarg :state-class
                :initform (required-argument "State class is required argument.")
                :reader state-class)))


(-> back-to (symbol &optional t)
    (values back-to &optional))

(defun back-to (state-class &optional result)
  (make-instance 'back-to
                 :state-class state-class
                 :result result))


(defclass back-to-nth-parent (back)
  ((n :initarg :n
      :initform (required-argument "Parent number required argument.")
      :type (integer 1)
      :reader parent-number)))


(-> back-to-nth-parent ((integer 1) &optional t)
    (values back-to-nth-parent &optional))

(defun back-to-nth-parent (n &optional result)
  (make-instance 'back-to-nth-parent
                 :n n
                 :result result))


(defclass back-to-id (back)
  ((id :initarg :id
       :initform (required-argument "Parent id is required argument.")
       :type string
       :reader parent-id)))


(-> back-to-id (string &optional t)
    (values back-to-id &optional))

(defun back-to-id (id &optional result)
  (make-instance 'back-to-id
                 :id id
                 :result result))


(defmethod process ((item back) update)
  ;; If a some action returns a BACK object when processing a list of actions,
  ;; then PROCESS generic-function will be called on it again
  ;; and in this case we should return the same BACK object to interrupt the list processing
  (values item))
