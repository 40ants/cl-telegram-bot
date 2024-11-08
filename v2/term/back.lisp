(uiop:define-package #:cl-telegram-bot2/term/back
  (:use #:cl)
  (:import-from #:alexandria
                #:required-argument)
  (:import-from #:serapeum
                #:->)
  (:export #:back
           #:back-to
           #:back-to-nth-parent
           #:result
           #:parent-number
           #:state-class))
(in-package #:cl-telegram-bot2/term/back)


;; TODO: probably we have to define a TERMINATOR
;; class for objects which interrupt processing
;; of the actions chain
(defclass back ()
  ((result :initarg :result
           :initform nil
           :reader result)))


(defun back (&optional result)
  (make-instance 'back
                 :result result))


(defclass back-to (back)
  ((state-class :initarg :state-class
                :initform (required-argument "State class is required argument.")
                :reader state-class)))


(defun back-to (state-class &optional result)
  (make-instance 'back-to
                 :state-class state-class
                 :result result))


(defclass back-to-nth-parent (back)
  ((n :initarg :n
      :initform (required-argument "Parent number required argument.")
      :type (integer 1)
      :reader parent-number)))


(-> back-to-nth-parent ((integer 1) &optional t))

(defun back-to-nth-parent (n &optional result)
  (make-instance 'back-to-nth-parent
                 :n n
                 :result result))
