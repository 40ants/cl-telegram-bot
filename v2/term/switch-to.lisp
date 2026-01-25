(uiop:define-package #:cl-telegram-bot2/term/switch-to
  (:use #:cl)
  (:import-from #:alexandria
                #:required-argument)
  (:import-from #:serapeum
                #:->)
  (:import-from #:cl-telegram-bot2/state
                #:state)
  (:import-from #:cl-telegram-bot2/term/back
                #:back)
  (:import-from #:trivial-types
                #:function-designator)
  (:export #:switch-to
           #:delete-prev-state-p
           #:switch-to-state))
(in-package #:cl-telegram-bot2/term/switch-to)


(defparameter *default-delete-prev-state-p*
  (lambda (state)
    (declare (ignore state))
    t))


(defclass switch-to (back)
  ((state :initarg :state
          :initform (required-argument "State is required argument.")
          :type state
          :reader switch-to-state)
   (delete-prev-state-p :initarg :delete-prev-state-p
                        :initform *default-delete-prev-state-p*
                        :type function-designator
                        :reader delete-prev-state-p
                        :documentation "A function of on argument - current state. If predicate returns T, then state will be replaced, otherwise it will be kept on stack. By default, any state is replaced."))
  (:documentation "Replaces current state with a new one.

                   Generic-function CL-TELEGRAM-BOT2/GENERICS:ON-STATE-DELETION will be called on the old state.")
  (:default-initargs
   :process-result-p nil))


(-> switch-to (state &key (:delete-prev-state-p function-designator))
    (values switch-to &optional))

(defun switch-to (state &key (delete-prev-state-p *default-delete-prev-state-p*))
  (make-instance 'switch-to
                 :state state
                 :delete-prev-state-p delete-prev-state-p))


(defmethod print-object ((obj switch-to) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~S"
            (switch-to-state obj))))
