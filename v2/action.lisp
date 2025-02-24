(uiop:define-package #:cl-telegram-bot2/action
  (:use #:cl)
  (:export #:action
           #:call-if-action))
(in-package #:cl-telegram-bot2/action)


(defclass action ()
  ())



(defun call-if-action (obj func &rest args)
  "Useful in CL-TELEGRAM-BOT2/GENERICS:PROCESS handlers in case if
   state has additional handler stored in the slot and this
   slot can be either state or action.

   This function is recursive, because processing of an action
   could return another action and we should call FUNC until
   a new state or NIL will be returned."
  (typecase obj
    (list
       ;; Some handlers may represent a list of actions
       ;; and states, thus we need to call FUNC
       ;; while a non-nil and non-action object will be returned.
       (loop for item in obj
             thereis (apply #'call-if-action
                            item func args)))
    (action
       (apply #'call-if-action
              (apply func obj args)
              args))
    (t
       obj)))
