(uiop:define-package #:cl-telegram-bot2/action
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/debug/diagram/generics
                #:to-text
                #:render-handler-link)
  (:import-from #:cl-telegram-bot2/debug/diagram/utils
                #:render-mapslot-value)
  (:export #:action
           #:call-if-action))
(in-package #:cl-telegram-bot2/action)


(defclass action ()
  ())



(defun call-if-action (obj func &rest args)
  "Useful in CL-TELEGRAM-BOT2/GENERICS:PROCESS handlers in case if
   state has additional handler stored in the slot and this
   slot can be either state or action or a list of actions and states.

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


(defmethod render-handler-link ((action action))
  (render-mapslot-value
   "action"
   (symbol-name
    (class-name (class-of action)))))


(defmethod to-text ((action action))
  ;; NOTE: Decided to not render blocks for funcs and actions.
  ;; Probably will need to show again if I decide to render
  ;; output arrows from functions.
  (values))
