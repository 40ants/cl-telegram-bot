(uiop:define-package #:cl-telegram-bot2/actor-reference
  (:use #:cl)
  (:import-from #:sento.actor-context
                #:actor-context)
  (:import-from #:alexandria
                #:required-argument)
  (:import-from #:serapeum
                #:->))
(in-package #:cl-telegram-bot2/actor-reference)


(defclass actor-ref ()
  ((context :initarg :context
            :initform (required-argument "Argument :CONTEXT is required.")
            :type actor-context
            :reader %actor-context)
   (actor-name :initarg :actor-name
               :initform (required-argument "Argument :ACTOR-NAME is required.")
               :type string
               :reader %actor-name)))


(-> actor-ref (actor-context string)
    (values actor-ref &optional))

(defun actor-ref (context actor-name)
  "Creates an actor reference which can be used to ASK actor with given name.

   NAME should be a full name of the actor in the given CONTEXT, like /user/request-processor."
  (unless (sento.actor-context:find-actors
           context actor-name)
    (error "Actor ~A does not exist."
           actor-name))
  
  (make-instance 'actor-ref
                 :context context
                 :actor-name actor-name))


(defmethod sento.actor:ask ((self actor-ref) message &key (time-out nil))
  (sento.actor:ask (first
                    (sento.actor-context:find-actors
                     (%actor-context self)
                     (%actor-name self)))
                   message
                   :time-out time-out))


(defmethod sento.actor:ask-s ((self actor-ref) message &key (time-out nil))
  (sento.actor:ask-s (first
                      (sento.actor-context:find-actors
                       (%actor-context self)
                       (%actor-name self)))
                     message
                     :time-out time-out))
