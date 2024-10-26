(uiop:define-package #:cl-telegram-bot2/generics
  (:use #:cl)
  (:export
   #:process
   #:on-state-activation
   #:on-state-deletion
   #:on-result))
(in-package #:cl-telegram-bot2/generics)


(defgeneric process (bot-or-state object)
  (:documentation "This method is called by when processing a single update.
                   It is called multiple times on different parts of an update.
                   Whole pipeline looks like that:

                   For each update we call:
                     process(bot, update)
                     process(actor-state, update)
                   "))


(defmethod process (bot-or-state object)
  "By default, processing does nothing"
  (log:warn "No PROCESS method for processing objects of ~A type by ~A."
            (type-of object)
            (type-of bot-or-state))
  (values))


(defmethod process :around (bot-or-state object)
  "By default, processing does nothing"
  (log:error "Calling PROCESS method for processing objects of ~A type by ~A."
            (type-of object)
            (type-of bot-or-state))
  (call-next-method))



(defgeneric on-state-activation (state)
  (:documentation "This method is called when chat actor's state is changed to a given STATE.

                   Such hook can be used to send some prompt to the user.
                   ")
  (:method ((state t))
    "By default, nothing happens on activation."
    (values))
  
  (:method :around ((state t))
    (log:error "Calling ON-STATE-ACTIVATION method for processing object of ~A type."
               (type-of state))
    (call-next-method)))


(defgeneric on-state-deletion (state)
  (:documentation "This method is called when chat actor's state is returned from a given STATE back to the previous state.

                   The method is called only when state is removed from the stack. When a new state is added to the stack,
                   this method will not be called for a previous state.

                   Such hook can be used to hide a keyboard or to delete temporary messages.
                   ")
  (:method ((state t))
    "By default, nothing happens on deactivation."
    (values))

  (:method :around ((state t))
    (log:error "Calling ON-STATE-DELETION method for processing object of ~A type."
               (type-of state))
    (call-next-method)))


(defgeneric on-result (state result)
  (:documentation "This method is called when some state exits and returns a result using BACK function.")
  
  (:method ((state t) (result t))
    "By default, nothing happens for state processing."
    (values))

  (:method :around ((state t) result)
    (log:error "Calling ON-RESULT method for processing object of ~A type and result ~A."
               (type-of state)
               result)
    (call-next-method)))
