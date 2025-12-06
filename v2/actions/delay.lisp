(uiop:define-package #:cl-telegram-bot2/actions/delay
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/spec
                #:*token*)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:sento.wheel-timer
                #:schedule-once)
  (:import-from #:sento.actor-context
                #:system)
  (:import-from #:sento.actor-system
                #:scheduler)
  (:import-from #:sento.actor
                #:*self*)
  (:export #:delay
           #:cancel-delayed-execution))
(in-package #:cl-telegram-bot2/actions/delay)


(defvar *system* nil
  "We need to store current actor system because during the delayed code execution *self* variable does not point to any actor.")


(defun current-system ()
  (or *system*
      (when *self*
        (system *self*))
      (error "Unable to get current actor system")))


(defun call-delay (seconds thunk)
  (let* ((system (current-system))
         (timer-wheel (scheduler system))
         (token *token*))

    (flet ((wrapped-delayed-thunk ()
             (with-log-unhandled ()
               (let ((*token* token)
                     (*system* system))
                 (funcall thunk)))))

      (schedule-once timer-wheel seconds
                     #'wrapped-delayed-thunk))))


(defmacro delay ((seconds) &body body)
  "This macro allows to call BODY with given delay.

   During the body call current bot's telegram token will be available and you can send messages.

   Retruned value (signature) can be used to cancel delayed code using call to CANCEL-DELAYED-EXECUTION."
  `(flet ((delayed-thunk ()
            ,@body))
     (call-delay ,seconds #'delayed-thunk)))


(defun cancel-delayed-execution (signature)
  "Cancels code execution delayed by DELAY macro."
  (let* ((system (current-system))
         (timer-wheel (scheduler system)))

    (sento.wheel-timer:cancel timer-wheel signature)))
