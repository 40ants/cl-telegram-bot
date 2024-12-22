(uiop:define-package #:cl-telegram-bot2/states/ask-for-number
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/generics
                #:process
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/state
                #:base-state)
  (:import-from #:cl-telegram-bot2/states/base
                #:state-var)
  (:import-from #:cl-telegram-bot2/pipeline
                #:back)
  (:import-from #:cl-telegram-bot2/high
                #:reply)
  (:import-from #:cl-telegram-bot2/api
                #:message-text
                #:update-message)
  (:import-from #:str
                #:trim)
  (:import-from #:serapeum
                #:soft-list-of)
  (:import-from #:cl-telegram-bot2/action
                #:action)
  (:import-from #:cl-telegram-bot2/state-with-commands
                #:state-with-commands-mixin)
  (:export #:ask-for-number))
(in-package #:cl-telegram-bot2/states/ask-for-number)


(defparameter *default-var-name* "result")


;; To allow this state process global commands, we need
;; to inherit it from state-with-commands-mixin.
(defclass ask-for-number (state-with-commands-mixin base-state)
  ((prompt :initarg :prompt
           :type string
           :reader prompt)
   (var-name :initarg :to
             :initform *default-var-name*
             :type string
             :reader var-name)
   (on-success :initarg :on-success
               :initform nil
               :type (soft-list-of
                      (or base-state
                          action
                          back))
               :reader on-success)
   (on-validation-error :initarg :on-validation-error
                        :initform nil
                        :type (soft-list-of
                               (or base-state
                                   action
                                   back))
                        :reader on-validation-error)))


(defun ask-for-number (prompt &key (to *default-var-name*)
                                   on-success
                                   on-validation-error)
  (make-instance 'ask-for-number
                 :prompt prompt
                 :to to
                 :on-success (uiop:ensure-list
                              on-success)
                 :on-validation-error (uiop:ensure-list
                              on-validation-error)))


(defmethod on-state-activation ((state ask-for-number))
  (reply (prompt state))
  (values))


(defmethod process ((state ask-for-number) update)
  (let* ((message
           (update-message
            update))
         (text
           (when message
             (message-text message))))
    
    (cond
      (text
       (let ((parsed (ignore-errors
                      (parse-integer (trim text)))))
         (cond
           (parsed
            (setf (state-var state
                             (var-name state))
                  parsed)
            
            (process (on-success state)
                     update))
           (t
            (process (on-validation-error state)
                     update)))))
      (t
       (values)))))
