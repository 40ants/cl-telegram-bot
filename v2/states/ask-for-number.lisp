(uiop:define-package #:cl-telegram-bot2/states/ask-for-number
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/generics
                #:process-state
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/state
                #:validate-on-deletion-arg
                #:base-state)
  (:import-from #:cl-telegram-bot2/states/base
                #:generate-state-id
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
  (:import-from #:cl-telegram-bot2/state
                #:state)
  (:import-from #:cl-telegram-bot2/debug/diagram/generics
                #:get-slots)
  (:import-from #:cl-telegram-bot2/debug/diagram/slot
                #:slot)
  (:export #:ask-for-number
           #:prompt
           #:var-name
           #:on-success
           #:on-validation-error))
(in-package #:cl-telegram-bot2/states/ask-for-number)


(defparameter *default-var-name* "result")


;; To allow this state process global commands, we need
;; to inherit it from state-with-commands-mixin.
(defclass ask-for-number (state)
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


(defun ask-for-number (prompt &key
                              (id (generate-state-id))
                              (to *default-var-name*)
                              on-success
                              on-validation-error
                              on-deletion)

  (make-instance 'ask-for-number
                 :id id
                 :prompt prompt
                 :to to
                 :on-success (uiop:ensure-list
                              on-success)
                 :on-validation-error (uiop:ensure-list
                                       on-validation-error)
                 :on-deletion (validate-on-deletion-arg on-deletion)))


(defmethod on-state-activation ((state ask-for-number))
  (reply (prompt state))
  (values))


(defmethod process-state ((bot t) (state ask-for-number) update)
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
            
            (process-state bot
                           (on-success state)
                           update))
           (t
            (process-state bot
                           (on-validation-error state)
                           update)))))
      (t
       (values)))))


(defmethod get-slots ((state ask-for-number))
  (append
   (loop for slot-name in (list
                           'on-success
                           'on-validation-error)
         collect
         (slot (string-downcase slot-name)
               (slot-value state slot-name)))
   (call-next-method)))
