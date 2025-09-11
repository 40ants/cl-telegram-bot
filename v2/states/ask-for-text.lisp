(uiop:define-package #:cl-telegram-bot2/states/ask-for-text
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/generics
                #:process-state
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/state
                #:callback-query-handlers
                #:validate-on-deletion-arg
                #:base-state)
  (:import-from #:cl-telegram-bot2/states/base
                #:state-var)
  (:import-from #:cl-telegram-bot2/pipeline
                #:back)
  (:import-from #:cl-telegram-bot2/high
                #:reply)
  (:import-from #:cl-telegram-bot2/api
                #:inline-keyboard-markup
                #:message-text
                #:update-message)
  (:import-from #:str
                #:trim)
  (:import-from #:serapeum
                #:dict
                #:->
                #:soft-list-of)
  (:import-from #:cl-telegram-bot2/action
                #:action)
  (:import-from #:cl-telegram-bot2/state
                #:state)
  (:import-from #:cl-telegram-bot2/debug/diagram/generics
                #:get-slots)
  (:import-from #:cl-telegram-bot2/debug/diagram/slot
                #:slot)
  (:import-from #:cl-telegram-bot2/state-with-commands
                #:state-with-commands-mixin)
  (:import-from #:cl-ppcre
                #:count-matches)
  (:import-from #:cl-telegram-bot2/match
                #:matchp
                #:matcher)
  (:import-from #:cl-telegram-bot2/matchers/regex
                #:regex-matcher)
  (:import-from #:cl-telegram-bot2/workflow
                #:workflow-blocks)
  (:export #:ask-for-text
           #:prompt
           #:var-name
           #:on-success
           #:on-validation-error
           #:validation-regex))
(in-package #:cl-telegram-bot2/states/ask-for-text)


(defparameter *default-var-name* "result")


(defclass ask-for-text (state)
  ((prompt :initarg :prompt
           :type string
           :reader prompt)
   (prompt-keyboard :initarg :prompt-keyboard
                    :type (or null inline-keyboard-markup)
                    :reader prompt-keyboard)
   (matcher :initarg :matcher
            :type matcher
            :reader text-matcher)
   (var-name :initarg :to
             :initform *default-var-name*
             :type string
             :reader var-name)
   (on-success :initarg :on-success
               :initform nil
               :type workflow-blocks
               :reader on-success)
   (on-validation-error :initarg :on-validation-error
                        :initform nil
                        :type workflow-blocks
                        :reader on-validation-error)))


(-> ask-for-text (string
                  &key
                  (:prompt-keyboard (or null inline-keyboard-markup))
                  (:to string)
                  (:regex string)
                  (:on-success workflow-blocks)
                  (:on-validation-error workflow-blocks)
                  (:on-deletion workflow-blocks)
                  (:on-callback-query callback-query-handlers)
                  (:vars (or null hash-table))))

(defun ask-for-text (prompt &key
                              prompt-keyboard
                              (to *default-var-name*)
                              (regex ".*")
                              on-success
                              on-validation-error
                              on-deletion
                              on-callback-query
                              vars)

  (make-instance 'ask-for-text
                 :prompt prompt
                 :prompt-keyboard prompt-keyboard
                 :matcher (regex-matcher regex)
                 :to to
                 :on-success (uiop:ensure-list
                              on-success)
                 :on-validation-error (uiop:ensure-list
                                       on-validation-error)
                 :on-deletion (validate-on-deletion-arg on-deletion)
                 :on-callback-query on-callback-query
                 :vars (or vars
                           (dict))))


(defmethod on-state-activation ((state ask-for-text))
  (reply (prompt state)
         :reply-markup (prompt-keyboard state))
  (values))


(defmethod process-state ((bot t) (state ask-for-text) update)
  (let* ((message
           (update-message
            update))
         (text
           (when message
             (message-text message))))
    
    (cond
      (text
       (let ((trimmed (trim text)))
         (cond
           ((matchp (text-matcher state) trimmed)
            (setf (state-var state
                             (var-name state))
                  trimmed)
            
            (process-state bot
                           (on-success state)
                           update))
           (t
            (process-state bot
                           (on-validation-error state)
                           update)))))
      (t
       ;; To make callback queries handler work,
       ;; we need to call method of the parent class
       (call-next-method)))))


(defmethod get-slots ((state ask-for-text))
  (append
   (loop for slot-name in (list
                           'on-success
                           'on-validation-error)
         collect
         (slot (string-downcase slot-name)
               (slot-value state slot-name)))
   (call-next-method)))
