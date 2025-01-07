(uiop:define-package #:cl-telegram-bot2/states/ask-for-choice
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/generics
                #:on-state-deletion
                #:process
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/state
                #:base-state)
  (:import-from #:cl-telegram-bot2/states/base
                #:state-var)
  (:import-from #:cl-telegram-bot2/pipeline
                #:back)
  (:import-from #:cl-telegram-bot2/high
                #:collect-sent-messages
                #:reply)
  (:import-from #:cl-telegram-bot2/api
                #:delete-message
                #:chat-id
                #:message-message-id
                #:update
                #:message-text
                #:update-message)
  (:import-from #:str
                #:trim)
  (:import-from #:serapeum
                #:->
                #:soft-list-of)
  (:import-from #:cl-telegram-bot2/action
                #:action)
  (:import-from #:cl-telegram-bot2/utils
                #:call-if-needed)
  (:import-from #:cl-telegram-bot2/vars
                #:*current-chat*)
  (:import-from #:cl-telegram-bot2/actions/send-text
                #:send-text)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:cl-telegram-bot2/workflow
                #:workflow-block
                #:workflow-blocks)
  (:import-from #:cl-telegram-bot2/state
                #:state)
  (:import-from #:cl-telegram-bot2/debug/diagram/generics
                #:get-slots)
  (:import-from #:cl-telegram-bot2/debug/diagram/slot
                #:slot)
  (:export #:ask-for-choice
           #:prompt
           #:var-name
           #:buttons
           #:on-success
           #:on-wrong-user-message
           #:delete-messages-p
           #:delete-wrong-user-messages-p
           #:message-ids-to-delete))
(in-package #:cl-telegram-bot2/states/ask-for-choice)


(defparameter *default-var-name* "result")


;; To allow this state process global commands, we need
;; to inherit it from state-with-commands-mixin.
(defclass ask-for-choice (state)
  ((prompt :initarg :prompt
           :type (or string symbol)
           :reader prompt)
   (var-name :initarg :to
             :initform *default-var-name*
             :type string
             :reader var-name)
   (buttons :initarg :buttons
            :initform nil
            :type list
            :reader buttons)
   (on-success :initarg :on-success
               :initform nil
               :type workflow-blocks
               :reader on-success)
   (on-wrong-user-message :initarg :on-wrong-user-message
                          :initform nil
                          :type workflow-blocks
                          :reader on-wrong-user-message)
   (delete-messages :initarg :delete-messages
                    :initform t
                    :type boolean
                    :documentation "Delete message with the keyboard and all warning messages when the choice was made or a new state was added to the stack."
                    :reader delete-messages-p)
   (delete-wrong-user-messages :initarg :delete-wrong-user-messages
                               :initform t
                               :type boolean
                               :documentation "Delete usual user messages which he might send by a mistake."
                    :reader delete-wrong-user-messages-p)
   (message-ids-to-delete :initform nil
                          :accessor message-ids-to-delete)))


(-> ask-for-choice ((or string symbol) (soft-list-of string)
                    &key
                    (:to string)
                    (:delete-messages boolean)
                    (:delete-wrong-user-messages boolean)
                    (:on-success (or workflow-block
                                     workflow-blocks))
                    (:on-wrong-user-message (or workflow-block
                                                workflow-blocks)))
    (values ask-for-choice &optional))


(defun ask-for-choice (prompt buttons &key (to *default-var-name*)
                                           (delete-messages t)
                                           (delete-wrong-user-messages t)
                                           on-success
                                           (on-wrong-user-message
                                            (send-text "Please push one of the buttons.")))
  (unless buttons
    (error "Please, specify at least one button."))
  
  (make-instance 'ask-for-choice
                 :prompt prompt
                 :buttons buttons
                 :to to
                 :delete-messages delete-messages
                 :delete-wrong-user-messages delete-wrong-user-messages
                 :on-success (uiop:ensure-list
                              on-success)
                 :on-wrong-user-message (uiop:ensure-list
                                         on-wrong-user-message)))


(defmethod on-state-activation ((state ask-for-choice))
  (let ((message
          (reply (call-if-needed (prompt state))
                 :reply-markup (make-instance 'cl-telegram-bot2/api:inline-keyboard-markup
                                              :inline-keyboard
                                              (list
                                               (loop for choice in (buttons state)
                                                     collect (make-instance 'cl-telegram-bot2/api:inline-keyboard-button
                                                                            :text choice
                                                                            :callback-data choice)))))))
    (when (delete-messages-p state)
      (push (message-message-id message)
            (message-ids-to-delete state))))
  (values))


(defun delete-created-messages (state)
  (when (message-ids-to-delete state)
    (cl-telegram-bot2/api:delete-messages (chat-id *current-chat*)
                                          (message-ids-to-delete state))
    (setf (message-ids-to-delete state)
          nil))
  (values))


(defmethod process ((state ask-for-choice) (update update))
  (let* ((callback (cl-telegram-bot2/api:update-callback-query update))
         (callback-data
           (when callback
             (cl-telegram-bot2/api:callback-query-data callback))))

    (cond
      (callback-data
       (setf (state-var state
                        (var-name state))
             callback-data)
      
       (prog1 (process (on-success state)
                       update)
         (delete-created-messages state)))
      (t
       ;; Here we are saving user message to delete it later
       (when (and (update-message update)
                  (delete-wrong-user-messages-p state))
         (push (message-message-id
                (update-message update))
               (message-ids-to-delete state)))
       
       (multiple-value-bind (sent-messages result)
           (collect-sent-messages
             (process (on-wrong-user-message state)
                      update))
         (when (delete-messages-p state)
           (loop for message in sent-messages
                 do (push (message-message-id message)
                          (message-ids-to-delete state))))
         (values result))))))


(defmethod on-state-deletion ((state ask-for-choice))
  (delete-created-messages state)
  (values))


(defmethod get-slots ((state ask-for-choice))
  (append
   (loop for slot-name in (list
                           'on-success
                           'on-wrong-user-message)
         collect
         (slot (string-downcase slot-name)
               (slot-value state slot-name)))
   (call-next-method)))
