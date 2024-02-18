(uiop:define-package #:cl-telegram-bot/callback
  (:use #:cl)
  (:import-from #:cl-telegram-bot/message
                #:message
                #:get-chat
                #:*current-message*
                #:get-rest-args
                #:get-text
                #:*current-bot*
                #:send-message)
  (:import-from #:cl-telegram-bot/pipeline
                #:process)
  (:import-from #:cl-telegram-bot/chat
                #:get-chat-id)
  (:import-from #:cl-telegram-bot/response-processing
                #:process-response)
  (:export #:callback-data
           #:callback
           #:make-callback
           #:on-callback
           #:callback-id
           #:callback-message
           #:callback-chat))
(in-package #:cl-telegram-bot/callback)


(defclass callback ()
  ((id :initarg :id
       :type string
       :reader callback-id)
   (data :initarg :data
         :type string
         :reader callback-data)
   (message :initarg :message
            :type message
            :reader callback-message)))


(defgeneric on-callback (bot callback)
  (:documentation "Called when user clicks callback button. Second argument is an object of CALLBACK type.")
  (:method ((bot t) (callback t))
    ;; Doing nothing
    (values)))


(defgeneric make-callback (bot callback-data)
  (:documentation "Called when user clicks callback button. Should return an instance of CALLBACK class.

                   Application may override this method to return objects of different callback classes depending on
                   callback-data string. This way it mab be easier to define more specific methods for
                   ON-CALLBACK generic-function.")
  (:method ((bot t) (callback-data t))
    (let ((id (getf callback-data :|id|))
          (data (getf callback-data :|data|))
          (message-data (getf callback-data :|message|)))
      (make-instance 'callback
                     :id id
                     :data data
                     :message (cl-telegram-bot/message:make-message message-data)))))


(defmethod process ((bot t) (callback callback))
  ""
  (log:debug "Processing callback" callback)
  
  (let ((*current-bot* bot)
        (*current-message* callback))
    (handler-case
        (on-callback bot callback)
      (cl-telegram-bot/response-processing:interrupt-processing (condition)
        (declare (ignore condition))
        (log:debug "Interrupting callback processing" callback))))
  (values))


(defgeneric callback-chat (callback)
  (:documentation "Returns a chat from where callback was sent.")
  
  (:method ((callback callback))
    (cl-telegram-bot/message:get-chat (callback-message callback))))
