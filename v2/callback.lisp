(uiop:define-package #:cl-telegram-bot2/callback
  (:use #:cl)
  (:import-from #:alexandria
                #:required-argument)
  (:import-from #:serapeum
                #:->)
  (:import-from #:cl-telegram-bot2/workflow
                #:workflow-block
                #:workflow-blocks)
  (:export #:callback
           #:callback-data
           #:callback-handlers))
(in-package #:cl-telegram-bot2/callback)


(defclass callback ()
  ((data :initarg :data
         :type string
         :initform (required-argument "DATA is required argument for CALLBACK class.")
         :reader callback-data)
   (handlers :initarg :handlers
             :type workflow-blocks
             :initform (required-argument "HANDLERS is required argument for CALLBACK class.")
             :reader callback-handlers))
  (:documentation "Representation of callback handlers to be triggered on given callback data."))


(-> callback (string (or workflow-block
                         workflow-blocks))
    (values callback &optional))

(defun callback (data handlers)
  (let ((handlers (uiop:ensure-list handlers)))
    (unless handlers
      (error "Callback should have at least one handler."))
    (make-instance 'callback
                   :data data
                   :handlers handlers)))
