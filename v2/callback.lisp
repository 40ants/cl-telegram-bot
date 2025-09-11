(uiop:define-package #:cl-telegram-bot2/callback
  (:use #:cl)
  (:import-from #:alexandria
                #:required-argument)
  (:import-from #:serapeum
                #:->)
  (:import-from #:cl-telegram-bot2/workflow
                #:workflow-block
                #:workflow-blocks)
  (:import-from #:cl-telegram-bot2/debug/diagram/generics
                #:to-text
                #:render-handlers)
  (:import-from #:cl-telegram-bot2/debug/diagram/utils
                #:render-handlers-inner
                #:obj-id)
  (:import-from #:cl-telegram-bot2/match
                #:matcher)
  (:import-from #:cl-telegram-bot2/matchers/string
                #:string-matcher)
  (:export #:callback
           #:callback-matcher
           #:callback-handlers))
(in-package #:cl-telegram-bot2/callback)


(defclass callback ()
  ((matcher :initarg :matcher
            :type matcher
            :initform (required-argument "DATA is required argument for CALLBACK class.")
            :reader callback-matcher)
   (handlers :initarg :handlers
             :type workflow-blocks
             :initform (required-argument "HANDLERS is required argument for CALLBACK class.")
             :reader callback-handlers))
  (:documentation "Representation of callback handlers to be triggered on given callback data."))


(-> callback ((or string matcher)
              (or workflow-block
                  workflow-blocks))
    (values callback &optional))

(defun callback (string-or-matcher handlers)
  (let ((handlers (uiop:ensure-list handlers)))
    (unless handlers
      (error "Callback should have at least one handler."))
    (make-instance 'callback
                   :matcher (etypecase string-or-matcher
                              (string
                               (string-matcher string-or-matcher))
                              (matcher
                               string-or-matcher))
                   :handlers handlers)))


(defmethod render-handlers ((obj callback))
  (render-handlers-inner (callback-handlers obj)
                         (obj-id obj)))


;; NOTE: previously we used callback-data as slot name,
;; but then this data slot was replaced with matcher.
;; (defmethod slot-name ((obj callback))
;;   (cl-telegram-bot2/callback:callback-data obj))


(defmethod to-text ((command callback))
  (to-text
   (callback-handlers command)))
