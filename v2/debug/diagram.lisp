(uiop:define-package #:cl-telegram-bot2/debug/diagram
  (:use #:cl)
  (:import-from #:40ants-plantuml)
  (:import-from #:cl-telegram-bot2/actions/send-text
                #:send-text)
  (:import-from #:cl-telegram-bot2/bot
                #:initial-state)
  (:import-from #:cl-telegram-bot2/generics
                #:process-state)
  (:import-from #:cl-telegram-bot2/actions/send-photo
                #:send-photo)
  (:import-from #:cl-telegram-bot2/debug/diagram/vars
                #:*state-to-name*
                #:*id-to-state*
                #:*name-to-state*
                #:*obj-to-id*
                #:*id-to-obj*
                #:*current-map-id*
                #:*current-obj-id*
                #:*diagram-stream*
                #:*on-after-object*
                #:*objects-created*)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:cl-telegram-bot2/debug/diagram/utils
                #:obj-id
                #:on-after-object
                #:with-on-after
                #:render-handlers-inner
                #:render-mapslot-value)
  (:import-from #:cl-telegram-bot2/debug/diagram/generics
                #:to-text
                #:render-handlers
                #:render-handler-link)
  (:import-from #:cl-telegram-bot2/action
                #:action)
  (:import-from #:cl-telegram-bot2/vars
                #:*current-bot*)
  (:export #:render-workflow-diagram))
(in-package #:cl-telegram-bot2/debug/diagram)


(defmethod render-handler-link ((symbol symbol))
  (render-mapslot-value
   "call"
   (symbol-name
    symbol)))


(defmethod render-handlers ((obj list))
  (render-handlers-inner obj (obj-id obj)))


(defmethod to-text :around ((obj t))
  (let ((*current-obj-id* (obj-id obj)))
    (call-next-method)
    (on-after-object *current-obj-id*)))


(defmethod to-text ((symbol symbol))
  ;; NOTE: Decided to not render blocks for funcs and actions.
  ;; Probably will need to show again if I decide to render
  ;; output arrows from functions.
  (values))


(defmethod to-text ((objects list))
  (loop for obj in objects
        do (to-text obj)))


(defun workflow-to-text (bot &key left-to-right)
  (with-output-to-string (*diagram-stream*)
    (with-on-after
      (let ((*state-to-name* (make-hash-table))
            (*name-to-state* (make-hash-table :test 'equal))
            (*id-to-state* (make-hash-table :test 'equal))
            (*obj-to-id* (make-hash-table))
            (*id-to-obj* (make-hash-table :test 'equal)))
        (format *diagram-stream*
                "@startuml~%")
        (when left-to-right
          (format *diagram-stream*
                  "left to right direction~%"))
        (to-text (cl-telegram-bot2/bot::initial-state bot))
        (format *diagram-stream*
                ;; remove @unlinked
                "@enduml~%")))))


(defclass render-workflow-diagram (action)
  ())


(defun render-workflow-diagram ()
  (make-instance 'render-workflow-diagram))


(defmethod process-state ((bot t) (action render-workflow-diagram) (update t))
  (handler-case
      (let ((workflow (workflow-to-text *current-bot*)))
        (uiop:with-temporary-file (:pathname temp-file :keep t)
          (40ants-plantuml:render workflow
                                  temp-file)
          
          (send-photo temp-file)))
    (serious-condition (err)
      (send-text (fmt "~A"
                      err)))))


