(uiop:define-package #:cl-telegram-bot2/debug/diagram
  (:use #:cl)
  (:import-from #:40ants-plantuml)
  (:import-from #:cl-telegram-bot2/state
                #:state)
  (:import-from #:cl-telegram-bot2/server
                #:stop-polling
                #:start-polling)
  (:import-from #:cl-telegram-bot2/actions/send-text
                #:send-text)
  (:import-from #:cl-telegram-bot2/state-with-commands
                #:global-command
                #:command)
  (:import-from #:cl-telegram-bot2-examples/calc)
  (:import-from #:cl-telegram-bot2-examples/commands)
  (:import-from #:cl-telegram-bot2-examples/gallery)
  (:import-from #:cl-telegram-bot2-examples/payments)
  (:import-from #:cl-telegram-bot2-examples/mini-app)
  (:import-from #:cl-telegram-bot2-examples/echo)
  (:import-from #:cl-telegram-bot2-examples/text-chain)
  (:import-from #:cl-telegram-bot2/high/keyboard
                #:inline-keyboard
                #:call-callback)
  (:import-from #:cl-telegram-bot2/bot
                #:initial-state)
  (:import-from #:cl-telegram-bot2/term/back
                #:back-to-id)
  (:import-from #:cl-telegram-bot2/actions/delete-messages
                #:delete-messages)
  (:import-from #:cl-telegram-bot2/api
                #:pre-checkout-query
                #:pre-checkout-query-id
                #:answer-pre-checkout-query)
  (:import-from #:cl-telegram-bot2/generics
                #:on-pre-checkout-query)
  (:import-from #:cl-telegram-bot2/actions/send-photo
                #:send-photo)
  (:import-from #:cl-telegram-bot2/callback
                #:callback-data
                #:callback)
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
  (:import-from #:alexandria
                #:once-only)
  (:import-from #:serapeum
                #:push-end)
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


(defun workflow-to-text (bot)
  (with-output-to-string (*diagram-stream*)
    (with-on-after
      (let ((*state-to-name* (make-hash-table))
            (*name-to-state* (make-hash-table :test 'equal))
            (*id-to-state* (make-hash-table :test 'equal))
            (*obj-to-id* (make-hash-table))
            (*id-to-obj* (make-hash-table :test 'equal)))
        (format *diagram-stream*
                "@startuml
left to right direction~%")
        (to-text (cl-telegram-bot2/bot::initial-state bot))
        (format *diagram-stream*
                ;; remove @unlinked
                "@enduml~%")))))


;; (defun render-workflow-diagram ()
;;   (setf *bot*
;;         cl-telegram-bot2/vars::*current-bot*)
;;   (40ants-plantuml:render "
;; @startuml
;; object firstObj
;; object seconObj
;; @enduml
;; "
;;                           "~/test7.png")
;;   #P"~/test7.png"
;;   )

