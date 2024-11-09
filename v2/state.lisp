(uiop:define-package #:cl-telegram-bot2/state
  (:use #:cl)
  (:import-from #:serapeum
                #:->
                #:pretty-print-hash-table
                #:dict
                #:soft-list-of)
  (:import-from #:cl-telegram-bot2/action
                #:action)
  (:import-from #:cl-telegram-bot2/generics
                #:process
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/term/back
                #:back)
  (:import-from #:sento.actor
                #:*state*)
  (:import-from #:print-items
                #:print-items
                #:print-items-mixin)
  (:import-from #:cl-telegram-bot2/state-with-commands
                #:command
                #:state-with-commands-mixin)
  (:import-from #:cl-telegram-bot2/states/base
                #:base-state)
  (:import-from #:cl-telegram-bot2/workflow
                #:workflow-block
                #:workflow-blocks)
  (:export #:state))
(in-package #:cl-telegram-bot2/state)


(defclass state (state-with-commands-mixin base-state)
  ((on-activation :initarg :on-activation
                  :type workflow-blocks
                  :reader on-activation)
   (on-update :initarg :on-update
              :type workflow-blocks
              :reader on-update)
   (on-result :initarg :on-result
              :type workflow-blocks
              :reader on-result)))


(defmethod print-items append ((state state))
  (append
   (when (on-activation state)
     (list (list :on-activation
                 " on-activation = ~S"
                 (on-activation state))))
   (when (on-update state)
     (list (list :on-update
                 " on-update = ~S"
                 (on-update state))))))


(-> state ((or workflow-block
               workflow-blocks)
           &key
           (:id (or null string))
           (:commands (or null
                          command
                          (soft-list-of command)))
           (:on-update (or null
                           workflow-block
                           workflow-blocks))
           (:on-result (or null
                           workflow-block
                           workflow-blocks)))
    (values state &optional))

(defun state (on-activation &key id commands on-update on-result)
  (make-instance 'state
                 :id id
                 :commands (uiop:ensure-list commands)
                 :on-activation (uiop:ensure-list on-activation)
                 :on-update (uiop:ensure-list on-update)
                 :on-result (uiop:ensure-list on-result)))


(defmethod on-state-activation ((state state))
  (loop for obj in (on-activation state)
        thereis (typecase obj
                  (action
                   (on-state-activation obj))
                  (t
                   obj))))


(defmethod process ((state state) update)
  (process (on-update state)
           update))


(defmethod process ((items list) update)
  (loop for obj in items
        thereis (etypecase obj
                  (action
                   (process obj update))
                  (base-state
                   obj)
                  (back
                   obj))))


(defmethod cl-telegram-bot2/generics:on-result ((state state) result)
  (cl-telegram-bot2/generics:on-result (on-result state)
                                       result))


(defmethod cl-telegram-bot2/generics:on-result ((items list) result)
  (loop for obj in items
        thereis (etypecase obj
                  (action
                   (cl-telegram-bot2/generics:on-result obj result))
                  (base-state
                   obj)
                  (back
                   obj))))
