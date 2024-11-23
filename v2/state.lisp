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


(deftype callback-query-handlers ()
  '(serapeum:soft-alist-of string
    (or 
     workflow-block
     workflow-blocks)))


(defclass state (state-with-commands-mixin base-state)
  ((on-activation :initarg :on-activation
                  :type workflow-blocks
                  :reader on-activation)
   (on-update :initarg :on-update
              :type workflow-blocks
              :reader on-update)
   (on-result :initarg :on-result
              :type workflow-blocks
              :reader on-result)
   (on-callback-query :initarg :on-callback-query
                      :type callback-query-handlers
                      :reader on-callback-query)))


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
                           workflow-blocks))
           (:on-callback-query callback-query-handlers))
    (values state &optional))

(defun state (on-activation &key id commands on-update on-result on-callback-query)
  (make-instance 'state
                 :id id
                 :commands (uiop:ensure-list commands)
                 :on-activation (uiop:ensure-list on-activation)
                 :on-update (uiop:ensure-list on-update)
                 :on-result (uiop:ensure-list on-result)
                 :on-callback-query on-callback-query))


(defmethod on-state-activation ((state state))
  (loop for obj in (on-activation state)
        thereis (typecase obj
                  (action
                   (on-state-activation obj))
                  (t
                   obj))))


(defmethod process ((state state) update)
  (let* ((callback (cl-telegram-bot2/api:update-callback-query update))
         (callback-data
           (when callback
             (cl-telegram-bot2/api:callback-query-data callback))))
    (cond
      ;; If user pushed an inline keyboard button, then we'll try to
      ;; find a handler for it:
      (callback-data
       (loop for (expected-value . workflow-blocks) in (on-callback-query state)
             when (string= callback-data expected-value)
               do (return (process workflow-blocks update))))
      ;; Otherwise call an ON-UPDATE action.
      (t
       (process (on-update state)
                update)))))


(defmethod process ((items list) update)
  (loop for obj in items
        thereis (etypecase obj
                  (symbol
                     (cond
                       ((fboundp obj)
                        (process (funcall obj)
                                 update))
                       (t
                        (error "Symbol ~S should be funcallble."
                               obj))))
                  (action
                     (process obj update))
                  (base-state
                     obj)
                  (back
                     obj))))



(defmethod process ((item symbol) update)
  (cond
    ((fboundp item)
     ;; NOTE: Not sure if we need to pass update to the
     ;; funcall here. Probably we should make it accessible
     ;; using some dynamic variable, because
     ;; some callbacks might be called when update is not available
     ;; yet, for example, on state activation.
     (process (funcall item)
              update))
    (t
     (error "Symbol ~S should be funcallable to process update."
            item))))


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