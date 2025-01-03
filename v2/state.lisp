(uiop:define-package #:cl-telegram-bot2/state
  (:use #:cl)
  (:import-from #:serapeum
                #:fmt
                #:->
                #:pretty-print-hash-table
                #:dict
                #:soft-list-of)
  (:import-from #:cl-telegram-bot2/action
                #:call-if-action
                #:action)
  (:import-from #:cl-telegram-bot2/generics
                #:process
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/term/back
                #:back)
  (:import-from #:print-items
                #:print-items
                #:print-items-mixin)
  (:import-from #:cl-telegram-bot2/state-with-commands
                #:state-commands
                #:command
                #:state-with-commands-mixin)
  (:import-from #:cl-telegram-bot2/states/base
                #:base-state)
  (:import-from #:cl-telegram-bot2/workflow
                #:workflow-block
                #:workflow-blocks)
  (:import-from #:cl-telegram-bot2/utils
                #:call-if-needed
                #:call-with-one-or-zero-args)
  (:import-from #:cl-telegram-bot2/vars
                #:*current-state*)
  (:import-from #:cl-telegram-bot2/callback
                #:callback-handlers
                #:callback-data
                #:callback)
  (:import-from #:cl-telegram-bot2/debug/diagram/generics
                #:render-handlers
                #:to-text
                #:get-slots)
  (:import-from #:cl-telegram-bot2/debug/diagram/group
                #:group-name
                #:group-slots
                #:sort-slots-and-groups
                #:group)
  (:import-from #:cl-telegram-bot2/debug/diagram/slot
                #:slot-handlers
                #:slot-name
                #:slot)
  (:import-from #:cl-telegram-bot2/debug/diagram/utils
                #:obj-id
                #:on-after-object
                #:after-object
                #:render-objects-link)
  (:export #:state
           #:on-activation
           #:on-update
           #:on-result
           #:on-callback-query
           #:on-web-app-data
           #:callback-query-handlers
           #:validate-on-deletion-arg
           #:on-deletion))
(in-package #:cl-telegram-bot2/state)


(deftype callback-query-handlers ()
  "Type of ON-CALLBACK-QUERY argument of the STATE class."
  '(soft-list-of callback))


(defclass state (state-with-commands-mixin base-state)
  ((on-activation :initarg :on-activation
                  :type workflow-blocks
                  :initform nil
                  :reader on-activation)
   (on-update :initarg :on-update
              :type workflow-blocks
              :initform nil
              :reader on-update)
   (on-deletion :initarg :on-deletion
                :type workflow-blocks
                :initform nil
                :reader on-deletion
                :documentation "Result of these handlers is ignored, but they can be used for side-effects.

                                Generic-function cl-telegram-bot2/generics:on-state-deletion will be
                                called on these handlers.")
   (on-result :initarg :on-result
              :type workflow-blocks
              :initform nil
              :reader on-result)
   (on-callback-query :initarg :on-callback-query
                      :type callback-query-handlers
                      :initform nil
                      :reader on-callback-query)
   (on-web-app-data :initarg :on-web-app-data
                    :type workflow-blocks
                    :initform nil
                    :reader on-web-app-data)))


(defmethod print-items append ((state state))
  (macrolet ((handler (accessor-name)
               `(when (,accessor-name state)
                  (list (list (alexandria:make-keyword ',accessor-name)
                              " ~A = ~S"
                              (string-downcase ',accessor-name)
                              (,accessor-name state))))))
    (append
     (handler on-activation)
     (handler on-update)
     (handler on-deletion)
     (handler on-result)
     (handler on-callback-query)
     (handler on-web-app-data))))


(-> validate-on-deletion-arg ((or null
                                  workflow-block
                                  workflow-blocks))
    (values workflow-blocks &optional))

(defun validate-on-deletion-arg (on-deletion)
  "Validates if argument is valid for passing as :ON-DELETION argument to the state constructor.

   It also normalizes an argument and return it as a list of workflow blocks."
  (let ((on-deletion (uiop:ensure-list on-deletion)))
    (loop for block in on-deletion
          unless (typep block
                        '(or action symbol))
            do (error "Blocks of type ~S can't be used as handlers for state deletion."
                      (type-of block)))
    (values on-deletion)))


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
           (:on-deletion (or null
                             workflow-block
                             workflow-blocks))
           (:on-result (or null
                           workflow-block
                           workflow-blocks))
           (:on-callback-query callback-query-handlers)
           (:on-web-app-data (or null
                                 workflow-block
                                 workflow-blocks)))
    (values state &optional))

(defun state (on-activation &key id commands on-update on-deletion on-result on-callback-query on-web-app-data)
  (make-instance 'state
                 :id id
                 :commands (uiop:ensure-list commands)
                 :on-activation (uiop:ensure-list on-activation)
                 :on-update (uiop:ensure-list on-update)
                 :on-deletion (validate-on-deletion-arg on-deletion)
                 :on-result (uiop:ensure-list on-result)
                 :on-callback-query on-callback-query
                 :on-web-app-data (uiop:ensure-list on-web-app-data)))


(defmethod on-state-activation ((state state))
  (loop for obj in (on-activation state)
        thereis (typecase obj
                  (symbol
                     (on-state-activation
                      (call-if-needed obj)))
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
       (loop for callback in (on-callback-query state)
             for expected-data = (callback-data callback)
             for workflow-blocks = (callback-handlers callback)
             when (string= callback-data expected-data)
               do (return
                    ;; PROCESS should be repeated for actions and states
                    ;; from this list until we'll find a final action like BACK
                    ;; or a state to switch to.

                    ;; NOTE:
                    ;; PROCESS works differently when it is processing
                    ;; a list - it only executes actions and returns states as is.
                    ;; So here we should ensure a list is passed:
                    (process (uiop:ensure-list workflow-blocks)
                             update))))
      ;; Otherwise call an ON-UPDATE action.
      (t
       (let* ((message (cl-telegram-bot2/api:update-message update))
              (web-app-data (cl-telegram-bot2/api:message-web-app-data message)))
         (cond
           (web-app-data
            (process (on-web-app-data state)
                     web-app-data))
           (t
            (process (on-update state)
                     update))))))))


(defmethod process ((items list) update)
  (loop for obj in items
        thereis (etypecase obj
                  (symbol
                     (let ((maybe-other-action
                             (call-with-one-or-zero-args obj update)
                             ;; (cond
                             ;;   ((fboundp obj)
                             ;;    (case (arity obj)
                             ;;      (0
                             ;;         (funcall obj))
                             ;;      (1
                             ;;         ;; If function accepts a single argument,
                             ;;         ;; then we call it with update object.
                             ;;         ;; This way objects like web-app-data
                             ;;         ;; could be processed.
                             ;;         (funcall obj update))
                             ;;      (otherwise
                             ;;         (error "Unable to process ~A because function ~S requires ~A arguments."
                             ;;                update
                             ;;                obj
                             ;;                (arity obj)))))
                             ;;   (t
                             ;;    (error "Symbol ~S should be funcallble."
                             ;;           obj)))
                             ))
                       (when maybe-other-action
                         (process maybe-other-action
                                  update))))
                  (action
                     (process obj update))
                  ;; Here is a little kludge,
                  ;; PROCESS is only called once on the current action
                  ;; and if it returns a list, then we don't need
                  ;; to call PROCESS on the state in the list again,
                  ;; because this is the state to switch to, not to
                  ;; process the UPDATE.
                  ;;
                  ;; QUESTION:
                  ;; Probably these should be separate functions
                  ;; like PROCESS-UPDATE and PROCESS-ACTIONS?
                  (base-state
                     obj)
                  (back
                     obj))))



(defmethod process ((item symbol) update)
  (cond
    ((null item)
     ;; We don't need to do anything to process NIL items.
     (values))
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


(defmethod cl-telegram-bot2/generics:on-state-deletion ((state state))
  (cl-telegram-bot2/generics:on-state-deletion (on-deletion state)))


(defmethod cl-telegram-bot2/generics:on-state-deletion ((workflow-blocks list))
  "Processing list of actions."
  ;; Only actions and symbols are accepted as handlers for state deletion.
  (loop for block in workflow-blocks
        do (typecase block
             (action
                (cl-telegram-bot2/generics:on-state-deletion block))
             (symbol
                (funcall block))
             ;; There should be a check in the constructor,
             ;; but I'll leave this additional check here:
             (t
                (error "Blocks of type ~S can't be used as handlers for state deletion."
                       (type-of block)))))
  (values))


(defmethod cl-telegram-bot2/generics:on-result ((state state) result)
  (cl-telegram-bot2/generics:on-result (on-result state)
                                       result))


(defmethod cl-telegram-bot2/generics:on-result ((items list) result)
  (loop for obj in items
        thereis (etypecase obj
                  (symbol
                     ;; Here we apply on-result again to the
                     ;; function result because it can return
                     ;; another action which should be processed
                     ;; in it's turn
                     (let ((maybe-other-action
                             (call-with-one-or-zero-args obj result)
                             ;; (cond
                             ;;   ((fboundp obj)
                             ;;    (case (arity obj)
                             ;;      (0
                             ;;         (funcall obj))
                             ;;      (1
                             ;;         ;; If function accepts a single argument,
                             ;;         ;; then we call it with update object.
                             ;;         ;; This way objects like web-app-data
                             ;;         ;; could be processed.
                             ;;         (funcall obj result))
                             ;;      (otherwise
                             ;;         (error "Unable to process ~A because function ~S requires ~A arguments."
                             ;;                result
                             ;;                obj
                             ;;                (arity obj)))))
                             ;;   (t
                             ;;    (error "Symbol ~S should be funcallble."
                             ;;           obj)))
                             ))
                       (when maybe-other-action
                         (cl-telegram-bot2/generics:on-result
                          maybe-other-action
                          result))))
                  (action
                     (cl-telegram-bot2/generics:on-result obj result))
                  (base-state
                     obj)
                  (back
                     obj))))


(defmethod get-slots ((state state))
    (list*
     (group "Commands"
            (state-commands state))
     (group "Callbacks"
            (on-callback-query state))
     (loop for slot-name in (list
                             'on-activation
                             'on-update
                             'on-deletion
                             'on-result
                             'on-web-app-data)
           collect
           (slot (string-downcase slot-name)
                 (slot-value state slot-name)))))

