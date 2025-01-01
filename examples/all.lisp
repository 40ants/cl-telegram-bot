(uiop:define-package #:cl-telegram-bot2-examples
  (:use #:cl)
  (:nicknames #:cl-telegram-bot2-examples/all)
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
  (:import-from #:serapeum
                #:defvar-unbound
                #:fmt
                #:push-end)
  (:import-from #:alexandria
                #:once-only))
(in-package #:cl-telegram-bot2-examples)


(defclass all-examples-bot (cl-telegram-bot2/bot::bot)
  ())


(defvar *bot* nil)


(defvar *diagram-stream*)


(defvar *obj-to-id*)
(defvar *id-to-obj*)
(defvar *state-to-name*)
(defvar *name-to-state*)
(defvar *id-to-state*)
(defvar *current-map-id*)
(defvar *current-obj-id*)
(defvar *send-text-limit* 30)

(defun state-name (state)
  (let ((id (cl-telegram-bot2/states/base:state-id state))
        (name (gethash state *state-to-name*)))
    (flet ((store-name (name)
             (when id
               (setf (gethash id *id-to-state*)
                     state))
             (setf (gethash state *state-to-name*)
                   name)
             (setf (gethash name *name-to-state*)
                   state)
             (values name)))
      (cond
        (name
         (values name))
        (id
         (let ((name (format nil "state '~A'"
                             id)))
           (store-name name)))
        (t
         (loop for idx upfrom 1
               for possible-name = (format nil "state #~A"
                                           idx)
               when (null (gethash possible-name *name-to-state*))
                 do (return (store-name possible-name))))))))


(defun obj-id (obj)
  (let ((obj-id (gethash obj *obj-to-id*)))
    (cond
      (obj-id
       (values obj-id))
      (t
       (loop for idx upfrom 1
             for possible-id = (format nil "obj_~A"
                                       idx)
             when (null (gethash possible-id *id-to-obj*))
               do (return (progn
                            (setf (gethash obj *obj-to-id*)
                                  possible-id)
                            (setf (gethash possible-id *id-to-obj*)
                                  obj)
                            (values possible-id))))))))


(defvar-unbound *objects-created*
  "A list of object ids created by to-text generic-function.")


(defvar-unbound *on-after-object*
  "A hash-table to register callbacks to be called after some object was rendered.")


(defmacro with-on-after (&body body)
  `(cond
     ((boundp '*objects-created*)
      (error "Nested call of WITH-ON-AFTER is prohibited."))
     (t
      (let ((*objects-created* nil)
            (*on-after-object* (make-hash-table :test 'equal)))
        ,@body))))


(defmacro after-object ((obj-id) &body body)
  (once-only (obj-id)
    `(let ((already-created (member ,obj-id *objects-created*
                                    :test #'equal)))
       (cond
         (already-created
          ,@body)
         (t
          (push-end
           (let ((dynamic-bindings (when (boundp '*current-map-id*)
                                     (list (cons '*current-map-id*
                                                 *current-map-id*)))))
             (lambda ()
               (progv
                   (mapcar #'car dynamic-bindings)
                   (mapcar #'cdr dynamic-bindings)
                 ,@body)))
           (gethash ,obj-id *on-after-object*))))
       (values))))


(defun on-after-object (obj-id)
  (loop for callback in (gethash obj-id *on-after-object*)
        do (funcall callback))
  
  (push obj-id
        *objects-created*)
  
  (values))


(defun render-objects-link (from to)
  (format *diagram-stream*
          "~A ----> ~A~%"
          from
          to))


(defmacro render-map ((name id) &body body)
  (once-only (name id)
    `(let ((*current-map-id* ,id))
       (format *diagram-stream*
               "map \"~A\" as ~A {~%"
               ,name
               ,id)

       ,@body
    
       (format *diagram-stream*
               "}~%"))))


(defun render-mapslot-link (from to)
  (format *diagram-stream*
          "~A *---> ~A~%"
          from
          to))

(defun render-mapslot-value-with-link (key value link-to-obj-id)
  (format *diagram-stream*
          "~A => ~A~%"
          key
          value)
  ;; We don't want this piece to render inside the current
  ;; map and we need it to render only after the linked object
  ;; will be rendered. Otherwise PlantUML will complain
  ;; it is now know about such object.
  (after-object (*current-obj-id*)
    (after-object (link-to-obj-id)
      (format *diagram-stream*
              "~A::~A ---> ~A~%"
              *current-map-id*
              key
              link-to-obj-id))))


(defun render-mapslot-value (key value)
  (format *diagram-stream*
          "~A => ~A~%"
          key
          value))


(defgeneric render-handler-link (handler)
  (:method ((action cl-telegram-bot2/term/back:back-to-id))

    (let* ((parent-id (cl-telegram-bot2/term/back:parent-id action))
           (state (or (gethash parent-id
                               *id-to-state*)
                      (error "Unable to find state with id ~S."
                             parent-id))))
      (render-handler-link state)))
  
  (:method ((state cl-telegram-bot2/states/base:base-state))
    (let ((state-id (obj-id state))
          (state-name (state-name state)))
      (render-mapslot-value-with-link
       "goto"
       state-name
       (fmt "~A_slots" state-id))))

  (:method ((symbol symbol))
    (render-mapslot-value
     "call"
     (symbol-name
      symbol)))
  
  (:method ((action cl-telegram-bot2/actions/send-text:send-text))
    (render-mapslot-value
     "action"
     (fmt "~A\\n~A"
          (class-name (class-of action))
          (let ((text (cl-telegram-bot2/actions/send-text:text action)))
            (etypecase text
              (string
                 (str:shorten *send-text-limit*
                              text))
              (symbol
                 text))))))
  
  (:method ((action cl-telegram-bot2/action:action))
    (render-mapslot-value
     "action"
     (symbol-name
      (class-name (class-of action))))))


(defun render-handlers-inner (handlers obj-id)
  (let ((handlers-id (format nil "~A_handlers"
                             obj-id))
        (handlers (uiop:ensure-list handlers)))
    (when handlers
      (render-map ("handlers" handlers-id)

        (loop for handler in handlers
              do (render-handler-link handler))))))


(defgeneric render-handlers (object)

  (:method ((obj list))
    (render-handlers-inner obj (obj-id obj)))
  
  (:method ((slot slot))
    (render-handlers (slot-handlers slot)))
  
  (:method ((group group))
    (loop for slot in (group-slots group)
          do (render-handlers slot)))
  
  (:method ((obj cl-telegram-bot2/state-with-commands:command))
    (render-handlers-inner (cl-telegram-bot2/state-with-commands:command-handler obj)
                           (obj-id obj)))
  
  (:method ((obj cl-telegram-bot2/callback:callback))
    (render-handlers-inner (cl-telegram-bot2/callback:callback-handlers obj)
                           (obj-id obj))))


(defgeneric slot-name (obj)
  (:method ((obj cl-telegram-bot2/state-with-commands:command))
    (cl-telegram-bot2/state-with-commands:command-name obj))
  
  (:method ((obj cl-telegram-bot2/callback:callback))
    (cl-telegram-bot2/callback:callback-data obj)))


(defun render-command-link (command)
  (let ((handlers-id (format nil "~A_handlers"
                             (obj-id command))))
    (render-mapslot-link
     (cl-telegram-bot2/state-with-commands:command-name command)
     handlers-id)))


(defun render-callback-link (callback)
  (let ((handlers-id (format nil "~A_handlers"
                             (obj-id callback))))
    (render-mapslot-link
     (callback-data callback)
     handlers-id)))


(defclass slot ()
  ((name :initarg :name
         :reader slot-name)
   (value :initarg :value
          :reader slot-handlers)))


(defclass group ()
  ((name :initarg :name
         :reader group-name)
   (slots :initarg :slots
          :reader group-slots)))


(defun slot (name value)
  (when value
    (make-instance 'slot
                   :name name
                   :value value)))

(defun group (name slots)
  (when slots
    (make-instance 'group
                   :name name
                   :slots slots)))


(defun sort-slots-and-groups (objs)
  (sort (copy-list objs)
        (lambda (left right)
          (cond
            ((and (typep left 'slot)
                  (typep right 'slot))
             (string< (slot-name left)
                      (slot-name right)))
            ((and (typep left 'slot)
                  (typep right 'group))
             t)
            ((and (typep left 'group)
                  (typep right 'slot))
             nil)
            ((and (typep left 'group)
                  (typep right 'group))
             (string< (group-name left)
                      (group-name right)))))))


(defun to-slot (obj)
  (slot (slot-name obj)
        obj))


(defgeneric get-slots (state)
  (:method ((state cl-telegram-bot2/state:state))
    (list*
     (group "Commands"
            (mapcar #'to-slot
                    (cl-telegram-bot2/state-with-commands:state-commands state)))
     (group "Callbacks"
            (mapcar #'to-slot
                    (cl-telegram-bot2/state:on-callback-query state)))
     (loop for slot-name in (list
                             'cl-telegram-bot2/state:on-activation
                             'cl-telegram-bot2/state:on-update
                             'cl-telegram-bot2/state:on-deletion
                             'cl-telegram-bot2/state:on-result
                             'cl-telegram-bot2/state:on-web-app-data)
           collect
           (slot (string-downcase slot-name)
                 (slot-value state slot-name))))))


(defgeneric to-text (state-or-action)
  (:method :around ((obj t))
    (let ((*current-obj-id* (obj-id obj)))
      (call-next-method)
      (on-after-object *current-obj-id*)))

  ;; NOTE: Decided to not render blocks for funcs and actions.
  ;; Probably will need to show again if I decide to render
  ;; output arrows from functions.
  (:method ((symbol symbol))
    (values)
    ;; (when (fboundp symbol)
    ;;   (format *diagram-stream*
    ;;           "object \"func ~A\" as ~A~%"
    ;;           (symbol-name
    ;;            symbol)
    ;;           (obj-id symbol)))
    )
  
  (:method ((action cl-telegram-bot2/action:action))
    (values)
    ;; (format *diagram-stream*
    ;;         "object \"action ~A\" as ~A~%"
    ;;         (symbol-name
    ;;          (class-name (class-of action)))
    ;;         (obj-id action))
    )

  ;; We don't render back blocks explicintly, replacing the
  ;; with a link between handler in the map and the state\
  (:method ((action cl-telegram-bot2/term/back:back))
    (values)
    ;; (format *diagram-stream*
    ;;         "object \"term ~A\" as ~A~%"
    ;;         (symbol-name
    ;;          (class-name (class-of action)))
    ;;         (obj-id action))
    )
  
  ;; (:method :after ((action cl-telegram-bot2/term/back:back-to-id))
  ;;   (let ((parent-id (cl-telegram-bot2/term/back:parent-id action))
  ;;         (action-id (obj-id action)))
  ;;     (format *diagram-stream*
  ;;             "~A : id = ~S~%"
  ;;             action-id
  ;;             parent-id)))

  (:method ((slot slot))
    (to-text (slot-handlers slot)))

  (:method ((group group))
    (to-text (group-slots group)))
  
  (:method ((objects list))
    (loop for obj in objects
          do (to-text obj)))
  
  (:method ((command cl-telegram-bot2/state-with-commands:command))
    (to-text
     (uiop:ensure-list
      (cl-telegram-bot2/state-with-commands:command-handler command))))
  
  (:method ((command cl-telegram-bot2/callback:callback))
    (to-text
     (cl-telegram-bot2/callback:callback-handlers command)))
  
  
  (:method ((state cl-telegram-bot2/state:state))
    (let* ((name (state-name state))
           (obj-id (obj-id state))
           (slots-id (fmt "~A_slots"
                          obj-id))
           (slots-or-groups (sort-slots-and-groups
                             (remove nil
                                     (get-slots state)))))

      (flet ((add-slot-link (slot-name handlers-id)
               (after-object (slots-id)
                 (render-objects-link 
                  (fmt "\"~A::~A\""
                       slots-id
                       slot-name)
                  handlers-id))))

        ;; Traverse tree of workflow depth first:
        (loop for slot-or-group in slots-or-groups
              do (to-text slot-or-group))

        ;; End of traverse,
        ;; rendering the state itself:
        
        (format *diagram-stream*
                "package ~S as ~A {~%"
                name
                obj-id)

        (loop for slot-or-group in slots-or-groups
              do (render-handlers slot-or-group))
        
        (format *diagram-stream*
                "object \"**state slots**\" as ~A {~%"
                slots-id)

        (flet ((render-slot (slot)
                 (let* ((name (slot-name slot))
                        (obj-id (obj-id (slot-handlers slot)))
                        (handlers-id (fmt "~A_handlers" obj-id)))
                   
                   (format *diagram-stream*
                           "~A~%"
                           name)
                   (add-slot-link name handlers-id))))

          (loop for slot-or-group in slots-or-groups
                do (etypecase slot-or-group
                     (slot
                        (render-slot slot-or-group))
                     (group
                        (format *diagram-stream*
                                ".. ~A ..~%"
                                (group-name slot-or-group))
                        (mapc #'render-slot
                              (sort (copy-list
                                     (group-slots slot-or-group))
                                    #'string<
                                    :key #'slot-name))))))
        
        ;; End of object
        (format *diagram-stream*
                "}~%")

        ;; Output links
        (on-after-object slots-id)
      
        ;; End of package
        (format *diagram-stream*
                "}~%")))))


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


(defun render-workflow-diagram ()
  (setf *bot*
        cl-telegram-bot2/vars::*current-bot*)
  (40ants-plantuml:render "
@startuml
object firstObj
object seconObj
@enduml
"
                          "~/test7.png")
  #P"~/test7.png"
  )


(defun show-menu-buttons ()
  (send-text "Choose an example to run:"
             :reply-markup
             (inline-keyboard
              (list
               (list
                (call-callback "Echo"
                               "open-echo")
                (call-callback "Text Chain"
                               "open-text-chain")
                (call-callback "Calc"
                               "open-calc"))
               (list
                (call-callback "Commands"
                               "open-commands")
                (call-callback "Gallery"
                               "open-gallery")
                (call-callback "Mini-app"
                               "open-mini-app"))
               (list
                (call-callback "Payments"
                               "open-payments"))))))


(defun make-mega-bot (token &rest args)
  (let* ((calc-state (initial-state
                      (cl-telegram-bot2-examples/calc::make-test-bot token)))
         (echo-state (initial-state
                      (cl-telegram-bot2-examples/echo::make-test-bot token)))
         (text-chain-state (initial-state
                            (cl-telegram-bot2-examples/text-chain::make-test-bot token)))
         (commands-state (initial-state
                          (cl-telegram-bot2-examples/commands::make-test-bot token)))
         (gallery-state (initial-state
                         (cl-telegram-bot2-examples/gallery::make-test-bot token)))
         (mini-app-state (initial-state
                          (cl-telegram-bot2-examples/mini-app::make-test-bot token)))
         (payments-state (initial-state
                          (cl-telegram-bot2-examples/payments::make-test-bot token)))
         (mega-state
           (state 'show-menu-buttons
                  :id "megabot-main-menu"
                  :commands (list (global-command "/menu"
                                                  (list (delete-messages)
                                                        (back-to-id "megabot-main-menu"))
                                                  :description "Show menu with all examples.")
                                  (global-command "/debug"
                                                  (send-photo 'render-workflow-diagram)
                                                  :description "Show menu with all examples."))
                  :on-result 'show-menu-buttons
                  :on-callback-query (list (callback "open-echo"
                                                     (list (delete-messages)
                                                           echo-state))
                                           (callback "open-text-chain"
                                                     (list (delete-messages)
                                                           text-chain-state))
                                           (callback "open-calc"
                                                     (list (delete-messages)
                                                           calc-state))
                                           (callback "open-commands"
                                                     (list (delete-messages)
                                                           commands-state))
                                           (callback "open-gallery"
                                                     (list (delete-messages)
                                                           gallery-state))
                                           (callback "open-mini-app"
                                                     (list (delete-messages)
                                                           mini-app-state))
                                           (callback "open-payments"
                                                     (list (delete-messages)
                                                           payments-state))))))
    (apply #'make-instance
           'all-examples-bot
           :token token
           :initial-state mega-state
           args)))


(defmethod on-pre-checkout-query ((bot all-examples-bot) (query pre-checkout-query))
  (answer-pre-checkout-query (pre-checkout-query-id query)
                             t)
  (values))


(defvar *bot* nil)


(defun start (&key (log-level :warn) (debug t))
  (stop)

  (40ants-logging:setup-for-repl :level log-level)

  (unless *bot*
    (setf *bot*
          (make-mega-bot (uiop:getenv "TELEGRAM_TOKEN"))))
  
  (start-polling *bot* :debug debug))


(defun stop ()
  (when *bot*
    (stop-polling *bot*)
    (setf *bot* nil)

    (sleep 1)
    (bt:all-threads)))

