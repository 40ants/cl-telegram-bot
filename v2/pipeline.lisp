(uiop:define-package #:cl-telegram-bot2/pipeline
  (:use #:cl)
  (:import-from #:log)
  (:import-from #:cl-telegram-bot2/api)
  (:import-from #:cl-telegram-bot2/bot
                #:initial-state
                #:api-uri
                #:token
                #:get-last-update-id
                #:bot)
  (:import-from #:cl-telegram-bot2/vars
                #:*default-special-bindings*
                #:*current-state*
                #:*current-bot*
                #:*current-user*)
  (:import-from #:cl-telegram-bot2/generics
                #:on-state-activation
                #:process-update
                #:process-state)
  (:import-from #:cl-telegram-bot2/spec
                #:telegram-object
                #:*token*
                #:*api-url*)
  (:import-from #:serapeum
                #:->
                #:fmt)
  (:import-from #:sento.actor-cell
                #:*state*)
  (:import-from #:sento.actor-context)
  (:import-from #:cl-telegram-bot2/vars
                #:*current-chat*)
  (:import-from #:closer-mop
                #:slot-definition-type)
  (:import-from #:cl-telegram-bot2/states/base
                #:state-id
                #:base-state)
  (:import-from #:cl-telegram-bot2/term/back
                #:back-to-id
                #:back
                #:back-to
                #:back-to-nth-parent)
  (:import-from #:cl-telegram-bot2/utils
                #:deep-copy)
  (:import-from #:sento.actor)
  (:import-from #:alexandria
                #:named-lambda))
(in-package cl-telegram-bot2/pipeline)



(defgeneric split-stack (back-command state-stack)
  (:documentation "Should return two values: a list of states to be deleted because we rolled back and a list of states in a new state stack.")
  
  (:method ((back-command back) (state-stack list))
    (let ((new-stack (cdr state-stack))
          (states-to-delete (list (car state-stack))))
      (values states-to-delete
              new-stack)))
  
  (:method ((back-command back-to) (state-stack list))
    (loop with needed-state-class = (cl-telegram-bot2/term/back:state-class back-command)
          for rest-states on state-stack
          for current-state = (car rest-states)
          until (typep current-state
                       needed-state-class)
          collect current-state into states-to-delete
          finally (return (values states-to-delete
                                  rest-states))))
  
  (:method ((back-command back-to-nth-parent) (state-stack list))
    (loop for rest-states on state-stack
          for current-state = (car rest-states)
          for n upto (cl-telegram-bot2/term/back:parent-number back-command)
          collect current-state into states-to-delete
          finally (return (values states-to-delete
                                  rest-states))))
  
  (:method ((back-command back-to-id) (state-stack list))
    (loop with id-to-search = (cl-telegram-bot2/term/back:parent-id back-command)
          for rest-states on state-stack
          for current-state = (car rest-states)
          until (string= (state-id current-state)
                         id-to-search)
          collect current-state into states-to-delete
          finally (return (values states-to-delete
                                  rest-states)))))


(defun get-updates (bot &key limit timeout)
  "https://core.telegram.org/bots/api#getupdates"
  (let* ((current-id (get-last-update-id bot))
         (*api-url* (api-uri bot))
         (*token* (token bot))
         (updates (progn
                    (log:debug "Requesting updates" current-id)
                    (cl-telegram-bot2/api:get-updates :limit limit
                                                      :offset current-id
                                                      :timeout timeout))))
    
    (when updates
      (let ((max-id (reduce #'max
                            updates
                            :key #'cl-telegram-bot2/api:update-update-id)))
        ;; In original cl-telegram-bot a bug was here, because
        ;; it saved update's id only the first time, and after that,
        ;; just incremented that value
        (log:debug "Setting new" max-id)
        (setf (get-last-update-id bot)
              (+ max-id 1))))
    
    (values updates)))


;; Generics

(defgeneric process-updates (bot)
  (:documentation "By default, this method starts an infinite loop and fetching new updates using long polling."))


(defmethod process-updates ((bot t))
  "Starts inifinite loop to process updates using long polling."
  (loop with *current-bot* = bot
    do (loop for update in (restart-case
                               (get-updates bot
                                            :timeout 10)
                             (continue-processing (&optional delay)
                               :report "Continue processing updates from Telegram"
                               (when delay
                                 (sleep delay))
                               ;; Return no updates
                               (values)))
             do (restart-case
                    (process-update-in-actor bot update)
                  (continue-processing (&optional delay)
                    :report "Continue processing updates from Telegram"
                    (when delay
                      (sleep delay)))))))


(defvar *last-update* nil)


(defgeneric get-chat (object)
  (:method ((object null))
    nil)
  
  (:method ((object telegram-object))
    (loop with slots = (closer-mop:class-slots (class-of object))
          for slot in slots
          for slot-name = (closer-mop:slot-definition-name slot)
          thereis (and (subtypep (slot-definition-type slot) 'telegram-object)
                       (slot-boundp object
                                    slot-name)
                       (get-chat (slot-value object
                                             slot-name)))))
  
  (:method ((object cl-telegram-bot2/api:chat))
    object))


(defgeneric get-user (object)
  (:documentation "Returns a use associated with object, author of the message.

                   If found, it will be CL-TELEGRAM-BOT2/API:USER, otherwise - NIL.")
  
  (:method ((object null))
    nil)
  
  (:method ((object telegram-object))
    (loop with slots = (closer-mop:class-slots (class-of object))
          for slot in slots
          for slot-name = (closer-mop:slot-definition-name slot)
          thereis (or
                   (and (string-equal slot-name
                                      "from")
                        (subtypep (slot-definition-type slot)
                                  'cl-telegram-bot2/api:user)
                        (slot-boundp object
                                     slot-name)
                        (slot-value object
                                    slot-name))
                   (and (subtypep (slot-definition-type slot)
                                  'telegram-object)
                        (slot-boundp object
                                     slot-name)
                        (get-user (slot-value object
                                              slot-name)))))))


(defun process-update-in-actor (bot update)
  "By default, just calls `process' on the payload."
  (log:debug "Processing update" update)

  ;; TODO: remove this debugging code
  (setf *last-update* update)

  (let* ((*token* (cl-telegram-bot2/bot::token bot))
         (*current-chat* (get-chat update))
         (*current-user* (get-user update)))
    (cond
      (*current-chat*
       (let* ((chat-id (cl-telegram-bot2/api:chat-id *current-chat*))
              (chat-actor (get-or-create-chat-actor bot chat-id)))
         (sento.actor:ask chat-actor update)))
      ((cl-telegram-bot2/api:update-pre-checkout-query update)
       (cl-telegram-bot2/generics:on-pre-checkout-query
        bot
        (cl-telegram-bot2/api:update-pre-checkout-query update)))
      (t
       (error "Processing of ~S is not implemented yet"
              update)))))


(defun %compute-special-bindings (bindings)
  (remove-duplicates bindings
                     :from-end t :key #'car))

(defun %establish-dynamic-env (function special-bindings)
  "Return a closure that binds the symbols in SPECIAL-BINDINGS and calls
FUNCTION."
  (let* ((bindings (%compute-special-bindings special-bindings))
         (specials (mapcar #'car bindings))
         (values (mapcar (lambda (f) (eval (cdr f))) bindings)))
    (named-lambda %call-with-dynamic-env (&rest args)
      (progv specials values
        (apply function args)))))


(defun get-or-create-chat-actor (bot chat-id)
  (flet ((local-process-update (update)
           (let ((*current-bot* bot)
                 (*token* (cl-telegram-bot2/bot::token bot))
                 (*print-readably*
                   ;; bordeaux-threads sets this var to T and this breaks logging
                   ;; our objects. So we have to turn this off.
                   nil))
             (process-update bot update))))
    (let* ((actor-name (fmt "chat-~A" chat-id))
           (system (cl-telegram-bot2/bot::actors-system bot))
           (actor (or (first
                       (sento.actor-context:find-actors
                        system
                        actor-name))
                      (let* ((initial-state
                               (etypecase (initial-state bot)
                                 (symbol
                                  (make-instance
                                   (initial-state bot)))
                                 (base-state
                                  ;; Here we need to copy a state
                                  ;; to prevent results sharing between different chats
                                  (deep-copy
                                   (initial-state bot)))))
                             ;; TODO: here we call on-state activation
                             ;; only once, however we should do this
                             ;; until  new state is returned from
                             ;; the call:
                             (state-stack
                               (probably-switch-to-new-state
                                initial-state
                                nil)))
                        (log:info "Creating new actor with" actor-name)
                        (sento.actor-context:actor-of
                         system
                         :name actor-name
                         :receive (%establish-dynamic-env #'local-process-update
                                                          *default-special-bindings*)
                         :state state-stack)))))
      (values actor))))


(defun probably-switch-to-new-state (new-state state-stack)
  "Returns two values, probably new stack as first value and a flag. If flag is NIL, then the state stack was not changed."
  (let ((*current-state* (car state-stack)))
    (cond
      ((and new-state
            (not (eql *current-state* new-state)))
       (cond
         ;; If next state is a symbol, we need to instantiate it
         ;; using either as a function or as a class name:
         ((symbolp new-state)
          (probably-switch-to-new-state
           (cond
             ((fboundp new-state)
              (funcall new-state))
             (t
              (make-instance new-state)))
           state-stack))
         ;; Processing BACK actions:
         ((typep new-state 'back)
          (let* ((result (cl-telegram-bot2/term/back:result new-state))
                 ;; Result can be an fbound symbol and in this case we
                 ;; neet to call it while we didn't change the current state.
                 ;; This way a custom code may be used to calculate
                 ;; result before it will be passed to some previous state.
                 (result (cond
                           ((and (symbolp result)
                                 (fboundp result))
                            (funcall result))
                           (t
                            result))))

            (multiple-value-bind (states-to-delete new-stack)
                (split-stack new-state state-stack)
               
              (unless new-stack
                (error "Unexpected behaviour - no states left in the stack."))

              (loop for state-to-delete in states-to-delete
                    do (let ((*current-state* state-to-delete))
                         (cl-telegram-bot2/generics:on-state-deletion state-to-delete)))
              
              (let ((*current-state* (car new-stack))
                    (*state* new-stack))
                (log:debug "New state is ~A" *current-state*)
                 
                (let* ((on-result-return-value
                         ;; We need to call ON-RESULT handler
                         ;; when the state to which we have returned
                         ;; is bound to current-state, because
                         ;; handler can send new messages and
                         ;; we need them to be saved inside the message
                         ;; to which we've returned:
                         (cl-telegram-bot2/generics:on-result *current-state*
                                                              ;; Result might be empty
                                                              result)))
                  (probably-switch-to-new-state on-result-return-value
                                                new-stack))))))
         ((typep new-state 'base-state)
          (log:debug "New state is ~A" new-state)
           
          (let ((*state* (list* new-state
                                state-stack))
                (*current-state* new-state))
            (probably-switch-to-new-state
             (on-state-activation new-state)
             *state*)))
         (t
          (log:warn "Object ~S is not of BASE-STATE class and can't be pushed to the states stack."
                    new-state)
          (values state-stack
                  nil))))
      (t
       (values state-stack
               nil)))))


(defmethod process-update ((bot bot) (update cl-telegram-bot2/api:update))
  (handler-bind ((serious-condition #'invoke-debugger))
    (log:info "Processing chat update"
              update)
    (let* ((*current-state* (car *state*))
           (*current-chat* (get-chat update))
           (*current-user* (get-user update))
           (new-state (process-state bot *current-state* update)))

      (setf *state*
            (probably-switch-to-new-state new-state *state*)))
    (values)))

