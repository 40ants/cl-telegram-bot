(uiop:define-package #:cl-telegram-bot2/pipeline
  (:use #:cl)
  (:import-from #:log4cl)
  ;; (:import-from #:cl-telegram-bot/message
  ;;               #:*current-bot*
  ;;               #:make-message)
  ;; (:import-from #:cl-telegram-bot/network
  ;;               #:make-request)
  (:import-from #:cl-telegram-bot2/bot
                #:initial-state-class
                #:api-uri
                #:token
                #:get-last-update-id
                #:bot)
  ;; (:import-from #:cl-telegram-bot/pipeline
  ;;               #:process)
  ;; (:import-from #:cl-telegram-bot/callback
  ;;               #:make-callback)
  (:import-from #:anaphora
                #:it
                #:acond)
  ;; (:import-from #:cl-telegram-bot/envelope
  ;;               #:edited-message
  ;;               #:channel-post
  ;;               #:edited-channel-post)
  ;; (:import-from #:cl-telegram-bot/chat
  ;;               #:get-chat)
  ;; (:import-from #:cl-telegram-bot/payments
  ;;               #:make-successful-payment
  ;;               #:make-pre-checkout-query)
  ;; (:import-from #:cl-telegram-bot/user
  ;;               #:get-user-info)
  (:import-from #:cl-telegram-bot2/vars
                #:*current-state*
                #:*current-bot*
                #:*current-user*)
  (:import-from #:cl-telegram-bot2/generics
                #:on-state-activation
                #:process)
  (:import-from #:cl-telegram-bot2/spec
                #:telegram-object
                #:*token*
                #:*api-url*)
  (:import-from #:serapeum
                #:->
                #:fmt)
  (:import-from #:sento.actor
                #:*state*)
  (:import-from #:cl-telegram-bot2/vars
                #:*current-chat*)
  (:import-from #:closer-mop
                #:slot-definition-type)
  (:import-from #:alexandria
                #:required-argument)
  (:import-from #:cl-telegram-bot2/states/base
                #:state-id
                #:base-state)
  (:import-from #:cl-telegram-bot2/term/back
                #:back-to-id
                #:back
                #:back-to
                #:back-to-nth-parent)
  (:import-from #:cl-telegram-bot2/utils
                #:deep-copy))
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

;; (defclass update ()
;;   ((id :initarg :id
;;        :reader get-update-id)
;;    (payload :initarg :payload
;;             :reader get-payload)
;;    (raw-data :initarg :raw-data
;;              :reader get-raw-data)))


;; (defun make-update (data)
;;   (let ((update-id (getf data :|update_id|))
;;         (payload
;;           (acond
;;             ((getf data :|message|)
;;              (cond
;;                ((getf it :|successful_payment|))
;;                (t
;;                 (make-message it))))
;;             ((getf data :|edited_message|)
;;              (make-instance 'edited-message
;;                             :message (make-message it)))
;;             ((getf data :|channel_post|)
;;              (make-instance 'channel-post
;;                             :message (make-message it)))
;;             ((getf data :|edited_channel_post|)
;;              (make-instance 'edited-channel-post
;;                             :message (make-message it)))
;;             ((getf data :|callback_query|)
;;              (make-callback *current-bot*
;;                             it))
;;             ((getf data :|pre_checkout_query|)
;;              (make-pre-checkout-query *current-bot*
;;                                       it))
;;             ((getf data :|successful_payment|)
;;              (make-successful-payment *current-bot*
;;                                       it))
;;             (t
;;              (log:warn "Received not supported update type"
;;                        data)
;;              nil))))
;;     (make-instance 'update
;;                    :id update-id
;;                    :payload payload
;;                    :raw-data data)))


(defun get-updates (bot &key limit timeout)
  "https://core.telegram.org/bots/api#getupdates"
  (let* ((current-id (get-last-update-id bot))
         (*api-url* (api-uri bot))
         (*token* (token bot))
         (updates (cl-telegram-bot2/api:get-updates :limit limit
                                                    :offset current-id
                                                    :timeout timeout)))
    
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
                    (process bot update)
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
                                             slot-name))))
    
    ;; (or (get-chat (cl-telegram-bot2/api:update-message update))
    ;;     (get-chat (cl-telegram-bot2/api:update-message update))
    ;;     (get-chat (cl-telegram-bot2/api:update-edited-message update))
    ;;     (get-chat (cl-telegram-bot2/api:update-callback-query update)))
    )
  
  (:method ((object cl-telegram-bot2/api:chat))
    object
    ;; (cl-telegram-bot2/api:message-chat object)
    ))


(defgeneric get-user (object)
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
                                              slot-name)))))
    
    ;; (or (get-chat (cl-telegram-bot2/api:update-message update))
    ;;     (get-chat (cl-telegram-bot2/api:update-message update))
    ;;     (get-chat (cl-telegram-bot2/api:update-edited-message update))
    ;;     (get-chat (cl-telegram-bot2/api:update-callback-query update)))
    )
  
  ;; (:method ((object cl-telegram-bot2/api:chat))
  ;;   object
  ;;   ;; (cl-telegram-bot2/api:message-chat object)
  ;;   )
  )


(defmethod process ((bot bot) (update cl-telegram-bot2/api:update))
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


(defun get-or-create-chat-actor (bot chat-id)
  (flet ((local-process-chat-update (update)
           (let ((*current-bot* bot)
                 (*token* (cl-telegram-bot2/bot::token bot))
                 (*print-readably*
                   ;; bordeaux-threads sets this var to T and this breaks logging
                   ;; our objects. So we have to turn this off.
                   nil))
             (process-chat-update update))))
    (let* ((actor-name (fmt "chat-~A" chat-id))
           (system (cl-telegram-bot2/bot::actors-system bot))
           (actor (or (first
                       (sento.actor-context:find-actors
                        system
                        actor-name))
                      (let* ((initial-state
                               (etypecase (initial-state-class bot)
                                 (symbol
                                  (make-instance
                                   (initial-state-class bot)))
                                 (base-state
                                  ;; Here we need to copy a state
                                  ;; to prevent results sharing between different chats
                                  (deep-copy
                                   (initial-state-class bot)))))
                             (probably-new-state
                               (on-state-activation initial-state))
                             (state-stack
                               (if (and probably-new-state
                                        (not (eql initial-state
                                                  probably-new-state)))
                                   (list probably-new-state
                                         initial-state)
                                   (list initial-state))))
                        (log:info "Creating new actor with" actor-name)
                        (sento.actor-context:actor-of
                         system
                         :name actor-name
                         :receive #'local-process-chat-update
                         :state state-stack)))))
      (values actor))))


(defun process-chat-update (update)
  (handler-bind ((serious-condition #'invoke-debugger))
    (log:info "Processing chat update"
              update)
    (let* ((*current-state* (car *state*))
           (*current-chat* (get-chat update))
           (*current-user* (get-user update))
           (new-state (process *current-state* update)))
      
      (labels ((probably-switch-to-new-state (new-state)
                 (let ((*current-state* (car *state*)))
                   (when (and new-state
                              (not (eql *current-state* new-state)))
                     
                     (cond
                       ;; If next state is a symbol, we need to instantiate it:
                       ((symbolp new-state)
                        (probably-switch-to-new-state
                         (make-instance new-state)))
                       ((typep new-state 'back)
                        (let* ((result (cl-telegram-bot2/term/back:result new-state)))

                          (multiple-value-bind (states-to-delete new-stack)
                              (split-stack new-state *state*)
                            
                            (unless new-stack
                              (error "Unexpected behaviour - no states left in the stack."))

                            (let ((state-to-which-return (car new-stack)))
                          
                              (setf *state*
                                    new-stack)

                              (log:debug "New state is ~A" (car *state*))
                          
                              (loop for state-to-delete in states-to-delete
                                    do (cl-telegram-bot2/generics:on-state-deletion state-to-delete))
                          
                              (let ((on-result-return-value
                                      (cl-telegram-bot2/generics:on-result state-to-which-return
                                                                           ;; Result might be empty
                                                                           result)))
                                (probably-switch-to-new-state on-result-return-value))))))
                       ((typep new-state 'base-state)
                        (setf *state*
                              (list* new-state
                                     *state*))

                        (log:debug "New state is ~A" (car *state*))
                        
                        (probably-switch-to-new-state
                         (on-state-activation new-state)))
                       (t
                        (break)
                        (log:warn "Object ~S is not of BASE-STATE class and can't be pushed to the states stack."
                                  new-state)))))))
        (probably-switch-to-new-state new-state)))
    (values)))

;; (defmethod get-chat ((update update))
;;   (get-chat (get-payload update)))


;; (defmethod get-user-info ((update update))
;;   (get-user-info (get-payload update)))
