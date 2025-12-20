(uiop:define-package #:cl-telegram-bot2/bot
  (:use #:cl)
  (:import-from #:alexandria
                #:assoc-value
                #:required-argument
                #:once-only)
  (:import-from #:sento.actor-system)
  (:import-from #:sento.actor-context)
  (:import-from #:cl-telegram-bot2/api)
  (:import-from #:cl-telegram-bot2/vars
                #:*current-bot*)
  (:import-from #:cl-telegram-bot2/states/base
                #:base-state)
  (:import-from #:serapeum
                #:soft-list-of)
  (:import-from #:closer-mop
                #:slot-definition-name
                #:class-direct-slots)
  (:import-from #:str
                #:replace-all)
  (:export #:defbot
           #:bot
           #:get-last-update-id
           #:token
           #:api-uri
           #:get-endpoint
           #:file-endpoint
           #:bot-info
           #:debug-mode
           #:sent-commands-cache
           #:actors-system
           #:initial-state
           #:bot-allowed-updates
           #:get-default-allowed-updates))
(in-package #:cl-telegram-bot2/bot)



(defclass bot ()
  ((id
    :documentation "Update id"
    :initform 0
    :accessor get-last-update-id)
   (token
    :initarg :token
    :documentation "Bot token given by BotFather"
    :accessor token
    :initform nil)
   (api-uri
    :initarg  :api-uri
    :initform "https://api.telegram.org/"
    :accessor api-uri)
   (endpoint
    :initarg :endpoint
    :reader get-endpoint
    :documentation "HTTPS endpoint")
   (file-endpoint
    :initarg :file-endpoint
    :accessor file-endpoint
    :documentation "HTTPS file-endpoint"
    :initform nil)
   (bot-info :initform nil
             :documentation "This slot will be filled with CL-TELEGRAM-BOT/USER:USER object on first access using a call to CL-TELEGRAM-BOT/USER:GET-ME function."
             :reader bot-info)
   (debug-mode
    :initform nil
    :initarg :debug-mode
    :accessor debug-mode
    :documentation "When debug mode is T, then interactive debugger will be called on each error.")
   (sent-commands-cache :initform nil
                        :documentation "Command processing code will use this cache to update commands list on the server
                                        when a new method for CL-TELEGRAM-BOT/ENTITIES/COMMAND:ON-COMMAND generic-function is defined.

                                        This slot is for internal use."
                        :accessor sent-commands-cache)
   (actors-system :initform nil
                  :type (or null sento.actor-system::actor-system)
                  :accessor actors-system)
   (initial-state :initarg :initial-state
                  :initform (required-argument ":INITIAL-STATE is required argument.")
                  :type (or symbol
                            base-state)
                  :reader initial-state)
   (allowed-updates :initarg :allowed-updates
                    :initform nil
                    :type (soft-list-of string)
                    :reader bot-allowed-updates
                    :documentation "If given, then only listed updates will be received by the bot. Or you might pass ADDITIONAL-ALLOWED-UPDATES argument to get some update types additionally to default types. According to TG API documentation, these update types aren't received by default: chat_member, message_reaction, and message_reaction_count."))
  (:default-initargs :additional-allowed-updates nil))


(defmacro defbot (name base-classes &optional slots &rest options)
  "Use this macro to define a class of your Telegram bot.

   Each bot has a state machine inside. The simplest bot has only one state:

   ```
   (defbot test-bot ()
     ()
     (:initial-state
      (state (send-text \"Hello world!\"))))
   ```

   This bot will green each who activates it.

   To learn more about bot states and actions see CL-TELEGRAM-BOT-DOCS/STATES::@STATES-AND-ACTIONS section.
"
  (unless (member 'bot base-classes)
    (setf base-classes
          (append base-classes
                  (list 'bot))))
  (let* ((initial-state
           (first
            (assoc-value options :initial-state)))
         (explicit-default-initargs
           (assoc-value options :default-initargs))
         (default-initargs
           (append
            explicit-default-initargs
            
            (when initial-state
              (list :initial-state
                    initial-state)))))
    `(progn
       (defclass ,name ,base-classes
         ,slots
         ;; ,@options
         (:default-initargs ,@default-initargs))

       (defun ,(alexandria:symbolicate 'make- name) (token &rest args &key allowed-updates additional-allowed-updates &allow-other-keys)
         (declare (ignore allowed-updates additional-allowed-updates))
         (apply 'make-instance
                ',name
                :token token
                args)))))


(defparameter *update-types-excluded-from-default-list*
  '("chat_member" "message_reaction" "message_reaction_count"))


(defun get-default-allowed-updates ()
  "Returns a list of strings suitable to pass as ALLOWED-UPDATES argument to the bot constructor."
  (loop for slot in (class-direct-slots (find-class 'cl-telegram-bot2/api::update))
        for slot-name = (slot-definition-name slot)
        for slot-name-as-tg = (replace-all "-" "_" (string-downcase slot-name))
        unless (member slot-name-as-tg *update-types-excluded-from-default-list*
                       :test #'string=)
          collect slot-name-as-tg))


(defmethod initialize-instance :around ((bot bot) &rest rest &key allowed-updates additional-allowed-updates &allow-other-keys)
  (let* ((allowed-updates
           (cond
             ((and allowed-updates
                   additional-allowed-updates)
              (error "Only ALLOWED-UPDATES or ADDITIONAL-ALLOWED-UPDATES should be used."))
             (allowed-updates allowed-updates)
             (additional-allowed-updates
              (loop with result = (get-default-allowed-updates)
                    for method in additional-allowed-updates
                    do (pushnew method result :test #'string-equal)
                    finally (return result)))))
         (new-args (list* :allowed-updates
                          allowed-updates
                          rest))
         (result (apply #'call-next-method bot new-args)))
    (with-accessors ((token         token)
                     (file-endpoint file-endpoint)
                     (api-uri       api-uri)) bot
      (setf (slot-value bot 'endpoint)
            (concatenate 'string api-uri "bot" token "/")
            (slot-value bot 'file-endpoint)
            (concatenate 'string api-uri "file/" "bot" token "/")))
    (values result)))


(defmethod print-object ((bot bot) stream)
  (print-unreadable-object
      (bot stream :type t)
    (format stream
            "id=~A" (get-last-update-id bot))))


(defun start-actors (bot)
  (unless (actors-system bot)
    (setf (actors-system bot)
          (sento.actor-system:make-actor-system)))
  (values))


(defun stop-actors (bot)
  (when (actors-system bot)
    (sento.actor-context:shutdown (actors-system bot))
    (setf (actors-system bot)
          nil))
  (values))


(defmethod bot-info :around ((bot bot))
  (with-slots (bot-info)
      bot
    (unless bot-info
      (setf bot-info
            (cl-telegram-bot2/api::get-me)))
    (values bot-info)))


(defun bot-name ()
  (cl-telegram-bot2/api:user-username
   (bot-info *current-bot*)))
