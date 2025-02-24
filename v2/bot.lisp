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
  (:export #:defbot))
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
                  :reader initial-state)))


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
         (default-initargs
           (when initial-state
             (list :initial-state
                   initial-state))))
    `(progn
       (defclass ,name ,base-classes
         ,slots
         ;; ,@options
         (:default-initargs ,@default-initargs))

       (defun ,(alexandria:symbolicate 'make- name) (token &rest args)
         (apply 'make-instance
                ',name
                :token token
                args)))))


(defmethod initialize-instance :after ((bot bot) &key &allow-other-keys)
  (with-accessors ((token         token)
                   (file-endpoint file-endpoint)
                   (api-uri       api-uri)) bot
    (setf (slot-value bot 'endpoint)
          (concatenate 'string api-uri "bot" token "/")
          (slot-value bot 'file-endpoint)
          (concatenate 'string api-uri "file/" "bot" token "/"))))


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
