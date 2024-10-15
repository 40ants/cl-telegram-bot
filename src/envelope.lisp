(uiop:define-package #:cl-telegram-bot/envelope
  (:use #:cl)
  (:import-from #:cl-telegram-bot/pipeline
                #:process)
  (:export #:wrapped-message
           #:envelope
           #:edited-message
           #:channel-post
           #:edited-channel-post
           #:edited-message-p
           #:channel-post-p))
(in-package #:cl-telegram-bot/envelope)


(defvar *wrappers* nil
  "This var will hold a list of wrappers during the call to PROCESS generic-function. It is used by functions CHANNEL-POST-P and EDITED-MESSAGE-P.")


(defclass envelope ()
  ((message :initarg :message
            :reader wrapped-message))
  (:documentation "This is the container for a message. From the type of container we can understand if this message was sent to a channel or maybe edited, etc."))


(defclass edited-message (envelope)
  ()
  (:documentation "This container wraps CL-TELEGRAM-BOT/MESSAGE:MESSAGE when user edits a message."))


(defclass channel-post (envelope)
  ()
  (:documentation "This container wraps CL-TELEGRAM-BOT/MESSAGE:MESSAGE when somebody sends a message to a channel."))


(defclass edited-channel-post (envelope)
  ()
  (:documentation "This container wraps CL-TELEGRAM-BOT/MESSAGE:MESSAGE when somebody edits a message in a channel."))


(defmethod process ((bot t) (envelope envelope))
  "By default, just calls `process' on the wrapped message."
  (log:debug "Processing envelope" envelope)
  (let ((message (wrapped-message envelope))
        (*wrappers* (cons envelope *wrappers*)))
    (process bot message)))


(declaim (ftype (function () boolean)
                channel-post-p))

(defun channel-post-p ()
  "Returns T if current message was posted to a channel."
  (loop for wrapper in *wrappers*
        thereis (or (typep wrapper 'channel-post)
                    (typep wrapper 'edited-channel-post))))


(declaim (ftype (function () boolean)
                edited-message-p))

(defun edited-message-p ()
  "Returns T if current message is an update for existing message in the channel of group chat."
  (loop for wrapper in *wrappers*
        thereis (or (typep wrapper 'edited-message)
                    (typep wrapper 'edited-channel-post))))
