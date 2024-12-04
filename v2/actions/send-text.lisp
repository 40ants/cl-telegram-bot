(uiop:define-package #:cl-telegram-bot2/actions/send-text
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/action
                #:action)
  (:import-from #:cl-telegram-bot2/generics
                #:on-result
                #:process
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/high
                #:reply)
  (:import-from #:serapeum
                #:soft-list-of
                #:->)
  (:import-from #:cl-telegram-bot2/utils
                #:call-if-needed)
  (:import-from #:cl-telegram-bot2/types
                #:inline-keyboard-buttons)
  (:export #:send-text))
(in-package #:cl-telegram-bot2/actions/send-text)


(defparameter *default-keyboad-type*
  :inline)


(defclass send-text (action)
  ((text :initarg :text
         :type (or string
                   symbol)
         :reader text)
   (buttons :initarg :buttons
            :initform nil
            :type list
            :reader buttons)
   (keyboard-type :initarg :keyboard-type
                  :initform *default-keyboad-type*
                  :type (member :main :inline)
                  :reader keyboard-type)
   (parse-mode :initarg :parse-mode
               :initform nil
               :type (or null string)
               :documentation "Supported values are: \"Markdown\", \"MarkdownV2\" or \"HTML\". Read more about formatting options in the Telegram documentaion: https://core.telegram.org/bots/api#formatting-options"
               :reader parse-mode)))


(-> send-text ((or string symbol)
               &key
               (:buttons inline-keyboard-buttons)
               (:keyboard-type (member :main :inline))
               (:parse-mode (or null string)))
    (values send-text &optional))

(defun send-text (text-or-func-name &key buttons (keyboard-type *default-keyboad-type*) parse-mode)
  (when (and (symbolp text-or-func-name)
             (not (fboundp text-or-func-name)))
    (error "SEND-TEXT waits a text or fbound symbol. ~S is not fbound."
           text-or-func-name))
  
  (make-instance 'send-text
                 :text text-or-func-name
                 :buttons buttons
                 :keyboard-type keyboard-type
                 :parse-mode parse-mode))


(defmethod print-object ((obj send-text) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S"
            (text obj))))


(-> do-action (send-text)
    (values &optional))

(defun do-action (action)
  (let* ((text (call-if-needed
                (text action)))
         (buttons (call-if-needed (buttons action)))
         (parse-mode (call-if-needed (parse-mode action)))
         (reply-markup
           (when buttons
             (ecase (keyboard-type action)
               (:inline
                  (make-instance 'cl-telegram-bot2/api:inline-keyboard-markup
                                 :inline-keyboard
                                 (list
                                  (loop for button in buttons
                                        collect (etypecase button
                                                  (cl-telegram-bot2/api:inline-keyboard-button
                                                     button)
                                                  (string
                                                     (make-instance 'cl-telegram-bot2/api:inline-keyboard-button
                                                                    :text button
                                                                    :callback-data button)))))))
               (:main
                  (make-instance 'cl-telegram-bot2/api:reply-keyboard-markup 
                                 :keyboard
                                 (list
                                  (loop for button in buttons
                                        collect (etypecase button
                                                  (cl-telegram-bot2/api:keyboard-button
                                                     button)
                                                  (string
                                                     (make-instance 'cl-telegram-bot2/api:keyboard-button
                                                                    :text button)))))))))))
    (apply #'reply
           text 
           (append
            (when parse-mode
              (list :parse-mode parse-mode))
            (when reply-markup
              (list :reply-markup reply-markup)))))
  (values))


(defmethod on-state-activation ((action send-text))
  (do-action action))


(defmethod process ((action send-text) update)
  (do-action action))


(defmethod on-result ((action send-text) result)
  (do-action action))
