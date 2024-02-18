(uiop:define-package #:cl-telegram-bot/inline-keyboard
  (:use #:cl)
  (:import-from #:cl-telegram-bot/network
                #:make-request)
  (:import-from #:cl-telegram-bot/callback
                #:callback)
  (:import-from #:cl-telegram-bot/markup
                #:to-markup)
  (:import-from #:serapeum
                #:dict)
  (:export #:answer-callback-query
           #:inline-keyboard
           #:keyboard-rows
           #:button-text
           #:inline-keyboard-button
           #:callback-button
           #:url-button
           #:callback-button-data
           #:button-url))
(in-package cl-telegram-bot/inline-keyboard)


(defclass inline-keyboard ()
  ((rows :initarg :rows
         :type list
         :initform nil
         :reader keyboard-rows))
  (:documentation "Represents an inline keyboard as specified in API https://core.telegram.org/bots/api#inlinekeyboardmarkup."))


(defclass inline-keyboard-button ()
  ((text :initarg :text
         :type string
         :reader button-text))
  (:documentation "Base class for all inline keyboard buttons.

                   API: https://core.telegram.org/bots/api#inlinekeyboardbutton"))


(defclass callback-button (inline-keyboard-button)
  ((data :initarg :data
         :type string
         :reader callback-button-data)))


(defclass url-button (inline-keyboard-button)
  ((url :initarg :data
        :type string
        :reader button-url)))


(defun inline-keyboard (rows)
  "Returns an inline keyboard which can be passed
   to CL-TELEGRAM-BOT/RESPONSE:REPLY as REPLY-MARKUP argument.

   Each row should be a list of INLINE-KEYBOARD-BUTTON objects or a single
   object of this class. In latter case, such row will have only one button."
  (make-instance 'inline-keyboard
                 :rows (mapcar #'uiop:ensure-list rows)))


(defun callback-button (text data)
  "Creates a button which will call a callback."
  (make-instance 'callback-button :text text
                                  :data data))

(defun url-button (text url)
  "Creates a button which will open an url."
  (make-instance 'url-button :text text
                             :url url))


(defun answer-callback-query (bot callback &key text show-alert url)
  "https://core.telegram.org/bots/api#answercallbackquery"
  (check-type callback callback)
  (let ((options
          (append
           (list
            :callback_query_id (cl-telegram-bot/callback:callback-id callback))
           (when text
             (list :text text))
           (when show-alert
             (list :show_alert show-alert))
           (when url
             (list :url url)))))
    (apply #'make-request bot "answerCallbackQuery" options)))


(defmethod to-markup ((keyboard inline-keyboard))
  (dict "inline_keyboard"
        (loop for row in (keyboard-rows keyboard)
              collect (mapcar #'to-markup row))))

(defmethod to-markup ((button callback-button))
  (dict "text" (button-text button)
        "callback_data" (callback-button-data button)))


(defmethod to-markup ((button url-button))
  (dict "text" (button-text button)
        "url" (button-url button)))
