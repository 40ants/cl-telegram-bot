(uiop:define-package #:cl-telegram-bot/keyboard
  (:use #:cl)
  (:import-from #:cl-telegram-bot/markup
                #:to-markup)
  (:import-from #:serapeum
                #:dict)
  (:export #:keyboard
           #:keyboard-rows
           #:button-text
           #:button))
(in-package #:cl-telegram-bot/keyboard)


(defclass keyboard ()
  ((rows :initarg :rows
         :type list
         :initform nil
         :reader keyboard-rows)
   (persistent :initarg :persistent
               :type boolean
               :initform nil
               :reader persistentp
               :documentation "Requests clients to always show the keyboard when the regular keyboard is hidden.")
   (resize :initarg :resize
           :type boolean
           :initform nil
           :reader resizep
           :documentation "Requests clients to resize the keyboard vertically for optimal fit (e.g., make the keyboard smaller if there are just two rows of buttons).")
   (one-time :initarg :one-time
             :type boolean
             :initform nil
             :reader one-time-p
             :documentation "Requests clients to hide the keyboard as soon as it's been used.")
   (selective :initarg :selective
              :type boolean
              :initform nil
              :reader selectivep
              :documentation "Use this parameter if you want to show the keyboard to specific users only.")
   (input-field-placeholder :initarg :input-field-placeholder
                            :initform nil
                            :reader input-field-placeholder
                            :documentation "The placeholder to be shown in the input field when the keyboard is active."))
  (:documentation "Represents a keyboard specified in API https://core.telegram.org/bots/api#replykeyboardmarkup."))


(defclass button ()
  ((text :initarg :text
         :type string
         :reader button-text))
  (:documentation "Base class for all inline keyboard buttons.

                   API: https://core.telegram.org/bots/api#keyboardbutton"))


(defun keyboard (rows &rest args &key peristent resize one-time selective input-field-placeholder)
  "Returns a keyboard which can be passed
   to CL-TELEGRAM-BOT/RESPONSE:REPLY as REPLY-MARKUP argument.

   Each row should be a list of BUTTON objects or a single
   object of this class. In latter case, such row will have only one button."
  (declare (ignore peristent resize one-time selective input-field-placeholder))
  (apply #'make-instance
         'keyboard
         :rows (mapcar #'uiop:ensure-list rows)
         args))


(defun button (text)
  (make-instance 'button
                 :text text))


(defmethod to-markup ((keyboard keyboard))
  (let ((result (dict "keyboard"
                      (loop for row in (keyboard-rows keyboard)
                            collect (mapcar #'to-markup row)))))
    (when (persistentp keyboard)
      (setf (gethash "is_persistent" result) t))
    
    (when (resizep keyboard)
      (setf (gethash "resize_keyboard" result) t))
    
    (when (one-time-p keyboard)
      (setf (gethash "one_time_keyboard" result) t))
    
    (when (selectivep keyboard)
      (setf (gethash "selective" result) t))
    
    (when (input-field-placeholder keyboard)
      (setf (gethash "input_field_placeholder" result)
            (input-field-placeholder keyboard)))
    
    (values result)))


(defmethod to-markup ((button button))
  (dict "text" (button-text button)))

