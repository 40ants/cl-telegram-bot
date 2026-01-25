(uiop:define-package #:cl-telegram-bot2/states/screen
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/state-with-commands
                #:global-command)
  (:import-from #:cl-telegram-bot2/actions/send-text
                #:send-text)
  
  (:import-from #:cl-telegram-bot2/state
                #:state)
  (:import-from #:cl-telegram-bot2/generics
                #:process-state
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/high
                #:reply-with-photo
                #:reply)
  (:import-from #:cl-telegram-bot2/actions/delete-messages
                #:delete-messages)
  (:import-from #:cl-telegram-bot2/term/switch-to
                #:switch-to)
  (:import-from #:cl-telegram-bot2/screen-widgets/text
                #:text-widget-text
                #:text-widget)
  (:import-from #:cl-telegram-bot2/screen-widgets/base
                #:widget-keyboard
                #:base-widget)
  (:import-from #:serapeum
                #:->
                #:soft-list-of)
  (:import-from #:global-vars
                #:define-global-var)
  (:import-from #:str
                #:join)
  (:import-from #:cl-telegram-bot2/screen-widgets/image
                #:image-widget-to-tg
                #:image-widget)
  (:import-from #:anaphora
                #:awhen
                #:it)
  (:import-from #:cl-telegram-bot2/api
                #:reply-keyboard-remove
                #:inline-keyboard-markup
                #:reply-keyboard-markup)
  (:export #:screen
           #:screen-widgets
           #:switch-to-screen
           #:screenp
           #:screen-keyboard))
(in-package #:cl-telegram-bot2/states/screen)


(defclass screen (state)
  ((widgets :initform nil
            :initarg :widgets
            :type (soft-list-of base-widget)
            :reader screen-widgets)
   (keyboard :initarg :keyboard
             :initform nil
             :type (or null
                       reply-keyboard-markup
                       reply-keyboard-remove)
             :reader screen-keyboard))
  (:default-initargs
   :on-deletion (list (delete-messages))))


(defclass widgets-group ()
  ((widgets :initform nil
            :initarg :widgets
            :type (soft-list-of base-widget)
            :reader widgets-group-widgets)
   (keyboard :initarg :keyboard
             :initform nil
             :type (or null
                       reply-keyboard-remove
                       reply-keyboard-markup
                       inline-keyboard-markup)
             :reader widgets-group-keyboard)))


(-> widgets-group ((soft-list-of base-widget)
                   &key (:keyboard
                         (or null
                             reply-keyboard-remove
                             reply-keyboard-markup
                             inline-keyboard-markup)))
    (values widgets-group &optional))

(defun widgets-group (widgets &key keyboard)
  (make-instance 'widgets-group
                 :widgets widgets
                 :keyboard keyboard))


(defun ensure-widget (obj)
  (etypecase obj
    (base-widget
     obj)
    (string
     (text-widget obj))))


(-> screen ((soft-list-of (or string base-widget))
            &key (:keyboard (or null reply-keyboard-markup))))

(defun screen (widgets &key (keyboard nil keyboard-given-p))
  "If KEYBOARD argument was not supplied, then existing reply keyboard will stay on screen.

   Pass a REPLY-KEYBOARD-MARKUP object as KEYBOARD argument, to show a new keyboard.
   Pass a NIL as KEYBOARD argument, to hide current reply keyboard keyboard.
   "
  (let ((widgets (mapcar #'ensure-widget
                         widgets)))
    (when (widget-keyboard
           (first widgets))
      (error "First widget should not have a keyboard because in this case screen can't show or hide reply keyboard."))

    (make-instance 'screen
                   :widgets widgets
                   :keyboard (cond
                               (keyboard-given-p
                                (or keyboard
                                    (make-instance 'reply-keyboard-remove)))
                               (t
                                nil)))))


(-> group-widgets ((soft-list-of base-widget)
                   (or null reply-keyboard-markup
                       reply-keyboard-remove))
    (values (soft-list-of widgets-group) &optional))

(defun group-widgets (widgets screen-keyboard)
  (loop with group = nil
        with groups = nil
        with group-keyboard = screen-keyboard
        for rest-widgets on widgets
        for widget = (car rest-widgets)
        for next-widget = (cadr rest-widgets)
        do (push widget group)
        when (widget-keyboard widget)
          do (setf group-keyboard (widget-keyboard widget))
        when (and next-widget
                  (or (typep next-widget 'image-widget)
                      (and group-keyboard
                           (widget-keyboard next-widget))))
          do (push (widgets-group (nreverse group)
                                  :keyboard group-keyboard)
                   groups)
             (setf group nil)
             (setf group-keyboard nil)
        finally (return (progn (push (widgets-group (nreverse group)
                                                    :keyboard group-keyboard)
                                     groups)
                               (nreverse groups)))))

(define-global-var *empty-line*
    (coerce (list #\Newline #\Newline)
            'string))


(-> group-to-reply (widgets-group)
    (values &optional))

(defun group-to-reply (group)
  (let ((text-items nil)
        (photo nil)
        (keyboard (widgets-group-keyboard group)))

    (loop for widget in (widgets-group-widgets group)
          do (etypecase widget
               (text-widget
                (push (text-widget-text widget)
                      text-items))
               (image-widget
                (setf photo
                      (image-widget-to-tg widget)))))

    (flet ((get-text ()
             (when text-items
               (join *empty-line*
                     (nreverse text-items)))))
      (cond
        (photo
         (apply #'reply-with-photo
                photo
                (append
                 (awhen (get-text)
                   (list :caption it))

                 (awhen keyboard
                   (list :reply-markup it)))))
        (text-items
         (apply #'reply
                (get-text)
                (append
                 (awhen keyboard
                   (list :reply-markup it))))))))
  (values))


(defmethod on-state-activation ((state screen))
  (loop with all-groups = (group-widgets (screen-widgets state)
                                         (screen-keyboard state))
        for group in all-groups
        do (group-to-reply group))
  
  (values))


(defun screenp (obj)
  (typep obj 'screen))


(defun switch-to-screen (screen)
  (switch-to screen
             :delete-prev-state-p #'screenp))
