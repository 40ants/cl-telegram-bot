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
  (:import-from #:cl-telegram-bot2/term/back
                #:switch-to
                #:back)
  (:import-from #:cl-telegram-bot2/screen-widgets/text
                #:text-widget-text
                #:text-widget)
  (:import-from #:cl-telegram-bot2/screen-widgets/base
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
  (:export #:screen
           #:screen-widgets
           #:switch-to-screen
           #:screenp))
(in-package #:cl-telegram-bot2/states/screen)


(defclass screen (state)
  ((widgets :initform nil
            :initarg :widgets
            :type (soft-list-of base-widget)
            :reader screen-widgets))
  (:default-initargs
   :on-deletion (delete-messages)))


(defun ensure-widget (obj)
  (etypecase obj
    (base-widget
     obj)
    (string
     (text-widget obj))))


(defun screen (widgets)
  (make-instance 'screen
                 :widgets (mapcar #'ensure-widget
                                  widgets)))


(defun group-widgets (widgets)
  (loop with group = nil
        with groups = nil
        for rest-widgets on widgets
        for widget = (car rest-widgets)
        for next-widget = (cadr rest-widgets)
        do (push widget group)
        when (typep next-widget 'image-widget)
          do (push (nreverse group)
                   groups)
             (setf group nil)
        finally (return (progn (push (nreverse group)
                                     groups)
                               (nreverse groups)))))

(define-global-var *empty-line*
    (coerce (list #\Newline #\Newline)
            'string))


(-> group-to-reply ((soft-list-of base-widget))
    (values &optional))

(defun group-to-reply (group)
  (let ((text-items nil)
        (photo nil))
    (loop for widget in group
          do (etypecase widget
               (text-widget
                (push (text-widget-text widget)
                      text-items))
               (image-widget
                (setf photo
                      (image-widget-to-tg widget)))))
    
    (when text-items
      (let ((text (join *empty-line*
                        (nreverse text-items))))
        (cond
          (photo
           (reply-with-photo photo
                             :caption text))
          (text
           (reply text))))))
  (values))


(defun send-widgets (widgets)
  (loop for group in (group-widgets widgets)
        do (group-to-reply group)))


(defmethod on-state-activation ((state screen))
  (send-widgets (screen-widgets state))
  (values))


(defun screenp (obj)
  (typep obj 'screen))


(defun switch-to-screen (screen)
  (switch-to screen
             :delete-prev-state-p #'screenp))
