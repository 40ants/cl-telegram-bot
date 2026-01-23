(uiop:define-package #:cl-telegram-bot2/screen-widgets/text
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/screen-widgets/base
                #:base-widget)
  (:import-from #:serapeum
                #:->)
  (:import-from #:trivial-types
                #:function-designator)
  (:export #:text-widget
           #:text-widget-text))
(in-package #:cl-telegram-bot2/screen-widgets/text)


(defclass text-widget (base-widget)
  ((text :initarg :text
         :type (or string function-designator)
         :reader text-widget-text)))


(defmethod text-widget-text :around ((widget text-widget))
  (let ((value (call-next-method)))
    (etypecase value
      (string
       value)
      (function-designator
       (let ((final-value (funcall value)))
         (unless (typep final-value 'string)
           (error "Value of type ~A was returned instead of string from function ~A."
                  (type-of final-value)
                  value))
         final-value)))))


(-> text-widget ((or string
                     function-designator))
    (values text-widget &optional))

(defun text-widget (text)
  (make-instance 'text-widget
                 :text text))
