(uiop:define-package #:cl-telegram-bot2/screen-widgets/text
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/screen-widgets/base
                #:base-widget)
  (:import-from #:serapeum
                #:->)
  (:import-from #:trivial-types
                #:function-designator)
  (:import-from #:cl-telegram-bot2/api
                #:inline-keyboard-markup)
  (:import-from #:cl-telegram-bot2/state
                #:callback-query-handlers
                #:callback)
  (:export #:text-widget
           #:text-widget-text
           #:text-widget-parse-mode))
(in-package #:cl-telegram-bot2/screen-widgets/text)


(defclass text-widget (base-widget)
  ((text :initarg :text
         :type (or string function-designator)
         :reader text-widget-text)
   (parse-mode :initarg :parse-mode
               :initform nil
               :type (or null string)
               :reader text-widget-parse-mode)))


(defmethod text-widget-text :around ((widget text-widget))
  (let ((value (call-next-method)))
    (etypecase value
      (string
         value)
      (function-designator
         (let ((final-value (when value
                              (funcall value))))
           (unless (typep final-value 'string)
             (error "Value of type ~A was returned instead of string from function ~A."
                    (type-of final-value)
                    value))
           final-value)))))


(-> text-widget ((or string
                     function-designator)
                 &key
                 (:keyboard (or null
                                inline-keyboard-markup
                                function-designator))
                 (:on-callback-query (or callback
                                         callback-query-handlers))
                 (:parse-mode (or null
                                  string)))
    (values text-widget &optional))

(defun text-widget (text &key keyboard on-callback-query parse-mode)
  (make-instance 'text-widget
                 :text text
                 :keyboard keyboard
                 :parse-mode parse-mode
                 :on-callback-query (uiop:ensure-list on-callback-query)))
