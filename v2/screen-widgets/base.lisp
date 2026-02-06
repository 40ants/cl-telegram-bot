(uiop:define-package #:cl-telegram-bot2/screen-widgets/base
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/api
                #:inline-keyboard-markup)
  (:import-from #:cl-telegram-bot2/state
                #:callback-query-handlers)
  (:import-from #:trivial-types
                #:function-designator)
  (:export #:base-widget
           #:widget-keyboard
           #:on-callback-query
           #:maybe-widget-type))
(in-package #:cl-telegram-bot2/screen-widgets/base)


(defclass base-widget ()
  ((keyboard :initarg :keyboard
             :initform nil
             :type (or null
                       inline-keyboard-markup
                       function-designator)
             :reader widget-keyboard)
   (on-callback-query :initarg :on-callback-query
                      :type callback-query-handlers
                      :initform nil
                      :reader on-callback-query)))

(deftype maybe-widget-type ()
  "A type for which can be CL-TELEGRAM-BOT2/STATES/SCREEN:ENSURE-WIDGET function can return a real object of BASE-WIDGET class.

   - If object is already of type BASE-WIDGET, then it is returned as is.
   - If it is a string, then a new text widget will be created.
   - If it is a function-designator, then it will be funcalled with a single CL-TELEGRAM-BOT2/API:UPDATE object.ww"
  '(or
    base-widget
    string
    function-designator))
