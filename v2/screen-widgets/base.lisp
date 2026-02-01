(uiop:define-package #:cl-telegram-bot2/screen-widgets/base
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/api
                #:inline-keyboard-markup)
  (:import-from #:cl-telegram-bot2/state
                #:callback-query-handlers)
  (:export #:base-widget
           #:widget-keyboard
           #:on-callback-query))
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
