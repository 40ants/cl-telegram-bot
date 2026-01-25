(uiop:define-package #:cl-telegram-bot2/screen-widgets/base
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/api
                #:inline-keyboard-markup)
  (:export #:base-widget
           #:widget-keyboard))
(in-package #:cl-telegram-bot2/screen-widgets/base)


(defclass base-widget ()
  ((keyboard :initarg :keyboard
             :initform nil
             :type (or null inline-keyboard-markup)
             :reader widget-keyboard)))
