(uiop:define-package #:cl-telegram-bot2/screen-widgets/image
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/screen-widgets/base
                #:base-widget)
  (:import-from #:trivial-types
                #:function-designator)
  (:import-from #:cl-telegram-bot2/api
                #:inline-keyboard-markup)
  (:import-from #:serapeum
                #:->)
  (:import-from #:cl-telegram-bot2/state
                #:callback-query-handlers
                #:callback)
  (:export #:image-widget
           #:image-url-widget))
(in-package #:cl-telegram-bot2/screen-widgets/image)


(defclass image-widget (base-widget)
  ())


(defclass image-url-widget (image-widget)
  ((url :initarg :url
        :type string)))


(defgeneric image-widget-to-tg (widget)
  (:method ((widget image-widget))
    nil)
  (:method ((widget image-url-widget))
    (slot-value widget 'url)))


(-> image-url-widget (string
                      &key
                      (:keyboard (or null
                                     inline-keyboard-markup
                                     function-designator))
                      (:on-callback-query (or callback
                                              callback-query-handlers)))
    (values image-url-widget &optional))

(defun image-url-widget (url &key on-callback-query keyboard)
  (make-instance 'image-url-widget
                 :url url
                 :keyboard keyboard
                 :on-callback-query (uiop:ensure-list on-callback-query)))
