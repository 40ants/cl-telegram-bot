(uiop:define-package #:cl-telegram-bot2/screen-widgets/image
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/screen-widgets/base
                #:base-widget)
  (:import-from #:serapeum
                #:->)
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


(-> image-url-widget (string)
    (values image-url-widget &optional))

(defun image-url-widget (url)
  (make-instance 'image-url-widget
                 :url url))
