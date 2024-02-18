(uiop:define-package #:cl-telegram-bot/markup
  (:use #:cl)
  (:export #:to-markup))
(in-package #:cl-telegram-bot/markup)


(defgeneric to-markup (obj)
  (:documentation "Transforms object into markup of Telegram API.

                   Methods of this class should return a hash-table, representing OBJ
                   in terms of Telegram API.")
  (:method ((obj hash-table))
    obj))
