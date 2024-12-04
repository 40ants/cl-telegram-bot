(uiop:define-package #:cl-telegram-bot2/types
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/api)
  (:import-from #:serapeum
                #:soft-list-of)
  (:export #:inline-keyboard-button
           #:inline-keyboard-buttons))
(in-package #:cl-telegram-bot2/types)


(deftype inline-keyboard-button ()
  `(or string
       cl-telegram-bot2/api:keyboard-button))


(deftype inline-keyboard-buttons ()
  '(soft-list-of inline-keyboard-button))
