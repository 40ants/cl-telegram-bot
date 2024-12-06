(uiop:define-package #:cl-telegram-bot2/types
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/api)
  (:import-from #:serapeum
                #:soft-list-of)
  (:export #:reply-markup-type))
(in-package #:cl-telegram-bot2/types)


(deftype reply-markup-type ()
  `(or cl-telegram-bot2/api:reply-keyboard-markup
       cl-telegram-bot2/api:reply-keyboard-remove
       cl-telegram-bot2/api:inline-keyboard-markup
       cl-telegram-bot2/api:force-reply))
