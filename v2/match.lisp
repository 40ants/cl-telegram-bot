(uiop:define-package #:cl-telegram-bot2/match
  (:use #:cl)
  (:export #:matchp
           #:matcher))
(in-package #:cl-telegram-bot2/match)


(defclass matcher ()
  ())


(defgeneric matchp (matcher obj)
  (:documentation "Should return T if OBJ match to the matcher. The simples matcher compares two strings."))
