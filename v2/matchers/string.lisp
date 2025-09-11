(uiop:define-package #:cl-telegram-bot2/matchers/string
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:import-from #:cl-telegram-bot2/match
                #:matcher
                #:matchp)
  (:export #:string-matcher
           #:matcher-string
           #:case-insensitive-p))
(in-package #:cl-telegram-bot2/matchers/string)


(defclass string-matcher (matcher)
  ((string :initarg :string
           :type string
           :reader matcher-string)
   (case-insensitive :initarg :case-insensitive
                     :type boolean
                     :reader case-insensitive-p)))


(-> string-matcher (string &key (:case-insensitive boolean))
    (values string-matcher &optional))


(defun string-matcher (string &key case-insensitive)
  (make-instance 'string-matcher
                 :string string
                 :case-insensitive case-insensitive))


(defmethod matchp ((matcher string-matcher) (obj string))
  (cond
    ((case-insensitive-p matcher)
     (string-equal (matcher-string matcher)
                   obj))
    (t
     (string= (matcher-string matcher)
              obj))))
