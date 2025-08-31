(uiop:define-package #:cl-telegram-bot2/matchers/regex
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:import-from #:cl-telegram-bot2/match
                #:matchp
                #:matcher)
  (:import-from #:str
                #:ensure-prefix
                #:ensure-suffix)
  (:import-from #:cl-ppcre
                #:count-matches)
  (:export #:regex-matcher
           #:matcher-regex))
(in-package #:cl-telegram-bot2/matchers/regex)


(defclass regex-matcher (matcher)
  ((regex :initarg :regex
          :type string
          :reader matcher-regex)))


(-> regex-matcher (string)
    (values regex-matcher &optional))


(defun regex-matcher (string)
  (make-instance 'regex-matcher
                 :regex (ensure-prefix "^"
                                       (ensure-suffix "$" string))))


(defmethod matchp ((matcher regex-matcher) (obj string))
  (not (zerop (count-matches (matcher-regex matcher)
                             obj))))
