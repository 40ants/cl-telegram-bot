(uiop:define-package #:cl-telegram-bot2/utils
  (:use #:cl)
  (:import-from #:trivial-arguments
                #:arglist)
  (:import-from #:lambda-fiddle
                #:required-lambda-vars)
  (:import-from #:serapeum
                #:->)
  (:import-from #:alexandria
                #:non-negative-fixnum
                #:positive-fixnum)
  (:export #:call-if-needed
           #:deep-copy
           #:arity))
(in-package #:cl-telegram-bot2/utils)


(defun call-if-needed (value &rest args)
  "If value is a fbound SYMBOL, then calls as a function and then returns a result."
  (typecase value
    (symbol
     (if (fboundp value)
         (apply value args)
         value))
    (t
     value)))



(-> arity ((or symbol function))
    (values non-negative-fixnum &optional))

(defun arity (funcallable)
  (length (required-lambda-vars
           (arglist funcallable))))


;; This deep copy code was taken from CL-MOP
;; https://github.com/Inaimathi/cl-mop
;; but code for copying a list was replaced with code
;; a code making a deep copy each list item.


(defgeneric deep-copy (object)
  (:documentation "Does a general deep-copy on the given object and sub-pieces.
Returns atoms, numbers and chars. 
Runs copy-tree on lists, and copy-seq on other sequences.
Runs copy-structure on pathnames, hash tables and other structure-objects"))

(defmethod deep-copy (object)
  "The default unspecialized case should only catch atoms, numbers and characters.
It merely returns its results."
  object)

(defmethod deep-copy ((object standard-object))
  "The default deep copy specializes on STANDARD-OBJECT. It takes an object and returns a deep copy."
  (let ((copy (allocate-instance (class-of object))))
    (loop for slot in (closer-mop:class-slots
                       (class-of object))
          for slot-name = (closer-mop:slot-definition-name slot)
          when (slot-boundp object slot-name)
          do (setf (slot-value copy slot-name)
                   (deep-copy (slot-value object slot-name))))
    (values copy)))

(defmethod deep-copy ((object cons))
  "A deep copy of a general sequence is merely (copy-seq sequence)."
  (cons (deep-copy (car object))
        (deep-copy (cdr object))))

(defmethod deep-copy ((object sequence))
  "A deep copy of a general sequence is merely (copy-seq sequence)."
  (map (type-of object)
       #'deep-copy
       object))

(defmethod deep-copy ((object structure-object))
  "A deep copy of a structure-object is (copy-structure object)."
  (copy-structure object))
