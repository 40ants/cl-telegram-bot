(uiop:define-package #:cl-telegram-bot2/utils
  (:use #:cl)
  (:export #:call-if-needed
           #:deep-copy))
(in-package #:cl-telegram-bot2/utils)


(defun call-if-needed (value)
  "If value is a fbound SYMBOL, then calls as a function and then returns a result."
  (typecase value
    (symbol
     (if (fboundp value)
         (funcall value)
         value))
    (t
     value)))


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
