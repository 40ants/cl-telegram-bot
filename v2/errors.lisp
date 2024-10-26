(uiop:define-package #:cl-telegram-bot2/errors
  (:use #:cl)
  (:import-from #:alexandria
                #:required-argument)
  (:export #:telegram-error
           #:error-description))
(in-package #:cl-telegram-bot2/errors)


(define-condition telegram-error ()
  ((description :initarg :description
                :initform (required-argument "DESCRIPTION is required argument for TELEGRAM-ERROR class.")
                :type string
                :reader error-description))
  (:report (lambda (c stream)
             (print-unreadable-object (c stream :type t)
               (write-string (error-description c) stream)))))
