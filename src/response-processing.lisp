(uiop:define-package #:cl-telegram-bot/response-processing
  (:use #:cl)
  (:export #:process-response
           #:interrupt-processing))
(in-package #:cl-telegram-bot/response-processing)


(define-condition interrupt-processing ()
  ())


(defgeneric process-response (bot message response)
  (:documentation "Processes immediate responses of different types."))


(defun interrupt-processing ()
  (signal 'interrupt-processing))
