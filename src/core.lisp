(defpackage #:cl-telegram-bot/core
  (:use #:cl)
  (:nicknames #:cl-telegram-bot)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:destroy-thread)
  (:import-from #:cl-telegram-bot/update
                #:process-updates)
  (:import-from #:cl-telegram-bot/bot
                #:defbot)
  (:import-from #:cl-telegram-bot/message
                #:on-message
                #:reply)
  ;; This package exports only essential symbols, needed
  ;; in 80% cases.
  (:export #:defbot
           #:on-message
           #:reply
           #:start-processing
           #:stop-processing))
(in-package cl-telegram-bot/core)



(defvar *threads* nil)


(defun start-processing (bot)
  (when (getf *threads* bot)
    (error "Processing already started."))

  (log:info "Starting thread to process updates for" bot)
  (setf (getf *threads* bot)
        (make-thread
         (lambda ()
           (process-updates bot))
         :name "telegram-bot")))


(defun stop-processing (bot)
  (when (getf *threads* bot)
    (log:info "Stopping thread for" bot)
    
    (destroy-thread (getf *threads* bot))
    (setf (getf *threads* bot)
          nil)))
