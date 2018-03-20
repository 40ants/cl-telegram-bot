(defpackage #:cl-telegram-bot/core
  (:use #:cl)
  (:nicknames #:cl-telegram-bot)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:destroy-thread)
  (:import-from #:log4cl)
  (:import-from #:cl-telegram-bot/update
                #:process-updates)
  (:import-from #:cl-telegram-bot/bot
                #:defbot)
  (:import-from #:cl-telegram-bot/message
                #:on-message
                #:reply)
  (:import-from #:cl-telegram-bot/entities/command
                #:on-command)
  (:import-from #:trivial-backtrace
                #:print-backtrace)
  ;; This package exports only essential symbols, needed
  ;; in 80% cases.
  (:export #:defbot
           #:on-message
           #:reply
           #:start-processing
           #:stop-processing
           #:on-command))
(in-package cl-telegram-bot/core)



(defvar *threads* nil)


(defun start-processing (bot &key debug (delay-between-retries 10))
  (when (getf *threads* bot)
    (error "Processing already started."))

  (log:info "Starting thread to process updates for" bot)
  (flet ((continue-processing-if-not-debug (condition)
           (let ((restart (find-restart 'cl-telegram-bot/update::continue-processing
                                        condition)))
             (when restart
               (let ((traceback (print-backtrace
                                 condition :output nil)))
                 (log:error "Unable to process Telegram updates" traceback))
               
               (unless debug
                 (invoke-restart restart delay-between-retries))))))
    (setf (getf *threads* bot)
          (make-thread
           (lambda ()
             (handler-bind ((error #'continue-processing-if-not-debug))
               (process-updates bot)))
           :name "telegram-bot"))))


(defun stop-processing (bot)
  (when (getf *threads* bot)
    (log:info "Stopping thread for" bot)
    
    (destroy-thread (getf *threads* bot))
    (setf (getf *threads* bot)
          nil)))
