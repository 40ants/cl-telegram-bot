(uiop:define-package #:cl-telegram-bot2/server
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:destroy-thread)
  (:import-from #:log)
  ;; (:import-from #:cl-telegram-bot/update
  ;;               #:process-updates)
  ;; (:import-from #:cl-telegram-bot/response
  ;;               #:reply)
  ;; (:import-from #:cl-telegram-bot/bot
  ;;               #:debug-mode
  ;;               #:defbot)
  ;; (:import-from #:cl-telegram-bot/message
  ;;               #:on-message)
  ;; (:import-from #:cl-telegram-bot/entities/command
  ;;               #:update-commands
  ;;               #:on-command)
  (:import-from #:trivial-backtrace
                #:print-backtrace)
  (:import-from #:cl-telegram-bot2/pipeline
                #:process-updates
                #:continue-processing)
  (:import-from #:cl-telegram-bot2/bot
                #:debug-mode)
  ;; This package exports only essential symbols, needed
  ;; in 80% cases.
  (:export ;; #:defbot
           ;; #:on-message
           ;; #:reply
           #:start-polling
           #:stop-polling
           ;; #:on-command
           ))
(in-package #:cl-telegram-bot2/server)



(defvar *threads* nil)


(defun start-polling (bot &key debug
                                  (delay-between-retries 10)
                                  (thread-name "telegram-bot"))
  (when (getf *threads* bot)
    (error "Processing already started."))

  (setf (debug-mode bot) debug)

  (log:info "Starting thread to process updates for" bot)
  (flet ((continue-processing-if-not-debug (condition)
           (let ((restart (find-restart 'continue-processing
                                        condition)))
             (when restart
               (let ((traceback (print-backtrace
                                 condition :output nil)))
                 (log:error "Unable to process Telegram updates" traceback))
               
               (unless (debug-mode bot)
                 (invoke-restart restart delay-between-retries)))))
         (stop-bot ()
           (stop-polling bot)))

    ;; (update-commands bot)

    (cl-telegram-bot2/bot::start-actors bot)
    
    (setf (getf *threads* bot)
          (make-thread
           (lambda ()
             (handler-bind ((error #'continue-processing-if-not-debug))
               (process-updates bot)))
           :name thread-name))

    ;; Here we return a closure to stop the bot:
    #'stop-bot))


(defun stop-polling (bot)
  (let ((thread (getf *threads* bot)))
    (when thread
      (when (bt:thread-alive-p thread)
        (log:info "Stopping thread for" bot)
        (destroy-thread thread))
      (setf (getf *threads* bot)
            nil))
    (cl-telegram-bot2/bot::stop-actors bot)))
