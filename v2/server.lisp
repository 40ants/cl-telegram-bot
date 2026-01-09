(uiop:define-package #:cl-telegram-bot2/server
  (:use #:cl)
  (:import-from #:log)
  (:import-from #:trivial-backtrace
                #:print-backtrace)
  (:import-from #:cl-telegram-bot2/pipeline
                #:process-updates
                #:continue-processing)
  (:import-from #:cl-telegram-bot2/bot
                #:debug-mode)
  (:import-from #:bordeaux-threads-2
                #:bordeaux-threads-simple-error
                #:make-thread
                #:destroy-thread
                #:thread-name
                #:all-threads)
  (:import-from #:str
                #:starts-with?)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:export #:start-polling
           #:stop-polling))
(in-package #:cl-telegram-bot2/server)



(defvar *threads* nil)


(defun start-polling (bot &key
                          debug
                          (delay-between-retries 10)
                          (thread-name "telegram-bot"))
  "Start processing new updates from the Telegram API.

   Pass bot instance as the first argument and maybe some other optional arguments.

   If DEBUG argument is T, then bot will ignore updates which it can't to process without errors.
   Otherwise, an interactive debugger will popup."
  
  (when (getf *threads* bot)
    (error "Processing already started."))

  (setf (debug-mode bot) debug)

  (log:info "Starting thread to process updates for" bot)
  (flet ((continue-processing-if-not-debug (condition)
           (let ((restart (find-restart 'continue-processing
                                        condition)))
             (when restart
               ;; If bot is not in debug mode, then we ignore the error
               ;; and will do the next attempt to fetch updates.
               ;; Otherwise an interactive debugger will be shown
               (unless (debug-mode bot)
                 (invoke-restart restart delay-between-retries)))))
         (stop-bot ()
           (stop-polling bot)))

    (cl-telegram-bot2/bot::start-actors bot)
    
    (setf (getf *threads* bot)
          (make-thread
           (lambda ()
             (handler-bind ((serious-condition #'continue-processing-if-not-debug))
               (with-log-unhandled ()
                 (process-updates bot))))
           :name thread-name))

    ;; Here we return a closure to stop the bot:
    #'stop-bot))


(defun destroy-thread-safely (thread)
  (when (bt2:thread-alive-p thread)
    (log:info "Stopping thread for" thread)
    (handler-bind ((bordeaux-threads-simple-error
                     (lambda (e)
                       (declare (ignore e))
                       ;; In case if thread was already stopped as the result
                       ;; of race-condition we need to check it again
                       (unless (bt2:thread-alive-p thread)
                         (return-from destroy-thread-safely)))))
      (destroy-thread thread))))


(defun clean-threads ()
  "TODO: we need to figure out why the threads are not being cleaned up. Maybe this happens when errors happen?"
  (loop for tr in (all-threads)
        when (or (starts-with? "message-thread" (thread-name tr))
                 (starts-with? "timer-wheel" (thread-name tr))
                 (starts-with? "telegram-bot" (thread-name tr)))
          do (destroy-thread-safely tr)))


(defun stop-polling (bot)
  (let ((thread (getf *threads* bot)))
    (when thread
      (destroy-thread-safely thread)
      
      (setf (getf *threads* bot)
            nil))
    (cl-telegram-bot2/bot::stop-actors bot)
    (clean-threads)))
