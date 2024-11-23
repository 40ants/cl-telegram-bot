(uiop:define-package #:cl-telegram-bot2-examples/gallery
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/bot
                #:defbot)
  (:import-from #:cl-telegram-bot2/server
                #:stop-polling
                #:start-polling)
  (:import-from #:cl-telegram-bot2/high
                #:reply
                #:chat-state)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:cl-telegram-bot2/pipeline
                #:back-to
                #:back)
  (:import-from #:cl-telegram-bot2/api
                #:message-message-id)
  (:import-from #:cl-telegram-bot2/state-with-commands
                #:global-command
                #:command
                #:state-with-commands-mixin)
  (:import-from #:cl-telegram-bot2/generics
                #:on-result
                #:on-state-activation
                #:process)
  (:import-from #:cl-telegram-bot2/state
                #:state)
  (:import-from #:cl-telegram-bot2/term/back
                #:back-to-id)
  (:import-from #:cl-telegram-bot2/actions/send-text
                #:send-text)
  (:import-from #:str
                #:trim)
  (:import-from #:cl-telegram-bot2/actions/send-photo
                #:send-photo))
(in-package #:cl-telegram-bot2-examples/gallery)


(defparameter *photos*
  (directory (uiop:wilden
              (asdf:system-relative-pathname
               :cl-telegram-bot2-examples
               (make-pathname :directory '(:relative "examples" "images"))))))


(defbot test-bot ()
  ()
  (:initial-state
   (state nil
          :on-update (send-photo (first *photos*)
                                 :caption "Cat 1"
                                 :inline-keyboard (list "Prev" "Next"))
          :on-callback-query
          (list (cons "Next"
                      (edit-photo (second *photos*)
                                  :caption "Cat 2"
                                  :inline-keyboard (list "Prev" "Next")))
                (cons "Prev"
                      (send-text "No prev photo"))))))


(defvar *bot* nil)


(defun clean-threads ()
  (loop for tr in (bt:all-threads)
        when (or (str:starts-with? "message-thread" (bt:thread-name tr))
                 (str:starts-with? "timer-wheel" (bt:thread-name tr))
                 (str:starts-with? "telegram-bot" (bt:thread-name tr)))
        do (bt:destroy-thread tr)))


(defun stop ()
  (when *bot*
    (stop-polling *bot*)
    (setf *bot* nil)
    (clean-threads))
  (values))


(defun start ()
  (stop)
  
  (unless *bot*
    (setf *bot*
          (make-test-bot (uiop:getenv "TELEGRAM_TOKEN"))))
  
  (start-polling *bot* :debug t))
