(uiop:define-package #:cl-telegram-bot/commands
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:cl-telegram-bot/chat
                #:get-chat-id
                #:make-chat
                #:chat)
  (:import-from #:cl-telegram-bot/entities/core
                #:make-entity)
  (:import-from #:cl-telegram-bot/network
                #:make-request)
  (:import-from #:cl-telegram-bot/pipeline
                #:process)
  (:import-from #:cl-telegram-bot/bot
                #:bot)
  (:import-from #:serapeum
                #:soft-alist-of
                #:defvar-unbound)
  (:import-from #:cl-telegram-bot/utils
                #:def-telegram-call)
  (:import-from #:cl-telegram-bot/response-processing
                #:process-response
                #:interrupt-processing)
  (:export))
(in-package #:cl-telegram-bot/commands)


(declaim (ftype (function (bot (or (soft-alist-of string string)
                                   (serapeum:soft-list-of string)))
                          (values))
                set-my-commands))

;; TODO: Support scope and language optional arguments
(defun set-my-commands (bot commands)
  "https://core.telegram.org/bots/api#setmycommands"
  (log:debug "Sending commands to the server" commands)
  (make-request bot "setMyCommands"
                :|commands| (loop for command in commands
                                  collect (etypecase command
                                            (string (list :|command| command
                                                          :|description| "No documentation."))
                                            (cons (list :|command| (car command)
                                                        :|description| (cdr command))))))
  (values))
