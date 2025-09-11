(uiop:define-package #:cl-telegram-bot/commands
  (:use #:cl)
  (:import-from #:log)
  (:import-from #:cl-telegram-bot/network
                #:make-request)
  (:import-from #:cl-telegram-bot/bot
                #:bot)
  (:import-from #:serapeum
                #:soft-alist-of
                #:defvar-unbound))
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
