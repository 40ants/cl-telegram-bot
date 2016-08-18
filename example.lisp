(require 'cl-ppcre)
(require 'cl-telegram-bot)

(defpackage :example-bot
 (:use #:cl-telegram-bot)
 (:use #:cl))

(in-package :example-bot)

(defun match-command (regex text function)
  (multiple-value-bind (msg match)
    (cl-ppcre:scan-to-strings regex text)
    (when match 
          (funcall function msg match))))

(let ((bot (make-bot "123456789:YOUR TOKEN HERE")))
  (loop
    (with-package :example-bot
     (loop for update across (get-updates bot) do
        (let* ((message (access update 'message))
	         (text (access message 'text))
		     (message-id (access message 'message--id))
		     (chat-id (access message 'chat 'id))
		     (first-name (access message 'from 'first--name)))
	    (format t "---~%ID: ~a~%chat: ~a~%user: ~a~%text: <<~a>>~%"
		 message-id
         chat-id
         first-name
         text)
        (when text
          (match-command "^/echo (.*)$" text
             (lambda (msg args)
              (send-message bot
               chat-id
               (elt args 0))))))))
          (sleep 1)))
