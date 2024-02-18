(uiop:define-package #:example-bot/bot
  (:use #:cl)
  (:import-from #:cl-telegram-bot
                #:start-processing
                #:on-message
                #:defbot)
  (:import-from #:cl-telegram-bot/chat
                #:private-chat
                #:get-username)
  (:import-from #:cl-telegram-bot/message
                #:get-current-chat)
  (:import-from #:serapeum
                #:dict
                #:fmt)
  (:import-from #:cl-telegram-bot/response
                #:alert
                #:notify
                #:reply)
  (:import-from #:cl-telegram-bot/inline-keyboard
                #:callback-button
                #:inline-keyboard))
(in-package #:example-bot/bot)


(defbot example-bot)


(defmethod on-message ((bot example-bot)
                       text)
  (let* ((chat (get-current-chat))
         (username (get-username chat)))
    (log:info "Talking to" username)
    (let ((keyboard (when (string-equal text "show")
                      (inline-keyboard
                       (list 
                        (callback-button "Alert" "alert")
                        (callback-button "Notify" "notify")
                        (callback-button "Text me" "text"))))))
      (reply (fmt "Привет ~A!"
                  username)
             :reply-markup keyboard))))


(defmethod cl-telegram-bot/callback:on-callback ((bot example-bot)
                                                 callback)
  (let ((data (cl-telegram-bot/callback:callback-data callback)))
    (cond
      ((string-equal data
                     "alert")
       (cl-telegram-bot/response:alert "You pressed alert button!"))
      
      ((string-equal data
                     "notify")
       (cl-telegram-bot/response:reply "Just replying with text.")
       (cl-telegram-bot/response:notify "You pressed notify button!"))
      (t
       (cl-telegram-bot/response:reply "Just replying with text.")))))


(defun start (&key token)
  (start-processing (make-example-bot (or token
                                          (uiop:getenv "TELEGRAM_TOKEN")
                                          (error "Define TELEGRAM_TOKEN env var.")))
                    :debug t))
