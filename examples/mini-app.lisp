(uiop:define-package #:cl-telegram-bot2-examples/mini-app
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
                #:dict
                #:->
                #:fmt)
  (:import-from #:spinneret
                #:with-html-string)
  (:import-from #:ningle)
  (:import-from #:clack)
  (:import-from #:cl-telegram-bot2/api
                #:web-app-data
                #:message-message-id)
  (:import-from #:cl-telegram-bot2/state-with-commands
                #:global-command
                #:command
                #:state-with-commands-mixin)
  (:import-from #:cl-telegram-bot2/state
                #:state)
  (:import-from #:cl-telegram-bot2/actions/send-text
                #:send-text)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:yason
                #:with-output-to-string*)
  (:import-from #:cl-telegram-bot2/high/keyboard
                #:remove-keyboard
                #:open-web-app
                #:keyboard))
(in-package #:cl-telegram-bot2-examples/mini-app)


(defvar *server* nil)


(defun get-mini-app-interface ()
  (or (uiop:getenv "APP_INTERFACE")
      "localhost"))


(defun get-mini-app-port ()
  (parse-integer
   (or (uiop:getenv "APP_PORT")
       "10120")))


(defun get-mini-app-url ()
  (or (uiop:getenv "MINI_APP_URL")
      (fmt "http://~A:~A/"
           (get-miniapp-interface)
           (get-miniapp-port))))


(defun is-mini-app-in-debug-mode-p ()
  (member (uiop:getenv "MINI_APP_DEBUG")
          '("yes" "on" "true" "1")
          :test #'string-equal))


(defparameter *index-page*
  (with-html-string
    (:html :style "background: white"
           (:head
            (:script :src "https://telegram.org/js/telegram-web-app.js")
            (:script :src "https://unpkg.com/htmx.org@2.0.3")
            (:script :src "https://cdn.tailwindcss.com"))
           
           (:body
            (:form :class "flex flex-col gap-8 m-4"
                   (:p :class "text-xl"
                       "To finish the registration, please, enter additional data:")
                   (:div :class "flex flex-col gap-2"
                         (:label :for "email"
                                 :class "text-l"
                                 "Your email address:")
                         (:input :class "border rounded p-2 mx-[-8]"
                                 :type "email"
                                 :name "email"
                                 :placeholder "your@email.com"))
                   (:div :class "flex flex gap-2 items-center"
                         (:label :for "sign"
                                 "Sign to our newsletter:")
                         (:input :type "checkbox"
                                 :name "sign"
                                 :checked t))
                   (:button :class "middle none center mr-4 rounded-lg bg-blue-500 py-3 px-6 font-sans text-xs font-bold uppercase text-white shadow-md shadow-blue-500/20 transition-all hover:shadow-lg hover:shadow-blue-500/40 focus:opacity-[0.85] focus:shadow-none active:opacity-[0.85] active:shadow-none disabled:pointer-events-none disabled:opacity-50 disabled:shadow-none"
                            :hx-post "/clicked"
                            "Submit"))))))


(defun on-click (params)
  (let* ((email (assoc-value params "email" :test #'string-equal))
         (sign (assoc-value params "sign" :test #'string-equal))
         (data (dict "email" email
                     "sign" sign))
         (encoded (with-output-to-string* ()
                    (yason:encode data))))
    (with-html-string
      (:script (:raw (fmt "window.Telegram.WebApp.sendData('~A')"
                          encoded))))))


(defun start-web-app ()
  (let ((app (make-instance 'ningle:app)))
    (uiop:with-muffled-conditions ('(warning))
      (setf (ningle:route app "/")
            *index-page*)

      (setf (ningle:route app "/clicked" :method :POST)
            #'on-click))
  
    (when *server*
      (clack:stop *server*)
      (setf *server* nil))
  
    (setf *server*
          (clack:clackup app
                         :port (get-mini-app-port)
                         :address (get-mini-app-interface)
                         :debug (is-mini-app-in-debug-mode-p))))
  (values))



(defun on-help-command (arg update)
  (declare (ignore arg update))
  (reply "There are two commands are available:

/app - opens a mini-app.
/help - shows this text.
")
  ;; It is important to return nothing if we want switch
  ;; bot to a new state from this handler
  (values))


(defvar *form-data* nil
  "In real app you should save data to the database.")


(-> save-form-data (web-app-data)
    (values &optional))

(defun save-form-data (data)
  (setf *form-data*
        (yason:parse
         (cl-telegram-bot2/api:web-app-data-data data)))
  (values))


(defun format-response-text ()
  (let ((email (gethash "email" *form-data*)))
    (fmt "Thank you for filling the form.

We will send the verification link to `~A`."
         email)))


(defun make-keyboard ()
  (keyboard (open-web-app "Open Mini App"
                          (get-mini-app-url))
            :one-time-keyboard t))


(defbot test-bot ()
  ()
  (:initial-state
   (state (list
           'start-web-app
           (send-text "Initial state. To open the mini-app press this button:"
                      :reply-markup (make-keyboard)))
          :id "mini-app-example"
          :on-web-app-data (list 'save-form-data
                                 (send-text 'format-response-text
                                            :parse-mode "Markdown"
                                            :reply-markup (remove-keyboard)))
          :on-result (send-text "Welcome back!")
          :on-update (send-text "To open the mini-app press this button:"
                                :reply-markup (make-keyboard))
          :commands (list
                     ;; (command "/app"
                     ;;          (send-text "App opening is not implemented yet.")
                     ;;          :description "Open a mini-app.")
                     (global-command "/help" 'on-help-command
                                     :description "Show information about bot's commands.")))))


(defvar *bot* nil)


(defun stop ()
  (when *bot*
    (stop-polling *bot*)
    (setf *bot* nil)))


(defun start ()
  (stop)

  (unless *bot*
    (setf *bot*
          (make-test-bot (uiop:getenv "TELEGRAM_TOKEN"))))
  
  (start-polling *bot* :debug t))

