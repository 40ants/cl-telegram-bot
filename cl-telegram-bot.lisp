; cl-telegram-bot
;
; MIT License
;
; Copyright (c) 2016 Rei <https://github.com/unwind-protect>
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

(in-package :cl-telegram-bot)

(defclass bot ()
  ((id
    :documentation "Update id"
    :initform 0
    :accessor id)
   (token
    :initarg :token
    :documentation "Bot token given by BotFather"
    :accessor token
    :initform nil)
   (endpoint
    :initarg :endpoint
    :accessor endpoint
    :documentation "HTTPS endpoint"
    :initform nil)))

(defmethod initialize-instance :after ((b bot) &key (api-uri "https://api.telegram.org/bot"))
  (setf (endpoint b)
        (concatenate 'string api-uri (token b) "/")))

(defun make-bot (token)
  "Create a new bot instance. Takes a token string."
  (make-instance 'bot :token token))

#+sbcl
(defun get-class-slots (obj)
  "Get a list of class slots, useful to inspect Fluid classes. SBCL only."
  (mapcar #'sb-mop:slot-definition-name
          (sb-mop:class-slots
           (class-of obj))))

(defun make-request (b name options)
  "Perform HTTP request to 'name API method with 'options JSON-encoded object."
  (drakma:http-request
   (concatenate 'string (endpoint b) name)
   :method :post
   :want-stream t
   :content-type "application/json"
   :content (json:encode-json-alist-to-string options)))

(defun access (update &rest args)
  "Access update field. update.first.second. ... => (access update 'first 'second ...). Nil if unbound."
  (unless update
   (return-from access nil))
  (let ((current update))
    (dolist (r args)
      (unless (slot-boundp current r)
        (return-from access nil))
      (setf current (slot-value current r)))
    current))

(defun get-slot (update slot)
  "Access slot. Since fluid classes signal error on unbound slot access, this instead returns nil."
  (if (slot-boundp update slot)
      (slot-value update slot)
    nil))

(defmacro with-package (package &rest body)
 `(let ((json:*json-symbols-package* ,package)) ,@body))

(defmacro decode (obj)
 `(json:with-decoder-simple-clos-semantics
   (json:decode-json ,obj)))

(define-condition request-error (error)
   ((what :initarg :what :reader what)))

(defmacro find-json-symbol (sym)
 `(find-symbol (symbol-name ,sym) json:*json-symbols-package*))

(defmacro trace-http ()
 '(setf drakma:*header-stream* *standard-output*))

; Telegram API methods, see https://core.telegram.org/bots/api

(defun get-updates (b &key limit timeout)
  "https://core.telegram.org/bots/api#getupdates"
  (let* ((current-id (id b))
         (request
          (decode (make-request b "getUpdates"
                        (list (cons :offset current-id)
                              (cons :limit limit)
                              (cons :timeout timeout)))))
         (results (slot-value request (find-json-symbol :result))))
    (when (eql (slot-value request (find-json-symbol :ok)) nil)
      (error 'request-error :what request))
    (when (> (length results) 0)
      (let* ((last-update (elt results (- (length results) 1)))
             (id (slot-value last-update (find-json-symbol :update--id))))
        (when (= current-id 0)
          (setf (id b) id))
        (incf (id b))))
    results))

(defun set-webhook (b &key url certificate)
  "https://core.telegram.org/bots/api#setwebhook"
  (let ((options '()))
       (when url (nconc options `((:url . ,url))))
       (when certificate (nconc options `((:certificate . ,certificate))))
        (make-request b "setWebhook" options)))

(defun send-message (b chat-id text &key parse-mode disable-web-page-preview disable-notification reply)
  "https://core.telegram.org/bots/api#sendmessage"
  (let ((options
         (list 
          (cons :chat_id chat-id)
          (cons :text text))))
       (when parse-mode (nconc options `((:parse_mode . ,parse-mode))))
       (when disable-web-page-preview (nconc options `((:disable_web_page_preview . ,disable-web-page-preview))))
       (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
       (when reply (nconc options `((:reply_to_message_id . ,reply))))
        (make-request b "sendMessage" options)))

(defun forward-message (b chat-id from-chat-id message-id &key disable-notification)
  "https://core.telegram.org/bots/api#forwardmessage"
  (let ((options
         (list 
          (cons :chat_id chat-id)
          (cons :from_chat_id from-chat-id)
          (cons :message_id message-id))))
       (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
        (make-request b "forwardMessage" options)))

(defun send-photo (b chat-id photo &key caption disable-notification reply reply-markup)
  "https://core.telegram.org/bots/api#sendphoto"
  (let ((options
         (list 
          (cons :chat_id chat-id)
          (cons :photo photo))))
       (when caption (nconc options `((:caption . ,caption))))
       (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
       (when reply (nconc options `((:reply_to_message_id . ,reply))))
       (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
        (make-request b "sendPhoto" options)))

(defun send-audio (b chat-id audio &key duration performer title disable-notification reply reply-markup)
  "https://core.telegram.org/bots/api#sendaudio"
  (let ((options
         (list 
          (cons :chat_id chat-id)
          (cons :audio audio))))
       (when duration (nconc options `((:duration . ,duration))))
       (when performer (nconc options `((:performer . ,performer))))
       (when title (nconc options `((:title . ,title))))
       (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
       (when reply (nconc options `((:reply_to_message_id . ,reply))))
       (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
        (make-request b "sendAudio" options)))

(defun send-document (b chat-id document &key caption disable-notification reply reply-markup)
  "https://core.telegram.org/bots/api#senddocument"
  (let ((options
         (list 
          (cons :chat_id chat-id)
          (cons :document document))))
       (when caption (nconc options `((:caption . ,caption))))
       (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
       (when reply (nconc options `((:reply_to_message_id . ,reply))))
       (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
        (make-request b "sendDocument" options)))

(defun send-sticker (b chat-id sticker &key disable-notification reply reply-markup)
  "https://core.telegram.org/bots/api#sendsticker"
  (let ((options
         (list 
          (cons :chat_id chat-id)
          (cons :sticker sticker))))
       (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
       (when reply (nconc options `((:reply_to_message_id . ,reply))))
       (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
        (make-request b "sendSticker" options)))

(defun send-video (b chat-id video &key duration width height caption disable-notification reply reply-markup)
  "https://core.telegram.org/bots/api#sendvideo"
  (let ((options
         (list 
          (cons :chat_id chat-id)
          (cons :video video))))
       (when duration (nconc options `((:duration . ,duration))))
       (when width (nconc options `((:width . ,width))))
       (when height (nconc options `((:height . ,height))))
       (when caption (nconc options `((:caption . ,caption))))
       (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
       (when reply (nconc options `((:reply_to_message_id . ,reply))))
       (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
        (make-request b "sendVideo" options)))

(defun send-voice (b chat-id voice &key duration disable-notification reply reply-markup)
  "https://core.telegram.org/bots/api#sendvoice"
  (let ((options
         (list 
          (cons :chat_id chat-id)
          (cons :voice voice))))
       (when duration (nconc options `((:duration . ,duration))))
       (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
       (when reply (nconc options `((:reply_to_message_id . ,reply))))
       (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
        (make-request b "sendVoice" options)))

(defun send-location (b chat-id latitude longitude &key disable-notification reply reply-markup)
  "https://core.telegram.org/bots/api#sendlocation"
  (let ((options
         (list 
          (cons :chat_id chat-id)
          (cons :latitude latitude)
          (cons :longitude longitude))))
       (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
       (when reply (nconc options `((:reply_to_message_id . ,reply))))
       (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
        (make-request b "sendLocation" options)))

(defun send-venue (b chat-id latitude longitude title address &key foursquare-id disable-notification reply reply-markup)
  "https://core.telegram.org/bots/api#sendvenue"
  (let ((options
         (list 
          (cons :chat_id chat-id)
          (cons :latitude latitude)
          (cons :longitude longitude)
          (cons :title title)
          (cons :address address))))
       (when foursquare-id (nconc options `((:foursquare_id . ,foursquare-id))))
       (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
       (when reply (nconc options `((:reply_to_message_id . ,reply))))
       (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
        (make-request b "sendVenue" options)))

(defun send-contact (b chat-id phone-number first-name &key last-name disable-notification reply reply-markup)
  "https://core.telegram.org/bots/api#sendcontact"
  (let ((options
         (list 
          (cons :chat_id chat-id)
          (cons :phone_number phone-number)
          (cons :first_name first-name))))
       (when last-name (nconc options `((:last_name . ,last-name))))
       (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
       (when reply (nconc options `((:reply_to_message_id . ,reply))))
       (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
        (make-request b "sendContact" options)))

(defun send-chat-action (b chat-id action)
  "https://core.telegram.org/bots/api#sendchataction"
  (let ((options
         (list 
          (cons :chat_id chat-id)
          (cons :action action))))
        (make-request b "sendChatAction" options)))

(defun get-user-profile-photos (b user-id &key offset limit)
  "https://core.telegram.org/bots/api#getuserprofilephotos"
  (let ((options
         (list 
          (cons :user_id user-id))))
       (when offset (nconc options `((:offset . ,offset))))
       (when limit (nconc options `((:limit . ,limit))))
        (make-request b "getUserProfilePhotos" options)))

(defun get-file (b file-id)
  "https://core.telegram.org/bots/api#getfile"
  (let ((options
         (list 
          (cons :file_id file-id))))
        (make-request b "getFile" options)))

(defun kick-chat-member (b chat-id user-id)
  "https://core.telegram.org/bots/api#kickchatmember"
  (let ((options
         (list 
          (cons :chat_id chat-id)
          (cons :user_id user-id))))
        (make-request b "kickChatMember" options)))

(defun leave-chat (b chat-id)
  "https://core.telegram.org/bots/api#leavechat"
  (let ((options
         (list 
          (cons :chat_id chat-id))))
        (make-request b "leaveChat" options)))

(defun unban-chat-member (b chat-id user-id)
  "https://core.telegram.org/bots/api#unbanchatmember"
  (let ((options
         (list 
          (cons :chat_id chat-id)
          (cons :user_id user-id))))
        (make-request b "unbanChatMember" options)))

(defun get-chat (b chat-id)
  "https://core.telegram.org/bots/api#getchat"
  (let ((options
         (list 
          (cons :chat_id chat-id))))
        (make-request b "getChat" options)))

(defun get-chat-administrators (b chat-id)
  "https://core.telegram.org/bots/api#getchatadministrators"
  (let ((options
         (list 
          (cons :chat_id chat-id))))
        (make-request b "getChatAdministrators" options)))

(defun get-chat-members-count (b chat-id)
  "https://core.telegram.org/bots/api#getchatmemberscount"
  (let ((options
         (list 
          (cons :chat_id chat-id))))
        (make-request b "getChatMembersCount" options)))

(defun get-chat-member (b chat-id user-id)
  "https://core.telegram.org/bots/api#getchatmember"
  (let ((options
         (list 
          (cons :chat_id chat-id)
          (cons :user_id user-id))))
        (make-request b "getChatMember" options)))

(defun answer-callback-query (b callback-query-id &key text show-alert)
  "https://core.telegram.org/bots/api#answercallbackquery"
  (let ((options
         (list 
          (cons :callback_query_id callback-query-id))))
       (when text (nconc options `((:text . ,text))))
       (when show-alert (nconc options `((:show_alert . ,show-alert))))
        (make-request b "answerCallbackQuery" options)))

(defun edit-message-text (b chat-id message-id inline-message-id text &key parse-mode disable-web-page-preview reply-markup)
  "https://core.telegram.org/bots/api#editmessagetext"
  (let ((options
         (list 
          (cons :chat_id chat-id)
          (cons :message_id message-id)
          (cons :inline_message_id inline-message-id)
          (cons :text text))))
       (when parse-mode (nconc options `((:parse_mode . ,parse-mode))))
       (when disable-web-page-preview (nconc options `((:disable_web_page_preview . ,disable-web-page-preview))))
       (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
        (make-request b "editMessageText" options)))

(defun edit-message-caption (b chat-id message-id inline-message-id &key caption reply-markup)
  "https://core.telegram.org/bots/api#editmessagecaption"
  (let ((options
         (list 
          (cons :chat_id chat-id)
          (cons :message_id message-id)
          (cons :inline_message_id inline-message-id))))
       (when caption (nconc options `((:caption . ,caption))))
       (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
        (make-request b "editMessageCaption" options)))

(defun edit-message-reply-markup (b chat-id message-id inline-message-id &key reply-markup)
  "https://core.telegram.org/bots/api#editmessagereplymarkup"
  (let ((options
         (list 
          (cons :chat_id chat-id)
          (cons :message_id message-id)
          (cons :inline_message_id inline-message-id))))
       (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
        (make-request b "editMessageReplyMarkup" options)))

(defun answer-inline-query (b inline-query-id results &key cache-time is-personal next-offset switch-pm-text)
  "https://core.telegram.org/bots/api#answerinlinequery"
  (let ((options
         (list 
          (cons :inline_query_id inline-query-id)
          (cons :results results))))
       (when cache-time (nconc options `((:cache_time . ,cache-time))))
       (when is-personal (nconc options `((:is_personal . ,is-personal))))
       (when next-offset (nconc options `((:next_offset . ,next-offset))))
       (when switch-pm-text (nconc options `((:switch_pm_text . ,switch-pm-text))))
        (make-request b "answerInlineQuery" options)))

