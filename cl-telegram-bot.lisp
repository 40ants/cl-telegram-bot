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

(defun decode (obj)
  (let ((cl-json:*json-symbols-package* :cl-telegram-bot)
        (decoded-object
          (json:with-decoder-simple-clos-semantics
            (let ((decoded-json (json:decode-json obj)))
                (with-slots (ok result) decoded-json
                    (values decoded-json
                            (class-of decoded-json) ok result))))))
   decoded-object))

(define-condition request-error (error)
   ((what :initarg :what :reader what)))

; Telegram API methods, see https://core.telegram.org/bots/api

(defun get-updates (b &key limit timeout)
  "https://core.telegram.org/bots/api#getupdates"
  (let* ((current-id (id b))
         (cl-json:*json-symbols-package* :cl-telegram-bot)
         (request
          (decode (make-request b "getUpdates"
                        (list (cons :offset current-id)
                              (cons :limit limit)
                              (cons :timeout timeout)))))
         (results (slot-value request 'result)))

    (when (eql (slot-value request 'ok) nil)
      (error 'request-error :what request))
    (when (> (length results) 0)
      (let* ((last-update (elt results (- (length results) 1)))
             (id (slot-value last-update 'update--id)))
        (when (= current-id 0)
          (setf (id b) id))
        (incf (id b))))
    results))

(defun set-webhook (b &key url certificate)
  "https://core.telegram.org/bots/api#setwebhook"
  (make-request b "setWebhook"
    (list (cons :url url)
          (cons :certificate certificate))))

(defun send-message (b chat-id text &key parse-mode disable-web-page-preview disable-notification reply)
  "https://core.telegram.org/bots/api#sendmessage"
  (make-request b "sendMessage"
    (list (cons :chat-id chat-id)
          (cons :text text)
          (cons :parse-mode parse-mode)
          (cons :disable-web-page-preview disable-web-page-preview)
          (cons :disable-notification disable-notification)
          (cons :reply reply))))

(defun forward-message (b chat-id from-chat-id message-id &key disable-notification)
  "https://core.telegram.org/bots/api#forwardmessage"
  (make-request b "forwardMessage"
    (list (cons :chat-id chat-id)
          (cons :from-chat-id from-chat-id)
          (cons :message-id message-id)
          (cons :disable-notification disable-notification))))

(defun send-photo (b chat-id photo &key caption disable-notification reply reply-markup)
  "https://core.telegram.org/bots/api#sendphoto"
  (make-request b "sendPhoto"
    (list (cons :chat-id chat-id)
          (cons :photo photo)
          (cons :caption caption)
          (cons :disable-notification disable-notification)
          (cons :reply reply)
          (cons :reply-markup reply-markup))))

(defun send-audio (b chat-id audio &key duration performer title disable-notification reply reply-markup)
  "https://core.telegram.org/bots/api#sendaudio"
  (make-request b "sendAudio"
    (list (cons :chat-id chat-id)
          (cons :audio audio)
          (cons :duration duration)
          (cons :performer performer)
          (cons :title title)
          (cons :disable-notification disable-notification)
          (cons :reply reply)
          (cons :reply-markup reply-markup))))

(defun send-document (b chat-id document &key caption disable-notification reply reply-markup)
  "https://core.telegram.org/bots/api#senddocument"
  (make-request b "sendDocument"
    (list (cons :chat-id chat-id)
          (cons :document document)
          (cons :caption caption)
          (cons :disable-notification disable-notification)
          (cons :reply reply)
          (cons :reply-markup reply-markup))))

(defun send-sticker (b chat-id sticker &key disable-notification reply reply-markup)
  "https://core.telegram.org/bots/api#sendsticker"
  (make-request b "sendSticker"
    (list (cons :chat-id chat-id)
          (cons :sticker sticker)
          (cons :disable-notification disable-notification)
          (cons :reply reply)
          (cons :reply-markup reply-markup))))

(defun send-video (b chat-id video &key duration width height caption disable-notification reply reply-markup)
  "https://core.telegram.org/bots/api#sendvideo"
  (make-request b "sendVideo"
    (list (cons :chat-id chat-id)
          (cons :video video)
          (cons :duration duration)
          (cons :width width)
          (cons :height height)
          (cons :caption caption)
          (cons :disable-notification disable-notification)
          (cons :reply reply)
          (cons :reply-markup reply-markup))))

(defun send-voice (b chat-id voice &key duration disable-notification reply reply-markup)
  "https://core.telegram.org/bots/api#sendvoice"
  (make-request b "sendVoice"
    (list (cons :chat-id chat-id)
          (cons :voice voice)
          (cons :duration duration)
          (cons :disable-notification disable-notification)
          (cons :reply reply)
          (cons :reply-markup reply-markup))))

(defun send-location (b chat-id latitude longitude &key disable-notification reply reply-markup)
  "https://core.telegram.org/bots/api#sendlocation"
  (make-request b "sendLocation"
    (list (cons :chat-id chat-id)
          (cons :latitude latitude)
          (cons :longitude longitude)
          (cons :disable-notification disable-notification)
          (cons :reply reply)
          (cons :reply-markup reply-markup))))

(defun send-venue (b chat-id latitude longitude title address &key foursquare-id disable-notification reply reply-markup)
  "https://core.telegram.org/bots/api#sendvenue"
  (make-request b "sendVenue"
    (list (cons :chat-id chat-id)
          (cons :latitude latitude)
          (cons :longitude longitude)
          (cons :title title)
          (cons :address address)
          (cons :foursquare-id foursquare-id)
          (cons :disable-notification disable-notification)
          (cons :reply reply)
          (cons :reply-markup reply-markup))))

(defun send-contact (b chat-id phone-number first-name &key last-name disable-notification reply reply-markup)
  "https://core.telegram.org/bots/api#sendcontact"
  (make-request b "sendContact"
    (list (cons :chat-id chat-id)
          (cons :phone-number phone-number)
          (cons :first-name first-name)
          (cons :last-name last-name)
          (cons :disable-notification disable-notification)
          (cons :reply reply)
          (cons :reply-markup reply-markup))))

(defun send-chat-action (b chat-id action)
  "https://core.telegram.org/bots/api#sendchataction"
  (make-request b "sendChatAction"
    (list (cons :chat-id chat-id)
          (cons :action action))))

(defun get-user-profile-photos (b user-id &key offset limit)
  "https://core.telegram.org/bots/api#getuserprofilephotos"
  (make-request b "getUserProfilePhotos"
    (list (cons :user-id user-id)
          (cons :offset offset)
          (cons :limit limit))))

(defun get-file (b file-id)
  "https://core.telegram.org/bots/api#getfile"
  (make-request b "getFile"
    (list           (cons :file-id file-id))))

(defun kick-chat-member (b chat-id user-id)
  "https://core.telegram.org/bots/api#kickchatmember"
  (make-request b "kickChatMember"
    (list (cons :chat-id chat-id)
          (cons :user-id user-id))))

(defun leave-chat (b chat-id)
  "https://core.telegram.org/bots/api#leavechat"
  (make-request b "leaveChat"
    (list           (cons :chat-id chat-id))))

(defun unban-chat-member (b chat-id user-id)
  "https://core.telegram.org/bots/api#unbanchatmember"
  (make-request b "unbanChatMember"
    (list (cons :chat-id chat-id)
          (cons :user-id user-id))))

(defun get-chat (b chat-id)
  "https://core.telegram.org/bots/api#getchat"
  (make-request b "getChat"
    (list           (cons :chat-id chat-id))))

(defun get-chat-administrators (b chat-id)
  "https://core.telegram.org/bots/api#getchatadministrators"
  (make-request b "getChatAdministrators"
    (list           (cons :chat-id chat-id))))

(defun get-chat-members-count (b chat-id)
  "https://core.telegram.org/bots/api#getchatmemberscount"
  (make-request b "getChatMembersCount"
    (list           (cons :chat-id chat-id))))

(defun get-chat-member (b chat-id user-id)
  "https://core.telegram.org/bots/api#getchatmember"
  (make-request b "getChatMember"
    (list (cons :chat-id chat-id)
          (cons :user-id user-id))))

(defun answer-callback-query (b callback-query-id &key text show-alert)
  "https://core.telegram.org/bots/api#answercallbackquery"
  (make-request b "answerCallbackQuery"
    (list (cons :callback-query-id callback-query-id)
          (cons :text text)
          (cons :show-alert show-alert))))

(defun edit-message-text (b chat-id message-id inline-message-id text &key parse-mode disable-web-page-preview reply-markup)
  "https://core.telegram.org/bots/api#editmessagetext"
  (make-request b "editMessageText"
    (list (cons :chat-id chat-id)
          (cons :message-id message-id)
          (cons :inline-message-id inline-message-id)
          (cons :text text)
          (cons :parse-mode parse-mode)
          (cons :disable-web-page-preview disable-web-page-preview)
          (cons :reply-markup reply-markup))))

(defun edit-message-caption (b chat-id message-id inline-message-id &key caption reply-markup)
  "https://core.telegram.org/bots/api#editmessagecaption"
  (make-request b "editMessageCaption"
    (list (cons :chat-id chat-id)
          (cons :message-id message-id)
          (cons :inline-message-id inline-message-id)
          (cons :caption caption)
          (cons :reply-markup reply-markup))))

(defun edit-message-reply-markup (b chat-id message-id inline-message-id &key reply-markup)
  "https://core.telegram.org/bots/api#editmessagereplymarkup"
  (make-request b "editMessageReplyMarkup"
    (list (cons :chat-id chat-id)
          (cons :message-id message-id)
          (cons :inline-message-id inline-message-id)
          (cons :reply-markup reply-markup))))

(defun answer-inline-query (b inline-query-id results &key cache-time is-personal next-offset switch-pm-text)
  "https://core.telegram.org/bots/api#answerinlinequery"
  (make-request b "answerInlineQuery"
    (list (cons :inline-query-id inline-query-id)
          (cons :results results)
          (cons :cache-time cache-time)
          (cons :is-personal is-personal)
          (cons :next-offset next-offset)
          (cons :switch-pm-text switch-pm-text))))

