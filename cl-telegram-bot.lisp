;; cl-telegram-bot
;;
;; MIT License
;;
;; Copyright (c) 2016 Rei <https://github.com/sovietspaceship>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(defpackage :cl-telegram-bot/cl-telegram-bot
  (:use #:cl)
  (:nicknames #:cl-telegram-bot
              #:telegram-bot
              #:tg-bot)
  (:import-from #:log4cl)
  (:import-from #:cl-telegram-bot/update
                #:get-update-id
                #:make-update)
  (:import-from #:cl-ppcre
                #:regex-replace)
  (:import-from #:cl-telegram-bot/chat
                #:get-chat-id)
  (:import-from #:cl-telegram-bot/message
                #:get-chat)
  (:export
   #:bot
   #:make-bot
   #:access
   #:get-updates
   #:set-webhook
   #:get-webhook-info
   #:send-message
   #:forward-message
   #:send-photo
   #:send-audio
   #:send-document
   #:send-video
   #:send-voice
   #:send-video-note
   #:send-location
   #:send-venue
   #:send-contact
   #:send-chat-action
   #:get-user-profile-photos
   #:get-file
   #:kick-chat-member
   #:unban-chat-member
   #:restrict-chat-member
   #:promote-chat-member
   #:export-chat-invite-link
   #:set-chat-photo
   #:delete-chat-photo
   #:set-chat-title
   #:set-chat-description
   #:pin-chat-message
   #:unpin-chat-message
   #:leave-chat
   #:get-chat-by-id
   #:get-chat-administrators
   #:get-chat-members-count
   #:get-chat-member
   #:answer-callback-query
   #:edit-message-text
   #:edit-message-caption
   #:edit-message-reply-markup
   #:delete-message
   #:send-sticker
   #:get-sticker-set
   #:upload-sticker-file
   #:create-new-sticker-set
   #:add-sticker-to-set
   #:set-sticker-position-in-set
   #:delete-sticker-from-set
   #:answer-inline-query
   #:send-invoice
   #:answer-shipping-query
   #:answer-pre-checkout-query
   #:send-game
   #:set-game-score
   #:get-game-high-scores))

(in-package :cl-telegram-bot/cl-telegram-bot)

(alexandria:define-constant +http-ok+ 200 :test #'=)

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
   (api-uri
    :initarg  :api-uri
    :initform "https://api.telegram.org/"
    :accessor api-uri)
   (endpoint
    :initarg :endpoint
    :accessor endpoint
    :documentation "HTTPS endpoint")
   (file-endpoint
    :initarg :file-endpoint
    :accessor file-endpoint
    :documentation "HTTPS file-endpoint"
    :initform nil)))

(defmethod initialize-instance :after ((object bot) &key &allow-other-keys)
  (with-accessors ((token         token)
                   (endpoint      endpoint)
                   (file-endpoint file-endpoint)
                   (api-uri       api-uri)) object
                   (setf endpoint      (concatenate 'string api-uri "bot" token "/")
                         file-endpoint (concatenate 'string api-uri "file/" "bot" token "/"))))


(defmethod print-object ((bot bot) stream)
  (print-unreadable-object
      (bot stream :type t)
    (format stream
            "id=~A" (id bot))))

(defun make-bot (token)
  "Create a new bot instance. Takes a token string."
  (make-instance 'bot :token token))

#+sbcl
(defun get-class-slots (obj)
  "Get a list of class slots, useful to inspect Fluid classes. SBCL only."
  (mapcar #'sb-mop:slot-definition-name
          (sb-mop:class-slots
           (class-of obj))))

(defun obfuscate (url)
  (regex-replace "/bot.*?/"
                 url
                 "/bot<token>/"))


(defun make-request (b name options &key (streamp nil))
  "Perform HTTP request to 'name API method with 'options JSON-encoded object."
  (let ((url (concatenate 'string (endpoint b) name)))
    (log:debug "Posting data to"
               (obfuscate url)
               options)
    (dexador:post url
                  :stream streamp
                  :headers '(("Content-Type" . "application/json"))
                  :content (jonathan:to-json options))))

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

(defgeneric decode (object))

(defmethod decode ((object stream))
  (error "Decode is not implemented for stream.")
  (json:with-decoder-simple-clos-semantics
    (prog1
        (json:decode-json object)
      (close object))))


(defmethod decode ((object string))
  (jonathan:parse object))


(defmethod decode ((object vector))
  (error "Decode is not implemented for vector.")
  (decode (map 'string #'code-char object)))

(define-condition request-error (error)
  ((what :initarg :what :reader what))
  (:report (lambda (condition stream)
             (format stream "Request error: ~A" (what condition)))))

(defmacro find-json-symbol (sym)
  `(find-symbol (symbol-name ,sym) json:*json-symbols-package*))

(defmacro trace-http ()
  '(setf drakma:*header-stream* *standard-output*))

(defun download-file (b file-id)
  "Get the  path for a  file from a  file-id (see: get-file)  and then
   download it.  Returns nil if the value of the http response code is
   not  success (200);  otherwise it  will returns  three values:  the
   data, the http headers and the exension of the original file"
  (with-package :cl-telegram-bot
                (let* ((file-spec (decode (get-file b file-id))))
                  (with-ok-results (file-spec results)
                                   (alexandria:when-let* ((path      (access results 'file--path))
                                               (uri       (concatenate 'string (file-endpoint b) path))
                                               (extension (cl-ppcre:scan-to-strings "\\..*$" path)))
                                              (multiple-value-bind (body code headers)
                                                  (drakma:http-request uri :method :get)
                                                (when (= code +http-ok+)
                                                  (values body headers extension))))))))

;; Telegram API methods, see https://core.telegram.org/bots/api

(defmacro with-ok-results ((unserialized results) &body body)
  `(let ((,results (slot-value ,unserialized (find-json-symbol :result))))
     (if (slot-value ,unserialized (find-json-symbol :ok))
         (progn ,@body)
       nil)))


(defun get-updates (bot &key limit timeout)
  "https://core.telegram.org/bots/api#getupdates"
  (let* ((current-id (id bot))
         (response (decode (make-request bot "getUpdates"
                                         (list :|offset| current-id
                                               :|limit| limit
                                               :|timeout| timeout)
                                         :streamp t)))
         (results (getf response :|result|)))
    (when (null (getf response :|ok|))
      (error 'request-error :what response))

    
    (let* ((updates (mapcar 'make-update results))
           ;; Make update can return a nil if this type of
           ;; update is not supported
           (updates (remove-if #'null
                               updates)))
      (when updates
        (let ((max-id (reduce #'max
                              updates
                              :key #'get-update-id)))
          ;; In original cl-telegram-bot a bug was here, because
          ;; it saved update's id only the first time, and after that,
          ;; just incremented that value
          (log:debug "Setting new" max-id)
          (setf (id bot)
                (+ max-id 1))))
    
      (values updates))))

;; Compiled method list

(defun set-webhook (b url &key certificate max-connections allowed-updates)
  "https://core.telegram.org/bots/api#setwebhook"
  (let ((options
         (list
          (cons :url url))))
    (when certificate (nconc options `((:certificate . ,certificate))))
    (when max-connections (nconc options `((:max_connections . ,max-connections))))
    (when allowed-updates (nconc options `((:allowed_updates . ,allowed-updates))))
    (make-request b "setWebhook" options)))


(defun get-webhook-info (bot)
  "https://core.telegram.org/bots/api#getwebhookinfo"
  (log:debug "Retriving webhook info")
  (make-request bot "getWebhookInfo" nil))


(defun send-message (bot chat text &key
                                     parse-mode
                                     disable-web-page-preview
                                     disable-notification
                                     reply-to-message-id)
  "https://core.telegram.org/bots/api#sendmessage"
  (log:debug "Sending message" chat text)
  (let ((options
          (append
           `(:|chat_id| ,(get-chat-id chat)
              :|text| ,text)
           (when parse-mode
             `(:|parse_mode| ,parse-mode))
           (when disable-web-page-preview
             `(:disable_web_page_preview ,disable-web-page-preview))
           (when disable-notification
             `(:disable_notification ,disable-notification))
           (when reply-to-message-id
             `(:reply_to_message_id ,reply-to-message-id)))))
    (make-request bot "sendMessage" options)))


(defun reply (bot message &rest args)
  "Works like a send-message, and accepts almost same options,
   but automatically sends to a chat from where message
   came from."
  (log:debug "Replying to" message)
  (apply #'send-message
         bot
         (get-chat message)
         args))


(defun forward-message (b chat-id from-chat-id message-id &key disable-notification)
  "https://core.telegram.org/bots/api#forwardmessage"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :from_chat_id from-chat-id)
          (cons :message_id message-id))))
    (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
    (make-request b "forwardMessage" options)))

(defun send-photo (b chat-id photo &key caption disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendphoto"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :photo photo))))
    (when caption (nconc options `((:caption . ,caption))))
    (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
    (when reply-to-message-id (nconc options `((:reply_to_message_id . ,reply-to-message-id))))
    (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
    (make-request b "sendPhoto" options)))

(defun send-audio (b chat-id audio &key caption duration performer title disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendaudio"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :audio audio))))
    (when caption (nconc options `((:caption . ,caption))))
    (when duration (nconc options `((:duration . ,duration))))
    (when performer (nconc options `((:performer . ,performer))))
    (when title (nconc options `((:title . ,title))))
    (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
    (when reply-to-message-id (nconc options `((:reply_to_message_id . ,reply-to-message-id))))
    (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
    (make-request b "sendAudio" options)))

(defun send-document (b chat-id document &key caption disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#senddocument"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :document document))))
    (when caption (nconc options `((:caption . ,caption))))
    (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
    (when reply-to-message-id (nconc options `((:reply_to_message_id . ,reply-to-message-id))))
    (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
    (make-request b "sendDocument" options)))

(defun send-video (b chat-id video &key duration width height caption disable-notification reply-to-message-id reply-markup)
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
    (when reply-to-message-id (nconc options `((:reply_to_message_id . ,reply-to-message-id))))
    (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
    (make-request b "sendVideo" options)))

(defun send-voice (b chat-id voice &key caption duration disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendvoice"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :voice voice))))
    (when caption (nconc options `((:caption . ,caption))))
    (when duration (nconc options `((:duration . ,duration))))
    (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
    (when reply-to-message-id (nconc options `((:reply_to_message_id . ,reply-to-message-id))))
    (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
    (make-request b "sendVoice" options)))

(defun send-video-note (b chat-id video-note &key duration length disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendvideonote"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :video_note video-note))))
    (when duration (nconc options `((:duration . ,duration))))
    (when length (nconc options `((:length . ,length))))
    (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
    (when reply-to-message-id (nconc options `((:reply_to_message_id . ,reply-to-message-id))))
    (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
    (make-request b "sendVideoNote" options)))

(defun send-location (b chat-id latitude longitude &key disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendlocation"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :latitude latitude)
          (cons :longitude longitude))))
    (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
    (when reply-to-message-id (nconc options `((:reply_to_message_id . ,reply-to-message-id))))
    (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
    (make-request b "sendLocation" options)))

(defun send-venue (b chat-id latitude longitude title address &key foursquare-id disable-notification reply-to-message-id reply-markup)
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
    (when reply-to-message-id (nconc options `((:reply_to_message_id . ,reply-to-message-id))))
    (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
    (make-request b "sendVenue" options)))

(defun send-contact (b chat-id phone-number first-name &key last-name disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendcontact"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :phone_number phone-number)
          (cons :first_name first-name))))
    (when last-name (nconc options `((:last_name . ,last-name))))
    (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
    (when reply-to-message-id (nconc options `((:reply_to_message_id . ,reply-to-message-id))))
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

(defun kick-chat-member (b chat-id user-id until-date)
  "https://core.telegram.org/bots/api#kickchatmember"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :user_id user-id)
          (cons :until_date until-date))))
    (make-request b "kickChatMember" options)))

(defun unban-chat-member (b chat-id user-id)
  "https://core.telegram.org/bots/api#unbanchatmember"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :user_id user-id))))
    (make-request b "unbanChatMember" options)))

(defun restrict-chat-member (b chat-id user-id until-date can-send-messages can-send-media-messages can-send-other-messages can-add-web-page-previews)
  "https://core.telegram.org/bots/api#restrictchatmember"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :user_id user-id)
          (cons :until_date until-date)
          (cons :can_send_messages can-send-messages)
          (cons :can_send_media_messages can-send-media-messages)
          (cons :can_send_other_messages can-send-other-messages)
          (cons :can_add_web_page_previews can-add-web-page-previews))))
    (make-request b "restrictChatMember" options)))

(defun promote-chat-member (b chat-id user-id can-change-info can-post-messages can-edit-messages can-delete-messages can-invite-users can-restrict-members can-pin-messages can-promote-members)
  "https://core.telegram.org/bots/api#promotechatmember"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :user_id user-id)
          (cons :can_change_info can-change-info)
          (cons :can_post_messages can-post-messages)
          (cons :can_edit_messages can-edit-messages)
          (cons :can_delete_messages can-delete-messages)
          (cons :can_invite_users can-invite-users)
          (cons :can_restrict_members can-restrict-members)
          (cons :can_pin_messages can-pin-messages)
          (cons :can_promote_members can-promote-members))))
    (make-request b "promoteChatMember" options)))

(defun export-chat-invite-link (b chat-id)
  "https://core.telegram.org/bots/api#exportchatinvitelink"
  (let ((options
         (list
          (cons :chat_id chat-id))))
    (make-request b "exportChatInviteLink" options)))

(defun set-chat-photo (b chat-id photo)
  "https://core.telegram.org/bots/api#setchatphoto"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :photo photo))))
    (make-request b "setChatPhoto" options)))

(defun delete-chat-photo (b chat-id)
  "https://core.telegram.org/bots/api#deletechatphoto"
  (let ((options
         (list
          (cons :chat_id chat-id))))
    (make-request b "deleteChatPhoto" options)))

(defun set-chat-title (b chat-id title)
  "https://core.telegram.org/bots/api#setchattitle"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :title title))))
    (make-request b "setChatTitle" options)))

(defun set-chat-description (b chat-id description)
  "https://core.telegram.org/bots/api#setchatdescription"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :description description))))
    (make-request b "setChatDescription" options)))

(defun pin-chat-message (b chat-id message-id disable-notification)
  "https://core.telegram.org/bots/api#pinchatmessage"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :message_id message-id)
          (cons :disable_notification disable-notification))))
    (make-request b "pinChatMessage" options)))

(defun unpin-chat-message (b chat-id)
  "https://core.telegram.org/bots/api#unpinchatmessage"
  (let ((options
         (list
          (cons :chat_id chat-id))))
    (make-request b "unpinChatMessage" options)))

(defun leave-chat (b chat-id)
  "https://core.telegram.org/bots/api#leavechat"
  (let ((options
         (list
          (cons :chat_id chat-id))))
    (make-request b "leaveChat" options)))

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

(defun answer-callback-query (b callback-query-id &key text show-alert url)
  "https://core.telegram.org/bots/api#answercallbackquery"
  (let ((options
         (list
          (cons :callback_query_id callback-query-id))))
    (when text (nconc options `((:text . ,text))))
    (when show-alert (nconc options `((:show_alert . ,show-alert))))
    (when url (nconc options `((:url . ,url))))
    (make-request b "answerCallbackQuery" options)))

(defun edit-message-text (b text &key chat-id message-id inline-message-id parse-mode disable-web-page-preview reply-markup)
  "https://core.telegram.org/bots/api#editmessagetext"
  (let ((options
         (list
          (cons :text text))))
    (when chat-id (nconc options `((:chat_id . ,chat-id))))
    (when message-id (nconc options `((:message_id . ,message-id))))
    (when inline-message-id (nconc options `((:inline_message_id . ,inline-message-id))))
    (when parse-mode (nconc options `((:parse_mode . ,parse-mode))))
    (when disable-web-page-preview (nconc options `((:disable_web_page_preview . ,disable-web-page-preview))))
    (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
    (make-request b "editMessageText" options)))

(defun edit-message-caption (b &key chat-id message-id inline-message-id caption reply-markup)
  "https://core.telegram.org/bots/api#editmessagecaption"
  (let ((options '()))
    (when chat-id (nconc options `((:chat_id . ,chat-id))))
    (when message-id (nconc options `((:message_id . ,message-id))))
    (when inline-message-id (nconc options `((:inline_message_id . ,inline-message-id))))
    (when caption (nconc options `((:caption . ,caption))))
    (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
    (make-request b "editMessageCaption" options)))

(defun edit-message-reply-markup (b &key chat-id message-id inline-message-id reply-markup)
  "https://core.telegram.org/bots/api#editmessagereplymarkup"
  (let ((options '()))
    (when chat-id (nconc options `((:chat_id . ,chat-id))))
    (when message-id (nconc options `((:message_id . ,message-id))))
    (when inline-message-id (nconc options `((:inline_message_id . ,inline-message-id))))
    (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
    (make-request b "editMessageReplyMarkup" options)))

(defun delete-message (b chat-id message-id)
  "https://core.telegram.org/bots/api#deletemessage"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :message_id message-id))))
    (make-request b "deleteMessage" options)))

(defun send-sticker (b chat-id sticker &key disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendsticker"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :sticker sticker))))
    (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
    (when reply-to-message-id (nconc options `((:reply_to_message_id . ,reply-to-message-id))))
    (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
    (make-request b "sendSticker" options)))

(defun get-sticker-set (b name)
  "https://core.telegram.org/bots/api#getstickerset"
  (let ((options
         (list
          (cons :name name))))
    (make-request b "getStickerSet" options)))

(defun upload-sticker-file (b user-id png-sticker)
  "https://core.telegram.org/bots/api#uploadstickerfile"
  (let ((options
         (list
          (cons :user_id user-id)
          (cons :png_sticker png-sticker))))
    (make-request b "uploadStickerFile" options)))

(defun create-new-sticker-set (b user-id name title png-sticker emojis &key contains-masks mask-position)
  "https://core.telegram.org/bots/api#createnewstickerset"
  (let ((options
         (list
          (cons :user_id user-id)
          (cons :name name)
          (cons :title title)
          (cons :png_sticker png-sticker)
          (cons :emojis emojis))))
    (when contains-masks (nconc options `((:contains_masks . ,contains-masks))))
    (when mask-position (nconc options `((:mask_position . ,mask-position))))
    (make-request b "createNewStickerSet" options)))

(defun add-sticker-to-set (b user-id name png-sticker emojis &key mask-position)
  "https://core.telegram.org/bots/api#addstickertoset"
  (let ((options
         (list
          (cons :user_id user-id)
          (cons :name name)
          (cons :png_sticker png-sticker)
          (cons :emojis emojis))))
    (when mask-position (nconc options `((:mask_position . ,mask-position))))
    (make-request b "addStickerToSet" options)))

(defun set-sticker-position-in-set (b sticker position)
  "https://core.telegram.org/bots/api#setstickerpositioninset"
  (let ((options
         (list
          (cons :sticker sticker)
          (cons :position position))))
    (make-request b "setStickerPositionInSet" options)))

(defun delete-sticker-from-set (b sticker)
  "https://core.telegram.org/bots/api#deletestickerfromset"
  (let ((options
         (list
          (cons :sticker sticker))))
    (make-request b "deleteStickerFromSet" options)))

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

(defun send-invoice (b chat-id title description payload provider-token start-parameter currency prices &key photo-url photo-size photo-width photo-height need-name need-phone-number need-email need-shipping-address is-flexible disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendinvoice"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :title title)
          (cons :description description)
          (cons :payload payload)
          (cons :provider_token provider-token)
          (cons :start_parameter start-parameter)
          (cons :currency currency)
          (cons :prices prices))))
    (when photo-url (nconc options `((:photo_url . ,photo-url))))
    (when photo-size (nconc options `((:photo_size . ,photo-size))))
    (when photo-width (nconc options `((:photo_width . ,photo-width))))
    (when photo-height (nconc options `((:photo_height . ,photo-height))))
    (when need-name (nconc options `((:need_name . ,need-name))))
    (when need-phone-number (nconc options `((:need_phone_number . ,need-phone-number))))
    (when need-email (nconc options `((:need_email . ,need-email))))
    (when need-shipping-address (nconc options `((:need_shipping_address . ,need-shipping-address))))
    (when is-flexible (nconc options `((:is_flexible . ,is-flexible))))
    (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
    (when reply-to-message-id (nconc options `((:reply_to_message_id . ,reply-to-message-id))))
    (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
    (make-request b "sendInvoice" options)))

(defun answer-shipping-query (b shipping-query-id ok &key shipping-options error-message)
  "https://core.telegram.org/bots/api#answershippingquery"
  (let ((options
         (list
          (cons :shipping_query_id shipping-query-id)
          (cons :ok ok))))
    (when shipping-options (nconc options `((:shipping_options . ,shipping-options))))
    (when error-message (nconc options `((:error_message . ,error-message))))
    (make-request b "answerShippingQuery" options)))

(defun answer-pre-checkout-query (b pre-checkout-query-id ok &key error-message)
  "https://core.telegram.org/bots/api#answerprecheckoutquery"
  (let ((options
         (list
          (cons :pre_checkout_query_id pre-checkout-query-id)
          (cons :ok ok))))
    (when error-message (nconc options `((:error_message . ,error-message))))
    (make-request b "answerPreCheckoutQuery" options)))

(defun send-game (b chat-id game-short-name &key disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendgame"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :game_short_name game-short-name))))
    (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
    (when reply-to-message-id (nconc options `((:reply_to_message_id . ,reply-to-message-id))))
    (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
    (make-request b "sendGame" options)))

(defun set-game-score (b user-id score &key force disable-edit-message chat-id message-id inline-message-id)
  "https://core.telegram.org/bots/api#setgamescore"
  (let ((options
         (list
          (cons :user_id user-id)
          (cons :score score))))
    (when force (nconc options `((:force . ,force))))
    (when disable-edit-message (nconc options `((:disable_edit_message . ,disable-edit-message))))
    (when chat-id (nconc options `((:chat_id . ,chat-id))))
    (when message-id (nconc options `((:message_id . ,message-id))))
    (when inline-message-id (nconc options `((:inline_message_id . ,inline-message-id))))
    (make-request b "setGameScore" options)))

(defun get-game-high-scores (b user-id &key chat-id message-id inline-message-id)
  "https://core.telegram.org/bots/api#getgamehighscores"
  (let ((options
         (list
          (cons :user_id user-id))))
    (when chat-id (nconc options `((:chat_id . ,chat-id))))
    (when message-id (nconc options `((:message_id . ,message-id))))
    (when inline-message-id (nconc options `((:inline_message_id . ,inline-message-id))))
    (make-request b "getGameHighScores" options)))
