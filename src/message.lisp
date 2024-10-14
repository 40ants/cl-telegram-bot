(uiop:define-package #:cl-telegram-bot/message
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:cl-telegram-bot/chat
                #:get-chat-id
                #:make-chat
                #:chat
                #:get-chat)
  (:import-from #:cl-telegram-bot/entities/generic
                #:make-entity)
  (:import-from #:cl-telegram-bot/network
                #:make-request)
  (:import-from #:cl-telegram-bot/pipeline
                #:process)
  (:import-from #:cl-telegram-bot/bot
                #:bot)
  (:import-from #:serapeum
                #:defvar-unbound)
  (:import-from #:cl-telegram-bot/utils
                #:split-by-lines
                #:def-telegram-call)
  (:import-from #:cl-telegram-bot/response-processing
                #:process-response
                #:interrupt-processing)
  (:import-from #:alexandria
                #:remove-from-plistf)
  (:export #:animation
           #:animation-message
           #:audio
           #:audio-message
           #:delete-message
           #:document
           #:document-message
           #:file
           #:file-message
           #:forward-message
           #:get-caption
           #:get-current-chat
           #:get-current-message
           #:get-duration
           #:get-emoji
           #:get-entities
           #:get-file
           #:get-file-id
           #:get-file-name
           #:get-file-size
           #:get-file-unique-id
           #:get-forward-from
           #:get-forward-from-chat
           #:get-forward-sender-name
           #:get-height
           #:get-is-animated
           #:get-is-video
           #:get-length
           #:get-message-id
           #:get-mime-type
           #:get-performer
           #:get-photo-options
           #:get-raw-data
           #:get-reply-to-message
           #:get-set-name
           #:get-text
           #:get-title
           #:get-width
           #:make-message
           #:message
           #:on-message
           #:photo
           #:photo-message
           #:reply
           #:send-animation
           #:send-audio
           #:send-document
           #:send-message
           #:send-photo
           #:send-sticker
           #:send-video
           #:send-video-note
           #:send-voice
           #:spatial
           #:sticker
           #:sticker-message
           #:temporal
           #:unispatial
           #:video
           #:video-message
           #:video-note
           #:video-note-message
           #:voice
           #:voice-message
           #:get-sender-chat
           #:get-current-bot))
(in-package cl-telegram-bot/message)


(defvar-unbound *current-bot*
  "An internal variable to hold current bot for replying.")

(defvar-unbound *current-message*
  "An internal variable to hold current message for replying.")


(defclass message ()
  ((id :initarg :id
       :reader get-message-id)
   (text :initarg :text
         :type (or null string)
         :reader get-text)
   ;; Caption for file messages
   (caption :initarg :caption
            :type (or null string)
            :reader get-caption
            :documentation "Caption for the animation, audio, document, photo, video or voice.")
   (chat :initarg :chat
         :type chat
         :reader get-chat)
   (entities :initarg :entities
             :initform nil
             :type list
             :reader get-entities)
   (raw-data :initarg :raw-data
             :reader get-raw-data)
   (sender-chat :initarg :sender-chat
                :type (or null chat)
                :reader get-sender-chat
                :documentation "Sender of the message, sent on behalf of a chat. For example, the channel itself for channel posts, the supergroup itself for messages from anonymous group administrators, the linked channel for messages automatically forwarded to the discussion group.")
   (forward-from :initarg :forward-from
                 :type (or null chat)
                 :reader get-forward-from
                 :documentation "For forwarded messages, sender of the original message.")
   (forward-sender-name :initarg :forward-sender-name
                        :type (or null string)
                        :reader get-forward-sender-name
                        :documentation "For forwarded messages, sender of the original message.")
   (forward-from-chat :initarg :forward-from-chat
                      :type (or null chat)
                      :reader get-forward-from-chat
                      :documentation "For messages forwarded from channels or from anonymous
administrators, information about the original sender chat.")))

(defmethod initialize-instance :after ((message message) &key data &allow-other-keys)
  (when data
    (setf (slot-value message 'id) (getf data :|message_id|)
          (slot-value message 'text) (getf data :|text|)
          (slot-value message 'chat) (make-chat (getf data :|chat|))
          (slot-value message 'entities) (mapcar (lambda (item)
                                                   (make-entity message item))
                                                 (getf data :|entities|))
          (slot-value message 'raw-data) data
          (slot-value message 'sender-chat) (when (getf data :|sender_chat|)
                                              (make-chat (getf data :|sender_chat|)))
          (slot-value message 'forward-from-chat) (when (getf data :|forward_from_chat|)
                                                    (make-chat (getf data :|forward_from_chat|)))
          (slot-value message 'forward-from) (when (getf data :|forward_from|)
                                               (make-chat (getf data :|forward_from|)))
          (slot-value message 'forward-sender-name) (getf data :|forward_sender_name|)
          (slot-value message 'caption) (getf data :|caption|))))

(defclass temporal ()
  ((duration
    :initarg :duration
    :type (or null integer)
    :reader get-duration
    :documentation "Duration of the file in seconds as defined by sender.")))

(defclass spatial ()
  ((height
    :initarg :height
    :type (or null integer)
    :reader get-height
    :documentation "File height as defined by sender.")
   (width
    :initarg :width
    :type (or null integer)
    :reader get-width
    :documentation "File width as defined by sender.")))

(defclass unispatial ()
  ((length
    :initarg :length
    :type (or null integer)
    :reader get-length)))

(defclass file ()
  ((file-id
    :initarg :file-id
    :type (or null string)
    :reader get-file-id
    :documentation "Identifier for this file, which can be used to download or reuse the file.")
   (file-unique-id
    :initarg :file-unique-id
    :type (or null string)
    :reader get-file-unique-id
    :documentation "Unique identifier for this file, which is supposed to be the same
over time and for different bots. Can't be used to download or reuse
the file.")
   (file-name
    :initarg :file-name
    :type (or null string)
    :reader get-file-name
    :documentation "Original filename as defined by sender.")
   (file-size
    :initarg :file-size
    :type (or null integer)
    :reader get-file-size
    :documentation "File size in bytes.")
   (mime-type
    :initarg :mime-type
    :type (or null string)
    :reader get-mime-type
    :documentation "MIME type of the file as defined by sender.")))

(defclass photo (file spatial) ())

(defclass audio (file temporal)
  ((performer
    :initarg :performer
    :type (or null string)
    :reader get-performer
    :documentation "Performer of the audio as defined by sender or by audio tags.")
   (title
    :initarg :title
    :type (or null string)
    :reader get-title
    :documentation "Title of the audio as defined by sender or by audio tags.")))

(defclass animation (file temporal spatial) ())

(defclass document (file) ())

(defclass video (file temporal spatial) ())

(defclass video-note (file temporal unispatial) ())

(defclass voice (file temporal) ())

;; TODO: premium_animation, thumb, mask_position
(defclass sticker (file spatial)
  ((is-animated
    :initarg :is-animated
    :reader get-is-animated
    :documentation "True if the sticker is animated.")
   (is-video
    :initarg :is-video
    :reader get-is-video
    :documentation "True if the sticker is a video sticker.")
   (emoji
    :initarg :emoji
    :reader get-emoji
    :documentation "Emoji associated with the sticker")
   (set-name
    :initarg :set-name
    :type (or null string)
    :reader get-set-name
    :documentation "Name of the sticker set to which the sticker belongs.")))

(defmethod initialize-instance :after ((file file) &key data &allow-other-keys)
  (when data
    (let ((slots (mapcar #'closer-mop:slot-definition-name
                         (closer-mop:class-slots (class-of file)))))
      (mapc
       (lambda (slot underscored)
         (setf (slot-value file slot) (getf data underscored)))
       slots
       (mapcar (alexandria:compose #'kebab:to-snake-case #'alexandria:make-keyword)
               slots)))))

(defclass file-message (message)
  ((file :initarg :file
         :reader get-file)))

(defmethod set-file ((message file-message) data &key file-attribute-name file-class &allow-other-keys)
  (setf (slot-value message 'file)
        (make-instance file-class :data (getf data file-attribute-name))))

(defmethod initialize-instance :after ((message file-message)
                                       &key data file-attribute-name file-class &allow-other-keys)
  (when data
    (set-file message data :file-class file-class :file-attribute-name file-attribute-name)))

(defclass audio-message (file-message) ())

(defclass document-message (file-message) ())

(defclass animation-message (file-message) ())

(defclass photo-message (file-message)
  ((photo-options
    :initarg :photo-options
    :reader get-photo-options)))

(defmethod set-file ((message photo-message) data &key &allow-other-keys)
  (setf (slot-value message 'photo-options) (mapcar
                                             (lambda (option) (make-instance 'photo :data option))
                                             (getf data :|photo|))
        (slot-value message 'file) (alexandria:lastcar (slot-value message 'photo-options))))

(defclass sticker-message (file-message) ())

(defclass video-message (file-message) ())

(defclass video-note-message (file-message) ())

(defclass voice-message (file-message) ())


(defclass reply (message)
  ((reply-to-message :initarg :reply-to-message
                     :reader get-reply-to-message)))


(defmethod initialize-instance :after ((reply reply) &key data &allow-other-keys)
  (when data
    (setf (slot-value reply 'reply-to-message)
          (make-message (getf data :|reply_to_message|)))))


(defun make-message (data)
  (when data
    (destructuring-bind (class &optional file-attribute-name file-class)
        (cond
          ((getf data :|reply_to_message|) '(reply))
          ((getf data :|audio|) '(audio-message :|audio| audio))
          ((getf data :|animation|) '(animation-message :|animation| animation))
          ((getf data :|document|) '(document-message :|document| document))
          ((getf data :|photo|) '(photo-message))
          ((getf data :|sticker|) '(sticker-message :|sticker| sticker))
          ((getf data :|video|) '(video-message :|video| video))
          ((getf data :|video-note|) '(video-note-message :|video_note| vide-note))
          ((getf data :|voice|) '(voice-message :|voice| voice))
          (t '(message)))
      (make-instance
       class :data data
       :file-attribute-name file-attribute-name
       :file-class file-class))))


(defmethod print-object ((message message) stream)
  (print-unreadable-object
      (message stream :type t)
    (format stream
            "text=~A chat=~A"
            (get-text message)
            (get-chat message))))


(defun send-message (bot chat text
                     &rest options
                     &key parse-mode
                          disable-web-page-preview
                          disable-notification
                          reply-to-message-id
                          (autosplit nil)
                          reply-markup)
  "https://core.telegram.org/bots/api#sendmessage"
  (declare (ignorable parse-mode disable-web-page-preview disable-notification
                      reply-to-message-id reply-markup))
  (log:debug "Sending message" chat text)

  (remove-from-plistf options :autosplit)

  (flet ((send (text)
           (apply #'make-request bot "sendMessage"
                  :|chat_id| (typecase chat
                               (string chat)
                               (t
                                (get-chat-id chat)))
                  :|text| text
                  options)))
    (cond
      ((and (serapeum:length< 4096 text)
            autosplit)
       (mapc #'send
             (split-by-lines text :max-size 4096)))
      (t
       (send text)))))


(defgeneric send-photo (bot chat photo
                        &rest options
                        &key caption parse-mode caption-entities
                             disable-notification protect-content reply-to-message-id
                             allow-sending-without-reply reply-markup))

(defmethod send-photo (bot chat (photo string)
                       &rest options
                       &key caption parse-mode caption-entities
                         disable-notification protect-content reply-to-message-id
                         allow-sending-without-reply reply-markup)
  "A method for photo sending based on photo ID.

The file-based method does not work yet.

https://core.telegram.org/bots/api#sendphoto"
  (declare (ignorable caption parse-mode caption-entities
                      disable-notification protect-content reply-to-message-id
                      allow-sending-without-reply reply-markup))
  (log:debug "Sending photo" chat photo)
  (apply #'make-request bot "sendPhoto"
         :|chat_id| (get-chat-id chat)
         :|photo| photo
         options))

(defmethod send-photo (bot chat (photo photo)
                       &rest options
                       &key caption parse-mode caption-entities
                         disable-notification protect-content reply-to-message-id
                         allow-sending-without-reply reply-markup)
  "A method for photo sending based on photo object.

https://core.telegram.org/bots/api#sendphoto"
  (declare (ignorable caption parse-mode caption-entities
                      disable-notification protect-content reply-to-message-id
                      allow-sending-without-reply reply-markup))
  (log:debug "Sending photo" chat (get-file-name photo))
  (apply #'send-photo bot chat (get-file-id photo) options))

(defmethod send-audio (bot chat (audio string)
                       &rest options
                       &key caption parse-mode caption-entities
                         duration performer title thumb
                         disable-notification protect-content reply-to-message-id
                         allow-sending-without-reply reply-markup)
  "A method for audio sending based on its ID.

The file-based method does not work yet.

https://core.telegram.org/bots/api#sendaudio"
  (declare (ignorable caption parse-mode caption-entities
                      duration performer title thumb
                      disable-notification protect-content reply-to-message-id
                      allow-sending-without-reply reply-markup))
  (log:debug "Sending audio" chat audio)
  (apply #'make-request bot "sendAudio"
         :|chat_id| (get-chat-id chat)
         :|audio| audio
         options))

(defgeneric send-audio (bot chat audio
                       &rest options
                       &key caption parse-mode caption-entities
                         duration performer title thumb
                         disable-notification protect-content reply-to-message-id
                         allow-sending-without-reply reply-markup))

(defmethod send-audio (bot chat (audio audio)
                       &rest options
                       &key caption parse-mode caption-entities
                         duration performer title thumb
                         disable-notification protect-content reply-to-message-id
                         allow-sending-without-reply reply-markup)
  "A method for audio sending based on audio object.

https://core.telegram.org/bots/api#sendaudio"
  (declare (ignorable caption parse-mode caption-entities
                      duration performer title thumb
                      disable-notification protect-content reply-to-message-id
                      allow-sending-without-reply reply-markup))
  (apply #'send-audio bot chat (get-file-id audio) options))

(defmethod send-document (bot chat (document string)
                          &rest options
                          &key caption parse-mode caption-entities
                            disable-content-type-detection thumb
                            disable-notification protect-content reply-to-message-id
                            allow-sending-without-reply reply-markup)
  "A method for document sending based on ID.

The file-based method does not work yet.

https://core.telegram.org/bots/api#senddocument"
  (declare (ignorable caption parse-mode caption-entities
                      disable-content-type-detection thumb
                      disable-notification protect-content reply-to-message-id
                      allow-sending-without-reply reply-markup))
  (log:debug "Sending document" chat document)
  (apply #'make-request bot "sendDocument"
         :|chat_id| (get-chat-id chat)
         :|document| document
         options))

(defgeneric send-document (bot chat document
                          &rest options
                          &key caption parse-mode caption-entities
                            disable-content-type-detection thumb
                            disable-notification protect-content reply-to-message-id
                            allow-sending-without-reply reply-markup))

(defmethod send-document (bot chat (document document)
                          &rest options
                          &key caption parse-mode caption-entities
                            disable-content-type-detection thumb
                            disable-notification protect-content reply-to-message-id
                            allow-sending-without-reply reply-markup)
  "A method for document sending based on document object.

https://core.telegram.org/bots/api#senddocument"
  (declare (ignorable caption parse-mode caption-entities
                      disable-content-type-detection thumb
                      disable-notification protect-content reply-to-message-id
                      allow-sending-without-reply reply-markup))
  (apply #'send-document bot chat (get-file-id document) options))

(defmethod send-video (bot chat (video string)
                       &rest options
                       &key caption parse-mode caption-entities
                         duration width height thumb
                         disable-notification protect-content reply-to-message-id
                         allow-sending-without-reply reply-markup)
  "A method for video sending based on ID.

The file-based method does not work yet.

https://core.telegram.org/bots/api#sendvideo"
  (declare (ignorable caption parse-mode caption-entities
                      duration width height thumb
                      disable-notification protect-content reply-to-message-id
                      allow-sending-without-reply reply-markup))
  (log:debug "Sending video" chat video)
  (apply #'make-request bot "sendVideo"
         :|chat_id| (get-chat-id chat)
         :|video| video
         options))

(defgeneric send-video (bot chat video
                       &rest options
                       &key caption parse-mode caption-entities
                         duration width height thumb
                         disable-notification protect-content reply-to-message-id
                         allow-sending-without-reply reply-markup))

(defmethod send-video (bot chat (video video)
                       &rest options
                       &key caption parse-mode caption-entities
                         duration width height thumb
                         disable-notification protect-content reply-to-message-id
                         allow-sending-without-reply reply-markup)
  "A method for video sending based on video object.

https://core.telegram.org/bots/api#sendvideo"
  (declare (ignorable caption parse-mode caption-entities
                      duration width height thumb
                      disable-notification protect-content reply-to-message-id
                      allow-sending-without-reply reply-markup))
  (apply #'send-video bot chat (get-file-id video) options))

(defmethod send-animation (bot chat (animation string)
                           &rest options
                           &key caption parse-mode caption-entities
                             duration width height thumb
                             disable-notification protect-content reply-to-message-id
                             allow-sending-without-reply reply-markup)
  "A method for animation sending based on ID.

The file-based method does not work yet.

https://core.telegram.org/bots/api#sendanimation"
  (declare (ignorable caption parse-mode caption-entities
                      duration width height thumb
                      disable-notification protect-content reply-to-message-id
                      allow-sending-without-reply reply-markup))
  (log:debug "Sending animation" chat animation)
  (apply #'make-request bot "sendAnimation"
         :|chat_id| (get-chat-id chat)
         :|animation| animation
         options))

(defgeneric send-animation (bot chat animation
                           &rest options
                           &key caption parse-mode caption-entities
                             duration width height thumb
                             disable-notification protect-content reply-to-message-id
                             allow-sending-without-reply reply-markup)
  (:documentation "Sends animation to a chat."))

(defmethod send-animation (bot chat (animation animation)
                           &rest options
                           &key caption parse-mode caption-entities
                             duration width height thumb
                             disable-notification protect-content reply-to-message-id
                             allow-sending-without-reply reply-markup)
  "A method for animation sending based on animation object.

https://core.telegram.org/bots/api#sendanimation"
  (declare (ignorable caption parse-mode caption-entities
                      duration width height thumb
                      disable-notification protect-content reply-to-message-id
                      allow-sending-without-reply reply-markup))
  (apply #'send-video bot chat (get-file-id animation) options))

(defgeneric send-video-note (bot chat video-note
                            &rest options
                            &key caption parse-mode caption-entities
                              duration length thumb
                              disable-notification protect-content reply-to-message-id
                              allow-sending-without-reply reply-markup))

(defmethod send-video-note (bot chat (video-note string)
                            &rest options
                            &key caption parse-mode caption-entities
                              duration length thumb
                              disable-notification protect-content reply-to-message-id
                              allow-sending-without-reply reply-markup)
  "A method for video note sending based on ID.

The file-based method does not work yet.

https://core.telegram.org/bots/api#sendvideonote"
  (declare (ignorable caption parse-mode caption-entities
                      duration length thumb
                      disable-notification protect-content reply-to-message-id
                      allow-sending-without-reply reply-markup))
  (log:debug "Sending video note" chat video-note)
  (apply #'make-request bot "sendVideoNote"
         :|chat_id| (get-chat-id chat)
         :|video_note| video-note
         options))

(defmethod send-video-note (bot chat (video-note video-note)
                            &rest options
                            &key caption parse-mode caption-entities
                              duration length thumb
                              disable-notification protect-content reply-to-message-id
                              allow-sending-without-reply reply-markup)
  "A method for video note sending based on video-note object.

https://core.telegram.org/bots/api#sendvideonote"
  (declare (ignorable caption parse-mode caption-entities
                      duration length thumb
                      disable-notification protect-content reply-to-message-id
                      allow-sending-without-reply reply-markup))
  (apply #'send-video-note bot chat (get-file-id video-note) options))

(defgeneric send-voice (bot chat voice
                       &rest options
                       &key caption parse-mode caption-entities
                         duration
                         disable-notification protect-content reply-to-message-id
                         allow-sending-without-reply reply-markup))

(defmethod send-voice (bot chat (voice string)
                       &rest options
                       &key caption parse-mode caption-entities
                         duration
                         disable-notification protect-content reply-to-message-id
                         allow-sending-without-reply reply-markup)
  "A method for voice message sending based on its ID.

The file-based method does not work yet.

https://core.telegram.org/bots/api#sendvoice"
  (declare (ignorable caption parse-mode caption-entities
                      duration
                      disable-notification protect-content reply-to-message-id
                      allow-sending-without-reply reply-markup))
  (log:debug "Sending voice" chat voice)
  (apply #'make-request bot "sendVoice"
         :|chat_id| (get-chat-id chat)
         :|voice| voice
         options))

(defmethod send-voice (bot chat (voice voice)
                       &rest options
                       &key caption parse-mode caption-entities
                         duration
                         disable-notification protect-content reply-to-message-id
                         allow-sending-without-reply reply-markup)
  "A method for voice sending based on voice object.

https://core.telegram.org/bots/api#sendvoice"
  (declare (ignorable caption parse-mode caption-entities
                      duration
                      disable-notification protect-content reply-to-message-id
                      allow-sending-without-reply reply-markup))
  (apply #'send-voice bot chat (get-file-id voice) options))

(defgeneric send-sticker (bot chat sticker
                          &rest options
                          &key disable-notification protect-content reply-to-message-id
                            allow-sending-without-reply reply-markup)
  (:documentation "A function to send sticker."))

(defmethod send-sticker (bot chat (sticker string)
                         &rest options
                         &key disable-notification protect-content reply-to-message-id
                           allow-sending-without-reply reply-markup)
  "A method for sticker sending based on ID.

The file-based method does not work yet.

https://core.telegram.org/bots/api#sendsticker"
  (declare (ignorable disable-notification protect-content reply-to-message-id
                      allow-sending-without-reply reply-markup))
  (log:debug "Sending sticker" chat sticker)
  (apply #'make-request bot "sendSticker"
         :|chat_id| (get-chat-id chat)
         :|sticker| sticker
         options))

(defmethod send-sticker (bot chat (sticker sticker)
                         &rest options
                         &key disable-notification protect-content reply-to-message-id
                           allow-sending-without-reply reply-markup)
  "A method for sticker sending based on sticker object.

https://core.telegram.org/bots/api#sendsticker"
  (declare (ignorable disable-notification protect-content reply-to-message-id
                      allow-sending-without-reply reply-markup))
  (apply #'send-sticker bot chat (get-file-id sticker) options))



;; TODO: сделать так чтобы def-telegram-call работал c 
;; (def-telegram-call send-message (chat text &key
;;                                       parse-mode
;;                                       disable-web-page-preview
;;                                       disable-notification
;;                                       reply-to-message-id)
;;   "https://core.telegram.org/bots/api#sendmessage"
;;   (log:debug "Sending message" chat text)
;;   (let ((options
;;           (append
;;            `(:|chat_id| ,(get-chat-id chat)
;;              :|text| ,text)
;;            (when parse-mode
;;              `(:|parse_mode| ,parse-mode))
;;            (when disable-web-page-preview
;;              `(:disable_web_page_preview ,disable-web-page-preview))
;;            (when disable-notification
;;              `(:disable_notification ,disable-notification))
;;            (when reply-to-message-id
;;              `(:reply_to_message_id ,reply-to-message-id)))))
;;     (make-request bot "sendMessage" options)))

(defun forward-message (bot chat from-chat message &key disable-notification)
  "https://core.telegram.org/bots/api#forwardmessage"
  (let ((options
         (append (list :|chat_id| (get-chat-id chat)
                       :|from_chat_id| (get-chat-id from-chat)
                       :|message_id| (get-message-id message))
                 (when disable-notification
                   (list :|disable_notification| t)))))
    (apply #'make-request bot "forwardMessage" options)))

;; TODO: refactor

;; (defun edit-message-text (b text &key chat-id message-id inline-message-id parse-mode disable-web-page-preview reply-markup)
;;   "https://core.telegram.org/bots/api#editmessagetext"
;;   (let ((options
;;          (list
;;           (cons :text text))))
;;     (when chat-id (nconc options `((:chat_id . ,chat-id))))
;;     (when message-id (nconc options `((:message_id . ,message-id))))
;;     (when inline-message-id (nconc options `((:inline_message_id . ,inline-message-id))))
;;     (when parse-mode (nconc options `((:parse_mode . ,parse-mode))))
;;     (when disable-web-page-preview (nconc options `((:disable_web_page_preview . ,disable-web-page-preview))))
;;     (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
;;     (make-request b "editMessageText" options)))

;; (defun edit-message-caption (b &key chat-id message-id inline-message-id caption reply-markup)
;;   "https://core.telegram.org/bots/api#editmessagecaption"
;;   (let ((options '()))
;;     (when chat-id (nconc options `((:chat_id . ,chat-id))))
;;     (when message-id (nconc options `((:message_id . ,message-id))))
;;     (when inline-message-id (nconc options `((:inline_message_id . ,inline-message-id))))
;;     (when caption (nconc options `((:caption . ,caption))))
;;     (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
;;     (make-request b "editMessageCaption" options)))

;; (defun edit-message-reply-markup (b &key chat-id message-id inline-message-id reply-markup)
;;   "https://core.telegram.org/bots/api#editmessagereplymarkup"
;;   (let ((options '()))
;;     (when chat-id (nconc options `((:chat_id . ,chat-id))))
;;     (when message-id (nconc options `((:message_id . ,message-id))))
;;     (when inline-message-id (nconc options `((:inline_message_id . ,inline-message-id))))
;;     (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
;;     (make-request b "editMessageReplyMarkup" options)))

(defun delete-message (bot chat message)
  "https://core.telegram.org/bots/api#deletemessage"
  (make-request bot "deleteMessage"
                :|chat_id| (get-chat-id chat)
                :|message_id| (get-message-id message)))


(defgeneric on-message (bot text)
  (:documentation "This method gets called with raw text from the message.
                   By default it does nothing."))


(defmethod on-message ((bot t) text)
  (declare (ignorable text))
  (log:warn "Ignoring messages's text. Define on-message method to process it.")
  (values))


(defmethod process ((bot t) (message message))
  "By default, just calls `process' on each entity. And after that calls (on-message bot text).

   This method binds its arguments to *current-bot* and *current-message*
   to make it easier to use (reply \"text\") in 99% usecases.

   If (reply \"text\") is called during processing of some entity or inside the on-message, then
   whole processing pipeline will be stopped and next update will be processed."
  (log:debug "Processing message" message)
  
  (let ((*current-bot* bot)
        (*current-message* message))
    
    (handler-case
        (progn (loop for entity in (get-entities message)
                     do (process bot entity))

               (on-message bot
                           (get-text message)))
      (interrupt-processing (condition)
        (declare (ignore condition))
        (log:debug "Interrupting processing of message"))))
  (values))


(defun get-current-bot ()
  "Returns a bot to which message was addressed."
  (unless (boundp '*current-bot*)
    (error "Seems (get-current-bot) was called outside of processing pipeline, because no current bot is available."))
  (values *current-bot*))


(defun get-current-message ()
  "Returns currently processed message."
  (unless (boundp '*current-message*)
    (error "Seems (get-current-message) was called outside of processing pipeline, because no current message is available."))
  (values *current-message*))


(defun get-current-chat ()
  "Returns a chat where currently processing message was received."
  (unless (boundp '*current-message*)
    (error "Seems (get-current-chat) was called outside of processing pipeline, because no current message is available."))

  (get-chat *current-message*))
