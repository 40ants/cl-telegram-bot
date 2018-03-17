(defpackage #:cl-telegram-bot/media
  (:use #:cl))
(in-package cl-telegram-bot/media)


;; TODO: refactor

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


