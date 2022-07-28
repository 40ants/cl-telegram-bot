(defpackage #:cl-telegram-bot/media
  (:use #:cl))
(in-package cl-telegram-bot/media)


;; TODO: refactor

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


