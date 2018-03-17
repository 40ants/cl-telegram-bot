(defpackage #:cl-telegram-bot/stickers
  (:use #:cl))
(in-package cl-telegram-bot/stickers)

;; TODO: refactor


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

