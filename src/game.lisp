(defpackage #:cl-telegram-bot/game
  (:use #:cl))
(in-package cl-telegram-bot/game)


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

