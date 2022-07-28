(defpackage #:cl-telegram-bot/profile
  (:use #:cl))
(in-package cl-telegram-bot/profile)


;; TODO: refactor

(defun get-user-profile-photos (b user-id &key offset limit)
  "https://core.telegram.org/bots/api#getuserprofilephotos"
  (let ((options
         (list
          (cons :user_id user-id))))
    (when offset (nconc options `((:offset . ,offset))))
    (when limit (nconc options `((:limit . ,limit))))
    (apply #'make-request b "getUserProfilePhotos" options)))
