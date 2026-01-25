(uiop:define-package #:cl-telegram-bot2/high/permissions
  (:use #:cl)
  (:import-from #:serapeum
                #:dict
                #:->
                #:soft-list-of)
  (:import-from #:str
                #:replace-all)
  (:import-from #:cl-telegram-bot2/utils
                #:to-json)
  (:import-from #:yason)
  (:import-from #:cl-telegram-bot2/api
                #:chat-administrator-rights)
  (:export
   #:chat-administration-permission
   #:chat-administration-permissions))
(in-package #:cl-telegram-bot2/high/permissions)


(deftype chat-administration-permission ()
  "API docs: https://core.telegram.org/bots/api#chatadministratorrights"
  `(member :is-anonymous
           :can-manage-chat
           :can-delete-messages
           :can-manage-video-chats
           :can-restrict-members
           :can-promote-members
           :can-change-info
           :can-invite-users
           :can-post-stories
           :can-edit-stories
           :can-delete-stories
           :can-post-messages
           :can-edit-messages
           :can-pin-messages
           :can-manage-topics))


(deftype chat-administration-permissions ()
  `(soft-list-of chat-administration-permission))


(-> permissions-to-json (chat-administration-permissions)
    (values string &optional))

(defun permissions-to-json (permissions)
  (loop with hash = (dict)
        for permission in permissions
        do (setf (gethash (replace-all "-" "_"
                                       (string-downcase permission))
                          hash)
                 yason:true)
        finally (return (to-json hash))))


(-> permissions-to-tg-obj (chat-administration-permissions)
    (values chat-administrator-rights &optional))

(defun permissions-to-tg-obj (permissions)

  (let ((args (loop for perm in permissions
                    appending (list perm t))))
    (apply #'make-instance
           'chat-administrator-rights
           args)))
