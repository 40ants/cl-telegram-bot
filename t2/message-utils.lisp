(uiop:define-package #:cl-telegram-bot2-tests/message-utils
  (:use #:cl)
  (:import-from #:rove
                #:ok
                #:deftest)
  (:import-from #:serapeum
                #:->)
  (:import-from #:cl-telegram-bot2/api
                #:chat
                #:user
                #:message
                #:message-origin
                #:message-origin-channel
                #:message-origin-user)
  (:import-from #:cl-telegram-bot2/message-utils
                #:get-chat-id-and-message-id
                #:trim-channel-id))
(in-package #:cl-telegram-bot2-tests/message-utils)


(-> make-chat (integer)
    (values chat &optional))
(defun make-chat (id)
  (make-instance 'chat
                 :id id))


(-> make-origin-channel (integer integer)
    (values message-origin-channel &optional))
(defun make-origin-channel (chat-id message-id)
  (make-instance 'message-origin-channel
                 :chat (make-chat chat-id)
                 :message-id message-id))


(-> make-origin-user ()
    (values message-origin-user &optional))
(defun make-origin-user ()
  (make-instance 'message-origin-user
                 :sender-user (make-instance 'user
                                             :id 1
                                             :is-bot nil)))


(-> make-message (integer integer &key (:forward-origin (or null message-origin)))
    (values message &optional))
(defun make-message (chat-id message-id &key forward-origin)
  (make-instance 'message
                 :chat (make-chat chat-id)
                 :message-id message-id
                 :forward-origin forward-origin))


(deftest test-get-chat-id-and-message-id ()
  (ok (equal (list 123 456 nil)
             (multiple-value-list
              (get-chat-id-and-message-id
               (make-message 123 456))))
      "Own chat id and message id are returned for a message without a forward origin.")
  (let ((origin (make-origin-channel -1002102092834 999)))
    (ok (equal (list -1002102092834 999 origin)
               (multiple-value-list
                (get-chat-id-and-message-id
                 (make-message 123 456
                               :forward-origin origin))))
        "For a channel forward origin, ids of the original message in the source channel are returned."))
  (let ((origin (make-origin-user)))
    (ok (equal (list 123 456 origin)
               (multiple-value-list
                (get-chat-id-and-message-id
                 (make-message 123 456
                               :forward-origin origin))))
        "For a non-channel forward origin, the message's own ids and the origin are returned.")))


(deftest test-trim-channel-id ()
  (ok (= 2102092834
         (trim-channel-id -1002102092834))
      "The -100 prefix is cut from a channel id.")
  (ok (= -1001234
         (trim-channel-id -1001234))
      "A group id which is too small to be a channel id is returned as is.")
  (ok (= 12345
         (trim-channel-id 12345))
      "A positive non-channel id is returned as is."))
