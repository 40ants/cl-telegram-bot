(uiop:define-package #:cl-telegram-bot/user
  (:use #:cl)
  (:import-from #:cl-telegram-bot/utils
                #:api-response-to-plist)
  (:import-from #:cl-telegram-bot/bot
                #:bot-info
                #:bot)
  (:import-from #:cl-telegram-bot/network
                #:make-request)
  (:import-from #:serapeum
                #:->)
  (:export #:user
           #:user-id
           #:username
           #:is-premium
           #:language-code
           #:first-name
           #:bot-p
           #:can-connect-to-business-p
           #:supports-inline-queries-p
           #:can-read-all-group-messages-p
           #:can-join-groups-p
           #:raw-data
           #:get-me
           #:last-name
           #:get-user-info))
(in-package #:cl-telegram-bot/user)


(defclass user ()
  ((id :initarg :id
       :type integer
       :reader user-id)
   (username :initarg :username
             :type (or null string)
             :initform nil
             :reader username)
   (first-name :initarg :first-name
               :type string
               :reader first-name)
   (last-name :initarg :last-name
              :type (or null string)
              :initform nil
              :reader last-name)
   (language-code :initarg :language-code
                  :type (or null string)
                  :initform nil
                  :reader language-code)
   (is-premium :initarg :is-premium
               :type boolean
               :initform nil
               :reader is-premium)
   (is-bot :initarg :is-bot
           :type boolean
           :reader bot-p)
   (can-connect-to-business :initarg :can-connect-to-business
                            :type boolean
                            :initform nil
                            :reader can-connect-to-business-p)
   (supports-inline-queries :initarg :supports-inline-queries
                            :type boolean
                            :initform nil
                            :reader supports-inline-queries-p)
   (can-read-all-group-messages :initarg :can-read-all-group-messages
                                :type boolean
                                :initform nil
                                :reader can-read-all-group-messages-p)
   (can-join-groups :initarg :can-join-groups
                    :type boolean
                    :initform nil
                    :reader can-join-groups-p)
   (raw-data :initarg :raw-data
             :reader raw-data)))


(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type t)
    (format stream "~S @~A"
            (first-name user)
            (username user))))


(defun make-user-from-raw (raw-data)
  (let ((initargs (api-response-to-plist raw-data)))
    (apply #'make-instance
           'user
           :raw-data raw-data
           initargs)))


(-> get-me (bot)
    (values user &optional))

(defun get-me (bot)
  "https://core.telegram.org/bots/api#getme"
  (make-user-from-raw
   (make-request bot "getMe")))


(defmethod bot-info :around ((bot bot))
  (unless (slot-value bot 'bot-info)
    (setf (slot-value bot 'bot-info)
          (get-me bot)))
  (call-next-method))



(defgeneric get-user-info (obj)
  (:documentation "Returns a USER object related to the object.

                   If object is not bound to a user, then NIL should be returned.")
  (:method ((obj t))
    (values nil)))
