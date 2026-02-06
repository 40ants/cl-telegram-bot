(uiop:define-package #:cl-telegram-bot2/high/keyboard
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/utils
                #:optional-arguments-plist)
  (:import-from #:serapeum
                #:soft-list-of
                #:->)
  (:import-from #:cl-telegram-bot2/api
                #:inline-keyboard-markup
                #:reply-keyboard-markup)
  (:import-from #:alexandria
                #:alist-plist
                #:required-argument)
  (:import-from #:cl-telegram-bot2/high/permissions
                #:permissions-to-tg-obj
                #:chat-administration-permissions)
  (:export #:keyboard
           #:text-button
           #:open-web-app
           #:inline-keyboard
           #:request-users
           #:request-chat
           #:request-contact
           #:request-location
           #:request-poll
           #:open-url
           #:call-callback
           #:open-login-url
           #:switch-inline-query
           #:switch-inline-query-current-chat
           #:switch-inline-query-choosen-chat
           #:copy-text
           #:open-game
           #:pay-button
           #:remove-keyboard
           #:inline-query
           #:query
           #:allow-user-chats-p
           #:allow-bot-chats-p
           #:allow-group-chats-p
           #:allow-channel-chats-p
           #:request-users-request-id
           #:request-chat-request-id
           #:chat-is-channel-p
           #:chat-is-forum-p
           #:chat-has-username-p
           #:chat-is-created-p
           #:user-administrator-rights
           #:bot-administrator-rights
           #:bot-is-member-p
           #:request-title-p
           #:request-username-p
           #:request-photo-p
           #:user-is-bot-p
           #:user-is-premium-p
           #:max-quantity
           #:request-name-p
           #:requested-poll-type
           #:url
           #:callback-data
           #:login-url
           #:forward-text
           #:bot-username
           #:request-write-access-p
           #:text-to-copy
           #:web-app-url
           #:ensure-call-callback))
(in-package #:cl-telegram-bot2/high/keyboard)


(defgeneric make-main-keyboard-button (button)
  (:documentation "Returns API CL-TELEGRAM-BOT2/API:KEYBOARD-BUTTON class instance from given high level button description.")
  (:method ((button cl-telegram-bot2/api:keyboard-button))
    button))


(defgeneric make-inline-keyboard-button (button)
  (:documentation "Returns API CL-TELEGRAM-BOT2/API:INLINE-KEYBOARD-BUTTON class instance from given high level button description.")
  (:method ((button cl-telegram-bot2/api:inline-keyboard-button))
    button))


(defclass button ()
  ((title :initarg :title
          :type string
          :reader button-title)))


(defclass keyboard-button-mixin ()
  ())


(defclass inline-keyboard-button-mixin ()
  ())


;; Buttons supported in both main and inline keyboards

(defclass text-button (keyboard-button-mixin inline-keyboard-button-mixin button)
  ())


(-> text-button (string)
    (values text-button &optional))

(defun text-button (title)
  (make-instance 'text-button
                 :title title))


(defmethod make-main-keyboard-button ((button text-button))
  (make-instance 'cl-telegram-bot2/api:keyboard-button
                 :text (button-title button)))


(defmethod make-inline-keyboard-button ((button text-button))
  (error "Text buttons can't be used in inline keyboards. Use call-callback instead."))


(defclass open-web-app (keyboard-button-mixin inline-keyboard-button-mixin button)
  ((url :initarg :url
        :type string
        :reader web-app-url)))


(-> open-web-app (string string)
    (values open-web-app &optional))


(defun open-web-app (title web-app-url)
  (make-instance 'open-web-app
                 :title title
                 :url web-app-url))


(defmethod make-main-keyboard-button ((button open-web-app))
  (make-instance 'cl-telegram-bot2/api:keyboard-button
                 :text (button-title button)
                 :web-app (make-instance 'cl-telegram-bot2/api:web-app-info
                                         :url (web-app-url button))))


(defmethod make-inline-keyboard-button ((button open-web-app))
  (make-instance 'cl-telegram-bot2/api:inline-keyboard-button
                 :text (button-title button)
                 :web-app (make-instance 'cl-telegram-bot2/api:web-app-info
                                         :url (web-app-url button))))


;; Main keyboard buttons 

(defclass request-users (keyboard-button-mixin button)
  ((request-id :initarg :request-id
               :type integer
               :initform (required-argument "Argument :request-id is required.")
               :reader request-users-request-id)
   (user-is-bot :initarg :user-is-bot
                :type boolean
                :initform nil
                :reader user-is-bot-p)
   (user-is-premium :initarg :user-is-premium
                    :type boolean
                    :initform nil
                    :reader user-is-premium-p)
   (max-quantity :initarg :max-quantity
                 :type (integer 1 10)
                 :initform 1
                 :reader max-quantity)
   (request-name :initarg :request-name
                 :type boolean
                 :initform nil
                 :reader request-name-p)
   (request-username :initarg :request-username
                     :type boolean
                     :initform nil
                     :reader request-username-p)
   (request-photo :initarg :request-photo
                  :type boolean
                  :initform nil
                  :reader request-photo-p)))


(-> request-users (string integer
                   &key
                   (:user-is-bot boolean)
                   (:user-is-premium boolean)
                   (:max-quantity (integer 1 10))
                   (:request-name boolean)
                   (:request-username boolean)
                   (:request-photo boolean))
    (values request-users &optional))


(defun request-users (title request-id
                      &rest rest
                      &key
                      user-is-bot
                      user-is-premium
                      max-quantity
                      request-name
                      request-username
                      request-photo)
  (declare (ignore user-is-bot
                   user-is-premium
                   max-quantity
                   request-name
                   request-username
                   request-photo))
  (apply #'make-instance
         'request-users
         :title title
         :request-id request-id
         rest))


(defmethod make-main-keyboard-button ((button request-users))
  (make-instance 'cl-telegram-bot2/api:keyboard-button
                 :text (button-title button)
                 :request-users (make-instance 'cl-telegram-bot2/api:keyboard-button-request-users
                                               :request-id (request-users-request-id button)
                                               :user-is-bot (user-is-bot-p button)
                                               :user-is-premium (user-is-premium-p button)
                                               :max-quantity (max-quantity button)
                                               :request-name (request-name-p button)
                                               :request-username (request-username-p button)
                                               :request-photo (request-photo-p button))))


(-> request-chat (string integer
                  &key
                  (:chat-is-channel boolean)
                  (:chat-is-forum boolean)
                  (:chat-has-username boolean)
                  (:chat-is-created boolean)
                  (:user-administrator-rights chat-administration-permissions)
                  (:bot-administrator-rights chat-administration-permissions)
                  (:bot-is-member boolean)
                  (:request-title boolean)
                  (:request-username boolean)
                  (:request-photo boolean))
    (values request-chat &optional))


(defclass request-chat (keyboard-button-mixin button)
  ((request-id :initarg :request-id
               :type integer
               :initform (required-argument "Argument :request-id is required.")
               :reader request-chat-request-id)
   (chat-is-channel :initarg :chat-is-channel
                    :type boolean
                    :initform nil
                    :reader chat-is-channel-p)
   ;; Optional attributes, if unbound, then will not be sent to Telegram API
   (chat-is-forum :initarg :chat-is-forum
                  :type boolean
                  :reader chat-is-forum-p)
   (chat-has-username :initarg :chat-has-username
                      :type boolean
                      :reader chat-has-username-p)
   (chat-is-created :initarg :chat-is-created
                    :type boolean
                    :reader chat-is-created-p)
   (user-administrator-rights :initarg :user-administrator-rights
                              :type chat-administration-permissions
                              :reader user-administrator-rights)
   (bot-administrator-rights :initarg :bot-administrator-rights
                             :type chat-administration-permissions
                             :reader bot-administrator-rights)
   (bot-is-member :initarg :bot-is-member
                  :type boolean
                  :reader bot-is-member-p)
   (request-title :initarg :request-title
                  :type boolean
                  :reader request-title-p)
   (request-username :initarg :request-username
                     :type boolean
                     :reader request-username-p)
   (request-photo :initarg :request-photo
                  :type boolean
                  :reader request-photo-p)))


(defun request-chat (title request-id
                     &rest rest
                     &key
                     chat-is-channel
                     chat-is-forum
                     chat-has-username
                     chat-is-created
                     user-administrator-rights
                     bot-administrator-rights
                     bot-is-member
                     request-title
                     request-username
                     request-photo)
  (declare (ignore chat-is-channel
                   chat-is-forum
                   chat-has-username
                   chat-is-created
                   user-administrator-rights
                   bot-administrator-rights
                   bot-is-member
                   request-title
                   request-username
                   request-photo))
  (apply #'make-instance
         'request-chat
         :title title
         :request-id request-id
         rest))


(defmethod make-main-keyboard-button ((button request-chat))
  (make-instance 'cl-telegram-bot2/api:keyboard-button
                 :text (button-title button)
                 :request-chat (apply #'make-instance
                                      'cl-telegram-bot2/api:keyboard-button-request-chat
                                      :request-id (request-chat-request-id button)
                                      :chat-is-channel (chat-is-channel-p button)
                                      
                                      (optional-arguments-plist
                                        :chat-is-forum (chat-is-forum-p button)
                                        :chat-has-username (chat-has-username-p button)
                                        :chat-is-created (chat-is-created-p button)
                                        :user-administrator-rights (permissions-to-tg-obj
                                                                    (or (user-administrator-rights button)
                                                                        (list :is-anonymous)))
                                        :bot-administrator-rights (permissions-to-tg-obj
                                                                   (or
                                                                    (bot-administrator-rights button)
                                                                    (list :is-anonymous)))
                                        :bot-is-member (bot-is-member-p button)
                                        :request-title (request-title-p button)
                                        :request-username (request-username-p button)
                                        :request-photo (request-photo-p button)))))


(defclass request-contact (keyboard-button-mixin button)
  ())


(-> request-contact (string)
    (values request-contact &optional))


(defun request-contact (title)
  (make-instance 'request-contact
                 :title title))


(defmethod make-main-keyboard-button ((button request-contact))
  (make-instance 'cl-telegram-bot2/api:keyboard-button
                 :text (button-title button)
                 :request-contact t))


(defclass request-location (keyboard-button-mixin button)
  ())


(-> request-location (string)
    (values request-location &optional))


(defun request-location (title)
  (make-instance 'request-location
                 :title title))


(defmethod make-main-keyboard-button ((button request-location))
  (make-instance 'cl-telegram-bot2/api:keyboard-button
                 :text (button-title button)
                 :request-location t))


(defclass request-poll (keyboard-button-mixin button)
  ((poll-type :initarg :poll-type
              :type (or null string)
              :initform nil
              :documentation "If \"quiz\" is passed, the user will be allowed to create only polls in the quiz mode. If \"regular\" is passed, only regular polls will be allowed. Otherwise, the user will be allowed to create a poll of any type.

                              API: https://core.telegram.org/bots/api#keyboardbuttonpolltype"
              :reader requested-poll-type)))


(-> request-poll (string &key (:poll-type string))
    (values request-poll &optional))


(defun request-poll (title
                     &rest rest
                     &key poll-type)
  (declare (ignore poll-type))
  (apply #'make-instance 'request-poll
         :title title
         rest))


(defmethod make-main-keyboard-button ((button request-poll))
  (make-instance 'cl-telegram-bot2/api:keyboard-button
                 :text (button-title button)
                 :request-poll (make-instance 'cl-telegram-bot2/api:keyboard-button-poll-type
                                              :type (requested-poll-type button))))


;; Inline keyboard buttons

(defclass open-url (inline-keyboard-button-mixin button)
  ((url :initarg :url
        :type string
        :reader url)))


(-> open-url (string string)
    (values open-url &optional))


(defun open-url (title url)
  (make-instance 'open-url
                 :title title
                 :url url))


(defmethod make-inline-keyboard-button ((button open-url))
  (make-instance 'cl-telegram-bot2/api:inline-keyboard-button
                 :text (button-title button)
                 :url (url button)))


(defclass call-callback (inline-keyboard-button-mixin button)
  ((callback-data :initarg :callback-data
                  :type string
                  :reader callback-data)))


(-> call-callback (string string)
    (values call-callback &optional))


(defun call-callback (title callback-data)
  (make-instance 'call-callback
                 :title title
                 :callback-data callback-data))


(defmethod make-inline-keyboard-button ((button call-callback))
  (make-instance 'cl-telegram-bot2/api:inline-keyboard-button
                 :text (button-title button)
                 :callback-data (callback-data button)))


(-> ensure-call-callback ((or cons call-callback))
    (values call-callback &optional))

(defun ensure-call-callback (obj)
  "Makes a CALL-CALLBACK button from a (title . data) cons."
  (etypecase obj
    (call-callback
     (values obj))
    (cons
     (unless (typep (car obj) 'string)
       (error "~A is not of type string" (car obj)))
     (unless (typep (cdr obj) 'string)
       (error "~A is not of type string" (cdr obj)))
     (values
      (call-callback (car obj) (cdr obj))))))


(defclass open-login-url (inline-keyboard-button-mixin button)
  ((login-url :initarg :login-url
              :type string
              :reader login-url)
   (forward-text :initarg :forward-text
                 :type (or null string)
                 :initform nil
                 :reader forward-text)
   (bot-username :initarg :bot-username
                 :type (or null string)
                 :initform nil
                 :reader bot-username)
   (request-write-access :initarg :request-write-access
                         :type boolean
                         :initform nil
                         :reader request-write-access-p)))


(-> open-login-url (string string
                    &key
                    (:forward-text (or null string))
                    (:bot-username (or null string))
                    (:request-write-access boolean))
    (values open-login-url &optional))


(defun open-login-url (button-title login-url
                       &rest rest
                       &key
                       forward-text
                       bot-username
                       request-write-access)
  (declare (ignore forward-text bot-username request-write-access))
  (apply #'make-instance
         'open-login-url
         :title button-title
         :login-url login-url
         rest))


(defmethod make-inline-keyboard-button ((button open-login-url))
  (make-instance 'cl-telegram-bot2/api:inline-keyboard-button
                 :text (button-title button)
                 :login-url (make-instance 'cl-telegram-bot2/api:login-url
                                           :url (login-url button)
                                           :forward-text (forward-text button)
                                           :bot-username (bot-username button)
                                           :request-write-access (request-write-access-p button))))


(defclass switch-inline-query (inline-keyboard-button-mixin button)
  ((inline-query :initarg :inline-query
                 :type string
                 :reader inline-query)))


(-> switch-inline-query (string string)
    (values switch-inline-query &optional))


(defun switch-inline-query (title inline-query)
  (make-instance 'switch-inline-query
                 :title title
                 :inline-query inline-query))


(defmethod make-inline-keyboard-button ((button switch-inline-query))
  (make-instance 'cl-telegram-bot2/api:inline-keyboard-button
                 :text (button-title button)
                 :switch-inline-query (inline-query button)))


(defclass switch-inline-query-current-chat (inline-keyboard-button-mixin button)
  ((inline-query :initarg :inline-query
                 :type string
                 :reader inline-query)))


(-> switch-inline-query-current-chat (string string)
    (values switch-inline-query &optional))


(defun switch-inline-query-current-chat (title inline-query)
  (make-instance 'switch-inline-query-current-chat
                 :title title
                 :inline-query inline-query))


(defmethod make-inline-keyboard-button ((button switch-inline-query-current-chat))
  (make-instance 'cl-telegram-bot2/api:inline-keyboard-button
                 :text (button-title button)
                 :switch-inline-query-current-chat (inline-query button)))


(defclass switch-inline-query-choosen-chat (inline-keyboard-button-mixin button)
  ((query :initarg :query
          :type (or null string)
          :reader query)
   (allow-user-chats :initarg :allow-user-chats
                     :type boolean
                     :reader allow-user-chats-p)
   (allow-bot-chats :initarg :allow-bot-chats
                    :type boolean
                    :reader allow-bot-chats-p)
   (allow-group-chats :initarg :allow-group-chats
                      :type boolean
                      :reader allow-group-chats-p)
   (allow-channel-chats :initarg :allow-channel-chats
                        :type boolean
                        :reader allow-channel-chats-p)))


(-> switch-inline-query-choosen-chat (string
                                      &key
                                      (:query string)
                                      (:allow-user-chats boolean)
                                      (:allow-bot-chats boolean)
                                      (:allow-group-chats boolean)
                                      (:allow-channel-chats boolean))
    (values switch-inline-query-choosen-chat &optional))


(defun switch-inline-query-choosen-chat (title
                                         &rest rest
                                         &key
                                         query
                                         allow-user-chats
                                         allow-bot-chats
                                         allow-group-chats
                                         allow-channel-chats)
  (declare (ignore query
                   allow-user-chats
                   allow-bot-chats
                   allow-group-chats
                   allow-channel-chats))
  (apply #'make-instance
         'switch-inline-query-choosen-chat
         :title title
         rest))


(defmethod make-inline-keyboard-button ((button switch-inline-query-choosen-chat))
  (make-instance 'cl-telegram-bot2/api:inline-keyboard-button
                 :text (button-title button)
                 :switch-inline-query-choosen-chat
                 (make-instance 'cl-telegram-bot2/api:switch-inline-query-chosen-chat
                                :query (query button)
                                :allow-user-chats (allow-user-chats-p button)
                                :allow-bot-chats (allow-bot-chats-p button)
                                :allow-group-chats (allow-group-chats-p button)
                                :allow-channel-chats (allow-channel-chats-p button))))


(defclass copy-text (inline-keyboard-button-mixin button)
  ((text-to-copy :initarg :text-to-copy
                 :type string
                 :reader text-to-copy)))


(-> copy-text (string string)
    (values copy-text &optional))


(defun copy-text (button-title text-to-copy)
  (make-instance 'copy-text
                 :title button-title
                 :text-to-copy text-to-copy))


(defmethod make-inline-keyboard-button ((button copy-text))
  (make-instance 'cl-telegram-bot2/api:inline-keyboard-button
                 :text (button-title button)
                 :copy-text
                 (make-instance 'cl-telegram-bot2/api:copy-text-button
                                :text (text-to-copy button))))


(defclass open-game (inline-keyboard-button-mixin button)
  ())


(-> open-game (string)
    (values open-game &optional))


(defun open-game (button-title)
  (make-instance 'open-game
                 :title button-title))


(defmethod make-inline-keyboard-button ((button open-game))
  (make-instance 'cl-telegram-bot2/api:inline-keyboard-button
                 :text (button-title button)
                 :callback-game
                 (make-instance 'cl-telegram-bot2/api:callback-game)))


(defclass pay-button (inline-keyboard-button-mixin button)
  ())


(-> pay-button (string)
    (values pay-button &optional))


(defun pay-button (button-title)
  (make-instance 'pay-button
                 :title button-title))


(defmethod make-inline-keyboard-button ((button pay-button))
  (make-instance 'cl-telegram-bot2/api:inline-keyboard-button
                 :text (button-title button)
                 :pay t))


;; Type Helpers

(deftype keyboard-button-type ()
   `(or keyboard-button-mixin
        cl-telegram-bot2/api:keyboard-button))

(deftype keyboard-buttons-row ()
  '(soft-list-of keyboard-button-type))


(deftype keyboard-buttons-rows ()
  '(soft-list-of keyboard-buttons-row))


(deftype inline-keyboard-button-type ()
   `(or inline-keyboard-button-mixin
        cl-telegram-bot2/api:inline-keyboard-button))

(deftype inline-keyboard-buttons-row ()
  '(soft-list-of inline-keyboard-button-type))


(deftype inline-keyboard-buttons-rows ()
  '(soft-list-of inline-keyboard-buttons-row))



;; Keyboard constructors

;; Main keyboard

(-> ensure-buttons-rows ((or keyboard-button-type
                             keyboard-buttons-row
                             keyboard-buttons-rows))
    (values keyboard-buttons-rows &optional))


(defun ensure-buttons-rows (value)
  (etypecase value
    (keyboard-button-type
       (list (list value)))
    (keyboard-buttons-row
       (list value))
    (keyboard-buttons-rows
       value)))


(-> keyboard ((or keyboard-button-type
                  keyboard-buttons-row
                  keyboard-buttons-rows)
              &key
              (:is-persistent boolean)
              (:resize-keyboard boolean)
              (:one-time-keyboard boolean)
              (:input-field-placeholder string)
              (:selective boolean))
    (values reply-keyboard-markup))


(defun keyboard (buttons
                 &rest rest
                 &key
                 is-persistent
                 resize-keyboard
                 one-time-keyboard
                 input-field-placeholder
                 selective)
  "Returns object of CL-TELEGRAM-BOT2/API:REPLY-KEYBOARD-MARKUP class.

   API docs: https://core.telegram.org/bots/api#replykeyboardmarkup"

  (declare (ignore is-persistent
                   resize-keyboard
                   one-time-keyboard
                   input-field-placeholder
                   selective))

  (apply #'make-instance
         'reply-keyboard-markup 
         :keyboard
         (loop for row in (ensure-buttons-rows buttons)
               collect (loop for button in row
                             collect (make-main-keyboard-button button)))
         rest))


(-> ensure-inline-buttons-rows ((or inline-keyboard-button-type
                                    inline-keyboard-buttons-row
                                    inline-keyboard-buttons-rows))
    (values inline-keyboard-buttons-rows &optional))



(defun ensure-inline-buttons-rows (value)
  (etypecase value
    (inline-keyboard-button-type
       (list (list value)))
    (inline-keyboard-buttons-row
       (list value))
    (inline-keyboard-buttons-rows
       value)))


(-> inline-keyboard ((or inline-keyboard-button-type
                         inline-keyboard-buttons-row
                         inline-keyboard-buttons-rows))
    (values inline-keyboard-markup))


(defun inline-keyboard (buttons)
  "Returns object of CL-TELEGRAM-BOT2/API:INLINE-KEYBOARD-MARKUP class.

   API docs: https://core.telegram.org/bots/api#replykeyboardmarkup"
  (let ((api-buttons
          (loop for row in (ensure-inline-buttons-rows buttons)
                collect (loop for button in row
                              collect (make-inline-keyboard-button button)))))
    (make-instance 'inline-keyboard-markup 
                   :inline-keyboard api-buttons)))


(-> remove-keyboard (&key
                     (:selective boolean))
    (values cl-telegram-bot2/api:reply-keyboard-remove &optional))


(defun remove-keyboard (&key selective)
  (make-instance 'cl-telegram-bot2/api:reply-keyboard-remove
                 :remove-keyboard t
                 :selective selective))
