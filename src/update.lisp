(uiop:define-package #:cl-telegram-bot/update
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:cl-telegram-bot/message
                #:*current-bot*
                #:make-message)
  (:import-from #:cl-telegram-bot/network
                #:make-request)
  (:import-from #:cl-telegram-bot/bot
                #:get-last-update-id
                #:bot)
  (:import-from #:cl-telegram-bot/pipeline
                #:process)
  (:import-from #:cl-telegram-bot/callback
                #:make-callback)
  (:import-from #:anaphora
                #:it
                #:acond)
  (:import-from #:cl-telegram-bot/envelope
                #:edited-message
                #:channel-post
                #:edited-channel-post)
  (:import-from #:cl-telegram-bot/chat
                #:get-chat)
  (:import-from #:cl-telegram-bot/payments
                #:make-successful-payment
                #:make-pre-checkout-query)
  (:import-from #:cl-telegram-bot/user
                #:get-user-info)
  (:export #:make-update
           #:get-raw-data
           #:get-update-id
           #:process-updates
           #:update
           #:get-payload))
(in-package cl-telegram-bot/update)


(defclass update ()
  ((id :initarg :id
       :reader get-update-id)
   (payload :initarg :payload
            :reader get-payload)
   (raw-data :initarg :raw-data
             :reader get-raw-data)))


(defun make-update (data)
  (let ((update-id (getf data :|update_id|))
        (payload
          (acond
            ((getf data :|message|)
             (cond
               ((getf it :|successful_payment|))
               (t
                (make-message it))))
            ((getf data :|edited_message|)
             (make-instance 'edited-message
                            :message (make-message it)))
            ((getf data :|channel_post|)
             (make-instance 'channel-post
                            :message (make-message it)))
            ((getf data :|edited_channel_post|)
             (make-instance 'edited-channel-post
                            :message (make-message it)))
            ((getf data :|callback_query|)
             (make-callback *current-bot*
                            it))
            ((getf data :|pre_checkout_query|)
             (make-pre-checkout-query *current-bot*
                                      it))
            ((getf data :|successful_payment|)
             (make-successful-payment *current-bot*
                                      it))
            (t
             (log:warn "Received not supported update type"
                       data)
             nil))))
    (make-instance 'update
                   :id update-id
                   :payload payload
                   :raw-data data)))


(defun get-updates (bot &key limit timeout)
  "https://core.telegram.org/bots/api#getupdates"
  (let* ((current-id (get-last-update-id bot))
         (results (make-request bot "getUpdates"
                                :|offset| current-id
                                :|limit| limit
                                :|timeout| timeout
                                :streamp t
                                :timeout timeout)))
    
    (let ((updates (mapcar 'make-update results)))
      (when updates
        (let ((max-id (reduce #'max
                              updates
                              :key #'get-update-id)))
          ;; In original cl-telegram-bot a bug was here, because
          ;; it saved update's id only the first time, and after that,
          ;; just incremented that value
          (log:debug "Setting new" max-id)
          (setf (get-last-update-id bot)
                (+ max-id 1))))
    
      (values updates))))


;; Generics

(defgeneric process-updates (bot)
  (:documentation "By default, this method starts an infinite loop and fetching new updates using long polling."))


(defmethod process-updates ((bot t))
  "Starts inifinite loop to process updates using long polling."
  (loop with *current-bot* = bot
    do (loop for update in (restart-case
                               (get-updates bot
                                            :timeout 10)
                             (continue-processing (&optional delay)
                               :report "Continue processing updates from Telegram"
                               (when delay
                                 (sleep delay))
                               ;; Return no updates
                               (values)))
             do (restart-case
                    (process bot update)
                  (continue-processing (&optional delay)
                    :report "Continue processing updates from Telegram"
                    (when delay
                      (sleep delay)))))))


(defmethod process ((bot t) (update update))
  "By default, just calls `process' on the payload."
  (log:debug "Processing update" update)
  (let ((payload (get-payload update)))
    (process bot payload)))



(defmethod get-chat ((update update))
  (get-chat (get-payload update)))


(defmethod get-user-info ((update update))
  (get-user-info (get-payload update)))
