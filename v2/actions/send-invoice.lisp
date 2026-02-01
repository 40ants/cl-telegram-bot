(uiop:define-package #:cl-telegram-bot2/actions/send-invoice
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/action
                #:action)
  (:import-from #:cl-telegram-bot2/generics
                #:process-state
                #:on-result
                #:on-state-activation)
  (:import-from #:cl-telegram-bot2/high
                #:register-sent-message
                #:reply)
  (:import-from #:cl-telegram-bot2/high/keyboard
                #:remove-keyboard
                #:inline-keyboard
                #:pay-button
                #:call-callback)
  (:import-from #:cl-telegram-bot2/api)
  (:import-from #:log4cl-extras/secrets
                #:with-secrets)
  (:import-from #:serapeum
                #:soft-list-of
                #:->)
  (:import-from #:cl-telegram-bot2/utils
                #:call-if-needed)
  (:import-from #:cl-telegram-bot2/workflow
                #:workflow-blocks
                #:workflow-block)
  (:import-from #:cl-telegram-bot2/states/wait-for-payment
                #:wait-for-payment)
  (:import-from #:cl-telegram-bot2/state-with-commands
                #:command)
  (:import-from #:secret-values
                #:ensure-value-revealed
                #:ensure-value-concealed
                #:secret-value)
  (:import-from #:cl-telegram-bot2/term/back
                #:back)
  (:export #:send-invoice
           #:title
           #:description
           #:payload
           #:provider-token
           #:currency
           #:prices
           #:on-success
           #:commands
           #:prices-list
           #:on-cancel
           #:cancel-button-text
           #:*default-pay-button-text*
           #:*default-cancel-button-text*
           #:*default-on-success*
           #:*default-on-cancel*
           #:*default-prepare-text*
           #:pay-button-text
           #:prepare-text))
(in-package #:cl-telegram-bot2/actions/send-invoice)


(deftype prices-list ()
  "Type of PRICES arguments for SEND-INVOICE class."
  '(soft-list-of hash-table))


(defvar *default-pay-button-text* "Pay")

(defvar *default-cancel-button-text* "Cancel")

(defvar *default-prepare-text* "Preparing invoice.")

(defvar *default-on-success* (back))

(defvar *default-on-cancel* (back))


(defclass send-invoice (action)
  ((title :initarg :title
          :type (or string
                    symbol)
          :reader title)
   (description :initarg :description
                :type (or string
                          symbol)
                :reader description)
   (prepare-text :initarg :prepare-text
                 :initform *default-prepare-text*
                 :type (or string
                           symbol)
                 :reader prepare-text)
   (payload :initarg :payload
            :type (or string
                      symbol)
            :reader payload)
   (provider-token :initarg :provider-token
                   :type (or secret-value
                             symbol)
                   :reader provider-token)
   (currency :initarg :currency
             :type (or string
                       symbol)
             :reader currency)
   (prices :initarg :prices
           :type (or prices-list
                     symbol)
           :reader prices)
   (on-success :initarg :on-success
               :initform *default-on-success*
               :type (or workflow-block
                         workflow-blocks
                         symbol)
               :reader on-success
               :documentation "Called when user finished the payment. If not given then \"back\" action is returned.")
   (on-cancel :initarg :on-cancel
              :initform *default-on-cancel*
              :type (or workflow-block
                        workflow-blocks
                        symbol)
              :reader on-cancel
              :documentation "Called when user pushes the \"Cancel\" button. If not given then \"back\" action is returned.")
   (pay-button-text :initarg :pay-button-text
                    :initform *default-pay-button-text*
                    :type (or string
                              symbol)
                    :documentation "Showed on a button needed to be pushed to pay an invoice."
                    :reader pay-button-text)
   (cancel-button-text :initarg :cancel-button-text
                       :initform *default-cancel-button-text*
                       :type (or string
                                 symbol)
                       :documentation "Shown on the button to cancel a payment. If this button pushed, then on-cancel callback will be called and may return another state."
                       :reader cancel-button-text)
   (commands :initarg :commands
             :initform nil
             :type (soft-list-of command)
             :documentation "Additional commands to be available while waiting for the payment. "
             :reader commands)))


(-> send-invoice ((or string symbol)
                  (or string symbol)
                  (or string symbol)
                  (or string secret-value symbol)
                  (or string symbol)
                  (or prices-list symbol)
                  &key
                  (:on-success (or workflow-block
                                   workflow-blocks
                                   symbol))
                  (:on-cancel (or workflow-block
                                  workflow-blocks
                                  symbol))
                  (:prepare-text (or string
                                     symbol))
                  (:pay-button-text (or string
                                        symbol))
                  (:cancel-button-text (or string
                                           symbol))
                  (:commands (soft-list-of command)))
    (values send-invoice &optional))

(defun send-invoice (title description payload provider-token currency prices
                     &key
                     (on-success *default-on-success*)
                     (on-cancel *default-on-cancel*)
                     (prepare-text *default-prepare-text*)
                     (cancel-button-text *default-cancel-button-text*)
                     (pay-button-text *default-pay-button-text*)
                     (commands nil))
  (make-instance 'send-invoice
                 :title title
                 :description description
                 :payload payload
                 :provider-token (typecase provider-token
                                   (symbol provider-token)
                                   (t
                                      (ensure-value-concealed provider-token)))
                 :currency currency
                 :prices prices
                 :on-success on-success
                 :on-cancel on-cancel
                 :prepare-text prepare-text
                 :cancel-button-text cancel-button-text
                 :pay-button-text pay-button-text
                 :commands commands))


(defmethod print-object ((obj send-invoice) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S"
            (title obj))))


(-> perform-action (send-invoice)
    (values wait-for-payment &optional))

(defun perform-action (action)
  (let ((provider-token (call-if-needed
                         (provider-token action)))
        (chat-id (cl-telegram-bot2/api::chat-id cl-telegram-bot2/vars::*current-chat*)))
    (with-secrets (provider-token)
      ;; We need this message to hide a reply keyboard, because
      ;; wait-for-payment state can't process messages from reply keyboard:
      (register-sent-message
       (cl-telegram-bot2/api::send-message chat-id
                                           (call-if-needed
                                            (prepare-text action))
                                           :reply-markup
                                           (remove-keyboard)))
      ;; Now send an invoice with Pay and Cancel buttons
      (register-sent-message
       (cl-telegram-bot2/api::send-invoice
        chat-id
        ;; title
        (call-if-needed
         (title action))
        ;; description
        (call-if-needed
         (description action))
        ;; payload
        (call-if-needed
         (payload action))
        ;; currency
        (call-if-needed
         (currency action))
        ;; prices
        (call-if-needed
         (prices action))
        :provider-token (ensure-value-revealed provider-token)
        :reply-markup (inline-keyboard
                       (list
                        (list
                         (pay-button
                          (call-if-needed
                           (pay-button-text action)))
                         (call-callback
                          (call-if-needed
                           (cancel-button-text action))
                          "cancel"))))))))

  (wait-for-payment :on-success (on-success action)
                    :on-cancel (on-cancel action)))


(defmethod on-state-activation ((action send-invoice))
  (perform-action action))


(defmethod process-state ((bot t) (action send-invoice) update)
  (perform-action action))


(defmethod on-result ((action send-invoice) result)
  (perform-action action))
