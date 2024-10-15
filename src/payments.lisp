(uiop:define-package #:cl-telegram-bot/payments
  (:use #:cl)
  (:import-from #:log)
  (:import-from #:cl-telegram-bot/network
                #:make-request)
  (:import-from #:cl-telegram-bot/user
                #:get-user-info
                #:make-user-from-raw
                #:user)
  (:import-from #:cl-telegram-bot/pipeline
                #:process)
  (:import-from #:cl-telegram-bot/message
                #:*current-message*
                #:*current-bot*)
  (:import-from #:serapeum
                #:->)
  (:import-from #:cl-telegram-bot/bot
                #:bot)
  (:export
   #:on-pre-checkout-query
   #:answer-pre-checkout-query
   #:send-invoice
   #:answer-shipping-query))
(in-package cl-telegram-bot/payments)


(defclass pre-checkout-query ()
  ((id :initarg :id
       :type string
       :reader pre-checkout-query-id)
   (invoice-payload :initarg :invoice-payload
                    :type string
                    :reader invoice-payload)
   (total-amount :initarg :total-amount
                 :type integer
                 :reader total-amount)
   (currency :initarg :currency
             :type string
             :reader currency)
   (raw-data :initarg :raw-data
             :reader callback-raw-data)
   (user :initarg :user
         :type user
         :reader pre-checkout-query-user)))


(defclass successful-payment ()
  ((provider-payment-charge-id :initarg :provider-payment-charge-id
                               :type string
                               :reader provider-payment-charge-id)
   (telegram-payment-charge-id :initarg :telegram-payment-charge-id
                               :type string
                               :reader telegram-payment-charge-id)
   (invoice-payload :initarg :invoice-payload
                    :type string
                    :reader invoice-payload)
   (total-amount :initarg :total-amount
                 :type integer
                 :reader total-amount)
   (currency :initarg :currency
             :type string
             :reader currency)
   (shipping-option-id :initarg :shipping-option-id
                       :type (or null string)
                       :initform nil
                       :reader shipping-option-id)
   (raw-data :initarg :raw-data
             :reader get-raw-data)
   ;; TODO: support optional OrderInfo
   ))


(defun send-invoice (b chat-id title description payload provider-token start-parameter currency prices &key photo-url photo-size photo-width photo-height need-name need-phone-number need-email need-shipping-address is-flexible disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendinvoice"
  (let ((options
          (append
           (list
            :chat_id chat-id
            :title title
            :description description
            :payload payload
            :provider_token provider-token
            :start_parameter start-parameter
            :currency currency
            :prices prices)
           (when photo-url
             (list :photo_url photo-url))
           (when photo-size
             (list :photo_size photo-size))
           (when photo-width
             (list :photo_width photo-width))
           (when photo-height
             (list :photo_height photo-height))
           (when need-name
             (list :need_name need-name))
           (when need-phone-number
             (list :need_phone_number need-phone-number))
           (when need-email
             (list :need_email need-email))
           (when need-shipping-address
             (list :need_shipping_address need-shipping-address))
           (when is-flexible
             (list :is_flexible is-flexible))
           (when disable-notification
             (list :disable_notification disable-notification))
           (when reply-to-message-id
             (list :reply_to_message_id reply-to-message-id))
           (when reply-markup
             (list :reply_markup reply-markup)))))
    (apply #'make-request b "sendInvoice" options)))


(defun answer-shipping-query (b shipping-query-id ok &key shipping-options error-message)
  "https://core.telegram.org/bots/api#answershippingquery"
  (let ((options
          (append
           (list
            :shipping_query_id shipping-query-id
            :ok ok)
           (when shipping-options
             (list :shipping_options shipping-options))
           (when error-message
             (list :error_message error-message)))))
    (apply #'make-request b "answerShippingQuery" options)))


(-> answer-pre-checkout-query (bot pre-checkout-query &key (:error-message string)))

(defun answer-pre-checkout-query (bot pre-checkout-query &key error-message)
  "If ERROR-MESSAGE argument was given, then response considered is not OK and transaction will be cancelled.

   https://core.telegram.org/bots/api#answerprecheckoutquery"
  (let ((options
          (append
           (list
            :pre_checkout_query_id (pre-checkout-query-id pre-checkout-query)
            :ok (not error-message))
           (when error-message
             (list :error_message error-message)))))
    (apply #'make-request bot
           "answerPreCheckoutQuery" options)))


(defgeneric make-pre-checkout-query (bot data)
  (:documentation "Called when user starts payment process.

Parses data like this:

(:|pre_checkout_query|
 (:|invoice_payload| \"foo-bar-payload\"
  :|total_amount| 12000
  :|currency| \"RUB\"
  :|from|
  (:|is_premium| T :|language_code| \"en\" :|username| \"svetlyak40wt\"
   :|last_name| \"svetlyak40wt\" :|first_name| \"Alexander ƛrtemenko\" :|is_bot|
   NIL :|id| 76226374)
  :|id| \"327389787349253259\")
 :|update_id| 7764933)
")
  (:method ((bot t) (data t))
    (let ((id (getf data :|id|))
          (invoice-payload (getf data :|invoice_payload|))
          (total-amount (getf data :|total_amount|))
          (currency (getf data :|currency|))
          (from (make-user-from-raw (getf data :|from|))))
      (make-instance 'pre-checkout-query
                     :id id
                     :invoice-payload invoice-payload
                     :total-amount total-amount
                     :currency currency
                     :user from
                     :raw-data data))))


(defgeneric make-successful-payment (bot data)
  (:method ((bot t) (data t))
    (let ((invoice-payload (getf data :|invoice_payload|))
          (total-amount (getf data :|total_amount|))
          (currency (getf data :|currency|))
          (shipping-option-id (getf data :|shipping_option_id|))
          (telegram-payment-charge-id (getf data :|telegram_payment_charge_id|))
          (provider-payment-charge-id (getf data :|provider_payment_charge_id|)))
      (make-instance 'successful-payment
                     :invoice-payload invoice-payload
                     :total-amount total-amount
                     :currency currency
                     :shipping-option-id shipping-option-id
                     :telegram-payment-charge-id telegram-payment-charge-id
                     :provider-payment-charge-id provider-payment-charge-id
                     :raw-data data))))


(defmethod get-user-info ((payload pre-checkout-query))
  (pre-checkout-query-user payload))


(defgeneric on-pre-checkout-query (bot query)
  (:documentation "Called when user enters payment method credentials and hit \"Pay\" button. Second argument is an object of PRE-CHECKOUT-QUERY type.

                   A method should respond with with a call to ANSWER-PRE-CHECKOUT-QUERY function.")
  (:method ((bot t) (query t))
    ;; Doing nothing
    (log:warn "There is no ON-PRE-CHECKOUT-QUERY method for" bot)
    (values)))


(defmethod process ((bot t) (payload pre-checkout-query))
  ""
  (log:debug "Processing pre-checkout-query" payload)
  
  (let ((*current-bot* bot)
        (*current-message* payload))
    (handler-case
        (on-pre-checkout-query bot payload)
      (cl-telegram-bot/response-processing:interrupt-processing (condition)
        (declare (ignore condition))
        (log:debug "Interrupting pre-checkout-query processing" payload))))
  (values))
