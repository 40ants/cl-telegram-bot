(defpackage #:cl-telegram-bot/payments
  (:use #:cl))
(in-package cl-telegram-bot/payments)


;; TODO: refactor

(defun send-invoice (b chat-id title description payload provider-token start-parameter currency prices &key photo-url photo-size photo-width photo-height need-name need-phone-number need-email need-shipping-address is-flexible disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendinvoice"
  (let ((options
         (list
          (cons :chat_id chat-id)
          (cons :title title)
          (cons :description description)
          (cons :payload payload)
          (cons :provider_token provider-token)
          (cons :start_parameter start-parameter)
          (cons :currency currency)
          (cons :prices prices))))
    (when photo-url (nconc options `((:photo_url . ,photo-url))))
    (when photo-size (nconc options `((:photo_size . ,photo-size))))
    (when photo-width (nconc options `((:photo_width . ,photo-width))))
    (when photo-height (nconc options `((:photo_height . ,photo-height))))
    (when need-name (nconc options `((:need_name . ,need-name))))
    (when need-phone-number (nconc options `((:need_phone_number . ,need-phone-number))))
    (when need-email (nconc options `((:need_email . ,need-email))))
    (when need-shipping-address (nconc options `((:need_shipping_address . ,need-shipping-address))))
    (when is-flexible (nconc options `((:is_flexible . ,is-flexible))))
    (when disable-notification (nconc options `((:disable_notification . ,disable-notification))))
    (when reply-to-message-id (nconc options `((:reply_to_message_id . ,reply-to-message-id))))
    (when reply-markup (nconc options `((:reply_markup . ,reply-markup))))
    (apply #'make-request b "sendInvoice" options)))


(defun answer-shipping-query (b shipping-query-id ok &key shipping-options error-message)
  "https://core.telegram.org/bots/api#answershippingquery"
  (let ((options
         (list
          (cons :shipping_query_id shipping-query-id)
          (cons :ok ok))))
    (when shipping-options (nconc options `((:shipping_options . ,shipping-options))))
    (when error-message (nconc options `((:error_message . ,error-message))))
    (apply #'make-request b "answerShippingQuery" options)))


(defun answer-pre-checkout-query (b pre-checkout-query-id ok &key error-message)
  "https://core.telegram.org/bots/api#answerprecheckoutquery"
  (let ((options
         (list
          (cons :pre_checkout_query_id pre-checkout-query-id)
          (cons :ok ok))))
    (when error-message (nconc options `((:error_message . ,error-message))))
    (apply #'make-request b "answerPreCheckoutQuery" options)))
