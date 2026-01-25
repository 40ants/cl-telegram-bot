(defpackage #:cl-telegram-bot/webhooks
  (:use #:cl)
  (:import-from #:log))
(in-package cl-telegram-bot/webhooks)

;; TODO: refactor

(defun set-webhook (b url &key certificate max-connections allowed-updates)
  "https://core.telegram.org/bots/api#setwebhook"
  (let ((options
         (list
          (cons :url url))))
    (when certificate (nconc options `((:certificate . ,certificate))))
    (when max-connections (nconc options `((:max_connections . ,max-connections))))
    (when allowed-updates (nconc options `((:allowed_updates . ,allowed-updates))))
    (apply #'make-request b "setWebhook" options)))


(defun get-webhook-info (bot)
  "https://core.telegram.org/bots/api#getwebhookinfo"
  (log:debug "Retriving webhook info")
  (make-request bot "getWebhookInfo"))
