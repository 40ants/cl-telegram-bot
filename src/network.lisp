(defpackage #:cl-telegram-bot/network
  (:use #:cl)
  (:import-from #:cl-telegram-bot/utils
                #:obfuscate)
  (:import-from #:cl-telegram-bot/bot
                #:get-endpoint)
  (:export
   #:make-request
   #:request-error))
(in-package cl-telegram-bot/network)


(define-condition request-error (error)
  ((what :initarg :what :reader what))
  (:report (lambda (condition stream)
             (format stream "Request error: ~A" (what condition)))))


(defun make-request (bot name options &key (streamp nil) timeout)
  "Perform HTTP request to 'name API method with 'options JSON-encoded object."
  (let ((url (concatenate 'string (get-endpoint bot) name)))
    (log:debug "Posting data to"
               (obfuscate url)
               options)
    (let* ((response (dexador:post url
                                   :stream streamp
                                   :headers '(("Content-Type" . "application/json"))
                                   :content (jonathan:to-json options)
                                   :timeout timeout))
           (data (jonathan:parse response)))
      (unless (getf data :|ok|)
        (log:error "Wrong data received from the server" data)
        (error 'request-error :what data))

      (getf data :|result|))))


