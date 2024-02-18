(defpackage #:cl-telegram-bot/network
  (:use #:cl)
  (:import-from #:alexandria)
  (:import-from #:dexador)
  (:import-from #:log4cl)
  (:import-from #:cl-telegram-bot/utils
                #:obfuscate)
  (:import-from #:cl-telegram-bot/bot
                #:get-endpoint)
  (:export
   #:make-request
   #:request-error
   #:set-proxy
   #:what))
(in-package cl-telegram-bot/network)

(defvar *proxy* nil)

(defun set-proxy (proxy)
  (setf *proxy* proxy))

(define-condition request-error (error)
  ((what :initarg :what
         :reader what))
  (:report (lambda (condition stream)
             (format stream "Request error: ~A" (what condition)))))


(defun make-request (bot name &rest options &key (streamp nil) (timeout 3) &allow-other-keys)
  (declare (ignore streamp))
  "Perform HTTP request to 'name API method with 'options JSON-encoded object."
  (let ((url (concatenate 'string (get-endpoint bot) name)))
    (log:debug "Posting data to"
               (obfuscate url)
               options)
    (let* ((max-timeout (* timeout 10))
           (processed-options (loop for (key value)
                                      on (alexandria:remove-from-plist options :timeout :streamp)
                                        by #'cddr
                                    when value
                                      collect (kebab:to-snake-case key)
                                      and
                                        collect value))
           (response
             (if *proxy*
                 (dexador:post url
                               :headers '(("Content-Type" . "application/json"))
                               :content (jonathan:to-json processed-options)
                               :read-timeout max-timeout
                               :connect-timeout max-timeout
                               :proxy *proxy*)
                 (dexador:post url
                               :headers '(("Content-Type" . "application/json"))
                               :content (jonathan:to-json processed-options)
                               :read-timeout max-timeout
                               :connect-timeout max-timeout)))
           (data (jonathan:parse response)))
      (unless (getf data :|ok|)
        (log:error "Wrong data received from the server" data)
        (error 'request-error :what data))

      (getf data :|result|))))
