(defpackage #:cl-telegram-bot/network
  (:use #:cl)
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

(defun process-options (options return-alist-p)
  "Process request options, returning alist if required"
  (let* ((options (alexandria:remove-from-plist options :timeout :streamp))
	 (sanitized-options (loop for (key value) on options by #'cddr
				  when value
				    collect (kebab:to-snake-case key)
				    and collect value)))
    (if return-alist-p
	(alexandria:plist-alist sanitized-options)
	sanitized-options)))

(defun make-request (bot name &rest options &key (streamp nil) (timeout 3) &allow-other-keys)
  "Perform HTTP request to 'name API method with 'options JSON-encoded object or multi-part form-data."
  (declare (ignore streamp))

  (let ((url (concatenate 'string (get-endpoint bot) name)))
    (log:debug "Posting data to"
               (obfuscate url)
               options)
    (let* ((max-timeout (* timeout 10))
	   (multipart-required (find-if #'pathnamep options))
           (processed-options (process-options options multipart-required))
           (response
             (dexador:post url
                           :headers (unless multipart-required '(("Content-Type" . "application/json")))
                           :content (if multipart-required
					processed-options
					(jonathan:to-json processed-options))
                           :read-timeout max-timeout
                           :connect-timeout max-timeout
                           :proxy (if *proxy* *proxy* dex:*default-proxy*)))
           (data (jonathan:parse response)))
      (unless (getf data :|ok|)
        (log:error "Wrong data received from the server" data)
        (error 'request-error :what data))

      (getf data :|result|))))
