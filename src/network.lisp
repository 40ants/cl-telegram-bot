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

(defun process-options (&rest options &key (pathname-p nil) &allow-other-keys)
  (loop for (key value)
          on (alexandria:remove-from-plist options :timeout :streamp :pathname-p)
	by #'cddr
	when value
	  if pathname-p
	    collect (cons (kebab:to-snake-case key) value)
	else
	  collect (kebab:to-snake-case key)
	  and
            collect value))


(defun make-request (bot name &rest options &key (streamp nil) (timeout 3) (pathname-p nil) &allow-other-keys)
  "Perform HTTP request to 'name API method with 'options JSON-encoded object or multi-part form-data."
  (declare (ignore streamp))

  (let ((url (concatenate 'string (get-endpoint bot) name)))
    (log:debug "Posting data to"
               (obfuscate url)
               options)
    (let* ((max-timeout (* timeout 10))
           (processed-options (apply #'process-options options))
           (response
             (dexador:post url
                           :headers (unless pathname-p '(("Content-Type" . "application/json")))
                           :content (if pathname-p processed-options (jonathan:to-json processed-options))
                           :read-timeout max-timeout
                           :connect-timeout max-timeout
                           :proxy (if *proxy* *proxy* dex:*default-proxy*)))
           (data (jonathan:parse response)))
      (unless (getf data :|ok|)
        (log:error "Wrong data received from the server" data)
        (error 'request-error :what data))

      (getf data :|result|))))
