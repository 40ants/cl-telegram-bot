(defpackage #:cl-telegram-bot/files
  (:use #:cl))
(in-package cl-telegram-bot/files)

;; TODO: refactor

(defmacro with-ok-results ((unserialized results) &body body)
  `(let ((,results (slot-value ,unserialized (find-json-symbol :result))))
     (if (slot-value ,unserialized (find-json-symbol :ok))
         (progn ,@body)
       nil)))


(defun download-file (b file-id)
  "Get the  path for a  file from a  file-id (see: get-file)  and then
   download it.  Returns nil if the value of the http response code is
   not  success (200);  otherwise it  will returns  three values:  the
   data, the http headers and the exension of the original file"
  (with-package :cl-telegram-bot
    (let* ((file-spec (decode (get-file b file-id))))
      (with-ok-results (file-spec results)
        (alexandria:when-let* ((path      (access results 'file--path))
                               (uri       (concatenate 'string (file-endpoint b) path))
                               (extension (cl-ppcre:scan-to-strings "\\..*$" path)))
          (dexador:get uri))))))


(defun get-file (b file-id)
  "https://core.telegram.org/bots/api#getfile"
  (let ((options
         (list
          (cons :file_id file-id))))
    (make-request b "getFile" options)))

