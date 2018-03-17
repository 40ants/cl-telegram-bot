(defpackage #:cl-telegram-bot/utils
  (:use #:cl)
  (:import-from #:alexandria)
  (:import-from #:cl-arrows
                #:->)
  (:import-from #:cl-ppcre
                #:regex-replace)
  (:import-from #:cl-strings
                #:replace-all)

  (:export
   #:make-keyword
   #:obfuscate))
(in-package cl-telegram-bot/utils)


(defun make-keyword (text)
  (-> text
      (replace-all "_" "-")
      (nstring-upcase)
      (alexandria:make-keyword)))


(defun obfuscate (url)
  (regex-replace "/bot.*?/"
                 url
                 "/bot<token>/"))
