(defpackage #:cl-telegram-bot/utils
  (:use #:cl)
  (:import-from #:arrows
                #:->)
  (:import-from #:cl-ppcre
                #:regex-replace)
  (:import-from #:cl-strings
                #:replace-all)
  (:import-from #:kebab
                #:to-snake-case)

  (:export
   #:make-keyword
   #:obfuscate
   #:def-telegram-call))
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


(defun make-json-keyword (arg)
  (check-type arg symbol)
  (-> arg
      (symbol-name)
      (to-snake-case)
      (alexandria:make-keyword)))


