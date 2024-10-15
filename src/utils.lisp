(uiop:define-package #:cl-telegram-bot/utils
  (:use #:cl)
  (:import-from #:str)
  (:import-from #:arrows
                #:->)
  (:import-from #:serapeum
                #:collecting
                #:soft-list-of)
  (:import-from #:cl-ppcre
                #:regex-replace)
  (:import-from #:cl-strings
                #:replace-all)
  (:import-from #:kebab
                #:to-snake-case)
  (:import-from #:alexandria
                #:positive-fixnum
                #:proper-list)

  (:export #:make-keyword
           #:obfuscate
           #:api-response-to-plist
           #:split-by-lines))
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


(serapeum:-> api-response-to-plist (proper-list)
             (values proper-list &optional))

(defun api-response-to-plist (plist)
  "Transforms a plist with keys like :|foo_bar| into a plist with keys like :foo-bar.

   This can be useful to pass data into CL object contructors."
  (loop for (key value) on plist by #'cddr
        append (list (-> key
                         (symbol-name)
                         (make-keyword))
                     value)))

(serapeum:-> split-by-lines (string &key
                                    (:max-size positive-fixnum)
                                    (:trim-whitespaces-p boolean))
             (values (soft-list-of string)))

(defun split-by-lines (text &key (max-size 4096) (trim-whitespaces-p t))
  (flet ((trim-if-needed (text)
           (if trim-whitespaces-p
               (str:trim text)
               text)))
    (declare (dynamic-extent #'trim-if-needed))
    
    (collecting
      (loop with start-at = 0
            with end-at = 0
            for char across text
            for pos upfrom 0
            when (char= char #\Newline)
            do (cond
                 ((<= (- pos start-at)
                      max-size)
                  (setf end-at pos))
                 (t
                  (collect
                      (trim-if-needed
                       (subseq text start-at
                               (1+ end-at))))
                  (setf start-at
                        (1+ end-at))
                  (setf end-at
                        pos)))
            finally (collect
                        (trim-if-needed
                         (subseq text start-at)))))))
