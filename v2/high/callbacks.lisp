(uiop:define-package #:cl-telegram-bot2/high/callbacks
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:import-from #:cl-telegram-bot2/api
                #:update-callback-query
                #:callback-query-data
                #:update)
  (:export #:get-callback-data))
(in-package #:cl-telegram-bot2/high/callbacks)


(-> get-callback-data (update)
    (values string &optional))


(defun get-callback-data (update)
  "Extracts callback query data from the UPDATE object."
  (let* ((query (update-callback-query update)))
    (callback-query-data query)))
