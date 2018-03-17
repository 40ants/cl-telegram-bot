(defpackage #:cl-telegram/bot/entities/command
  (:use #:cl)
  (:import-from #:cl-telegram-bot/entities/core
                #:make-entity-internal)
  (:import-from #:cl-telegram-bot/message
                #:message
                #:get-text)
  (:import-from #:cl-telegram-bot/utils
                #:make-keyword)
  (:export
   #:get-command
   #:bot-command))
(in-package cl-telegram/bot/entities/command)


(defclass bot-command (entity)
  ((command :type :keyword
            :initarg :command
            :reader get-command)))


(defmethod make-entity-internal ((entity-type (eql :bot-command))
                                 (payload message) data)
  (declare (ignorable payload entity-type))
  (let* ((text (get-text payload))
         (offset (getf :|offset| data))
         (length (getf :|length| data))
         (command (make-keyword (subseq text
                                        offset
                                        (+ offset length)))))
    (make-instance 'bot-command
                   :command command
                   :raw-data data)))
