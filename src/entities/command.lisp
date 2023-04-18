(defpackage #:cl-telegram-bot/entities/command
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:cl-telegram-bot/entities/core
                #:entity
                #:make-entity-internal)
  (:import-from #:cl-telegram-bot/message
                #:message
                #:get-text)
  (:import-from #:cl-telegram-bot/utils
                #:make-keyword)
  (:import-from #:cl-telegram-bot/pipeline
                #:process)
  (:export
   #:get-command
   #:bot-command
   #:get-rest-text
   #:on-command))
(in-package cl-telegram-bot/entities/command)


(defclass bot-command (entity)
  ((command :type keyword
            :initarg :command
            :reader get-command)
   (rest-text :type string
              :initarg :rest-text
              :reader get-rest-text)))


(defmethod make-entity-internal ((entity-type (eql :bot-command))
                                 (payload message) data)
  (declare (ignorable payload entity-type))
  (let* ((text (get-text payload))
         (offset (getf data :|offset|))
         (length (getf data :|length|))
         (command (make-keyword (subseq text
                                        (+ offset 1)
                                        (+ offset length))))
         (rest-text (string-trim " "
                                 (subseq text
                                         (+ offset length)))))
    (make-instance 'bot-command
                   :command command
                   :rest-text rest-text
                   :raw-data data)))


(defgeneric on-command (bot command rest-text)
  (:documentation "This method will be called for each command.
                   First argument is a keyword. If user input was /save_note, then
                   first argument will be :save-note.

                   By default, logs call and does nothing."))


(defmethod on-command ((bot t) (command t) rest-text)
  (log:debug "Command was called" command rest-text))


(defmethod process ((bot t) (command bot-command))
  (on-command bot
              (get-command command)
              (get-rest-text command)))
