(uiop:define-package #:cl-telegram-bot/entities/command
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
  (:import-from #:cl-telegram-bot/bot
                #:bot
                #:sent-commands-cache)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:serapeum
                #:soft-alist-of
                #:soft-list-of)
  (:import-from #:closer-mop
                #:generic-function-methods
                #:method-specializers)
  (:import-from #:cl-telegram-bot/commands
                #:set-my-commands)
  (:import-from #:str
                #:replace-all)
  (:export #:get-command
           #:bot-command
           #:get-rest-text
           #:on-command))
(in-package #:cl-telegram-bot/entities/command)


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


(declaim (ftype (function (bot) (soft-list-of closer-mop:method))
                bot-methods))

(defun bot-methods (bot)
  (loop for method in (generic-function-methods #'on-command)
        for specializers = (method-specializers method)
        when (eql (first specializers)
                  (class-of bot))
        collect method))


(declaim (ftype (function (bot)
                          (soft-alist-of string string))
                bot-commands))

(defun bot-commands (bot)
  (loop for method in (bot-methods bot)
        for specializers = (closer-mop:method-specializers method)
        for specializer = (second specializers)
        when (typep specializer 'closer-mop:eql-specializer)
        collect (cons (replace-all "-" "_"
                                   (string-downcase
                                    (closer-common-lisp:eql-specializer-object specializer)))
                      (or (documentation method t)
                          "No documentation."))))


(declaim (ftype (function (bot &key (:command-name-to-check (or null
                                                                string)))
                          (soft-alist-of string string))
                update-commands))

(defun update-commands (bot &key command-name-to-check)
  (let ((commands (bot-commands bot)))
    ;; We don't want to send commands each time when user
    ;; enters /blah-something to prevent DoS attacks.
    ;; That is why we update commands list on the server
    ;; only if command is known:
    (when (or (null command-name-to-check)
              (assoc-value (sent-commands-cache bot)
                           command-name-to-check
                           :test #'string-equal))
      (set-my-commands bot commands))
    
    (values commands)))


(defmethod process ((bot t) (command bot-command))
  (let* ((command-name (get-command command))
         (command-str-name (str:replace-all "-" "_"
                                            (string-downcase command-name))))
    (unless (assoc-value (sent-commands-cache bot)
                         command-str-name
                         :test #'string-equal)
      (setf (sent-commands-cache bot)
            (update-commands bot
                             :command-name-to-check command-str-name)))
    
    (on-command bot
                command-name
                (get-rest-text command))))
