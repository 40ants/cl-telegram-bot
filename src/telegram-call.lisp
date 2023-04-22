(uiop:define-package #:cl-telegram-bot/telegram-call
  (:use #:cl)
  (:import-from #:arrows
                #:->)
  (:import-from #:cl-telegram-bot/utils
                #:make-json-keyword)
  (:import-from #:cl-telegram-bot/network
                #:make-request)
  (:import-from #:alexandria
                #:with-gensyms
                #:ensure-symbol
                #:make-keyword)
  (:import-from #:kebab
                #:to-camel-case))
(in-package cl-telegram-bot/telegram-call)


(defgeneric prepare-arg (arg)
  (:documentation "Returns argument as a list with two values.
   Input argument is a keyword.

   For example, if arg is :user-id, then output will be:
   (list :|user_id| user-id)

   You can redefine this method to process special cases, for example,
   :chat is such special case. Ee should transform it to pass chat_id:
   (list :|chat_id| (get-chat-id chat))
   "))


(defmethod prepare-arg ((arg t))
  `(,(make-json-keyword arg)
    ;; We need to intern symbol into the package which calls our macro
    ,(ensure-symbol arg)))


(defun get-method-name (name)
  "Returns a name for Telegram method.
   It is a camelcased string.
   As input, receives either a symbol or a list with two items."
  (typecase name
    ;; If it is a symbol, we need to create a camel-cased string from it
    (symbol (-> name
                (symbol-name)
                (to-camel-case)))
    ;; If it is a list, then just return the second item, because it denotes
    ;; a Telegram's method name.
    (list (unless (= (length name)
                     2)
            (error "~S should be a symbol or a list of two items"
                   name))
     (let ((second-item (second name)))
       (check-type second-item string)
       (values second-item)))
    (t (error "~S should be a symbol or a list of two items"
              name))))


(defun get-func-name (name)
  "Returns a name for the Lisp function to call a Telegram's method."
  (typecase name
    ;; If it is a symbol, return it as is.
    (symbol name)
    ;; If it is a list, then just return the first item, because it denotes
    ;; a Telegram's method name.
    (list (unless (= (length name)
                     2)
            (error "~S should be a symbol or a list of two items"
                   name))
     (let ((first-item (first name)))
       (check-type first-item symbol)
       (values first-item)))
    (t (error "~S should be a symbol or a list of two items"
              name))))


(defun get-docstring (body)
  (check-type body list)
  (when (typep (first body)
               'string)
    (first body)))


(defun without-docstring (body)
  "Strips docstring if it was provided."
  (check-type body list)
  (cond
    ((typep (first body)
            'string)
     (rest body))
    (t body)))


(defmacro def-telegram-call (name args &body body)
  "During the body evaluaction, result of call to API will be available
   as `response'"
  (with-gensyms (opts-var bot-var)
    (let* ((func-name (get-func-name name))
           (telegram-method-name (get-method-name name))
           (prepared-args (loop for arg in args
                                for keyworded-arg = (make-keyword arg)
                                appending (prepare-arg keyworded-arg))))
      `(defun ,func-name (,bot-var ,@args)
         ,(get-docstring body)
         (let* ((,opts-var (list ,@prepared-args))
                (response (apply #'make-request ,bot-var
                                 ,telegram-method-name
                                 ,opts-var)))
           (declare (ignorable response))
           ,@(or (without-docstring
                   body)
                 '(response)))))))
