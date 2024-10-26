(uiop:define-package #:test-bot
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/bot
                #:defbot)
  (:import-from #:cl-telegram-bot2/server
                #:stop-polling
                #:start-polling)
  (:import-from #:cl-telegram-bot2/high
                #:reply
                #:chat-state)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:cl-telegram-bot2/pipeline
                #:back-to
                #:back)
  (:import-from #:cl-telegram-bot2/api
                #:message-message-id))
(in-package #:test-bot)


;; (defun initial (bot state)
;;   (declare (ignore bot))
;;   (values state))

;; (defun show-help (bot state)
;;   (declare (ignore bot))
;;   (values state))

;; (defun ask-for-number (bot state)
;;   (declare (ignore bot))
;;   (values state))


(defclass calc (chat-state)
  ())

(defmethod cl-telegram-bot2/generics:process ((state calc) (update cl-telegram-bot2/api:update))
  (reply "Давай посчитаем!")

  (make-instance 'ask-for-number
                 :prompt "Введите первое число:"))

(defmethod cl-telegram-bot2/generics:on-result ((state calc) (result t))
  (when result
    (make-instance 'has-one-number
                   :first-num result)))


(defclass has-one-number (chat-state)
  ((first-num :initarg :first-num
              :type integer
              :accessor first-num)))


(defmethod cl-telegram-bot2/generics:process ((state has-one-number) (update cl-telegram-bot2/api:update))
  (make-instance 'ask-for-number
                 :prompt "Введите второе число:"))


(defmethod cl-telegram-bot2/generics:on-result ((state has-one-number) (result t))
  (when result
    (make-instance 'has-two-numbers
                   :first-num (first-num state)
                   :second-num result)))


(defclass has-two-numbers (chat-state)
  ((first-num :initarg :first-num
              :type integer
              :accessor first-num)
   (second-num :initarg :second-num
               :type integer
               :accessor second-num)))


(defmethod cl-telegram-bot2/generics:process ((state has-two-numbers) (update cl-telegram-bot2/api:update))
  (reply (fmt "~A + ~A = ~A"
              (first-num state)
              (second-num state)
              (+ (first-num state)
                 (second-num state))))
  (back-to 'calc))



(defclass initial (chat-state)
  ((first-num :initform nil
              :type (or null integer)
              :accessor first-num)
   (second-num :initform nil
               :type (or null integer)
               :accessor second-num)))


(defmethod cl-telegram-bot2/generics:process ((state initial) (update cl-telegram-bot2/api:update))
  (reply "Переходим к ask-for-choice.")

  (make-instance 'ask-for-choice
                 :buttons '("Foo" "Bar" "Blah"))

  ;; (progn

  ;;   (reply "Мне нужны два числа.")

  ;;   (cond
  ;;     ((null (first-num state))
  ;;      (make-instance 'ask-for-number
  ;;                     :prompt "Введите первое число:"
  ;;                     :on-number (lambda (num)
  ;;                                  (setf (first-num state)
  ;;                                        num))))
  ;;     ((null (second-num state))
  ;;      (make-instance 'ask-for-number
  ;;                     :prompt "Введите второе число:"
  ;;                     :on-number (lambda (num)
  ;;                                  (setf (second-num state)
  ;;                                        num))))
  ;;     (t
  ;;      (reply (fmt "Вот их сумма: ~A" (+ (first-num state)
  ;;                                        (second-num state)))))))
  )


(defmethod cl-telegram-bot2/generics:on-result ((state initial) (result t))
  (reply (fmt "Получен результат: ~A" result))

  (reply "Снова переходим к ask-for-choice.")

  (make-instance 'ask-for-choice
                 :buttons '("Foo" "Bar" "Blah")))


(defclass ask-for-number (chat-state)
  ((prompt :initarg :prompt
           :reader prompt)
   (on-number :initarg :on-number
              :reader on-number)))


(defmethod cl-telegram-bot2/generics:on-state-activation ((state ask-for-number))
  (reply (prompt state))
  (values))


(defmethod cl-telegram-bot2/generics:process ((state ask-for-number) (update cl-telegram-bot2/api:update))
  (reply "Спасибо за число.")
  (back (or (ignore-errors
             (parse-integer
              (let* ((message (cl-telegram-bot2/api:update-message update))
                     (text (cl-telegram-bot2/api:message-text message)))
                text)))
            1)))


(defclass ask-for-choice (chat-state)
  ((buttons :initarg :buttons
            :reader buttons)
   (message-id :initform nil
               :accessor message-id)))


(defmethod cl-telegram-bot2/generics:on-state-activation ((state ask-for-choice))
  ;; (reply "прячем клаву"
  ;;        :reply-markup
  ;;        (make-instance 'cl-telegram-bot2/api:reply-keyboard-remove
  ;;                       :remove-keyboard t))
  (let* ((message
           (reply "Выберите один из вариантов:"
                  :reply-markup (make-instance 'cl-telegram-bot2/api:inline-keyboard-markup
                                               :inline-keyboard
                                               ;; :input-field-placeholder "Выбери вариант:"
                                               ;; :keyboard
                                               (list
                                                (loop for choice in (buttons state)
                                                      collect (make-instance 'cl-telegram-bot2/api:inline-keyboard-button
                                                                             :text choice
                                                                             :callback-data choice)))))))
    (setf (message-id state)
          (message-message-id message)))
  (values))


(defmethod cl-telegram-bot2/generics:on-state-deletion ((state ask-for-choice))
  ;; (when (message-id state)
  ;;   (let ((chat-id (cl-telegram-bot2/api:chat-id cl-telegram-bot2/vars::*current-chat*)))
  ;;     (ignore-errors
  ;;      (cl-telegram-bot2/api:delete-message chat-id
  ;;                                           (message-id state)))))
  (values))


(defmethod cl-telegram-bot2/generics:process ((state ask-for-choice) (update cl-telegram-bot2/api:update))
  (let* ((message (cl-telegram-bot2/api:update-message update))
         (text (when message
                 (cl-telegram-bot2/api:message-text message)))
         (callback (cl-telegram-bot2/api:update-callback-query update))
         (callback-choice
           (when callback
             (cl-telegram-bot2/api:callback-query-data callback))))

    (cond
      ((and text
            (string-equal text "back"))
       (reply "Возвращшаетмся к предыдущему состоянию.")
       (back))
      (text
       (reply (fmt "Нужно выбрать ответ (text = ~A)."
                   text))
       (values))
      (callback-choice
       (reply (fmt "Выбран ответ: ~A"
                   callback-choice))
       (back callback-choice)))))



;; (defmethod cl-telegram-bot2/generics:process ((state ask-for-number) (update cl-telegram-bot2/api:update))
;;   (let* ((message (cl-telegram-bot2/api:update-message update))
;;          (chat (cl-telegram-bot2/api:message-chat message))
;;          (chat-id (cl-telegram-bot2/api:chat-id chat)))
;;     (cl-telegram-bot2/api:send-message chat-id "Введите число.")
;;     :back
;;     ;; (make-instance 'initial)
;;     ))


(defbot test-bot ()
  ()
  (:initial-state 'calc)
  ;; (:states (:initial initial)
  ;;          (:help show-help)
  ;;          (:ask-for-number ask-for-number))
  )


(defvar *bot* nil)


(defun stop ()
  (when *bot*
    (stop-polling *bot*)
    (setf *bot* nil)))


(defun start ()
  (stop)

  (unless *bot*
    (setf *bot*
          (make-test-bot (uiop:getenv "TELEGRAM_TOKEN"))))
  
  (start-polling *bot* :debug t))


(defun test-processing (msg)
  (log:info "TRACE" msg))


(defun clean-threads ()
  "TODO: надо разобраться почему треды не подчищаются. Возможно это происходит когда случаются ошибки?"
  (loop for tr in (bt:all-threads)
        when (or (str:starts-with? "message-thread" (bt:thread-name tr))
                 (str:starts-with? "timer-wheel" (bt:thread-name tr))
                 (str:starts-with? "telegram-bot" (bt:thread-name tr)))
        do (bt:destroy-thread tr))
  )
