(uiop:define-package #:cl-telegram-bot2/vars
  (:use #:cl)
  (:import-from #:serapeum
                #:defvar-unbound))
(in-package #:cl-telegram-bot2/vars)


(defvar-unbound *current-bot*
  "An internal variable to hold current bot for replying.")

;; (defvar-unbound *current-message*
;;   "An internal variable to hold current message for replying.")

(defvar-unbound *current-chat*
  "This var will be bound when PROCESS method is called on chat state.")
