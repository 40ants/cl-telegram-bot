(uiop:define-package #:cl-telegram-bot2/vars
  (:use #:cl)
  (:import-from #:serapeum
                #:defvar-unbound)
  (:export #:*default-special-bindings*))
(in-package #:cl-telegram-bot2/vars)


(defvar-unbound *current-bot*
  "An internal variable to hold current bot for replying.")

(defvar-unbound *current-user*
  "An internal variable to hold current user talking to the bot.")

(defvar-unbound *current-chat*
  "This var will be bound when PROCESS method is called on chat state.")

(defvar-unbound *current-state*
  "This var will be bound when PROCESS method is called on chat state.")


(defvar *default-special-bindings*
  nil
  "This variable holds an alist associating special variable symbols
   to forms to evaluate.

   State processing might be done by different threads that is
   why you might want to keep some context around.

   Don't modify this variable, just cons onto it. Preceeding values
   will take a priority.")
