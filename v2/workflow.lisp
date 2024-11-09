(uiop:define-package #:cl-telegram-bot2/workflow
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/states/base
                #:base-state)
  (:import-from #:cl-telegram-bot2/term/back
                #:back)
  (:import-from #:cl-telegram-bot2/action
                #:action)
  (:import-from #:serapeum
                #:soft-list-of)
  (:export #:workflow-blocks
           #:workflow-block))
(in-package #:cl-telegram-bot2/workflow)


(deftype workflow-block ()
    '(or
      base-state
      action
      back))


(deftype workflow-blocks ()
  '(soft-list-of workflow-block))


