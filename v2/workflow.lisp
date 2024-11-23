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
           #:workflow-block
           #:funcallable-symbol))
(in-package #:cl-telegram-bot2/workflow)


(deftype funcallable-symbol ()
  '(and symbol
    (satisfies fboundp)))


(deftype workflow-block ()
  '(or
    funcallable-symbol
    base-state
    action
    back))


(deftype workflow-blocks ()
  '(soft-list-of workflow-block))


