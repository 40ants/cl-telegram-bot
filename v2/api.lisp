(uiop:define-package #:cl-telegram-bot2/api
  ;; Don't use any symbols from other packages
  ;; This prevents any conflicts in future and
  ;; we don't have to shadow any symbols here:
  (:use)
  (:import-from #:cl-telegram-bot2/spec
                #:define-tg-apis))
(cl:in-package #:cl-telegram-bot2/api)


(define-tg-apis)
