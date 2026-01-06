(uiop:define-package #:cl-telegram-bot2/restarts
  (:use #:cl)
  (:export #:retry))
(in-package #:cl-telegram-bot2/restarts)


(defun retry (&optional condition)
  "Invokes `retry` restart if it is available or signals `control-error`."
  (let ((restart (find-restart 'retry condition)))
    (cond
      (restart
       (invoke-restart restart))
      (t
       (error 'control-error
              "There is no RETRY restart on the current stack.")))))
