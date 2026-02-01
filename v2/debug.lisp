(uiop:define-package #:cl-telegram-bot2/debug
  (:use #:cl)
  (:import-from #:bordeaux-threads-2
                #:thread-alive-p)
  (:import-from #:sento.actor-cell)
  (:import-from #:sento.messageb)
  (:import-from #:sento.actor-system)
  (:import-from #:cl-telegram-bot2/bot)
  (:export
   #:bot-actors-info))
(in-package #:cl-telegram-bot2/debug)


;; (defun queue-size (q)
;;   (etypecase q
;;     (sento.queue::queue
;;      (+ (length (sento.queue::queue-head q))
;;         (length (sento.queue::queue-tail q))))
;;     (sento.queue:queue-bounded
;;      (sento.queue:queued-count q))
    
;;     (sento.queue:queue-unbounded
;;      (let ((inner-queue (slot-value q
;;                                     'sento.queue::queue)))
;;        (queue-size inner-queue)))))


(defun bot-actors-info (bot)
  (actors-info (cl-telegram-bot2/bot::actors-system bot)))


(defun actors-info (system &key (verbose nil))
  (let* ((actors (append (sento.actor-system::%all-actors system :user)
                         (sento.actor-system::%all-actors system :internal))))
    (loop for actor in (sort
                        ;; Sort is destructive, so we have to copy actors list here
                        (copy-list actors)
                        #'string<
                        :key #'sento.actor-cell:name)
          for msgbox = (sento.actor-cell:msgbox actor)
          for pinned = (typep msgbox 'sento.messageb:message-box/bt)
          for thread = (when pinned
                         (slot-value msgbox 'sento.messageb::queue-thread))
          for queue = (slot-value msgbox
                                  'sento.messageb::queue)
          do (if thread
                 (format t "~A: ~A (~A) ~@[msgbox-name=~A~]~%"
                         (sento.actor-cell:name actor)
                         (sento.queue:queued-count queue)
                         ;; (queue-size queue)
                         (if (thread-alive-p thread)
                             "thread alive"
                             "thread died")
                         (when verbose
                           (sento.messageb::name msgbox)))
                 (format t "~A: ~A~%"
                         (sento.actor-cell:name actor)
                         (sento.queue:queued-count queue)
                         ;; (queue-size queue)
                         ))))) ;; 

