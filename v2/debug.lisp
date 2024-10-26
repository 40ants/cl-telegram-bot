(uiop:define-package #:cl-telegram-bot2/debug
  (:use #:cl))
(in-package #:cl-telegram-bot2/debug)


(defun queue-size (q)
  (+ (length (sento.queue::queue-head q))
     (length (sento.queue::queue-tail q))))


(defun actors-tree (bot)
  (let* ((system (cl-telegram-bot2/bot::actors-system bot))
         (actors (append (sento.actor-system::%all-actors system :user)
                         (sento.actor-system::%all-actors system :internal))))
    (loop for actor in (sort
                        ;; TODO: убедиться что sort ломает внутреннюю структуру
                        (copy-list actors)
                        #'string<
                        :key #'sento.actor-cell:name)
          for msgbox = (sento.actor-cell:msgbox actor)
          for pinned = (typep msgbox 'sento.messageb:message-box/bt)
          for thread = (when pinned
                         (slot-value msgbox 'sento.messageb::queue-thread))
          for queue = (slot-value (slot-value msgbox
                                              'sento.messageb::queue)
                                  'sento.queue::queue)
          do (if thread
                 (format t "~A: ~A (~A)~%"
                         (sento.actor-cell:name actor)
                         (queue-size queue)
                         (if (bt2:thread-alive-p thread)
                             "thread alive"
                             "thread died"))
                 (format t "~A: ~A~%"
                         (sento.actor-cell:name actor)
                         (queue-size queue))))));; 
