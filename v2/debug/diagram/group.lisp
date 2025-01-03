(uiop:define-package #:cl-telegram-bot2/debug/diagram/group
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/debug/diagram/generics
                #:to-text
                #:render-handlers)
  (:import-from #:cl-telegram-bot2/debug/diagram/slot
                #:slot
                #:slot-name
                #:to-slot)
  (:export #:group
           #:group-name
           #:group-slots))
(in-package #:cl-telegram-bot2/debug/diagram/group)


(defclass group ()
  ((name :initarg :name
         :reader group-name)
   (slots :initarg :slots
          :reader group-slots)))


(defun group (name objects)
  (when objects
    (make-instance 'group
                   :name name
                   :slots
                   (mapcar #'to-slot
                           objects))))


(defmethod render-handlers ((group group))
    (loop for slot in (group-slots group)
          do (render-handlers slot)))


(defmethod to-text ((group group))
  (to-text (group-slots group)))


(defun sort-slots-and-groups (objs)
  (sort (copy-list objs)
        (lambda (left right)
          (cond
            ((and (typep left 'slot)
                  (typep right 'slot))
             (string< (slot-name left)
                      (slot-name right)))
            ((and (typep left 'slot)
                  (typep right 'group))
             t)
            ((and (typep left 'group)
                  (typep right 'slot))
             nil)
            ((and (typep left 'group)
                  (typep right 'group))
             (string< (group-name left)
                      (group-name right)))))))
