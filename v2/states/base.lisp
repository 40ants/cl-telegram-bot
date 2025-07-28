(uiop:define-package #:cl-telegram-bot2/states/base
  (:use #:cl)
  (:import-from #:serapeum
                #:fmt
                #:->
                #:pretty-print-hash-table
                #:dict
                #:soft-list-of)
  (:import-from #:cl-telegram-bot2/debug/diagram/group
                #:group-name
                #:group-slots
                #:sort-slots-and-groups
                #:group)
  (:import-from #:cl-telegram-bot2/debug/diagram/utils
                #:after-object
                #:on-after-object
                #:render-objects-link)
  (:import-from #:cl-telegram-bot2/debug/diagram/slot
                #:slot
                #:slot-name
                #:slot-handlers)
  (:import-from #:sento.actor-cell
                #:*state*)
  (:import-from #:cl-telegram-bot2/debug/diagram/vars
                #:*diagram-stream*)
  (:import-from #:print-items
                #:print-items
                #:print-items-mixin)
  (:import-from #:cl-telegram-bot2/generics
                #:on-result
                #:on-state-activation
                #:process)
  (:import-from #:cl-telegram-bot2/api
                #:update
                #:update-message
                #:message-message-id)
  (:import-from #:cl-telegram-bot2/high
                #:collect-sent-messages)
  (:import-from #:cl-telegram-bot2/debug/diagram/utils
                #:obj-id
                #:render-mapslot-value-with-link)
  (:import-from #:cl-telegram-bot2/debug/diagram/generics
                #:to-text
                #:get-slots
                #:render-handlers
                #:render-handler-link)
  (:import-from #:cl-telegram-bot2/debug/diagram/vars
                #:*name-to-state*
                #:*id-to-state*
                #:*state-to-name*)
  (:export #:var
           #:state-var
           #:clear-state-vars
           #:base-state
           #:state-id
           #:sent-message-ids
           #:state-vars
           #:received-message-ids))
(in-package #:cl-telegram-bot2/states/base)


(defclass base-state (print-items-mixin)
  ((id :initarg :id
       :initform nil
       :type (or null string)
       :reader state-id)
   (vars :initform (dict)
         :reader state-vars)
   (sent-message-ids :initform nil
                     :accessor sent-message-ids)
   (received-message-ids :initform nil
                         :accessor received-message-ids)))


(defmethod print-items append ((state base-state))
  (append
   (when (state-id state)
     (list (list :id
                 "id = ~S"
                 (state-id state))))
   (unless (zerop (hash-table-count (state-vars state)))
     (list (list :vars
                 "vars = {~A}"
                 (with-output-to-string (s)

                   (loop for key being the hash-key of (state-vars state)
                         using (hash-value value)
                         do (format s "~S: ~S"
                                    key value))))))))


(defgeneric state-var (state var-name)
  (:method ((state base-state) var-name)
    (gethash var-name (state-vars state))))


(defgeneric clear-state-vars (state)
  (:method ((state base-state))
    (clrhash (state-vars state))))


(defgeneric (setf state-var) (new-value state var-name)
  (:method (new-value (state base-state) var-name)
    (setf (gethash var-name (state-vars state))
          new-value)))


(defun var (var-name)
  (loop for state in *state*
        thereis (and (typep state 'base-state)
                     (state-var state var-name))))


(defun (setf var) (new-value var-name)
  (setf (state-var (first *state*)
                   var-name)
        new-value))


(defun state-name (state)
  "Returns name of the STATE object to be used as block title
   on `PlantUML` diagram. If state object has some id, then
   this id will be used as a name. Otherwise, name will be
   generated automatically."
  (let ((id (state-id state))
        (name (gethash state *state-to-name*)))
    (flet ((store-name (name)
             (when id
               (setf (gethash id *id-to-state*)
                     state))
             (setf (gethash state *state-to-name*)
                   name)
             (setf (gethash name *name-to-state*)
                   state)
             (values name)))
      (cond
        (name
         (values name))
        (id
         (let ((name (fmt "~A '~A'"
                          (string-downcase
                           (class-name
                            (class-of state)))
                          id)))
           (store-name name)))
        (t
         (loop for idx upfrom 1
               for possible-name = (fmt "~A #~A"
                                        (string-downcase
                                         (class-name
                                          (class-of state)))
                                        idx)
               when (null (gethash possible-name *name-to-state*))
                 do (return (store-name possible-name))))))))


(defmethod render-handler-link ((state base-state))
  (let ((state-id (obj-id state))
        (state-name (state-name state)))
    (render-mapslot-value-with-link
     "goto"
     state-name
     (fmt "~A_slots" state-id))))


(defmethod to-text ((state base-state))
  (let* ((name (state-name state))
         (obj-id (cl-telegram-bot2/debug/diagram/utils:obj-id state))
         (slots-id (fmt "~A_slots"
                        obj-id))
         (slots-or-groups (sort-slots-and-groups
                           (remove nil
                                   (get-slots state)))))

    (flet ((add-slot-link (slot-name handlers-id)
             (after-object (slots-id)
               (render-objects-link 
                (fmt "\"~A::~A\""
                     slots-id
                     slot-name)
                handlers-id))))

      ;; Traverse tree of workflow depth first:
      (loop for slot-or-group in slots-or-groups
            do (to-text slot-or-group))

      ;; End of traverse,
      ;; rendering the state itself:
        
      (format *diagram-stream*
              "package ~S as ~A {~%"
              name
              obj-id)

      (loop for slot-or-group in slots-or-groups
            do (render-handlers slot-or-group))
        
      (format *diagram-stream*
              "object \"**state slots**\" as ~A {~%"
              slots-id)

      (flet ((render-slot (slot)
               (let* ((name (slot-name slot))
                      (obj-id (obj-id (slot-handlers slot)))
                      (handlers-id (fmt "~A_handlers" obj-id)))

                 (format *diagram-stream*
                         "~A~%"
                         name)
                 (add-slot-link name handlers-id))))

        (loop for slot-or-group in slots-or-groups
              do (etypecase slot-or-group
                   (slot
                      (render-slot slot-or-group))
                   (group
                      (format *diagram-stream*
                              ".. ~A ..~%"
                              (group-name slot-or-group))
                      (mapc #'render-slot
                            (sort (copy-list
                                   (group-slots slot-or-group))
                                  #'string<
                                  :key #'slot-name))))))
        
      ;; End of object
      (format *diagram-stream*
              "}~%")

      ;; Output links
      (on-after-object slots-id)
      
      ;; End of package
      (format *diagram-stream*
              "}~%"))))
