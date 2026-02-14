(uiop:define-package #:cl-telegram-bot2/states/base
  (:use #:cl)
  (:import-from #:serapeum
                #:fmt
                #:->
                #:pretty-print-hash-table
                #:dict
                #:soft-list-of)
  (:import-from #:cl-telegram-bot2/api
                #:chat-id)
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
  (:import-from #:cl-telegram-bot2/vars
                #:*current-chat*
                #:*current-state*)
  (:import-from #:fset2
                #:do-map
                #:ch-map)
  (:import-from #:alexandria
                #:plist-alist
                #:proper-list-p)
  (:export #:var
           #:state-var
           #:clear-state-vars
           #:base-state
           #:state-id
           #:sent-message-ids
           #:state-vars
           #:received-message-ids
           #:state-initial-vars))
(in-package #:cl-telegram-bot2/states/base)


(defgeneric state-vars (state))
(defgeneric sent-message-ids (state))
(defgeneric received-message-ids (state))



(defclass state-data (print-items-mixin)
  ((vars :initarg :vars
         :initform (dict)
         :type hash-table
         :reader state-vars)
   (sent-message-ids :initform nil
                     :accessor sent-message-ids)
   (received-message-ids :initform nil
                         :accessor received-message-ids))
  (:documentation "This data is kept aside from state object, because state objects can be shared between different chats if you are using declarative style of pipeline definition."))


(defun generate-state-id ()
  (symbol-name (gensym "auto-state-id-")))


(defclass base-state (print-items-mixin)
  ((id :initarg :id
       :initform (generate-state-id)
       :type string
       :reader state-id)
   (initial-vars :initarg :vars
                 :initform nil
                 :type (or null ch-map)
                 :reader state-initial-vars
                 :documentation "Variables set on state when it was created. Immutable. When STATE-VARS generic-function is called, this value will be used as default for current chat and state."))
  (:default-initargs :vars nil))


(defvar *state-data* (make-hash-table)
  "Chat-id -> state -> data hash-table")

;; For debug
;; (clrhash *state-data*)


(-> make-immutable-vars ((or null hash-table list))
    (values (or ch-map null) &optional))

(defun make-immutable-vars (vars)
  (etypecase vars
    (null
       vars)
    (hash-table
       (fset2:convert 'ch-map vars))
    (list
       (cond
         ;; plist
         ((and (proper-list-p vars)
               (keywordp (car vars)))
          (fset2:convert 'ch-map
                         (plist-alist vars)))
         ;; alist
         ((and (proper-list-p vars)
               (consp (car vars)))
          (fset2:convert 'ch-map vars))
         (t
          (error "Unable to convert ~S to immutable map." vars))))))


(-> immutable-vars-to-dict ((or null ch-map))
    (values hash-table))

(defun immutable-vars-to-dict (vars)
  (etypecase vars
    (null
       (dict))
    (ch-map
       ;; Here we do not use fset2:convert, because even if keys are strings it will
       ;; create a hash-table with function 'eql
       (let ((result (dict)))
         (do-map (key value vars result)
           (setf (gethash key result)
                 value))))))


(defmethod initialize-instance :around ((state base-state) &rest initargs &key vars)
  (let* ((vars (make-immutable-vars vars)))
    (apply #'call-next-method
           state
           (list* :vars vars
                  initargs))))


(-> get-current-state-data ()
    (values state-data &optional))

(defun get-current-state-data ()
  (unless (boundp '*current-chat*)
    (break))
  (let* ((chat (or *current-chat*
                   (error "No current chat")))
         (chat-id (chat-id chat))
         (data-by-state (or (gethash chat-id *state-data*)
                            (setf (gethash chat-id *state-data*)
                                  (make-hash-table))))
         (state (or *current-state*
                    (error "No current state")))
         (state-id (state-id state))
         (state-data (or (gethash state-id data-by-state)
                         (setf (gethash state-id data-by-state)
                               (make-instance 'state-data
                                              :vars (immutable-vars-to-dict
                                                     (state-initial-vars state)))))))
    (values state-data)))


(-> delete-state-data (base-state)
    (values &optional))

(defun delete-state-data (state)
  (let* ((chat (or *current-chat*
                   (error "No current chat")))
         (chat-id (chat-id chat))
         (data-by-state (or (gethash chat-id *state-data*)
                            (setf (gethash chat-id *state-data*)
                                  (make-hash-table))))
         (state-id (state-id state)))
    (remhash state-id data-by-state)
    (values)))


(defmethod state-vars ((state base-state))
  (state-vars (get-current-state-data)))


(defmethod sent-message-ids ((state base-state))
  (sent-message-ids (get-current-state-data)))


(defmethod (setf sent-message-ids) ((new-value t) (state base-state))
  (setf (sent-message-ids (get-current-state-data))
        new-value))


(defmethod received-message-ids ((state base-state))
  (received-message-ids (get-current-state-data)))


(defmethod (setf received-message-ids) ((new-value t) (state base-state))
  (setf (received-message-ids (get-current-state-data))
        new-value))


(defmethod print-items append ((state state-data))
  (append
   (list
    (list :vars
          "vars = {~A}"
          (with-output-to-string (s)
            (loop for key being the hash-key of (state-vars state)
                    using (hash-value value)
                  do (format s "~S: ~S"
                             key value)))))
   (when (received-message-ids state)
     (list (list :received
                 " received = ~S"
                 (received-message-ids state))))
   
   (when (sent-message-ids state)
     (list (list :sent
                 " sent = ~S"
                 (sent-message-ids state))))))


(defmethod print-items append ((state base-state))
  (append
   (when (state-id state)
     (list (list :id
                 "id = ~S"
                 (state-id state))))
   (cond
     ((not (boundp '*current-chat*))
      (list (list :vars
                  "vars = no-current-chat")))
     ((not (zerop (hash-table-count (state-vars state))))
      (list (list :vars
                  "vars = {~A}"
                  (with-output-to-string (s)
                    (loop for key being the hash-key of (state-vars state)
                            using (hash-value value)
                          do (format s "~S: ~S"
                                     key value)))))))))


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
