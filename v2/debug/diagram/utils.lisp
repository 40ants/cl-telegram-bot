(uiop:define-package #:cl-telegram-bot2/debug/diagram/utils
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/debug/diagram/vars
                #:*state-to-name*
                #:*id-to-state*
                #:*name-to-state*
                #:*obj-to-id*
                #:*id-to-obj*
                #:*current-map-id*
                #:*current-obj-id*
                #:*diagram-stream*
                #:*on-after-object*
                #:*objects-created*)
  (:import-from #:alexandria
                #:once-only)
  (:import-from #:serapeum
                #:push-end)
  (:import-from #:cl-telegram-bot2/debug/diagram/generics
                #:render-handler-link)
  (:export #:find-state-by-id
           #:render-mapslot-value-with-link
           #:render-mapslot-value
           #:obj-id
           #:after-object
           #:render-objects-link
           #:on-after-object
           #:render-handlers-inner))
(in-package #:cl-telegram-bot2/debug/diagram/utils)


(defun find-state-by-id (state-id)
  "Returns a state with given ID.

   Works only during `PlantUML` diagram rendering."
  (gethash state-id
           *id-to-state*))


(defun obj-id (obj)
  "Returns an alias of obj to be used in `PlantUML` diagram as a reference."
  (let ((obj-id (gethash obj *obj-to-id*)))
    (cond
      (obj-id
       (values obj-id))
      (t
       (loop for idx upfrom 1
             for possible-id = (format nil "obj_~A"
                                       idx)
             when (null (gethash possible-id *id-to-obj*))
               do (return (progn
                            (setf (gethash obj *obj-to-id*)
                                  possible-id)
                            (setf (gethash possible-id *id-to-obj*)
                                  obj)
                            (values possible-id))))))))


(defmacro with-on-after (&body body)
  `(cond
     ((boundp '*objects-created*)
      (error "Nested call of WITH-ON-AFTER is prohibited."))
     (t
      (let ((*objects-created* nil)
            (*on-after-object* (make-hash-table :test 'equal)))
        ,@body))))


(defmacro after-object ((obj-id) &body body)
  "Executes block of code after the `PlantUML` entity with OBJ-ID alias
   has been rendered. Useful for ensuring that both objects are known
   to the `PlantUML` renderer when rendering a link between objects."
  (once-only (obj-id)
    `(let ((already-created (member ,obj-id *objects-created*
                                    :test #'equal)))
       (cond
         (already-created
          ,@body)
         (t
          (push-end
           (let ((dynamic-bindings (when (boundp '*current-map-id*)
                                     (list (cons '*current-map-id*
                                                 *current-map-id*)))))
             (lambda ()
               (progv
                   (mapcar #'car dynamic-bindings)
                   (mapcar #'cdr dynamic-bindings)
                 ,@body)))
           (gethash ,obj-id *on-after-object*))))
       (values))))


(defun on-after-object (obj-id)
  "Call this function after you've finished rendering of the `PlantUML` object."
  (loop for callback in (gethash obj-id *on-after-object*)
        do (funcall callback))
  
  (push obj-id
        *objects-created*)
  
  (values))


(defun render-objects-link (from to)
  (format *diagram-stream*
          "~A ----> ~A~%"
          from
          to))


(defmacro render-map ((name id) &body body)
  (once-only (name id)
    `(let ((*current-map-id* ,id))
       (format *diagram-stream*
               "map \"~A\" as ~A {~%"
               ,name
               ,id)

       ,@body
    
       (format *diagram-stream*
               "}~%"))))


(defun render-mapslot-link (from to)
  (format *diagram-stream*
          "~A *---> ~A~%"
          from
          to))


(defun render-mapslot-value-with-link (key value link-to-obj-id)
  (format *diagram-stream*
          "~A => ~A~%"
          key
          value)
  ;; We don't want this piece to render inside the current
  ;; map and we need it to render only after the linked object
  ;; will be rendered. Otherwise PlantUML will complain
  ;; it is now know about such object.
  (after-object (*current-obj-id*)
    (after-object (link-to-obj-id)
      (format *diagram-stream*
              "~A::~A ---> ~A~%"
              *current-map-id*
              key
              link-to-obj-id))))


(defun render-mapslot-value (key value)
  (format *diagram-stream*
          "~A => ~A~%"
          key
          value))


(defun render-handlers-inner (handlers obj-id)
  (let ((handlers-id (format nil "~A_handlers"
                             obj-id))
        (handlers (uiop:ensure-list handlers)))
    (when handlers
      (render-map ("handlers" handlers-id)

        (loop for handler in handlers
              do (render-handler-link handler))))))
