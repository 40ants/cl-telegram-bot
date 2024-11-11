(uiop:define-package #:cl-telegram-bot2/spec
  (:use #:cl)
  (:import-from #:serapeum
                #:->
                #:dict
                #:fmt
                #:export-always
                #:eval-always)
  (:import-from #:dex)
  (:import-from #:log)
  (:import-from #:closer-mop
                #:slot-definition-name
                #:slot-definition-type
                #:class-slots
                #:finalize-inheritance)
  (:import-from #:alexandria
                #:hash-table-keys
                #:curry
                #:make-keyword
                #:hash-table-alist)
  (:import-from #:cl-telegram-bot2/deps)
  (:import-from #:njson
                #:jget)
  (:import-from #:cl-json
                #:simplified-camel-case-to-lisp)
  (:import-from #:str
                #:param-case)
  (:import-from #:cl-telegram-bot2/errors
                #:telegram-error)
  (:import-from #:quri))
(in-package #:cl-telegram-bot2/spec)


(eval-always
  (defvar *types* (serapeum:dict 'equal
                                 "int" 'integer
                                 "float" 'float
                                 "str" 'string
                                 "file" 'pathname
                                 "bool" 't
                                 "array" 'sequence)
    "The table with all the types API has, from the Telegram name to Lisp type")

  
  (defvar *parents* nil
    "The list of all generic classes")

  (defvar *child-to-parent* (serapeum:dict)
    "The hash table from the subclasses to their generic classes")

  
  (defvar *api-url* "https://api.telegram.org/"
    "The base URL to send bot methods to.
Bot token and method name is appended to it")

  (defvar *token* nil "Telegram bot token. Bound per bot thread")
  
  (defvar *timeout* 10
    "The timeout for telegram requests and waiting on response. Bound per bot thread."))


(defparameter *chat-member-status-to-class*
  (dict
   "kicked" "chat-member-banned"
   "left" "chat-member-left"
   "restricted" "chat-member-restricted"
   "member" "chat-member-member"
   "creator" "chat-member-owner"
   "administrator" "chat-member-administrator"))


(-> guess-generic-subclass (symbol hash-table)
    (values (or null symbol) &optional))

(defun guess-generic-subclass (generic-class object)
  (let ((result
          (cond
            ((string-equal generic-class "maybe-inaccessible-message")
             (cond
               ((equal (sort (hash-table-keys object)
                             #'string<)
                       '("chat" "date" "message-id"))
                "inaccessible-message")
               (t
                "message")))
            ((string-equal generic-class "chat-member")
             (let ((real-class
                     (gethash (gethash "status" object)
                              *chat-member-status-to-class*)))
               (or real-class
                   (error "Unable to parse generic CHAT-MEMBER with status \"~A\"."
                          (gethash "status" object))))))))
    (when result
      (values (intern (string-upcase result)
                      (symbol-package generic-class))))))


(-> parse-as (symbol t))

(defun parse-as (class-symbol object)
  (let ((real-class
          (cond
            ((member class-symbol *parents*)
             (let ((subclass (guess-generic-subclass class-symbol object)))
               (if subclass
                   subclass
                   (error "Generic ~S cant be parsed."
                          class-symbol))))
            (t class-symbol))))
    (declare (type symbol real-class))
    
    (etypecase object
      (hash-table
       (finalize-inheritance (find-class real-class))
       (apply #'make-instance
              real-class
              (loop with slots = (class-slots (find-class real-class))
                    for (key . value) in (hash-table-alist  object)
                    for name = (string-upcase (substitute #\- #\_ key))
                    for slot = (or (find name slots
                                         :test #'string-equal :key #'slot-definition-name)
                                   ;; Telegram API can add new slots which are absent from our spec.json,
                                   ;; but we should not fail in such case.
                                   (log:warn "Unable to find slot \"~A\" in class ~S. Probably spec.json file should be updated."
                                             name
                                             real-class)
                                   nil)
                    when slot
                      append (list (make-keyword name)
                                   (if (subtypep (slot-definition-type slot)
                                                 'telegram-object)
                                       (parse-as (slot-definition-type slot)
                                                 value)
                                       value)))))
      (sequence (map 'list (curry #'parse-as real-class)
                     object))
      (t object))))


(eval-always
  (export 'telegram-object)
  
  (defclass telegram-object ()
    ())
  
  (defgeneric unparse (object)
    (:method ((object t))
      object)
    (:method ((object list))
      (mapcar #'unparse
              object))
    (:method ((object telegram-object))
      (loop with result = (serapeum:dict)
            for slot in (mapcar #'closer-mop:slot-definition-name
                                (closer-mop:class-slots (class-of object)))
            when (slot-boundp object slot)
              do (setf (gethash (string-downcase (substitute #\_ #\- (symbol-name slot)))
                                result)
                       (unparse (slot-value object slot)))
            finally (return result)))
    (:documentation "Transform the object into an NJSON-friendly hash table of literal values when necessary")))


(defmethod print-object ((object telegram-object) stream)
  (print-unreadable-object (object stream :type t)
    ;; TODO: Think what to do with ID, probably generate print-object methods as well?
    ;; (when (ignore-errors (id object))
    ;;   (cl:format stream "~a=~a" 'id (id object)))
    (dolist (slot (mapcar #'closer-mop:slot-definition-name
                          (closer-mop:class-slots (class-of object))))
      (when (slot-boundp object slot)
        (cl:format stream " ~a=~a" slot
                   (slot-value object slot)
                   ;; (funcall slot object)
                   )))))


(defun invoke-method (method-name &rest args &key &allow-other-keys)
  (unless *token*
    (error 'telegram-error
           :description "Variable *TOKEN* is NIL."))

  (let* ((url *api-url*)
         (content
           (loop for (key value) on args by #'cddr
                 for prepared-key = (string-downcase (substitute #\_ #\- (symbol-name key)))
                 for prepared-value = (unparse value)
                 collect (cons prepared-key
                               (typecase prepared-value
                                 ((or pathname
                                      string)
                                  prepared-value)
                                 (t
                                  (njson:encode prepared-value))))))
         (return (njson:decode
                  (handler-case
                      (dex:post
                             ;; NOTE: probably it is better to pass token as a header?
                             ;;       I didn't find information in the official docs
                             ;;       how to do this :(
                             (quri:render-uri (quri:make-uri :path (uiop:strcat "bot" *token* "/" method-name)
                                                             :defaults url))
                             ;; NOTE: previously I've send data as JSON
                             ;; but this makes us impossible to send local files by providing
                             ;; their pathnames. Thus we let dexador to decide if to use
                             ;; application/x-www-form-urlencoded
                             ;; or
                             ;; multipart/form-data
                             ;; 
                             ;; :headers '(("Content-Type" . "application/json"))
                             :read-timeout *timeout*
                             :connect-timeout *timeout*
                             :content content)
                    (dex:http-request-failed (e)
                      (dex:response-body e))))))
    
    (cond
      ((ignore-errors (jget "ok" return))
       (jget "result" return))
      (t
       (cerror "Ignore this error"
               'telegram-error :description (jget "description" return))))))

(eval-always
  (defclass telegram-method (standard-generic-function)
    ()
    (:metaclass closer-mop:funcallable-standard-class))

  
  (defun json->name (json-name &optional (package *package*))
    (intern
     (string-upcase (param-case json-name))
     package))

  
  (defun type-name (type)
    "Return two values:
- Primitive `parse-as'-friendly type, preferably atomic.  If the TYPE
  is a mere \"array\" without element type, then, well, returns the
  corresponding Lisp array type.
- The outermost type. Same as the first value, unless array TYPE."
    (cond
      ((arrayp type)
       (let* ((inner-type (labels ((inner-type (type)
                                     (cond
                                       ((and (arrayp type)
					     (not (stringp type))
                                             (equal "array" (elt type 0)))
                                        (inner-type (elt (elt type 1) 0)))
                                       ((and (arrayp type)
					     (not (stringp type)))
                                        (elt type 0))
                                       (t type))))
                            (inner-type type)))
              (inner-type (or (gethash inner-type *types*)
                              (json->name inner-type))))
         (values
          inner-type
          (if (equal "array" (elt type 0))
              'sequence
              inner-type))))
      (t (let ((type (or (gethash type *types*)
                         (json->name type))))
           (values type type)))))

  
  (defun define-generics (generics)
    (loop for generic across generics
          for name = (json->name (jget "name" generic))
          do (setf (gethash (jget "name" generic) *types*)
                   name)
          do (loop for subtype across (jget "subtypes" generic)
		   do (setf (gethash (json->name subtype) *child-to-parent*)
			    name)
                      (pushnew name *parents*))
          collect `(export-always ',name)
          collect `(defclass ,name (telegram-object) ())))

  
  (defun define-classes (classes)
    (loop for class across classes
          for class-name
             = (json->name (jget "name" class))
          do (setf (gethash (jget "name" class) *types*)
                   class-name)
          collect `(export-always ',class-name)
          collect (let ((class-name class-name))
                    `(defclass ,class-name (,@(if (gethash class-name *child-to-parent*)
                                                  (list (gethash class-name *child-to-parent*))
                                                  (list 'telegram-object)))
                       (,@(loop for param across (jget "params" class)
                                for name = (json->name (jget "name" param))
                                collect `(,(json->name (jget "name" param))
                                          :initarg ,(alexandria:make-keyword (json->name (jget "name" param)))
                                          :type ,(if (serapeum:single (jget "type" param))
                                                     (nth-value 1 (type-name (elt (jget "type" param) 0)))
                                                     `(or ,@(map 'list (lambda (type)
									 (nth-value 1 (type-name type)))
								 (jget "type" param))))
                                          :documentation ,(jget "description" param))))
                       (:documentation ,(jget "description" class))))
          append (loop for param across (jget "params" class)
)
          append (loop for param across (jget "params" class)
                       for slot-name = (json->name
                                        (jget "name" param))
                       for name = (json->name (fmt "~A-~A"
                                                   class-name
                                                   slot-name))
                       collect `(export-always ',name)
                       ;; GETTER
                       collect `(defgeneric ,name (object)
                                  (:generic-function-class telegram-method))
                       collect `(defmethod ,name ((object ,class-name))
                                  (slot-value object ',slot-name))
                       collect `(defmethod ,name :around ((object ,class-name))
                                  (handler-case
                                      (values
                                       ,(alexandria:if-let
                                            ;; TODO: надо избавиться от этого и parse-as вызывать сразу как
                                            ;; получили изначальный объект.
                                            ((type (set-difference (map 'list #'type-name (jget "type" param))
                                                                   '(integer float string pathname t nil sequence))))
                                          `(parse-as ',(elt type 0) (call-next-method))
                                          `(call-next-method))
                                       t)
                                    (unbound-slot ()
                                      (values nil nil)))))))

  
  (defun define-methods (methods)
    (loop for method across methods
          for params = (jget "params" method)
          for method-name
             = (json->name (jget "name" method))
          for required-args
             = (remove-if (curry #'jget "optional")
                          params)
          for required-arg-names
             = (loop for arg across required-args
                     collect (json->name (jget "name" arg)))
          for optional-args
             = (remove-if (lambda (p)
                            (find p required-args))
			  params)
          for optional-arg-names
             = (loop for arg across optional-args
                     collect (json->name (jget "name" arg)))
          collect `(export-always ',method-name)
          collect `(defgeneric ,method-name
                       (,@required-arg-names
                        ,@(when (plusp (cl:length optional-args))
			    (append '(&rest args &key)
				    optional-arg-names
				    '(&allow-other-keys))))
                     (:generic-function-class telegram-method)
                     (:documentation ,(apply
                                       #'concatenate
                                       'string
                                       (jget "description" method)
                                       (string #\newline)
                                       (map 'list
					    (lambda (p)
					      (cl:format nil "~:@(~a~) -- ~a~&"
                                                         (substitute #\- #\_ (jget "name" p))
                                                         (jget "description" p)))
					    params))))
          append (labels ((type-combinations (types)
			    (cond
			      ((> (cl:length types) 1)
			       (loop for type in (elt types 0)
				     append (loop for ending in (type-combinations (subseq types 1))
						  collect (cons type ending))))
			      ((= (cl:length types) 1)
			       (map 'list #'list (elt types 0)))))
                          (method-body (method required-arg-names rest-args?)
                            `(let ((result
                                     (apply
                                      #'invoke-method
                                      ,(jget "name" method)
                                      (append
                                       (list ,@(loop for name in required-arg-names
                                                     append (list (alexandria:make-keyword name)
                                                                  name)))
                                       ,(when rest-args? 'args)))))
                               ,(if (equalp #("true") (jget "return" method))
                                    'result
                                    `(parse-as ',(type-name (elt (jget "return" method) 0))
                                               result)))))
                   (let ((combinations (type-combinations
                                        (map 'list (lambda (arg)
						     (map 'list (lambda (type) (nth-value 1 (type-name type)))
                                                          (jget "type" arg)))
					     required-args))))
                     (if combinations
                         (loop for combination in combinations
                               collect `(defmethod ,method-name (,@(loop for name in required-arg-names
                                                                         for type in combination
                                                                         collect (list name type))
                                                                 ,@(when (plusp (cl:length optional-args))
                                                                     (append '(&rest args &key)
                                                                             optional-arg-names
                                                                             '(&allow-other-keys))))
                                          (declare (ignorable ,@required-arg-names ,@optional-arg-names))
                                          ,(method-body method required-arg-names (plusp (cl:length optional-args)))))
                         `((defmethod ,method-name (,@(when (plusp (cl:length optional-args))
                                                        (append '(&rest args &key)
                                                                optional-arg-names
                                                                '(&allow-other-keys))))
                             (declare (ignorable ,@optional-arg-names))
                             ,(method-body method required-arg-names (plusp (cl:length optional-args))))))))))

  
  (defmacro define-tg-apis ()
    (let ((api (njson:decode (asdf:system-relative-pathname "cl-telegram-bot2"
                                                            (make-pathname :directory '(:relative "v2")
                                                                           :name "spec"
                                                                           :type "json")))))
      `(progn
         ,@(define-generics (jget "generics" api))
         ,@(define-classes (jget "models" api))
         ,@(define-methods (jget "methods" api))

         ;; After the expansion we need to free memory,
         ;; because this process generates abot 700Mb of ram.
         #+sbcl
         (sb-ext:gc :full t)))))


;; Difference from cl-telegram-bot-auto-api:
;; - all API symbols are interned into a separate package cl-telegram-bot2/api (in cl-telegram-bot-auto-api these symbols were mixed with symbols of the library itself.
;; - getter methods are constructed as <class-name>-<slot-name> instead of just <slot-name>.
;; - improvements in invoke-method allow to upload files by providing pathname as an argument.

