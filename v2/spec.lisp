(uiop:define-package #:cl-telegram-bot2/spec
  (:use #:cl)
  (:import-from #:serapeum
                #:soft-list-of
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
                #:read-file-into-string
                #:hash-table-keys
                #:curry
                #:make-keyword
                #:hash-table-alist)
  (:import-from #:cl-json
                #:simplified-camel-case-to-lisp)
  (:import-from #:str
                #:param-case)
  (:import-from #:cl-telegram-bot2/errors
                #:telegram-error)
  (:import-from #:quri)
  (:import-from #:global-vars
                #:define-global-parameter)
  (:import-from #:cl-telegram-bot2/utils
                #:to-json
                #:from-json)
  (:export
   #:telegram-object))
(in-package #:cl-telegram-bot2/spec)


(eval-always
  (defvar *types* (dict "int" 'integer
                        "float" 'float
                        "str" 'string
                        "file" 'pathname
                        "bool" 't
                        "array" 'sequence)
    "The table with all the types API has, from the Telegram name to Lisp type")

  (defparameter *adjusted-types*
    (dict "InputMediaPhoto"
          (dict "media"
                '(or string pathname))))
  
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


(-> parse-as ((soft-list-of symbol) symbol t))

(defun parse-as (all-generic-classes class-symbol object)
  (let ((real-class
          (cond
            ((member class-symbol all-generic-classes)
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
                                       (parse-as all-generic-classes
                                                 (slot-definition-type slot)
                                                 value)
                                       value)))))
      (sequence (map 'list (curry #'parse-as
                                  all-generic-classes
                                  real-class)
                     object))
      (t object))))


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
  (:documentation "Transform the object into a hash table of literal values when necessary"))


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
           ;; editMessageMedia documentation say that media file can be uploaded
           ;; using multipart/form-data. But to make this work, we need to extract
           ;; pathnames from the nested structures to the top level and to replace
           ;; these nested pathnames with links like attach://<file_attach_name>
           (let ((pathnames-to-upload nil))
             (labels ((replace-pathnames (obj)
                        "This function replaces pathname values in the dicts with a placeholder
                         and pushes these placeholders and pathnames to PATHNAMES-TO-UPLOAD alist."
                        (typecase obj
                          (list (mapcar #'replace-pathnames
                                        obj))
                          (hash-table
                             (loop with data-to-change = nil
                                   with attach-number = 0
                                   for key being the hash-key of obj
                                     using (hash-value value)
                                   when (typep value 'pathname)
                                     do (let ((attach-name (fmt "attach-"
                                                                (incf attach-number))))
                                          (push (cons attach-name
                                                      value)
                                                pathnames-to-upload)
                                          ;; Replace value with the placeholder.
                                          ;; NOTE: I'm not sure if changing value of existing key
                                          ;; should work correctly for every CL implementation,
                                          ;; thus we'll store updates into the variable
                                          ;; and apply them after the loop
                                          (push (cons key (fmt "attach://~A"
                                                               attach-name))
                                                data-to-change))
                                   finally (loop for (key . value) in data-to-change
                                                 do (setf (gethash key obj)
                                                          value)))
                             (values obj))
                          (t
                             obj))))
               (append
                (loop for (key value) on args by #'cddr
                      for prepared-key = (string-downcase (substitute #\_ #\- (symbol-name key)))
                      for prepared-value = (replace-pathnames
                                            (unparse value))
                      collect (cons prepared-key
                                    (typecase prepared-value
                                      ((or pathname
                                           string)
                                         prepared-value)
                                      (t
                                         (to-json prepared-value)))))
                pathnames-to-upload))))
         (return (from-json
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
      ((ignore-errors (gethash "ok" return))
       (gethash "result" return))
      (t
       (cerror "Ignore this error"
               'telegram-error :description (gethash "description" return))))))

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
- Primitive `parse-as'-friendly type, preferably atomic. If the TYPE
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
    (loop with child-to-parent = (dict)
          for generic across generics
          for name = (json->name (gethash "name" generic))
          do (setf (gethash (gethash "name" generic) *types*)
                   name)
          do (loop for subtype across (gethash "subtypes" generic)
		   do (setf (gethash (json->name subtype)
                                     child-to-parent)
			    name))
          collect name into parents
          collect `(export-always ',name)
            into forms
          collect `(defclass ,name (telegram-object)
                     ())
            into forms
          finally (return (values forms parents child-to-parent))))


  (-> adjust-type (string string (or symbol
                                     (soft-list-of symbol))))

  (defun adjust-type (class-name slot-name original-type)
    (or
     (let ((slot-types
             (gethash class-name *adjusted-types*)))
       (when slot-types
         (let ((type-or-func (gethash slot-name slot-types)))
           (when type-or-func
             (cond
               ((or (typep type-or-func 'function)
                    (and (typep type-or-func 'symbol)
                         (fboundp type-or-func)))
                (funcall type-or-func original-type))
               (t
                type-or-func))))))
     original-type))
  
  
  (defun define-classes (all-generics-symbol child-to-parent classes)
    (loop for class across classes
          for string-class-name = (gethash "name" class)
          for class-name = (json->name string-class-name)
          do (setf (gethash (gethash "name" class) *types*)
                   class-name)
          collect `(export-always ',class-name)
          collect (let ((class-name class-name))
                    `(defclass ,class-name (,@(if (gethash class-name child-to-parent)
                                                (list (gethash class-name child-to-parent))
                                                (list 'telegram-object)))
                       (,@(loop for param across (gethash "params" class)
                                for string-slot-name = (gethash "name" param)
                                for slot-name = (json->name string-slot-name)
                                for initarg = (alexandria:make-keyword slot-name)
                                for documentation = (gethash "description" param)
                                for type = (cond
                                             ((serapeum:single (gethash "type" param))
                                              (nth-value 1 (type-name (elt (gethash "type" param) 0))))
                                             (t
                                              `(or ,@(map 'list (lambda (type)
								  (nth-value 1 (type-name type)))
							  (gethash "type" param)))))
                                for adjusted-type = (adjust-type string-class-name string-slot-name type)
                                collect `(,(json->name (gethash "name" param))
                                          :initarg ,initarg
                                          :type ,adjusted-type
                                          :documentation ,documentation)))
                       (:documentation ,(gethash "description" class))))
          append (loop for param across (gethash "params" class)
                       )
          append (loop for param across (gethash "params" class)
                       for slot-name = (json->name
                                        (gethash "name" param))
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
                                            ((type (set-difference (map 'list #'type-name (gethash "type" param))
                                                                   '(integer float string pathname t nil sequence))))
                                          `(parse-as ,all-generics-symbol
                                                     ',(elt type 0)
                                                     (call-next-method))
                                          `(call-next-method))
                                       t)
                                    (unbound-slot ()
                                      (values nil nil)))))))

  
  (defun define-methods (all-generics-symbol methods)
    (loop for method across methods
          for params = (gethash "params" method)
          for method-name
            = (json->name (gethash "name" method))
          for required-args
            = (remove-if (curry #'gethash "optional")
                         params)
          for required-arg-names
            = (loop for arg across required-args
                    collect (json->name (gethash "name" arg)))
          for optional-args
            = (remove-if (lambda (p)
                           (find p required-args))
			 params)
          for optional-arg-names
            = (loop for arg across optional-args
                    collect (json->name (gethash "name" arg)))
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
                                       (gethash "description" method)
                                       (string #\newline)
                                       (map 'list
					    (lambda (p)
					      (cl:format nil "~:@(~a~) -- ~a~&"
                                                         (substitute #\- #\_ (gethash "name" p))
                                                         (gethash "description" p)))
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
                                      ,(gethash "name" method)
                                      (append
                                       (list ,@(loop for name in required-arg-names
                                                     append (list (alexandria:make-keyword name)
                                                                  name)))
                                       ,(when rest-args? 'args)))))
                               ,(if (equalp #("true") (gethash "return" method))
                                  'result
                                  `(parse-as ,all-generics-symbol
                                             ',(type-name (elt (gethash "return" method) 0))
                                             result)))))
                   (let ((combinations (type-combinations
                                        (map 'list (lambda (arg)
						     (map 'list (lambda (type) (nth-value 1 (type-name type)))
                                                          (gethash "type" arg)))
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
    (let ((api (from-json
                (read-file-into-string
                 (asdf:system-relative-pathname "cl-telegram-bot2"
                                                (make-pathname :directory '(:relative "v2")
                                                               :name "spec"
                                                               :type "json"))))))
      (multiple-value-bind (generic-forms all-generic-classes child-to-parent)
          (define-generics (gethash "generics" api))
        (let* ((all-generics-symbol (intern "*ALL-GENERIC-CLASSES*"))
               (classes-forms (define-classes all-generics-symbol child-to-parent (gethash "models" api)))
               (methods-forms (define-methods all-generics-symbol (gethash "methods" api))))

          `(progn
             (define-global-parameter ,all-generics-symbol ',all-generic-classes
               "This var will hold a list of all parent classes defined in a spec.")
           
             ,@generic-forms
             ,@classes-forms
             ,@methods-forms

             ;; After the expansion we need to free memory,
             ;; because this process generates abot 700Mb of ram.
             #+sbcl
             (sb-ext:gc :full t)))))))


;; Difference from cl-telegram-bot-auto-api:
;; - all API symbols are interned into a separate package cl-telegram-bot2/api (in cl-telegram-bot-auto-api these symbols were mixed with symbols of the library itself.
;; - getter methods are constructed as <class-name>-<slot-name> instead of just <slot-name>.
;; - improvements in invoke-method allow to upload files by providing pathname as an argument.

