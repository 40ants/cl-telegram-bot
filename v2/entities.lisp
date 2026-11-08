(uiop:define-package #:cl-telegram-bot2/entities
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/api
                #:message-entity-type
                #:message-entity-url
                #:message-entity-language
                #:message-entity-length
                #:message-entity-offset
                #:message-text
                #:message-caption
                #:message-entities
                #:message-caption-entities)
  (:import-from #:str)
  (:import-from #:serapeum
                #:->
                #:length<=)
  (:import-from #:trivial-utf-16
                #:to-unicode-string
                #:encode-utf-16
                #:from-unicode-string
                #:decode-utf-16)
  (:import-from #:log)
  (:documentation "Utilities to convert Telegram message entities into Markdown.")
  (:export #:to-markdown
           #:extract-hashtags
           #:ensure-has-schema
           #:tag-line-p
           #:remove-tag-line))
(in-package #:cl-telegram-bot2/entities)


(-> extract-hashtags (cl-telegram-bot2/api:message)
    (values list &optional))
(defun extract-hashtags (message)
  "Return a list of hashtags mentioned in the MESSAGE.

Hashtags are collected from the message text or, if there is no text,
from the caption. Each hashtag is returned without the leading # character."
  (let* ((text (or (message-text message)
                   (message-caption message)))
         (utf-16-points
           (encode-utf-16 (to-unicode-string text)))
         (entities (or (message-entities message)
                       (message-caption-entities message))))
    (flet ((get-substring (from to)
             (from-unicode-string
              (decode-utf-16
               (subseq utf-16-points from to)))))
      (loop for entity in entities
            for offset = (message-entity-offset entity)
            for length = (message-entity-length entity)
            when (string-equal (message-entity-type entity)
                               "hashtag")
              collect (str:trim-left (get-substring offset (+ offset length))
                                     :char-bag "#")))))


(-> ensure-has-schema (string)
    (values string &optional))
(defun ensure-has-schema (url)
  "Return the URL with an https:// schema, prepending it if the URL has none."
  (cond
    ((str:containsp "://" url)
     url)
    (t
     (concatenate 'string
                  "https://"
                  url))))


(-> format-entity (stream
                   (function (integer (or null integer))
                             (values string &optional))
                   cl-telegram-bot2/api:message-entity)
    (values null &optional))
(defun format-entity (stream get-substring entity)
  "Write the ENTITY rendered as Markdown into the STREAM.

Unsupported entity types are rendered as a plain text and logged with log:debug."
  (let ((offset (message-entity-offset entity))
        (length (message-entity-length entity))
        (type (message-entity-type entity)))
    (cond
      ((string-equal type "url")
       (let* ((url (funcall get-substring offset (+ offset length)))
              (full-url (ensure-has-schema url)))
         (format stream "[~A](~A)"
                 url
                 full-url)))
      ((string-equal type "text_link")
       (let* ((title (funcall get-substring offset (+ offset length)))
              (url (message-entity-url entity)))
         (format stream "[~A](~A)"
                 title
                 url)))
      ((string-equal type "bold")
       (format stream "**~A**"
               (funcall get-substring offset (+ offset length))))
      ((string-equal type "italic")
       (format stream "*~A*"
               (funcall get-substring offset (+ offset length))))
      ((string-equal type "code")
       (format stream "`~A`"
               (funcall get-substring offset (+ offset length))))
      ((string-equal type "pre")
       (format stream "```~@[~A~%~]~A~%```~%"
               (message-entity-language entity)
               (funcall get-substring offset (+ offset length))))
      (t
       (log:debug "Entities of type ~S are not supported and will be rendered as text."
                  type)
       (format stream "~A"
               (funcall get-substring offset (+ offset length)))))))


(-> %to-markdown (string list)
    (values string &optional))
(defun %to-markdown (text entities)
  "Convert the TEXT with ENTITIES into a Markdown string."
  (let ((sorted-entities (sort (copy-list entities)
                               #'<
                               :key #'message-entity-offset))
        ;; Telegram calculates offset and entity lengths in UTF-16 code units
        ;; Thus we need to make a vector of UTF-16 points and extract pieces of code from it.
        ;; Otherwise, symbols which are represented with two UTF-16 code units will break
        ;; our markup.
        ;; Example of such symbols: "🐜 📝 💌 👍"
        (utf-16-points
          (encode-utf-16 (to-unicode-string text))))
    (flet ((get-substring (from to)
             (from-unicode-string
              (decode-utf-16
               (subseq utf-16-points from to)))))
      (with-output-to-string (s)
        (loop with idx = 0
              for entity in sorted-entities
              for offset = (message-entity-offset entity)
              for length = (message-entity-length entity)
              do (write-string (get-substring idx offset) s)
                 (format-entity s #'get-substring entity)
                 (setf idx
                       (+ offset length))
              finally
                 (write-string (get-substring idx nil)
                               s))))))


(-> to-markdown (cl-telegram-bot2/api:message)
    (values (or null string) &optional))
(defun to-markdown (message)
  "Return the text or the caption of the MESSAGE converted into Markdown.

Returns NIL if the message has neither text nor caption."
  (let ((text (or (message-text message)
                  (message-caption message)))
        (entities (or (message-entities message)
                      (message-caption-entities message))))
    (when text
      (%to-markdown text entities))))


(-> tag-line-p (string)
    (values boolean &optional))
(defun tag-line-p (line)
  "Return T if the LINE is a tag line like #foo or #foo, #bar.

Such lines are not Markdown titles, because a Markdown title usually
has a space between # and the title text."
  (and (length<= 2 line)
       (char= (elt line 0)
              #\#)
       (not (or (char= (elt line 1)
                       #\Space)
                (char= (elt line 1)
                       #\#)))))


(-> remove-tag-line (string)
    (values string &optional))
(defun remove-tag-line (markdown)
  "Tag line is a line like:

   #foo

   or

   #foo, #bar

   We need to get rid of them because otherwise they will be rendered as titles.
   Usually in Markdown title has a space between # and title."
  (with-output-to-string (s)
    (loop for line in (str:split #\Newline markdown)
          unless (tag-line-p line)
            do (write-string line s)
               (terpri s))))
