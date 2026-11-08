(uiop:define-package #:cl-telegram-bot2-tests/entities
  (:use #:cl)
  (:import-from #:rove
                #:ok
                #:deftest)
  (:import-from #:serapeum
                #:->)
  (:import-from #:cl-telegram-bot2/api
                #:message
                #:message-entity)
  (:import-from #:cl-telegram-bot2/entities
                #:to-markdown
                #:extract-hashtags
                #:ensure-has-schema
                #:tag-line-p
                #:remove-tag-line))
(in-package #:cl-telegram-bot2-tests/entities)


(-> make-entity (string integer integer
                 &key (:url (or null string))
                      (:language (or null string)))
    (values message-entity &optional))
(defun make-entity (type offset length &key url language)
  (make-instance 'message-entity
                 :type type
                 :offset offset
                 :length length
                 :url url
                 :language language))


(-> make-message ((or null string) list
                  &key (:caption (or null string))
                       (:caption-entities list))
    (values message &optional))
(defun make-message (text entities &key caption caption-entities)
  (make-instance 'message
                 :text text
                 :entities entities
                 :caption caption
                 :caption-entities caption-entities))


(deftest test-simple-markers ()
  (ok (string= "hello **bold** world"
               (to-markdown (make-message "hello bold world"
                                          (list (make-entity "bold" 6 4)))))
      "Bold entity is wrapped into ** **.")
  (ok (string= "hello *italic* world"
               (to-markdown (make-message "hello italic world"
                                          (list (make-entity "italic" 6 6)))))
      "Italic entity is wrapped into * *.")
  (ok (string= "use `code` here"
               (to-markdown (make-message "use code here"
                                          (list (make-entity "code" 4 4)))))
      "Code entity is wrapped into ` `."))


(deftest test-pre-entity ()
  (ok (string= "check this ```lisp
code
```
"
               (to-markdown (make-message "check this code"
                                          (list (make-entity "pre" 11 4
                                                             :language "lisp")))))
      "Pre entity with language is rendered as a fenced code block with the language.")
  (ok (string= "check this ```code
```
"
               (to-markdown (make-message "check this code"
                                          (list (make-entity "pre" 11 4)))))
      "Pre entity without language is rendered as a fenced code block."))


(deftest test-links ()
  (ok (string= "see [example.com](https://example.com) now"
               (to-markdown (make-message "see example.com now"
                                          (list (make-entity "url" 4 11)))))
      "URL entity without schema is rendered as a link with https:// schema.")
  (ok (string= "click [here](https://example.com) now"
               (to-markdown (make-message "click here now"
                                          (list (make-entity "text_link" 6 4
                                                             :url "https://example.com")))))
      "Text link entity is rendered as a link to the entity's URL."))


(deftest test-ensure-has-schema ()
  (ok (string= "https://example.com"
               (ensure-has-schema "example.com"))
      "Schema is prepended to a schema-less URL.")
  (ok (string= "http://example.com"
               (ensure-has-schema "http://example.com"))
      "URL with a schema is returned as is."))


(deftest test-unsupported-entity ()
  (ok (string= "hey @user there"
               (to-markdown (make-message "hey @user there"
                                          (list (make-entity "mention" 4 5)))))
      "Unsupported entity type is rendered as a plain text."))


(deftest test-non-bmp-emoji-before-entity ()
  ;; Telegram counts offsets and lengths in UTF-16 code units.
  ;; Ant emojis are outside of the BMP and take two code units each,
  ;; that is why "bold" starts at offset 12, not at offset 6:
  ;; 🐜 🐜 hello bold world
  ;; |__| |__| |____|
  (let ((text "🐜 🐜 hello bold world"))
    (ok (string= "🐜 🐜 hello **bold** world"
                 (to-markdown (make-message text
                                            (list (make-entity "bold" 12 4)))))
        "Markup is applied to the right word even when non-BMP emojis precede the entity.")))


(deftest test-multiple-entities ()
  (ok (string= "**bold** and *italic* end"
               (to-markdown (make-message "bold and italic end"
                                          ;; Entities are given out of order
                                          ;; to check that they are sorted by offset:
                                          (list (make-entity "italic" 9 6)
                                                (make-entity "bold" 0 4)))))
      "Multiple entities of different types are rendered and sorted by offset.")
  (ok (string= "`code` and **bold** here"
               (to-markdown (make-message "code and bold here"
                                          (list (make-entity "code" 0 4)
                                                (make-entity "bold" 9 4)))))
      "Several entities of different types are rendered in one text."))


(deftest test-extract-hashtags ()
  (ok (equal '("foo" "bar")
             (extract-hashtags (make-message "#foo and #bar"
                                             (list (make-entity "hashtag" 0 4)
                                                   (make-entity "hashtag" 9 4)))))
      "Hashtags are extracted from the message text without the leading #.")
  (ok (equal '("baz")
             (extract-hashtags (make-message nil nil
                                             :caption "#baz qux"
                                             :caption-entities (list (make-entity "hashtag" 0 4)))))
      "Hashtags are extracted from the caption when there is no text."))


(deftest test-to-markdown-nil ()
  (ok (null (to-markdown (make-message nil nil)))
      "NIL is returned for a message without text and caption.")
  (ok (string= "cap **bold** here"
               (to-markdown (make-message nil nil
                                          :caption "cap bold here"
                                          :caption-entities (list (make-entity "bold" 4 4)))))
      "Caption is used when there is no text."))


(deftest test-tag-line-p ()
  (ok (tag-line-p "#foo")
      "A hashtag line is detected.")
  (ok (tag-line-p "#foo, #bar")
      "A line with multiple hashtags is detected.")
  (ok (not (tag-line-p "# заголовок"))
      "A Markdown title with a space after # is not a tag line.")
  (ok (not (tag-line-p "## double"))
      "A line starting with ## is not a tag line.")
  (ok (not (tag-line-p "plain text"))
      "A plain text line is not a tag line."))


(deftest test-remove-tag-line ()
  (ok (string= "hello
"
               (remove-tag-line "#foo, #bar
hello"))
      "Tag lines are removed from the markdown.")
  (ok (string= "hello
"
               (remove-tag-line "hello
#foo"))
      "Tag lines are removed from the middle of the markdown.")
  (ok (string= "# заголовок
hello
"
               (remove-tag-line "# заголовок
hello"))
      "Lines which are not tag lines are kept as is.")
  (ok (string= "hello
"
               (remove-tag-line "hello"))
      "A markdown without tag lines is left intact."))
