(uiop:define-package #:cl-telegram-bot-docs/index
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  #+quicklisp
  (:import-from #:quicklisp)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:cl-telegram-bot-docs/changelog
                #:@changelog)
  (:import-from #:cl-telegram-bot-docs/states
                #:@states-and-actions)
  (:import-from #:docs-config
                #:docs-config)
  (:import-from #:40ants-doc/autodoc
                #:defautodoc)
  (:import-from #:cl-telegram-bot-docs/tutorial
                #:@first-bot)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:cl-telegram-bot-docs/index)

(in-readtable pythonic-string-syntax)


(defmethod docs-config ((system (eql (asdf:find-system "cl-telegram-bot-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (ql:quickload "40ants-doc-theme-40ants")
  #-quicklisp
  (asdf:load-system "40ants-doc-theme-40ants")
  
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))
        :full-package-names nil
        :root-sections '(@index
                         @first-bot
                         @states-and-actions
                         @api-v2
                         @v1)))


(defsection @intro (:title "cl-telegram-bot - Telegram Bot API"
                    :ignore-words ("API"
                                   "MIT"
                                   "JSON"
                                   "DSL"
                                   "CLOS"))
  (cl-telegram-bot system)
  (cl-telegram-bot2 system)
  "
[![](https://github-actions.40ants.com/40ants/cl-telegram-bot/matrix.svg?only=ci.run-tests)](https://github.com/40ants/cl-telegram-bot/actions)

![Quicklisp](http://quickdocs.org/badge/cl-telegram-bot.svg)
"
  
  (@note section)
  (@installation section)
  (@credits section))


(defsection-copy @index @intro)

(defsection-copy @readme @intro)


(defsection @note (:title "Important Note"
                   :ignore-words ("JSON"
                                  "ASDF"
                                  "DSL"
                                  "API"))
  "There are two different ASDF systems:

   - cl-telegram-bot: the legacy system that is no longer supported.
   - cl-telegram-bot2: the new version that generates classes and methods from the JSON specification and adds a declarative DSL on top.

   The new version is still incomplete, and the high-level API may change in the future.
   If you encounter any issues, please refer to ChangeLog.md.
   Pull requests with features and fixes are welcome!
")

(defsection @installation (:title "Installation")
  """
You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :cl-telegram-bot)
```
""")


(defsection @quickstart (:title "Quickstart")
  "
The system uses CLOS to add new methods to process incoming messages.
To create a simple bot, all you need is to define `on-message` method.

If you want to match on a particular command, like `/help` or `/make-me-happy 7 times`,
then you better to define a `on-command` method.

During messages processing, function `(reply \"some text\")` is available, which will send
given text into the right chat. Also, there is `send-message` and other function exists
which allow your bot to post messages, images and other media into the any chat.

Here is example of a simple bot which reacts on the text message and `/echo` command:

```lisp
CL-USER> (defpackage the-bot (:use :cl :cl-telegram-bot))
#<Package \"THE-BOT\">
CL-USER> (in-package the-bot)
#<Package \"THE-BOT\">
THE-BOT> (defbot echo-bot)
MAKE-ECHO-BOT
THE-BOT> (defmethod on-message ((bot echo-bot)
                                text)
           (reply text))
#<STANDARD-METHOD ON-MESSAGE (ECHO-BOT T)>
THE-BOT> (defmethod on-command ((bot echo-bot)
                                (command (eql :help))
                                text)
           (declare (ignorable text))
           (reply \"Just send me any text and I'll reply with the same text.\"))
#<STANDARD-METHOD ON-COMMAND (ECHO-BOT (EQL :HELP) T)>
THE-BOT> (defmethod on-command ((bot echo-bot)
                                (command (eql :start))
                                text)
           (declare (ignorable text))
           (reply \"Welcome Lisper! Have a fun, playing with cl-telegram-bot!\"))
#<STANDARD-METHOD ON-COMMAND (ECHO-BOT (EQL :START) T)>
```

Now, stop for the minute, open your Telegram client, and create a new bot
using the BotFather bot:

![](asdf:cl-telegram-bot-media:images/create-a-bot.png)

When you've got token, return to the REPL and start our bot:

```
THE-BOT> (start-processing (make-echo-bot \"5205125**********************************\")
                           :debug t)
 <INFO> [08:31:09] cl-telegram-bot core.lisp (start-processing) - Starting thread to process updates for CL-TELEGRAM-BOT/CORE::BOT: #<ECHO-BOT id=0> 
#<PROCESS telegram-bot(33) [Reset] #x30200709246D>
THE-BOT> 
```

This will start a new thread for processing incoming messages.

Now, find your bot in the Telegram client:

![](asdf:cl-telegram-bot-media:images/choose-the-bot.png)

And start communicating with him:

![](asdf:cl-telegram-bot-media:images/write-to-the-bot.png)

")


(defsection @credits (:title "Credits")
  "
* [Rei](https://github.com/sovietspaceship) – initial version.
* [Alexander Artemenko](https://github.com/svetlyak40wt) – large refactoring, usage of CLOS classes, etc.
")


(defsection @v1 (:title "v1 (old API)"
                 :ignore-words ("CLOS"
                                "REPL"
                                "HTTPS"
                                "HTTP"
                                "JSON"
                                "MIME"
                                "CL"
                                "API"))
  (@quickstart section)
  (@api section))


(defautodoc @api (:title "API v1 (old version)"
                  :system :cl-telegram-bot))

(defautodoc @api-v2 (:title "API v2"
                     :system :cl-telegram-bot2
                     :ignore-words ("CL-TELEGRAM-BOT2/API:PRE-CHECKOUT-QUERY-INVOICE-PAYLOAD"
                                    "API")
                     :ignore-packages ("cl-telegram-bot2/api")))
