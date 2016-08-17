(asdf:defsystem #:cl-telegram-bot
  :description "Telegram Bot API"
  :author "Rei <https://github.com/unwind-protect>"
  :license "MIT"
  :depends-on (#:cl-json #:drakma)
  :serial t
  :components ((:file "package")
               (:file "cl-telegram-bot")))
