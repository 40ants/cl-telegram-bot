(asdf:defsystem #:cl-telegram-bot
  :description "Telegram Bot API"
  :author "Rei <https://github.com/unwind-protect>"
  :license "MIT"
  :depends-on (#:cl-json #:drakma)
  :serial t
  :components ((:file "cl-telegram-bot.lisp" "package.lisp")))
