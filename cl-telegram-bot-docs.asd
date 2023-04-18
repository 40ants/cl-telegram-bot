(defsystem "cl-telegram-bot-docs"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "MIT"
  :homepage "https://40ants.com/cl-telegram-bot/"
  :class :package-inferred-system
  :description "Provides documentation for cl-telegram-bot."
  :source-control (:git "https://github.com/40ants/cl-telegram-bot")
  :bug-tracker "https://github.com/40ants/cl-telegram-bot/issues"
  :pathname "docs"
  :depends-on ("cl-telegram-bot"
               "cl-telegram-bot-docs/index"))
