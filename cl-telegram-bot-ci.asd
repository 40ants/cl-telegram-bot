(defsystem "cl-telegram-bot-ci"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "MIT"
  :homepage "https://40ants.com/cl-telegram-bot/"
  :class :package-inferred-system
  :description "Provides CI settings for cl-telegram-bot."
  :source-control (:git "https://github.com/40ants/cl-telegram-bot")
  :bug-tracker "https://github.com/40ants/cl-telegram-bot/issues"
  :pathname "src"
  :depends-on ("40ants-ci"
               "cl-telegram-bot-ci/ci"))
