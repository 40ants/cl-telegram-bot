#-asdf3.1 (error "cl-telegram-bot requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "cl-telegram-bot2"
  :description "Telegram Bot API, completely rewritten. Autogenerates code from JSON spec and adds high-level declarative DSL on top."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "MIT"
  :homepage "https://40ants.com/cl-telegram-bot/"
  :source-control (:git "https://github.com/40ants/cl-telegram-bot")
  :bug-tracker "https://github.com/40ants/cl-telegram-bot/issues"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "v2"
  :depends-on ("cl-telegram-bot2/api"
               "cl-telegram-bot2/pipeline"
               "cl-telegram-bot2/server"
               "cl-telegram-bot2/state"
               "cl-telegram-bot2/actions/delete-messages"
               "cl-telegram-bot2/actions/edit-message-media"
               "cl-telegram-bot2/actions/send-invoice"
               "cl-telegram-bot2/actions/send-photo"
               "cl-telegram-bot2/actions/send-text"
               "cl-telegram-bot2/actions/delete-messages"
               "cl-telegram-bot2/actions/delay"
               "cl-telegram-bot2/matchers/string"
               "cl-telegram-bot2/matchers/regex"
               "cl-telegram-bot2/states/ask-for-text"
               "cl-telegram-bot2/states/ask-for-choice"
               "cl-telegram-bot2/states/ask-for-number"
               "cl-telegram-bot2/high/keyboard"
               "cl-telegram-bot2/high/callbacks"
               ;; Work in progress on these modules
               "cl-telegram-bot2/actor-reference"
               "cl-telegram-bot2/debug")
  :in-order-to ((test-op (test-op "cl-telegram-bot2-tests"))))


(asdf:register-system-packages "bordeaux-threads" '("BORDEAUX-THREADS-2"))
(asdf:register-system-packages "log4cl" '("LOG"))
(asdf:register-system-packages "utilities.print-items" '("PRINT-ITEMS"))
(asdf:register-system-packages "dexador" '("DEX"))
(asdf:register-system-packages "sento" '("SENTO.ACTOR-SYSTEM"
                                         "SENTO.ACTOR-CONTEXT"
                                         "SENTO.ACTOR"
                                         "SENTO.WHEEL-TIMER"
                                         "SENTO.ACTOR-CELL"))
