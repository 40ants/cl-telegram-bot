#-asdf3.1 (error "cl-telegram-bot requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "cl-telegram-bot2"
  :description "Telegram Bot API, based on sovietspaceship's work but mostly rewritten."
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
               "cl-telegram-bot2/server")
  :in-order-to ((test-op (test-op "cl-telegram-bot2-tests"))))


(defsystem "cl-telegram-bot2/deps"
  :description "Utility system to load non-package-inferred systems using package-inferred imports."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "MIT"
  :homepage "https://40ants.com/cl-telegram-bot/"
  :source-control (:git "https://github.com/40ants/cl-telegram-bot")
  :bug-tracker "https://github.com/40ants/cl-telegram-bot/issues"
  :depends-on ("njson/jzon"))


(asdf:register-system-packages "bordeaux-threads" '("BORDEAUX-THREADS-2"))
(asdf:register-system-packages "log4cl" '("LOG"))
(asdf:register-system-packages "utilities.print-items" '("PRINT-ITEMS"))
(asdf:register-system-packages "dexador" '("DEX"))
(asdf:register-system-packages "sento" '("SENTO.ACTOR-SYSTEM"
                                         "SENTO.ACTOR-CONTEXT"
                                         "SENTO.ACTOR"))
