(defsystem "cl-telegram-bot2-tests"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "MIT"
  :homepage "https://40ants.com/cl-telegram-bot/"
  :class :package-inferred-system
  :description "Provides tests for cl-telegram-bot."
  :source-control (:git "https://github.com/40ants/cl-telegram-bot")
  :bug-tracker "https://github.com/40ants/cl-telegram-bot/issues"
  :pathname "t2"
  :depends-on ("cl-telegram-bot2-tests/matchers/regex")
  :perform (test-op (op c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))
