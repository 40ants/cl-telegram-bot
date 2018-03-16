(defsystem cl-telegram-bot-tests
  :class :package-inferred-system
  :depends-on ("cl-telegram-bot-tests/tests")
  :perform (test-op :after (op c)
                    (symbol-call :rove :run c)))
