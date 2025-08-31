(uiop:define-package #:cl-telegram-bot2-tests/matchers/regex
  (:use #:cl)
  (:import-from #:rove
                #:ok
                #:deftest)
  (:import-from #:cl-telegram-bot2/match
                #:matchp)
  (:import-from #:cl-telegram-bot2/matchers/regex
                #:regex-matcher))
(in-package #:cl-telegram-bot2-tests/matchers/regex)


(deftest test-regex-matcher ()
  (let ((matcher (regex-matcher "bar-\\d+")))
    (ok (matchp matcher "bar-1234"))
    (ok (not (matchp matcher "foo-bar-1234")))
    (ok (not (matchp matcher "bar-1234-blah")))))
