(uiop:define-package #:cl-telegram-bot-ci/ci
  (:use #:cl)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/jobs/run-tests
                #:run-tests)
  (:import-from #:40ants-ci/jobs/docs
                #:build-docs)
  (:import-from #:40ants-ci/workflow
                #:defworkflow))
(in-package #:cl-telegram-bot-ci/ci)


(defworkflow linter
  :on-push-to "clos-everywhere"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((40ants-ci/jobs/linter:linter
          :asdf-systems ("cl-telegram-bot"
                         "cl-telegram-bot-docs"
                         "cl-telegram-bot-tests")
          :env (("DYNAMIC_SPACE_SIZE" . "4Gb")))))

(defworkflow docs
  :on-push-to "clos-everywhere"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((build-docs
          :asdf-system "cl-telegram-bot-docs"
          :dynamic-space-size "4gb")))


(defworkflow ci
  :on-push-to "clos-everywhere"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((run-tests
          :asdf-system "cl-telegram-bot"
          :lisp ("sbcl-bin"
                 "ccl-bin")
          :coverage t)))
