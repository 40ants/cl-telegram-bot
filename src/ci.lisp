(uiop:define-package #:cl-telegram-bot-ci/ci
  (:use #:cl)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/jobs/run-tests
                #:run-tests)
  (:import-from #:40ants-ci/jobs/docs
                #:build-docs)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/steps/sh
                #:sh))
(in-package #:cl-telegram-bot-ci/ci)


(defworkflow linter
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((40ants-ci/jobs/linter:linter
          :asdf-systems ("cl-telegram-bot"
                         "cl-telegram-bot2"
                         "cl-telegram-bot2-examples"
                         ;; Documentation intentionally has
                         ;; a lot of unused imports. Actually they
                         ;; are used in docstrings, but linter
                         ;; can't understand this case.
                         ;; "cl-telegram-bot-docs"
                         "cl-telegram-bot-tests")
          :env (("DYNAMIC_SPACE_SIZE" . "4Gb"))
          :check-imports t)))


(defworkflow docs
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((build-docs
          :os "ubuntu-24.04"
          :asdf-system "cl-telegram-bot-docs"
          :dynamic-space-size "4gb"
          :steps (list
                  (sh "Install PlantUML"
                      "sudo apt-get install -y plantuml")
                  ;; Ubuntu 24.04 uses old PlantUML version, so we upgrade it here:
                  (sh "Update PlantUML"
                      "curl -L https://github.com/plantuml/plantuml/releases/download/v1.2024.8/plantuml-mit-1.2024.8.jar | sudo cat > /usr/share/plantuml/plantuml.jar")))))


(defworkflow ci
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((run-tests
          :asdf-system "cl-telegram-bot"
          :lisp ("sbcl-bin"
                 "ccl-bin")
          :coverage t)))
