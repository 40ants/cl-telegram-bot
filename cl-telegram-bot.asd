(defun search-version-in-changelog (lines)
  (let* ((line (nth 4 lines))
         (space-pos (position #\Space line)))
    (when space-pos
      (subseq line 0 space-pos))))


(defsystem cl-telegram-bot
  :description "Telegram Bot API, based on sovietspaceship's work but mostly rewritten."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :version (:read-file-line "ChangeLog.rst" :at search-version-in-changelog)
  :license "MIT"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("cl-telegram-bot/core"))
