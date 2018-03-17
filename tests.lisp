(defpackage #:cl-telegram-bot-tests/tests
  (:use #:cl
        #:rove
        #:cl-arrows)
  (:import-from #:cl-telegram-bot/message
                #:get-entities
                #:get-text
                #:get-chat)
  (:import-from #:cl-telegram-bot/update
                #:get-update-id
                #:get-payload
                #:make-update))
(in-package cl-telegram-bot-tests/tests)

(deftest convert-message-into-the-object
  (let* ((data '(:|message|
                 (:|entities| ((:|type| "bot_command" :|length| 5 :|offset| 0))
                  :|text| "/echo Привет Мир!"
                  :|date| 1521048276
                  :|chat| (:|type| "private"
                           :|username| "svetlyak40wt"
                           :|last_name| "svetlyak40wt"
                           :|first_name| "Alexander Artemenko"
                           :|id| 76226374)
                  :|from| (:|language_code| "en"
                           :|username| "svetlyak40wt"
                           :|last_name| "svetlyak40wt"
                           :|first_name| "Alexander Artemenko"
                           :|is_bot| NIL
                           :|id| 76226374)
                  :|message_id| 3)
                 :|update_id| 617953963))
         (update (make-update data)))

    (ok (= (get-update-id update)
           617953963)
        "Update should have an ID.")
    
    (let* ((message (get-payload update)))
      
      (ok message
          "Update object should have a message inside")

      (ok (string=
           (get-text message)
           "/echo Привет Мир!")
          "And message should have text \"/echo Привет Мир!\"")

      (let ((chat (get-chat message)))
        (ok chat
            "Message should be bound to a private chat")
        (ok (typep chat 'cl-telegram-bot/chat:private-chat)))

      (let ((entities (get-entities message)))
        (ok (= (length entities)
               1))
        (let ((entity (first entities)))
          (ok (typep entity )))))))
