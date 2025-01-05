(uiop:define-package #:cl-telegram-bot-docs/examples
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:40ants-doc-plantuml
                #:defdiagram)
  (:import-from #:cl-telegram-bot2/debug/diagram
                #:workflow-to-text)
  (:import-from #:cl-telegram-bot2-examples/calc))
(in-package #:cl-telegram-bot-docs/examples)


(defsection @examples (:title "Examples")
  "Directory [examples](https://github.com/40ants/cl-telegram-bot/tree/master/examples) contains
   code of all v2 examples. There are different kinds of bots:

   - calc - shows how to switch states while collecting information from user and how to return results back to initial state.
   - commands - shows how to bind commands to the telegram bot states.
   - echo - a simple bot which replies with the same text.
   - gallery - demonstrates how to show an image with inline keyboard and how to edit this message, replacing the image when users has switched to the next one.
   - mini-app - this example starts a web-server and opens a mini-app inside the Telegram messenger.
   - payments - a demo showing how to send an invoice and to process a payment. You will need to register your own payment provider in the BotFather and to change provider token in the bot's code.
   - text-chain - a simple demo changing states each type bot receives a message.

   All these examples can be run on their own or as a part of the bigger Megabot. See @RUNNING section to learn how to run the Megabot.
   "
  (@running section)
  (@calc section))


(defsection @running (:title "Running Examples")
  "
To run a bot combining all examples, register some bot at BotFather, then do:

```
CL-USER> (ql:quickload :cl-telegram-bot2-examples)

CL-USER> (setf (uiop:getenv \"TELEGRAM_TOKEN\")
               \"520*****:AAH*****\")

CL-USER> (cl-telegram-bot2-examples:start)
```

")


(defsection @calc (:title "Calc Example")
  "

Example in the calc.lisp file consist of 3 types of the state.

In it's first state it greets the user and then switches to the next state
where it awaits while user will provide a number. Then it switches to the next
state where waits for another number and finally to the state where user should
choose one of the arithmetic operations:

![](asdf:cl-telegram-bot-media:images/examples/calc-bot.gif)

If we'll use /debug command to draw the diagram of the state transitions, then
it will show a picture like this:

"

  (@calc-states diagram))



(defdiagram @calc-states ()
  (workflow-to-text
   (cl-telegram-bot2-examples/calc::make-test-bot "")))

