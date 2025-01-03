(uiop:define-package #:cl-telegram-bot-docs/examples
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection))
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
  (@running section))


(defsection @running (:title "Running Examples")
  "
CL-USER> (ql:quickload :cl-telegram-bot2-examples)

CL-USER> (setf (uiop:getenv \"TELEGRAM_TOKEN\")
               \"520*****:AAH*****\")

CL-USER> (cl-telegram-bot2-examples:start)
")
