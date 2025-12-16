(uiop:define-package #:cl-telegram-bot-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:cl-telegram-bot-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "API"
                              "REPL"
                              "CL-TELEGRAM-BOT/MESSAGE:REPLY"
                              "HTTP"))

  (0.12.0 2025-12-16
          "
Changed
=======

Move API spec from 0.8.0 to 0.9.0.")
  (0.11.0 2025-12-06
          "
Added
=====

New macro CL-TELEGRAM-BOT2/ACTIONS/DELAY:DELAY and function CL-TELEGRAM-BOT2/ACTIONS/DELAY:CANCEL-DELAYED-EXECUTION were added. They allow a delayed response to the user.")

  (0.10.0 2025-11-11
          "
Changed
=======

* Invoke debugger was moved from process-update method to the outer scope allowing to catch errors even if it occurs in any :around process-update method.
* Also, now debugger is invoked only when there is an active SLY connection.

")
  (0.9.1 2025-10-05
         "
Fixed
=====

* CL-TELEGRAM-BOT2/STATES/WAIT-FOR-PAYMENT:WAIT-FOR-PAYMENT class definition was fixed.")
  (0.9.0 2025-09-17
         "
Changed
=======

* Ask-for-choice state now can accept a list or list of list of buttons and each button should be a `(cons title data)`.

Fixed
=====

* Fixed on-state-activation method for ask-for-text state. Previously it was broken in case if keyboard was not provided to the state.")
  (0.8.0 2025-09-14
         "
Changed
=======

* Combined all examples into one megabot.

Added
=====

* New action for message deletion was added. Create it
  using CL-TELEGRAM-BOT2/ACTIONS/DELETE-MESSAGES:DELETE-MESSAGES function.
  It is able to delete messages send by bot and messages received from user as well.
  By default, both message types are deleted.
* Added :ON-DELETION argument to the CL-TELEGRAM-BOT2/STATE:STATE function.
* Added a new feature allowing to render the workflow diagram of the botc.

Fixed
=====

* Inline keyboard high level constructor was fixed.
* Fixed race-conditions in threads stopping code.
* Fixed issues in pipeline processing when sent messages were not saved to the current state.
* Fixed issue with global command processing in case if there is a state which does not support command processing. But to make global commands work, you still need to ensure that current state has a correct mixin.

")
  (0.7.0 2024-12-14
         "
Huge Changes
============

A new ASDF system, cl-telegram-bot2, has been added with a completely rewritten codebase.

This new approach separates the code into low-level and high-level components.
The low-level code is generated from the Telegram API specification and encompasses all the objects and methods available in the API.

In contrast, the high-level API allows you to define the bot's behavior in a declarative manner,
 somewhat similar to how today's popular \"no-code\" bot builders operate.
However, we do it better because we have Common Lisp at our fingertips.

You can find more examples in the `examples` directory and in the CL-TELEGRAM-BOT-DOCS/TUTORIAL::@FIRST-BOT section.
")
  (0.6.0 2024-10-15
         "
Changed
=======

* CL-TELEGRAM-BOT/CHAT:GET-CHAT generic-function is now exported from cl-telegram-bot/chat package instead of cl-telegram-bot/message.
  Also, now it is applicable to updates, and other objects which can be associated with a chat.
* `callback-chat` function was removed from cl-telegram-bot/callback package. Use abovementionned `get-chat` generic-function.

Added
=====

* CL-TELEGRAM-BOT/BOT:BOT-INFO was added to CL-TELEGRAM-BOT/BOT:BOT class.
* Macro CL-TELEGRAM-BOT/BOT:DEFBOT now accepts optional slots and options like DEFCLASS macro does.
* Class CL-TELEGRAM-BOT/ENTITIES/COMMAND:BOT-COMMAND now has bot-username slot and CL-TELEGRAM-BOT/ENTITIES/COMMAND:ON-COMMAND
  generic-function is called in a group chat only if the command was addressed to a current bot. Previously, bot was not
  able to process commands in group chats.
* Some kinds of messages are wrapped into an envelope class now to distinguish between edited message, channel posts, and edited channel post. These envelope classes are gathered in package cl-telegram-bot/envelope.
* CL-TELEGRAM-BOT/MESSAGE:GET-SENDER-CHAT was added.
* CL-TELEGRAM-BOT/MESSAGE:GET-CURRENT-BOT function was added.
* Functions CL-TELEGRAM-BOT/PAYMENTS:SEND-INVOICE, CL-TELEGRAM-BOT/PAYMENTS:ANSWER-SHIPPING-QUERY and CL-TELEGRAM-BOT/PAYMENTS:ANSWER-PRE-CHECKOUT-QUERY were fixed.
* Package cl-telegram-bot/user was added with a bunch of classes and functions.

")
  (0.5.0 2024-02-18
         "
Added
=====

* Now bot can be started in debug mode. When this mode is on, then interactive debugger will pop up on errors.
* If bot defines some commands implementing CL-TELEGRAM-BOT/ENTITIES/COMMAND:ON-COMMAND generic-function, then
  these commands will be reported to the telegram server automatically and it will show the to user when he
  starts text with `/`.
* Added support for buttons with callbacks. To define a callback, implement a method for
  CL-TELEGRAM-BOT/CALLBACK:ON-CALLBACK generic-function. After that, you can construct an inline keyboard
  using CL-TELEGRAM-BOT/INLINE-KEYBOARD:INLINE-KEYBOARD function and CL-TELEGRAM-BOT/INLINE-KEYBOARD:CALLBACK-BUTTON function.
  This keyboard object can be supplied as :REPLY-MARKUP argument to CL-TELEGRAM-BOT/RESPONSE:REPLY function.
* New functions CL-TELEGRAM-BOT/RESPONSE:ALERT and CL-TELEGRAM-BOT/RESPONSE:NOTIFY were added. An example usage of these functions
  along with inline keyboard was added to `example/bot.lisp`.
* Function CL-TELEGRAM-BOT/RESPONSE-PROCESSING:INTERRUPT-PROCESSING was added in case if you want to interrupt processing of
  current message and skip the rest of the handler.
* Function CL-TELEGRAM-BOT/MESSAGE:GET-CURRENT-MESSAGE was added.
* Function CL-TELEGRAM-BOT/MESSAGE:GET-CURRENT-CHAT was added.

Removed
=======

* Function CL-TELEGRAM-BOT/MESSAGE:REPLY was removed and replaced with CL-TELEGRAM-BOT/RESPONSE:REPLY function.
  Previously it interrupted the processing flow and you only was able to reply once. With the new function
  you can respond with different pieces, for example to show user a image and text with inline keyboard.
")
  (0.4.0 2023-04-22
         "
* Changed a lot of imports some symbols not bound to functions were removed, some readers and accessors are exported.
* Added an autogenerated API reference.")
  (0.3.2 2022-07-10
         "
* Change the parameters of `make-request` to allow passing request parameters straight into it.
* Add `id` slot to `message`; add `forward-message`, `delete-message` functions reliant on `id`.
* Add more `message` types: `reply`, `video-message`, `document-message` and other media message types.
* Add `send-*` media-sending message types.
* Add more `chat` types: `group`, `supergroup`, and `channel`.
")
  (0.3.1 2020-01-04
         "
* Fixed work with latest `dexador`, because it does not accept `:stream t` anymore.
")
  (0.3.0 2019-09-22
         "
* Bot was fixed to use latest Dexador with support
  of `read-timeout` and `connect-timeout`.
")
  (0.2.0 2019-09-16
         "
* Added a dependency from `trivial-timeout` and now connect timeout is used when
  doing requests to API.
* Function `make-<bot-class>` now proxie any parameters to the class's constructor.
* Now function `stop-processing` checks if thread is alive before destroying it.

")
  (0.1.0 2018-09-17
         "
Refactorings
------------

Project was broken down to subpackages, nicknames `telegram-bot` and
`tg-bot` were removed, because now system uses ASDF's
package-inferred-system class and each file have it's own separate packages.
"))
