<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# cl-telegram-bot - Telegram Bot API

<a id="cl-telegram-bot-asdf-system-details"></a>

## CL-TELEGRAM-BOT ASDF System Details

* Description: Telegram Bot `API`, based on sovietspaceship's work but mostly rewritten.
* Licence: `MIT`
* Author: Alexander Artemenko <svetlyak.40wt@gmail.com>
* Homepage: [https://40ants.com/cl-telegram-bot/][6949]
* Bug tracker: [https://github.com/40ants/cl-telegram-bot/issues][5798]
* Source control: [GIT][53d1]
* Depends on: [alexandria][8236], [anaphora][c9ae], [arrows][b590], [bordeaux-threads][3dbf], [cl-ppcre][49b9], [cl-strings][2ecb], [closer-mop][61a4], [dexador][8347], [jonathan][6dd8], [kebab][5186], [log4cl][7f8b], [serapeum][c41d], [str][ef7f], [trivial-backtrace][fc0e], [yason][aba2]

[![](https://github-actions.40ants.com/40ants/cl-telegram-bot/matrix.svg?only=ci.run-tests)][7bb5]

![](http://quickdocs.org/badge/cl-telegram-bot.svg)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40INSTALLATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Installation

You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :cl-telegram-bot)
```
<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40V2-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## v2

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FSTATES-3A-3A-40STATES-AND-ACTIONS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### States and Actions

This framework makes it possible to define bot with all allowed state.

The state defines behaviour of the bot, the way it should respond to commands, updates and other events.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FSTATES-3A-3A-40STATES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### States

There can be more than one handler for the event. We call these handlers "Actions".

An action should return a `NIL` or a new state. In latter case, the current bot's state will be changed to the new one and handlers for `on-activation` event will be called.

State is constructed using [`state`][03e8] function, which accepts handlers for different kinds of events. Here is simples state which greets a user when it start the chat and then reply with the same text:

```
(defun reply-with-same-text (update)
  (reply (message-text
          (update-message update)))
  (values))


(state (send-text "Hello, I'm the echo bot.")
       :on-update 'reply-with-same-text)
```
The first argument to [`state`][03e8] function is a handler for `on-activation` event. If you don't want to react on activation, you can pass `NIL` instead. The [`send-text`][5d6f] function returns an action instance. This way, we tell what bot should do, we use a declarative way to describe bot's behaviour.

The `:ON-UPDATE` argument specifies a handler for `on-update` event. This is the most generic event which occur when bot receives an update which wasn't processed by other event handlers. For this handler we are using a custom function bound to the symbol `reply-with-same-text`. The function accepts a single argument - update object. Use generic functions from `cl-telegram-bot2/api` package to work with this update object.

The reason why we only accept a special action object or a symbol but not a lambda function is because this way we'll be able to generate schemas of all states and transitions between them. Another reason is that it will be possible to redefine fbound function and use interactive approach to changing bot's behaviour.

See other support events in [`state`][03e8] function documentation.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FSTATES-3A-3A-40ACTIONS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Actions

Actions in cl-telegra-bot are small objects holding an information about what should be done on some event. Typically, you will want to reply with some text or send a photo.

Usually, actions are created using a function having the same name as action's class. Here are which actions are available:

* [`send-text`][5d6f]
* [`send-photo`][7c91]
* [`send-invoice`][d23c]
* [`edit-message-media`][46f2]

More actions will be added in future and you can create your own.

Also, a function bound symbol can be used instead an action object. Why do we require a symbol but not a function object? Because symbol has a name and it can be useful when we want to save bot's state or to render states graph.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FSTATES-3A-3A-40EVENT-PROCESSING-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Event processing

When some event occur, a corresponding generic function is called first on state object then on an action specified for this kind.

For example, if new update was received, then [`cl-telegram-bot2/generics:process`][9647] generic-function will be called with current state as the first argument
and update object as the second argument. Then the method specified on state class will call the same [`cl-telegram-bot2/generics:process`][9647] generic-function
on the object specified as `:ON-UPDATE` argument for the action. If action is a symbol, then it's function will be called with update object as a single argument in case if this function accepts one argument and without any arguments otherwise.

Action's method should return should return a new state object if it wants to change the current bot's state or `NIL` otherwise. If new state was returned, then `on-activate` event will be processed afterwards.

Instead of one action a list of actions can be specified as an event handler. In this case processing will stop on an action which returns a new state.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FTUTORIAL-3A-3A-40FIRST-BOT-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Our First Bot Tutorial

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40API-V2-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### API

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FACTION-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT2/ACTION

<a id="x-28-23A-28-2823-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT2-2FACTION-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot2/action`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FACTION-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FACTION-24ACTION-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### ACTION

<a id="x-28CL-TELEGRAM-BOT2-2FACTION-3AACTION-20CLASS-29"></a>

####### [class](f8a1) `action` ()

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FACTION-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT2-2FACTION-3ACALL-IF-ACTION-20FUNCTION-29"></a>

###### [function](935e) `call-if-action` obj func &rest args

Useful in [`cl-telegram-bot2/generics:process`][9647] handlers in case if
state has additional handler stored in the slot and this
slot can be either state or action.

This function is recursive, because processing of an action
could return another action and we should call `FUNC` until
a new state or `NIL` will be returned.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FACTIONS-2FEDIT-MESSAGE-MEDIA-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT2/ACTIONS/EDIT-MESSAGE-MEDIA

<a id="x-28-23A-28-2843-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT2-2FACTIONS-2FEDIT-MESSAGE-MEDIA-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot2/actions/edit-message-media`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FACTIONS-2FEDIT-MESSAGE-MEDIA-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FACTIONS-2FEDIT-MESSAGE-MEDIA-24EDIT-MESSAGE-MEDIA-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### EDIT-MESSAGE-MEDIA

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FEDIT-MESSAGE-MEDIA-3AEDIT-MESSAGE-MEDIA-20CLASS-29"></a>

####### [class](62c9) `edit-message-media` (action)

**Readers**

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FEDIT-MESSAGE-MEDIA-3A-3ACAPTION-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FACTIONS-2FEDIT-MESSAGE-MEDIA-3AEDIT-MESSAGE-MEDIA-29-29"></a>

####### [reader](51de) `caption` (edit-message-media) (:caption)

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FEDIT-MESSAGE-MEDIA-3A-3AINLINE-KEYBOARD-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FACTIONS-2FEDIT-MESSAGE-MEDIA-3AEDIT-MESSAGE-MEDIA-29-29"></a>

####### [reader](8a73) `inline-keyboard` (edit-message-media) (:inline-keyboard)

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FEDIT-MESSAGE-MEDIA-3A-3AMEDIA-PATH-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FACTIONS-2FEDIT-MESSAGE-MEDIA-3AEDIT-MESSAGE-MEDIA-29-29"></a>

####### [reader](63dc) `media-path` (edit-message-media) (:path)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FACTIONS-2FEDIT-MESSAGE-MEDIA-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FEDIT-MESSAGE-MEDIA-3AEDIT-MESSAGE-MEDIA-20FUNCTION-29"></a>

###### [function](cb16) `edit-message-media` path-or-func-name &key caption inline-keyboard

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT2/ACTIONS/SEND-INVOICE

<a id="x-28-23A-28-2837-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot2/actions/send-invoice`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-24SEND-INVOICE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### SEND-INVOICE

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3ASEND-INVOICE-20CLASS-29"></a>

####### [class](b31b) `send-invoice` (action)

**Readers**

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3A-3ACOMMANDS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3ASEND-INVOICE-29-29"></a>

####### [reader](5d6d) `commands` (send-invoice) (:commands = nil)

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3A-3ACURRENCY-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3ASEND-INVOICE-29-29"></a>

####### [reader](f37b) `currency` (send-invoice) (:currency)

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3A-3ADESCRIPTION-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3ASEND-INVOICE-29-29"></a>

####### [reader](841b) `description` (send-invoice) (:description)

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3A-3AON-SUCCESS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3ASEND-INVOICE-29-29"></a>

####### [reader](e70b) `on-success` (send-invoice) (:on-success)

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3A-3APAYLOAD-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3ASEND-INVOICE-29-29"></a>

####### [reader](2bc5) `payload` (send-invoice) (:payload)

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3A-3APRICES-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3ASEND-INVOICE-29-29"></a>

####### [reader](ffe1) `prices` (send-invoice) (:prices)

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3A-3APROVIDER-TOKEN-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3ASEND-INVOICE-29-29"></a>

####### [reader](6ffa) `provider-token` (send-invoice) (:provider-token)

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3A-3ATITLE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3ASEND-INVOICE-29-29"></a>

####### [reader](9343) `title` (send-invoice) (:title)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3ASEND-INVOICE-20FUNCTION-29"></a>

###### [function](44fc) `send-invoice` title description payload provider-token currency prices &key on-success commands

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-PHOTO-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT2/ACTIONS/SEND-PHOTO

<a id="x-28-23A-28-2835-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-PHOTO-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot2/actions/send-photo`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-PHOTO-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-PHOTO-24SEND-PHOTO-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### SEND-PHOTO

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-PHOTO-3ASEND-PHOTO-20CLASS-29"></a>

####### [class](db28) `send-photo` (action)

**Readers**

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-PHOTO-3A-3ACAPTION-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-PHOTO-3ASEND-PHOTO-29-29"></a>

####### [reader](5a24) `caption` (send-photo) (:caption)

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-PHOTO-3A-3AIMAGE-PATH-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-PHOTO-3ASEND-PHOTO-29-29"></a>

####### [reader](23c1) `image-path` (send-photo) (:path)

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-PHOTO-3A-3AINLINE-KEYBOARD-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-PHOTO-3ASEND-PHOTO-29-29"></a>

####### [reader](79e9) `inline-keyboard` (send-photo) (:inline-keyboard)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-PHOTO-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-PHOTO-3ASEND-PHOTO-20FUNCTION-29"></a>

###### [function](6b64) `send-photo` path-or-func-name &key caption inline-keyboard

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-TEXT-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT2/ACTIONS/SEND-TEXT

<a id="x-28-23A-28-2834-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-TEXT-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot2/actions/send-text`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-TEXT-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-TEXT-24SEND-TEXT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### SEND-TEXT

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-TEXT-3ASEND-TEXT-20CLASS-29"></a>

####### [class](e756) `send-text` (action)

**Readers**

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-TEXT-3A-3APARSE-MODE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-TEXT-3ASEND-TEXT-29-29"></a>

####### [reader](aebc) `parse-mode` (send-text) (:parse-mode = nil)

Supported values are: `"Markdown"`, `"MarkdownV2"` or `"HTML"`. Read more about formatting options in the Telegram documentaion: https://core.telegram.org/bots/api#formatting-options

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-TEXT-3A-3AREPLY-MARKUP-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-TEXT-3ASEND-TEXT-29-29"></a>

####### [reader](ba2b) `reply-markup` (send-text) (:reply-markup = nil)

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-TEXT-3A-3ATEXT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-TEXT-3ASEND-TEXT-29-29"></a>

####### [reader](caff) `text` (send-text) (:text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-TEXT-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-TEXT-3ASEND-TEXT-20FUNCTION-29"></a>

###### [function](8d82) `send-text` text-or-func-name &key reply-markup parse-mode

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FBOT-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT2/BOT

<a id="x-28-23A-28-2820-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT2-2FBOT-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot2/bot`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FBOT-3FMacros-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Macros

<a id="x-28CL-TELEGRAM-BOT2-2FBOT-3ADEFBOT-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

###### [macro](7b29) `defbot` name base-classes &optional slots &rest options

Use this macro to define a class of your Telegram bot.

Each bot has a state machine inside. The simplest bot has only one state:

```
(defbot test-bot ()
  ()
  (:initial-state
   (state (send-text "Hello world!"))))
```
This bot will green each who activates it.

To learn more about bot states and actions see [`States and Actions`][8e99] section.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FERRORS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT2/ERRORS

<a id="x-28-23A-28-2823-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT2-2FERRORS-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot2/errors`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FERRORS-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FERRORS-24TELEGRAM-ERROR-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### TELEGRAM-ERROR

<a id="x-28CL-TELEGRAM-BOT2-2FERRORS-3ATELEGRAM-ERROR-20CONDITION-29"></a>

####### [condition](f6f1) `telegram-error` (error)

**Readers**

<a id="x-28CL-TELEGRAM-BOT2-2FERRORS-3AERROR-DESCRIPTION-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FERRORS-3ATELEGRAM-ERROR-29-29"></a>

####### [reader](f6f1) `error-description` (telegram-error) (:DESCRIPTION = '(REQUIRED-ARGUMENT
  "DESCRIPTION is required argument for TELEGRAM-ERROR class."))

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FGENERICS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT2/GENERICS

<a id="x-28-23A-28-2825-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT2-2FGENERICS-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot2/generics`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FGENERICS-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Generics

<a id="x-28CL-TELEGRAM-BOT2-2FGENERICS-3AON-PRE-CHECKOUT-QUERY-20GENERIC-FUNCTION-29"></a>

###### [generic-function](5431) `on-pre-checkout-query` bot query

Pre-checkout-query object will be passed as this single arguement and
function should return a boolean. When the function return True, user
may proceed to the payment.

Pre-checkout queries are not bound the the chat, so
current-chat and current-state are not available during processing.
This is why methods of this generic function should be defined on bot class.

You can use `CL-TELEGRAM-BOT2/API:PRE-CHECKOUT-QUERY-INVOICE-PAYLOAD` function
to extract payload from the query and find associated invoice.

<a id="x-28CL-TELEGRAM-BOT2-2FGENERICS-3AON-RESULT-20GENERIC-FUNCTION-29"></a>

###### [generic-function](6b39) `on-result` state result

This method is called when some state exits and returns a result using `BACK` function.

<a id="x-28CL-TELEGRAM-BOT2-2FGENERICS-3AON-STATE-ACTIVATION-20GENERIC-FUNCTION-29"></a>

###### [generic-function](a4ae) `on-state-activation` state

This method is called when chat actor's state is changed to a given `STATE`.

Such hook can be used to send some prompt to the user.

<a id="x-28CL-TELEGRAM-BOT2-2FGENERICS-3AON-STATE-DELETION-20GENERIC-FUNCTION-29"></a>

###### [generic-function](214b) `on-state-deletion` state

This method is called when chat actor's state is returned from a given `STATE` back to the previous state.

The method is called only when state is removed from the stack. When a new state is added to the stack,
this method will not be called for a previous state.

Such hook can be used to hide a keyboard or to delete temporary messages.

<a id="x-28CL-TELEGRAM-BOT2-2FGENERICS-3APROCESS-20GENERIC-FUNCTION-29"></a>

###### [generic-function](4dd8) `process` bot-or-state object

This method is called by when processing a single update.
It is called multiple times on different parts of an update.
Whole pipeline looks like that:

For each update we call:
  process(bot, update)
  process(actor-state, update)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FHIGH-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT2/HIGH

<a id="x-28-23A-28-2821-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT2-2FHIGH-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot2/high`

High level `API` for implementing Telegram bots.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FHIGH-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FHIGH-24CHAT-STATE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### CHAT-STATE

<a id="x-28CL-TELEGRAM-BOT2-2FHIGH-3ACHAT-STATE-20CLASS-29"></a>

####### [class](ee98) `chat-state` ()

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FHIGH-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT2-2FHIGH-3AREPLY-20FUNCTION-29"></a>

###### [function](89a5) `reply` text &rest rest &key business-connection-id message-thread-id parse-mode entities link-preview-options disable-notification protect-content allow-paid-broadcast message-effect-id reply-parameters reply-markup

<a id="x-28CL-TELEGRAM-BOT2-2FHIGH-3AREPLY-WITH-PHOTO-20FUNCTION-29"></a>

###### [function](18d9) `reply-with-photo` photo &rest rest &key business-connection-id message-thread-id caption parse-mode caption-entities show-caption-above-media has-spoiler disable-notification protect-content allow-paid-broadcast message-effect-id reply-parameters reply-markup

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FHIGH-3FMacros-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Macros

<a id="x-28CL-TELEGRAM-BOT2-2FHIGH-3ACOLLECT-SENT-MESSAGES-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

###### [macro](2905) `collect-sent-messages` &body body

Returns as the first value a list of messages created by [`reply`][60a4] function called
during `BODY` execution. Values returned by the `BODY` code are returned as the second,
third and following arguments.

Also, messages are collected when these actions are called:

* `cl-telegram-bot2/actions/send-text:send-text` ([`1`][5d6f] [`2`][c8e7])
* `cl-telegram-bot2/actions/send-photo:send-photo` ([`1`][7c91] [`2`][e0f8])

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FSPEC-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT2/SPEC

<a id="x-28-23A-28-2821-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT2-2FSPEC-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot2/spec`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FSPEC-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FSPEC-24TELEGRAM-OBJECT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### TELEGRAM-OBJECT

<a id="x-28CL-TELEGRAM-BOT2-2FSPEC-3ATELEGRAM-OBJECT-20CLASS-29"></a>

####### [class](e4e2) `telegram-object` ()

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FSTATE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT2/STATE

<a id="x-28-23A-28-2822-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT2-2FSTATE-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot2/state`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FSTATE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FSTATE-24STATE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### STATE

<a id="x-28CL-TELEGRAM-BOT2-2FSTATE-3ASTATE-20CLASS-29"></a>

####### [class](ece1) `state` (state-with-commands-mixin base-state)

**Readers**

<a id="x-28CL-TELEGRAM-BOT2-2FSTATE-3A-3AON-ACTIVATION-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FSTATE-3ASTATE-29-29"></a>

####### [reader](cfb7) `on-activation` (state) (:on-activation = nil)

<a id="x-28CL-TELEGRAM-BOT2-2FSTATE-3A-3AON-CALLBACK-QUERY-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FSTATE-3ASTATE-29-29"></a>

####### [reader](0aec) `on-callback-query` (state) (:on-callback-query = nil)

<a id="x-28CL-TELEGRAM-BOT2-2FSTATE-3A-3AON-RESULT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FSTATE-3ASTATE-29-29"></a>

####### [reader](8c0d) `on-result` (state) (:on-result = nil)

<a id="x-28CL-TELEGRAM-BOT2-2FSTATE-3A-3AON-UPDATE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FSTATE-3ASTATE-29-29"></a>

####### [reader](115c) `on-update` (state) (:on-update = nil)

<a id="x-28CL-TELEGRAM-BOT2-2FSTATE-3A-3AON-WEB-APP-DATA-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FSTATE-3ASTATE-29-29"></a>

####### [reader](4978) `on-web-app-data` (state) (:on-web-app-data = nil)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FSTATE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT2-2FSTATE-3ASTATE-20FUNCTION-29"></a>

###### [function](de12) `state` on-activation &key id commands on-update on-result on-callback-query on-web-app-data

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FSTATE-WITH-COMMANDS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT2/STATE-WITH-COMMANDS

<a id="x-28-23A-28-2836-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT2-2FSTATE-WITH-COMMANDS-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot2/state-with-commands`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FSTATE-WITH-COMMANDS-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FSTATE-WITH-COMMANDS-24COMMAND-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### COMMAND

<a id="x-28CL-TELEGRAM-BOT2-2FSTATE-WITH-COMMANDS-3ACOMMAND-20CLASS-29"></a>

####### [class](0685) `command` (base-command)

This type of command is available only in the state where it is defined.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FSTATE-WITH-COMMANDS-24GLOBAL-COMMAND-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### GLOBAL-COMMAND

<a id="x-28CL-TELEGRAM-BOT2-2FSTATE-WITH-COMMANDS-3AGLOBAL-COMMAND-20CLASS-29"></a>

####### [class](7d88) `global-command` (command)

This command will be available during in all bot states.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FSTATE-WITH-COMMANDS-24STATE-WITH-COMMANDS-MIXIN-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### STATE-WITH-COMMANDS-MIXIN

<a id="x-28CL-TELEGRAM-BOT2-2FSTATE-WITH-COMMANDS-3ASTATE-WITH-COMMANDS-MIXIN-20CLASS-29"></a>

####### [class](9fe8) `state-with-commands-mixin` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT2-2FSTATE-WITH-COMMANDS-3ASTATE-COMMANDS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FSTATE-WITH-COMMANDS-3ASTATE-WITH-COMMANDS-MIXIN-29-29"></a>

####### [reader](cfdf) `state-commands` (state-with-commands-mixin) (:commands = nil)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FSTATE-WITH-COMMANDS-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT2-2FSTATE-WITH-COMMANDS-3ACOMMAND-20FUNCTION-29"></a>

###### [function](fea3) `command` name handler &key description

<a id="x-28CL-TELEGRAM-BOT2-2FSTATE-WITH-COMMANDS-3AGLOBAL-COMMAND-20FUNCTION-29"></a>

###### [function](0b6d) `global-command` name handler &key description

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FSTATES-2FBASE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT2/STATES/BASE

<a id="x-28-23A-28-2828-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT2-2FSTATES-2FBASE-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot2/states/base`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FSTATES-2FBASE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FSTATES-2FBASE-24BASE-STATE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### BASE-STATE

<a id="x-28CL-TELEGRAM-BOT2-2FSTATES-2FBASE-3ABASE-STATE-20CLASS-29"></a>

####### [class](6d13) `base-state` (print-items-mixin)

**Readers**

<a id="x-28CL-TELEGRAM-BOT2-2FSTATES-2FBASE-3ASENT-MESSAGE-IDS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FSTATES-2FBASE-3ABASE-STATE-29-29"></a>

####### [reader](029c) `sent-message-ids` (base-state) (= nil)

<a id="x-28CL-TELEGRAM-BOT2-2FSTATES-2FBASE-3ASTATE-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FSTATES-2FBASE-3ABASE-STATE-29-29"></a>

####### [reader](e4b3) `state-id` (base-state) (:id = nil)

<a id="x-28CL-TELEGRAM-BOT2-2FSTATES-2FBASE-3A-3ASTATE-VARS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FSTATES-2FBASE-3ABASE-STATE-29-29"></a>

####### [reader](4482) `state-vars` (base-state) (= (dict))

**Accessors**

<a id="x-28CL-TELEGRAM-BOT2-2FSTATES-2FBASE-3ASENT-MESSAGE-IDS-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-TELEGRAM-BOT2-2FSTATES-2FBASE-3ABASE-STATE-29-29"></a>

####### [accessor](029c) `sent-message-ids` (base-state) (= nil)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FSTATES-2FBASE-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Generics

<a id="x-28CL-TELEGRAM-BOT2-2FSTATES-2FBASE-3ACLEAR-STATE-VARS-20GENERIC-FUNCTION-29"></a>

###### [generic-function](15c2) `clear-state-vars` state

<a id="x-28CL-TELEGRAM-BOT2-2FSTATES-2FBASE-3ASTATE-VAR-20GENERIC-FUNCTION-29"></a>

###### [generic-function](4b91) `state-var` state var-name

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FSTATES-2FBASE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT2-2FSTATES-2FBASE-3AVAR-20FUNCTION-29"></a>

###### [function](bcc3) `var` var-name

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FSTATES-2FWAIT-FOR-PAYMENT-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT2/STATES/WAIT-FOR-PAYMENT

<a id="x-28-23A-28-2840-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT2-2FSTATES-2FWAIT-FOR-PAYMENT-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot2/states/wait-for-payment`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FSTATES-2FWAIT-FOR-PAYMENT-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FSTATES-2FWAIT-FOR-PAYMENT-24WAIT-FOR-PAYMENT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### WAIT-FOR-PAYMENT

<a id="x-28CL-TELEGRAM-BOT2-2FSTATES-2FWAIT-FOR-PAYMENT-3AWAIT-FOR-PAYMENT-20CLASS-29"></a>

####### [class](72c2) `wait-for-payment` (state-with-commands-mixin base-state)

**Readers**

<a id="x-28CL-TELEGRAM-BOT2-2FSTATES-2FWAIT-FOR-PAYMENT-3A-3AON-SUCCESS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FSTATES-2FWAIT-FOR-PAYMENT-3AWAIT-FOR-PAYMENT-29-29"></a>

####### [reader](d979) `on-success` (wait-for-payment) (:on-success = nil)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FSTATES-2FWAIT-FOR-PAYMENT-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT2-2FSTATES-2FWAIT-FOR-PAYMENT-3AWAIT-FOR-PAYMENT-20FUNCTION-29"></a>

###### [function](b8dc) `wait-for-payment` &key on-success commands

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FTERM-2FBACK-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT2/TERM/BACK

<a id="x-28-23A-28-2826-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT2-2FTERM-2FBACK-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot2/term/back`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FTERM-2FBACK-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FTERM-2FBACK-24BACK-TO-ID-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### BACK-TO-ID

<a id="x-28CL-TELEGRAM-BOT2-2FTERM-2FBACK-3ABACK-TO-ID-20CLASS-29"></a>

####### [class](ea4e) `back-to-id` (back)

**Readers**

<a id="x-28CL-TELEGRAM-BOT2-2FTERM-2FBACK-3APARENT-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FTERM-2FBACK-3ABACK-TO-ID-29-29"></a>

####### [reader](2f47) `parent-id` (back-to-id) (:ID = (REQUIRED-ARGUMENT "Parent id is required argument."))

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FTERM-2FBACK-24BACK-TO-NTH-PARENT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### BACK-TO-NTH-PARENT

<a id="x-28CL-TELEGRAM-BOT2-2FTERM-2FBACK-3ABACK-TO-NTH-PARENT-20CLASS-29"></a>

####### [class](76a3) `back-to-nth-parent` (back)

**Readers**

<a id="x-28CL-TELEGRAM-BOT2-2FTERM-2FBACK-3APARENT-NUMBER-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FTERM-2FBACK-3ABACK-TO-NTH-PARENT-29-29"></a>

####### [reader](ff05) `parent-number` (back-to-nth-parent) (:N = (REQUIRED-ARGUMENT "Parent number required argument."))

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FTERM-2FBACK-24BACK-TO-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### BACK-TO

<a id="x-28CL-TELEGRAM-BOT2-2FTERM-2FBACK-3ABACK-TO-20CLASS-29"></a>

####### [class](d94a) `back-to` (back)

**Readers**

<a id="x-28CL-TELEGRAM-BOT2-2FTERM-2FBACK-3ASTATE-CLASS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FTERM-2FBACK-3ABACK-TO-29-29"></a>

####### [reader](4f6c) `state-class` (back-to) (:STATE-CLASS = (REQUIRED-ARGUMENT "State class is required argument."))

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FTERM-2FBACK-24BACK-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### BACK

<a id="x-28CL-TELEGRAM-BOT2-2FTERM-2FBACK-3ABACK-20CLASS-29"></a>

####### [class](4744) `back` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT2-2FTERM-2FBACK-3ARESULT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT2-2FTERM-2FBACK-3ABACK-29-29"></a>

####### [reader](3e45) `result` (back) (:result = nil)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FTERM-2FBACK-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT2-2FTERM-2FBACK-3ABACK-20FUNCTION-29"></a>

###### [function](a4fc) `back` &optional result

<a id="x-28CL-TELEGRAM-BOT2-2FTERM-2FBACK-3ABACK-TO-20FUNCTION-29"></a>

###### [function](52e8) `back-to` state-class &optional result

<a id="x-28CL-TELEGRAM-BOT2-2FTERM-2FBACK-3ABACK-TO-ID-20FUNCTION-29"></a>

###### [function](ad5d) `back-to-id` id &optional result

<a id="x-28CL-TELEGRAM-BOT2-2FTERM-2FBACK-3ABACK-TO-NTH-PARENT-20FUNCTION-29"></a>

###### [function](417b) `back-to-nth-parent` n &optional result

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT2-2FUTILS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT2/UTILS

<a id="x-28-23A-28-2822-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT2-2FUTILS-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot2/utils`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FUTILS-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Generics

<a id="x-28CL-TELEGRAM-BOT2-2FUTILS-3ADEEP-COPY-20GENERIC-FUNCTION-29"></a>

###### [generic-function](677d) `deep-copy` object

Does a general deep-copy on the given object and sub-pieces.
Returns atoms, numbers and chars. 
Runs copy-tree on lists, and copy-seq on other sequences.
Runs copy-structure on pathnames, hash tables and other structure-objects

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT2-2FUTILS-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT2-2FUTILS-3AARITY-20FUNCTION-29"></a>

###### [function](c7dd) `arity` funcallable

<a id="x-28CL-TELEGRAM-BOT2-2FUTILS-3ACALL-IF-NEEDED-20FUNCTION-29"></a>

###### [function](3d05) `call-if-needed` value &rest args

If value is a fbound `SYMBOL`, then calls as a function and then returns a result.

<a id="x-28CL-TELEGRAM-BOT2-2FUTILS-3AFROM-JSON-20FUNCTION-29"></a>

###### [function](2353) `from-json` string

<a id="x-28CL-TELEGRAM-BOT2-2FUTILS-3ATO-JSON-20FUNCTION-29"></a>

###### [function](83c1) `to-json` obj

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40V1-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## v1

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40QUICKSTART-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Quickstart

The system uses `CLOS` to add new methods to process incoming messages.
To create a simple bot, all you need is to define `on-message` method.

If you want to match on a particular command, like `/help` or `/make-me-happy 7 times`,
then you better to define a `on-command` method.

During messages processing, function `(reply "some text")` is available, which will send
given text into the right chat. Also, there is `send-message` and other function exists
which allow your bot to post messages, images and other media into the any chat.

Here is example of a simple bot which reacts on the text message and `/echo` command:

```lisp
CL-USER> (defpackage the-bot (:use :cl :cl-telegram-bot))
#<Package "THE-BOT">
CL-USER> (in-package the-bot)
#<Package "THE-BOT">
THE-BOT> (defbot echo-bot)
MAKE-ECHO-BOT
THE-BOT> (defmethod on-message ((bot echo-bot)
                                text)
           (reply text))
#<STANDARD-METHOD ON-MESSAGE (ECHO-BOT T)>
THE-BOT> (defmethod on-command ((bot echo-bot)
                                (command (eql :help))
                                text)
           (declare (ignorable text))
           (reply "Just send me any text and I'll reply with the same text."))
#<STANDARD-METHOD ON-COMMAND (ECHO-BOT (EQL :HELP) T)>
THE-BOT> (defmethod on-command ((bot echo-bot)
                                (command (eql :start))
                                text)
           (declare (ignorable text))
           (reply "Welcome Lisper! Have a fun, playing with cl-telegram-bot!"))
#<STANDARD-METHOD ON-COMMAND (ECHO-BOT (EQL :START) T)>
```
Now, stop for the minute, open your Telegram client, and create a new bot
using the BotFather bot:

![](images/create-a-bot.png)

When you've got token, return to the `REPL` and start our bot:

```
THE-BOT> (start-processing (make-echo-bot "5205125**********************************")
                           :debug t)
 <INFO> [08:31:09] cl-telegram-bot core.lisp (start-processing) - Starting thread to process updates for CL-TELEGRAM-BOT/CORE::BOT: #<ECHO-BOT id=0> 
#<PROCESS telegram-bot(33) [Reset] #x30200709246D>
THE-BOT> 
```
This will start a new thread for processing incoming messages.

Now, find your bot in the Telegram client:

![](images/choose-the-bot.png)

And start communicating with him:

![](images/write-to-the-bot.png)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### API

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FBOT-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT/BOT

<a id="x-28-23A-28-2819-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FBOT-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot/bot`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FBOT-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FBOT-24BOT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### BOT

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ABOT-20CLASS-29"></a>

####### [class](dc41) `bot` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AAPI-URI-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

####### [reader](3e78) `api-uri` (bot) (:API-URI = "https://api.telegram.org/")

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ABOT-INFO-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

####### [reader](97d9) `bot-info` (bot) (= nil)

This slot will be filled with [`cl-telegram-bot/user:user`][81a4] object on first access using a call to [`cl-telegram-bot/user:get-me`][d037] function.

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ADEBUG-MODE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

####### [reader](66e4) `debug-mode` (bot) (:debug-mode = nil)

When debug mode is T, then interactive debugger will be called on each error.

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AFILE-ENDPOINT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

####### [reader](3642) `file-endpoint` (bot) (:file-endpoint = nil)

`HTTPS` file-endpoint

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AGET-ENDPOINT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

####### [reader](f161) `get-endpoint` (bot) (:endpoint)

`HTTPS` endpoint

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AGET-LAST-UPDATE-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

####### [reader](6bc1) `get-last-update-id` (bot) (= 0)

Update id

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ASENT-COMMANDS-CACHE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

####### [reader](fb4e) `sent-commands-cache` (bot) (= nil)

Command processing code will use this cache to update commands list on the server
when a new method for [`cl-telegram-bot/entities/command:on-command`][56c0] generic-function is defined.

This slot is for internal use.

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ATOKEN-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

####### [reader](a13e) `token` (bot) (:token = nil)

Bot token given by BotFather

**Accessors**

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AAPI-URI-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

####### [accessor](3e78) `api-uri` (bot) (:API-URI = "https://api.telegram.org/")

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ADEBUG-MODE-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

####### [accessor](66e4) `debug-mode` (bot) (:debug-mode = nil)

When debug mode is T, then interactive debugger will be called on each error.

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AFILE-ENDPOINT-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

####### [accessor](3642) `file-endpoint` (bot) (:file-endpoint = nil)

`HTTPS` file-endpoint

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AGET-LAST-UPDATE-ID-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

####### [accessor](6bc1) `get-last-update-id` (bot) (= 0)

Update id

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ASENT-COMMANDS-CACHE-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

####### [accessor](fb4e) `sent-commands-cache` (bot) (= nil)

Command processing code will use this cache to update commands list on the server
when a new method for [`cl-telegram-bot/entities/command:on-command`][56c0] generic-function is defined.

This slot is for internal use.

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ATOKEN-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

####### [accessor](a13e) `token` (bot) (:token = nil)

Bot token given by BotFather

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FBOT-3FMacros-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Macros

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ADEFBOT-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

###### [macro](574f) `defbot` name &optional slots options

Use this macro to define a class of your Telegram bot.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCALLBACK-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT/CALLBACK

<a id="x-28-23A-28-2824-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FCALLBACK-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot/callback`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCALLBACK-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCALLBACK-24CALLBACK-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### CALLBACK

<a id="x-28CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-20CLASS-29"></a>

####### [class](4cfa) `callback` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-DATA-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-29-29"></a>

####### [reader](4abb) `callback-data` (callback) (:data)

<a id="x-28CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-29-29"></a>

####### [reader](4edc) `callback-id` (callback) (:id)

<a id="x-28CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-MESSAGE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-29-29"></a>

####### [reader](7ba7) `callback-message` (callback) (:message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCALLBACK-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Generics

<a id="x-28CL-TELEGRAM-BOT-2FCALLBACK-3AMAKE-CALLBACK-20GENERIC-FUNCTION-29"></a>

###### [generic-function](fd99) `make-callback` bot callback-data

Called when user clicks callback button. Should return an instance of [`callback`][6611] class.

Application may override this method to return objects of different callback classes depending on
callback-data string. This way it mab be easier to define more specific methods for
[`on-callback`][1b93] generic-function.

<a id="x-28CL-TELEGRAM-BOT-2FCALLBACK-3AON-CALLBACK-20GENERIC-FUNCTION-29"></a>

###### [generic-function](e751) `on-callback` bot callback

Called when user clicks callback button. Second argument is an object of `CALLBACK` type.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCHAT-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT/CHAT

<a id="x-28-23A-28-2820-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FCHAT-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot/chat`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCHAT-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCHAT-24CHANNEL-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### CHANNEL

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ACHANNEL-20CLASS-29"></a>

####### [class](a46f) `channel` (base-group)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCHAT-24CHAT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### CHAT

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ACHAT-20CLASS-29"></a>

####### [class](1d5a) `chat` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CHAT-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ACHAT-29-29"></a>

####### [reader](5234) `get-chat-id` (chat) (:id)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-HAS-PROTECTED-CONTENT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ACHAT-29-29"></a>

####### [reader](b16c) `get-has-protected-content` (chat) (:has-protected-content)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-MESSAGE-AUTO-DELETE-TIME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ACHAT-29-29"></a>

####### [reader](ad07) `get-message-auto-delete-time` (chat) (:message-auto-delete-time)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-RAW-DATA-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ACHAT-29-29"></a>

####### [reader](efd4) `get-raw-data` (chat) (:raw-data)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-USERNAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ACHAT-29-29"></a>

####### [reader](8267) `get-username` (chat) (:username)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCHAT-24GROUP-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### GROUP

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGROUP-20CLASS-29"></a>

####### [class](94db) `group` (base-group)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCHAT-24PRIVATE-CHAT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### PRIVATE-CHAT

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3APRIVATE-CHAT-20CLASS-29"></a>

####### [class](84fe) `private-chat` (chat)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-BIO-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3APRIVATE-CHAT-29-29"></a>

####### [reader](84ed) `get-bio` (private-chat) (:bio)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-FIRST-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3APRIVATE-CHAT-29-29"></a>

####### [reader](afe6) `get-first-name` (private-chat) (:first-name)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-HAS-PRIVATE-FORWARDS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3APRIVATE-CHAT-29-29"></a>

####### [reader](1f9b) `get-has-private-forwards` (private-chat) (:has-private-forwards)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-LAST-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3APRIVATE-CHAT-29-29"></a>

####### [reader](5917) `get-last-name` (private-chat) (:last-name)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCHAT-24SUPER-GROUP-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### SUPER-GROUP

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ASUPER-GROUP-20CLASS-29"></a>

####### [class](7cfc) `super-group` (base-group)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CAN-SET-STICKER-SET-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ASUPER-GROUP-29-29"></a>

####### [reader](5b7c) `get-can-set-sticker-set` (super-group) (:can-set-sticker-set)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-JOIN-BY-REQUEST-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ASUPER-GROUP-29-29"></a>

####### [reader](7af4) `get-join-by-request` (super-group) (:join-by-request)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-JOIN-TO-SEND-MESSAGES-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ASUPER-GROUP-29-29"></a>

####### [reader](ba71) `get-join-to-send-messages` (super-group) (:join-to-send-messages)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-SLOW-MODE-DELAY-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ASUPER-GROUP-29-29"></a>

####### [reader](2c2d) `get-slow-mode-delay` (super-group) (:slow-mode-delay)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-STICKER-SET-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ASUPER-GROUP-29-29"></a>

####### [reader](9c98) `get-sticker-set-name` (super-group) (:sticker-set-name)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCHAT-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Generics

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CHAT-20GENERIC-FUNCTION-29"></a>

###### [generic-function](4763) `get-chat` obj

Returns a chat associated with object.

Object could be a message, update, callback, etc. Should return an object of [`chat`][b692] class or `NIL`.
Some types of updates aren't bound to a chat. In this case a method should return `NIL`.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCHAT-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ADELETE-CHAT-PHOTO-20FUNCTION-29"></a>

###### [function](75b5) `delete-chat-photo` bot-var1 chat

https://core.telegram.org/bots/api#deletechatphoto

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AEXPORT-CHAT-INVITE-LINK-20FUNCTION-29"></a>

###### [function](0ce5) `export-chat-invite-link` bot-var1 chat

https://core.telegram.org/bots/api#exportchatinvitelink

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CHAT-ADMINISTRATORS-20FUNCTION-29"></a>

###### [function](4366) `get-chat-administrators` bot-var1 chat

https://core.telegram.org/bots/api#getchatadministrators

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CHAT-BY-ID-20FUNCTION-29"></a>

###### [function](0aeb) `get-chat-by-id` bot-var1 chat-id

https://core.telegram.org/bots/api#getchat

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CHAT-MEMBER-20FUNCTION-29"></a>

###### [function](607f) `get-chat-member` bot-var1 chat user-id

https://core.telegram.org/bots/api#getchatmember

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CHAT-MEMBERS-COUNT-20FUNCTION-29"></a>

###### [function](1d14) `get-chat-members-count` bot-var1 chat

https://core.telegram.org/bots/api#getchatmemberscount

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AKICK-CHAT-MEMBER-20FUNCTION-29"></a>

###### [function](f367) `kick-chat-member` bot-var1 chat user-id until-date

https://core.telegram.org/bots/api#kickchatmember

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ALEAVE-CHAT-20FUNCTION-29"></a>

###### [function](b703) `leave-chat` bot-var1 chat

https://core.telegram.org/bots/api#leavechat

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3APIN-CHAT-MESSAGE-20FUNCTION-29"></a>

###### [function](ce92) `pin-chat-message` bot-var1 chat message-id disable-notification

https://core.telegram.org/bots/api#pinchatmessage

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3APROMOTE-CHAT-MEMBER-20FUNCTION-29"></a>

###### [function](5d10) `promote-chat-member` bot-var1 chat user-id can-change-info can-post-messages can-edit-messages can-delete-messages can-invite-users can-restrict-members can-pin-messages can-promote-members

https://core.telegram.org/bots/api#promotechatmember

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ARESTRICT-CHAT-MEMBER-20FUNCTION-29"></a>

###### [function](064c) `restrict-chat-member` bot-var1 chat user-id until-date can-send-messages can-send-media-messages can-send-other-messages can-add-web-page-previews

https://core.telegram.org/bots/api#restrictchatmember

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ASEND-CHAT-ACTION-20FUNCTION-29"></a>

###### [function](1152) `send-chat-action` bot-var1 chat action

https://core.telegram.org/bots/api#sendchataction

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ASET-CHAT-DESCRIPTION-20FUNCTION-29"></a>

###### [function](85dd) `set-chat-description` bot-var1 chat description

https://core.telegram.org/bots/api#setchatdescription

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ASET-CHAT-PHOTO-20FUNCTION-29"></a>

###### [function](4131) `set-chat-photo` bot-var1 chat photo

https://core.telegram.org/bots/api#setchatphoto

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ASET-CHAT-TITLE-20FUNCTION-29"></a>

###### [function](a214) `set-chat-title` bot-var1 chat title

https://core.telegram.org/bots/api#setchattitle

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AUNBAN-CHAT-MEMBER-20FUNCTION-29"></a>

###### [function](a34f) `unban-chat-member` bot-var1 chat user-id

https://core.telegram.org/bots/api#unbanchatmember

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AUNPIN-CHAT-MESSAGE-20FUNCTION-29"></a>

###### [function](b272) `unpin-chat-message` bot-var1 chat

https://core.telegram.org/bots/api#unpinchatmessage

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCORE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT/CORE

<a id="x-28-23A-28-2820-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FCORE-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot/core`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCORE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24REPLY-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### REPLY

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AREPLY-20CLASS-29"></a>

####### [class](40ce) `reply` (response-with-text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCORE-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Generics

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3AON-COMMAND-20GENERIC-FUNCTION-29"></a>

###### [generic-function](38ba) `on-command` bot command rest-text

This method will be called for each command.
First argument is a keyword. If user input was /save_note, then
first argument will be :save-note.

By default, logs call and does nothing.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AON-MESSAGE-20GENERIC-FUNCTION-29"></a>

###### [generic-function](5903) `on-message` bot text

This method gets called with raw text from the message.
By default it does nothing.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCORE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AREPLY-20FUNCTION-29"></a>

###### [function](f2f6) `reply` text &rest args &key parse-mode disable-web-page-preview disable-notification reply-to-message-id reply-markup (immediately \*reply-immediately\*)

Works like a [`send-message`][38a1], but only when an incoming message is processed.
Automatically sends reply to a chat from where current message came from.

<a id="x-28CL-TELEGRAM-BOT-2FCORE-3ASTART-PROCESSING-20FUNCTION-29"></a>

###### [function](9f56) `start-processing` BOT &KEY DEBUG (DELAY-BETWEEN-RETRIES 10) (THREAD-NAME "telegram-bot")

<a id="x-28CL-TELEGRAM-BOT-2FCORE-3ASTOP-PROCESSING-20FUNCTION-29"></a>

###### [function](ebca) `stop-processing` bot

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCORE-3FMacros-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Macros

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ADEFBOT-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

###### [macro](574f) `defbot` name &optional slots options

Use this macro to define a class of your Telegram bot.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT/ENTITIES/COMMAND

<a id="x-28-23A-28-2832-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot/entities/command`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-24BOT-COMMAND-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### BOT-COMMAND

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3ABOT-COMMAND-20CLASS-29"></a>

####### [class](5eee) `bot-command` (entity)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3ABOT-USERNAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3ABOT-COMMAND-29-29"></a>

####### [reader](7989) `bot-username` (bot-command) (:bot-username)

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3AGET-COMMAND-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3ABOT-COMMAND-29-29"></a>

####### [reader](3634) `get-command` (bot-command) (:command)

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3AGET-REST-TEXT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3ABOT-COMMAND-29-29"></a>

####### [reader](705c) `get-rest-text` (bot-command) (:rest-text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Generics

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3AON-COMMAND-20GENERIC-FUNCTION-29"></a>

###### [generic-function](38ba) `on-command` bot command rest-text

This method will be called for each command.
First argument is a keyword. If user input was /save_note, then
first argument will be :save-note.

By default, logs call and does nothing.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FENTITIES-2FGENERIC-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT/ENTITIES/GENERIC

<a id="x-28-23A-28-2832-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FENTITIES-2FGENERIC-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot/entities/generic`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FENTITIES-2FGENERIC-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Generics

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FGENERIC-3AMAKE-ENTITY-INTERNAL-20GENERIC-FUNCTION-29"></a>

###### [generic-function](3e08) `make-entity-internal` entity-type payload data

Extendable protocol to support entities of different kinds.
First argument is a keyword, denoting a type of the entity.
Payload is an object of type `message'.
And data is a plist with data, describing the entity.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FENTITIES-2FGENERIC-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FGENERIC-3AMAKE-ENTITY-20FUNCTION-29"></a>

###### [function](879b) `make-entity` payload data

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FENVELOPE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT/ENVELOPE

<a id="x-28-23A-28-2824-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FENVELOPE-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot/envelope`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FENVELOPE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FENVELOPE-24CHANNEL-POST-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### CHANNEL-POST

<a id="x-28CL-TELEGRAM-BOT-2FENVELOPE-3ACHANNEL-POST-20CLASS-29"></a>

####### [class](2adb) `channel-post` (envelope)

This container wraps [`cl-telegram-bot/message:message`][7239] when somebody sends a message to a channel.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FENVELOPE-24EDITED-CHANNEL-POST-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### EDITED-CHANNEL-POST

<a id="x-28CL-TELEGRAM-BOT-2FENVELOPE-3AEDITED-CHANNEL-POST-20CLASS-29"></a>

####### [class](d6c6) `edited-channel-post` (envelope)

This container wraps [`cl-telegram-bot/message:message`][7239] when somebody edits a message in a channel.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FENVELOPE-24EDITED-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### EDITED-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FENVELOPE-3AEDITED-MESSAGE-20CLASS-29"></a>

####### [class](815c) `edited-message` (envelope)

This container wraps [`cl-telegram-bot/message:message`][7239] when user edits a message.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FENVELOPE-24ENVELOPE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### ENVELOPE

<a id="x-28CL-TELEGRAM-BOT-2FENVELOPE-3AENVELOPE-20CLASS-29"></a>

####### [class](3aca) `envelope` ()

This is the container for a message. From the type of container we can understand if this message was sent to a channel or maybe edited, etc.

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FENVELOPE-3AWRAPPED-MESSAGE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FENVELOPE-3AENVELOPE-29-29"></a>

####### [reader](ad2e) `wrapped-message` (envelope) (:message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FENVELOPE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT-2FENVELOPE-3ACHANNEL-POST-P-20FUNCTION-29"></a>

###### [function](2bd8) `channel-post-p`

Returns T if current message was posted to a channel.

<a id="x-28CL-TELEGRAM-BOT-2FENVELOPE-3AEDITED-MESSAGE-P-20FUNCTION-29"></a>

###### [function](72ec) `edited-message-p`

Returns T if current message is an update for existing message in the channel of group chat.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT/INLINE-KEYBOARD

<a id="x-28-23A-28-2831-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot/inline-keyboard`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-24CALLBACK-BUTTON-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### CALLBACK-BUTTON

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3ACALLBACK-BUTTON-20CLASS-29"></a>

####### [class](0c11) `callback-button` (inline-keyboard-button)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3ACALLBACK-BUTTON-DATA-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3ACALLBACK-BUTTON-29-29"></a>

####### [reader](0eb7) `callback-button-data` (callback-button) (:data)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-24INLINE-KEYBOARD-BUTTON-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### INLINE-KEYBOARD-BUTTON

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AINLINE-KEYBOARD-BUTTON-20CLASS-29"></a>

####### [class](39d2) `inline-keyboard-button` ()

Base class for all inline keyboard buttons.

`API`: https://core.telegram.org/bots/api#inlinekeyboardbutton

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3ABUTTON-TEXT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AINLINE-KEYBOARD-BUTTON-29-29"></a>

####### [reader](8ed2) `button-text` (inline-keyboard-button) (:text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-24INLINE-KEYBOARD-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### INLINE-KEYBOARD

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AINLINE-KEYBOARD-20CLASS-29"></a>

####### [class](5389) `inline-keyboard` ()

Represents an inline keyboard as specified in `API` https://core.telegram.org/bots/api#inlinekeyboardmarkup.

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AKEYBOARD-ROWS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AINLINE-KEYBOARD-29-29"></a>

####### [reader](1bd8) `keyboard-rows` (inline-keyboard) (:rows = nil)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-24URL-BUTTON-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### URL-BUTTON

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AURL-BUTTON-20CLASS-29"></a>

####### [class](bc27) `url-button` (inline-keyboard-button)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3ABUTTON-URL-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AURL-BUTTON-29-29"></a>

####### [reader](bed6) `button-url` (url-button) (:data)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AANSWER-CALLBACK-QUERY-20FUNCTION-29"></a>

###### [function](e05a) `answer-callback-query` bot callback &key text show-alert url

https://core.telegram.org/bots/api#answercallbackquery

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3ACALLBACK-BUTTON-20FUNCTION-29"></a>

###### [function](67a7) `callback-button` text data

Creates a button which will call a callback.

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AINLINE-KEYBOARD-20FUNCTION-29"></a>

###### [function](6eb6) `inline-keyboard` rows

Returns an inline keyboard which can be passed
to `cl-telegram-bot/response:reply` ([`1`][0d9a] [`2`][9ce6]) as `REPLY-MARKUP` argument.

Each row should be a list of [`inline-keyboard-button`][cc87] objects or a single
object of this class. In latter case, such row will have only one button.

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AURL-BUTTON-20FUNCTION-29"></a>

###### [function](9be5) `url-button` text url

Creates a button which will open an url.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMARKUP-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT/MARKUP

<a id="x-28-23A-28-2822-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FMARKUP-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot/markup`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FMARKUP-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Generics

<a id="x-28CL-TELEGRAM-BOT-2FMARKUP-3ATO-MARKUP-20GENERIC-FUNCTION-29"></a>

###### [generic-function](5b59) `to-markup` obj

Transforms object into markup of Telegram `API`.

Methods of this class should return a hash-table, representing `OBJ`
in terms of Telegram `API`.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT/MESSAGE

<a id="x-28-23A-28-2823-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FMESSAGE-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot/message`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FMESSAGE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24ANIMATION-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### ANIMATION-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AANIMATION-MESSAGE-20CLASS-29"></a>

####### [class](edf4) `animation-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24ANIMATION-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### ANIMATION

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AANIMATION-20CLASS-29"></a>

####### [class](7d83) `animation` (file temporal spatial)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24AUDIO-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### AUDIO-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AAUDIO-MESSAGE-20CLASS-29"></a>

####### [class](6466) `audio-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24AUDIO-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### AUDIO

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AAUDIO-20CLASS-29"></a>

####### [class](37d0) `audio` (file temporal)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-PERFORMER-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AAUDIO-29-29"></a>

####### [reader](e185) `get-performer` (audio) (:performer)

Performer of the audio as defined by sender or by audio tags.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-TITLE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AAUDIO-29-29"></a>

####### [reader](ce13) `get-title` (audio) (:title)

Title of the audio as defined by sender or by audio tags.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24DOCUMENT-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### DOCUMENT-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ADOCUMENT-MESSAGE-20CLASS-29"></a>

####### [class](7424) `document-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24DOCUMENT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### DOCUMENT

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ADOCUMENT-20CLASS-29"></a>

####### [class](9c987) `document` (file)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24FILE-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### FILE-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-MESSAGE-20CLASS-29"></a>

####### [class](e0b8) `file-message` (message)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FILE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-MESSAGE-29-29"></a>

####### [reader](53ee) `get-file` (file-message) (:file)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24FILE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### FILE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-20CLASS-29"></a>

####### [class](9c65) `file` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FILE-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-29-29"></a>

####### [reader](0e78) `get-file-id` (file) (:file-id)

Identifier for this file, which can be used to download or reuse the file.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FILE-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-29-29"></a>

####### [reader](b191) `get-file-name` (file) (:file-name)

Original filename as defined by sender.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FILE-SIZE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-29-29"></a>

####### [reader](6c07) `get-file-size` (file) (:file-size)

File size in bytes.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FILE-UNIQUE-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-29-29"></a>

####### [reader](1fb2) `get-file-unique-id` (file) (:file-unique-id)

Unique identifier for this file, which is supposed to be the same
over time and for different bots. Can't be used to download or reuse
the file.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-MIME-TYPE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-29-29"></a>

####### [reader](1114) `get-mime-type` (file) (:mime-type)

`MIME` type of the file as defined by sender.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-20CLASS-29"></a>

####### [class](3e81) `message` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-CAPTION-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

####### [reader](4b1f) `get-caption` (message) (:caption)

Caption for the animation, audio, document, photo, video or voice.

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CHAT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

####### [reader](830e) `get-chat` (message) (:chat)

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-ENTITIES-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

####### [reader](19b7) `get-entities` (message) (:entities = nil)

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FORWARD-FROM-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

####### [reader](8413) `get-forward-from` (message) (:forward-from)

For forwarded messages, sender of the original message.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FORWARD-FROM-CHAT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

####### [reader](98d9) `get-forward-from-chat` (message) (:forward-from-chat)

For messages forwarded from channels or from anonymous
administrators, information about the original sender chat.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FORWARD-SENDER-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

####### [reader](c910) `get-forward-sender-name` (message) (:forward-sender-name)

For forwarded messages, sender of the original message.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-MESSAGE-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

####### [reader](eb11) `get-message-id` (message) (:id)

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-RAW-DATA-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

####### [reader](006c) `get-raw-data` (message) (:raw-data)

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-SENDER-CHAT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

####### [reader](5174) `get-sender-chat` (message) (:sender-chat)

Sender of the message, sent on behalf of a chat. For example, the channel itself for channel posts, the supergroup itself for messages from anonymous group administrators, the linked channel for messages automatically forwarded to the discussion group.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-TEXT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

####### [reader](0cfb) `get-text` (message) (:text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24PHOTO-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### PHOTO-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3APHOTO-MESSAGE-20CLASS-29"></a>

####### [class](68c2) `photo-message` (file-message)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-PHOTO-OPTIONS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3APHOTO-MESSAGE-29-29"></a>

####### [reader](e84a) `get-photo-options` (photo-message) (:photo-options)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24PHOTO-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### PHOTO

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3APHOTO-20CLASS-29"></a>

####### [class](d726) `photo` (file spatial)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24REPLY-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### REPLY

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AREPLY-20CLASS-29"></a>

####### [class](b34c) `reply` (message)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-REPLY-TO-MESSAGE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AREPLY-29-29"></a>

####### [reader](7385) `get-reply-to-message` (reply) (:reply-to-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24SPATIAL-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### SPATIAL

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASPATIAL-20CLASS-29"></a>

####### [class](2bee) `spatial` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-HEIGHT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ASPATIAL-29-29"></a>

####### [reader](7538) `get-height` (spatial) (:height)

File height as defined by sender.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-WIDTH-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ASPATIAL-29-29"></a>

####### [reader](c790) `get-width` (spatial) (:width)

File width as defined by sender.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24STICKER-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### STICKER-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASTICKER-MESSAGE-20CLASS-29"></a>

####### [class](d33b) `sticker-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24STICKER-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### STICKER

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASTICKER-20CLASS-29"></a>

####### [class](cbb6) `sticker` (file spatial)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-EMOJI-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ASTICKER-29-29"></a>

####### [reader](d888) `get-emoji` (sticker) (:emoji)

Emoji associated with the sticker

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-IS-ANIMATED-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ASTICKER-29-29"></a>

####### [reader](a1e8) `get-is-animated` (sticker) (:is-animated)

True if the sticker is animated.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-IS-VIDEO-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ASTICKER-29-29"></a>

####### [reader](9c28) `get-is-video` (sticker) (:is-video)

True if the sticker is a video sticker.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-SET-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ASTICKER-29-29"></a>

####### [reader](53b6) `get-set-name` (sticker) (:set-name)

Name of the sticker set to which the sticker belongs.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24TEMPORAL-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### TEMPORAL

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ATEMPORAL-20CLASS-29"></a>

####### [class](d062) `temporal` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-DURATION-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ATEMPORAL-29-29"></a>

####### [reader](4343) `get-duration` (temporal) (:duration)

Duration of the file in seconds as defined by sender.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24UNISPATIAL-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### UNISPATIAL

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AUNISPATIAL-20CLASS-29"></a>

####### [class](01d3) `unispatial` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-LENGTH-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AUNISPATIAL-29-29"></a>

####### [reader](b047) `get-length` (unispatial) (:length)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24VIDEO-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### VIDEO-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AVIDEO-MESSAGE-20CLASS-29"></a>

####### [class](f11e) `video-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24VIDEO-NOTE-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### VIDEO-NOTE-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AVIDEO-NOTE-MESSAGE-20CLASS-29"></a>

####### [class](46e3) `video-note-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24VIDEO-NOTE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### VIDEO-NOTE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AVIDEO-NOTE-20CLASS-29"></a>

####### [class](13d6) `video-note` (file temporal unispatial)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24VIDEO-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### VIDEO

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AVIDEO-20CLASS-29"></a>

####### [class](b9dd) `video` (file temporal spatial)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24VOICE-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### VOICE-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AVOICE-MESSAGE-20CLASS-29"></a>

####### [class](23cd) `voice-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24VOICE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### VOICE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AVOICE-20CLASS-29"></a>

####### [class](e9da) `voice` (file temporal)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FMESSAGE-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Generics

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AON-MESSAGE-20GENERIC-FUNCTION-29"></a>

###### [generic-function](5903) `on-message` bot text

This method gets called with raw text from the message.
By default it does nothing.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-ANIMATION-20GENERIC-FUNCTION-29"></a>

###### [generic-function](e210) `send-animation` bot chat animation &rest options &key caption parse-mode caption-entities duration width height thumb disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

Sends animation to a chat.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-AUDIO-20GENERIC-FUNCTION-29"></a>

###### [generic-function](69cf) `send-audio` bot chat audio &rest options &key caption parse-mode caption-entities duration performer title thumb disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-DOCUMENT-20GENERIC-FUNCTION-29"></a>

###### [generic-function](128d) `send-document` bot chat document &rest options &key caption parse-mode caption-entities disable-content-type-detection thumb disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-PHOTO-20GENERIC-FUNCTION-29"></a>

###### [generic-function](7220) `send-photo` bot chat photo &rest options &key caption parse-mode caption-entities disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-STICKER-20GENERIC-FUNCTION-29"></a>

###### [generic-function](30cb) `send-sticker` bot chat sticker &rest options &key disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

A function to send sticker.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-VIDEO-20GENERIC-FUNCTION-29"></a>

###### [generic-function](af2d) `send-video` bot chat video &rest options &key caption parse-mode caption-entities duration width height thumb disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-VIDEO-NOTE-20GENERIC-FUNCTION-29"></a>

###### [generic-function](faab) `send-video-note` bot chat video-note &rest options &key caption parse-mode caption-entities duration length thumb disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-VOICE-20GENERIC-FUNCTION-29"></a>

###### [generic-function](db31) `send-voice` bot chat voice &rest options &key caption parse-mode caption-entities duration disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FMESSAGE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ADELETE-MESSAGE-20FUNCTION-29"></a>

###### [function](0567) `delete-message` bot chat message

https://core.telegram.org/bots/api#deletemessage

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AFORWARD-MESSAGE-20FUNCTION-29"></a>

###### [function](4982) `forward-message` bot chat from-chat message &key disable-notification

https://core.telegram.org/bots/api#forwardmessage

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-CURRENT-BOT-20FUNCTION-29"></a>

###### [function](f4d1) `get-current-bot`

Returns a bot to which message was addressed.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-CURRENT-CHAT-20FUNCTION-29"></a>

###### [function](ae23) `get-current-chat`

Returns a chat where currently processing message was received.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-CURRENT-MESSAGE-20FUNCTION-29"></a>

###### [function](7e28) `get-current-message`

Returns currently processed message.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AMAKE-MESSAGE-20FUNCTION-29"></a>

###### [function](57a5) `make-message` data

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-MESSAGE-20FUNCTION-29"></a>

###### [function](2dd8) `send-message` bot chat text &rest options &key parse-mode disable-web-page-preview disable-notification reply-to-message-id (autosplit nil) reply-markup

https://core.telegram.org/bots/api#sendmessage

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FNETWORK-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT/NETWORK

<a id="x-28-23A-28-2823-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FNETWORK-22-29-20PACKAGE-29"></a>

##### [package](d3bb) `cl-telegram-bot/network`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FNETWORK-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FNETWORK-24REQUEST-ERROR-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### REQUEST-ERROR

<a id="x-28CL-TELEGRAM-BOT-2FNETWORK-3AREQUEST-ERROR-20CONDITION-29"></a>

####### [condition](e72b) `request-error` (error)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FNETWORK-3AWHAT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FNETWORK-3AREQUEST-ERROR-29-29"></a>

####### [reader](e72b) `what` (request-error) (:what)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FNETWORK-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT-2FNETWORK-3AMAKE-REQUEST-20FUNCTION-29"></a>

###### [function](0348) `make-request` bot name &rest options &key (streamp nil) (timeout 3) &allow-other-keys

Perform `HTTP` request to 'name `API` method with 'options `JSON`-encoded object.

<a id="x-28CL-TELEGRAM-BOT-2FNETWORK-3ASET-PROXY-20FUNCTION-29"></a>

###### [function](9ee3) `set-proxy` proxy

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FPAYMENTS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT/PAYMENTS

<a id="x-28-23A-28-2824-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FPAYMENTS-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot/payments`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FPAYMENTS-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Generics

<a id="x-28CL-TELEGRAM-BOT-2FPAYMENTS-3AON-PRE-CHECKOUT-QUERY-20GENERIC-FUNCTION-29"></a>

###### [generic-function](33f4) `on-pre-checkout-query` bot query

Called when user enters payment method credentials and hit "Pay" button. Second argument is an object of `PRE-CHECKOUT-QUERY` type.

A method should respond with with a call to [`answer-pre-checkout-query`][2afa] function.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FPAYMENTS-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT-2FPAYMENTS-3AANSWER-PRE-CHECKOUT-QUERY-20FUNCTION-29"></a>

###### [function](03cb) `answer-pre-checkout-query` bot pre-checkout-query &key error-message

If `ERROR-MESSAGE` argument was given, then response considered is not `OK` and transaction will be cancelled.

https://core.telegram.org/bots/api#answerprecheckoutquery

<a id="x-28CL-TELEGRAM-BOT-2FPAYMENTS-3AANSWER-SHIPPING-QUERY-20FUNCTION-29"></a>

###### [function](d223) `answer-shipping-query` b shipping-query-id ok &key shipping-options error-message

https://core.telegram.org/bots/api#answershippingquery

<a id="x-28CL-TELEGRAM-BOT-2FPAYMENTS-3ASEND-INVOICE-20FUNCTION-29"></a>

###### [function](2d4e) `send-invoice` b chat-id title description payload provider-token start-parameter currency prices &key photo-url photo-size photo-width photo-height need-name need-phone-number need-email need-shipping-address is-flexible disable-notification reply-to-message-id reply-markup

https://core.telegram.org/bots/api#sendinvoice

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FPIPELINE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT/PIPELINE

<a id="x-28-23A-28-2824-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FPIPELINE-22-29-20PACKAGE-29"></a>

##### [package](e9d2) `cl-telegram-bot/pipeline`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FPIPELINE-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Generics

<a id="x-28CL-TELEGRAM-BOT-2FPIPELINE-3APROCESS-20GENERIC-FUNCTION-29"></a>

###### [generic-function](e933) `process` bot object

This method is called by when processing a single update.
It is called multiple times on different parts of an update.
Whole pipeline looks like that:

For each update we call:
  process(update)
  process(update.payload)
  For each entity in payload:
    process(entity)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT/RESPONSE

<a id="x-28-23A-28-2824-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FRESPONSE-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot/response`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FRESPONSE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24ALERT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### ALERT

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AALERT-20CLASS-29"></a>

####### [class](6b60) `alert` (response-with-text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24NOTIFY-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### NOTIFY

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3ANOTIFY-20CLASS-29"></a>

####### [class](1ce5) `notify` (response-with-text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24OPEN-URL-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### OPEN-URL

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AOPEN-URL-20CLASS-29"></a>

####### [class](7783) `open-url` (response)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AURL-TO-OPEN-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FRESPONSE-3AOPEN-URL-29-29"></a>

####### [reader](3b87) `url-to-open` (open-url) (:text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24REPLY-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### REPLY

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AREPLY-20CLASS-29"></a>

####### [class](40ce) `reply` (response-with-text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24RESPONSE-WITH-TEXT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### RESPONSE-WITH-TEXT

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3ARESPONSE-WITH-TEXT-20CLASS-29"></a>

####### [class](bd6a) `response-with-text` (response)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3ARESPONSE-TEXT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FRESPONSE-3ARESPONSE-WITH-TEXT-29-29"></a>

####### [reader](2c38) `response-text` (response-with-text) (:text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24RESPONSE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### RESPONSE

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3ARESPONSE-20CLASS-29"></a>

####### [class](026e) `response` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AREST-ARGS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FRESPONSE-3ARESPONSE-29-29"></a>

####### [reader](4c2a) `rest-args` (response) (:args)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FRESPONSE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AALERT-20FUNCTION-29"></a>

###### [function](2449) `alert` text

Works like a [`send-message`][38a1], but only when an incoming message is processed.
Automatically sends reply to a chat from where current message came from.

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3ANOTIFY-20FUNCTION-29"></a>

###### [function](0155) `notify` text

Works like a [`send-message`][38a1], but only when an incoming message is processed.
Automatically sends reply to a chat from where current message came from.

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AOPEN-URL-20FUNCTION-29"></a>

###### [function](216f) `open-url` url

Works like a [`send-message`][38a1], but only when an incoming message is processed.
Automatically sends reply to a chat from where current message came from.

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AREPLY-20FUNCTION-29"></a>

###### [function](f2f6) `reply` text &rest args &key parse-mode disable-web-page-preview disable-notification reply-to-message-id reply-markup (immediately \*reply-immediately\*)

Works like a [`send-message`][38a1], but only when an incoming message is processed.
Automatically sends reply to a chat from where current message came from.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT/RESPONSE-PROCESSING

<a id="x-28-23A-28-2835-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot/response-processing`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-24INTERRUPT-PROCESSING-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### INTERRUPT-PROCESSING

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3AINTERRUPT-PROCESSING-20CONDITION-29"></a>

####### [condition](e321) `interrupt-processing` ()

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Generics

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3APROCESS-RESPONSE-20GENERIC-FUNCTION-29"></a>

###### [generic-function](bba1) `process-response` bot message response

Processes immediate responses of different types.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3AINTERRUPT-PROCESSING-20FUNCTION-29"></a>

###### [function](05f5) `interrupt-processing`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FUPDATE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT/UPDATE

<a id="x-28-23A-28-2822-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FUPDATE-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot/update`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FUPDATE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FUPDATE-24UPDATE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### UPDATE

<a id="x-28CL-TELEGRAM-BOT-2FUPDATE-3AUPDATE-20CLASS-29"></a>

####### [class](0b32) `update` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FUPDATE-3AGET-PAYLOAD-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUPDATE-3AUPDATE-29-29"></a>

####### [reader](0f30) `get-payload` (update) (:payload)

<a id="x-28CL-TELEGRAM-BOT-2FUPDATE-3AGET-RAW-DATA-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUPDATE-3AUPDATE-29-29"></a>

####### [reader](c293) `get-raw-data` (update) (:raw-data)

<a id="x-28CL-TELEGRAM-BOT-2FUPDATE-3AGET-UPDATE-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUPDATE-3AUPDATE-29-29"></a>

####### [reader](fbcb) `get-update-id` (update) (:id)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FUPDATE-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Generics

<a id="x-28CL-TELEGRAM-BOT-2FUPDATE-3APROCESS-UPDATES-20GENERIC-FUNCTION-29"></a>

###### [generic-function](86cd) `process-updates` bot

By default, this method starts an infinite loop and fetching new updates using long polling.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FUPDATE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT-2FUPDATE-3AMAKE-UPDATE-20FUNCTION-29"></a>

###### [function](06f9) `make-update` data

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FUSER-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT/USER

<a id="x-28-23A-28-2820-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FUSER-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot/user`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FUSER-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FUSER-24USER-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

###### USER

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3AUSER-20CLASS-29"></a>

####### [class](1ac6) `user` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3ABOT-P-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

####### [reader](12d1) `bot-p` (user) (:is-bot)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3ACAN-CONNECT-TO-BUSINESS-P-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

####### [reader](b6b1) `can-connect-to-business-p` (user) (:can-connect-to-business = nil)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3ACAN-JOIN-GROUPS-P-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

####### [reader](504f) `can-join-groups-p` (user) (:can-join-groups = nil)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3ACAN-READ-ALL-GROUP-MESSAGES-P-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

####### [reader](dbfb) `can-read-all-group-messages-p` (user) (:can-read-all-group-messages = nil)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3AFIRST-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

####### [reader](85b8) `first-name` (user) (:first-name)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3AIS-PREMIUM-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

####### [reader](7875) `is-premium` (user) (:is-premium = nil)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3ALANGUAGE-CODE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

####### [reader](ffd3) `language-code` (user) (:language-code = nil)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3ALAST-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

####### [reader](8c12) `last-name` (user) (:last-name = nil)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3ARAW-DATA-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

####### [reader](53ce) `raw-data` (user) (:raw-data)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3ASUPPORTS-INLINE-QUERIES-P-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

####### [reader](dcbb) `supports-inline-queries-p` (user) (:supports-inline-queries = nil)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3AUSER-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

####### [reader](a788) `user-id` (user) (:id)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3AUSERNAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

####### [reader](5c66) `username` (user) (:username = nil)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FUSER-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Generics

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3AGET-USER-INFO-20GENERIC-FUNCTION-29"></a>

###### [generic-function](5755) `get-user-info` obj

Returns a [`user`][81a4] object related to the object.

If object is not bound to a user, then `NIL` should be returned.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FUSER-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3AGET-ME-20FUNCTION-29"></a>

###### [function](6202) `get-me` bot

https://core.telegram.org/bots/api#getme

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FUTILS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### CL-TELEGRAM-BOT/UTILS

<a id="x-28-23A-28-2821-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FUTILS-22-29-20PACKAGE-29"></a>

##### [package] `cl-telegram-bot/utils`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FUTILS-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-28CL-TELEGRAM-BOT-2FUTILS-3AAPI-RESPONSE-TO-PLIST-20FUNCTION-29"></a>

###### [function](1379) `api-response-to-plist` plist

Transforms a plist with keys like :|foo_bar| into a plist with keys like :foo-bar.

This can be useful to pass data into `CL` object contructors.

<a id="x-28CL-TELEGRAM-BOT-2FUTILS-3AMAKE-KEYWORD-20FUNCTION-29"></a>

###### [function](dffd) `make-keyword` text

<a id="x-28CL-TELEGRAM-BOT-2FUTILS-3AOBFUSCATE-20FUNCTION-29"></a>

###### [function](693c) `obfuscate` url

<a id="x-28CL-TELEGRAM-BOT-2FUTILS-3ASPLIT-BY-LINES-20FUNCTION-29"></a>

###### [function](5ba6) `split-by-lines` text &key (max-size 4096) (trim-whitespaces-p t)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CREDITS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Credits

* [Rei][b588] – initial version.
* [Alexander Artemenko][891d] – large refactoring, usage of `CLOS` classes, etc.


[6949]: https://40ants.com/cl-telegram-bot/
[6611]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-20CLASS-29
[1b93]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT-2FCALLBACK-3AON-CALLBACK-20GENERIC-FUNCTION-29
[b692]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT-2FCHAT-3ACHAT-20CLASS-29
[56c0]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3AON-COMMAND-20GENERIC-FUNCTION-29
[cc87]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AINLINE-KEYBOARD-BUTTON-20CLASS-29
[7239]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-20CLASS-29
[38a1]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-MESSAGE-20FUNCTION-29
[2afa]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT-2FPAYMENTS-3AANSWER-PRE-CHECKOUT-QUERY-20FUNCTION-29
[9ce6]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT-2FRESPONSE-3AREPLY-20CLASS-29
[0d9a]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT-2FRESPONSE-3AREPLY-20FUNCTION-29
[d037]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT-2FUSER-3AGET-ME-20FUNCTION-29
[81a4]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT-2FUSER-3AUSER-20CLASS-29
[8e99]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT-DOCS-2FSTATES-3A-3A-40STATES-AND-ACTIONS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[46f2]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT2-2FACTIONS-2FEDIT-MESSAGE-MEDIA-3AEDIT-MESSAGE-MEDIA-20FUNCTION-29
[d23c]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-INVOICE-3ASEND-INVOICE-20FUNCTION-29
[e0f8]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-PHOTO-3ASEND-PHOTO-20CLASS-29
[7c91]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-PHOTO-3ASEND-PHOTO-20FUNCTION-29
[c8e7]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-TEXT-3ASEND-TEXT-20CLASS-29
[5d6f]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT2-2FACTIONS-2FSEND-TEXT-3ASEND-TEXT-20FUNCTION-29
[9647]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT2-2FGENERICS-3APROCESS-20GENERIC-FUNCTION-29
[60a4]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT2-2FHIGH-3AREPLY-20FUNCTION-29
[03e8]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT2-2FSTATE-3ASTATE-20FUNCTION-29
[53d1]: https://github.com/40ants/cl-telegram-bot
[7bb5]: https://github.com/40ants/cl-telegram-bot/actions
[dc41]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/bot.lisp#L19
[6bc1]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/bot.lisp#L20
[a13e]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/bot.lisp#L24
[3e78]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/bot.lisp#L29
[f161]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/bot.lisp#L33
[3642]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/bot.lisp#L37
[97d9]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/bot.lisp#L42
[66e4]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/bot.lisp#L45
[fb4e]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/bot.lisp#L50
[574f]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/bot.lisp#L58
[4cfa]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/callback.lisp#L26
[4edc]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/callback.lisp#L27
[4abb]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/callback.lisp#L30
[7ba7]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/callback.lisp#L33
[e751]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/callback.lisp#L38
[fd99]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/callback.lisp#L45
[94db]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L112
[7cfc]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L116
[ba71]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L117
[7af4]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L119
[2c2d]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L121
[9c98]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L123
[5b7c]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L125
[a46f]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L128
[0aeb]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L158
[f367]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L164
[a34f]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L168
[064c]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L172
[5d10]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L182
[0ce5]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L195
[4131]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L199
[75b5]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L203
[a214]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L207
[85dd]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L211
[ce92]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L215
[b272]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L219
[b703]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L223
[4366]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L227
[1d14]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L231
[607f]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L235
[1152]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L239
[4763]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L244
[1d5a]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L55
[5234]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L56
[8267]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L58
[b16c]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L60
[ad07]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L62
[efd4]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L64
[84fe]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L84
[afe6]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L85
[5917]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L87
[84ed]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L89
[1f9b]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/chat.lisp#L91
[9f56]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/core.lisp#L37
[ebca]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/core.lisp#L72
[5eee]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/entities/command.lisp#L41
[3634]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/entities/command.lisp#L42
[7989]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/entities/command.lisp#L45
[705c]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/entities/command.lisp#L48
[38ba]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/entities/command.lisp#L78
[3e08]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/entities/generic.lisp#L12
[879b]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/entities/generic.lisp#L19
[3aca]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/envelope.lisp#L19
[ad2e]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/envelope.lisp#L20
[815c]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/envelope.lisp#L25
[2adb]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/envelope.lisp#L30
[d6c6]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/envelope.lisp#L35
[2bd8]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/envelope.lisp#L51
[72ec]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/envelope.lisp#L61
[5389]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/inline-keyboard.lisp#L23
[1bd8]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/inline-keyboard.lisp#L24
[39d2]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/inline-keyboard.lisp#L36
[8ed2]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/inline-keyboard.lisp#L37
[0c11]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/inline-keyboard.lisp#L50
[0eb7]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/inline-keyboard.lisp#L51
[bc27]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/inline-keyboard.lisp#L56
[bed6]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/inline-keyboard.lisp#L57
[6eb6]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/inline-keyboard.lisp#L62
[67a7]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/inline-keyboard.lisp#L72
[9be5]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/inline-keyboard.lisp#L77
[e05a]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/inline-keyboard.lisp#L83
[5b59]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/markup.lisp#L7
[3e81]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L103
[eb11]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L104
[0cfb]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L106
[4b1f]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L110
[830e]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L114
[19b7]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L117
[006c]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L121
[5174]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L123
[8413]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L127
[c910]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L131
[98d9]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L135
[d062]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L159
[4343]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L160
[2bee]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L166
[7538]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L167
[c790]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L172
[01d3]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L178
[b047]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L179
[9c65]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L184
[0e78]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L185
[1fb2]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L190
[b191]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L197
[6c07]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L202
[1114]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L207
[d726]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L213
[37d0]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L215
[e185]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L216
[ce13]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L221
[7d83]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L227
[9c987]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L229
[b9dd]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L231
[13d6]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L233
[e9da]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L235
[cbb6]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L238
[a1e8]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L239
[9c28]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L243
[d888]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L247
[53b6]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L251
[e0b8]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L268
[53ee]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L269
[6466]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L281
[7424]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L283
[edf4]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L285
[68c2]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L287
[e84a]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L288
[d33b]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L298
[f11e]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L300
[46e3]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L302
[23cd]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L304
[b34c]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L307
[7385]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L308
[57a5]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L318
[2dd8]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L347
[7220]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L379
[69cf]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L439
[128d]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L482
[af2d]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L525
[e210]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L568
[faab]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L591
[db31]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L634
[30cb]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L677
[4982]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L735
[0567]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L779
[5903]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L786
[f4d1]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L822
[7e28]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L829
[ae23]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/message.lisp#L836
[d3bb]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/network.lisp#L1
[9ee3]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/network.lisp#L20
[e72b]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/network.lisp#L23
[0348]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/network.lisp#L30
[d223]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/payments.lisp#L113
[03cb]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/payments.lisp#L129
[33f4]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/payments.lisp#L197
[2d4e]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/payments.lisp#L73
[e9d2]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/pipeline.lisp#L1
[e933]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/pipeline.lisp#L8
[bba1]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/response-processing.lisp#L12
[05f5]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/response-processing.lisp#L16
[e321]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/response-processing.lisp#L8
[0155]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/response.lisp#L104
[2449]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/response.lisp#L118
[216f]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/response.lisp#L132
[026e]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/response.lisp#L34
[4c2a]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/response.lisp#L35
[bd6a]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/response.lisp#L40
[2c38]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/response.lisp#L41
[40ce]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/response.lisp#L45
[1ce5]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/response.lisp#L49
[6b60]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/response.lisp#L53
[7783]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/response.lisp#L57
[3b87]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/response.lisp#L58
[f2f6]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/response.lisp#L67
[86cd]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/update.lisp#L112
[0b32]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/update.lisp#L39
[fbcb]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/update.lisp#L40
[0f30]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/update.lisp#L42
[c293]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/update.lisp#L44
[06f9]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/update.lisp#L48
[5755]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/user.lisp#L108
[1ac6]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/user.lisp#L30
[a788]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/user.lisp#L31
[5c66]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/user.lisp#L34
[85b8]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/user.lisp#L38
[8c12]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/user.lisp#L41
[ffd3]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/user.lisp#L45
[7875]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/user.lisp#L49
[12d1]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/user.lisp#L53
[b6b1]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/user.lisp#L56
[dcbb]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/user.lisp#L60
[dbfb]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/user.lisp#L64
[504f]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/user.lisp#L68
[53ce]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/user.lisp#L72
[6202]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/user.lisp#L94
[dffd]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/utils.lisp#L26
[693c]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/utils.lisp#L33
[1379]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/utils.lisp#L50
[5ba6]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/src/utils.lisp#L65
[935e]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/action.lisp#L13
[f8a1]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/action.lisp#L8
[62c9]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/edit-message-media.lisp#L31
[63dc]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/edit-message-media.lisp#L32
[51de]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/edit-message-media.lisp#L37
[8a73]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/edit-message-media.lisp#L40
[cb16]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/edit-message-media.lisp#L52
[b31b]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/send-invoice.lisp#L31
[9343]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/send-invoice.lisp#L32
[841b]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/send-invoice.lisp#L36
[2bc5]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/send-invoice.lisp#L40
[6ffa]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/send-invoice.lisp#L44
[f37b]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/send-invoice.lisp#L48
[ffe1]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/send-invoice.lisp#L52
[e70b]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/send-invoice.lisp#L56
[5d6d]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/send-invoice.lisp#L61
[44fc]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/send-invoice.lisp#L80
[db28]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/send-photo.lisp#L28
[23c1]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/send-photo.lisp#L29
[5a24]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/send-photo.lisp#L34
[79e9]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/send-photo.lisp#L37
[6b64]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/send-photo.lisp#L48
[e756]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/send-text.lisp#L23
[caff]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/send-text.lisp#L24
[ba2b]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/send-text.lisp#L28
[aebc]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/send-text.lisp#L34
[8d82]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/actions/send-text.lisp#L50
[7b29]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/bot.lisp#L66
[f6f1]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/errors.lisp#L10
[4dd8]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/generics.lisp#L15
[a4ae]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/generics.lisp#L44
[214b]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/generics.lisp#L60
[6b39]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/generics.lisp#L79
[5431]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/generics.lisp#L93
[ee98]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/high.lisp#L25
[2905]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/high.lisp#L32
[89a5]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/high.lisp#L62
[18d9]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/high.lisp#L74
[e4e2]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/spec.lisp#L147
[0b6d]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/state-with-commands.lisp#L100
[9fe8]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/state-with-commands.lisp#L107
[cfdf]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/state-with-commands.lisp#L108
[0685]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/state-with-commands.lisp#L76
[7d88]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/state-with-commands.lisp#L81
[fea3]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/state-with-commands.lisp#L90
[ece1]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/state.lisp#L41
[cfb7]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/state.lisp#L42
[115c]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/state.lisp#L46
[8c0d]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/state.lisp#L50
[0aec]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/state.lisp#L54
[4978]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/state.lisp#L58
[de12]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/state.lisp#L95
[6d13]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/states/base.lisp#L28
[e4b3]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/states/base.lisp#L29
[4482]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/states/base.lisp#L33
[029c]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/states/base.lisp#L35
[4b91]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/states/base.lisp#L56
[15c2]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/states/base.lisp#L61
[bcc3]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/states/base.lisp#L72
[72c2]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/states/wait-for-payment.lisp#L40
[d979]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/states/wait-for-payment.lisp#L41
[b8dc]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/states/wait-for-payment.lisp#L47
[4744]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/term/back.lisp#L21
[3e45]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/term/back.lisp#L22
[a4fc]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/term/back.lisp#L30
[d94a]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/term/back.lisp#L35
[4f6c]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/term/back.lisp#L36
[52e8]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/term/back.lisp#L44
[76a3]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/term/back.lisp#L50
[ff05]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/term/back.lisp#L51
[417b]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/term/back.lisp#L60
[ea4e]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/term/back.lisp#L66
[2f47]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/term/back.lisp#L67
[ad5d]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/term/back.lisp#L76
[3d05]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/utils.lisp#L22
[c7dd]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/utils.lisp#L37
[83c1]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/utils.lisp#L46
[2353]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/utils.lisp#L54
[677d]: https://github.com/40ants/cl-telegram-bot/blob/054cc96ccfcd280727e977e2bfabdeff41caab5b/v2/utils.lisp#L65
[5798]: https://github.com/40ants/cl-telegram-bot/issues
[b588]: https://github.com/sovietspaceship
[891d]: https://github.com/svetlyak40wt
[8236]: https://quickdocs.org/alexandria
[c9ae]: https://quickdocs.org/anaphora
[b590]: https://quickdocs.org/arrows
[3dbf]: https://quickdocs.org/bordeaux-threads
[49b9]: https://quickdocs.org/cl-ppcre
[2ecb]: https://quickdocs.org/cl-strings
[61a4]: https://quickdocs.org/closer-mop
[8347]: https://quickdocs.org/dexador
[6dd8]: https://quickdocs.org/jonathan
[5186]: https://quickdocs.org/kebab
[7f8b]: https://quickdocs.org/log4cl
[c41d]: https://quickdocs.org/serapeum
[ef7f]: https://quickdocs.org/str
[fc0e]: https://quickdocs.org/trivial-backtrace
[aba2]: https://quickdocs.org/yason

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
