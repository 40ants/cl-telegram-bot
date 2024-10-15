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
<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40QUICKSTART-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Quickstart

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

## API

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FBOT-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/BOT

<a id="x-28-23A-28-2819-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FBOT-22-29-20PACKAGE-29"></a>

#### [package](0fe1) `cl-telegram-bot/bot`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FBOT-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FBOT-24BOT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### BOT

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ABOT-20CLASS-29"></a>

###### [class](704a) `bot` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AAPI-URI-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [reader](5a30) `api-uri` (bot) (:API-URI = "https://api.telegram.org/")

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ABOT-INFO-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [reader](6901) `bot-info` (bot) (= nil)

This slot will be filled with [`cl-telegram-bot/user:user`][81a4] object on first access using a call to [`cl-telegram-bot/user:get-me`][d037] function.

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ADEBUG-MODE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [reader](53db) `debug-mode` (bot) (:debug-mode = nil)

When debug mode is T, then interactive debugger will be called on each error.

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AFILE-ENDPOINT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [reader](0e47) `file-endpoint` (bot) (:file-endpoint = nil)

`HTTPS` file-endpoint

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AGET-ENDPOINT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [reader](d7ce) `get-endpoint` (bot) (:endpoint)

`HTTPS` endpoint

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AGET-LAST-UPDATE-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [reader](acdc) `get-last-update-id` (bot) (= 0)

Update id

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ASENT-COMMANDS-CACHE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [reader](b294) `sent-commands-cache` (bot) (= nil)

Command processing code will use this cache to update commands list on the server
when a new method for [`cl-telegram-bot/entities/command:on-command`][56c0] generic-function is defined.

This slot is for internal use.

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ATOKEN-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [reader](fc5c) `token` (bot) (:token = nil)

Bot token given by BotFather

**Accessors**

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AAPI-URI-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [accessor](5a30) `api-uri` (bot) (:API-URI = "https://api.telegram.org/")

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ADEBUG-MODE-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [accessor](53db) `debug-mode` (bot) (:debug-mode = nil)

When debug mode is T, then interactive debugger will be called on each error.

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AFILE-ENDPOINT-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [accessor](0e47) `file-endpoint` (bot) (:file-endpoint = nil)

`HTTPS` file-endpoint

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AGET-LAST-UPDATE-ID-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [accessor](acdc) `get-last-update-id` (bot) (= 0)

Update id

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ASENT-COMMANDS-CACHE-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [accessor](b294) `sent-commands-cache` (bot) (= nil)

Command processing code will use this cache to update commands list on the server
when a new method for [`cl-telegram-bot/entities/command:on-command`][56c0] generic-function is defined.

This slot is for internal use.

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ATOKEN-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [accessor](fc5c) `token` (bot) (:token = nil)

Bot token given by BotFather

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FBOT-3FMacros-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Macros

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ADEFBOT-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

##### [macro](11f3) `defbot` name &optional slots options

Use this macro to define a class of your Telegram bot.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCALLBACK-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/CALLBACK

<a id="x-28-23A-28-2824-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FCALLBACK-22-29-20PACKAGE-29"></a>

#### [package](947a) `cl-telegram-bot/callback`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCALLBACK-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCALLBACK-24CALLBACK-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### CALLBACK

<a id="x-28CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-20CLASS-29"></a>

###### [class](f5ef) `callback` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-DATA-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-29-29"></a>

###### [reader](797f) `callback-data` (callback) (:data)

<a id="x-28CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-29-29"></a>

###### [reader](5d90) `callback-id` (callback) (:id)

<a id="x-28CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-MESSAGE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-29-29"></a>

###### [reader](23f8) `callback-message` (callback) (:message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCALLBACK-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FCALLBACK-3AMAKE-CALLBACK-20GENERIC-FUNCTION-29"></a>

##### [generic-function](021a) `make-callback` bot callback-data

Called when user clicks callback button. Should return an instance of [`callback`][6611] class.

Application may override this method to return objects of different callback classes depending on
callback-data string. This way it mab be easier to define more specific methods for
[`on-callback`][1b93] generic-function.

<a id="x-28CL-TELEGRAM-BOT-2FCALLBACK-3AON-CALLBACK-20GENERIC-FUNCTION-29"></a>

##### [generic-function](000e) `on-callback` bot callback

Called when user clicks callback button. Second argument is an object of `CALLBACK` type.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCHAT-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/CHAT

<a id="x-28-23A-28-2820-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FCHAT-22-29-20PACKAGE-29"></a>

#### [package](b047) `cl-telegram-bot/chat`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCHAT-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCHAT-24CHANNEL-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### CHANNEL

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ACHANNEL-20CLASS-29"></a>

###### [class](4d89) `channel` (base-group)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCHAT-24CHAT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### CHAT

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ACHAT-20CLASS-29"></a>

###### [class](fe5b) `chat` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CHAT-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ACHAT-29-29"></a>

###### [reader](e97e) `get-chat-id` (chat) (:id)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-HAS-PROTECTED-CONTENT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ACHAT-29-29"></a>

###### [reader](3735) `get-has-protected-content` (chat) (:has-protected-content)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-MESSAGE-AUTO-DELETE-TIME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ACHAT-29-29"></a>

###### [reader](0a15) `get-message-auto-delete-time` (chat) (:message-auto-delete-time)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-RAW-DATA-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ACHAT-29-29"></a>

###### [reader](be63) `get-raw-data` (chat) (:raw-data)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-USERNAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ACHAT-29-29"></a>

###### [reader](1bf0) `get-username` (chat) (:username)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCHAT-24GROUP-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### GROUP

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGROUP-20CLASS-29"></a>

###### [class](f6dc) `group` (base-group)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCHAT-24PRIVATE-CHAT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### PRIVATE-CHAT

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3APRIVATE-CHAT-20CLASS-29"></a>

###### [class](88d4) `private-chat` (chat)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-BIO-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3APRIVATE-CHAT-29-29"></a>

###### [reader](beb4) `get-bio` (private-chat) (:bio)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-FIRST-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3APRIVATE-CHAT-29-29"></a>

###### [reader](1e4d) `get-first-name` (private-chat) (:first-name)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-HAS-PRIVATE-FORWARDS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3APRIVATE-CHAT-29-29"></a>

###### [reader](112d) `get-has-private-forwards` (private-chat) (:has-private-forwards)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-LAST-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3APRIVATE-CHAT-29-29"></a>

###### [reader](8b0b) `get-last-name` (private-chat) (:last-name)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCHAT-24SUPER-GROUP-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### SUPER-GROUP

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ASUPER-GROUP-20CLASS-29"></a>

###### [class](db7d) `super-group` (base-group)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CAN-SET-STICKER-SET-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ASUPER-GROUP-29-29"></a>

###### [reader](7a7e) `get-can-set-sticker-set` (super-group) (:can-set-sticker-set)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-JOIN-BY-REQUEST-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ASUPER-GROUP-29-29"></a>

###### [reader](f029) `get-join-by-request` (super-group) (:join-by-request)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-JOIN-TO-SEND-MESSAGES-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ASUPER-GROUP-29-29"></a>

###### [reader](764f) `get-join-to-send-messages` (super-group) (:join-to-send-messages)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-SLOW-MODE-DELAY-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ASUPER-GROUP-29-29"></a>

###### [reader](bc80) `get-slow-mode-delay` (super-group) (:slow-mode-delay)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-STICKER-SET-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ASUPER-GROUP-29-29"></a>

###### [reader](a937) `get-sticker-set-name` (super-group) (:sticker-set-name)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCHAT-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CHAT-20GENERIC-FUNCTION-29"></a>

##### [generic-function](170f) `get-chat` obj

Returns a chat associated with object.

Object could be a message, update, callback, etc. Should return an object of [`chat`][b692] class or `NIL`.
Some types of updates aren't bound to a chat. In this case a method should return `NIL`.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCHAT-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ADELETE-CHAT-PHOTO-20FUNCTION-29"></a>

##### [function](a26c) `delete-chat-photo` bot-var1 chat

https://core.telegram.org/bots/api#deletechatphoto

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AEXPORT-CHAT-INVITE-LINK-20FUNCTION-29"></a>

##### [function](c69e) `export-chat-invite-link` bot-var1 chat

https://core.telegram.org/bots/api#exportchatinvitelink

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CHAT-ADMINISTRATORS-20FUNCTION-29"></a>

##### [function](af96) `get-chat-administrators` bot-var1 chat

https://core.telegram.org/bots/api#getchatadministrators

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CHAT-BY-ID-20FUNCTION-29"></a>

##### [function](b5d5) `get-chat-by-id` bot-var1 chat-id

https://core.telegram.org/bots/api#getchat

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CHAT-MEMBER-20FUNCTION-29"></a>

##### [function](b313) `get-chat-member` bot-var1 chat user-id

https://core.telegram.org/bots/api#getchatmember

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CHAT-MEMBERS-COUNT-20FUNCTION-29"></a>

##### [function](800f) `get-chat-members-count` bot-var1 chat

https://core.telegram.org/bots/api#getchatmemberscount

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AKICK-CHAT-MEMBER-20FUNCTION-29"></a>

##### [function](9bcd) `kick-chat-member` bot-var1 chat user-id until-date

https://core.telegram.org/bots/api#kickchatmember

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ALEAVE-CHAT-20FUNCTION-29"></a>

##### [function](0740) `leave-chat` bot-var1 chat

https://core.telegram.org/bots/api#leavechat

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3APIN-CHAT-MESSAGE-20FUNCTION-29"></a>

##### [function](5216) `pin-chat-message` bot-var1 chat message-id disable-notification

https://core.telegram.org/bots/api#pinchatmessage

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3APROMOTE-CHAT-MEMBER-20FUNCTION-29"></a>

##### [function](627e) `promote-chat-member` bot-var1 chat user-id can-change-info can-post-messages can-edit-messages can-delete-messages can-invite-users can-restrict-members can-pin-messages can-promote-members

https://core.telegram.org/bots/api#promotechatmember

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ARESTRICT-CHAT-MEMBER-20FUNCTION-29"></a>

##### [function](6ffb) `restrict-chat-member` bot-var1 chat user-id until-date can-send-messages can-send-media-messages can-send-other-messages can-add-web-page-previews

https://core.telegram.org/bots/api#restrictchatmember

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ASEND-CHAT-ACTION-20FUNCTION-29"></a>

##### [function](2e07) `send-chat-action` bot-var1 chat action

https://core.telegram.org/bots/api#sendchataction

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ASET-CHAT-DESCRIPTION-20FUNCTION-29"></a>

##### [function](7c80) `set-chat-description` bot-var1 chat description

https://core.telegram.org/bots/api#setchatdescription

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ASET-CHAT-PHOTO-20FUNCTION-29"></a>

##### [function](8df4) `set-chat-photo` bot-var1 chat photo

https://core.telegram.org/bots/api#setchatphoto

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ASET-CHAT-TITLE-20FUNCTION-29"></a>

##### [function](7d2b) `set-chat-title` bot-var1 chat title

https://core.telegram.org/bots/api#setchattitle

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AUNBAN-CHAT-MEMBER-20FUNCTION-29"></a>

##### [function](55ae) `unban-chat-member` bot-var1 chat user-id

https://core.telegram.org/bots/api#unbanchatmember

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AUNPIN-CHAT-MESSAGE-20FUNCTION-29"></a>

##### [function](526f) `unpin-chat-message` bot-var1 chat

https://core.telegram.org/bots/api#unpinchatmessage

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCORE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/CORE

<a id="x-28-23A-28-2820-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FCORE-22-29-20PACKAGE-29"></a>

#### [package](2137) `cl-telegram-bot/core`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCORE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24REPLY-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### REPLY

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AREPLY-20CLASS-29"></a>

###### [class](12ab) `reply` (response-with-text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCORE-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3AON-COMMAND-20GENERIC-FUNCTION-29"></a>

##### [generic-function](1e8e) `on-command` bot command rest-text

This method will be called for each command.
First argument is a keyword. If user input was /save_note, then
first argument will be :save-note.

By default, logs call and does nothing.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AON-MESSAGE-20GENERIC-FUNCTION-29"></a>

##### [generic-function](3d43) `on-message` bot text

This method gets called with raw text from the message.
By default it does nothing.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCORE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AREPLY-20FUNCTION-29"></a>

##### [function](ba7d) `reply` text &rest args &key parse-mode disable-web-page-preview disable-notification reply-to-message-id reply-markup (immediately \*reply-immediately\*)

Works like a [`send-message`][38a1], but only when an incoming message is processed.
Automatically sends reply to a chat from where current message came from.

<a id="x-28CL-TELEGRAM-BOT-2FCORE-3ASTART-PROCESSING-20FUNCTION-29"></a>

##### [function](cf21) `start-processing` BOT &KEY DEBUG (DELAY-BETWEEN-RETRIES 10) (THREAD-NAME "telegram-bot")

<a id="x-28CL-TELEGRAM-BOT-2FCORE-3ASTOP-PROCESSING-20FUNCTION-29"></a>

##### [function](149e) `stop-processing` bot

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCORE-3FMacros-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Macros

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ADEFBOT-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

##### [macro](11f3) `defbot` name &optional slots options

Use this macro to define a class of your Telegram bot.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/ENTITIES/COMMAND

<a id="x-28-23A-28-2832-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-22-29-20PACKAGE-29"></a>

#### [package](902d) `cl-telegram-bot/entities/command`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-24BOT-COMMAND-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### BOT-COMMAND

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3ABOT-COMMAND-20CLASS-29"></a>

###### [class](f953) `bot-command` (entity)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3ABOT-USERNAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3ABOT-COMMAND-29-29"></a>

###### [reader](13a0) `bot-username` (bot-command) (:bot-username)

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3AGET-COMMAND-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3ABOT-COMMAND-29-29"></a>

###### [reader](d0da) `get-command` (bot-command) (:command)

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3AGET-REST-TEXT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3ABOT-COMMAND-29-29"></a>

###### [reader](c27b) `get-rest-text` (bot-command) (:rest-text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3AON-COMMAND-20GENERIC-FUNCTION-29"></a>

##### [generic-function](1e8e) `on-command` bot command rest-text

This method will be called for each command.
First argument is a keyword. If user input was /save_note, then
first argument will be :save-note.

By default, logs call and does nothing.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FENTITIES-2FGENERIC-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/ENTITIES/GENERIC

<a id="x-28-23A-28-2832-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FENTITIES-2FGENERIC-22-29-20PACKAGE-29"></a>

#### [package](b42e) `cl-telegram-bot/entities/generic`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FENTITIES-2FGENERIC-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FGENERIC-3AMAKE-ENTITY-INTERNAL-20GENERIC-FUNCTION-29"></a>

##### [generic-function](6f1c) `make-entity-internal` entity-type payload data

Extendable protocol to support entities of different kinds.
First argument is a keyword, denoting a type of the entity.
Payload is an object of type `message'.
And data is a plist with data, describing the entity.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FENTITIES-2FGENERIC-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FGENERIC-3AMAKE-ENTITY-20FUNCTION-29"></a>

##### [function](430e) `make-entity` payload data

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FENVELOPE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/ENVELOPE

<a id="x-28-23A-28-2824-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FENVELOPE-22-29-20PACKAGE-29"></a>

#### [package](be2a) `cl-telegram-bot/envelope`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FENVELOPE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FENVELOPE-24CHANNEL-POST-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### CHANNEL-POST

<a id="x-28CL-TELEGRAM-BOT-2FENVELOPE-3ACHANNEL-POST-20CLASS-29"></a>

###### [class](6690) `channel-post` (envelope)

This container wraps [`cl-telegram-bot/message:message`][7239] when somebody sends a message to a channel.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FENVELOPE-24EDITED-CHANNEL-POST-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### EDITED-CHANNEL-POST

<a id="x-28CL-TELEGRAM-BOT-2FENVELOPE-3AEDITED-CHANNEL-POST-20CLASS-29"></a>

###### [class](327f) `edited-channel-post` (envelope)

This container wraps [`cl-telegram-bot/message:message`][7239] when somebody edits a message in a channel.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FENVELOPE-24EDITED-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### EDITED-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FENVELOPE-3AEDITED-MESSAGE-20CLASS-29"></a>

###### [class](13ce) `edited-message` (envelope)

This container wraps [`cl-telegram-bot/message:message`][7239] when user edits a message.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FENVELOPE-24ENVELOPE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### ENVELOPE

<a id="x-28CL-TELEGRAM-BOT-2FENVELOPE-3AENVELOPE-20CLASS-29"></a>

###### [class](3a27) `envelope` ()

This is the container for a message. From the type of container we can understand if this message was sent to a channel or maybe edited, etc.

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FENVELOPE-3AWRAPPED-MESSAGE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FENVELOPE-3AENVELOPE-29-29"></a>

###### [reader](2201) `wrapped-message` (envelope) (:message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FENVELOPE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FENVELOPE-3ACHANNEL-POST-P-20FUNCTION-29"></a>

##### [function](b680) `channel-post-p`

Returns T if current message was posted to a channel.

<a id="x-28CL-TELEGRAM-BOT-2FENVELOPE-3AEDITED-MESSAGE-P-20FUNCTION-29"></a>

##### [function](2dff) `edited-message-p`

Returns T if current message is an update for existing message in the channel of group chat.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/INLINE-KEYBOARD

<a id="x-28-23A-28-2831-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-22-29-20PACKAGE-29"></a>

#### [package](2141) `cl-telegram-bot/inline-keyboard`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-24CALLBACK-BUTTON-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### CALLBACK-BUTTON

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3ACALLBACK-BUTTON-20CLASS-29"></a>

###### [class](9739) `callback-button` (inline-keyboard-button)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3ACALLBACK-BUTTON-DATA-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3ACALLBACK-BUTTON-29-29"></a>

###### [reader](69e3) `callback-button-data` (callback-button) (:data)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-24INLINE-KEYBOARD-BUTTON-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### INLINE-KEYBOARD-BUTTON

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AINLINE-KEYBOARD-BUTTON-20CLASS-29"></a>

###### [class](89c5) `inline-keyboard-button` ()

Base class for all inline keyboard buttons.

`API`: https://core.telegram.org/bots/api#inlinekeyboardbutton

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3ABUTTON-TEXT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AINLINE-KEYBOARD-BUTTON-29-29"></a>

###### [reader](82a5) `button-text` (inline-keyboard-button) (:text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-24INLINE-KEYBOARD-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### INLINE-KEYBOARD

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AINLINE-KEYBOARD-20CLASS-29"></a>

###### [class](8736) `inline-keyboard` ()

Represents an inline keyboard as specified in `API` https://core.telegram.org/bots/api#inlinekeyboardmarkup.

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AKEYBOARD-ROWS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AINLINE-KEYBOARD-29-29"></a>

###### [reader](d77c) `keyboard-rows` (inline-keyboard) (:rows = nil)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-24URL-BUTTON-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### URL-BUTTON

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AURL-BUTTON-20CLASS-29"></a>

###### [class](f713) `url-button` (inline-keyboard-button)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3ABUTTON-URL-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AURL-BUTTON-29-29"></a>

###### [reader](3965) `button-url` (url-button) (:data)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AANSWER-CALLBACK-QUERY-20FUNCTION-29"></a>

##### [function](af1b) `answer-callback-query` bot callback &key text show-alert url

https://core.telegram.org/bots/api#answercallbackquery

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3ACALLBACK-BUTTON-20FUNCTION-29"></a>

##### [function](aeaf) `callback-button` text data

Creates a button which will call a callback.

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AINLINE-KEYBOARD-20FUNCTION-29"></a>

##### [function](7f74) `inline-keyboard` rows

Returns an inline keyboard which can be passed
to `cl-telegram-bot/response:reply` ([`1`][0d9a] [`2`][9ce6]) as `REPLY-MARKUP` argument.

Each row should be a list of [`inline-keyboard-button`][cc87] objects or a single
object of this class. In latter case, such row will have only one button.

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AURL-BUTTON-20FUNCTION-29"></a>

##### [function](ae2e) `url-button` text url

Creates a button which will open an url.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMARKUP-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/MARKUP

<a id="x-28-23A-28-2822-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FMARKUP-22-29-20PACKAGE-29"></a>

#### [package](6821) `cl-telegram-bot/markup`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FMARKUP-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FMARKUP-3ATO-MARKUP-20GENERIC-FUNCTION-29"></a>

##### [generic-function](1caa) `to-markup` obj

Transforms object into markup of Telegram `API`.

Methods of this class should return a hash-table, representing `OBJ`
in terms of Telegram `API`.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/MESSAGE

<a id="x-28-23A-28-2823-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FMESSAGE-22-29-20PACKAGE-29"></a>

#### [package](6855) `cl-telegram-bot/message`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FMESSAGE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24ANIMATION-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### ANIMATION-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AANIMATION-MESSAGE-20CLASS-29"></a>

###### [class](e075) `animation-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24ANIMATION-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### ANIMATION

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AANIMATION-20CLASS-29"></a>

###### [class](f71b) `animation` (file temporal spatial)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24AUDIO-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### AUDIO-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AAUDIO-MESSAGE-20CLASS-29"></a>

###### [class](7777) `audio-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24AUDIO-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### AUDIO

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AAUDIO-20CLASS-29"></a>

###### [class](d337) `audio` (file temporal)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-PERFORMER-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AAUDIO-29-29"></a>

###### [reader](dacc) `get-performer` (audio) (:performer)

Performer of the audio as defined by sender or by audio tags.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-TITLE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AAUDIO-29-29"></a>

###### [reader](729b) `get-title` (audio) (:title)

Title of the audio as defined by sender or by audio tags.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24DOCUMENT-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### DOCUMENT-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ADOCUMENT-MESSAGE-20CLASS-29"></a>

###### [class](fec7) `document-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24DOCUMENT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### DOCUMENT

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ADOCUMENT-20CLASS-29"></a>

###### [class](10aa) `document` (file)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24FILE-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### FILE-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-MESSAGE-20CLASS-29"></a>

###### [class](c51d) `file-message` (message)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FILE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-MESSAGE-29-29"></a>

###### [reader](73bb) `get-file` (file-message) (:file)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24FILE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### FILE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-20CLASS-29"></a>

###### [class](6148) `file` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FILE-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-29-29"></a>

###### [reader](04fe) `get-file-id` (file) (:file-id)

Identifier for this file, which can be used to download or reuse the file.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FILE-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-29-29"></a>

###### [reader](3f31) `get-file-name` (file) (:file-name)

Original filename as defined by sender.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FILE-SIZE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-29-29"></a>

###### [reader](d838) `get-file-size` (file) (:file-size)

File size in bytes.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FILE-UNIQUE-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-29-29"></a>

###### [reader](3600) `get-file-unique-id` (file) (:file-unique-id)

Unique identifier for this file, which is supposed to be the same
over time and for different bots. Can't be used to download or reuse
the file.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-MIME-TYPE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-29-29"></a>

###### [reader](fbe4) `get-mime-type` (file) (:mime-type)

`MIME` type of the file as defined by sender.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-20CLASS-29"></a>

###### [class](5095) `message` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-CAPTION-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

###### [reader](fcd7) `get-caption` (message) (:caption)

Caption for the animation, audio, document, photo, video or voice.

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CHAT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

###### [reader](599c) `get-chat` (message) (:chat)

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-ENTITIES-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

###### [reader](c394) `get-entities` (message) (:entities = nil)

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FORWARD-FROM-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

###### [reader](6ba3) `get-forward-from` (message) (:forward-from)

For forwarded messages, sender of the original message.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FORWARD-FROM-CHAT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

###### [reader](3acb) `get-forward-from-chat` (message) (:forward-from-chat)

For messages forwarded from channels or from anonymous
administrators, information about the original sender chat.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FORWARD-SENDER-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

###### [reader](1be3) `get-forward-sender-name` (message) (:forward-sender-name)

For forwarded messages, sender of the original message.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-MESSAGE-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

###### [reader](8244) `get-message-id` (message) (:id)

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-RAW-DATA-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

###### [reader](e9c5) `get-raw-data` (message) (:raw-data)

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-SENDER-CHAT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

###### [reader](6d9c) `get-sender-chat` (message) (:sender-chat)

Sender of the message, sent on behalf of a chat. For example, the channel itself for channel posts, the supergroup itself for messages from anonymous group administrators, the linked channel for messages automatically forwarded to the discussion group.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-TEXT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

###### [reader](0b88) `get-text` (message) (:text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24PHOTO-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### PHOTO-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3APHOTO-MESSAGE-20CLASS-29"></a>

###### [class](d402) `photo-message` (file-message)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-PHOTO-OPTIONS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3APHOTO-MESSAGE-29-29"></a>

###### [reader](7e31) `get-photo-options` (photo-message) (:photo-options)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24PHOTO-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### PHOTO

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3APHOTO-20CLASS-29"></a>

###### [class](640e) `photo` (file spatial)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24REPLY-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### REPLY

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AREPLY-20CLASS-29"></a>

###### [class](2652) `reply` (message)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-REPLY-TO-MESSAGE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AREPLY-29-29"></a>

###### [reader](011d) `get-reply-to-message` (reply) (:reply-to-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24SPATIAL-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### SPATIAL

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASPATIAL-20CLASS-29"></a>

###### [class](8520) `spatial` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-HEIGHT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ASPATIAL-29-29"></a>

###### [reader](2ce7) `get-height` (spatial) (:height)

File height as defined by sender.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-WIDTH-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ASPATIAL-29-29"></a>

###### [reader](61fa) `get-width` (spatial) (:width)

File width as defined by sender.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24STICKER-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### STICKER-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASTICKER-MESSAGE-20CLASS-29"></a>

###### [class](d8c8) `sticker-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24STICKER-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### STICKER

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASTICKER-20CLASS-29"></a>

###### [class](e5ae) `sticker` (file spatial)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-EMOJI-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ASTICKER-29-29"></a>

###### [reader](a603) `get-emoji` (sticker) (:emoji)

Emoji associated with the sticker

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-IS-ANIMATED-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ASTICKER-29-29"></a>

###### [reader](6e43) `get-is-animated` (sticker) (:is-animated)

True if the sticker is animated.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-IS-VIDEO-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ASTICKER-29-29"></a>

###### [reader](e339) `get-is-video` (sticker) (:is-video)

True if the sticker is a video sticker.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-SET-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ASTICKER-29-29"></a>

###### [reader](3413) `get-set-name` (sticker) (:set-name)

Name of the sticker set to which the sticker belongs.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24TEMPORAL-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### TEMPORAL

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ATEMPORAL-20CLASS-29"></a>

###### [class](54e8) `temporal` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-DURATION-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ATEMPORAL-29-29"></a>

###### [reader](2af3) `get-duration` (temporal) (:duration)

Duration of the file in seconds as defined by sender.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24UNISPATIAL-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### UNISPATIAL

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AUNISPATIAL-20CLASS-29"></a>

###### [class](237c) `unispatial` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-LENGTH-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AUNISPATIAL-29-29"></a>

###### [reader](185e) `get-length` (unispatial) (:length)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24VIDEO-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### VIDEO-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AVIDEO-MESSAGE-20CLASS-29"></a>

###### [class](e1ac) `video-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24VIDEO-NOTE-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### VIDEO-NOTE-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AVIDEO-NOTE-MESSAGE-20CLASS-29"></a>

###### [class](f76c) `video-note-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24VIDEO-NOTE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### VIDEO-NOTE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AVIDEO-NOTE-20CLASS-29"></a>

###### [class](c3c7) `video-note` (file temporal unispatial)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24VIDEO-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### VIDEO

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AVIDEO-20CLASS-29"></a>

###### [class](6d0d) `video` (file temporal spatial)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24VOICE-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### VOICE-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AVOICE-MESSAGE-20CLASS-29"></a>

###### [class](cd6b) `voice-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24VOICE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### VOICE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AVOICE-20CLASS-29"></a>

###### [class](bde9) `voice` (file temporal)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FMESSAGE-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AON-MESSAGE-20GENERIC-FUNCTION-29"></a>

##### [generic-function](3d43) `on-message` bot text

This method gets called with raw text from the message.
By default it does nothing.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-ANIMATION-20GENERIC-FUNCTION-29"></a>

##### [generic-function](6b61) `send-animation` bot chat animation &rest options &key caption parse-mode caption-entities duration width height thumb disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

Sends animation to a chat.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-AUDIO-20GENERIC-FUNCTION-29"></a>

##### [generic-function](dc57) `send-audio` bot chat audio &rest options &key caption parse-mode caption-entities duration performer title thumb disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-DOCUMENT-20GENERIC-FUNCTION-29"></a>

##### [generic-function](f523) `send-document` bot chat document &rest options &key caption parse-mode caption-entities disable-content-type-detection thumb disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-PHOTO-20GENERIC-FUNCTION-29"></a>

##### [generic-function](b6a1) `send-photo` bot chat photo &rest options &key caption parse-mode caption-entities disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-STICKER-20GENERIC-FUNCTION-29"></a>

##### [generic-function](bb2d) `send-sticker` bot chat sticker &rest options &key disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

A function to send sticker.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-VIDEO-20GENERIC-FUNCTION-29"></a>

##### [generic-function](f0d3) `send-video` bot chat video &rest options &key caption parse-mode caption-entities duration width height thumb disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-VIDEO-NOTE-20GENERIC-FUNCTION-29"></a>

##### [generic-function](6c65) `send-video-note` bot chat video-note &rest options &key caption parse-mode caption-entities duration length thumb disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-VOICE-20GENERIC-FUNCTION-29"></a>

##### [generic-function](5025) `send-voice` bot chat voice &rest options &key caption parse-mode caption-entities duration disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FMESSAGE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ADELETE-MESSAGE-20FUNCTION-29"></a>

##### [function](9384) `delete-message` bot chat message

https://core.telegram.org/bots/api#deletemessage

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AFORWARD-MESSAGE-20FUNCTION-29"></a>

##### [function](9051) `forward-message` bot chat from-chat message &key disable-notification

https://core.telegram.org/bots/api#forwardmessage

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-CURRENT-BOT-20FUNCTION-29"></a>

##### [function](d5e5) `get-current-bot`

Returns a bot to which message was addressed.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-CURRENT-CHAT-20FUNCTION-29"></a>

##### [function](3b2e) `get-current-chat`

Returns a chat where currently processing message was received.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-CURRENT-MESSAGE-20FUNCTION-29"></a>

##### [function](09bb) `get-current-message`

Returns currently processed message.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AMAKE-MESSAGE-20FUNCTION-29"></a>

##### [function](9c1e) `make-message` data

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-MESSAGE-20FUNCTION-29"></a>

##### [function](ccc8) `send-message` bot chat text &rest options &key parse-mode disable-web-page-preview disable-notification reply-to-message-id (autosplit nil) reply-markup

https://core.telegram.org/bots/api#sendmessage

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FNETWORK-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/NETWORK

<a id="x-28-23A-28-2823-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FNETWORK-22-29-20PACKAGE-29"></a>

#### [package](32e7) `cl-telegram-bot/network`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FNETWORK-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FNETWORK-24REQUEST-ERROR-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### REQUEST-ERROR

<a id="x-28CL-TELEGRAM-BOT-2FNETWORK-3AREQUEST-ERROR-20CONDITION-29"></a>

###### [condition](fb11) `request-error` (error)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FNETWORK-3AWHAT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FNETWORK-3AREQUEST-ERROR-29-29"></a>

###### [reader](fb11) `what` (request-error) (:what)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FNETWORK-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FNETWORK-3AMAKE-REQUEST-20FUNCTION-29"></a>

##### [function](b038) `make-request` bot name &rest options &key (streamp nil) (timeout 3) &allow-other-keys

Perform `HTTP` request to 'name `API` method with 'options `JSON`-encoded object.

<a id="x-28CL-TELEGRAM-BOT-2FNETWORK-3ASET-PROXY-20FUNCTION-29"></a>

##### [function](43da) `set-proxy` proxy

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FPAYMENTS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/PAYMENTS

<a id="x-28-23A-28-2824-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FPAYMENTS-22-29-20PACKAGE-29"></a>

#### [package](596c) `cl-telegram-bot/payments`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FPAYMENTS-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FPAYMENTS-3AON-PRE-CHECKOUT-QUERY-20GENERIC-FUNCTION-29"></a>

##### [generic-function](ed94) `on-pre-checkout-query` bot query

Called when user enters payment method credentials and hit "Pay" button. Second argument is an object of `PRE-CHECKOUT-QUERY` type.

A method should respond with with a call to [`answer-pre-checkout-query`][2afa] function.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FPAYMENTS-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FPAYMENTS-3AANSWER-PRE-CHECKOUT-QUERY-20FUNCTION-29"></a>

##### [function](d015) `answer-pre-checkout-query` bot pre-checkout-query &key error-message

If `ERROR-MESSAGE` argument was given, then response considered is not `OK` and transaction will be cancelled.

https://core.telegram.org/bots/api#answerprecheckoutquery

<a id="x-28CL-TELEGRAM-BOT-2FPAYMENTS-3AANSWER-SHIPPING-QUERY-20FUNCTION-29"></a>

##### [function](9fb0) `answer-shipping-query` b shipping-query-id ok &key shipping-options error-message

https://core.telegram.org/bots/api#answershippingquery

<a id="x-28CL-TELEGRAM-BOT-2FPAYMENTS-3ASEND-INVOICE-20FUNCTION-29"></a>

##### [function](58ae) `send-invoice` b chat-id title description payload provider-token start-parameter currency prices &key photo-url photo-size photo-width photo-height need-name need-phone-number need-email need-shipping-address is-flexible disable-notification reply-to-message-id reply-markup

https://core.telegram.org/bots/api#sendinvoice

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FPIPELINE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/PIPELINE

<a id="x-28-23A-28-2824-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FPIPELINE-22-29-20PACKAGE-29"></a>

#### [package](6c44) `cl-telegram-bot/pipeline`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FPIPELINE-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FPIPELINE-3APROCESS-20GENERIC-FUNCTION-29"></a>

##### [generic-function](b814) `process` bot object

This method is called by when processing a single update.
It is called multiple times on different parts of an update.
Whole pipeline looks like that:

For each update we call:
  process(update)
  process(update.payload)
  For each entity in payload:
    process(entity)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/RESPONSE

<a id="x-28-23A-28-2824-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FRESPONSE-22-29-20PACKAGE-29"></a>

#### [package](82ae) `cl-telegram-bot/response`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FRESPONSE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24ALERT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### ALERT

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AALERT-20CLASS-29"></a>

###### [class](c0c9) `alert` (response-with-text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24NOTIFY-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### NOTIFY

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3ANOTIFY-20CLASS-29"></a>

###### [class](fbd7) `notify` (response-with-text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24OPEN-URL-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### OPEN-URL

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AOPEN-URL-20CLASS-29"></a>

###### [class](5ccd) `open-url` (response)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AURL-TO-OPEN-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FRESPONSE-3AOPEN-URL-29-29"></a>

###### [reader](0468) `url-to-open` (open-url) (:text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24REPLY-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### REPLY

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AREPLY-20CLASS-29"></a>

###### [class](12ab) `reply` (response-with-text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24RESPONSE-WITH-TEXT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### RESPONSE-WITH-TEXT

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3ARESPONSE-WITH-TEXT-20CLASS-29"></a>

###### [class](dcc9) `response-with-text` (response)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3ARESPONSE-TEXT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FRESPONSE-3ARESPONSE-WITH-TEXT-29-29"></a>

###### [reader](5de3) `response-text` (response-with-text) (:text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24RESPONSE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### RESPONSE

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3ARESPONSE-20CLASS-29"></a>

###### [class](eab1) `response` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AREST-ARGS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FRESPONSE-3ARESPONSE-29-29"></a>

###### [reader](6d6b) `rest-args` (response) (:args)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FRESPONSE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AALERT-20FUNCTION-29"></a>

##### [function](ef21) `alert` text

Works like a [`send-message`][38a1], but only when an incoming message is processed.
Automatically sends reply to a chat from where current message came from.

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3ANOTIFY-20FUNCTION-29"></a>

##### [function](b854) `notify` text

Works like a [`send-message`][38a1], but only when an incoming message is processed.
Automatically sends reply to a chat from where current message came from.

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AOPEN-URL-20FUNCTION-29"></a>

##### [function](c3c3) `open-url` url

Works like a [`send-message`][38a1], but only when an incoming message is processed.
Automatically sends reply to a chat from where current message came from.

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AREPLY-20FUNCTION-29"></a>

##### [function](ba7d) `reply` text &rest args &key parse-mode disable-web-page-preview disable-notification reply-to-message-id reply-markup (immediately \*reply-immediately\*)

Works like a [`send-message`][38a1], but only when an incoming message is processed.
Automatically sends reply to a chat from where current message came from.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/RESPONSE-PROCESSING

<a id="x-28-23A-28-2835-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-22-29-20PACKAGE-29"></a>

#### [package](22af) `cl-telegram-bot/response-processing`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-24INTERRUPT-PROCESSING-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### INTERRUPT-PROCESSING

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3AINTERRUPT-PROCESSING-20CONDITION-29"></a>

###### [condition](5aaf) `interrupt-processing` ()

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3APROCESS-RESPONSE-20GENERIC-FUNCTION-29"></a>

##### [generic-function](fe93) `process-response` bot message response

Processes immediate responses of different types.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3AINTERRUPT-PROCESSING-20FUNCTION-29"></a>

##### [function](c7c4) `interrupt-processing`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FUPDATE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/UPDATE

<a id="x-28-23A-28-2822-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FUPDATE-22-29-20PACKAGE-29"></a>

#### [package](013d) `cl-telegram-bot/update`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FUPDATE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FUPDATE-24UPDATE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### UPDATE

<a id="x-28CL-TELEGRAM-BOT-2FUPDATE-3AUPDATE-20CLASS-29"></a>

###### [class](0fc7) `update` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FUPDATE-3AGET-PAYLOAD-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUPDATE-3AUPDATE-29-29"></a>

###### [reader](68bb) `get-payload` (update) (:payload)

<a id="x-28CL-TELEGRAM-BOT-2FUPDATE-3AGET-RAW-DATA-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUPDATE-3AUPDATE-29-29"></a>

###### [reader](428e) `get-raw-data` (update) (:raw-data)

<a id="x-28CL-TELEGRAM-BOT-2FUPDATE-3AGET-UPDATE-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUPDATE-3AUPDATE-29-29"></a>

###### [reader](0dcf) `get-update-id` (update) (:id)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FUPDATE-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FUPDATE-3APROCESS-UPDATES-20GENERIC-FUNCTION-29"></a>

##### [generic-function](9197) `process-updates` bot

By default, this method starts an infinite loop and fetching new updates using long polling.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FUPDATE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FUPDATE-3AMAKE-UPDATE-20FUNCTION-29"></a>

##### [function](eb2a) `make-update` data

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FUSER-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/USER

<a id="x-28-23A-28-2820-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FUSER-22-29-20PACKAGE-29"></a>

#### [package](9bac) `cl-telegram-bot/user`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FUSER-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FUSER-24USER-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### USER

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3AUSER-20CLASS-29"></a>

###### [class](fa2a) `user` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3ABOT-P-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

###### [reader](0e5f) `bot-p` (user) (:is-bot)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3ACAN-CONNECT-TO-BUSINESS-P-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

###### [reader](8cb3) `can-connect-to-business-p` (user) (:can-connect-to-business = nil)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3ACAN-JOIN-GROUPS-P-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

###### [reader](7c34) `can-join-groups-p` (user) (:can-join-groups = nil)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3ACAN-READ-ALL-GROUP-MESSAGES-P-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

###### [reader](0400) `can-read-all-group-messages-p` (user) (:can-read-all-group-messages = nil)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3AFIRST-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

###### [reader](29dd) `first-name` (user) (:first-name)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3AIS-PREMIUM-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

###### [reader](9468) `is-premium` (user) (:is-premium = nil)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3ALANGUAGE-CODE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

###### [reader](52b0) `language-code` (user) (:language-code = nil)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3ALAST-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

###### [reader](cc93) `last-name` (user) (:last-name = nil)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3ARAW-DATA-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

###### [reader](963d) `raw-data` (user) (:raw-data)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3ASUPPORTS-INLINE-QUERIES-P-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

###### [reader](b8fd) `supports-inline-queries-p` (user) (:supports-inline-queries = nil)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3AUSER-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

###### [reader](c175) `user-id` (user) (:id)

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3AUSERNAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUSER-3AUSER-29-29"></a>

###### [reader](b52e) `username` (user) (:username = nil)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FUSER-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3AGET-USER-INFO-20GENERIC-FUNCTION-29"></a>

##### [generic-function](9969) `get-user-info` obj

Returns a [`user`][81a4] object related to the object.

If object is not bound to a user, then `NIL` should be returned.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FUSER-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FUSER-3AGET-ME-20FUNCTION-29"></a>

##### [function](2a58) `get-me` bot

https://core.telegram.org/bots/api#getme

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FUTILS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/UTILS

<a id="x-28-23A-28-2821-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FUTILS-22-29-20PACKAGE-29"></a>

#### [package](1a16) `cl-telegram-bot/utils`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FUTILS-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FUTILS-3AAPI-RESPONSE-TO-PLIST-20FUNCTION-29"></a>

##### [function](40ef) `api-response-to-plist` plist

Transforms a plist with keys like :|foo_bar| into a plist with keys like :foo-bar.

This can be useful to pass data into `CL` object contructors.

<a id="x-28CL-TELEGRAM-BOT-2FUTILS-3AMAKE-KEYWORD-20FUNCTION-29"></a>

##### [function](07eb) `make-keyword` text

<a id="x-28CL-TELEGRAM-BOT-2FUTILS-3AOBFUSCATE-20FUNCTION-29"></a>

##### [function](f19f) `obfuscate` url

<a id="x-28CL-TELEGRAM-BOT-2FUTILS-3ASPLIT-BY-LINES-20FUNCTION-29"></a>

##### [function](21d7) `split-by-lines` text &key (max-size 4096) (trim-whitespaces-p t)

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
[53d1]: https://github.com/40ants/cl-telegram-bot
[7bb5]: https://github.com/40ants/cl-telegram-bot/actions
[0fe1]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/bot.lisp#L1
[704a]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/bot.lisp#L19
[acdc]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/bot.lisp#L20
[fc5c]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/bot.lisp#L24
[5a30]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/bot.lisp#L29
[d7ce]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/bot.lisp#L33
[0e47]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/bot.lisp#L37
[6901]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/bot.lisp#L42
[53db]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/bot.lisp#L45
[b294]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/bot.lisp#L50
[11f3]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/bot.lisp#L58
[947a]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/callback.lisp#L1
[f5ef]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/callback.lisp#L26
[5d90]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/callback.lisp#L27
[797f]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/callback.lisp#L30
[23f8]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/callback.lisp#L33
[000e]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/callback.lisp#L38
[021a]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/callback.lisp#L45
[b047]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L1
[f6dc]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L112
[db7d]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L116
[764f]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L117
[f029]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L119
[bc80]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L121
[a937]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L123
[7a7e]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L125
[4d89]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L128
[b5d5]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L158
[9bcd]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L164
[55ae]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L168
[6ffb]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L172
[627e]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L182
[c69e]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L195
[8df4]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L199
[a26c]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L203
[7d2b]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L207
[7c80]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L211
[5216]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L215
[526f]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L219
[0740]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L223
[af96]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L227
[800f]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L231
[b313]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L235
[2e07]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L239
[170f]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L244
[fe5b]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L55
[e97e]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L56
[1bf0]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L58
[3735]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L60
[0a15]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L62
[be63]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L64
[88d4]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L84
[1e4d]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L85
[8b0b]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L87
[beb4]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L89
[112d]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/chat.lisp#L91
[2137]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/core.lisp#L1
[cf21]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/core.lisp#L37
[149e]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/core.lisp#L72
[902d]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/entities/command.lisp#L1
[f953]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/entities/command.lisp#L41
[d0da]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/entities/command.lisp#L42
[13a0]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/entities/command.lisp#L45
[c27b]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/entities/command.lisp#L48
[1e8e]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/entities/command.lisp#L78
[b42e]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/entities/generic.lisp#L1
[6f1c]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/entities/generic.lisp#L12
[430e]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/entities/generic.lisp#L19
[be2a]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/envelope.lisp#L1
[3a27]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/envelope.lisp#L19
[2201]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/envelope.lisp#L20
[13ce]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/envelope.lisp#L25
[6690]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/envelope.lisp#L30
[327f]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/envelope.lisp#L35
[b680]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/envelope.lisp#L51
[2dff]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/envelope.lisp#L61
[2141]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/inline-keyboard.lisp#L1
[8736]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/inline-keyboard.lisp#L23
[d77c]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/inline-keyboard.lisp#L24
[89c5]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/inline-keyboard.lisp#L36
[82a5]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/inline-keyboard.lisp#L37
[9739]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/inline-keyboard.lisp#L50
[69e3]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/inline-keyboard.lisp#L51
[f713]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/inline-keyboard.lisp#L56
[3965]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/inline-keyboard.lisp#L57
[7f74]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/inline-keyboard.lisp#L62
[aeaf]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/inline-keyboard.lisp#L72
[ae2e]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/inline-keyboard.lisp#L77
[af1b]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/inline-keyboard.lisp#L83
[6821]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/markup.lisp#L1
[1caa]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/markup.lisp#L7
[6855]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L1
[5095]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L103
[8244]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L104
[0b88]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L106
[fcd7]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L110
[599c]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L114
[c394]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L117
[e9c5]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L121
[6d9c]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L123
[6ba3]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L127
[1be3]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L131
[3acb]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L135
[54e8]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L159
[2af3]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L160
[8520]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L166
[2ce7]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L167
[61fa]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L172
[237c]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L178
[185e]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L179
[6148]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L184
[04fe]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L185
[3600]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L190
[3f31]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L197
[d838]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L202
[fbe4]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L207
[640e]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L213
[d337]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L215
[dacc]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L216
[729b]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L221
[f71b]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L227
[10aa]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L229
[6d0d]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L231
[c3c7]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L233
[bde9]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L235
[e5ae]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L238
[6e43]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L239
[e339]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L243
[a603]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L247
[3413]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L251
[c51d]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L268
[73bb]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L269
[7777]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L281
[fec7]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L283
[e075]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L285
[d402]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L287
[7e31]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L288
[d8c8]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L298
[e1ac]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L300
[f76c]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L302
[cd6b]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L304
[2652]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L307
[011d]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L308
[9c1e]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L318
[ccc8]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L347
[b6a1]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L379
[dc57]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L439
[f523]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L482
[f0d3]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L525
[6b61]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L568
[6c65]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L591
[5025]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L634
[bb2d]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L677
[9051]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L735
[9384]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L779
[3d43]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L786
[d5e5]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L822
[09bb]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L829
[3b2e]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/message.lisp#L836
[32e7]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/network.lisp#L1
[43da]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/network.lisp#L20
[fb11]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/network.lisp#L23
[b038]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/network.lisp#L30
[596c]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/payments.lisp#L1
[9fb0]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/payments.lisp#L113
[d015]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/payments.lisp#L129
[ed94]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/payments.lisp#L197
[58ae]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/payments.lisp#L73
[6c44]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/pipeline.lisp#L1
[b814]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/pipeline.lisp#L8
[22af]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/response-processing.lisp#L1
[fe93]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/response-processing.lisp#L12
[c7c4]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/response-processing.lisp#L16
[5aaf]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/response-processing.lisp#L8
[82ae]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/response.lisp#L1
[b854]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/response.lisp#L104
[ef21]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/response.lisp#L118
[c3c3]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/response.lisp#L132
[eab1]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/response.lisp#L34
[6d6b]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/response.lisp#L35
[dcc9]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/response.lisp#L40
[5de3]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/response.lisp#L41
[12ab]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/response.lisp#L45
[fbd7]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/response.lisp#L49
[c0c9]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/response.lisp#L53
[5ccd]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/response.lisp#L57
[0468]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/response.lisp#L58
[ba7d]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/response.lisp#L67
[013d]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/update.lisp#L1
[9197]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/update.lisp#L112
[0fc7]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/update.lisp#L39
[0dcf]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/update.lisp#L40
[68bb]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/update.lisp#L42
[428e]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/update.lisp#L44
[eb2a]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/update.lisp#L48
[9bac]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/user.lisp#L1
[9969]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/user.lisp#L108
[fa2a]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/user.lisp#L30
[c175]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/user.lisp#L31
[b52e]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/user.lisp#L34
[29dd]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/user.lisp#L38
[cc93]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/user.lisp#L41
[52b0]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/user.lisp#L45
[9468]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/user.lisp#L49
[0e5f]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/user.lisp#L53
[8cb3]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/user.lisp#L56
[b8fd]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/user.lisp#L60
[0400]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/user.lisp#L64
[7c34]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/user.lisp#L68
[963d]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/user.lisp#L72
[2a58]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/user.lisp#L94
[1a16]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/utils.lisp#L1
[07eb]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/utils.lisp#L26
[f19f]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/utils.lisp#L33
[40ef]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/utils.lisp#L50
[21d7]: https://github.com/40ants/cl-telegram-bot/blob/92be4300098d1ebd71d68690b28aef4f3ca2c1ab/src/utils.lisp#L65
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
