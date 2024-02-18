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
* Depends on: [alexandria][8236], [arrows][b590], [bordeaux-threads][3dbf], [cl-ppcre][49b9], [cl-strings][2ecb], [closer-mop][61a4], [dexador][8347], [jonathan][6dd8], [kebab][5186], [log4cl][7f8b], [serapeum][c41d], [str][ef7f], [trivial-backtrace][fc0e]

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

#### [package](a0ce) `cl-telegram-bot/bot`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FBOT-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FBOT-24BOT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### BOT

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ABOT-20CLASS-29"></a>

###### [class](be8b) `bot` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AAPI-URI-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [reader](b072) `api-uri` (bot) (:API-URI = "https://api.telegram.org/")

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ADEBUG-MODE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [reader](da65) `debug-mode` (bot) (:debug-mode = nil)

When debug mode is T, then interactive debugger will be called on each error.

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AFILE-ENDPOINT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [reader](7244) `file-endpoint` (bot) (:file-endpoint = nil)

`HTTPS` file-endpoint

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AGET-ENDPOINT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [reader](d193) `get-endpoint` (bot) (:endpoint)

`HTTPS` endpoint

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AGET-LAST-UPDATE-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [reader](a9f4) `get-last-update-id` (bot) (= 0)

Update id

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ASENT-COMMANDS-CACHE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [reader](6bea) `sent-commands-cache` (bot) (= nil)

Command processing code will use this cache to update commands list on the server
when a new method for [`cl-telegram-bot/entities/command:on-command`][56c0] generic-function is defined.

This slot is for internal use.

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ATOKEN-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [reader](9396) `token` (bot) (:token = nil)

Bot token given by BotFather

**Accessors**

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AAPI-URI-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [accessor](b072) `api-uri` (bot) (:API-URI = "https://api.telegram.org/")

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ADEBUG-MODE-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [accessor](da65) `debug-mode` (bot) (:debug-mode = nil)

When debug mode is T, then interactive debugger will be called on each error.

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AFILE-ENDPOINT-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [accessor](7244) `file-endpoint` (bot) (:file-endpoint = nil)

`HTTPS` file-endpoint

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3AGET-LAST-UPDATE-ID-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [accessor](a9f4) `get-last-update-id` (bot) (= 0)

Update id

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ASENT-COMMANDS-CACHE-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [accessor](6bea) `sent-commands-cache` (bot) (= nil)

Command processing code will use this cache to update commands list on the server
when a new method for [`cl-telegram-bot/entities/command:on-command`][56c0] generic-function is defined.

This slot is for internal use.

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ATOKEN-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-TELEGRAM-BOT-2FBOT-3ABOT-29-29"></a>

###### [accessor](9396) `token` (bot) (:token = nil)

Bot token given by BotFather

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FBOT-3FMacros-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Macros

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ADEFBOT-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

##### [macro](4509) `defbot` name

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCALLBACK-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/CALLBACK

<a id="x-28-23A-28-2824-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FCALLBACK-22-29-20PACKAGE-29"></a>

#### [package](cd8b) `cl-telegram-bot/callback`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCALLBACK-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCALLBACK-24CALLBACK-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### CALLBACK

<a id="x-28CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-20CLASS-29"></a>

###### [class](4364) `callback` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-DATA-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-29-29"></a>

###### [reader](1490) `callback-data` (callback) (:data)

<a id="x-28CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-29-29"></a>

###### [reader](fd4a) `callback-id` (callback) (:id)

<a id="x-28CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-MESSAGE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-29-29"></a>

###### [reader](a11d) `callback-message` (callback) (:message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCALLBACK-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-CHAT-20GENERIC-FUNCTION-29"></a>

##### [generic-function](349a) `callback-chat` callback

Returns a chat from where callback was sent.

<a id="x-28CL-TELEGRAM-BOT-2FCALLBACK-3AMAKE-CALLBACK-20GENERIC-FUNCTION-29"></a>

##### [generic-function](5d19) `make-callback` bot callback-data

Called when user clicks callback button. Should return an instance of [`callback`][6611] class.

Application may override this method to return objects of different callback classes depending on
callback-data string. This way it mab be easier to define more specific methods for
[`on-callback`][1b93] generic-function.

<a id="x-28CL-TELEGRAM-BOT-2FCALLBACK-3AON-CALLBACK-20GENERIC-FUNCTION-29"></a>

##### [generic-function](4fe4) `on-callback` bot callback

Called when user clicks callback button. Second argument is an object of `CALLBACK` type.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCHAT-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/CHAT

<a id="x-28-23A-28-2820-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FCHAT-22-29-20PACKAGE-29"></a>

#### [package](91f1) `cl-telegram-bot/chat`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCHAT-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCHAT-24CHANNEL-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### CHANNEL

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ACHANNEL-20CLASS-29"></a>

###### [class](a7be) `channel` (base-group)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCHAT-24CHAT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### CHAT

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ACHAT-20CLASS-29"></a>

###### [class](855e) `chat` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CHAT-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ACHAT-29-29"></a>

###### [reader](8c57) `get-chat-id` (chat) (:id)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-HAS-PROTECTED-CONTENT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ACHAT-29-29"></a>

###### [reader](716f) `get-has-protected-content` (chat) (:has-protected-content)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-MESSAGE-AUTO-DELETE-TIME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ACHAT-29-29"></a>

###### [reader](723f) `get-message-auto-delete-time` (chat) (:message-auto-delete-time)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-RAW-DATA-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ACHAT-29-29"></a>

###### [reader](3c4f) `get-raw-data` (chat) (:raw-data)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-USERNAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ACHAT-29-29"></a>

###### [reader](4c0b) `get-username` (chat) (:username)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCHAT-24GROUP-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### GROUP

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGROUP-20CLASS-29"></a>

###### [class](1333) `group` (base-group)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCHAT-24PRIVATE-CHAT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### PRIVATE-CHAT

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3APRIVATE-CHAT-20CLASS-29"></a>

###### [class](a326) `private-chat` (chat)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-BIO-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3APRIVATE-CHAT-29-29"></a>

###### [reader](65f1) `get-bio` (private-chat) (:bio)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-FIRST-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3APRIVATE-CHAT-29-29"></a>

###### [reader](7322) `get-first-name` (private-chat) (:first-name)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-HAS-PRIVATE-FORWARDS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3APRIVATE-CHAT-29-29"></a>

###### [reader](ba51) `get-has-private-forwards` (private-chat) (:has-private-forwards)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-LAST-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3APRIVATE-CHAT-29-29"></a>

###### [reader](8b8b) `get-last-name` (private-chat) (:last-name)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCHAT-24SUPER-GROUP-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### SUPER-GROUP

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ASUPER-GROUP-20CLASS-29"></a>

###### [class](27bb) `super-group` (base-group)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CAN-SET-STICKER-SET-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ASUPER-GROUP-29-29"></a>

###### [reader](9175) `get-can-set-sticker-set` (super-group) (:can-set-sticker-set)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-JOIN-BY-REQUEST-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ASUPER-GROUP-29-29"></a>

###### [reader](cca3) `get-join-by-request` (super-group) (:join-by-request)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-JOIN-TO-SEND-MESSAGES-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ASUPER-GROUP-29-29"></a>

###### [reader](b3e7) `get-join-to-send-messages` (super-group) (:join-to-send-messages)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-SLOW-MODE-DELAY-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ASUPER-GROUP-29-29"></a>

###### [reader](1ddb) `get-slow-mode-delay` (super-group) (:slow-mode-delay)

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-STICKER-SET-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FCHAT-3ASUPER-GROUP-29-29"></a>

###### [reader](ac23) `get-sticker-set-name` (super-group) (:sticker-set-name)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCHAT-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ADELETE-CHAT-PHOTO-20FUNCTION-29"></a>

##### [function](af8d) `delete-chat-photo` bot-var1 chat

https://core.telegram.org/bots/api#deletechatphoto

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AEXPORT-CHAT-INVITE-LINK-20FUNCTION-29"></a>

##### [function](fa29) `export-chat-invite-link` bot-var1 chat

https://core.telegram.org/bots/api#exportchatinvitelink

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CHAT-ADMINISTRATORS-20FUNCTION-29"></a>

##### [function](c38a) `get-chat-administrators` bot-var1 chat

https://core.telegram.org/bots/api#getchatadministrators

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CHAT-BY-ID-20FUNCTION-29"></a>

##### [function](fdac) `get-chat-by-id` bot-var1 chat-id

https://core.telegram.org/bots/api#getchat

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CHAT-MEMBER-20FUNCTION-29"></a>

##### [function](46c1) `get-chat-member` bot-var1 chat user-id

https://core.telegram.org/bots/api#getchatmember

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AGET-CHAT-MEMBERS-COUNT-20FUNCTION-29"></a>

##### [function](9a80) `get-chat-members-count` bot-var1 chat

https://core.telegram.org/bots/api#getchatmemberscount

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AKICK-CHAT-MEMBER-20FUNCTION-29"></a>

##### [function](8f95) `kick-chat-member` bot-var1 chat user-id until-date

https://core.telegram.org/bots/api#kickchatmember

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ALEAVE-CHAT-20FUNCTION-29"></a>

##### [function](be17) `leave-chat` bot-var1 chat

https://core.telegram.org/bots/api#leavechat

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3APIN-CHAT-MESSAGE-20FUNCTION-29"></a>

##### [function](2115) `pin-chat-message` bot-var1 chat message-id disable-notification

https://core.telegram.org/bots/api#pinchatmessage

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3APROMOTE-CHAT-MEMBER-20FUNCTION-29"></a>

##### [function](d010) `promote-chat-member` bot-var1 chat user-id can-change-info can-post-messages can-edit-messages can-delete-messages can-invite-users can-restrict-members can-pin-messages can-promote-members

https://core.telegram.org/bots/api#promotechatmember

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ARESTRICT-CHAT-MEMBER-20FUNCTION-29"></a>

##### [function](227d) `restrict-chat-member` bot-var1 chat user-id until-date can-send-messages can-send-media-messages can-send-other-messages can-add-web-page-previews

https://core.telegram.org/bots/api#restrictchatmember

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ASEND-CHAT-ACTION-20FUNCTION-29"></a>

##### [function](87d3) `send-chat-action` bot-var1 chat action

https://core.telegram.org/bots/api#sendchataction

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ASET-CHAT-DESCRIPTION-20FUNCTION-29"></a>

##### [function](d819) `set-chat-description` bot-var1 chat description

https://core.telegram.org/bots/api#setchatdescription

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ASET-CHAT-PHOTO-20FUNCTION-29"></a>

##### [function](b678) `set-chat-photo` bot-var1 chat photo

https://core.telegram.org/bots/api#setchatphoto

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3ASET-CHAT-TITLE-20FUNCTION-29"></a>

##### [function](9daf) `set-chat-title` bot-var1 chat title

https://core.telegram.org/bots/api#setchattitle

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AUNBAN-CHAT-MEMBER-20FUNCTION-29"></a>

##### [function](3edf) `unban-chat-member` bot-var1 chat user-id

https://core.telegram.org/bots/api#unbanchatmember

<a id="x-28CL-TELEGRAM-BOT-2FCHAT-3AUNPIN-CHAT-MESSAGE-20FUNCTION-29"></a>

##### [function](3555) `unpin-chat-message` bot-var1 chat

https://core.telegram.org/bots/api#unpinchatmessage

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FCORE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/CORE

<a id="x-28-23A-28-2820-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FCORE-22-29-20PACKAGE-29"></a>

#### [package](f327) `cl-telegram-bot/core`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCORE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24REPLY-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### REPLY

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AREPLY-20CLASS-29"></a>

###### [class](5c41) `reply` (response-with-text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCORE-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3AON-COMMAND-20GENERIC-FUNCTION-29"></a>

##### [generic-function](8619) `on-command` bot command rest-text

This method will be called for each command.
First argument is a keyword. If user input was /save_note, then
first argument will be :save-note.

By default, logs call and does nothing.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AON-MESSAGE-20GENERIC-FUNCTION-29"></a>

##### [generic-function](c070) `on-message` bot text

This method gets called with raw text from the message.
By default it does nothing.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCORE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AREPLY-20FUNCTION-29"></a>

##### [function](f39d) `reply` text &rest args &key parse-mode disable-web-page-preview disable-notification reply-to-message-id reply-markup (immediately t)

Works like a [`send-message`][38a1], but only when an incoming message is processed.
Automatically sends reply to a chat from where current message came from.

<a id="x-28CL-TELEGRAM-BOT-2FCORE-3ASTART-PROCESSING-20FUNCTION-29"></a>

##### [function](10f0) `start-processing` BOT &KEY DEBUG (DELAY-BETWEEN-RETRIES 10) (THREAD-NAME "telegram-bot")

<a id="x-28CL-TELEGRAM-BOT-2FCORE-3ASTOP-PROCESSING-20FUNCTION-29"></a>

##### [function](a365) `stop-processing` bot

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FCORE-3FMacros-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Macros

<a id="x-28CL-TELEGRAM-BOT-2FBOT-3ADEFBOT-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

##### [macro](4509) `defbot` name

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/ENTITIES/COMMAND

<a id="x-28-23A-28-2832-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-22-29-20PACKAGE-29"></a>

#### [package](b4da) `cl-telegram-bot/entities/command`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-24BOT-COMMAND-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### BOT-COMMAND

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3ABOT-COMMAND-20CLASS-29"></a>

###### [class](62b9) `bot-command` (entity)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3AGET-COMMAND-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3ABOT-COMMAND-29-29"></a>

###### [reader](3668) `get-command` (bot-command) (:command)

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3AGET-REST-TEXT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3ABOT-COMMAND-29-29"></a>

###### [reader](5c1f) `get-rest-text` (bot-command) (:rest-text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3AON-COMMAND-20GENERIC-FUNCTION-29"></a>

##### [generic-function](8619) `on-command` bot command rest-text

This method will be called for each command.
First argument is a keyword. If user input was /save_note, then
first argument will be :save-note.

By default, logs call and does nothing.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FENTITIES-2FCORE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/ENTITIES/CORE

<a id="x-28-23A-28-2829-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FENTITIES-2FCORE-22-29-20PACKAGE-29"></a>

#### [package](f674) `cl-telegram-bot/entities/core`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FENTITIES-2FCORE-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FCORE-3AMAKE-ENTITY-INTERNAL-20GENERIC-FUNCTION-29"></a>

##### [generic-function](59bf) `make-entity-internal` entity-type payload data

Extendable protocol to support entities of different kinds.
First argument is a keyword, denoting a type of the entity.
Payload is an object of type `message'.
And data is a plist with data, describing the entity.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FENTITIES-2FCORE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FENTITIES-2FCORE-3AMAKE-ENTITY-20FUNCTION-29"></a>

##### [function](874c) `make-entity` payload data

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/INLINE-KEYBOARD

<a id="x-28-23A-28-2831-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-22-29-20PACKAGE-29"></a>

#### [package](a4ec) `cl-telegram-bot/inline-keyboard`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-24CALLBACK-BUTTON-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### CALLBACK-BUTTON

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3ACALLBACK-BUTTON-20CLASS-29"></a>

###### [class](d441) `callback-button` (inline-keyboard-button)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3ACALLBACK-BUTTON-DATA-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3ACALLBACK-BUTTON-29-29"></a>

###### [reader](1293) `callback-button-data` (callback-button) (:data)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-24INLINE-KEYBOARD-BUTTON-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### INLINE-KEYBOARD-BUTTON

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AINLINE-KEYBOARD-BUTTON-20CLASS-29"></a>

###### [class](cc59) `inline-keyboard-button` ()

Base class for all inline keyboard buttons.

`API`: https://core.telegram.org/bots/api#inlinekeyboardbutton

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3ABUTTON-TEXT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AINLINE-KEYBOARD-BUTTON-29-29"></a>

###### [reader](4c62) `button-text` (inline-keyboard-button) (:text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-24INLINE-KEYBOARD-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### INLINE-KEYBOARD

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AINLINE-KEYBOARD-20CLASS-29"></a>

###### [class](7ae0) `inline-keyboard` ()

Represents an inline keyboard as specified in `API` https://core.telegram.org/bots/api#inlinekeyboardmarkup.

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AKEYBOARD-ROWS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AINLINE-KEYBOARD-29-29"></a>

###### [reader](bc81) `keyboard-rows` (inline-keyboard) (:rows = nil)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-24URL-BUTTON-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### URL-BUTTON

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AURL-BUTTON-20CLASS-29"></a>

###### [class](3d88) `url-button` (inline-keyboard-button)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3ABUTTON-URL-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AURL-BUTTON-29-29"></a>

###### [reader](4efb) `button-url` (url-button) (:data)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AANSWER-CALLBACK-QUERY-20FUNCTION-29"></a>

##### [function](48eb) `answer-callback-query` bot callback &key text show-alert url

https://core.telegram.org/bots/api#answercallbackquery

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3ACALLBACK-BUTTON-20FUNCTION-29"></a>

##### [function](662b) `callback-button` text data

Creates a button which will call a callback.

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AINLINE-KEYBOARD-20FUNCTION-29"></a>

##### [function](4c4e) `inline-keyboard` rows

Returns an inline keyboard which can be passed
to `cl-telegram-bot/response:reply` ([`1`][0d9a] [`2`][9ce6]) as `REPLY-MARKUP` argument.

Each row should be a list of [`inline-keyboard-button`][cc87] objects or a single
object of this class. In latter case, such row will have only one button.

<a id="x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AURL-BUTTON-20FUNCTION-29"></a>

##### [function](9865) `url-button` text url

Creates a button which will open an url.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMARKUP-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/MARKUP

<a id="x-28-23A-28-2822-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FMARKUP-22-29-20PACKAGE-29"></a>

#### [package](30c4) `cl-telegram-bot/markup`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FMARKUP-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FMARKUP-3ATO-MARKUP-20GENERIC-FUNCTION-29"></a>

##### [generic-function](5956) `to-markup` obj

Transforms object into markup of Telegram `API`.

Methods of this class should return a hash-table, representing `OBJ`
in terms of Telegram `API`.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/MESSAGE

<a id="x-28-23A-28-2823-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FMESSAGE-22-29-20PACKAGE-29"></a>

#### [package](8976) `cl-telegram-bot/message`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FMESSAGE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24ANIMATION-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### ANIMATION-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AANIMATION-MESSAGE-20CLASS-29"></a>

###### [class](8971) `animation-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24ANIMATION-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### ANIMATION

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AANIMATION-20CLASS-29"></a>

###### [class](c017) `animation` (file temporal spatial)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24AUDIO-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### AUDIO-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AAUDIO-MESSAGE-20CLASS-29"></a>

###### [class](fb99) `audio-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24AUDIO-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### AUDIO

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AAUDIO-20CLASS-29"></a>

###### [class](91fb) `audio` (file temporal)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-PERFORMER-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AAUDIO-29-29"></a>

###### [reader](6155) `get-performer` (audio) (:performer)

Performer of the audio as defined by sender or by audio tags.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-TITLE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AAUDIO-29-29"></a>

###### [reader](67d0) `get-title` (audio) (:title)

Title of the audio as defined by sender or by audio tags.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24DOCUMENT-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### DOCUMENT-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ADOCUMENT-MESSAGE-20CLASS-29"></a>

###### [class](adb6) `document-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24DOCUMENT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### DOCUMENT

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ADOCUMENT-20CLASS-29"></a>

###### [class](80d3) `document` (file)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24FILE-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### FILE-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-MESSAGE-20CLASS-29"></a>

###### [class](6ae3) `file-message` (message)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FILE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-MESSAGE-29-29"></a>

###### [reader](859b) `get-file` (file-message) (:file)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24FILE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### FILE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-20CLASS-29"></a>

###### [class](0d27) `file` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FILE-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-29-29"></a>

###### [reader](14c1) `get-file-id` (file) (:file-id)

Identifier for this file, which can be used to download or reuse the file.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FILE-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-29-29"></a>

###### [reader](7abc) `get-file-name` (file) (:file-name)

Original filename as defined by sender.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FILE-SIZE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-29-29"></a>

###### [reader](23b7) `get-file-size` (file) (:file-size)

File size in bytes.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FILE-UNIQUE-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-29-29"></a>

###### [reader](7089) `get-file-unique-id` (file) (:file-unique-id)

Unique identifier for this file, which is supposed to be the same
over time and for different bots. Can't be used to download or reuse
the file.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-MIME-TYPE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AFILE-29-29"></a>

###### [reader](b814) `get-mime-type` (file) (:mime-type)

`MIME` type of the file as defined by sender.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-20CLASS-29"></a>

###### [class](d7db) `message` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-CAPTION-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

###### [reader](966a) `get-caption` (message) (:caption)

Caption for the animation, audio, document, photo, video or voice.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-CHAT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

###### [reader](b31c) `get-chat` (message) (:chat)

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-ENTITIES-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

###### [reader](d05a) `get-entities` (message) (:entities = nil)

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FORWARD-FROM-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

###### [reader](dbcc) `get-forward-from` (message) (:forward-from)

For forwarded messages, sender of the original message.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FORWARD-FROM-CHAT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

###### [reader](561a) `get-forward-from-chat` (message) (:forward-from-chat)

For messages forwarded from channels or from anonymous
administrators, information about the original sender chat.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-FORWARD-SENDER-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

###### [reader](386a) `get-forward-sender-name` (message) (:forward-sender-name)

For forwarded messages, sender of the original message.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-MESSAGE-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

###### [reader](2212) `get-message-id` (message) (:id)

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-RAW-DATA-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

###### [reader](90fe) `get-raw-data` (message) (:raw-data)

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-TEXT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AMESSAGE-29-29"></a>

###### [reader](8101) `get-text` (message) (:text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24PHOTO-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### PHOTO-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3APHOTO-MESSAGE-20CLASS-29"></a>

###### [class](fc2e) `photo-message` (file-message)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-PHOTO-OPTIONS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3APHOTO-MESSAGE-29-29"></a>

###### [reader](9d8e) `get-photo-options` (photo-message) (:photo-options)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24PHOTO-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### PHOTO

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3APHOTO-20CLASS-29"></a>

###### [class](2f7d) `photo` (file spatial)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24REPLY-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### REPLY

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AREPLY-20CLASS-29"></a>

###### [class](cf83) `reply` (message)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-REPLY-TO-MESSAGE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AREPLY-29-29"></a>

###### [reader](e514) `get-reply-to-message` (reply) (:reply-to-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24SPATIAL-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### SPATIAL

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASPATIAL-20CLASS-29"></a>

###### [class](722a) `spatial` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-HEIGHT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ASPATIAL-29-29"></a>

###### [reader](b0ba) `get-height` (spatial) (:height)

File height as defined by sender.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-WIDTH-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ASPATIAL-29-29"></a>

###### [reader](19dc) `get-width` (spatial) (:width)

File width as defined by sender.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24STICKER-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### STICKER-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASTICKER-MESSAGE-20CLASS-29"></a>

###### [class](a7dc) `sticker-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24STICKER-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### STICKER

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASTICKER-20CLASS-29"></a>

###### [class](0955) `sticker` (file spatial)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-EMOJI-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ASTICKER-29-29"></a>

###### [reader](1c7e) `get-emoji` (sticker) (:emoji)

Emoji associated with the sticker

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-IS-ANIMATED-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ASTICKER-29-29"></a>

###### [reader](40ce) `get-is-animated` (sticker) (:is-animated)

True if the sticker is animated.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-IS-VIDEO-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ASTICKER-29-29"></a>

###### [reader](4e01) `get-is-video` (sticker) (:is-video)

True if the sticker is a video sticker.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-SET-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ASTICKER-29-29"></a>

###### [reader](c695) `get-set-name` (sticker) (:set-name)

Name of the sticker set to which the sticker belongs.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24TEMPORAL-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### TEMPORAL

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ATEMPORAL-20CLASS-29"></a>

###### [class](1b71) `temporal` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-DURATION-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3ATEMPORAL-29-29"></a>

###### [reader](7efa) `get-duration` (temporal) (:duration)

Duration of the file in seconds as defined by sender.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24UNISPATIAL-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### UNISPATIAL

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AUNISPATIAL-20CLASS-29"></a>

###### [class](80e8) `unispatial` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-LENGTH-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FMESSAGE-3AUNISPATIAL-29-29"></a>

###### [reader](9560) `get-length` (unispatial) (:length)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24VIDEO-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### VIDEO-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AVIDEO-MESSAGE-20CLASS-29"></a>

###### [class](c4d1) `video-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24VIDEO-NOTE-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### VIDEO-NOTE-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AVIDEO-NOTE-MESSAGE-20CLASS-29"></a>

###### [class](2664) `video-note-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24VIDEO-NOTE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### VIDEO-NOTE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AVIDEO-NOTE-20CLASS-29"></a>

###### [class](1ed9) `video-note` (file temporal unispatial)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24VIDEO-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### VIDEO

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AVIDEO-20CLASS-29"></a>

###### [class](cf60) `video` (file temporal spatial)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24VOICE-MESSAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### VOICE-MESSAGE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AVOICE-MESSAGE-20CLASS-29"></a>

###### [class](f363) `voice-message` (file-message)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FMESSAGE-24VOICE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### VOICE

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AVOICE-20CLASS-29"></a>

###### [class](4140) `voice` (file temporal)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FMESSAGE-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AON-MESSAGE-20GENERIC-FUNCTION-29"></a>

##### [generic-function](c070) `on-message` bot text

This method gets called with raw text from the message.
By default it does nothing.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-ANIMATION-20GENERIC-FUNCTION-29"></a>

##### [generic-function](d99d) `send-animation` bot chat animation &rest options &key caption parse-mode caption-entities duration width height thumb disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

Sends animation to a chat.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-AUDIO-20GENERIC-FUNCTION-29"></a>

##### [generic-function](42d6) `send-audio` bot chat audio &rest options &key caption parse-mode caption-entities duration performer title thumb disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-DOCUMENT-20GENERIC-FUNCTION-29"></a>

##### [generic-function](8be4) `send-document` bot chat document &rest options &key caption parse-mode caption-entities disable-content-type-detection thumb disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-PHOTO-20GENERIC-FUNCTION-29"></a>

##### [generic-function](dabd) `send-photo` bot chat photo &rest options &key caption parse-mode caption-entities disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-STICKER-20GENERIC-FUNCTION-29"></a>

##### [generic-function](6f83) `send-sticker` bot chat sticker &rest options &key disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

A function to send sticker.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-VIDEO-20GENERIC-FUNCTION-29"></a>

##### [generic-function](27cb) `send-video` bot chat video &rest options &key caption parse-mode caption-entities duration width height thumb disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-VIDEO-NOTE-20GENERIC-FUNCTION-29"></a>

##### [generic-function](f4a9) `send-video-note` bot chat video-note &rest options &key caption parse-mode caption-entities duration length thumb disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-VOICE-20GENERIC-FUNCTION-29"></a>

##### [generic-function](47c6) `send-voice` bot chat voice &rest options &key caption parse-mode caption-entities duration disable-notification protect-content reply-to-message-id allow-sending-without-reply reply-markup

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FMESSAGE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ADELETE-MESSAGE-20FUNCTION-29"></a>

##### [function](5695) `delete-message` bot chat message

https://core.telegram.org/bots/api#deletemessage

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AFORWARD-MESSAGE-20FUNCTION-29"></a>

##### [function](fe2c) `forward-message` bot chat from-chat message &key disable-notification

https://core.telegram.org/bots/api#forwardmessage

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-CURRENT-CHAT-20FUNCTION-29"></a>

##### [function](95dd) `get-current-chat`

Returns a chat where currently processing message was received.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AGET-CURRENT-MESSAGE-20FUNCTION-29"></a>

##### [function](f658) `get-current-message`

Returns currently processed message.

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3AMAKE-MESSAGE-20FUNCTION-29"></a>

##### [function](33af) `make-message` data

<a id="x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-MESSAGE-20FUNCTION-29"></a>

##### [function](ab63) `send-message` bot chat text &rest options &key parse-mode disable-web-page-preview disable-notification reply-to-message-id reply-markup

https://core.telegram.org/bots/api#sendmessage

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FNETWORK-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/NETWORK

<a id="x-28-23A-28-2823-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FNETWORK-22-29-20PACKAGE-29"></a>

#### [package](f220) `cl-telegram-bot/network`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FNETWORK-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FNETWORK-24REQUEST-ERROR-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### REQUEST-ERROR

<a id="x-28CL-TELEGRAM-BOT-2FNETWORK-3AREQUEST-ERROR-20CONDITION-29"></a>

###### [condition](7d33) `request-error` (error)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FNETWORK-3AWHAT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FNETWORK-3AREQUEST-ERROR-29-29"></a>

###### [reader](7d33) `what` (request-error) (:what)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FNETWORK-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FNETWORK-3AMAKE-REQUEST-20FUNCTION-29"></a>

##### [function](579b) `make-request` bot name &rest options &key (streamp nil) (timeout 3) &allow-other-keys

Perform `HTTP` request to 'name `API` method with 'options `JSON`-encoded object.

<a id="x-28CL-TELEGRAM-BOT-2FNETWORK-3ASET-PROXY-20FUNCTION-29"></a>

##### [function](5070) `set-proxy` proxy

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FPIPELINE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/PIPELINE

<a id="x-28-23A-28-2824-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FPIPELINE-22-29-20PACKAGE-29"></a>

#### [package](91c3) `cl-telegram-bot/pipeline`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FPIPELINE-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FPIPELINE-3APROCESS-20GENERIC-FUNCTION-29"></a>

##### [generic-function](f88b) `process` bot object

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

#### [package](c9b7) `cl-telegram-bot/response`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FRESPONSE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24ALERT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### ALERT

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AALERT-20CLASS-29"></a>

###### [class](dda9) `alert` (response-with-text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24NOTIFY-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### NOTIFY

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3ANOTIFY-20CLASS-29"></a>

###### [class](18d5) `notify` (response-with-text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24OPEN-URL-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### OPEN-URL

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AOPEN-URL-20CLASS-29"></a>

###### [class](e4f8) `open-url` (response)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AURL-TO-OPEN-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FRESPONSE-3AOPEN-URL-29-29"></a>

###### [reader](3134) `url-to-open` (open-url) (:text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24REPLY-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### REPLY

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AREPLY-20CLASS-29"></a>

###### [class](5c41) `reply` (response-with-text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24RESPONSE-WITH-TEXT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### RESPONSE-WITH-TEXT

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3ARESPONSE-WITH-TEXT-20CLASS-29"></a>

###### [class](c566) `response-with-text` (response)

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3ARESPONSE-TEXT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FRESPONSE-3ARESPONSE-WITH-TEXT-29-29"></a>

###### [reader](6a83) `response-text` (response-with-text) (:text)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-24RESPONSE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### RESPONSE

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3ARESPONSE-20CLASS-29"></a>

###### [class](da69) `response` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AREST-ARGS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FRESPONSE-3ARESPONSE-29-29"></a>

###### [reader](f7f8) `rest-args` (response) (:args)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FRESPONSE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AALERT-20FUNCTION-29"></a>

##### [function](4218) `alert` text

Works like a [`send-message`][38a1], but only when an incoming message is processed.
Automatically sends reply to a chat from where current message came from.

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3ANOTIFY-20FUNCTION-29"></a>

##### [function](0fff) `notify` text

Works like a [`send-message`][38a1], but only when an incoming message is processed.
Automatically sends reply to a chat from where current message came from.

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AOPEN-URL-20FUNCTION-29"></a>

##### [function](6554) `open-url` url

Works like a [`send-message`][38a1], but only when an incoming message is processed.
Automatically sends reply to a chat from where current message came from.

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-3AREPLY-20FUNCTION-29"></a>

##### [function](f39d) `reply` text &rest args &key parse-mode disable-web-page-preview disable-notification reply-to-message-id reply-markup (immediately t)

Works like a [`send-message`][38a1], but only when an incoming message is processed.
Automatically sends reply to a chat from where current message came from.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/RESPONSE-PROCESSING

<a id="x-28-23A-28-2835-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-22-29-20PACKAGE-29"></a>

#### [package](4a84) `cl-telegram-bot/response-processing`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-24INTERRUPT-PROCESSING-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### INTERRUPT-PROCESSING

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3AINTERRUPT-PROCESSING-20CONDITION-29"></a>

###### [condition](3476) `interrupt-processing` ()

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3APROCESS-RESPONSE-20GENERIC-FUNCTION-29"></a>

##### [generic-function](25bf) `process-response` bot message response

Processes immediate responses of different types.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FRESPONSE-PROCESSING-3AINTERRUPT-PROCESSING-20FUNCTION-29"></a>

##### [function](4559) `interrupt-processing`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FUPDATE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/UPDATE

<a id="x-28-23A-28-2822-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FUPDATE-22-29-20PACKAGE-29"></a>

#### [package](e6ee) `cl-telegram-bot/update`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FUPDATE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FUPDATE-24UPDATE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### UPDATE

<a id="x-28CL-TELEGRAM-BOT-2FUPDATE-3AUPDATE-20CLASS-29"></a>

###### [class](209e) `update` ()

**Readers**

<a id="x-28CL-TELEGRAM-BOT-2FUPDATE-3AGET-PAYLOAD-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUPDATE-3AUPDATE-29-29"></a>

###### [reader](83b1) `get-payload` (update) (:payload)

<a id="x-28CL-TELEGRAM-BOT-2FUPDATE-3AGET-RAW-DATA-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUPDATE-3AUPDATE-29-29"></a>

###### [reader](947a) `get-raw-data` (update) (:raw-data)

<a id="x-28CL-TELEGRAM-BOT-2FUPDATE-3AGET-UPDATE-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-TELEGRAM-BOT-2FUPDATE-3AUPDATE-29-29"></a>

###### [reader](645c) `get-update-id` (update) (:id)

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FUPDATE-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-TELEGRAM-BOT-2FUPDATE-3APROCESS-UPDATES-20GENERIC-FUNCTION-29"></a>

##### [generic-function](a390) `process-updates` bot

By default, this method starts an infinite loop and fetching new updates using long polling.

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FUPDATE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FUPDATE-3AMAKE-UPDATE-20FUNCTION-29"></a>

##### [function](ddc3) `make-update` data

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CL-TELEGRAM-BOT-2FUTILS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-TELEGRAM-BOT/UTILS

<a id="x-28-23A-28-2821-29-20BASE-CHAR-20-2E-20-22CL-TELEGRAM-BOT-2FUTILS-22-29-20PACKAGE-29"></a>

#### [package](276b) `cl-telegram-bot/utils`

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-7C-40CL-TELEGRAM-BOT-2FUTILS-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-TELEGRAM-BOT-2FUTILS-3AMAKE-KEYWORD-20FUNCTION-29"></a>

##### [function](4225) `make-keyword` text

<a id="x-28CL-TELEGRAM-BOT-2FUTILS-3AOBFUSCATE-20FUNCTION-29"></a>

##### [function](9d84) `obfuscate` url

<a id="x-28CL-TELEGRAM-BOT-DOCS-2FINDEX-3A-3A-40CREDITS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Credits

* [Rei][b588] – initial version.
* [Alexander Artemenko][891d] – large refactoring, usage of `CLOS` classes, etc.


[6949]: https://40ants.com/cl-telegram-bot/
[6611]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT-2FCALLBACK-3ACALLBACK-20CLASS-29
[1b93]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT-2FCALLBACK-3AON-CALLBACK-20GENERIC-FUNCTION-29
[56c0]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT-2FENTITIES-2FCOMMAND-3AON-COMMAND-20GENERIC-FUNCTION-29
[cc87]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT-2FINLINE-KEYBOARD-3AINLINE-KEYBOARD-BUTTON-20CLASS-29
[38a1]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT-2FMESSAGE-3ASEND-MESSAGE-20FUNCTION-29
[9ce6]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT-2FRESPONSE-3AREPLY-20CLASS-29
[0d9a]: https://40ants.com/cl-telegram-bot/#x-28CL-TELEGRAM-BOT-2FRESPONSE-3AREPLY-20FUNCTION-29
[53d1]: https://github.com/40ants/cl-telegram-bot
[7bb5]: https://github.com/40ants/cl-telegram-bot/actions
[a0ce]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/bot.lisp#L1
[be8b]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/bot.lisp#L19
[a9f4]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/bot.lisp#L20
[9396]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/bot.lisp#L24
[b072]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/bot.lisp#L29
[d193]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/bot.lisp#L33
[7244]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/bot.lisp#L37
[da65]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/bot.lisp#L42
[6bea]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/bot.lisp#L47
[4509]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/bot.lisp#L55
[cd8b]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/callback.lisp#L1
[4364]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/callback.lisp#L27
[fd4a]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/callback.lisp#L28
[1490]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/callback.lisp#L31
[a11d]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/callback.lisp#L34
[4fe4]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/callback.lisp#L39
[5d19]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/callback.lisp#L46
[349a]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/callback.lisp#L76
[91f1]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L1
[1333]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L111
[27bb]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L115
[b3e7]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L116
[cca3]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L118
[1ddb]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L120
[ac23]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L122
[9175]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L124
[a7be]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L127
[fdac]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L157
[8f95]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L163
[3edf]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L167
[227d]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L171
[d010]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L181
[fa29]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L194
[b678]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L198
[af8d]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L202
[9daf]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L206
[d819]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L210
[2115]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L214
[3555]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L218
[be17]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L222
[c38a]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L226
[9a80]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L230
[46c1]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L234
[87d3]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L238
[855e]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L54
[8c57]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L55
[4c0b]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L57
[716f]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L59
[723f]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L61
[3c4f]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L63
[a326]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L83
[7322]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L84
[8b8b]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L86
[65f1]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L88
[ba51]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/chat.lisp#L90
[f327]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/core.lisp#L1
[10f0]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/core.lisp#L37
[a365]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/core.lisp#L72
[b4da]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/entities/command.lisp#L1
[62b9]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/entities/command.lisp#L36
[3668]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/entities/command.lisp#L37
[5c1f]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/entities/command.lisp#L40
[8619]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/entities/command.lisp#L63
[f674]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/entities/core.lisp#L1
[59bf]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/entities/core.lisp#L23
[874c]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/entities/core.lisp#L36
[a4ec]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/inline-keyboard.lisp#L1
[7ae0]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/inline-keyboard.lisp#L23
[bc81]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/inline-keyboard.lisp#L24
[cc59]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/inline-keyboard.lisp#L31
[4c62]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/inline-keyboard.lisp#L32
[d441]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/inline-keyboard.lisp#L40
[1293]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/inline-keyboard.lisp#L41
[3d88]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/inline-keyboard.lisp#L46
[4efb]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/inline-keyboard.lisp#L47
[4c4e]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/inline-keyboard.lisp#L52
[662b]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/inline-keyboard.lisp#L62
[9865]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/inline-keyboard.lisp#L67
[48eb]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/inline-keyboard.lisp#L73
[30c4]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/markup.lisp#L1
[5956]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/markup.lisp#L7
[8976]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L1
[8101]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L101
[966a]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L105
[b31c]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L109
[d05a]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L112
[90fe]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L116
[dbcc]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L118
[386a]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L122
[561a]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L126
[1b71]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L148
[7efa]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L149
[722a]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L155
[b0ba]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L156
[19dc]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L161
[80e8]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L167
[9560]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L168
[0d27]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L173
[14c1]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L174
[7089]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L179
[7abc]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L186
[23b7]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L191
[b814]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L196
[2f7d]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L202
[91fb]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L204
[6155]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L205
[67d0]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L210
[c017]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L216
[80d3]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L218
[cf60]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L220
[1ed9]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L222
[4140]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L224
[0955]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L227
[40ce]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L228
[4e01]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L232
[1c7e]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L236
[c695]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L240
[6ae3]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L257
[859b]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L258
[fb99]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L270
[adb6]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L272
[8971]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L274
[fc2e]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L276
[9d8e]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L277
[a7dc]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L287
[c4d1]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L289
[2664]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L291
[f363]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L293
[cf83]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L296
[e514]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L297
[33af]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L307
[ab63]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L336
[dabd]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L355
[42d6]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L415
[8be4]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L458
[27cb]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L501
[d99d]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L544
[f4a9]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L567
[47c6]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L610
[6f83]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L653
[fe2c]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L711
[5695]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L755
[c070]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L762
[f658]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L798
[95dd]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L805
[d7db]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L98
[2212]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/message.lisp#L99
[f220]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/network.lisp#L1
[5070]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/network.lisp#L19
[7d33]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/network.lisp#L22
[579b]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/network.lisp#L29
[91c3]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/pipeline.lisp#L1
[f88b]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/pipeline.lisp#L8
[4a84]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/response-processing.lisp#L1
[25bf]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/response-processing.lisp#L12
[4559]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/response-processing.lisp#L16
[3476]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/response-processing.lisp#L8
[c9b7]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/response.lisp#L1
[4218]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/response.lisp#L110
[6554]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/response.lisp#L124
[da69]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/response.lisp#L31
[f7f8]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/response.lisp#L32
[c566]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/response.lisp#L37
[6a83]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/response.lisp#L38
[5c41]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/response.lisp#L42
[18d5]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/response.lisp#L46
[dda9]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/response.lisp#L50
[e4f8]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/response.lisp#L54
[3134]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/response.lisp#L55
[f39d]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/response.lisp#L61
[0fff]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/response.lisp#L96
[e6ee]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/update.lisp#L1
[209e]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/update.lisp#L25
[645c]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/update.lisp#L26
[83b1]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/update.lisp#L28
[947a]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/update.lisp#L30
[ddc3]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/update.lisp#L38
[a390]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/update.lisp#L97
[276b]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/utils.lisp#L1
[4225]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/utils.lisp#L17
[9d84]: https://github.com/40ants/cl-telegram-bot/blob/34362851f9d29da514b0dd7c39a3a4478fec3fc9/src/utils.lisp#L24
[5798]: https://github.com/40ants/cl-telegram-bot/issues
[b588]: https://github.com/sovietspaceship
[891d]: https://github.com/svetlyak40wt
[8236]: https://quickdocs.org/alexandria
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

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
