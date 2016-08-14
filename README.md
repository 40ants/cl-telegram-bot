# Telegram Bot API for Common Lisp

See the [Telegram Bot API](https://core.telegram.org/bots/api).

This library uses [Drakma](http://weitz.de/drakma/) and [CL-JSON](https://common-lisp.net/project/cl-json/) with CLOS semantics.

This library has the following aliases: cl-telegram-bot, tl-bot, tg-bot.

- function `(make-bot token)`
    Returns a bot instance for a given token. To get a new token, see [here](https://core.telegram.org/bots#3-how-do-i-create-a-bot).

- function `(access object &rest slot-list)`
    Convenience function to access nested fields in a JSON object. For example, to access update.message.from.id, you can use
    `(access update 'message 'from 'id)`. This operation is linear in time, so I suggest keeping it at a minimum,
    reusing the fields multiple times, e.g. using a let*. 
    You can use this function from any JSON field, so `(access message 'from 'id)` from a previously accessed message field
    should be used when many nested fields share a common parent.

- function `(decode json-object)`
    Decode JSON object to CLOS object. Use to convert the return value of API calls when needed, e.g.
    `(decode (send-message ...)` returns an object ready to be used (by `access`, for example).

- function `(get-slot obj slot)`
    Returns slot from obj, NIL is unbound. Use with JSON CLOS object.

- error `request-error`
    Used (currently) by get-updates on HTTP error.

- function `(cl-telegram-bot::get-class-slots object)` (SBCL only)
    Use this function to inspect JSON objects. For debugging only.

- function `(cl-telegram-bot::make-request b method-name options-alist)`
    Make direct API request using Drakma. Use for debugging only.

- function [`(get-updates bot &key limit timeout)`](https://core.telegram.org/bots/api#getupdates)

    NOTE: The offset parameter is omitted as it is internally managed by the tl-bot:bot class.

## API methods

NOTE: the keyword argument :reply from the official API was renamed to :reply in every function.

- function [`(set-webhook bot &key url certificate)`](https://core.telegram.org/bots/api#setwebhook)

- function [`(send-message bot chat-id text &key parse-mode disable-web-page-preview disable-notification reply)`](https://core.telegram.org/bots/api#sendmessage)

- function [`(forward-message bot chat-id from-chat-id message-id &key disable-notification)`](https://core.telegram.org/bots/api#forwardmessage)

- function [`(send-photo bot chat-id photo &key caption disable-notification reply reply-markup)`](https://core.telegram.org/bots/api#sendphoto)

- function [`(send-audio bot chat-id audio &key duration performer title disable-notification reply reply-markup)`](https://core.telegram.org/bots/api#sendaudio)

- function [`(send-document bot chat-id document &key caption disable-notification reply reply-markup)`](https://core.telegram.org/bots/api#senddocument)

- function [`(send-sticker bot chat-id sticker &key disable-notification reply reply-markup)`](https://core.telegram.org/bots/api#sendsticker)

- function [`(send-video bot chat-id video &key duration width height caption disable-notification reply reply-markup)`](https://core.telegram.org/bots/api#sendvideo)

- function [`(send-voice bot chat-id voice &key duration disable-notification reply reply-markup)`](https://core.telegram.org/bots/api#sendvoice)

- function [`(send-location bot chat-id latitude longitude &key disable-notification reply reply-markup)`](https://core.telegram.org/bots/api#sendlocation)

- function [`(send-venue bot chat-id latitude longitude title address &key foursquare-id disable-notification reply reply-markup)`](https://core.telegram.org/bots/api#sendvenue)

- function [`(send-contact bot chat-id phone-number first-name &key last-name disable-notification reply reply-markup)`](https://core.telegram.org/bots/api#sendcontact)

- function [`(send-chat-action bot chat-id action)`](https://core.telegram.org/bots/api#sendchataction)

- function [`(get-user-profile-photos bot user-id &key offset limit)`](https://core.telegram.org/bots/api#getuserprofilephotos)

- function [`(get-file bot file-id)`](https://core.telegram.org/bots/api#getfile)

- function [`(kick-chat-member bot chat-id user-id)`](https://core.telegram.org/bots/api#kickchatmember)

- function [`(leave-chat bot chat-id)`](https://core.telegram.org/bots/api#leavechat)

- function [`(unban-chat-member bot chat-id user-id)`](https://core.telegram.org/bots/api#unbanchatmember)

- function [`(get-chat bot chat-id)`](https://core.telegram.org/bots/api#getchat)

- function [`(get-chat-administrators bot chat-id)`](https://core.telegram.org/bots/api#getchatadministrators)

- function [`(get-chat-members-count bot chat-id)`](https://core.telegram.org/bots/api#getchatmemberscount)

- function [`(get-chat-member bot chat-id user-id)`](https://core.telegram.org/bots/api#getchatmember)

- function [`(answer-callback-query bot callback-query-id &key text show-alert)`](https://core.telegram.org/bots/api#answercallbackquery)

- function [`(edit-message-text bot chat-id message-id inline-message-id text &key parse-mode disable-web-page-preview reply-markup)`](https://core.telegram.org/bots/api#editmessagetext)

- function [`(edit-message-caption bot chat-id message-id inline-message-id &key caption reply-markup)`](https://core.telegram.org/bots/api#editmessagecaption)

- function [`(edit-message-reply-markup bot chat-id message-id inline-message-id &key reply-markup)`](https://core.telegram.org/bots/api#editmessagereplymarkup)

- function [`(answer-inline-query bot inline-query-id results &key cache-time is-personal next-offset switch-pm-text)`](https://core.telegram.org/bots/api#answerinlinequery)

