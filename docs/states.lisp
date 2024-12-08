(uiop:define-package #:cl-telegram-bot-docs/states
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:cl-telegram-bot2/state
                #:state)
  (:import-from #:cl-telegram-bot2/actions/send-text
                #:send-text))
(in-package #:cl-telegram-bot-docs/states)


(defsection @states-and-actions (:title "States and Actions")
  "
This framework makes it possible to define bot with all allowed state.

The state defines behaviour of the bot, the way it should respond to commands, updates and other events.
"
  (@states section)
  (@actions section))


(defsection @states (:title "States")
  "
There can be more than one handler for the event. We call these handlers \"Actions\".

An action should return a NIL or a new state. In latter case, the current bot's state will be changed to the new one and handlers for `on-activation` event will be called.

State is constructed using STATE function, which accepts handlers for different kinds of events. Here is simples state which greets a user when it start the chat and then reply with the same text:

```
(defun reply-with-same-text (update)
  (reply (message-text
          (update-message update)))
  (values))


(state (send-text \"Hello, I'm the echo bot.\")
       :on-update 'reply-with-same-text)
```

The first argument to STATE function is a handler for `on-activation` event. If you don't want to react on activation, you can pass NIL instead. The SEND-TEXT function returns an action instance. This way, we tell what bot should do, we use a declarative way to describe bot's behaviour.

The :ON-UPDATE argument specifies a handler for `on-update` event. This is the most generic event which occur when bot receives an update which wasn't processed by other event handlers. For this handler we are using a custom function bound to the symbol `reply-with-same-text`. The function accepts a single argument - update object. Use generic functions from `cl-telegram-bot2/api` package to work with this update object.

The reason why we only accept a special action object or a symbol but not a lambda function is because this way we'll be able to generate schemas of all states and transitions between them. Another reason is that it will be possible to redefine fbound function and use interactive approach to changing bot's behaviour.

See other support events in STATE function documentation.
")


(defsection @actions (:title "Actions")
  "
")
