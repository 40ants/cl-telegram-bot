(uiop:define-package #:cl-telegram-bot-docs/states
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:cl-telegram-bot2/state
                #:state)
  (:import-from #:cl-telegram-bot2/actions/send-text
                #:send-text)
  (:import-from #:cl-telegram-bot2/actions/send-photo
                #:send-photo)
  (:import-from #:cl-telegram-bot2/actions/send-invoice
                #:send-invoice)
  (:import-from #:cl-telegram-bot2/actions/edit-message-media
                #:edit-message-media))
(in-package #:cl-telegram-bot-docs/states)


(defsection @states-and-actions (:title "States and Actions")
  "
This framework makes it possible to define bot with all allowed state.

The state defines behaviour of the bot, the way it should respond to commands, updates and other events.
"
  (@states section)
  (@actions section)
  (@event-processing section))


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
Actions in cl-telegra-bot are small objects holding an information about what should be done on some event. Typically, you will want to reply with some text or send a photo.

Usually, actions are created using a function having the same name as action's class. Here are which actions are available:

- [SEND-TEXT][function]
- [SEND-PHOTO][function]
- [SEND-INVOICE][function]
- [EDIT-MESSAGE-MEDIA][function]

More actions will be added in future and you can create your own.

Also, a function bound symbol can be used instead an action object. Why do we require a symbol but not a function object? Because symbol has a name and it can be useful when we want to save bot's state or to render states graph.
")


(defsection @event-processing (:title "Event processing")
  "
When some event occur, a corresponding generic function is called first on state object then on an action specified for this kind.

For example, if new update was received, then CL-TELEGRAM-BOT2/GENERICS:PROCESS-UPDATE generic-function will be called with current state as the first argument
and update object as the second argument. Then the method specified on state class will call the same CL-TELEGRAM-BOT2/GENERICS:PROCESS-UPDATE generic-function
on the object specified as :ON-UPDATE argument for the action. If action is a symbol, then it's function will be called with update object as a single argument in case if this function accepts one argument and without any arguments otherwise.

Action's method should return should return a new state object if it wants to change the current bot's state or NIL otherwise. If new state was returned, then `on-activate` event will be processed afterwards.

Instead of one action a list of actions can be specified as an event handler. In this case processing will stop on an action which returns a new state.
")
