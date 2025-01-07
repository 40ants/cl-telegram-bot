(uiop:define-package #:cl-telegram-bot-docs/tutorial
  (:use #:cl)
  (:import-from #:cl-telegram-bot2/actions/send-text
                #:send-text)
  (:import-from #:40ants-doc
                #:defsection))
(in-package #:cl-telegram-bot-docs/tutorial)


(defsection @first-bot (:title "Our First Telegram Bot (tutorial)"
                        :ignore-words ("REPL"))
  "
For the start, you need to create a bot and get it's token from the BotFather bot:

![](asdf:cl-telegram-bot-media:images/create-a-bot.png)

When you've got token, go to the REPL and define our bot:

```
CL-USER> (defvar *token* \"52...\")

CL-USER> (cl-telegram-bot2/bot:defbot test-bot ()
           ())

CL-USER> (cl-telegram-bot2/server:start-polling (make-test-bot *token*))
```

Last call will be interrupted with `Required argument \"Initial state is required argument.\" missing.` error.
This is because in second version of cl-telegram-bot bot always should have some state. From the current state
depends bot's behaviour, commands which are available and a set of handlers for different events. Each chat
has it's own current state. This allows bot to keep context when working with each user.

When you are creating a bot instance, you should give at a state definition. Let's create a bot with simple state
which will great a new user:

```
CL-USER> (cl-telegram-bot2/server:start-polling
          (make-test-bot *token*
                         :initial-state
                         (cl-telegram-bot2/state:state
                          (cl-telegram-bot2/actions/send-text:send-text
                           \"Hello from cl-telegram-bot!\"))))
     
#<FUNCTION (FLET CL-TELEGRAM-BOT2/SERVER::STOP-BOT :IN CL-TELEGRAM-BOT2/SERVER:START-POLLING) {100B11524B}>

CL-USER> (defparameter *stop-func* *)
```

Note, the bot was started and a function which can stop it was returned from CL-TELEGRAM-BOT2/SERVER:START-POLLING function.

Now let's see how our bot will behave:

![](asdf:cl-telegram-bot-media:images/tutorial/tutorial-1.gif)

As you can see, our bot greets the user but does not respond ot it's message. What if we'll use SEND-TEXT function to create
an action for update event?

```
CL-USER> (funcall *stop-func*)

CL-USER> (setf *stop-func*
               (cl-telegram-bot2/server:start-polling
                (make-test-bot *token*
                               :initial-state
                               (cl-telegram-bot2/state:state
                                (cl-telegram-bot2/actions/send-text:send-text
                                 \"Hello from cl-telegram-bot!\")
                                :on-update (cl-telegram-bot2/actions/send-text:send-text
                                            \"The response to the message\")))))
```

Now our bot will respond to the message with a static message:

![](asdf:cl-telegram-bot-media:images/tutorial/tutorial-2.gif)

Good! But what if we want to execute some custom logic before response? The one way is to define your own action class, but the easiest way
is to use a function. For demonstration, we'll create a function which will reply with a reversed text:

```
CL-USER> (funcall *stop-func*)

CL-USER> (defun reply-with-reversed-text (update)
           (cl-telegram-bot2/high:reply
            (reverse (cl-telegram-bot2/api:message-text
                      (cl-telegram-bot2/api:update-message update))))
           ;; Return no values because we don't want to change
           ;; the state:
           (values))

CL-USER> (setf *stop-func*
               (cl-telegram-bot2/server:start-polling
                (make-test-bot *token*
                               :initial-state
                               (cl-telegram-bot2/state:state
                                (cl-telegram-bot2/actions/send-text:send-text
                                 \"Hello from cl-telegram-bot!\")
                                ;; Note, here we specify as a handler the fbound symbol:
                                :on-update 'reply-with-reversed-text))))
```

![](asdf:cl-telegram-bot-media:images/tutorial/tutorial-3.gif)

Now let's combine two actions together. First we'll send a static text, then call a function
and send the reversed user input:

```
CL-USER> (funcall *stop-func*)

CL-USER> (setf *stop-func*
               (cl-telegram-bot2/server:start-polling
                (make-test-bot *token*
                               :initial-state
                               (cl-telegram-bot2/state:state
                                (cl-telegram-bot2/actions/send-text:send-text
                                 \"Hello from cl-telegram-bot!\")
                                ;; Note, here we specify as a handler the list of an action
                                ;; and a fbound symbol:
                                :on-update (list (cl-telegram-bot2/actions/send-text:send-text
                                                  \"Here how you text will look like when reversed:\")
                                                 'reply-with-reversed-text)))))
```

![](asdf:cl-telegram-bot-media:images/tutorial/tutorial-4.gif)

As we said in the beginning of the tutorial, the real power of the second version of cl-telegram-bot is
it's ability to keep context as the current state. At the next step we'll create the second state at which
bot will calculate the number of symbols in the user input.

Here is how the workflow will work:

- On /start command the bot will switch to the first state and greet the user.
- On any message, the bot will repond with it's reversed version and then switch to the second state.
- In the second state the bot will respond with a number of symbols in user's input.

```
CL-USER> (defun reply-with-num-symbols (update)
           (let ((input-text
                   (cl-telegram-bot2/api:message-text
                      (cl-telegram-bot2/api:update-message update))))
             (cl-telegram-bot2/high:reply
              (format nil \"Your input has ~A chars.\"
                      (length input-text)))
             ;; Return no values because we don't want to change
             ;; the state:
             (values)))

CL-USER> (funcall *stop-func*)

CL-USER> (setf *stop-func*
               (cl-telegram-bot2/server:start-polling
                (make-test-bot *token*
                               :initial-state
                               (cl-telegram-bot2/state:state
                                (cl-telegram-bot2/actions/send-text:send-text
                                 \"Hello from cl-telegram-bot!\")
                                ;; Note, here we specify as a handler the list of an action
                                ;; and a fbound symbol:
                                :on-update (list (cl-telegram-bot2/actions/send-text:send-text
                                                  \"Here how you text will look like when reversed:\")
                                                 'reply-with-reversed-text
                                                 ;; Now switch to the second state
                                                 (cl-telegram-bot2/state:state
                                                  (cl-telegram-bot2/actions/send-text:send-text
                                                   \"Now bot is in the second state.\")
                                                  ;; This is how we count the symbols in user input
                                                  :on-update 'reply-with-num-symbols))))))
```


![](asdf:cl-telegram-bot-media:images/tutorial/tutorial-5.gif)

As you can see, now our bot has stuck in the second state and there is no way to jump back to the first one.
How would we do this?

State change inside the bot creates a stack of states:

- Second state (current)
- First state

There are special actions which allows to unwind this stack to one of the previous states:

- [cl-telegram-bot2/term/back:back][function] - goes the previous state.
- [cl-telegram-bot2/term/back:back-to][function] - goes the previous state or given class.
- [cl-telegram-bot2/term/back:back-to-nth-parent][function] - just like [cl-telegram-bot2/term/back:back][function], but allows to jump a few levels higher.
- [cl-telegram-bot2/term/back:back-to-id][function] - allows to go to the state with a given id (id is an optional state attribute and especially useful for returning  to the particular state).

Let's try to use the simplest form to return to the first state:

```
CL-USER> (funcall *stop-func*)

CL-USER> (setf *stop-func*
               (cl-telegram-bot2/server:start-polling
                (make-test-bot *token*
                               :initial-state
                               (cl-telegram-bot2/state:state
                                (cl-telegram-bot2/actions/send-text:send-text
                                 \"Hello from cl-telegram-bot!\")
                                ;; Note, here we specify as a handler the list of an action
                                ;; and a fbound symbol:
                                :on-update (list (cl-telegram-bot2/actions/send-text:send-text
                                                  \"Here how you text will look like when reversed:\")
                                                 'reply-with-reversed-text
                                                 ;; Now switch to the second state
                                                 (cl-telegram-bot2/state:state
                                                  (cl-telegram-bot2/actions/send-text:send-text
                                                   \"Now bot is in the second state.\")
                                                  ;; This is how we count the symbols in user input
                                                  ;; and return the the previous state:
                                                  :on-update (list 'reply-with-num-symbols
                                                                   (cl-telegram-bot2/term/back:back))))))))
```

![](asdf:cl-telegram-bot-media:images/tutorial/tutorial-6.gif)

As you can see, now bot switches between first and second states. But `back` function can do more, because
this kind of actions are special and is able not only to switch current bot's state, but also to return some
results to this parent state.

To return some result we should give it as an optional argument to the CL-TELEGRAM-BOT2/TERM/BACK:BACK function:

```
(cl-telegram-bot2/term/back:back \"Some result\")
```

and to process this result, we have to specify `on-result` event handler on the first state.
Here is how complete example will look like:

```
CL-USER> (funcall *stop-func*)

CL-USER> (defun reply-with-num-symbols (update)
           (let* ((input-text
                    (cl-telegram-bot2/api:message-text
                     (cl-telegram-bot2/api:update-message update)))
                  (num-symbols
                    (length input-text)))
             (cl-telegram-bot2/high:reply
              (format nil \"Your input has ~A chars.\"
                      num-symbols))
             ;; Return BACK action to return num symbols to the first state:
             (cl-telegram-bot2/term/back:back num-symbols)))

CL-USER> (defun process-result (num-symbols)
           (cl-telegram-bot2/high:reply
            (format nil \"Now we are in the first state and the second state returned ~A chars.\"
                    num-symbols))
           (values))

CL-USER> (setf *stop-func*
               (cl-telegram-bot2/server:start-polling
                (make-test-bot *token*
                               :initial-state
                               (cl-telegram-bot2/state:state
                                (cl-telegram-bot2/actions/send-text:send-text
                                 \"Hello from cl-telegram-bot!\")
                                ;; Note, here we specify as a handler the list of an action
                                ;; and a fbound symbol:
                                :on-update (list (cl-telegram-bot2/actions/send-text:send-text
                                                  \"Here how you text will look like when reversed:\")
                                                 'reply-with-reversed-text
                                                 ;; Now switch to the second state
                                                 (cl-telegram-bot2/state:state
                                                  (cl-telegram-bot2/actions/send-text:send-text
                                                   \"Now bot is in the second state.\")
                                                  ;; This is how we count the symbols in user input
                                                  ;; and return it to the initial state:
                                                  :on-update 'reply-with-num-symbols))
                                :on-result 'process-result))))
```

![](asdf:cl-telegram-bot-media:images/tutorial/tutorial-7.gif)

This is all for now. In the next tutorial we'll see how to define a custom states to make some building blocks for our workflow.

")
