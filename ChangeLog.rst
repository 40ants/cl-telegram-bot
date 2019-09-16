

* Added a dependency from trivial-timeout and now connect timeout is used when
  doing requests to API.

Refactorings
============

Project was broken down to subpackages, nicknames ``telegram-bot`` and
``tg-bot`` were removed, because now system uses ASDF's
package-inferred-system class and each file have it's own separate packages.
