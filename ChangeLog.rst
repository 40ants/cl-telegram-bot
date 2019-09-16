===========
 ChangeLog
===========

0.2.0 (2019-09-16)
===================

* Added a dependency from trivial-timeout and now connect timeout is used when
  doing requests to API.
* Function ``make-<bot-class>`` now proxie any parameters to the class's constructor.

0.1.0
=====

Refactorings
------------

Project was broken down to subpackages, nicknames ``telegram-bot`` and
``tg-bot`` were removed, because now system uses ASDF's
package-inferred-system class and each file have it's own separate packages.
