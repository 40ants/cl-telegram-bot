===========
 ChangeLog
===========

0.3.0 (2019-09-22)
==================

* Bot was fixed to use latest Dexador with support
  of ``read-timeout`` and ``connect-timeout``.

0.2.0 (2019-09-16)
===================

* Added a dependency from trivial-timeout and now connect timeout is used when
  doing requests to API.
* Function ``make-<bot-class>`` now proxie any parameters to the class's constructor.
* Now function ``stop-processing`` checks if thread is alive before destroying it.

0.1.0
=====

Refactorings
------------

Project was broken down to subpackages, nicknames ``telegram-bot`` and
``tg-bot`` were removed, because now system uses ASDF's
package-inferred-system class and each file have it's own separate packages.
