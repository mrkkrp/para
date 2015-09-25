# Para

*Work in progress. Not usable right now, but you can read a rant.*

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/mrkkrp/para.svg?branch=master)](https://travis-ci.org/mrkkrp/para)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/para/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/para?branch=master)

This package allows to deal with pairs efficiently. It provides the
following:

* Functions for navigation in and manipulation of S-expressions. These
  include:

    * `para-beginning-of`
    * `para-end-of`
    * `para-down`
    * `para-up`
    * `para-backward-down`
    * `para-backward-up`
    * `para-forward`
    * `para-backward`
    * `para-next`
    * `para-previous`
    * `para-backward-unwrap`
    * `para-unwrap`
    * `para-forward-slurp`
    * `para-forward-barf`
    * `para-backward-slurp`
    * `para-backward-barf`
    * `para-transpose`
    * `para-kill`
    * `para-kill-hybrid`
    * `para-select-next`
    * `para-select-previous`

* Automatic insertion of closing pair. Para *does not* skip closing pair if
  it already exists, it inserts closing pair if you press key corresponding
  to it. It's easy to get into the habit of moving cursor one character
  forward instead of trying to insert already inserted character. This
  feature makes the package simpler, this is one of aims of this project.

* Automatic deletion of complete pairs. If cursor is placed behind whole
  pair and you press backspace, closing pair is deleted, as usual. If cursor
  is between paired characters and there is no text between them, only in
  that case deleting of opening pair will delete the closing pair as
  well. Otherwise opening pair is deleted as usual.

* Wrapping of active region. If you have active region and enter paired
  character the region is wrapped and mark is deactivated. Due to certain
  confusion that this feature can lead to (if you prefer to delete selected
  text when new text is typed), only single-character pairs work this way.

* Some pairs, like simple single quote which often serves as apostrophe and
  can be part of identifiers in some languages (Haskell), are handled
  intelligently. When it can be part of word or identifier it's not
  automatically paired. You can give this behavior to other pairs, since the
  package is quite flexible, but default configuration should be OK for 99%
  of users.

* The package can work with multi-character pairs and pairs where opening
  and closing sequences are identical.

However, Para is not a clone of Smartparens. It's much more clean and
minimalistic. My opinion is that packages for basic editing should be
simple, robust, and they should not ever let you down. Every such package
should be like a hammer. To achieve these goals Para adheres to the
following principles:

* No state in your editing. Every combination of user's actions always
  result in the same thing.

* No useless flashy things. Overlays in basic editing? Forget it.

* Clear API for users. To define a pair you need to specify exactly two
  things: opening pair and closing pair. You can specify when this pair will
  be active too, but it's a different thing.

* Clean design from the very beginning and a lot of tests covering
  *everything*.

* Situations when something doesn't work are rare even if you use (normal,
  non-stable) MELPA. See more about this in the next section if you want
  philosophy.

## What's wrong with Smartparens?

I was user of [Smartparens](https://github.com/Fuco1/smartparens) for about
a year. Never liked it, but couldn't find decent replacement. I can imagine
it was better at the beginning when the author had more time to work on
it. But with right approach and design maintenance doesn't take much time.

Most Emacs Lisp packages are brilliant, from all packages I currently use I
don't like only two: Haskell Mode (which should be rewritten from scratch
for sure and then be maintained by a dictator to avoid
“[software rot](https://en.wikipedia.org/wiki/Software_rot)”) and
Smartparens.

So what the problem with this package? In my opinion it's bloated, or in
other words it tries to do too much. This is in conjunction with the fact
that it needs more energy from contributors/maintainer to remain in good
state. This is free software and it's mostly unpaid, but if you start doing
something, do it well. If you can estimate your abilities, it's easy to keep
it usable even when you have no time (e.g. don't add new features, just fix
bugs).

When I first tried to define my own pair I found API a bit convoluted. I
tried to follow documentation but it didn't worked. I expressed my confusion
in an issue. Then I was told that my approach was correct but package was
not working for some reason. Well, these things happen even with
state-of-art Magit. We are using packages built directly from their
repositories, so this is normal. Another thing is not normal: these problems
hang and they are not fixed.

Packages that are built directly from repo have all reasons to practice
[Kaizen](https://en.wikipedia.org/wiki/Kaizen): many small improvements
resulting in steady, continuous improvement.

What Smartparens's doing is described in “Broken Window Theory” mentioned in
“Pragmatic Programmer” and other books and articles:

> One broken window, left unrepaired for any substantial length of time,
> instills in the inhabitants of the building a sense of abandonment — a
> sense that the powers that be don't care about the building. So another
> window gets broken. People start littering. Graffiti appears. Serious
> structural damage begins. In a relatively short space of time, the
> building becomes damaged beyond the owner's desire to fix it, and the
> sense of abandonment becomes reality.

Then the book proposes a tip: “Don't Live with Broken Windows”.

Author of Smartparens says that we should use MELPA Stable instead and the
working branch may be broken in “silly ways”. Why should it ever get into
this state for any substantial period of time? It will be even harder to
completely “repair” it in future if you allow it now. MELPA Stable is an
excuse, this does not solve the problem because most of us cannot and do not
use MELPA Stable and I would like to see working setup that combines MELPA
and MELPA Stable because most of the packages are perfectly usable when
built from repositories directly. Obviously they follow Kaizen.

Maybe it's not that bad. But it's bad enough for me to spend a couple of
days and a bit of my mental energy to write this package so I can be
confident in every package I use.

## Installation

If you would like to install the package manually, download or clone it and
put on Emacs' `load-path`, then you can require it in your init file like
this:

```emacs-lisp
(require 'para)
```

## Usage

Coming soon…

## Customization

Coming soon…

## Why such name?

Para (пара) means “pair” in Russian. The word is a good name for a package
because it's quite unique and short and the same time.

## License

Copyright © 2015 Mark Karpov

Distributed under GNU GPL, version 3.
