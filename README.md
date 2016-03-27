# Para

*Work in progress. Not usable right now.*

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/mrkkrp/para.svg?branch=master)](https://travis-ci.org/mrkkrp/para)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/para/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/para?branch=master)

This package allows to deal with pairs efficiently. It provides the
following:

* Functions for navigation in and manipulation of S-expressions. These
  include:

    * `para-beginning-of-sexp`
    * `para-end-of-sexp`
    * `para-down-sexp`
    * `para-up-sexp`
    * `para-backward-down-sexp`
    * `para-backward-up-sexp`
    * `para-forward-sexp`
    * `para-backward-sexp`
    * `para-next-sexp`
    * `para-previous-sexp`
    * `para-backward-unwrap-sexp`
    * `para-unwrap-sexp`
    * `para-forward-slurp-sexp`
    * `para-forward-barf-sexp`
    * `para-backward-slurp-sexp`
    * `para-backward-barf-sexp`
    * `para-transpose-sexp`
    * `para-kill-sexp`
    * `para-backward-kill-sexp`
    * `para-kill-hybrid-sexp`
    * `para-select-next-sexp`
    * `para-select-previous-sexp`

* Automatic insertion of closing pair. Para *does not* skip closing pair if
  it already exists, it inserts closing pair if you press key corresponding
  to it. It's easy to get into the habit of moving cursor one character
  forward instead of trying to insert already inserted character. This
  feature makes the package stateless, this is one of aims of this project.

* Automatic deletion of complete pairs. If cursor is placed behind whole
  pair and you press backspace, closing pair is deleted, as usual. If cursor
  is between paired characters and there is no text between them, only in
  that case deleting of opening pair will delete the closing pair as
  well. Otherwise opening pair is deleted as usual.

* Wrapping of active region. If you have active region and enter paired
  character the region is wrapped. Due to certain confusion that this
  feature can lead to (if you prefer to delete selected text when new text
  is typed), only single-character pairs work this way.

* Some pairs, like simple single quote which often serves as apostrophe and
  can be part of identifiers in some languages (Haskell), are handled
  intelligently. When it can be part of word or identifier it's not
  automatically paired. You can give this behavior to other pairs, since the
  package is quite flexible, but default configuration should be OK in most
  cases.

* The package can work with multi-character pairs and pairs where opening
  and closing sequences are identical.

However, Para is not a clone of Smartparens. It's much more clean,
minimalistic, and fast. In my opinion packages for basic editing should be
simple, robust, and they should not ever let you down. To achieve these
goals Para adheres to the following principles:

* No state in your editing. Every combination of user's actions always
  result in the same thing no matter what manipulations user performed
  before it.

* No useless flashy things. Overlays in basic editing? I don't get it.

* Clear API for users. To define a pair you need to specify exactly two
  things: opening pair and closing pair. You can specify when this pair will
  be active too, but it's a different thing.

* Clean design from the very beginning and a lot of tests covering
  *everything*.

* Does not increase typing latency. Some packages use hooks that are
  executed after every key press and put heavy code there, and then it sums
  up and you get laggy text editor. It can get slower than IDE — certainly
  we can do better.

## What's wrong with Smartparens?

I was user of [Smartparens](https://github.com/Fuco1/smartparens) for about
two years. Never liked it because of its technical debt and overlays, but
couldn't find decent replacement.

I really tried several times to avoid writing this (`para`) package. I
suspended development several times thinking that Smartpares is getting
better. However, recently I had very annoying problems with typing latency
that frustrated me to the point that I was close to switch to Atom text
editor after years of Emacs. I tracked the latency problem down to two main
sources of delay: Flyspell and Smartparens. I've fixed the former with help
of [`flyspell-lazy`](https://github.com/rolandwalker/flyspell-lazy), which
works like a charm, but I see no solution to Smartparens problem except for
writing an alternative package.

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

Para (пара, “а” sounds as “u” in “but”) means “pair” in Russian. The word is
a good name for a package because it's quite unique and short and the same
time.

## License

Copyright © 2015–2016 Mark Karpov

Distributed under GNU GPL, version 3.
