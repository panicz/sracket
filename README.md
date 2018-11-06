# SRACKET - SLAYER ported to Racket

This repository contains a very incomplete compatibility layer from
[SLAYER](http://puszcza.gnu.org.ua/software/slayer/) environment
for multimedia Scheme applications to the [Racket](https://racket-lang.org/)
drawing and event processing API.

## What for?

Some programmers report that SLAYER provides a nice and straigh-forward
API for multimedia applications, whereas Racket, while powerful, suffers
from a slight over-complication in its design, providing an advanced
and complicated Object-Oriented Programming language built on top of
Racket specifically for handling graphics.

On the other hand, unlike Racket, SLAYER is difficult to build on
many platforms because of various version dependencies.

## Yeah but still...

In particular, SRACKET provides enough capabilities for expressing
the solutions to the [Draggable Rectangle Challenge](https://eidolon-language.quora.com/Draggable-rectangle-challenge-part-I-the-introduction),
whose purpose is to explore the means that are sufficient for building
composable graphical applications.

## Explore, huh?

Don't use it if you don't need to. You most probably won't ever need this.
Now leave.

## OK, you've got my attention. What do I need to do?

Having some recent and decent version of [Racket](https://racket-lang.org/)
installed, just clone this repository, and from your terminal run,
for example:

    $ cd sracket
    $ racket 1.rkt

## Is that it?

The file "sracket.rkt" contains the whole framework. Check out, for example,
the contents of the file "1.rkt" to see how to use the framework (there are
some minor differences between SLAYER and SRACKET. Over time, I will try
to eliminate them, but I doubt if I ever try to provide any further documentation).

## Wow.

Yeah.
