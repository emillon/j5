J5
==

A personal wiki system, based on Hakyll.

## Usage

    # put stuff with .mkd extensions under a wiki/ directory
    runhaskell j5.hs build

## WTF is it ?

`j5` is a simple wiki system. Basically, you write stuff in your favorite
text editor, and typing `make` will build a static website containing this
stuff. "Stuff" can contain
[Markdown](http://daringfireball.net/projects/markdown/) formatting, including
images, internal and external links.

## History

This is a rewrite of a former version [hacked together with
Perl](https://xkcd.com/224/) and Makefile logic. An [arrow-based rule
system](http://jaspervdj.be/hakyll/tutorials/03-arrows.html) is
more maintainable, I guess.

## Prerequisites

You will need :

  * A Haskell compiler. These days that probably means [the Haskell
    Platform](http://hackage.haskell.org/platform/)
  * [pandoc](http://github.com/jgm/pandoc), a document formatter
  * [hakyll](http://jaspervdj.be/hakyll/), a static website generator based on
    the former

Yes, Hakyll is part of J5. That [why it's called
J-5](http://www.discogs.com/artist/Dante+Givens) !

## Template system

There's a kind of template system, but it's a big work in progress. See
`Template.hs` if you're interested.

## Contributing

I hacked this small wiki 'app' to suit my needs. If you think it can be
improved, feel free to send me some patches !
