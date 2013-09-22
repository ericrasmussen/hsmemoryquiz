### hsmemoryquiz

`hsmemoryquiz` is a command-line utility for helping you test memory associations
using the Dominic system.


### Dominic System Overview

If you aren't familiar with the Dominic system, the best starting point is the
[MentatWiki](http://www.ludism.org/mentat/DominicSystem).

One goal of practicing the system is learning to memorize long
sequences of digits. There are many other uses for it, but it's a good
motivating case. We'll give some quick start steps below and then jump into
how `hsmemoryquiz` can help.


#### Memorize the basics

To become proficient at the Dominic system you need to first memorize the digit
to letter mapping:


| Digit | Letter |
|-------|--------|
| 0     | O      |
| 1     | A      |
| 2     | B      |
| 3     | C      |
| 4     | D      |
| 5     | E      |
| 6     | S      |
| 7     | G      |
| 8     | H      |
| 9     | N      |


Learn it well!


#### Memorizing digit pairs

Next, you'll need to associate every possible digit pair (00-99) with a
meaningful mental image. You can start by writing out a list of the digit
pairs, converting them to letter pairs, and thinking of an image to associate
with those letters.

Examples:

* 73 = GC = Grumpy Cat looking grumpy
* 93 = NC = Nyan Cat riding a rainbow

You can prepare to use `hsmemoryquiz` by creating a text file with entries in
the form:


```
# my_associations.txt
...
73: Grumpy Cat looking grumpy
...
93: Nyan Cat riding a rainbow
...
```

Ideally you want to make the images as vivid and memorable as possible for
*you*. Many people choose iconic characters from literature, celebrities,
cartoon characters, etc.

When you're first learning a system, having to memorize 100 items as a
pre-requisite can be daunting. `hsmemoryquiz` aims to alleviate some of that by
having you write out the images in a text file and then quizzing you on them
in different ways.


#### Composite images

It's worth noting that once you memorize the digit pairs, you can build
composite images by picturing the first entity performing the action of the
second. Continuing with the above example, I'd associate 7393 with Grumpy Cat
flying on a rainbow. If I needed to memorize 9373, I could picture Nyan Cat
making a grumpy face.

Once you know how to combine the pairs, there's not a lot of benefit to quizzing
yourself on them, so `hsmemoryquiz` doesn't have a feature for that. It only
helps you with memorizing the pairs. However, it's good to remember that one of
the benefits of learning the system is this ability to quickly associate four
digits with an image, so that memorizing, say, a 12-digit number is reduced to a
sequence of three composite images that you can learn to quickly decode.


### Installing hsmemoryquiz

These steps will eventually be simplified, but in the meantime, here's how to
get up and running:

* Install the [Haskell Platform](http://www.haskell.org/platform/)
* $ git clone https://github.com/ericrasmussen/hsmemoryquiz.git
* $ cd hsmemoryquiz
* $ cabal install
* Test that it works with: $ hsmemoryquiz --help


### Using hsmemoryquiz

You'll first need to create a file of your memory associations, using the
format:

```
00: my association for OO
01: my association for OA
...
99: my association for NN
```

Each line may contain only one association (it is not valid to have a newline
character in the association text).

In the root of this repository I have a file named dominic_sample.txt, comprised
of computer scientists (mostly borrowed from [this
page](https://en.wikipedia.org/wiki/List_of_computer_scientists) on Wikipedia).

To be clear: I don't recommend you try to memorize this list. That would only
help you memorize computer scientists. The goal is to fill it in with images
you find memorable, funny, or inspiring. But our sample file is convenient
for testing out the application.

It looks a little like this:

```
# dominic_sample.txt
...
44: Dorothy E. Denning
45: Douglas Engelbart
46: Dana Scott
47: David Gelernter
...
```

`hsmemoryquiz` can now read in that file and project the associations as digits,
letters, or mnemonics (the text part of your memory association). For instance,
to have the program quiz you on letters but have you answer in the form of
mnemonics, use these flags:

```console
$ hsmemoryquiz --from=letters --to=mnemonics --path=dominic_sample.txt
Welcome! Quit at any time with ":q" or by pressing ctrl-c
> HC:
We were looking for: Haskell Curry
> BD:
Caught: interrupt
Final score: 0/1 (0%)
```

You can also use the short flags:

```$ hsmemoryquiz -f letters -t mnemonics -p dominic_sample.txt```

By default you will be quizzed on questions at random, but you can use the -i or
--index flag with the arguments "ordered" (questions asked sequentially based on
their ordering in your text file), "reversed" (questions asked in reverse based
on the ordering in your text file), or the default, "random".

You can quit by pressing ctrl+c or typing ":q" at the prompt. The console
will also attempt to catch other exceptions and still print the final score,
such as the EOF exception (typically ctrl+d on *nix or ctrl+z for Windows).


#### Scoring

This program is just for fun and there are many ways you could contrive to cheat
yourself. Using the example 32 = CB = Charlie Brown, here is a breakdown of how
your answers will be checked based on the answer type:

| Answer Type | Methodology                                                    |
|-------------|----------------------------------------------------------------|
| digits      | requires both digits (ex. "> CB: 32")                           |
| letters     | requires both letters, case insensitive (ex. "> 32: cB")       |
| mnemonics   | requires at least 3 characters from the string, case insensitive (ex. "> CB: charlie")|


### Disclaimer

I'm not endorsing the Dominic system or recommending people learn it. If you do
wish to learn it, please look up online resources and check out available books
by its creator, Dominic O'Brien.

It's fun to learn memory tricks, but remember that, like exercise equipment,
they only work if you stay active with them. There's also no practical benefit
for many of us to remember large numbers (especially if you have a mobile device
to store everything!), but this system is useful in other ways too. In
particular, it has been used as the basis for a memory palace called "Hotel
Dominic". You can read more at
[MemoriseThis!](http://www.memorisethis.co.uk/techniques/images/dominic)