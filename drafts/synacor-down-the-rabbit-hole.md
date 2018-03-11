---
title: Synacor - Down the Rabbit Hole
date: 2017-12-26
logo: haskell
lang: en
---

This year I had the chance to participate in the 2017 edition of Advent of Code.
That was the first time and I *really* enjoyed it. Having to solve an
interesting problem, every day, made me learn a lot! It's also a good occasion
to practice some skills that might not be otherwise useful in your daily job
(and more importantly: have fun!).

While skimming through the discussions on Reddit about possible solutions to one
of the AoC's problems, someone mentioned "Synacor"... I never heard of it
before, but from what I understood it was some kind of programming challenge. I
did not need more to have a look:
[challenge.synacor.com](https://challenge.synacor.com).

The challenge is briefly introduced, the competition took place during two
conferences in the past and is now over. But you can still download the material
of the challenge and try to solve it.

After registering an email address and a password, I could download an archive:
`synacor-challenge.tgz`. Uncompressed, it contains two files:

1. arch-spec
2. challenge.bin

The first one describes the challenge. It is about creating "a virtual machine
capable of running the included binary [challenge.bin]". It also mentions that
some "codes" are to be found along the way. The rest of the document describes
the architecture of the virtual machine:

* 16 bits integers stored in little-endian format
* 16 bits addressable memory (the program starts at address `0`)
* 8 registers
* 1 stack containing 16 bits numbers
* 22 instructions: `halt`, `noop`, `set`, `push`, `jpm`, `add`, `mult`, etc.

Each instruction is described in details: opcode, number of expected
arguments and behavior when executed by the virtual machine. So
far so good, this looks pretty classic. The implementation of such
virtual machine is pretty straight forward. I choose `Haskell` for
the implementation (because why not, and I already implemented some
[Brainfuck interpreter/transpiler](https://github.com/remusao/Hodor)
before, so I thought I could reuse some learnings from there). Without going
into too much details, I used the following data structures to represent the
state of the VM:

```haskell
data Synacor = Synacor
  { pos :: Int
  , mem :: Map Register Word16
  , stack :: [Word16]
  , program :: Vector Word16
  }
```

After implementing most of the instructions, I tried to run the program:

> Welcome to the Synacor Challenge!
> Please record your progress by putting codes like
> this one into the challenge website: XXXXXXXXXXX
> 
> Executing self-test...

Nice, so the program actually starts by *testing the virtual machine*, to make
sure all instructions are implemented properly. It's pretty cool. It means that
you cannot proceed until your VM is fully functional (Using Haskell is a good
start!). After a few iterations of trial, error, fix, I completed my
implementation until...

> self-test complete, all tests pass
> The self-test completion code is: XXXXXXXXXXXX

Youpi! All good... but, wait, something else was displayed on the terminal:

> == Foothills ==
> You find yourself standing at the base of an enormous mountain. At its
> base to the north, there is a massive doorway. A sign nearby reads
> "Keep out! Definitely no treasure within!"
>
> Things of interest here:
> - tablet
> 
> There are 2 exits:
> - doorway
> - south
>
> What do you do?

Mind blown. The program you are running is actually a terminal-based
RPG! How cool is that? Well, very cool, and very smart. I clearly did
not expect that from the challenge. It's a bit like trying to open a
chess (the chess is a program and the virtual machine is the key) and
then you discover there is another riddle in the chess. So what's next?
Well play the game I guess...

I played for a bit, and discovered one more code in the game (Did I
already mention how cool all this is?). Which means some (all?) codes
can be found by playing the game. But then, how do I know if all codes
are in the game? How deep and rich can this game be? What if it takes 10
hours to find the codes? I already under-estimated the challenge once,
let's not do the same mistake again.

At this point I thought of two things that could be done:

1. Try to extract all text from the file `challenge.bin` and see if it contains
   any code.
2. Modify the VM so that it can play the game by itself, and explore all
   possible paths possible, some kind of depth-first search on the game itself
   in a way.
3. Disassemble the program and try to understand its structure. The codes must
   be stored somewhere (or generated somehow), by understanding (and maybe
   changing a bit) the code, it must be possible to bypass the game completely.

