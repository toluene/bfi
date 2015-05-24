BFI - A brainfuck REPL
===================

Building
-------

    cabal configure
    cabal build

Using
-----
Things you can specify:
- A file to be interpreted (optional if entering REPL)
- A Tape to start with, using the syntax -t ([y,y,y], [x,x,x]) where the first element of the second list is the current value, the left list the values before and the rest of the second list the values after

Syntax:

    bfi [-it] [file]
    -i                --interactive         Invoke REPL
    -t "(left, right)"  --tape="(left, right)"  Start with tape (left, right)

Options inside the REPL
--------------------
`:q`	Exit
`:t`	Print tape
`:c`	Print column
`:i`	Clear column
`:h`	Display help

