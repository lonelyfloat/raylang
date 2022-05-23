# raylang
Language of the Cult of Raylib

I noticed that there aren't any programming languages dedicated to praising raylib, the best game development library. I have rectified that issue.
The language is actually just a clone of Brainfuck, but all the commands are raylib. 

Here are all the commands - 

```
ray - data pointer increment
lib - data pointer decrement
raylib - increment value at pointer
libray - decrement value at pointer
rayray -  looping start
liblib - looping end
libraylib - input byte at pointer
raylibray - output byte at pointer
```

All other text is comments.

This language is really more of a test to see if I could write an interpreter in Haskell.
To use the language, run the executable at `bin/raylang` from the terminal, and pass in the relative directory of a source file.
An adding program is also in the `bin/` directory, so to run that program all you need to do is run the command `raylang test/add.rl`.
