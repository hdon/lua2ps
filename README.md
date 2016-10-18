> You see things as they are; and you ask, "Why?"
> But I dream of things that never were; and I ask, "Why not?"

-- George Bernard Shaw

lua2ps
======

`lua2ps` parses and consumes [Lua](http://www.lua.org/) 5.1 source code and produces its equivalent in [PostScript](http://www.adobe.com/products/postscript/).

It is very nearly capable of running [luaqrcode](https://github.com/speedata/luaqrcode).

## Why create such a thing?

I hadn't had much experience with the Lua programming language, and zero experience with PostScript. I thought this would be a great way to learn each of them in intricate detail, and [software transformation](http://en.wikipedia.org/wiki/Program_transformation) has been a long-time favorite of mine.

## Dependencies?

 * Lua 5.1
 * Ghostscript 9.x
 * Andre Murbach Maidl's [lua-parser](https://github.com/andremm/lua-parser.git)
 * Craig Barnes' [lpeg](https://github.com/lua/lpeg)

To get lua-parser and lpeg with luarocks (preferred) simply use:

`luarocks install --local lua-parser`

## How do I use it?

Well, you probably don't want to. I can't imagine any reason why anyone would want to write Lua code and have it interpreted by a PostScript interpreter. Perhaps you'd like to create PostScript documents using a newer programming language with infix notation; modularity; functional and object-oriented programming; closure scopes; dynamic arrays, hash tables, and strings as primitive types; first-class functions; variadic argument lists, and other popular and elegant features. It is [not without its flaws](http://stackoverflow.com/questions/2785704/why-do-lua-arraystables-start-at-1-instead-of-0) but it isn't bad.

In contrast, PostScript is an old stack machine language, with dozens of primitive types, and reads like [RPN](http://en.wikipedia.org/wiki/Reverse_Polish_notation). It has hash tables, strings, and arrays, but their backing storage must dimensioned *manually*. PostScript is overspecialized in the extreme. Nobody programs PostScript by hand, but it was once a popular output format describing a paper document, either for a physical printer or an on-screen renderer.

Setting this aside, you can clone the repo, and run all 29 tests (of which 26 currently pass!) with `./runtests.sh`. This will require a UNIX or Linux operating system and a copy of the [GhostScript](http://www.ghostscript.com/) PostScript interpreter.
