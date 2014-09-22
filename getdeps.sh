#!/bin/bash
# These deps are pretty small
git clone https://github.com/lua/lpeg.git lpeg
git clone https://github.com/andremm/lua-parser.git lua-parser
# To run lua2ps remember these environs:
echo Dependencies got. Edit your runtests.sh with these environs:
LUA_CPATH='/mnt/oih/hdon/src/hg/lua2ps/lpeg/?.so'
LUA_PATH='/mnt/oih/hdon/src/hg/lua2ps/?.lua;/mnt/oih/hdon/src/hg/lua2ps/lua-parser/?.lua'
