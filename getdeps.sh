#!/bin/bash
# These deps are pretty small
git clone https://github.com/lua/lpeg.git lpeg &&
pushd lpeg && 
make -f makefile &&
popd &&
git clone https://github.com/andremm/lua-parser.git lua-parser
# To run lua2ps remember these environs:
echo Dependencies got. Edit your runtests.sh with these environs:
echo export LUA_CPATH=\'`pwd`/lpeg/\?.so\'
echo export LUA_PATH=\'`pwd`/\?.lua\;`pwd`/lua-parser/\?.lua\'
