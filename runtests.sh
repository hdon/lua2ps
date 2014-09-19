#!/bin/bash
TEMPDIR=/tmp
export LUA_CPATH='/mnt/oih/hdon/src/hg/lua2ps/lpeg/?.so'
export LUA_PATH='/mnt/oih/hdon/src/hg/lua2ps/lua-parser/?.lua'

# Clean up previous test results
rm -f $TEMPDIR/lua2ps*

# Run test procedure for each test
for testscript in tests/* ; do
  TESTNAME=`basename $testscript`

  echo
  echo Running test $TESTNAME
  # Run test with lua interpreter
  lua ./$testscript \
    >  $TEMPDIR/lua2ps.$TESTNAME.stdout.lua \
    2> $TEMPDIR/lua2ps.$TESTNAME.stderr.lua &&

  # Translate test to PostScript
  lua lua2ps.lua $testscript $TEMPDIR/lua2ps.$TESTNAME.ps \
    >  $TEMPDIR/lua2ps.$TESTNAME.stdout.trans \
    2> $TEMPDIR/lua2ps.$TESTNAME.stderr.trans &&
  # Run the test in PostScript
  gs -sDEVICE=nullpage -q $TEMPDIR/lua2ps.$TESTNAME.ps \
    >  $TEMPDIR/lua2ps.$TESTNAME.stdout.ps \
    2> $TEMPDIR/lua2ps.$TESTNAME.stderr.ps

  # Any error?
  if [ -s $TEMPDIR/lua2ps.$TESTNAME.stderr.lua ] ; then
    echo 'stderr from lua'
    cat $TEMPDIR/lua2ps.$TESTNAME.stderr.lua
  else
    if [ -s $TEMPDIR/lua2ps.$TESTNAME.stderr.trans ] ; then
      echo 'stderr from lua2ps'
      cat $TEMPDIR/lua2ps.$TESTNAME.stderr.trans
    else
      if [ -s $TEMPDIR/lua2ps.$TESTNAME.stderr.ps ] ; then
        echo 'stderr from gs'
        cat $TEMPDIR/lua2ps.$TESTNAME.stderr.ps
      else
        # Nothing on stderr! Compare results!
        if cmp -s $TEMPDIR/lua2ps.$TESTNAME.stdout.lua $TEMPDIR/lua2ps.$TESTNAME.stdout.ps ; then
          echo Test passed!
        else
          echo Test failed!
          echo 'stdout from lua'
          cat $TEMPDIR/lua2ps.$TESTNAME.stdout.lua
          echo 'stdout from gs'
          cat $TEMPDIR/lua2ps.$TESTNAME.stdout.ps
        fi
      fi
    fi
  fi

done
