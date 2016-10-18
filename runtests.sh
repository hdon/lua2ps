#!/bin/bash
TEMPDIR=/tmp
eval `luarocks path`

if [ $# = 0 ] ; then
  TESTS=tests/*.lua
else
  TESTS=$@
fi

# Clean up previous test results
rm -f $TEMPDIR/lua2ps*

# Run test procedure for each test
NUMPASSED=0
NUMFAILED=0
for testscript in $TESTS ; do
  TESTNAME=`basename $testscript`

  echo
  echo Running test $TESTNAME
  # Run test with lua interpreter
  echo lua ./$testscript \
    \>  $TEMPDIR/lua2ps.$TESTNAME.stdout.lua \
    2\> $TEMPDIR/lua2ps.$TESTNAME.stderr.lua &&
  lua ./$testscript \
    >  $TEMPDIR/lua2ps.$TESTNAME.stdout.lua \
    2> $TEMPDIR/lua2ps.$TESTNAME.stderr.lua &&

  # Translate test to PostScript
  echo lua lua2ps.lua $testscript $TEMPDIR/lua2ps.$TESTNAME.ps \
    \>  $TEMPDIR/lua2ps.$TESTNAME.stdout.trans \
    2\> $TEMPDIR/lua2ps.$TESTNAME.stderr.trans &&
  lua lua2ps.lua $testscript $TEMPDIR/lua2ps.$TESTNAME.ps \
    >  $TEMPDIR/lua2ps.$TESTNAME.stdout.trans \
    2> $TEMPDIR/lua2ps.$TESTNAME.stderr.trans &&
  # Run the test in PostScript
  echo gs -sDEVICE=nullpage -q $TEMPDIR/lua2ps.$TESTNAME.ps \
    \>  $TEMPDIR/lua2ps.$TESTNAME.stdout.ps \
    2\> $TEMPDIR/lua2ps.$TESTNAME.stderr.ps &&
  gs -sDEVICE=nullpage -q $TEMPDIR/lua2ps.$TESTNAME.ps \
    >  $TEMPDIR/lua2ps.$TESTNAME.stdout.ps \
    2> $TEMPDIR/lua2ps.$TESTNAME.stderr.ps

  # Any error?
  PASSED=n
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
          PASSED=y
          echo [1\;32mTest passed![0m
        else
          echo $TEMPDIR/lua2ps.$TESTNAME.stdout.lua '(stdout from lua)'
          cat $TEMPDIR/lua2ps.$TESTNAME.stdout.lua
          echo $TEMPDIR/lua2ps.$TESTNAME.stdout.ps '(stdout from gs)'
          cat $TEMPDIR/lua2ps.$TESTNAME.stdout.ps
        fi
      fi
    fi
  fi
  if [ $PASSED = n ] ; then
    echo [1\;41mTest failed![0m
    NUMFAILED=$((NUMFAILED + 1))
  else
    NUMPASSED=$((NUMPASSED + 1))
  fi
  if [ -n "$SAVE_TEST_RESULTS" ] ; then
    echo -n $PASSED > "$testscript.passing.2"
  fi
done

echo Summary:
echo $NUMPASSED passed / $NUMFAILED failed
if [ -n "$SAVE_TEST_RESULTS" ] ; then
  for testscript in $TESTS ; do
    echo -n "$testscript "
    if [ -a "$testscript.passing" ] ; then
      R=$(cat "$testscript.passing")
      [ "$R" = y ] && echo -n [1\;32my[0m || echo -n [1\;41mn[0m
    else
      echo -n '-'
    fi
    echo -n ' '
    R=$(cat "$testscript.passing.2")
    [ "$R" = y ] && echo -n [1\;32my[0m || echo -n [1\;41mn[0m
    echo
    if [ -n "$OVERWRITE_TEST_RESULTS" ] ; then
      mv "$testscript.passing.2" "$testscript.passing"
    fi
  done
fi
