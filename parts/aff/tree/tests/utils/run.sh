#!/bin/bash
export RUN=$TOP/utils/lhpc-aff
export TEST_DIR=$TOP/tests/utils
if [[ $WKDIR == "" ]] ; then export WKDIR=. ; fi
export DIFF="diff -q"
export LOG="run.log"

LS=/bin/ls
BASH=/bin/bash

date >$LOG

for N in $($LS $TEST_DIR |grep '^test-')
	do echo -en "$N\t"
	if $BASH $TEST_DIR/$N
		then echo OK
		else echo failed ; exit 1
	fi
done
