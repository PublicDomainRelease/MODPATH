#!/bin/sh
# test.sh -- run modpath test data sets
#
# Usage: test.sh [start [stop]]
#        test.sh [start [stop] | tee test.out
#
#        where: start = starting test number
#                stop = ending test number (may be same as start)
#
# To use this script for another program in a different directory, globally
# change the program name (be sure to change both upper and lower case
# occurrences) and set the default value for the Stop variable as appropriate.
#
# History: 12/24/91, mblalock, initial coding
#          04/27/92, mygoze, restructuring
#          11/16/94, rsregan, modified for use with modpath/plot
#          06/16/00, mcrouse
#
# Variable definitions
# --------------------
  TOPDIR=..
  PROG=$TOPDIR/bin/mpath3
  PROG2=$TOPDIR/bin/mplot3
  DATA=$TOPDIR/data
  CHECK=./check.sh
  CLEAN=./clean.sh
  PROGNM=mpath
  PROGNM2=mplot
  END=8
  DIVD=========================================
#
  exec 2>&1                                # stderr shows up in .out file
  Start=${1:-1}                            # by default, start at 1
  Stop=${2:-$END}                          # by default, stop at $END
  if [ $Start -lt 1 ] ; then Start=1 ; fi
  if [ $Stop -lt 1 ] ; then Stop=$END ; fi
  if [ $Start -gt $END -o $Stop -gt $END ]
  then
    echo
    echo "Warning, invalid arguments--test range is 1 - $END for $PROGNM"
    echo "input arguments were $Start - $Stop"
    echo
    if [ $Stop -gt $END ] ; then Stop=$END ; fi
    if [ $Start -gt $END ]
    then
      echo "no $PROGNM tests will be performed"
    else
      echo "Tests $Start - $Stop will be performed"
    fi
    echo
  fi
#
# remove old output file
#
  $CLEAN
#
# begin test runs
#
  Test=$Start

  if [ $Test -ge $Start -a $Test -le $Stop ]
  then
    echo
    echo $DIVD$DIVD
    echo "Begin processing $PROGNM test runs $Start to $Stop"
    echo
    date
  fi

  while [ $Test -ge $Start -a $Test -le $Stop ]
  do
    cp -p $DATA/ibound.* .
    if [ $Test -lt 5 ] 
    then 
      type=s
      n=$Test
    else
      type=t
      n=`expr $Test - 4`
    fi
    echo
    echo $DIVD$DIVD
    echo "Test run number $Test"
    $PROG $DATA/path-$type$n.rsp
    mv summary.pth summary$Test.pth
    echo
    echo
    $PROG2 $DATA/plot-$type$n.rsp
    mv summary.plt summary$Test.plt
    mv mplot.ps mplot$Test.ps
    Test=`expr $Test + 1`
  done
  Test=`expr $Test - 1`
#
  if [ $Test -ge $Start -a $Test -le $Stop ]
  then
    echo
    echo
    echo $DIVD$DIVD
    echo "Completed $PROGNM test runs $Start to $Test"
    echo
  fi

# check output against original output in $DATA directory
  $CHECK summary mplot
