#! /bin/sh
#
#  check.sh:  differential file comparator (diff) run to compare original
#             output files against newly created output files
#
# output file names must follow format "nameX.sufx" where:  "name" is
# "test" by default or can be supplied as argument(s) to check.sh; all
# numbers X must appear in "for Test" loop list below; all suffixes must 
# appear in "for Sufx" loop list below
#
#  04/28/92, mygoze, initial coding
#  06/10/92, mygoze, added in NameList, Name, Sufx variables and "for Name",
#                    "for Sufx" loops to make check.sh more versatile 

DIVD=----------------------------------------
TOPDIR=..
DATA=$TOPDIR/data

# set list of names of files to be tested; name will be "test" by default
NameList=${@:-test}

# delete old output file  
if [ -f check.log ]; then rm check.log; fi

for Name in $NameList
do
  for Test in 1 2 3 4 5 6 7 8
  do
    for Sufx in pth plt ps
    do
      if [ -f $DATA/$Name$Test.$Sufx -a -f $Name$Test.$Sufx ]
      then
      # do comparison only if both orig. and new output files exist
        echo $DIVD$DIVD | tee -a check.log
        echo "comparison of $DATA/$Name$Test.$Sufx with $Name$Test.$Sufx" \
             | tee -a check.log

        if diff -w $DATA/$Name$Test.$Sufx $Name$Test.$Sufx >> check.log
        then
          echo FILES ARE IDENTICAL | tee -a check.log
        else
          echo FILES DIFFER:  see file check.log for differences
        fi
      fi
    done
  done
done
for Name in endpoint pathline timesers contours paths
do
  for Sufx in s1 s2 s3 s4 t1 t2 t3 t4 dcf 
  do
    if [ -f $DATA/$Name.$Sufx -a -f $Name.$Sufx ]
    then
    # do comparison only if both orig. and new output files exist
      echo $DIVD$DIVD | tee -a check.log
      echo "comparison of $DATA/$Name.$Sufx with $Name.$Sufx" \
           | tee -a check.log

      if diff -w $DATA/$Name.$Sufx $Name.$Sufx >> check.log
      then
        echo FILES ARE IDENTICAL | tee -a check.log
      else
        echo FILES DIFFER:  see file check.log for differences
      fi
    fi
  done
done
