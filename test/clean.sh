#! /bin/sh
# clean.sh - clean up files after test runs
# Usage: clean.sh
# History: 00/06/16 mcrouse

# remove output from each test
rm -f summary[1-8].pth summary[1-8].plt mplot[1-8].ps
rm -f endpoint.[st][1-4]
rm -f pathline.[st][13]
rm -f timesers.s4
rm -f contours.dcf paths.dcf

rm -f ibound.[14]
rm -f check.log errors.gks mplot.log mpsearch
