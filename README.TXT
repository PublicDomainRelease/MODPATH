README.TXT

                                MODPATH

          A particle-tracking postprocessing model for MODFLOW

                 MODPATH - Version 3.2  2000/06/14
                 MODPATH-PLOT - Version 3.2  2000/06/14

Instructions for installation, execution, and testing are provided
below.  After installation, see the modpath.txt file in the doc directory
of the MODPATH installation for summary information on MODPATH.

For assistance, enhancement requests, or to report bugs, contact the
Hydrologic Analysis Software Support Program by sending e-mail to
h2osoft@usgs.gov.


                          TABLE OF CONTENTS

                          A. DISTRIBUTION FILES
                          B. DOCUMENTATION
                          C. EXTRACTING FILES
                          D. COMPILING (optional)
                          E. INSTALLING
                          F. RUNNING THE PROGRAM
                          G. TESTING
                          H. CONTACTS


A. DISTRIBUTION FILES

The following distribution packages (containing the software, test data sets,
and information files) are currently available for UNIX systems:

  mpath3.2.Solaris.tar.gz - Compiled for Sun UltraSPARC-II under Solaris 2.6
  mpath3.2.source.tar.gz  - Source code only


B. DOCUMENTATION

Pollock, D.W., 1994, User's Guide for MODPATH/MODPATH-PLOT, Version 3: 
   A particle tracking post-processing package for MODFLOW, the
   U.S. Geological Survey finite-difference ground-water flow model:
   U.S. Geological Survey Open-File Report 94-464, 234 p.

   This document is available in electronic format.  A Portable Document
   Format (PDF) version is included in the doc subdirectory of the MODPATH
   program distribution.  This and other formats can also be found at
   http://water.usgs.gov/software/modpath.html.

See http://water.usgs.gov/software/ordering_documentation.html for 
information on ordering printed copies of USGS publications.


C. EXTRACTING FILES

Compressed tar files are used to distribute the source code and versions
of the software compiled for selected UNIX operating systems.  All of
the MODPATH files needed to install and test the program are contained in
the file mpath3.2.OS.tar.gz (where OS is a string indicating the intended
operating system).  If there is not a tar file for your operating
system or you want to compile the software, the source version of the
tar file contains all of the MODPATH files needed to compile and install
the program on a UNIX-based computer.  For all of these distributions,
the directory mpath3.2 is created (or overwritten) when the files are
extracted from the tar file; if this directory already exists, you may
want to delete or rename it before extracting the files.

Follow the steps below to extract the files from a distribution tar file.
The steps assume that MODPATH will be installed under /usr/opt/wrdapp,
where the wrdapp directory may be a separate file system mounted at
/usr/opt/wrdapp (wrdapp stands for Water Resources Division application).
You may install the software in another location, but you must change
each occurrence of "/usr/opt/wrdapp" in all the following instructions
to the alternate location that you choose.

  Steps in extracting files                 explanation
  ----------------------------------------  -----------------------------------
  mv mpath3.2.____.tar.gz /usr/opt/wrdapp   If the tar file is not already in
                                            the directory where you want the
                                            distribution installed, move it
                                            there.

  cd /usr/opt/wrdapp                        If you are not in the directory
                                            where the tar file is located, go
                                            there.

  gunzip mpath3.2.____.tar.gz               Uncompress the distribution file.

  tar -xvpof mpath3.2.____.tar              Extract files from the tar file.

This creates the following directory structure (the contents of each
directory are shown to the right):

   mpath3.2            file NOTICE.TXT and this README.TXT
     `-----bin         compiled executables
     `-----bin_data    data files required during execution
     `-----doc         documentation files
     `-----src         Makefile (and, with source tar, the source code)
     `-----test        scripts to run verification tests
     `-----data        standard data sets used in verification tests

Notes:  a) The bin and bin_data subdirectories are not included in the
           source distribution; they are created during compilation.
        b) Source code is included only with the source distribution.
        c) It is recommended that no user files be kept in the MODPATH
           directory structure.  If you plan to put files in the MODPATH
           directory structure, do so only by creating subdirectories.
        d) To compile a new version of the software, you will also need:
           (1) a Fortran compiler and (2) a Graphical Kernel System
           (GKS) library.


D. COMPILING (optional)

If you have retrieved a pre-compiled distribution of the software, skip to
the Installing section below.

If a compiled version of the software is not available for your computer
or you want to build the executable yourself, follow the instructions in
this section.  The source distribution is provided for those users' who
want the source code.  Little or no support can be provided for users
generating their own versions of the software.  In general, to compile a
new version of the software, you will need:

        a) a Fortran compiler,
        b) a minimal level of knowledge of Make, the compiler, and
           the UNIX operating system, and
        c) a Graphical Kernel System (GKS) library; GKS libraries
           available without fee include GLI/GKS (available from
           http://iff001.iff.kfa-juelich.de/gli/) and xgks (available
           as the file ftp://unidata.ucar.edu/pub/xgks/xgks-2.5.5.tar.Z).

As provided in the source distribution, the software is set up to be
compiled under Solaris.

Note that MODPATH and MODPATH-PLOT are written primarily in standard
Fortran 77 but that a few Fortran 90 statements have been added for
dynamic array allocation.  The code can be compiled with any standard
Fortran 90 compiler.  The code can also be compiled using a standard
Fortran 77 compiler when alternate Fortran 77 statements for static
array dimensioning are uncommented:  in mpath3.f and mplot3.f, uncomment
the statements found between the "#F77 - BEGIN#" and "#F77 - END#"
comments and comment out the statements between the "#F90 - BEGIN#"
and "#F90 - END#" comments.

To generate new executables, do the following:

1.  Change directory to the source directory:
        cd mpath3.2/src

2.  Modify the beginning of the file named Makefile to correctly specify 
    system-dependent variables:
     
        LGks         GKS library linker options
        GksObj       GKS-implementation-specific routines
        FC           Fortran compiler name
        FFLAGS       Fortran compiler flags

3.  Use the make program to initiate compilation of the source code and
    installation of the software:
        make [BINDIR=directory_for_links]

    where the optional BINDIR variable is used to specify a directory that
    is already included in each user's PATH.

    The above make command will:

        a.  create the MODPATH subdirectories bin and bin_data, if
            they do not already exist,
        b.  compile the source code, and
        c.  place the program executables in the MODPATH bin directory.


E. INSTALLING

To make the executables (mpath3 and mplot3) easy to use, a link to the
executables should be placed in a directory that is included in each
user's search path.  Run make in the MODPATH src subdirectory to create
the link:

    make install BINDIR=directory_for_links

A link to the executables will be placed in the directory assigned to
BINDIR.  For example, if each user's search path consists of

    /usr/bin:/usr/opt/bin:/usr/local/bin

using the command

    make install BINDIR=/usr/local/bin

will make the executables accessible from any directory without requiring
the full pathname of the executables.  Note that to create and delete
links to the executables, the installer must have sufficient rights to
the BINDIR directory.


F. RUNNING THE PROGRAM

To execute MODPATH type one of the following commands:
     mpath3 i
     mpath3 filename
     mpath3

  where
     i = the letter "i", short for interactive mode.  MODPATH prompts for
         input in the interactive mode.  Each interactive execution 
         produces a "response file" that records the prompts and responses
         from the execution.  You can make subsequent MODPATH executions 
         that read data from response files generated by previous 
         interactive executions.

     filename = the name of an existing "response file."
   
     If no option is given, MODPATH prompts for the name of a "response
     file".  If none is given, the program proceeds in the interactive mode.

MODPATH-PLOT is executed in a similar way to MODPATH using one of the
following commands:
     mplot3 i
     mplot3 filename
     mplot3

Note that the files device.dat and gks.dat in the bin_data directory must
be modified to match the GKS software linked to MODPATH-PLOT.  By default
they are set to match the configuration used under Solaris with GLI GKS.
Alternate versions of these files are also included for use with XGKS.
However, note that the only output type available with XGKS is X11
display output.


G. TESTING

Test data sets are provided to verify that the program is correctly installed
and running on the system.  The tests may also be looked at as examples of how
to use the program.  The test subdirectory contains the scripts to run the
tests.  The data subdirectory contains the input data and the expected results
for each test.

To test the installation, change to the mpath3.2/test directory and type
the command:

     ./test.sh [start [stop]]

where:  start = the number of the first test to perform, default = 1
        stop  = the number of the last test to perform, default = 8

For example:

     command                                 what happens
     --------------------------------------  --------------------------------
     ./test.sh                               runs all tests
     ./test.sh 1 1                           runs the first test
     ./test.sh 2 3                           runs tests 2 and 3

After the tests are completed, the results are compared to the expected
results (found in data subdirectory).  See the file check.log; if all
goes well, there should be no differences other than differences in
the creation date that is written to the *.ps PostScript files.

To clean up after the tests, type the command:

     ./clean.sh

Notes:  The standard data sets were created on a Sun UltraSPARC-II system.
        You may notice slight numeric differences in the results on other
        computers.  These are generally due to different round-off algorithms
        and the different architecture of the central processing unit chip.
        Slight differences in output formats may occur on other computers,
        particularly for the value 0.0.

The tests are described in the table below, where 'test' is the  test number,
'program' is the program used to run the test, and the 'usage' column indicates
how a file is used, with i for input, o for output, and i/o for both input and 
output.

test  program  description of test and files           file name & usage
----  -------  --------------------------------------  -----------------
               Tests 1-4 use the following input files
               in addition to the files listed below 
               for each test:

               Main data file                          demo-s.mp      i
               MODFLOW-generated cell-by-cell          demo-s.bud     i
                 budget data                           
               MODFLOW-generated head data             demo-s.hed     i
               MODFLOW Recharge Package Input          demo-s.rch     i
               MODFLOW River Package Input             demo-s.riv     i
               MODFLOW Well Package Input              demo-s.wel     i
               IBOUND array for layer 1                ibound.1       i
               IBOUND array for layer 4                ibound.4       i

  1   mpath3   Steady-state flow sample problem,       
      mplot3   forward-tracking endpoint analysis      

               MODPATH response file                   path-s1.rsp    i
               Name file                               demo-s1.mpn    i
               Summary of input data and simulation    summary1.pth   o
               Endpoint file                           endpoint.s1   i/o
               MODPATH-PLOT response file              plot-s1.rsp    i
               Summary of input data and plot status   summary1.plt   o
               Plot of results from MODPATH run        mplot1.ps      o

  2   mpath3   Steady-state flow sample problem,
      mplot3   backward-tracking endpoint analysis

               MODPATH response file                   path-s2.rsp    i
               Name file                               demo-s2.mpn    i
               Summary of input data and simulation    summary2.pth   o
               Endpoint file                           endpoint.s2   i/o
               MODPATH-PLOT response file              plot-s2.rsp    i
               Summary of input data and plot status   summary2.plt   o
               Plot of results from MODPATH run        mplot2.ps      o

  3   mpath3   Steady-state flow sample problem,
      mplot3   backward-tracking pathline analysis
               in cross-section

               MODPATH response file                   path-s3.rsp    i
               Name file                               demo-s3.mpn    i
               Starting locations file                 demo-s3.loc    i
               Summary of input data and simulation    summary3.pth   o
               Endpoint file                           endpoint.s3    o
               Pathline file                           pathline.s3   i/o
               MODPATH-PLOT response file              plot-s3.rsp    i
               Summary of input data and plot status   summary3.plt   o
               Plot of results from MODPATH run        mplot3.ps      o

  4   mpath3   Steady-state flow sample problem,
      mplot3   time-series analysis with multiple
               release times

               MODPATH response file                   path-s4.rsp    i
               Name file                               demo-s4.mpn    i
               Summary of input data and simulation    summary4.pth   o
               Endpoint file                           endpoint.s4    o
               Time-series file                        timesers.s4   i/o
               MODPATH-PLOT response file              plot-s4.rsp    i
               Summary of input data and plot status   summary4.plt   o
               Plot of results from MODPATH run        mplot4.ps      o


               Tests 5-8 use the following input files
               in addition to the files listed below 
               for each test:

               Main data file                          demo-t.mp      i
               MODFLOW-generated cell-by-cell          demo-t.bud     i
                 budget data                           
               MODPATH-generated composite budget file demo-t.cbf     i
               MODFLOW-generated head data             demo-t.hed     i
               MODFLOW Recharge Package Input          demo-t.rch     i
               MODFLOW River Package Input             demo-t.riv     i
               MODFLOW Well Package Input              demo-t.wel     i
               IBOUND array for layer 1                ibound.1       i
               IBOUND array for layer 4                ibound.4       i

  5   mpath5   Transient flow sample problem,          
      mplot5   map view backward-tracking pathline     
	       analysis
                                                       
               MODPATH response file                   path-t1.rsp    i
               Name file                               demo-t1.mpn    i
               Summary of input data and simulation    summary5.pth   o
               Endpoint file                           endpoint.t1    o
               Pathline file                           pathline.t1   i/o
               MODPATH-PLOT response file              plot-t1.rsp    i
               Summary of input data and plot status   summary5.plt   o
               Drawing commands file of contours       contours.dcf   o
               Drawing commands file of pathlines      paths.dcf      o
               Plot of results from MODPATH run        mplot5.ps      o
                                                       
  6   mpath3   Transient flow sample problem,
      mplot3   backward-tracking endpoint analysis 
               for well 2

               MODPATH response file                   path-t2.rsp    i
               Name file                               demo-t2.mpn    i
               Summary of input data and simulation    summary6.pth   o
               Endpoint file                           endpoint.t2   i/o
               MODPATH-PLOT response file              plot-t2.rsp    i
               Summary of input data and plot status   summary6.plt   o
               Plot of results from MODPATH run        mplot6.ps      o

  7   mpath3   Transient flow sample problem,
      mplot3   forward-tracking endpoint analysis 
               for wells 1 and 2

               MODPATH response file                   path-t3.rsp    i
               Name file                               demo-t3.mpn    i
               Summary of input data and simulation    summary7.pth   o
               Endpoint file                           endpoint.t3   i/o
               MODPATH-PLOT response file              plot-t3.rsp    i
               Summary of input data and plot status   summary7.plt   o
               Plot of results from MODPATH run        mplot7.ps      o

  8   mpath3   Transient flow sample problem,
      mplot3   backward-tracking endpoint analysis 
               for well 1

               MODPATH response file                   path-t4.rsp    i
               Name file                               demo-t4.mpn    i
               Summary of input data and simulation    summary8.pth   o
               Endpoint file                           endpoint.t4   i/o
               MODPATH-PLOT response file              plot-t4.rsp    i
               Summary of input data and plot status   summary8.plt   o
               Plot of results from MODPATH run        mplot8.ps      o


The following MODFLOW input files, used to generate the output processed
by MODPATH, are also included in the MODPATH data directory:

	       Steady-state flow sample problem input files
               --------------------------------------------
               MODFLOW Name File                       demo-s.nam
               MODFLOW Basic Package Input             demo-s.bas
               MODFLOW BCF Package Input               demo-s.bcf
               MODFLOW Output Control Input            demo-s.oc 
               MODFLOW SIP Package Input               demo-s.sip

	       Transient flow sample problem input files
               -----------------------------------------
               MODFLOW Name File                       demo-t.nam
               MODFLOW Basic Package Input             demo-t.bas
               MODFLOW BCF Package Input               demo-t.bcf
               MODFLOW Output Control Input            demo-t.oc 
               MODFLOW SIP Package Input               demo-t.sip


H. CONTACTS

Inquiries about this software distribution should be directed to:

  U.S. Geological Survey
  Hydrologic Analysis Software Support Program
  437 National Center
  Reston, VA  20192

  e-mail:  h2osoft@usgs.gov
