README.TXT
Revised: September 26, 2016

                  MODPATH - Version: 7.1.000

MODPATH is a particle-tracking model designed to work with output from 
MODFLOW-2005 and MODFLOW-USG. This version of MODPATH is packaged for 
personal computers using the Microsoft Windows operating system (versions
7, 8, or 10).  Executable files for personal computers are provided as 
well as the source code.  The source code can be compiled to run on 
other computers.

Instructions for installation, execution, and testing of MODPATH are
provided below.

                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING MODPATH
                         D. EXAMPLES
                         E. COMPILING

NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.

A. DISTRIBUTION FILE

The files for this distribution are provided in a ZIP archive file named:

  modpath_7_1_00.zip
  
To extract the files, select a directory and extract the zip file to the 
selected directory.The following directory structure will be created:

  |-- modpath_7_1_000
      |-- bin                          ; executables for MODPATH (both 32-bit and 64-bit)
      |-- doc                          ; User guide for MODPATH-7 and input instructions
      |-- examples                     ; Example simulations with input and output
         |-- example_1
             |-- completed
             |-- original
             |-- work
         |-- example_2
             |-- completed
             |-- original
             |-- work
      |-- source                       ; Source code for MODPATH
      |-- utilities
          |-- ModpathShapefileExporter ; Utility to convert MODPATH output to shapefiles
          |-- QuadpatchGridExporter    ; Utility to create unstructured quadpatch grid
              |-- test                   input files for MODFLOW-USG and MODPATH-7.
    
It is recommended that no user files are kept in the modpath_7_1_000 directory
structure.  If you do plan to put your own files in the modpath_7_1_000
directory structure, do so only by creating additional subdirectories.

The documentation for MODPATH is a Portable Document Format (PDF) file. PDF files are 
readable and printable on various computer platforms using Acrobat Reader from Adobe. 
The Acrobat Reader is freely available from 
the following World Wide Web site:

      http://www.adobe.com/


B. INSTALLING

To make the executable version of MODPATH accessible from any
directory, the directory containing the executables (modpath_7_1_000\bin)
should be included in the PATH environment variable.  Also, if a 
prior release of MODPATH version 7 is installed on your system, the
directory containing the executables for the prior release should
be removed from the PATH environment variable. Existing installations
of MODPATH version 6 or earlier are not affected by the installation
of MODPATH version 7 and do not need to be removed.

As an alternative, the executable file, mpath7.exe,in the modpath_7_1_000\bin 
directory can be copied into a directory already included in the PATH 
environment variable. System batch files also can be used to run MODPATH. Refer 
to the Microsoft Windows help topics or consult your system administrator for 
assistance in modifying the PATH environment variable

C. EXECUTING MODPATH

Two MODPATH runfiles are provided in the modpath_7_1_000\bin directory:

* mpath7.exe  -- a 64-bit executable file

* mpath7_32bit.exe  --  a 32-bit executable file

After the executable files in the modpath_7_1_000\bin directory are installed
in a directory that is included in your PATH, MODPATH is initiated in
a Windows Command-Prompt window using the following command:

          mpath7.exe [Fname]
          
or

          mpath7_32bit.exe [Fname]

The optional Fname argument is the name of a MODPATH simulation file.  
If no argument is used, the user is prompted to enter the name of a 
MODPATH simulation file.  If the simulation file name ends in ".mpsim", 
the file name can be specified without including ".mpsim". 

The data arrays in MODPATH are dynamically allocated, so models
are not limited by hard-coded array limits. However, it is best to have
enough random-access memory (RAM) available to hold all of the required
data.  If there is less available RAM than this, the program will use
virtual memory, but this slows computations significantly.

Documention for model input and output files is provided in the file
"MODPATH_7_input_instructions.pdf".

D. EXAMPLES

Two example problems are provided with this distribution. The example
problems are described in the MODPATH-7 documentation report which is
provided in file ofr20161086.pdf located in the doc directory.

Each example problem directory contains the following three subdirectories:
  - original
  - completed
  - work

The directory "original" includes a master copy of the MODFLOW and MODPATH 
data files setup to run MODPATH. The MODFLOW input and output files are
provided so there is no need to run MODFLOW prior to running MODPATH. The 
directory "work" is an empty directory that is provided as a convenient
place to make test runs of the example problems. To make a test run, copy 
all of the files in directory "original" into directory "work". Then, run
MODPATH by double clicking on the batch file named "runMPath7.bat" located
in directory "work". The batch file is setup to run the MODPATH executable
file mpath7.exe located in the bin directory. To preserve the original files, 
you should not run MODFLOW or MODPATH from directory "original". A completed 
run for each example is located in directory "completed" for comparison purposes.

E. COMPILING

The executable file provided in modpath_7_1_000\bin was created using the Intel
Parallel Studio XE2016 Composer Edition for Fortran Windows in combination
with Microsoft Visual Studio 2015.  Although executable versions of the program 
are provided, the source code is provided in the modpath_7_1_000\source directory 
so that MODPATH can be recompiled if necessary.  However, the USGS cannot 
provide assistance to those compiling MODPATH. In general, the requirements 
are a Fortran compiler that fully supports the Fortran-2003 standard.

