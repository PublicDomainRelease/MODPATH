README.TXT


                  MODPATH - Version: 6.0.01

MODPATH is a particle-tracking model designed to work with output from 
MODFLOW, the USGS finite-difference groundwater flow model. This version 
of MODPATH is packaged for personal computers using the Microsoft Windows XP 
or Windows 7 operating systems.  Executable files for personal computers 
are provided as well as the source code.  The source code can be compiled 
to run on other computers.

IMPORTANT: The input and output for MODPATH version 6 is substantially
different from previous versions of MODPATH. Users should review the file 
TM6A41_Modpath6.pdf, which contains the user guide for MODPATH version 6. 
Users also should review the file release.txt, which describes changes 
that have been introduced into MODPATH with each official release; these 
changes may substantially affect users.

Instructions for installation, execution, and testing of MODPATH are
provided below.

                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING MODPATH
                         D. TESTING
                         E. COMPILING

NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.

A. DISTRIBUTION FILE

The files for this distribution are provided in a ZIP archive file named:

  modpath.6_0_01.zip
  
To extract the files, select a directory and extract the zip file to the 
selected directory.The following directory structure will be created:

  |-- modpath.6_0
      |--bin               ; executables for MODPATH
      |--doc               ; User guide for MODPATH version 6
      |--example-out       ; Example simulations with input and output
      |--example-run       ; Example simulations with input only
      |--src               ; Source code for MODPATH
    
It is recommended that no user files are kept in the modpath.6_0 directory
structure.  If you do plan to put your own files in the modpath.6_0
directory structure, do so only by creating additional subdirectories.

The documentation for MODPATH version 6 is a Portable Document Format (PDF) 
file. PDF files are readable and printable on various computer platforms 
using Acrobat Reader from Adobe. The Acrobat Reader is freely available from 
the following World Wide Web site:

      http://www.adobe.com/


B. INSTALLING

To make the executable version of MODPATH accessible from any
directory, the directory containing the executables (modpath.6_0\bin)
should be included in the PATH environment variable.  Also, if a 
prior release of MODPATH version 6 is installed on your system, the
directory containing the executables for the prior release should
be removed from the PATH environment variable. Existing installations
of MODPATH version 5 or earlier are not affected by the installation
of MODPATH version 6 and do not need to be removed.

As an alternative, the executable file, mp6.exe,in the modpath.6_0\bin 
directory can be copied into a directory already included in the PATH 
environment variable. Refer to the Microsoft Windows help topics or 
consult your system administrator for assistance in modifying the 
PATH environment variable

C. EXECUTING MODPATH

A MODPATH runfile named mp6.exe is provided in the modpath.6_0\bin 
directory. The runfile provided in this distribution carries out all real 
number calculations in double precision. Real number variables are
not explicitly specified as double precision in the source code. The 
double precision runfile is created by using a compiler option that raises 
the precision of single precision numbers to double precision.If needed, 
a single precision version of the runfile can be created by recompiling 
the source code without the double precision option.

After the executable files in the modpath.6_0\bin directory are installed
in a directory that is included in your PATH, MODPATH is initiated in
a Windows Command-Prompt window using the following command:

          mp6.exe [Fname]

The optional Fname argument is the name of a MODPATH simulation file.  
If no argument is used, the user is prompted to enter the name of a 
MODPATH simulation file.  If the file name ends in ".mpsim", then the file 
name can be specified without including ".mpsim". For example, if the 
simulation file is named abc.mpsim, then MODPATH can be run by entering:

          mp6.exe abc

The data arrays in MODPATH are dynamically allocated, so models
are not limited by hard-coded array limits. However, it is best to have
enough random-access memory (RAM) available to hold all of the required
data.  If there is less available RAM than this, the program will use
virtual memory, but this slows computations significantly.                       

D. TESTING

Several example problems are provided with this distribution. A complete 
set of examples with output is provided in the directory, 
modpath.6_0\example_out.A duplicate set of example problem input files 
is provided in directory, modpath.6_0\example-run. Executing the example 
problem simulations from the example-run directory will prevent the 
output in the example-out directory from being overwritten. The example 
problem directories contain a file named Example_Descriptions.txt that 
describes the example simulations.

E. COMPILING

The executable file provided in modpath.6_0\bin was created using the Intel
Visual Fortran 11.1 compiler.  Although executable versions of the program 
are provided, the source code is provided in the modpath.6_0\src directory 
so that MODPATH can be recompiled ifnecessary.  However, the USGS cannot 
provide assistance to those compiling MODPATH. In general, the requirements 
are a Fortran compiler that fully supports the Fortran-95 standard as well
as the STREAM file-access attribute as defined by the Fortran-2003 standard.

