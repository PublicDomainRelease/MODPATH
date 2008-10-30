U S. Geological Survey


                      MODPATH/MODPATH-PLOT Version 5.0
              Distribution for IBM-PC compatible computers


NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.


Instructions for installing, executing, and testing MODPATH and
MODPATH-PLOT are provided below.

This version of the software is packaged for use on personal computers
using a Microsoft Windows operating system.


                             TABLE OF CONTENTS

                         A.  DISTRIBUTION FILE
                         B.  EXTRACTING FILES
                         C.  COMPILING
                         D.  INSTALLING
                         E.  RUNNING THE SOFTWARE
                         F.  TESTING
                         G.  INPUT INSTRUCTIONS

A. DISTRIBUTION FILE

The following self-extracting distribution file, containing the software,
test data sets, and information files, is currently available for
computer systems using one of the Microsoft Windows operating systems:

         mpath5_0.exe

File mpath.5_0/doc/modpath.txt describes the history of the software
versions.  Also included in directory mpath.5_0/doc is a Portable
Document Format (PDF) version of the MODPATH documentatoin
(ofr94464.pdf).


B. EXTRACTING FILES

Extract the files by executing file:

        mpath5_0.exe

When the extraction progrm runs, it is recommended that the files be
restored to directory C:\WRDAPP .  The following directory structure
will be created in C:\WRDAPP (the contents of each directory are shown
to the right):


	C:\WRDAPP
              |
              |-- Mpath.5_0
              |       |
              |       |-- Data       (MODPATH test data files)
              |       |
              |       |-- doc        (Documentation files)
              |       |
              |       |-- Mfoutput   (MODFLOW output files for demo problem)
              |       |
              |       |-- setup      (Setup files, MODPATH and MODPATH-PLOT 
              |       |               executable files, and BAT files for
              |       |               running MODPATH and MODPATH-PLOT)
              |       |-- src
              |              |
              |              |-- Advobs (Source for advobs program)
              |              |
              |              |-- Mpath  (Source for MODPATH)
              |              |
              |              |-- Mplot  (Source for MODPATH-PLOT)
              |
              |
              |


C. COMPILING

An executable version of the code for personal computers is provided in
the setup directory.  This version has been compiled using the
Lahey/Fujitsu Fortran 95 compiler version 5.7.

The source code is also provided in the src directory so that users can
generate the executable themselves. No support can be provided for
users generating their own versions of the software.  All of the source
code options that are specific to this Lahey compiler are contained in
files: MPATH5.FOR, MPLOT5LF.FOR, SYSLF.FOR, and OPENSPEC.INC.  SYSLF.FOR
contains code to retrieve command-line arguments.  OPENSPEC.INC includes
specifiers for opening binary files.

To recompile MODPATH-PLOT, it is necessary to have access to a Level 0A
GKS graphics subroutine library. This version of MODPATH-PLOT uses a
GKS library that is implemented using the Winteracter Graphics Library.
To recompile the code, a license for Winteracter is required.


D: INSTALLING

Files MPATH5.BAT and MPLOT5.BAT must be copied from the SETUP directory
to a directory in the user's search path. These BAT files rely on the
runfiles being located in directory C:\WRDAPP\MPATH.5_0\SETUP.



E. RUNNING THE SOFTWARE

After MODPATH and MODPATH-PLOT are properly installed in a directory
that is included in the user's search path, they are executed using the
commands MPATH5 and MPLOT5, respectively.  These programs can run
interactively, or they can run using response files that contain the
user input.  When MODPATH or MODPATH-PLOT run interactively, they
produce a response file that can be used to rerun the programs.

The MODPATH and MODPATH-PLOT commands have optional arguments, "i" and
the name of a response file. If the response file argument is specified,
the programs run using the responses in the response file. The "i" argument
tells the program to prompt for all input interactively.   If no option
is given, the program will prompt for the name of a "response file".  If
none is given, the program will proceed in the interactive mode.

Response files can be made to include interactive prompts by adding the
string "(?)" to the end of the last line of a prompt message.  This is
useful, for example, when plots are needed for multiple reference times
with all other responses the same.

MODPATH-PLOT has been compiled using a GKS library that uses the
Interacter Graphics Library by Interactive Software Services.  The
following graphics devices have been incorporated:

Device 1 -- Screen only
Device 2 -- Screen and Windows Meta File -- output goes into file mplot.wmf
Device 3 -- Screen and PostScript File -- output goes into file mplot.ps
Device 4 -- Screen and DXF -- output goes into file mplot.dxf
Device 5 -- Screen and Windows Print Manager -- output goes to the current
                    default printer
Device 6 -- Screen and Windows Clipboard -- output goes to the clipboard,
                    which allows it to be pasted into other applications
                    that can accept graphics from the clipboard

MODPATH-PLOT starts by creating a command window.  Prompts are displayed in
the command window.  Responses are entered into the "Response:" line at the
bottom of the command window.  After all responses are entered, MODPATH-PLOT
creates a graphics window and draws the plot.  To terminate the program,
select the Exit button at the top left side of the graphics screen , or
select the Exit option in the File Menu.


NOTE: The unformatted files generated by MODFLOW (".HED" and ".BUD"
files) were generated by a version of MODFLOW that was compiled using
options that cause MODFLOW to create unformatted files without any
structure. MODPATH and MODPATH-PLOT have been using the same compiler
options, and therefore MODPATH and MODPATH can read the unformatted
files geneated by the MODFLOW runfile. MODPATH and MODPATH-PLOT can be
compiled by different compilers provided that they cause unformatted
files to be written in the same unstructured manner.  However, if
options are used that cause a compiler to generate structured binary
files, then it is generally necessary that MODPATH and MODPATH-PLOT be
compiled with the same compiler used to compile MODFLOW.

Starting with MODFLOW-2000 version 1.2 and all MODFLOW-2005 versions,
the USGS-distributed MODFLOW runfile for personal computers is compiled
using the above-mentioned options for writing unstructured binary
files.  If you are using an earlier version of a MODFLOW runfile
produced by the USGS, then the head and budget unformatted files
will not work with the current version of MODPATH and MODPATH-PLOT.


F. TESTING

Directory C:\WRDAPP\MPATH.5_0\DATA has data files for running the example
problems described in the MODPATH documentation.  Run these problems as
follows:

1. The MODFLOW output files needed to run the MODPATH example problems are
   already present in the DATA directory. So, you can run the MODPATH and
   MODPATH-PLOT DATA without running MODFLOW. This allows the example problems
   to be run even on computers that do not yet have MODFLOW installed.
   Extra copies of the MODFLOW output files also are located in the MFOUTPUT
   directory.

   The files DEMO-S.NAM and DEMO-T.NAM are "name files" that contain a list 
   of MODFLOW data files and Fortran unit numbers for the MODFLOW steady 
   state and transient runs.

2. Once the MODFLOW output files have been generated, you can now run the 
   MODPATH example problems. The example problems are the same problems
   presented in the MODPATH manual (USGS Open-File Report 94-464). 

   MODPATH and MODPATH-PLOT use a combination of interactive and data file
   input. Interactive input is recorded in response files that can
   be used in subsequent runs in place of interactive input. Data
   files and response files have been prepared for 4 steady-state and 
   4 transient particle tracking analyses. The response files end with the
   suffix ".RSP". For example, PATH-S1.RSP and PLOT-S1.RSP are the response 
   files needed to run MODPATH and MODPATH-PLOT for the first steady state
   analysis. 
   
   To run MODPATH from a DOS window, type "MPATH5" and then press enter. To
   run MODPATH from Windows shortcut icon, edit the shortcut so that
   MODPATH will start in the DATA directory, then double click the MODPATH 
   shortcut icon to start the program. The program will prompt you for the 
   name of a response file. To have the program read input from a response 
   file, simply enter the name of the response file you want to use. If you 
   want to run MODPATH and enter the data interactively at the keyboard, just 
   press enter without typing the name of a file when prompted for the name 
   of a response file. 
   
   MODPATH-PLOT works in the same way. To run MODPATH-PLOT, either type 
   "MPLOT5" in a DOS window or double click the MODPATH-PLOT shortcut icon
   on the desktop. Then, enter the name of a response file, or enter a blank 
   line to input data interactively.

3. Once you have run through all of the examples, you can create your own
   new runs by running MODPATH and MODPATH-PLOT interactively rather than 
   with response files. Note that after you run MODPATH interactively, your 
   responses are recorded in a file name MPATH.RSP. If you want to save this 
   file for future use, you must rename it to avoid overwriting it the next 
   time you run MODPATH. MODPATH-PLOT creates a response file named MPLOT.RSP.


G. INPUT INSTRUCTIONS

Input files for version 4 of MODPATH will continue to work for version
5.  The input for the main Modpath data file is the same.  The Modpath
Name file is the same, except a new optional file type has been added
to activate the tracking of particles outside of the grid.  For
information about tracking particles otside of the grid, see file
TrackingOutsideGrid.pdf in the doc directory. 


MODPATH VERSION 5 INPUT INSTRUCTIONS:

  MODFLOW-2000 and MODFLOW-2005 input files

1.  When point stresses are applied to faces, declare IFACE as an
Auxilary Variable in each point-stress input file, and use the COMPACT
BUDGET in Output Control.  The IFACE values are only written into the
budget file if this Output Control option is used.


  MODPATH Name File

1.  The MODPATH Name file contains the the following information for each
file used by MODPATH and MODPATH-PLOT: File type, unit number, file name.
This is similar in concept to the MODFLOW Name file.  The MODPATH Name
file generally includes fewer files than the MODFLOW Name file.  The Name
File contains one line for each file. A typical Name File looks like:

  MAIN         11 test.mp
  DIS          12 test.dis
  BUDGET       50 test.bud
  HEAD(BINARY) 60 test.hed

The file type is not case sensitive. The following file types are recognized:

  MAIN = Modpath main data file
  DIS = MODFLOW discretization file
  BUDGET = binary (unformatted) budget file produced by MODFLOW
  HEAD(BINARY) = binary (unformatted) head file produced by MODFLOW
  HEAD = text head file produced by MODFLOW
  CBF = composite budget file
  ENDPOINT = endpoint file (default file is "ENDPOINT")
  PATHLINE = pathline file (default file is "PATHLINE")
  TIME-SERIES = time series file (default file is "TIMESERS".)
  ADVOBS = output file for tracking particles outside of the grid
  DCF = drawing commands file
  TIME = time data file
  LOCATIONS = starting locations file
  DRAWDOWN(BINARY) = binary (unformatted) drawdown file produced by MODFLOW
  DRAWDOWN = text drawdown file produced by MODFLOW
  CONTOUR-DATA = text file containing 2-D data to contour
  CONTOUR-LEVEL = text file containing contour levels
  DATA = ancillary text input data files
  LIST = summary output file
  GUA = grid unit array file

Ancillary data files usually contain large arrays that are referenced by
array control records in other data files. Ancillary data files always must
be declared as type DATA.


  Modpath Main data file

1.  MAXSIZ HNOFLO  HDRY  NPART  IRCHTP  IEVTTP

MAXSIZ indicates the maximum size (in bytes) of the composite budget file 
       that iscreated by MPATH for transient simulations.  This is used
       only as a check; the user is given the option to continue even if
       the size exceeded.  Specify 0 to have MPATH use a default size.

HNOFLO is the value specified in MODFLOW to represent head in inactive cells.

HDRY is the head assigned by MODFLOW to cells that have gone dry during the
       simulation.

NPART is the maximum number of particles allowed for a MODPATH run. The value of
       NPART has no effect in MODPATH-PLOT. If NPART is set equal to 0, MODPATH
       automatically resets NPART to a default value that is defined in the
       MODPATH main program.

IRCHTP indicates where recharge (if used in the model) is applied.
             0 indicates distributed.
         not 0 indicates the top face.

IEVTTP indicates where evapotranspiration (if used in the model) is applied.
             0 indicates distributed.
         not 0 indicates the top face.


2.   Options

Options is a 1-line character record that may contain one or more keyword
       codes that control  options in MODPATH and MODPAT-PLOT. A blank
       line must be included for this data item even if none of the
       keyword options is used. Keywords that only affect MODPATH-PLOT
       are ignored by MODPATH.

  XSECTION indicates that the model is a 1-row cross section for which
  IBOUND and the Grid Unit Array (GUA) should each be read as a single,
  2-dimensional array with dimensions of NCOL and NLAY.

  COMPACT indicates that MODPATH should generate endpoint, pathline, and
  time series files as text files using the global node number to indicate
  cell location. If COMPACT is omitted, the cell location is designated
  using the row-column-layer grid indices (as in previous versions of
  MODPATH).

  BINARY indicates that the endpoint, pathline, and time series files will
  be generated by MODPATH in binary form. This keyword also is required by
  MODPATH-PLOT in order to correctly read binary versions of these files.

  METERS indicates to MODPATH-PLOT that distances are in meters. The METERS
  parameter only affects the label on the distance scale produced by
  MODPATH-PLOT. It has no affect on computations produced by MODPATH. If
  the option METERS is not specified, MODPATH assumes that length units are
  in feet. If a length unit other than feet or meters is used in MODFLOW,
  the scaling performed by MODPATH-PLOT will not be correct.

  WT(OFF) indicates that the water table profile will not be drawn on true
  cross section plots. If none of the three "WT" options is specified,
  MODPATH-PLOT draws the water-table profile as a solid line with the same
  color used for drawing contour lines.

  WT(DOTTED) indicates that the water-table profile will be drawn as a
  dotted line on true cross section plots.

  WT(DASHED) indicates that the water-table profile will be drawn as a
  dotted line on true cross section plots.


3.  LAYCON(NLAY)

LAYCON is the layer-type code.  Read one value for each layer all on a single line:
        0 indicates confined.
    not 0 indicates convertible or water table.

4.  IBOUND(NCOL,NROW)
      Read one array for each layer.

  FORMAT: 2-D integer array reader (U2DINT)
      Or, if the model is a 1-row cross section with XSECTION specified,
      read a single 2-D array for the cross section:

IBOUND is the boundary array containing cell type codes.
   IBOUND < 0 : specified head cell
   IBOUND = 0 : inactive cell
   IBOUND > 0 : active cell

The absolute value of IBOUND is the zone code, which is used by MODPATH-PLOT to determine
the color of pathlines and particle points. MODPATH requires that the absolute value of
IBOUND be less than 1000.


5.  REPEAT THE FOLLOWING POROSITY DATA IN SEQUENCE FOR EACH LAYER:

5A. POR(NCOL,NROW)
   FORMAT: 2-D real array reader (U2DREL)

5B. PorCB(NCOL,NROW)
   FORMAT: 2-D real array reader (U2DREL)

POR is the array containing porosity values for a model layer.

PorCB is the array containing porosity values for a quasi-3D confining
      layer associated with the model layer.  Include PorCB only if there
      is an associated quasi-3D layer.  The PorCB array immediately follows
      the POR array for that model layer.

***** ITEMS 6A and 6B ARE NOT REQUIRED FOR STEADY-STATE SIMULATIONS.

6A.  TBEGIN

TBEGIN is the time value assigned to the beginning of the MODFLOW simulation.
      Any convenient value may be specified, including values less than zero.

6B. BeginPeriod BeginStep EndPeriod EndStep

Data item 6B marks the range of time steps that will be processed from the
MODFLOW cell-bycell budget file and used to generate the MODPATH composite
budget file. The MODFLOW cellby-cell budget file must contain data for all
of the time steps within the range specified in data item 6B. The MODFLOW
cell-by-cell budget file also may contain data for time steps that are
outside of the specified range. Item 6B is not read if the MODPATH analysis
uses an existing composite budget file.

BeginPeriod, BeginStep are the stress period and time step of the first time
      step in the block of time steps that will be processed.

EndPeriod, EndStep are the stress period and time step of the last time step
      in the block of time steps that will be processed.
