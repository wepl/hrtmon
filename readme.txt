
HRTmon source information
-------------------------

for more information go to the HRTmon homepage:
http://dumbo.cryogen.ch/hrtmon/index.html

or send me an email at:
Alain.Malek@cryogen.ch

the following files are included:

************************************
documentation files
************************************

HRTmon.guide
HRTmon.guide.info
HRTmon.readme

************************************
hrtmon command files (starts HRTmon without HRTmonPrefs)
************************************

HRTmon
HRTmon.c

************************************
HRTmon Prefs files
************************************

DMakefile		(makefile for DICE C compiler)

#GUI edited with GadToolsBox

HRTmonGUI2.c
HRTmonGUI2.h
HRTmonGUI2.gui
HRTmonGUI2.gui.info
HRTmonGUI2.o

HRTmonPrefs.c		(the main source of HRTmonPrefs)
HRTmonPrefs
HRTmonPrefs.info

HRTboot.s		(boot block for boot-disk)
HRTboot.o

assembler.h
assembler.s		(some asm routines for HRTmonPrefs)
assembler.o

************************************
main source files
************************************

(see directory 'src')
HRTmon.data		(assemble HRTmonV2.s to get this file)

************************************
data files
************************************

SPRfont.iff
SPRfont.raw
Topaz3.iff
Topaz3.raw

****************************************************************

The best place to start hacking the source is the HRTmonV2.s file.
(and all the files in the 'src' directory)
This file can be assembled at least with ASMOne, ASMPro, BASM or PhxAss.
The resulting file should be saved as HRTmon.data.
That's it, you have a new version of HRTmon.


