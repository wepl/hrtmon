#
# GNU Makefile for HRTmon
#
# supported platforms:
# Amiga	
#	'setenv AMIGA 1' for system detection
#	assign Includes: must point to system includes
#	requires WDate command, see WHDLoad-dev package Src/programs/WDate
#	basm default, use DEBUG=0 for vasm
# Linux/MacOSX
#	$INCLUDEOS3 must point to system includes
#	vasm default, use DEBUG=1 for basm (requires Vamos)
# includes for reqtools.library are needed additionally (aminet:util/libs/ReqToolsDev.lha)
#
# $@ target
# $< first dependency
# $^ all dependencies
# print all make variables
#$(foreach v,$(.VARIABLES),$(info $(v) = $($(v))))
# print all make variables defined between both statements
#VARS_OLD := $(.VARIABLES)
#$(foreach v,$(filter-out $(VARS_OLD) VARS_OLD,$(.VARIABLES)),$(info $(v) = $($(v))))

# different commands for build under Amiga or UNIX
ifdef AMIGA

# basm options: -x+ = use cachefile.library -s1+ = create SAS/D1 debug hunks
BASMOPT=-x+
BASMOPTDBG=-s1+
CFLAGS=
CP=Copy Clone
MV=Copy
RM=Delete All
DATE=wdate >

# on Amiga default=DEBUG
ifndef DEBUG
DEBUG=1
endif

else

# basm options: -x- = don't use cachefile.library -sa+ = create symbol hunks
BASMOPT=-x-
BASMOPTDBG=-sa+
VASMOPT=-I$(INCLUDEOS3)
CFLAGS=-I$(INCLUDEOS3)
CP=cp -p
MV=mv
RM=rm -fr
DATE=date "+(%d.%m.%Y)" | xargs printf >
VAMOS=vamos -qC68020 -m4096 -s128 --

# on UNIX default=NoDEBUG
ifndef DEBUG
DEBUG=0
endif

endif

ifeq ($(DEBUG),1)

# Debug options
# ASM creates executables, ASMB binary files, ASMO object files
# BASM: -H to show all unused Symbols/Labels, requires -OG-
ASM=$(VAMOS) basm -v+ $(BASMOPT) $(BASMOPTDBG) -O+ -ODc- -ODd- -wo- -dDEBUG=1
ASMB=$(ASM)
ASMO=$(ASM)
ASMDEF=-d
ASMOUT=-o
CC=vc -c99 -g -I. $(CFLAGS) -sc -deps

else

# Optimize options
# VASM: -wfail -warncomm -databss
ASMBASE=vasmm68k_mot $(VASMOPT) -ignore-mult-inc -nosym -quiet -wfail -opt-allbra -opt-clr -opt-lsl -opt-movem -opt-nmoveq -opt-pea -opt-size -opt-st -depend=make -depfile .depend/$@.dep
ASM=$(ASMBASE) -Fhunkexe
ASMB=$(ASMBASE) -Fbin
ASMO=$(ASMBASE) -Fhunk
ASMDEF=-D
ASMOUT=-o 
CC=vc -c99 -g -I. $(CFLAGS) -O2 -size -sc -deps

endif

# general programs
LN=vc

# objects and depend files are always created together
%.o: %.s | .depend
	${ASMO} $(ASMOUT)$@ $<

%.o: %.c | .depend
	$(CC) -o $@ -c $<
	$(MV) $*.dep .depend/

#
# HRTmon.data (default target)
#
HRTmon.data: src/HRTmonV2.s .date | .depend
	$(ASM) $(ASMOUT)$@ $<

#
# HRTmon loader
#
HRTmon: HRTmon.o assembler.o HRTmonLoad.o
	$(LN) $^ -o $@

#
# HRTmon preferences editor
#
HRTmonPrefs: HRTmonPrefs.o HRTmonGUI2.o HRTboot.o assembler.o HRTmonLoad.o
	$(LN) $^ -o $@

all: HRTmonPrefs HRTmon HRTmon.data

# how to create additionally listing files
%.list: %.s | .depend
	$(ASM) $(ASMOUT)$(@:.list=.o) -L $@ $<

HRTmon.data.list: src/HRTmonV2.s .date | .depend
	$(ASM) $(ASMOUT)HRTmon.data -L $@ $<

# check for unused labels
unused: HRTmon.data.list
	grep " LAB .* UNUSED" *.list

clean:
	$(RM) HRTmon HRTmonPrefs HRTmon.data *.o *.list .date .depend

# targets which must be always built
.PHONY: .date all clean unused

.date :
	$(DATE) $@

.depend:
	@mkdir .depend

include $(wildcard .depend/*)

