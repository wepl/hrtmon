;
; $Id$
;
;HRTmon Amiga system monitor
;Copyright (C) 1991-1998 Alain Malek Alain.Malek@cryogen.com
;
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You can find the full GNU GPL online at: http://www.gnu.org

help_txt
 dc.b "--------------------------------- HELP PAGE ----------------------------------",$a

 dc.b "R    reg new-val   : show or modify registers",$a
 dc.b "A    address instr : assemble 680x0 code",$a
 dc.b "D    address       : disassemble / assemble 680x0 code",$a
 dc.b "H    address       : hex dump of memory or modify memory",$a
 dc.b "M    address       : hex dump of memory or modify memory",$a
 dc.b "N    address       : ascII dump of memory",$a
 dc.b "E    offset        : show / edit custom registers",$a
 dc.b "TYPE addr.         : type memory",$a
 dc.b "C    src end dest  : copy a memory block",$a
 dc.b "Q    strt end dest : compare two memory blocks",$a
 dc.b "O    strt end val  : fill memory",$a
 dc.b "CE   src end dest  : exchange two memory blocks",$a
 dc.b "F    strt end val  : find",$a
 dc.b "FS   strt end str  : find string (not casesensitive)",$a
 dc.b "FI   strt end str  : find instruction",$a
 dc.b "COP  strt end      : find copper-list",$a
 dc.b "P    picno         : gfx-ripper (picno=0,1,2,..) (first use COP)",$a
 dc.b $a
 dc.b "B   address        : set/remove an illegal breakpoint",$a
 dc.b "BJ  address        : set/remove a JSR breakpoint",$a
 dc.b "BD                 : delete all breakpoints",$a
 dc.b "MW.(b|w|l) address : set/remove memory watch",$a
 dc.b "MW                 : view all memory watch",$a
 dc.b "MWD                : delete all memory watch",$a
 dc.b "G   address        : restart program at address",$a
 dc.b "T   nbsteps        : trace instruction(s)",$a
 dc.b "TA  address        : trace till 'address' is reached",$a
 dc.b "DEBUG              : patch exception vectors",$a
 dc.b $a
 dc.b "DRIVE no           : select drive (0/1) for format and read/write sector",$a
 dc.b "MOTOR              : turn drive motor on",$a
 dc.b "FORMAT name        : format a floppy disk",$a
 dc.b "FORMATQ name       : quick format a floppy disk",$a
 dc.b "DISKCHK            : check a floppy disk",$a
 dc.b "DIR path           : directory",$a
 dc.b "CD  path           : change current directory  ('/' for parent)",$a
 dc.b "L   name addr      : load a file in memory",$a
 dc.b "S   name strt end  : save memory to a file",$a
 dc.b "SP  name           : save the actual picture from the gfx-ripper",$a
 dc.b "MAKEDIR name       : create a new directory",$a
 dc.b "DEL name           : delete a file or an empty directory",$a
 dc.b "COPY src dest      : copy a file",$a
 dc.b "RS  addr strt nb   : read sector from floppy disk",$a
 dc.b "WS  addr strt nb   : write sector to floppy disk",$a
 dc.b "D2F name           : read a floppy disk and save it as a file",$a
 dc.b "F2D name           : read a file and write it as a floppy disk",$a
 dc.b "BB  addr           : calculate boot-block checksum",$a
 dc.b "IDE                : get information from IDE drives",$a
 dc.b "PART               : list all partitions",$a
 dc.b "LA name            : load all",$a
 dc.b "SA name            : save all",$a
 dc.b "SAC name           : save all crunched",$a
 dc.b "CLEAR              : clear all memory and reboot (HRTmon lost)",$a
 dc.b "REBOOT             : reboot and keep HRTmon resident",$a
 dc.b $a
 dc.b "TS strt end lives  : trainer start",$a
 dc.b "TSD strt end lives : deep trainer start",$a
 dc.b "TF  lives          : trainer find (normal and deep)",$a
 dc.b $a
 dc.b "EXCEP              : show exception vectors",$a
 dc.b "OUTPUT addr        : copy output to memory",$a
 dc.b "AF  addr instr     : assemble 65816",$a
 dc.b "DF  address        : disassemble 65816",$a
 dc.b "FIF strt end str   : find instr. 65816",$a
 dc.b "PAL                : set PAL display",$a
 dc.b "NTSC               : set NTSC display",$a
 dc.b "31K                : set 31Khz display (Productivity, Euro72,...)",$a
 dc.b "LED                : switch power led",$a
 dc.b "SETMAP mapname     : Set/show keymap (USA,CH,D,F)",$a
 dc.b "?   expression     : evaluate an expression",$a
 dc.b "VER                : show HRTmon version",$a
 dc.b "X                  : exit",$a
 dc.b "KILL               : remove HRTmon",$a
 dc.b "HEXLOCK (on|off)   : switches hexadecimal mode as default",$a

 dc.b $a
 dc.b "F1     : Clear screen",$a
 dc.b "F2     : Toggle insert mode on/off",$a
 dc.b "F10    : Switch pages 1/2",$a
 dc.b "F7     : Enter/Exit tracer",$a
 dc.b "F6     : Toggle 65816/65802 CPU mode",$a
 dc.b "Esc    : Break/ignore line",$a
 dc.b "Tab    : insert blank character",$a
 dc.b "ALT Tab       : insert blank line",$a
 dc.b "SHIFT Del     : insert a blank character",$a
 dc.b "ALT Del       : remove line",$a
 dc.b "SHIFT Backspc : clear current line",$a
 dc.b "CTRL  up,down : move into command history",$a
 dc.b "SHIFT up,down : set cursor to top/bottom of screen",$a
 dc.b $a
 dc.b "--- GFX ripper help ------------",$a
 dc.b $a
 dc.b "F10     Exit gfx-ripper",$a
 dc.b "Arrows  Move unlocked plans.",$a
 dc.b "Shift   Fast move",$a
 dc.b "1-8     Lock/Unlock plan 1-8",$a
 dc.b "M       Increase modulo",$a
 dc.b "N       Decrease modulo",$a
 dc.b ",       Clear modulo",$a
 dc.b "Q       Decrease picture width",$a
 dc.b "W       Increase picture width",$a
 dc.b "A       Decrease picture height",$a
 dc.b "S       Increase picture height",$a
 dc.b "R       Set all bitplans to the same address",$a
 dc.b "H       Toggle HAM on/off",$a
 dc.b "E       Toggle EHB on/off",$a
 dc.b "Help    Switch info panel on/off",$a
 dc.b "Del     Change color of info panel",$a
 dc.b "F1      Start/Stop height set mode",$a
 dc.b $a
 dc.b "--- WHDLoad help ---------------",$a
 dc.b $a
 dc.b "WL    name adr      : load a file in memory",$a
 dc.b "WS    name strt end : save memory to a file",$a
 dc.b "WPR   strt len      : protect memory from reading",$a
 dc.b "WPW   strt len      : protect memory from writing",$a
 dc.b "WPRW  strt len      : protect memory from reading and writing",$a
 dc.b "WPSMC strt len      : snoop memory region for selfmodifying code",$a
 dc.b "WQ                  : quit whdload",$a
 dc.b "WD                  : quit whdload with debug (dumpfile)",$a

	dc.b 0
		even
