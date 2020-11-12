
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

		cnop 0,4

board_list	dc.l board3,board4	;USA
		dc.l board1,board2	;CH
		dc.l board5,board6	;D
		dc.l board7,board8	;F

;-------------- setmap CH ---------------------------------------

board1		dc.b "`1234567890'^",$5c,0
		dc.b "0"			;$0f
		dc.b "qwertzuiopè#",0		;$10-$1c
		dc.b "123"			;1d-1f
		dc.b "asdfghjkléà$",0		;$20-$2c
		dc.b "456"			;$2d-$2f
		dc.b "<yxcvbnm,.-",0,"."	;$30-$3c
		dc.b "789"			;$3d-$3f
		dc.b " "
		dc.b 0				;$41 (backspace)
		dc.b 0				;$42 (Tab)
		dc.b 0,0			;$43 (Enter) $44 (Return)
		dc.b 0				;$45 (esc)
		dc.b 0				;$46 (del)
		dc.b 0,0,0,"-",0		;$47-$4b
		dc.b 0,0,0,0			;$4c-$4f arrows
		dc.b 0,0,0,0,0,0,0,0,0,0	;$50-$59 (F1-F10)
		dc.b "[]/*+"			;$5a-$5e
		dc.b 0				;$5f (Help)
		dc.b 0,0,0			;$60-$62 (Shift L,R CapsLock)
		dc.b 0				;$63 (CTRL)
		dc.b 0,0			;$64-$65 (Alt L,R)
		dc.b 0,0			;$66-$67 (Amiga L,R)

;Shift keyboard

board2		dc.b '`+"*ç%&/()=?^|',0
		dc.b "0"			;$0f
		dc.b "QWERTZUIOPü!",0		;$10-$1c
		dc.b "123"			;1d-1f
		dc.b "ASDFGHJKLöä£",0		;$20-$2c
		dc.b "456"			;$2d-$2f
		dc.b ">YXCVBNM;:_",0,"."	;$30-$3c
		dc.b "789"			;$3d-$3f
		dc.b " "
		dc.b 0				;$41 (backspace)
		dc.b 0				;$42 (Tab)
		dc.b 0,0			;$43 (Enter) $44 (Return)
		dc.b 0				;$45 (esc)
		dc.b 0				;$46 (del)
		dc.b 0,0,0,"-",0		;$47-$4b
		dc.b 0,0,0,0			;$4c-$4f arrows
		dc.b 0,0,0,0,0,0,0,0,0,0	;$50-$59 (F1-F10)
		dc.b "{}/*+"			;$5a-$5e
		dc.b 0				;$5f (Help)
		dc.b 0,0,0			;$60-$62 (Shift L,R CapsLock)
		dc.b 0				;$63 (CTRL)
		dc.b 0,0			;$64-$65 (Alt L,R)
		dc.b 0,0			;$66-$67 (Amiga L,R)

;Alt keyboard

board2a		dc.b '`+"#ç%&/()=?^|',0
		dc.b "0"			;$0f
		dc.b "QWERTZUIOPü!",0		;$10-$1c
		dc.b "123"			;1d-1f
		dc.b "ASDFGHJKLöä£",0		;$20-$2c
		dc.b "456"			;$2d-$2f
		dc.b ">YXCVBNM;:_",0,"."	;$30-$3c
		dc.b "789"			;$3d-$3f
		dc.b " "
		dc.b 0				;$41 (backspace)
		dc.b 0				;$42 (Tab)
		dc.b 0,0			;$43 (Enter) $44 (Return)
		dc.b 0				;$45 (esc)
		dc.b 0				;$46 (del)
		dc.b 0,0,0,"-",0		;$47-$4b
		dc.b 0,0,0,0			;$4c-$4f arrows
		dc.b 0,0,0,0,0,0,0,0,0,0	;$50-$59 (F1-F10)
		dc.b "{}/*+"			;$5a-$5e
		dc.b 0				;$5f (Help)
		dc.b 0,0,0			;$60-$62 (Shift L,R CapsLock)
		dc.b 0				;$63 (CTRL)
		dc.b 0,0			;$64-$65 (Alt L,R)
		dc.b 0,0			;$66-$67 (Amiga L,R)

;-------------- setmap USA ------------------------------------------

board3		dc.b "`1234567890-=",$5c,0
		dc.b "0"			;$0f
		dc.b "qwertyuiop[]",0		;$10-$1c
		dc.b "123"			;1d-1f
		dc.b "asdfghjkl;'$",0		;$20-$2c
		dc.b "456"			;$2d-$2f
		dc.b "<zxcvbnm,./",0,"."	;$30-$3c
		dc.b "789"			;$3d-$3f
		dc.b " "
		dc.b 0				;$41 (backspace)
		dc.b 0				;$42 (Tab)
		dc.b 0,0			;$43 (Enter) $44 (Return)
		dc.b 0				;$45 (esc)
		dc.b 0				;$46 (del)
		dc.b 0,0,0,"-",0		;$47-$4b
		dc.b 0,0,0,0			;$4c-$4f arrows
		dc.b 0,0,0,0,0,0,0,0,0,0	;$50-$59 (F1-F10)
		dc.b "[]/*+"			;$5a-$5e
		dc.b 0				;$5f (Help)
		dc.b 0,0,0			;$60-$62 (Shift L,R CapsLock)
		dc.b 0				;$63 (CTRL)
		dc.b 0,0			;$64-$65 (Alt L,R)
		dc.b 0,0			;$66-$67 (Amiga L,R)

;Shift keyboard

board4		dc.b '`!!#$%^&*()_+|',0
		dc.b "0"			;$0f
		dc.b "QWERTYUIOP{}",0		;$10-$1c
		dc.b "123"			;1d-1f
		dc.b 'ASDFGHJKL:"£',0		;$20-$2c
		dc.b "456"			;$2d-$2f
		dc.b ">ZXCVBNM<>?",0,"."	;$30-$3c
		dc.b "789"			;$3d-$3f
		dc.b " "
		dc.b 0				;$41 (backspace)
		dc.b 0				;$42 (Tab)
		dc.b 0,0			;$43 (Enter) $44 (Return)
		dc.b 0				;$45 (esc)
		dc.b 0				;$46 (del)
		dc.b 0,0,0,"-",0		;$47-$4b
		dc.b 0,0,0,0			;$4c-$4f arrows
		dc.b 0,0,0,0,0,0,0,0,0,0	;$50-$59 (F1-F10)
		dc.b "{}/*+"			;$5a-$5e
		dc.b 0				;$5f (Help)
		dc.b 0,0,0			;$60-$62 (Shift L,R CapsLock)
		dc.b 0				;$63 (CTRL)
		dc.b 0,0			;$64-$65 (Alt L,R)
		dc.b 0,0			;$66-$67 (Amiga L,R)

;Alt keyboard

board4a		dc.b '`!!#$%^&*()_+|',0
		dc.b "0"			;$0f
		dc.b "QWERTYUIOP{}",0		;$10-$1c
		dc.b "123"			;1d-1f
		dc.b 'ASDFGHJKL:"£',0		;$20-$2c
		dc.b "456"			;$2d-$2f
		dc.b ">ZXCVBNM<>?",0,"."	;$30-$3c
		dc.b "789"			;$3d-$3f
		dc.b " "
		dc.b 0				;$41 (backspace)
		dc.b 0				;$42 (Tab)
		dc.b 0,0			;$43 (Enter) $44 (Return)
		dc.b 0				;$45 (esc)
		dc.b 0				;$46 (del)
		dc.b 0,0,0,"-",0		;$47-$4b
		dc.b 0,0,0,0			;$4c-$4f arrows
		dc.b 0,0,0,0,0,0,0,0,0,0	;$50-$59 (F1-F10)
		dc.b "{}/*+"			;$5a-$5e
		dc.b 0				;$5f (Help)
		dc.b 0,0,0			;$60-$62 (Shift L,R CapsLock)
		dc.b 0				;$63 (CTRL)
		dc.b 0,0			;$64-$65 (Alt L,R)
		dc.b 0,0			;$66-$67 (Amiga L,R)

;-------------- setmap D ---------------------------------------

board5		dc.b "`1234567890ß´",$5c,0,"0"	;$00-$0f
		dc.b "qwertzuiopü+",0,"123"	;$10-$1f
		dc.b "asdfghjklöä#",0,"456"	;$20-$2f
		dc.b "<yxcvbnm,.-",0,".789"	;$30-$3f
		dc.b " "
		dc.b 0				;$41 (backspace)
		dc.b 0				;$42 (Tab)
		dc.b 0,0			;$43 (Enter) $44 (Return)
		dc.b 0				;$45 (esc)
		dc.b 0				;$46 (del)
		dc.b 0,0,0,"-",0		;$47-$4b
		dc.b 0,0,0,0			;$4c-$4f arrows
		dc.b 0,0,0,0,0,0,0,0,0,0	;$50-$59 (F1-F10)
		dc.b "[]/*+"			;$5a-$5e
		dc.b 0				;$5f (Help)
		dc.b 0,0,0			;$60-$62 (Shift L,R CapsLock)
		dc.b 0				;$63 (CTRL)
		dc.b 0,0			;$64-$65 (Alt L,R)
		dc.b 0,0			;$66-$67 (Amiga L,R)

;Shift keyboard

board6		dc.b '~!"§$%&/()=?`|',0,"0"	;$00-$0f
		dc.b "QWERTZUIOPÜ*",0,"123"	;$10-$1f
		dc.b "ASDFGHJKLÖÄ^",0,"456"	;$20-$2f
		dc.b ">YXCVBNM;:_",0,".789"	;$30-$3f
		dc.b " "
		dc.b 0				;$41 (backspace)
		dc.b 0				;$42 (Tab)
		dc.b 0,0			;$43 (Enter) $44 (Return)
		dc.b 0				;$45 (esc)
		dc.b 0				;$46 (del)
		dc.b 0,0,0,"-",0		;$47-$4b
		dc.b 0,0,0,0			;$4c-$4f arrows
		dc.b 0,0,0,0,0,0,0,0,0,0	;$50-$59 (F1-F10)
		dc.b "{}/*+"			;$5a-$5e
		dc.b 0				;$5f (Help)
		dc.b 0,0,0			;$60-$62 (Shift L,R CapsLock)
		dc.b 0				;$63 (CTRL)
		dc.b 0,0			;$64-$65 (Alt L,R)
		dc.b 0,0			;$66-$67 (Amiga L,R)

;Alt keyboard

board6a		dc.b "`¹@³°¼½¾·«»-=",$5c,0,"0"		;$00-$0f
		dc.b "å°©®þ¤µ¡ø¶[]",0,"123"		;$10-$1f
		dc.b "æßð",0,0,0,0,0,"£;'#",0,"456"	;$20-$2f
		dc.b "<±×çªº­¸,./",0,".789"		;$30-$3f
		dc.b " "
		dc.b 0				;$41 (backspace)
		dc.b 0				;$42 (Tab)
		dc.b 0,0			;$43 (Enter) $44 (Return)
		dc.b 0				;$45 (esc)
		dc.b 0				;$46 (del)
		dc.b 0,0,0,"-",0		;$47-$4b
		dc.b 0,0,0,0			;$4c-$4f arrows
		dc.b 0,0,0,0,0,0,0,0,0,0	;$50-$59 (F1-F10)
		dc.b "[]/*+"			;$5a-$5e
		dc.b 0				;$5f (Help)
		dc.b 0,0,0			;$60-$62 (Shift L,R CapsLock)
		dc.b 0				;$63 (CTRL)
		dc.b 0,0			;$64-$65 (Alt L,R)
		dc.b 0,0			;$66-$67 (Amiga L,R)

;-------------- setmap F ---------------------------------------

board7		dc.b '`&é"',"'(§è!çà)-",$5c,0
		dc.b "0"			;$0f
		dc.b "azertyuiop^$",0		;$10-$1c
		dc.b "123"			;1d-1f
		dc.b "qsdfghjklmuu",0		;$20-$2c
		dc.b "456"			;$2d-$2f
		dc.b "<wxcvbn,;:=",0,"."	;$30-$3c
		dc.b "789"			;$3d-$3f
		dc.b " "
		dc.b 0				;$41 (backspace)
		dc.b 0				;$42 (Tab)
		dc.b 0,0			;$43 (Enter) $44 (Return)
		dc.b 0				;$45 (esc)
		dc.b 0				;$46 (del)
		dc.b 0,0,0,"-",0		;$47-$4b
		dc.b 0,0,0,0			;$4c-$4f arrows
		dc.b 0,0,0,0,0,0,0,0,0,0	;$50-$59 (F1-F10)
		dc.b "[]/*+"			;$5a-$5e
		dc.b 0				;$5f (Help)
		dc.b 0,0,0			;$60-$62 (Shift L,R CapsLock)
		dc.b 0				;$63 (CTRL)
		dc.b 0,0			;$64-$65 (Alt L,R)
		dc.b 0,0			;$66-$67 (Amiga L,R)

;Shift keyboard

board8		dc.b "`1234567890°_|",0
		dc.b "0"			;$0f
		dc.b "AZERTYUIOP",0,"*",0	;$10-$1c
		dc.b "123"			;1d-1f
		dc.b "QSDFGHJKLM%£",0		;$20-$2c
		dc.b "456"			;$2d-$2f
		dc.b ">WXCVBN?./+",0,"."	;$30-$3c
		dc.b "789"			;$3d-$3f
		dc.b " "
		dc.b 0				;$41 (backspace)
		dc.b 0				;$42 (Tab)
		dc.b 0,0			;$43 (Enter) $44 (Return)
		dc.b 0				;$45 (esc)
		dc.b 0				;$46 (del)
		dc.b 0,0,0,"-",0		;$47-$4b
		dc.b 0,0,0,0			;$4c-$4f arrows
		dc.b 0,0,0,0,0,0,0,0,0,0	;$50-$59 (F1-F10)
		dc.b "{}/*+"			;$5a-$5e
		dc.b 0				;$5f (Help)
		dc.b 0,0,0			;$60-$62 (Shift L,R CapsLock)
		dc.b 0				;$63 (CTRL)
		dc.b 0,0			;$64-$65 (Alt L,R)
		dc.b 0,0			;$66-$67 (Amiga L,R)

;Alt keyboard

board8a		dc.b "`12#4567890°_|",0
		dc.b "0"			;$0f
		dc.b "AZERTYUIOP",0,"*",0	;$10-$1c
		dc.b "123"			;1d-1f
		dc.b "QSDFGHJKLM%£",0		;$20-$2c
		dc.b "456"			;$2d-$2f
		dc.b ">WXCVBN?./+",0,"."	;$30-$3c
		dc.b "789"			;$3d-$3f
		dc.b " "
		dc.b 0				;$41 (backspace)
		dc.b 0				;$42 (Tab)
		dc.b 0,0			;$43 (Enter) $44 (Return)
		dc.b 0				;$45 (esc)
		dc.b 0				;$46 (del)
		dc.b 0,0,0,"-",0		;$47-$4b
		dc.b 0,0,0,0			;$4c-$4f arrows
		dc.b 0,0,0,0,0,0,0,0,0,0	;$50-$59 (F1-F10)
		dc.b "{}/*+"			;$5a-$5e
		dc.b 0				;$5f (Help)
		dc.b 0,0,0			;$60-$62 (Shift L,R CapsLock)
		dc.b 0				;$63 (CTRL)
		dc.b 0,0			;$64-$65 (Alt L,R)
		dc.b 0,0			;$66-$67 (Amiga L,R)

