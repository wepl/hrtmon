;APS00000000000000000000000000000000000000000000000000000000000000000000000000000000
;
; $Id: HRTmonV2.s 1.6 2000/11/24 21:58:53 jah Exp jah $
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


*******************************
**        HRTmon v2.26       **
*******************************
**   code by Alain Malek    ***
*******************************

VER_MAJ equ 2
VER_MIN equ 26

***********************************************************

	IFD BARFLY
		MC68030
		SUPER			;disable supervisor warnings
		BOPT w4-		;disable 64k warnings
		OUTPUT "hrtmon.data"

	incdir includes:
	include whdload.i
	include whdmacros.i

OPT_OFF		MACRO
		BOPT OD6-		;disable branch optimizing
		ENDM
OPT_ON		MACRO
		BOPT OD6+		;enable branch optimizing
		ENDM

	ENDC

	IFD _PHXASS_
		MC68030
		OPT NBL

OPT_OFF		MACRO
		OPT 0
		ENDM
OPT_ON		MACRO
		OPT NBL
		ENDM

	ENDC

	IFND BARFLY
	IFND _PHXASS_

MC68000	MACRO
	ENDM
MC68010	MACRO
	ENDM
MC68020	MACRO
	ENDM
MC68030	MACRO
	ENDM
MC68040	MACRO
	ENDM
MC68060	MACRO
	ENDM

OPT_ON	MACRO
	ENDM
OPT_OFF	MACRO
	ENDM

	ENDC
	ENDC

***********************************************************

version	macro
	dc.b '0'+VER_MAJ,'.','0'+VER_MIN/10,'0'+VER_MIN-(VER_MIN/10*10)
	endm

SC_STEP equ 16		;fast scrolling step

PICSIZE equ $9800	;size of mem used for bitmap ( > MAX_SCREEN*h*80 )

MAX_SCREEN equ 52


	incdir include:
	include exec/tasks.i
	include hardware/custom.i

***********************************************************

	include exec/types.i
	include exec/execbase.i
	include devices/hardblocks.i

	incdir hrt:

***********************************************************
;-------------- MACRO same as reloc_pic subroutine --------

RELOC_PIC:	macro
		cmp.l	#$40000,a4
		bcs.b	.low\1
;-------------- bitmap needs to be in $20000 ---------
		cmp.l	#$20000,pic_ptr
		beq.b	.ok_corr\1
		bra.b	.doreloc\1

;-------------- bitmap needs to be in $60000 ---------
.low\1		cmp.l	#$60000,pic_ptr
		beq.b	.ok_corr\1
.doreloc\1	jsr	reloc_pic
.ok_corr\1
		endm

***********************************************************

start		;bra.w	monitor
		moveq	#0,d0
		rts
		dc.b "HRT!"		;4

	OPT_OFF
		bra.w	mon_install	;8  jmp to install routine
		bra.w	monitor		;12 jmp to monitor
		bra.w	mon_remove	;16 jmp to remove routine
	OPT_ON
mon_size	dc.l 0			;20 size of HRTmon (for FreeMem)
color0		dc.w 0			;24 color0
color1		dc.w $eee		;26 color1
config_RM	dc.b 0			;28 -1=right-mouse enter on 0=off
config2		dc.b 0			;29 no of keyboard to use
config_key	dc.b 0			;30 -1= \ key enter on 0=off
config_IDE	dc.b 0			;31 -1= IDE harddisk
config_A1200	dc.b 0			;32 -1= A1200
config_AGA	dc.b 0			;33 -1= AGA
config_insert	dc.b 0			;34 -1= insert
config_delay	dc.b 15			;35 repeat key delay (default = 15)
config_lview	dc.b 0			;36 -1= lview task
config_elsat	dc.b 0			;37 -1=elsat.device IDE (CD32)
config_screen	dc.b 0			;38 screen mode no 0=PAL,1=NTSC,2=MULTISCAN
config_novbr	dc.b 0			;39 0=move vbr, -1=don't move vbr
entered		dc.b 0			;40 bit 0 set if in HRTmon
config_hexmode	dc.b 0			;41 hexmode
error_sr	dc.w 0			;42 error sr
error_pc	dc.l 0			;44 error pc
error_status	dc.w 0			;48 error status
		dc.b "NEWHRT"		;50 id string used by WHDLoad to distinguish from older ones
mon_version	dc.w VER_MAJ		;56 version of HrtMon
mon_revision	dc.w VER_MIN		;58 revision of HrtMon
whd_base	dc.l 0			;60 resload base if HrtMon is called under WHDLoad
whd_version	dc.w 0			;64 version of WHDLoad
whd_revision	dc.w 0			;66 revision of WHDLoad
max_chip	dc.l 0			;68 maximum value for CHIP-RAM
		even


mon_remove	movem.l	d1-a6,-(a7)
		move.l	$4.w,a6
		lea.l	.super(pc),a5
		jsr	-30(a6)

		move.l	mon_size(pc),d0
		beq.b	.nofree
		lea.l	start(pc),a1
		jsr	-210(a6)	;FreeMem
.nofree
		tst.b	config_lview
		beq.b	.nolview
		move.l	$4.w,a6
		move.l	gfxbase(pc),a1
		jsr	-414(a6)

		lea.l	tc_block(pc),a1
		jsr	-288(a6)	;RemTask

.nolview
		movem.l	(a7)+,d1-a6
		moveq	#0,d0
		rts

.super
		sub.l	a4,a4
		tst.w	proc_type
		beq.b	.go68000
		movec	vbr,a4
.go68000
		move.l	a4,a3

		lea.l	backup_vbr,a0
		move.w	#$400/4-1,d0
.loop		move.l	(a4),a1
		cmp.l	#start,a1	;check if vector points in HRTmon
		bcs.b	.norestore
		cmp.l	#end,a1
		bcc.b	.norestore
		move.l	(a0),(a4)
.norestore	addq.l	#4,a0
		addq.l	#4,a4
		dbf	d0,.loop

		cmp.l	#new_except,a3
		bne.b	.novbr
		move.l	old_vbr(pc),a3
		movec	a3,vbr
.novbr
		rte


mon_install	movem.l	d1-a6,-(a7)

		lea.l	start(pc),a0

		move.w	#28,screen_height
		cmp.b	#1,config_screen
		bne.b	.nontsc
		move.w	#22,screen_height
.nontsc		cmp.b	#2,config_screen
		bne.b	.nomulti
		move.w	#52,screen_height
.nomulti

		move.w	#MAX_SCREEN,d2
		sub.w	screen_height,d2
		mulu	#80,d2
		move.l	d2,d1
		add.l	#ascII_page1,d1
		move.l	d1,ascII_ptr
		move.l	d1,ascII_ptr+4
		move.l	d2,d1
		add.l	#ascII_page2,d1
		move.l	d1,ascII_ptr+8
		move.l	d2,d1
		add.l	#ascII_Tracer,d1
		move.l	d1,ascII_ptr+12

		move.w	screen_height,d1
		subq.w	#4,d1
		move.w	d1,window_bot

		move.l	a0,location
		move.l	a0,location2

		tst.b	config_hexmode
		sne	hex_default

		move.l	$4.w,a6
		move.l	MaxLocMem(a6),max_chip

		tst.b	config_IDE
		beq.w	.noide

		tst.b	config_elsat
		beq.b	.noelsat

		move.l	#$eb8000,ide_base
		clr.l	ide_irq
		move.w	#$0,ide_data
		move.w	#$100,ide_error
		move.w	#$100,ide_feature
		move.w	#$200,ide_secnt
		move.w	#$300,ide_secnb
		move.w	#$400,ide_cyllo
		move.w	#$500,ide_cylhi
		move.w	#$600,ide_dhead
		move.w	#$700,ide_status
		move.w	#$700,ide_command
		move.w	#$4600,ide_altstat
		bra.w	.noide

.noelsat	move.w	#$0,ide_data
		move.w	#$4,ide_error
		move.w	#$4,ide_feature
		move.w	#$8,ide_secnt
		move.w	#$c,ide_secnb
		move.w	#$10,ide_cyllo
		move.w	#$14,ide_cylhi
		move.w	#$18,ide_dhead
		move.w	#$1c,ide_status
		move.w	#$1c,ide_command
		move.w	#$1c,ide_altstat

		move.l	#$da2000,ide_base
		move.l	#$da9000,ide_irq
		tst.b	config_A1200		;A1200 ?
		bne.b	.noide
		move.l	#$dd2020,ide_base
		move.l	#$dd3020,ide_irq
.noide
		move.b	config_insert(pc),insert_mode

		moveq	#0,d0
		move.b	config2,d0		;keyboard map
		mulu	#12,d0
		movem.l	.map(pc,d0.l),d0-d2
		movem.l	d0-d2,board_ptr

		tst.b	config_lview
		beq.b	.nolview
		lea.l	gfxname(pc),a1
		moveq	#0,d0
		move.l	$4.w,a6
		jsr	-552(a6)
		move.l	d0,gfxbase

		move.l	#lview_name,tc_block+LN_NAME
		move.b	#NT_TASK,tc_block+LN_TYPE
		move.b	#8,tc_block+LN_PRI
		move.l	#task_stackend,tc_block+TC_SPREG
		move.l	#task_stack,tc_block+TC_SPLOWER
		move.l	#task_stackend,tc_block+TC_SPUPPER

		lea.l	tc_block(pc),a1
		lea.l	task_code(pc),a2
		sub.l	a3,a3
		jsr	-282(a6)		;AddTask

.nolview
		lea.l	.super(pc),a5
		jsr	-30(a6)

		movem.l	(a7)+,d1-a6
		moveq	#0,d0
		rts

		cnop 0,4

.map		dc.l board3, board4, board4a
		dc.l board1, board2, board2a
		dc.l board5, board6, board6a
		dc.l board7, board8, board8a


.super		bsr	test_CPU
		sub.l	a4,a4
		move.w	d0,proc_type
		beq.b	.no_vbr
		movec	vbr,a4
.no_vbr		move.l	a4,old_vbr
		move.l	a4,a0
		lea.l	backup_vbr,a1
		move.w	#$400/4-1,d0
.copy		move.l	(a0)+,(a1)+	;backup vector table
		dbf	d0,.copy

		tst.b	config_novbr	;user doesn't want to move vbr ?
		bne.b	.no_vbr_move
		move.l	a4,d0
		bne.b	.no_vbr_move	;vbr already moved ?
		tst.w	proc_type
		beq.b	.no_vbr_move	;can't move vbr (68000) ?

;move vbr
		lea.l	backup_vbr,a0
		lea.l	new_except,a1
		move.l	a1,a4
		move.w	#$400/4-1,d0
.copy2		move.l	(a0)+,(a1)+
		dbf	d0,.copy2
		movec	a4,vbr

.no_vbr_move
		move.l	#monitor,$7c(a4)

		tst.b	config_RM
		bne.b	.okRM
		tst.b	config_key
		beq.b	.no_entry
.okRM
		lea.l	$60(a4),a4
		moveq	#0,d1
		lea.l	except_entry(pc),a1
		tst.w	proc_type
		bne.b	.go10plus
		moveq	#4,d1
		lea.l	except_entry0(pc),a1
.go10plus	moveq	#7-1,d0
.loop		move.l	a1,(a4)+
		add.l	d1,a1
		dbf	d0,.loop

.no_entry
		rte



		cnop	0,4
gfxbase		dc.l 0
wbview		dc.l 0
signal_no	dc.l 0
changed_disk	dcb.l 16,0
gfxname		dc.b "graphics.library",0
;dosname		dc.b "dos.library",0
lview_name	dc.b "HRTmon system task",0

		cnop	0,4
tc_block	dcb.b TC_SIZE,0
task_stack	dcb.b $400,0
task_stackend

task_code
		move.l	$4.w,a6
		moveq	#-1,d0
		jsr	-330(a6)		; AllocSignal
		move.l	d0,signal_no

.loop		move.l	signal_no(pc),d0
		move.l	$4.w,a6
		jsr	-318(a6)		; Wait

		move.l	gfxbase(pc),a6

		move.l	wbview(pc),a1
		jsr	-222(a6) 		; LoadView Fix view
		move.l	38(a6),$dff080		; Kick it into life


	bra.b	.loop

;experimental code removed should be done from a PROCESS

;		move.l	$4.w,a6
;		lea.l	dosname(pc),a1
;		moveq	#0,d0
;		jsr	-552(a6)
;		tst.l	d0
;		beq.b	.nodos
;		move.l	d0,a6
;
;		lea.l	changed_disk(pc),a4
;		moveq	#16-1,d7
;.next		move.l	(a4)+,d0
;		beq.b	.empty
;
;		move.l	d0,a0
;		lea.l	part_name2(a0),a0
;		moveq	#0,d0
;		lea.l	.tmp_name(pc),a1
;		move.b	(a0)+,d0
;.copy		move.b	(a0)+,(a1)+
;		dbf	d0,.copy
;		move.b	#':',(a1)+
;		sf	(a1)+
;
;		clr.l	-4(a4)
;
;		move.l	d7,-(a7)
;
;		move.l	#.tmp_name,d1
;		jsr	-174(a6)		; DeviceProc
;		tst.l	d0
;		beq.b	.err
;		move.l	d0,a3
;		move.l	a3,d1
;		moveq	#$1f,d2
;		moveq	#-1,d3
;		moveq	#0,d4
;		moveq	#0,d5
;		moveq	#0,d6
;		moveq	#0,d7
;		jsr	-240(a6)		; DoPkt
;		tst.l	d0
;		beq.b	.err
;		move.l	a3,d1
;		moveq	#$1f,d2
;		moveq	#0,d3
;		moveq	#0,d4
;		moveq	#0,d5
;		moveq	#0,d6
;		moveq	#0,d7
;		jsr	-240(a6)		; DoPkt
;.err
;		move.l	(a7)+,d7
;
;.empty		dbf	d7,.next
;
;		move.l	a6,a1
;		move.l	$4.w,a6
;		jsr	-414(a6)
;
;.nodos
;		bra.w	.loop
;
;		rts

;.tmp_name	dcb.b	34,0

		cnop	0,4
oldlev7		dc.l 0
old_vbr		dc.l 0

;-------------- interrupt routine to catch 'key' press and right-mouse
;-------------- this routine is used only on 68000

except_entry0
		OPT_OFF
		bsr.w	.entry
		bsr.w	.entry
		bsr.w	.entry
		bsr.w	.entry
		bsr.w	.entry
		bsr.w	.entry
		bsr.w	.entry
		OPT_ON

.entry
		tst.b	config_RM
		beq.b	.noright
		btst	#10-8,$dff016
		bne.b	.noright
		addq.l	#4,a7
		bra.w	monitor
.noright
		tst.b	config_key
		beq.b	.nokey
		cmp.b	#$e5,$bfec01
		bne.b	.nokey
		addq.l	#4,a7
		bra.w	monitor
.nokey
		move.w	sr,-(a7)
		move.w	#$2700,sr
		move.l	2(a7),.retaddr

		move.l	a0,2(a7)

		sub.l	#except_entry0+4,.retaddr
		lea.l	backup_vbr,a0
		add.l	.retaddr,a0
		move.l	$60(a0),.jmp_addr

		move.l	2(a7),a0

		move.l	.jmp_addr(pc),2(a7)
		rte

		cnop 0,4
.retaddr	dc.l 0
.jmp_addr	dc.l 0


;-------------- interrupt routine to catch 'key' press and right-mouse
;-------------- this routine is used only on 68010+

except_entry	tst.b	config_RM
		beq.b	.noright
		btst	#10-8,$dff016
		beq.w	monitor
.noright
		tst.b	config_key
		beq.b	.nokey
		cmp.b	#$e5,$bfec01
		beq.w	monitor
.nokey
		move.w	sr,-(a7)
		move.w	#$2700,sr
		move.w	(a7)+,.newsr

		movem.l	d0/a0,-(a7)

		move.w	8+6(a7),d0
		and.w	#$fff,d0
		lea.l	backup_vbr,a0
		add.w	d0,a0
		move.l	(a0),.jmp_addr

		movem.l	(a7)+,d0/a0

		clr.w	-(a7)
		move.l	.jmp_addr(pc),-(a7)
		move.w	.newsr(pc),-(a7)
		rte

		cnop 0,4
.jmp_addr	dc.l 0
.newsr		dc.w 0

		cnop 0,16

location	dc.l 0			;HRTmon location
		dc.b "HRT!"
new_except	dcb.l	$100,0

		dc.b "ATZ!"
		dc.l monitor			;trap vector

		dc.b "HRTmon "
		version
		dc.b " "
		dc.b "by Alain Malek "
		dc.b "(Hornet of Alcatraz) "

		cnop 0,4

pic_ptr		dc.l $20000		;address used for the bitplan of HRTmon
h=9					;heights between two lines

;-------------- init. HRTmon when entered for the 1st time ----

init_code	movem.l	d0-a6,-(a7)

		move.l	#$12345678,d0
		tst.w	Reset_Flag
		bne.b	.init
		cmp.l	inited,d0
		beq.w	.noinit
.init		move.l	d0,inited

		jsr	init_ascII

		jsr	test_drive	;test which floppy drive is present

		tst.b	drive_present+1
		beq.b	.noDF1
		move.l	#floppy1,floppy0	;add DF1: partition
.noDF1

		move.w	proc_type,d0
		add.b	#'0',d0
		move.b	d0,mc_txt+5

		jsr	init_fbuffer

		tst.b	config_IDE
		beq.b	.noide

		move.l	ide_irq,d0
		beq.b	.noIDEirq
		move.l	d0,a3
		tst.b	(a3)
		bpl.b	.noIDEirq
		move.l	ide_base,a3
		tst.b	config_A1200
		bne.b	.go1200
		move.w	ide_status,d0
		move.b	(a3,d0.w),d0
		bra.b	.noIDEirq
.go1200		jsr	wait_irq		;remove IRQ request from IDE
.noIDEirq

		jsr	find_part
.noide

;set default values for some write only custom registers

		lea.l	custom,a1
		move.w	#$4489,$7e(a1)
		move.w	#$20,$1dc(a1)
		move.w	#$3081,$8e(a1)
		move.w	#$30c1,$90(a1)
		move.w	#$0038,$92(a1)
		move.w	#$00d0,$94(a1)
		move.w	#0,$108(a1)
		move.w	#0,$10a(a1)
		move.w	#0,$106(a1)
		move.w	#0,$1fc(a1)
		move.w	#$9200,$100(a1)
	;	move.w	#0,$102(a1)		;avoid unneccessary gfx corruption


.noinit		movem.l	(a7)+,d0-a6
		rts


;test if exec is here and valid
;d0=0 ok, -1 = no system

exec_here	movem.l	d1/a0-a1,-(a7)

		move.l	(whd_base),d0
		bne	.nosafe

		move.l	$4.w,a0
		move.l	a0,d0
		lsr.l	#1,d0
		bcs.b	.nosafe
		lea.l	34(a0),a1
		moveq	#0,d0
		moveq	#$18-1,d1
.add		add.w	(a1)+,d0
		dbf	d1,.add
		not.w	d0
		cmp.w	82(a0),d0
		bne.b	.nosafe

		moveq	#0,d0
		bra.b	.ok

.nosafe		moveq	#-1,d0
.ok		movem.l	(a7)+,d1/a0-a1
		rts

		cnop 0,4

;-------------- when in HRTmon VBR points here -----------------

HRTmon_vbr
		dc.l error,error,recover,recover;$00-$0c
		dc.l recover,recover,error,error;$10-$1c
		dc.l error,error,error,error	;$20-$2c
		dc.l error,error,error,error	;$30-$3c
		dc.l error,error,error,error	;$40-$4c
		dc.l error,error,error,error	;$50-$5c
		dc.l error,error	;$60,$64
		dc.l error		;$68
		dc.l newirq		;$6c		;VBL irq
		dc.l error		;$70
		dc.l error		;$74
		dc.l error		;$78
		dc.l .onlyret		;$7c
		dc.l error,error,error,error	;$80-$8c
		dc.l error,error,error,error	;$90-$9c
		dc.l error,error,error,error	;$a0-$ac
		dc.l error,error,error,error	;$b0-$bc
		dc.l error,error,error,error	;$c0-$cc
		dc.l error,error,error,error	;$d0-$dc
		dc.l error,error,error,error	;$e0-$ec
		dc.l error,error,error,error	;$f0-$fc

.onlyret	rte

;-------------- jmp here if an unexpected exception is raised ----------
;-------------- (should never happen !) --------------------------------

error		move.w	#$2700,sr
		move.l	2(a7),error_pc
		move.w	(a7),error_sr
		move.w	6(a7),error_status
		move.w	#0,$dff106
		move.w	#$f00,$dff180		;red flash
		rte

;-------------- jmp here if an unexpected exception is raised ----------
;-------------- will try to recover from error  ------------------------

recover		move.w	#$2700,sr
		tst.b	cmd_executed
		beq.b	error
		move.l	a7,a0
		lea.l	crash_stack,a1
		move.w	#$100/2-1,d0
.copy		move.w	(a0)+,(a1)+
		cmp.l	#stack,a0
		dbhs	d0,.copy
		st	cmd_crashed
		bra.w	end_command

		cnop 0,4

**************************************************************************
;-------------- Entry point of HRTmon ------------------------------------

		cnop 0,4

location2	dc.l 0			;HRTmon location
		dc.b "HRT!"
monitor:	or	#$700,sr	;on 68060 it is granted that the first instruction
					;is executed before any pending interupts, so we 
					;stop them to avoid any confusing
		bset	#0,entered
		beq.b	.enter
		rte				;don't enter twice

.enter		move.w	#$2700,sr

		movem.l	d0-a7,registres

		move.l	2(a7),d0		;get PC
		cmp.l	#start,d0
		bcs.b	.notin
		cmp.l	#end,d0
		bcc.b	.notin
		move.l	registres,d0
		clr.b	entered
		rte
.notin
		lea.l	stack,a7		;Own stack
						;now bsr are allowed

		bsr	ClearCache

		tst.b	config_lview
		beq.b	.nolview
		bsr	exec_here
		bne.b	.nolview
		move.l	gfxbase(pc),a6
		move.l	34(a6),d0		;get actual view
		beq.b	.nolview
		move.l	d0,wbview
		sub.l	a1,a1
		jsr	-222(a6)		;LoadView(NULL)
		move.l	signal_no(pc),d0
		lea.l	tc_block(pc),a1
		move.l	$4.w,a6
		jsr	-324(a6)		;Signal
.nolview

		bsr	init_code

		bsr	ClearCache

		cmp.w	#4,proc_type
		blt.b	.notrans0
	MC68040
		movec	itt0,d0
		move.l	d0,itt0_reg
		movec	itt1,d0
		move.l	d0,itt1_reg
		movec	dtt0,d0
		move.l	d0,dtt0_reg
		movec	dtt1,d0
		move.l	d0,dtt1_reg
		movec	tc,d0
		move.l	d0,tc_reg
		cpusha	bc
		cinva	bc
		dc.w $f518		;pflusha
	MC68030
.notrans0

		lea.l	$dff000,a6
		move.l	$4(a6),OldRaster	;save raster pos
						;(wrong coz of .waittrap !)

.blit		btst	#6,($2,a6)
		bne.b	.blit			;wait end of blitter operation

		move.l	a7_reg,a0
		move.w	(a0),sr_reg
		move.l	2(a0),pc_reg

		btst	#5,$bfe001		;need to wait end of DMA disk ?
		bne.b	.no_drive
		move.l	#$20000,d1
.loop		subq.l	#1,d1
		beq.b	.no_drive
		move.w	$1e(a6),d0		;wait for DMAdisk IRQ
		and.w	#2,d0
		beq.b	.loop
.no_drive
		move.l	USP,a0
		move.l	a0,usp_reg
		move.l	a7_reg,ssp_reg
		move.w	sr_reg,d0
		and.w	#$2000,d0
		bne.b	ok_ssp_a7
		move.l	a0,a7_reg
ok_ssp_a7
		clr.l	vbr_reg
		cmp.w	#1,proc_type
		blt.b	.novbr
		movec	VBR,d0
		move.l	d0,vbr_reg
.novbr
		cmp.w	#2,proc_type
		blt.w	.no23
		movec	CACR,d0
		move.l	d0,cacr_reg
		moveq	#0,d0
		cmp.w	#6,proc_type
		bne.b	.no60
		move.l	#'BUSR',CAAR_txt
		dc.w	$4e7a,$0008		;movec	BUSCR,d0
		bra.b	.nocaar
.no60		cmp.w	#4,proc_type
		bge.b	.nocaar
		movec	CAAR,d0
.nocaar		move.l	d0,caar_reg
		move.l	ssp_reg,isp_reg
		move.l	ssp_reg,d0
		cmp.w	#6,proc_type		;68060 ?
		beq.b	.nomsp
		movec	MSP,d0
		bra.b	.okmsp
.nomsp		move.l	#'PCR=',MSP_txt
		dc.w	$4e7a,$0808		;movec	PCR,d0
.okmsp		move.l	d0,msp_reg

		cmp.w	#4,proc_type
		bge.b	.no23
		or.w	#$2111,d0		;switch all CACHE ON
		movec	d0,CACR			;(68020/68030 only)
.no23

	;wepl: if whdload is active don't change any vectors except the vbi
	;which is used by hrt, so we will get a real message if something
	;crashes
		tst.l	(whd_base)
		beq	.nowhd
	MC68010
		movec	vbr,a0
	MC68000
		move.l	($6c,a0),oldvbi
		move.l	#newirq,($6c,a0)
		bra	.anowhd
.nowhd		bsr	enter_vbr
.anowhd
		bsr	save_custom

		bsr	save_CIA

		move.l	#-1,trace_address

;-------------- don't trace subroutine in tracer ? --------

		tst.b	trace_bsr
		beq.b	.no_bsr
		tst.w	trace_count
		beq.b	.oky
		clr.w	trace_count
		sf	trace_bsr
		bra.b	.no_bsr
.oky
		move.w	sr_reg,d0

		move.l	a7_reg,a0
		and.w	#$2000,d0
		beq.b	.no_super
		addq.l	#6,a0			;skip SR and PC

		move.w	(a0),d0			;read stack frame no
		lsr.w	#4,d0
		lsr.w	#8,d0
		lea.l	frame(pc),a4
	MC68020
		add.w	(a4,d0.w*2),a0		;add stack frame size
	MC68000
.no_super	move.l	(a0),tracer_old_rts
		move.l	#.tracer_rts,(a0)
		move.l	vbr_reg,a4
		move.l	#.tracer_ill,$10(a4)
		sf	trace_bsr
		bra.b	.out
.tracer_rts	illegal
.tracer_ill	move.l	tracer_old_rts,2(a7)
		bra.w	monitor

.no_bsr
;-------------------------------------------

		bsr	ClearCache

		bsr.w	super			;enter main code

.out		move.w	#$7fff,d0
		move.w	d0,$9a(a6)
		move.w	d0,$96(a6)
		move.w	d0,$9c(a6)

		bsr	restore_CIA
		bsr	restore_custom

		bsr	init_mwatch

		cmp.w	#1,proc_type
		ble.b	.okmsp2
		cmp.w	#6,proc_type		;68060 ?
		beq.w	.nomsp2
		move.l	msp_reg,a0
	MC68030
		movec	a0,MSP
	MC68000
		bra.b	.okmsp2
.nomsp2		move.l	msp_reg,a0
		dc.w	$4e7b,$8808		;movec	a0,PCR
.okmsp2
		move.l	usp_reg,a0
		move.l	a0,USP

		move.l	ssp_reg,a7
		move.w	sr_reg,(a7)

		move.l	vbr_reg,a4

		tst.w	trace_count
		beq.b	trace_off
		or.w	#$8000,(a7)
		move.l	$24(a4),old_trace
		lea.l	trace_reach(pc),a0
		move.l	a0,$24(a4)

trace_off	cmp.l	#-1,trace_address
		beq.b	tracea_off
		or.w	#$8000,(a7)
		move.l	$24(a4),old_trace
		lea.l	tracea_reach(pc),a0
		move.l	a0,$24(a4)
tracea_off
		move.l	pc_reg,2(a7)

		bsr	ClearCache
		cmp.w	#6,proc_type
		bne.b	.no60
		move.l	caar_reg,a0
		dc.w	$4e7b,$8008		;movec a0,BUSCR
.no60		cmp.w	#1,proc_type
		ble.b	.nocache
		cmp.w	#4,proc_type
		bge.b	.nocaar2
	MC68030
		move.l	caar_reg,a0
		movec	a0,CAAR
.nocaar2	move.l	cacr_reg,d0
		movec	d0,CACR			;restore CACR
	MC68000
.nocache
		cmp.w	#4,proc_type
		blt.b	.notrans
	MC68040
		move.l	itt0_reg,d0
		movec	d0,itt0
		move.l	itt1_reg,d0
		movec	d0,itt1
		move.l	dtt0_reg,d0
		movec	d0,dtt0
		move.l	dtt1_reg,d0
		movec	d0,dtt1
		cpusha	bc
		cinva	bc
		dc.w $f518		;pflusha
	MC68000
.notrans

		move.l	custom+$4,d0
		lsr.l	#1,d0
		lsr.w	#7,d0
.raster		move.l	$4(a6),d1		;wait raster pos
		lsr.l	#1,d1			;for exit
		lsr.w	#7,d1
		cmp.w	d0,d1
;		bne.b	.raster

		tst.b	reboot
		beq.b	.noreboot
		sf	reboot
		lea.l	$48000,a0
		lea.l	start,a1
		move.l	#(end-start),d0
		lsr.l	#3,d0
.copy		move.l	(a1)+,(a0)+		;copy HRTmon to $48000
		move.l	(a1)+,(a0)+
		dbf	d0,.copy

.noreboot
		tst.b	kill
		beq.b	.nokill
	MC68030
		tst.w	proc_type
		beq.b	.okkill
		move.l	old_vbr(pc),a0
		movec	a0,VBR
	MC68000
		bra.b	.okkill

.nokill
	;wepl: if whdload is active we haven't init different, therefore...
		tst.l	(whd_base)
		beq	.nowhd
	MC68010
		movec	vbr,a0
	MC68000
		move.l	oldvbi,($6c,a0)
		bra	.anowhd
.nowhd		bsr	exit_vbr
.anowhd

.okkill		movem.l	registres,d0-d7/a0-a6
		sf	entered

simple_ret	rte

oldvbi		dc.l	0

*************************************************************

clean_vbr	movem.l	d0/a0-a1/a4,-(a7)

;remove all HRTmon ptr in vector table
		move.l	vbr_reg,a4
		lea.l	backup_vbr,a0
		move.w	#$400/4-1,d0
.loop		move.l	(a4),a1
		cmp.l	#start,a1	;check if vector points in HRTmon
		bcs.b	.norestore
		cmp.l	#end,a1
		bcc.b	.norestore
		move.l	(a0),(a4)
.norestore	addq.l	#4,a0
		addq.l	#4,a4
		dbf	d0,.loop

;update vector table backup
		lea.l	backup_vbr,a1
		move.l	vbr_reg,a0
		move.w	#$400/4-1,d0
.copy		move.l	(a0)+,(a1)+
		dbf	d0,.copy

		movem.l	(a7)+,d0/a0-a1/a4
		rts

*************************************************************

enter_vbr	movem.l	d0-a6,-(a7)

		bsr	clean_vbr

		tst.w	proc_type
		bne.b	.no68000

;		move.l	#newirq,$6c.w

		bra.b	.ok68000

.no68000	lea.l	HRTmon_vbr(pc),a0
	MC68030
		movec	a0,vbr
	MC68000

.ok68000	movem.l	(a7)+,d0-a6
		rts

*************************************************************

exit_vbr	tst.w	proc_type
		bne.b	.no68000

;		move.l	backup_vbr+$6c,$6c.w

		lea.l	$60.w,a0
		lea.l	except_entry0(pc),a1
		moveq	#7-1,d0
.loop0		move.l	a1,(a0)+
		addq.l	#4,a1
		dbf	d0,.loop0

		bra.b	.ok68000

.no68000	move.l	vbr_reg,a0
	MC68030
		movec	a0,vbr
	MC68000

		tst.b	config_RM
		bne.b	.okRM
		tst.b	config_key
		beq.b	.ok68000
.okRM
		lea.l	$60(a0),a0
		lea.l	except_entry(pc),a1
		moveq	#7-1,d0
.loop		move.l	a1,(a0)+
		dbf	d0,.loop

.ok68000	move.l	vbr_reg,a0
		move.l	#monitor,$7c(a0)
		rts

*************************************************************

;Clear instruction and data cache of 68000-68060

ClearCache	move.l	d0,-(a7)
		cmp.w	#1,proc_type	;no flush for 68000 and 68010
		bls.b	.end
		cmp.w	#4,proc_type
		bge.b	.cic_040

	MC68030
		movec	CACR,d0
		cmp.w	#3,proc_type
		bne.b	.no30
		or.w	#$0800,d0
.no30		or.w	#$8,d0
		movec	d0,CACR
		bra.b	.end
	MC68000
.cic_040
	MC68040
		cpusha	BC
		cinva	BC
	MC68000
.end
		move.l	(a7)+,d0
		rts

***********************************************************
;test if 68020,30,40,or 60 processor (by wepl)

;<- d0=processor type 1=68010,2=68020,3=68030,4=68040,6=68060

test_CPU	movem.l	d1/d5-d7/a2-a5,-(a7)
		moveq	#0,d0		;D0 = processor
		lea	($10).w,a4	;A4 = $10 ... illegal instruction
		move.l	(a4),d7		;D7 = ($10)
		lea	(.illegal,pc),a2
		move.l	a2,(a4)
		lea	.1,a2		;A2 = return
	MC68010
		movec	vbr,d1
		moveq	#1,d0
		lea	(a4,d1.l),a3	;A3 = vbr+$10 illegal instruction
		move.l	(a3),d6		;D6 = (vbr+$10)
		move.l	(a4),(a3)
		lea	($2c-$10,a3),a5	;A5 = vbr+$2c line-f emulator
		move.l	(a5),d5		;D5 = (vbr+$2c)
		move.l	(a4),(a5)
		lea	(.2,pc),a2	;A2 = return
	MC68020
		movec	cacr,d1
		moveq	#2,d0
		lea	(.3,pc),a2
	MC68030
		dc.w	$f02f,$0a00,$fffc	;pmove	tt0,(-4,a7)
		moveq	#3,d0
.3		lea	(.2,pc),a2
	MC68040
		movec	dtt0,d1
		moveq	#4,d0
	MC68060
		dc.w	$4e7a,$1808	;movec	PCR,d1
		moveq	#6,d0
	MC68000
.2		move.l	d5,(a5)
		move.l	d6,(a3)
.1		move.l	d7,(a4)
		movem.l	(a7)+,d1/d5-d7/a2-a5
		rts

.illegal	move.l	a2,(2,a7)	;new pc
		rte

***********************************************************
;-------------- save custom registers ---------------------
;-------------- palette saving done by read_palette -------

save_custom	movem.l	d0/a0-a4,-(a7)

;		lea.l	Action_CUST,a0
		lea.l	custom,a1
		move.l	a1,a4
;		move.w	#$200/8-1,d0
;.copy		move.l	(a0)+,(a1)+
;		move.l	(a0)+,(a1)+
;		dbf	d0,.copy

		move.w	$2(a6),d0
		move.w	d0,2(a4)
		or.w	#$8000,d0
		move.w	d0,$96(a4)	;DMACON
		move.w	#$7fff,$96(a6)
		move.w	$1c(a6),d0
		move.w	d0,$1c(a4)
		or.w	#$8000,d0
		move.w	d0,$9a(a4)	;INTENA
		move.w	#$7fff,$9a(a6)
		move.w	$1e(a6),d0
		move.w	d0,$1e(a4)
		or.w	#$8000,d0
		move.w	d0,$9c(a4)	;INTREQ
		move.w	#$7fff,$9c(a6)

		move.w	$10(a6),d0
		move.w	d0,$10(a4)
		or.w	#$8000,d0
		move.w	d0,$9e(a4)	;ADKCON

		move.l	OldRaster,$4(a4)	;VPOSR,VHPOSR

		lea.l	$a(a6),a0
		lea.l	$a(a4),a1
		moveq	#9-1,d0
.copy1		move.w	(a0)+,(a1)+
		dbf	d0,.copy1

		move.w	$7c(a6),$7c(a4)		;LISAID
		move.w	$1da(a6),$1da(a4)	;HHPOSR

	;	moveq	#0,d0
	;	jsr	analyse_copper

		movem.l	(a7)+,d0/a0-a4
		rts

;-------------- restore custom registers ------------------

restore_custom	movem.l	d0/a1,-(a7)
		lea.l	custom,a1

		;move.l	$20(a1),$20(a6)		;DISKPTR
		move.w	$7e(a1),$7e(a6)		;DSKSYNC
		move.w	$9e(a1),$9e(a6)		;ADKCON
		;move.w	$34(a1),$34(a6)		;POTGO
		move.w	$1dc(a1),$1dc(a6)	;BEAMCON0
		;move.w	$1e4(a1),$1e4(a6)	;DIWHIGH
		move.l	$8e(a1),$8e(a6)		;diwstrt/stop
		move.l	$92(a1),$92(a6)		;ddfstrt/stop
		;move.l	$e0(a1),$e0(a6)		;BPL1PT
		move.w	$1fc(a1),$1fc(a6)	;FMODE
		move.l	$108(a1),$108(a6)	;BPL1MOD
		move.w	$106(a1),$106(a6)	;BPLCON3
		move.l	$100(a1),$100(a6)	;BPLCON0
		;move.w	$10c(a1),$10c(a6)	;BPLCON4

		move.w	$96(a1),$96(a6)		;DMACON
		move.w	$9c(a1),$9c(a6)		;INTREQ
		move.w	$9a(a1),$9a(a6)		;INTENA

;		move.l	$84(a1),d0		;correct
;		cmp.l	#$f28,d0		;an AmigaOS
;		bne.b	.nof28			;problem
;		tst.l	$f28.w			;of the LoadView(NULL)
;		bne.b	.nof28
;		move.l	#$f24,$84(a1)
;		move.l	#$f24,$84(a6)
;.nof28
		movem.l	(a7)+,d0/a1
		rts

save_CIA	movem.l	a0-a1,-(a7)

;		lea.l	Action_CIAA,a0
;		lea.l	CIAA,a1
;		move.b	$1(a0),(a1)+
;		move.b	$101(a0),(a1)+
;		move.b	$201(a0),(a1)+
;		move.b	$301(a0),(a1)+
;		move.b	$401(a0),(a1)+
;		move.b	$501(a0),(a1)+
;		move.b	$601(a0),(a1)+
;		move.b	$701(a0),(a1)+
;		move.b	$801(a0),(a1)+
;		move.b	$901(a0),(a1)+
;		move.b	$a01(a0),(a1)+
;		move.b	$b01(a0),(a1)+
;		move.b	$c01(a0),(a1)+
;		move.b	$d01(a0),(a1)+
;		move.b	$e01(a0),(a1)+
;		move.b	$f01(a0),(a1)+
;
;		lea.l	Action_CIAB,a0
;		lea.l	CIAB,a1
;		move.b	(a0),(a1)+
;		move.b	$100(a0),(a1)+
;		move.b	$200(a0),(a1)+
;		move.b	$300(a0),(a1)+
;		move.b	$400(a0),(a1)+
;		move.b	$500(a0),(a1)+
;		move.b	$600(a0),(a1)+
;		move.b	$700(a0),(a1)+
;		move.b	$800(a0),(a1)+
;		move.b	$900(a0),(a1)+
;		move.b	$a00(a0),(a1)+
;		move.b	$b00(a0),(a1)+
;		move.b	$c00(a0),(a1)+
;		move.b	$d00(a0),(a1)+
;		move.b	$e00(a0),(a1)+
;		move.b	$f00(a0),(a1)+

		move.b	#$c0,$bfd200		;reinit CIAs
		move.b	#$ff,$bfd300		;for
		move.b	#$03,$bfe201		;AR
;		move.b	#$7f,$bfec01
		move.b	#$00,$bfee01
		move.b	#$88,$bfed01
		move.b	#$ff,$bfd100
		sf	mot_on

		move.w #$0f00,$34(a6)		;reinit right-mouse button

		movem.l	(a7)+,a0-a1
		rts

restore_CIA	movem.l	a0-a1,-(a7)

;		lea.l	$bfe000,a0
;		lea.l	CIAA,a1
;		clr.b	$e01(a0)
;		clr.b	$f01(a0)
;		move.b	(a1),$1(a0)
;		move.b	1(a1),$101(a0)
;		move.b	2(a1),$201(a0)
;		move.b	3(a1),$301(a0)
;		move.b	4(a1),$401(a0)
;		move.b	5(a1),$501(a0)
;		move.b	6(a1),$601(a0)
;		move.b	7(a1),$701(a0)
;		move.b	8(a1),$801(a0)
;		move.b	9(a1),$901(a0)
;		move.b	10(a1),$a01(a0)
;		move.b	11(a1),$b01(a0)
;		move.b	12(a1),$c01(a0)
;		move.b	14(a1),$e01(a0)
;		move.b	15(a1),$f01(a0)
;		move.b	13(a1),$d01(a0)
;
;		lea.l	$bfd000,a0
;		lea.l	CIAB,a1
;		clr.b	$e00(a0)
;		clr.b	$f00(a0)
;		move.b	(a1),(a0)
;		move.b	1(a1),$100(a0)
;		move.b	2(a1),$200(a0)
;		move.b	3(a1),$300(a0)
;		move.b	4(a1),$400(a0)
;		move.b	5(a1),$500(a0)
;		move.b	6(a1),$600(a0)
;		move.b	7(a1),$700(a0)
;		move.b	8(a1),$800(a0)
;		move.b	9(a1),$900(a0)
;		move.b	10(a1),$a00(a0)
;		move.b	11(a1),$b00(a0)
;		move.b	12(a1),$c00(a0)
;		move.b	14(a1),$e00(a0)
;		move.b	15(a1),$f00(a0)
;		move.b	13(a1),$d00(a0)

		movem.l	(a7)+,a0-a1
		rts

***********************************************************
; update status line (at bottom of screen)

print_status:	movem.l	d0-d3/a0,-(a7)
		lea.l	insert_off_txt,a0
		tst.b	insert_mode
		beq.b	.ok_ins_off
		lea.l	insert_on_txt,a0
.ok_ins_off	moveq	#76,d0
		move.w	screen_height,d1
		subq.w	#1,d1
		moveq	#3-1,d3
.loop		move.b	(a0)+,d2
		bsr	print_char2
		addq.w	#1,d0
		dbf	d3,.loop

		lea.l	start(pc),a0
		move.l	a0,d0
		lea.l	watch_txt,a0
		moveq	#8,d1
		bsr	conv_hex
		move.w	screen_height,d1
		subq.w	#1,d1
		moveq	#47,d0
		moveq	#8-1,d3
.loop2		move.b	(a0)+,d2
		bsr	print_char2
		addq.w	#1,d0
		dbf	d3,.loop2

		movem.l	(a7)+,d0-d3/a0
		rts

***********************************************************

super		move.w	#$2000,sr		;enable interrupts

		cmp.b	#1,config_screen
		bne.b	.nontsc
		move.w	#0,$1dc(a6)		;NTSC mode 15Khz
		bra.b	.okscreen
.nontsc		move.w	#$20,$1dc(a6)		;PAL mode 15Khz
.okscreen
		jsr	read_palette

		bsr	clear_break

		move.l	vbr_reg,a4
		move.l	illegal_except,d0	;restore vector used
		beq.b	no_illegal_init		;by breakpoint
		move.l	d0,$10(a4)
no_illegal_init

		bsr	set_pic		;init display area

		moveq	#0,d0
		lea.l	$140(a6),a0
		moveq	#$16-1,d1
clr_spr		move.l	d0,(a0)+		;clear sprites data
		dbf	d1,clr_spr

		bsr	print_page

		tst.b	trace_moni		;in tracer ?
		bne.b	no_print_trace

		bsr	check_debug		;print debug entry message

		tst.b	BP_reach		;entered from BreakPoint ?
		beq.b	no_BP_enter
		move.l	Break_Address,d0
		moveq	#8,d1
		lea.l	BP_enter_txt,a0
		bsr	print_curs
		bsr	print_hexCR
no_BP_enter
		tst.b	BPJ_reach		;entered from JSR BP ?
		beq.b	no_BPJ_enter
		move.l	Break_Address,d0
		moveq	#8,d1
		lea.l	BPJ_enter_txt,a0
		bsr	print_curs
		bsr	print_hexCR
no_BPJ_enter
		bsr	clear_cursor
		clr.w	cursor_x

		bsr	ShowWatchEntry

		move.l	ssp_reg,a0		;stackframe
		jsr	_ShowEntryReason
		bsr	print_reg		;print CPU registers
		move.l	pc_reg,dis_ptr
		bsr	single_inst		;disassemble 1 instr. at PC

no_print_trace
		sf	escape

		tst.b	BPatPC
		beq.b	.noPC
		st	escape
.noPC
		sf	p_used			;clr signal to allow sp command
		bsr	force_change		;force floppy disk change

;.joy		move.w	#0,$dff106
;		move.w	#$f,$dff180
;		btst	#7,$bfe001
;		bne.b	.joy

.raster		move.l	$4(a6),d0
		lsr.l	#1,d0
		lsr.w	#7,d0
		cmp.w	#$f0,d0
		blt.b	.raster

		move.b	#$7f,key_prev

		tst.b	config_IDE
		beq.b	.noIDEirq
		move.l	ide_irq,d0
		beq.b	.noIDEirq
		move.l	d0,a3
		tst.b	(a3)
		bpl.b	.noIDEirq
		move.l	ide_base,a3
		jsr	wait_irq		;remove IRQ request from IDE
.noIDEirq
		bsr	print_status

		moveq	#0,d2
		move.b	config_delay,d2
		move.w	d2,time_repeat

		move.w	#$7fff,$9c(a6)		;disable all INTREQ
		move.w	#$8300,$96(a6)		;bitplan DMA on
		move.w	#$c020,$9a(a6)		;VBL+CIAA IRQ on

		moveq	#3-1,d0
.loopk		sf	VBL
.waitk		tst.b	VBL			;wait to flush keyboard
		beq.b	.waitk			;buffer
		dbf	d0,.loopk

		clr.w	nb_keys

;-------------- Main Loop ---------------------------------

wait		tst.w	nb_keys
		bne.b	.go_fast
		stop	#$2000

.go_fast

;-------------- handle repeat key --------------------

		move.w	#$2700,sr
		move.b	key_prev,d0
		move.w	nb_keys,d1		;key in buffer ?
		beq.b	.no_key
		subq.w	#1,nb_keys
		subq.w	#1,d1
		lea.l	key_buffer,a0
		move.b	(a0),d0
.copy		move.b	1(a0),(a0)+		;remove one key from buffer
		dbf	d1,.copy
.no_key		move.w	#$2000,sr

		move.b	d0,d1
		cmp.b	key_prev,d0
		beq.b	same_key
		moveq	#0,d2
		move.b	config_delay,d2
		move.w	d2,time_repeat
		bra.b	ok_same_key
same_key
		tst.w	time_repeat
		bmi.w	ok_same_key
		move.b	#$7f,d0

ok_same_key	move.b	d1,key_prev
		move.b	d0,key

;-------------- main stuff ---------------------------

		bsr	special_keys

		tst.b	trace_moni	;tracer on ?
		beq.b	.go_key
		bsr.w	do_tracer
		bra.b	.no_key

.go_key		bsr	normal_keys

.no_key		tst.b	escape		;quit HRTmon ?
		beq.w	wait

;-------------- exit ---------------------------------

out_mon		st	tracer_refresh
		move.w	#$2700,sr
		move.w	#$7fff,$9a(a6)
		move.w	#$7fff,$96(a6)
		move.w	#$7fff,$9c(a6)

		tst.b	trace_moni
		bne.b	.noclr
		bsr	clear_cursor
.noclr
		bsr	remove_pic

		jsr	restore_palette

		bsr	init_break		;set breakpoints

		move.w	drive,-(a7)
		moveq	#3,d4			;first drive SEL (drive0)
		lea.l	drive_present,a3
		moveq	#4-1,d5			;4 drives
.loopd		tst.b	(a3)+
		beq.b	.nodrive
		move.w	d4,drive		;restore
		bsr	rest_head		;floppy drive head pos
.nodrive	addq.w	#1,d4
		dbf	d5,.loopd
		move.w	(a7)+,drive

		rts

*******************************************************
;------------------------------------------------
;-------------- get a key from keyboard ---------
;-------------- used for confirmation y/n -------

;<- d0=key (ascII)

get_key		clr.w	nb_keys
.wait		tst.w	nb_keys
		beq.b	.wait
		tst.b	key_buffer	;key released ?
		bmi.b	get_key
		move.w	#$2700,sr
		clr.w	nb_keys
		moveq	#0,d0
		move.b	key_buffer,d0
		move.b	#$7f,key_prev
		move.w	#$2000,sr
		move.l	a0,-(a7)
		move.l	board_ptr,a0
		move.b	(a0,d0.w),d0
		move.l	(a7)+,a0
		rts

*******************************************************
;-------------- Tracer --------------------------------

do_tracer	movem.l	d0-d7/a0-a6,-(a7)

		tst.b	tracer_refresh
		beq.w	.no_ref
		sf	tracer_refresh
		clr.l	cursor_x
		bsr	print_reg

		lea.l	empty_txt,a0
		bsr	print

		lea.l	tracer_addr,a3
		move.l	pc_reg,a4

		moveq	#5,d6			;no of actual line
		move.w	screen_height,d0
		sub.w	#11,d0
.seek		cmp.l	(a3)+,a4
		beq.b	.found
		addq.l	#1,d6
		dbf	d0,.seek

		moveq	#5,d6
		lea.l	tracer_addr,a3

		move.w	screen_height,d7
		sub.w	#11,d7
.loop		move.l	a4,(a3)+
		move.l	a4,-(a7)
		bsr	reloc_pic
		lea.l	general_txt,a0
		moveq	#7,d0		;upper case, d $address,indir. address
		jsr	disassemble
		move.l	(a7)+,a4
		add.w	d0,a4
		lea.l	general_txt,a0
		bsr	print
		dbf	d7,.loop

.found
	movem.w	d0-d1,-(a7)
	movem.w	actual_pos,d0-d1
	tst.w	d0
	bmi.b	.no
	move.b	#' ',d2
	bsr	print_char		;clear old '>'
.no	movem.w	(a7)+,d0-d1

		moveq	#11,d0
		move.l	d6,d1
		movem.w	d0-d1,actual_pos
		move.b	#'>',d2
		bsr	print_char

.no_ref
		lea.l	key,a0
		cmp.b	#$7f,d0
		beq.w	.nok
		cmp.b	#$4e,d0
		bne.b	.nor
		move.b	#$7f,(a0)
		move.w	#1,trace_count
		st	escape
		st	tracer_refresh
.nor		cmp.b	#$4d,d0
		bne.b	.nodown
		move.b	#$7f,(a0)
		movem.w	actual_pos,d0-d1
		move.l	ascII_ptr,a1
		mulu	#80,d1
		add.l	d1,a1
		cmp.b	#'J',1(a1,d0.w)
		bne.b	.no_jsr
		cmp.b	#'S',2(a1,d0.w)
		bne.b	.no_jsr
		cmp.b	#'R',3(a1,d0.w)
		beq.b	.go_bsr

.no_jsr
		cmp.b	#'B',1(a1,d0.w)
		bne.b	.no_bsr
		cmp.b	#'S',2(a1,d0.w)
		bne.b	.no_bsr
		cmp.b	#'R',3(a1,d0.w)
		bne.b	.no_bsr

.go_bsr		st	trace_bsr
.no_bsr		move.w	#1,trace_count
		st	escape
		st	tracer_refresh

.nodown
.nok
		movem.l	(a7)+,d0-d7/a0-a6
		rts

empty_txt	dcb.b 79," "
		dc.b $a,0
		even

*******************************************************
;-------------- restore pic-mem area used by display --

remove_pic	tst.b	pic_status
		beq.b	.okpic
		sf	pic_status
		move.w	#$2700,sr
		st	no_print

		tst.w	proc_type
		bne.b	.no68000
		move.l	backup_vbr+$6c,$6c.w
.no68000
		movem.l	d0/a0-a1,-(a7)
		moveq	#0,d0
		move.w	d0,$dff106
		move.l	d0,$dff180
		move.l	pic_ptr,a0
		lea.l	backup_pic,a1
		move.w	#PICSIZE/4-1,d0
.copy		move.l	(a1)+,(a0)+
		dbf	d0,.copy
		movem.l	(a7)+,d0/a0-a1
.okpic		rts

;-------------- save pic-mem area and print pic -------

set_pic		tst.b	pic_status
		bne.b	.okpic
		st	pic_status

		tst.w	proc_type
		bne.b	.no68000
		move.l	#newirq,$6c.w
.no68000

		movem.l	d0/a0-a1,-(a7)
		move.l	pic_ptr,a0
		lea.l	backup_pic,a1
		move.w	#PICSIZE/4-1,d0
.copy		move.l	(a0)+,(a1)+
		dbf	d0,.copy
		movem.l	(a7)+,d0/a0-a1
		sf	no_print
		bsr	print_page
		move.w	#$2000,sr
.okpic		rts

***********************************************************
;-------------- jmp here when a keyboard reset-warn code --
;-------------- has been received -------------------------

go_reset	move.w	#$2700,sr
		clr.l	inited
		sf	entered
		move.l	$f80004,a0
		subq.l	#2,a0
		move.w	#$200,$dff100
		move.w	#0,$dff1dc
		jmp	(a0)

***********************************************************
;-------------- VBL interrupt handler ---------------------

newirq		movem.l	d0-d7/a0-a6,-(a7)
		lea.l	$dff000,a6

		st	VBL		;signal a VBL

;-------------- init display registers ----------
		move.l	pic_ptr(pc),d0

		cmp.b	#1,config_screen
		bne.b	.nontsc

		move.w	#$9201,$100(a6)
		move.l	#$3081f7c1,$8e(a6)
		move.w	#$0,$1dc(a6)

		move.l	d0,$e0(a6)
		move.w	#$0c00,$106(a6)
		move.w	#$0000,$10c(a6)
		move.w	color0,$180(a6)
		move.w	color1,$182(a6)
		move.l	#$003c00d4,$92(a6)
		move.w	#0,$108(a6)

		move.w	#0,$1fc(a6)

		bra.w	.okmode


.nontsc		cmp.b	#2,config_screen
		bne.w	.nomulti

		move.l	d0,$e0(a6)

		move.l	#$2c4500e5,$8e(a6)
		move.l	#$00200068,$92(a6)
		move.w	#$1b88,$1dc(a6)
		move.w	#$1241,$100(a6)
		move.w	#0,$108(a6)
		move.w	#-80,$10a(a6)

		move.w	#$0000,$1fc(a6)

		move.w #$0071,$1c0(a6)
		move.w #$0008,$1c4(a6)
		move.w #$000e,$1de(a6)
		move.w #$001c,$1c2(a6)
		move.w #$001e,$1c6(a6)
		move.w #$0046,$1e2(a6)
		move.w #$020c,$1c8(a6)
		move.w #$0000,$1cc(a6)
		move.w #$0003,$1e0(a6)
		move.w #$0200,$1e4(a6)
		move.w #$0005,$1ca(a6)
		move.w #$001d,$1ce(a6)
	;	move.w #$0012,$104(a6)
		move.w #$0c21,$106(a6)

		move.w	color0,$180(a6)
		move.w	color1,$182(a6)

		bra.b	.okmode

.nomulti
		move.w	#$9201,$100(a6)
		move.l	#$30812cc1,$8e(a6)
		move.w	#$20,$1dc(a6)

		move.l	d0,$e0(a6)
		move.w	#$0c00,$106(a6)
		move.w	#$0000,$10c(a6)
		move.w	color0,$180(a6)
		move.w	color1,$182(a6)
		move.l	#$003c00d4,$92(a6)
		move.w	#0,$108(a6)

		move.w	#0,$1fc(a6)

.okmode


		tst.b	trace_moni		;test if in Tracer
		bne.b	.no_cur
		bsr	do_cursor
.no_cur
		tst.w	time_repeat
		bmi.b	no_sub_repeat
		subq.w	#1,time_repeat		;for repeat key
no_sub_repeat
		bsr	do_watch

		bsr.b	newirq2

		move.w	#$20,$9c(a6)		;interrupt ACK
		movem.l	(a7)+,d0-d7/a0-a6
		rte

; "IRQ handler" of CIA-A (keyboard)

newirq2		movem.l	d0-d1/a0/a1/a5-a6,-(a7)
		lea.l	$dff000,a6

		move.b	$bfed01,d0
		btst	#3,d0			;test if interrupt from keyboard
		beq	.pas_key

		move.b	$bfec01,d0		;get key
		not.b	d0
		ror.b	#1,d0

		cmp.b	#$78,d0
		beq.w	go_reset

		cmp.b	#$45,d0
		bne.b	.no_break
		st	break
.no_break
		tst.b	new_key			;test if waiting for key-pressed
		bne.b	.no_buf

		cmp.w	#64-1,nb_keys
		beq.b	.pas_key

		lea.l	key_buffer,a0
		move.w	nb_keys,d1
		move.b	d0,(a0,d1.w)		;save key in key buffer
		addq.w	#1,nb_keys
.no_buf		tst.b	d0
		bmi.b	.nonew
		sf	new_key
.nonew		bset	#6,$bfee01
		moveq	#3-1,d1
.wait		move.b	6(a6),d0		;wait 3 raster lines
.ras		cmp.b	6(a6),d0
		beq.b	.ras
		dbf	d1,.wait
		bclr	#6,$bfee01
.pas_key
		movem.l	(a7)+,d0-d1/a0/a1/a5-a6
		rts


********************************************************************
;------------- Display the actual address of a search process ------
;------------- at the bottom of the screen -------------------------

do_watch	movem.l	d0-d3/a0-a1,-(a7)

		lea.l	watch_txt,a0
		moveq	#8,d1
		move.l	watch,d0
		bsr	conv_hex

		lea.l	watch_txt,a0
		moveq	#31,d0
		move.w	screen_height,d1
		subq.w	#1,d1
		moveq	#8-1,d3
.print		move.b	(a0)+,d2
		bsr	print_char2
		addq.w	#1,d0
		dbf	d3,.print

		move.w	drive,d2
		add.b	#'0'-3,d2
		moveq	#18,d0
		move.w	screen_height,d1
		subq.w	#1,d1
		bsr	print_char2

		lea.l	track,a1
		move.w	drive,d0
		add.w	d0,d0
		move.w	-3*2(a1,d0.w),d0
		lea.l	watch_txt,a0
		lsr.w	#1,d0
		moveq	#0,d1
		move.b	#128+2,d1
		bsr	conv_dec
		moveq	#7,d0
		move.w	screen_height,d1
		subq.w	#1,d1
		moveq	#2-1,d3
		lea.l	watch_txt+1,a0
.print2		move.b	(a0)+,d2
		bsr	print_char2
		addq.w	#1,d0
		dbf	d3,.print2


		movem.l	(a7)+,d0-d3/a0-a1
		rts

*******************************************************
;print ascII page

print_page	movem.l	d0-d1,-(a7)
		moveq	#0,d0
		move.w	screen_height,d1
		subq.w	#1,d1
next_page_all	bsr	print_line
		addq.w	#1,d0
		dbf	d1,next_page_all
		movem.l	(a7)+,d0-d1
		rts

************************************************************
;-------------- handle all special keys --------------------
;-------------- shift,backspace,F1-F10,etc... --------------

special_keys	lea.l	key,a0
		move.b	(a0),d0
		cmp.b	#$7f,d0
		beq.w	no_special_keys

		cmp.b	#$60,d0
		blt.b	no_shift_on
		cmp.b	#$62,d0
		bgt.b	no_shift_on
		st	shift_mode
		move.b	#$7f,(a0)

no_shift_on	cmp.b	#$60+$80,d0
		blt.b	no_shift_off
		cmp.b	#$62+$80,d0
		bgt.b	no_shift_off
		sf	shift_mode
		move.b	#$7f,(a0)
no_shift_off
		cmp.b	#$63,d0
		bne.b	.no_ctrl_on
		st	ctrl_mode
		move.b	#$7f,(a0)
.no_ctrl_on
		cmp.b	#$63+$80,d0
		bne.b	.no_ctrl_off
		sf	ctrl_mode
		move.b	#$7f,(a0)
.no_ctrl_off
		cmp.b	#$64,d0
		beq.b	.alt_on
		cmp.b	#$65,d0
		bne.b	.no_alt_on
.alt_on		st	alt_mode
		move.b	#$7f,(a0)
.no_alt_on
		cmp.b	#$64+$80,d0
		beq.b	.alt_off
		cmp.b	#$65+$80,d0
		bne.b	.no_alt_off
.alt_off	sf	alt_mode
		move.b	#$7f,(a0)
.no_alt_off
		cmp.b	#$66,d0
		beq.b	.amiga_on
		cmp.b	#$67,d0
		bne.b	.no_amiga_on
.amiga_on	st	amiga_mode
		move.b	#$7f,(a0)
.no_amiga_on
		cmp.b	#$66+$80,d0
		beq.b	.amiga_off
		cmp.b	#$67+$80,d0
		bne.b	.no_amiga_off
.amiga_off	sf	amiga_mode
		move.b	#$7f,(a0)
.no_amiga_off

		tst.b	d0
		bmi.w	no_special_keys

		lea.l	cursor_x,a3

		tst.b	trace_moni
		bne.w	no_speck1

		cmp.b	#$45,d0
		bne.b	no_esc

ok_esc		bsr	clear_cursor
		move.b	#$7f,(a0)

		clr.w	(a3)
		move.w	window_bot,d1
		cmp.w	2(a3),d1
		bgt.b	.ok_do_esc
		bsr	scroll_up
		subq.w	#1,2(a3)
.ok_do_esc	addq.w	#1,2(a3)

		bsr	set_cursor
no_esc

;-------------- Return -------------------------------
		cmp.b	#$43,d0
		beq.b	ok_return
		cmp.b	#$44,d0
		bne.b	no_cr

ok_return	bsr	clear_cursor
		move.b	#$7f,(a0)

		clr.w	(a3)
		move.w	window_bot,d1
		cmp.w	2(a3),d1
		bgt.b	ok_do_cr
		bsr	scroll_up
		subq.w	#1,2(a3)
ok_do_cr	addq.w	#1,2(a3)

		bsr	command_line

		bsr	set_cursor
no_cr
;-------------- arrow keys --------------------------

		cmp.b	#$4f,d0
		bne.b	no_left
		move.b	#$7f,(a0)
		bsr	clear_cursor
		tst.b	shift_mode
		beq.b	.no_sh
		clr.w	(a3)
		bra.b	.ok_sh
.no_sh		tst.w	(a3)
		bne.b	.ok_left_edge
		move.w	#80,(a3)		;-1 -> 79
		tst.w	2(a3)
		bne.b	.ok_top_edge
		addq.w	#1,2(a3)		;keep 0 coz -1 later
		bsr	scroll_down
.ok_top_edge	subq.w	#1,2(a3)
.ok_left_edge	subq.w	#1,(a3)
.ok_sh		bsr	set_cursor

no_left		cmp.b	#$4e,d0
		bne.b	no_right
		move.b	#$7f,(a0)
		bsr	clear_cursor
		tst.b	shift_mode
		beq.b	.no_sh
		move.w	#79,(a3)
		bra.b	.ok_sh
.no_sh		cmp.w	#79,(a3)
		blt.b	.ok_right_edge
		move.w	#-1,(a3)
		move.w	window_bot,d1
		cmp.w	2(a3),d1
		bgt.b	.ok_bot_edge
		subq.w	#1,2(a3)		;keep 24 coz +1 later
		bsr	scroll_up
.ok_bot_edge	addq.w	#1,2(a3)
.ok_right_edge	addq.w	#1,(a3)
.ok_sh		bsr	set_cursor

no_right
		cmp.b	#$4c,d0
		bne.w	no_up
		move.b	#$7f,(a0)
		bsr	clear_cursor

		tst.b	shift_mode
		beq.b	.noshift
		move.w	(2,a3),d1
		clr.w	(2,a3)			;move cursor to top of screen
		tst.w	d1
		beq.b	.no_ctrl
		bra.w	.end_up

.noshift	tst.b	ctrl_mode
		beq.b	.no_ctrl
		movem.l	d0/a0,-(a7)
		clr.w	(a3)
		moveq	#16-1,d1			;nb history lines
.loop		subq.w	#1,history_disp
		and.w	#$f,history_disp
		move.w	history_disp,d0
		mulu	#80,d0
		lea.l	history,a0
		add.l	d0,a0
		tst.b	(a0)
		dbne	d1,.loop
		bsr	print
		lea.l	79(a0),a0
.back		cmp.b	#' ',-(a0)
		bne.b	.no_spc
		subq.w	#1,(a3)
		bra.b	.back
.no_spc		movem.l	(a7)+,d0/a0
		bra.b	.end_up

.no_ctrl	tst.w	2(a3)
		bne.b	.ok_up
		moveq	#1,d2			;scroll 1 line
		tst.b	shift_mode
		beq.b	.no_sh2
		clr.w	(a3)
		moveq	#SC_STEP,d2
.no_sh2		movem.l	d0-a4,-(a7)
		move.l	last_cmd,a0
		tst.l	(12,a0)
		sne	no_sc_clr
		bsr	scroll_down2
		move.l	(12,a0),d0
		beq.b	.no_up_cmd
		move.l	d0,a1
		jsr	(a1)
		beq.b	.no_up_cmd
		bsr	print_page
.no_up_cmd	movem.l	(a7)+,d0-a4
		bra.b	.end_up
.ok_up		subq.w	#1,2(a3)
.end_up		bsr	set_cursor

no_up
		cmp.b	#$4d,d0
		bne.w	no_down
		move.b	#$7f,(a0)
		bsr	clear_cursor

		tst.b	shift_mode
		beq.b	.noshift
		move.w	(2,a3),d1
		move.w	window_bot,(2,a3)	;move cursor to bot. of screen
		cmp.w	window_bot,d1
		beq.b	.no_ctrl
		bra.w	.end_down

.noshift	tst.b	ctrl_mode
		beq.b	.no_ctrl
		movem.l	d0/a0,-(a7)
		clr.w	(a3)
		moveq	#16-1,d1
.loop		addq.w	#1,history_disp
		and.w	#$f,history_disp
		move.w	history_disp,d0
		mulu	#80,d0
		lea.l	history,a0
		add.l	d0,a0
		tst.b	(a0)
		dbne	d1,.loop
		bsr	print
		lea.l	79(a0),a0
.back		cmp.b	#' ',-(a0)
		bne.b	.no_spc
		subq.w	#1,(a3)
		bra.b	.back
.no_spc		movem.l	(a7)+,d0/a0
		bra.b	.end_down

.no_ctrl	move.w	window_bot,d1
		cmp.w	2(a3),d1
		bgt.b	.ok_down
		moveq	#1,d2			;scroll 1 line
		tst.b	shift_mode
		beq.b	.no_sh2
		clr.w	(a3)
		moveq	#SC_STEP,d2
.no_sh2		movem.l	d0-a4,-(a7)
		move.l	last_cmd,a0
		tst.l	(16,a0)
		sne	no_sc_clr
		bsr	scroll_up2
		move.l	(16,a0),d0
		beq.b	.no_down_cmd
		move.l	d0,a1
		jsr	(a1)
		beq.b	.no_down_cmd
		bsr	print_page
.no_down_cmd	movem.l	(a7)+,d0-a4
		bra.b	.end_down
.ok_down	addq.w	#1,2(a3)
.end_down	bsr	set_cursor
no_down

;-------------- switch Tracer page ON/OFF F7 --------------
no_speck1	cmp.b	#$56,d0
		bne.b	.no_f7
		move.b	#$7f,(a0)
		not.b	trace_moni
		beq.b	.quittracer
		bsr	clear_cursor
		move.l	#ascII_Tracer,d1
		move.w	#MAX_SCREEN,d2
		sub.w	screen_height,d2
		mulu	#80,d2
		add.l	d2,d1
		move.l	d1,ascII_ptr
		bsr	print_page
		st	trace_moni
		move.l	cursor_x,old_cursor
		st	tracer_refresh
		bra.b	.endF7

.quittracer	bsr	clear_cursor
		move.l	old_cursor,cursor_x
		lea.l	ascII_ptr,a1
		movem.l	4(a1),d0-d1		;read ascII_page1,2 ptrs
		tst.b	ascII_page	;flip page flag
		bne.b	.noex2
		exg	d0,d1
.noex2		move.l	d1,(a1)			;set actual page ptr
		bsr	print_page
		bsr	set_cursor
.endF7

;-------------- switch page 1/2 F10 -----------------------
.no_f7
		cmp.b	#$59,d0
		bne.b	no_f10
		move.b	#$7f,(a0)
		tst.b	trace_moni
		bne.b	no_f10
		bsr	clear_cursor
		lea.l	ascII_ptr,a1
		movem.l	4(a1),d1-d2		;read ascII_page1,2 ptrs
		not.b	ascII_page	;flip page flag
		bne.b	.noex
		exg	d1,d2
.noex		move.l	d2,(a1)			;set actual page ptr
		bsr	print_page
		bsr	set_cursor
no_f10

;-------------- Clear screen F1 -----------------
		tst.b	trace_moni
		bne.w	no_speck2

		cmp.b	#$50,d0
		bne.b	no_f1
		move.b	#$7f,(a0)
		bsr	clear_cursor
		move.l	ascII_ptr,a1
		move.w	window_top,d2
		mulu	#80,d2
		add.l	d2,a1
		move.l	#'    ',d1
		move.w	window_bot,d2
		sub.w	window_top,d2
		addq.w	#1,d2
		mulu	#20,d2
		subq.w	#1,d2
clear_ascII	move.l	d1,(a1)+
		dbf	d2,clear_ascII
		bsr	print_page
		moveq	#0,d1
		move.w	window_top,d1
		move.l	d1,cursor_x
		bsr	set_cursor
no_f1
;-------------- change insert mode F2 -----------
		cmp.b	#$51,d0
		bne.b	no_f2
		move.b	#$7f,(a0)
		movem.l	d0/a0,-(a7)
		lea.l	insert_off_txt,a0
		not.b	insert_mode
		beq.b	ok_ins_off
		lea.l	insert_on_txt,a0
ok_ins_off	moveq	#76,d0
		move.w	screen_height,d1
		subq.w	#1,d1
		moveq	#3-1,d3
.loop		move.b	(a0)+,d2
		bsr	print_char2
		addq.w	#1,d0
		dbf	d3,.loop
		movem.l	(a7)+,d0/a0
no_f2

;-------------- change FCPU F6 ------------------
		cmp.b	#$55,d0
		bne.b	.no_f5
		move.b	#$7f,(a0)
		tst.b	fami_mode
		beq.b	.was802
		bsr	set_65802
		bra.b	.no_f5
.was802		bsr	set_65816
.no_f5
;-------------- backspace -----------------------
		cmp.b	#$41,d0
		bne.w	no_back
		move.b	#$7f,(a0)
		tst.b	shift_mode	;shift+backspace -> clear line
		beq.b	.no_clr

		bsr	clear_cursor
		move.w	d0,-(a7)
		move.l	ascII_ptr,a1
		move.w	2(a3),d0
		mulu	#80,d0
		add.l	d0,a1
		moveq	#80-1,d0
.fill		move.b	#' ',(a1)+
		dbf	d0,.fill
		clr.w	(a3)
		move.w	2(a3),d0
		bsr	print_line
		move.w	(a7)+,d0
		bsr	set_cursor
		bra.b	no_back

.no_clr		tst.w	(a3)
		beq.b	no_back
		bsr	clear_cursor
		move.l	ascII_ptr,a1
		add.w	(a3),a1			;add x cursor
		subq.w	#1,a1
		move.w	2(a3),d1
		mulu	#80,d1
		add.l	d1,a1			;add y cursor
		lea.l	1(a1),a2
		moveq	#80-1,d1
		sub.w	(a3),d1
scroll_line	move.b	(a2)+,(a1)+
		dbf	d1,scroll_line
		move.b	#' ',(a1)+
		move.w	d0,-(a7)
		move.w	2(a3),d0
		bsr	print_line
		move.w	(a7)+,d0
		subq.w	#1,(a3)
		bsr	set_cursor
no_back
;-------------- del -----------------------------
		cmp.b	#$46,d0
		bne.w	no_del
		move.b	#$7f,(a0)
		bsr	clear_cursor

		tst.b	alt_mode
		bne.s	.alt_mode

		tst.b	shift_mode
		beq.b	.do_del

		move.l	ascII_ptr,a1
		move.w	2(a3),d1
		mulu	#80,d1
		add.l	d1,a1
		lea.l	80(a1),a1
		lea.l	-1(a1),a2
		moveq	#80-2,d1
		sub.w	(a3),d1
		bmi.b	.end_del
.l		move.b	-(a2),-(a1)
		dbf	d1,.l
		move.b	#' ',(a2)
		bra.b	.end_del

.do_del		move.l	ascII_ptr,a1
		add.w	(a3),a1			;add x cursor
		move.w	2(a3),d1
		mulu	#80,d1
		add.l	d1,a1			;add y cursor
		lea.l	1(a1),a2
		moveq	#80-1,d1
		sub.w	(a3),d1
		bra.b	.do_dbf_scroll
.scroll_line2	move.b	(a2)+,(a1)+
.do_dbf_scroll	dbf	d1,.scroll_line2
		move.b	#' ',(a1)+
.end_del		move.w	d0,-(a7)
		move.w	2(a3),d0
		bsr	print_line
		move.w	(a7)+,d0
		bsr	set_cursor
		bra.b	no_del

.alt_mode	bsr	clear_cursor
		clr.w	(a3)			;first column
		move.l	ascII_ptr,a2
		move.w	2(a3),d1
		moveq	#0,d2
		move.w	d1,d2
		mulu	#80,d2
		lea.l	(a2,d2.l),a2		;screen ends
		lea.l	80(a2),a1
.movepart	moveq	#80/4-1,d0
.moveline	move.l	(a1)+,(a2)+
		dbf	d0,.moveline
		move.w	d1,d0
		bsr.w	print_line
		addq.w	#1,d1
		cmp.w	window_bot,d1
		bne.s	.movepart
		moveq	#80/4-1,d0
.clearline	move.l	#'    ',(a2)+
		dbf	d0,.clearline
		move.w	d1,d0
		bsr.w	print_line

		bsr	set_cursor

no_del
;-------------- HELP print help page ---------------

		cmp.b	#$5f,d0
		bne.b	no_help
		move.b	#$7f,(a0)
		move.l	a0,-(a7)
		lea.l	help_txt,a0
		bsr	print_curs
		tst.b	break
		beq.b	.nobrk
		sf	break
		lea.l	break_txt,a0
		bsr	print
.nobrk		move.l	(a7)+,a0
no_help

;-------------- Tab --------------------------------

		cmp.b	#$42,d0
		bne.w	no_tab
		move.b	#$7f,(a0)

		tst.b	alt_mode
		bne.b	.alt_mode

		bsr	clear_cursor

.do_ins		move.l	ascII_ptr,a1		;normal tab=insert a space
		move.w	2(a3),d1
		addq.w	#1,d1
		mulu	#80,d1
		add.l	d1,a1			;add y cursor
		lea.l	-1(a1),a2
		moveq	#80-1,d1
		sub.w	(a3),d1			;from cursor to eol
		bra.b	.do_dbf_scroll

.scroll_line2	move.b	-(a2),-(a1)
.do_dbf_scroll	dbf	d1,.scroll_line2
		move.b	#' ',-(a1)
.end_ins	move.w	d0,-(a7)
		move.w	2(a3),d0
		bsr	print_line
		move.w	(a7)+,d0
		bsr	set_cursor
		bra.s	no_tab

.alt_mode
		bsr	clear_cursor
		clr.w	(A3)			;first column
		move.l	ascII_ptr,a2
		move.w	window_bot,d1
		addq.w	#1,D1
		moveq	#0,d2
		move.w	d1,d2
		mulu	#80,d2
		lea.l	(a2,d2.l),a2		;screen ends
		lea.l	-80(a2),a1
		subq.w	#1,d1
.movepart	moveq	#80/4-1,d0
.moveline	move.l	-(a1),-(a2)
		dbf	d0,.moveline
		move.w	d1,d0
		bsr.w	print_line
		subq.w	#1,d1
		cmp.w	2(a3),d1
		bne.s	.movepart
		moveq	#80/4-1,d0
.clearline	move.l	#'    ',-(a2)
		dbf	d0,.clearline
		move.w	d1,d0
		bsr.w	print_line

;		bsr.w	print_page

		bsr	set_cursor
no_tab

no_speck2
no_special_keys	rts

;------------------------------------------------
;-------------- handle normal keys --------------
;-------------- 0-9,a-z,etc... ------------------

normal_keys	lea.l	key,a0
		tst.b	snap_buf
		beq.b	.nos
		move.b	#$40,(a0)
.nos		moveq	#0,d0
		move.b	(a0),d0
		cmp.b	#$7f,d0
		beq.w	no_normal_keys

		move.b	#$7f,(a0)

		tst.b	d0
		bmi.w	no_normal_keys

		bsr	clear_cursor

		move.l	board_ptr,a1
		tst.b	shift_mode
		beq.b	no_shift_mode
		move.l	board_ptr+4,a1
no_shift_mode	tst.b	alt_mode
		beq.b	.no_alt
		move.l	board_ptr+8,a1
.no_alt		move.b	(a1,d0.w),d2

		move.l	a0,-(a7)
		lea.l	snap_buf,a0
		tst.b	(a0)
		beq.b	empty_snap

resnap		move.b	(a0)+,d2

empty_snap	lea.l	cursor_x,a3

		tst.b	insert_mode
		beq.b	no_insert_mode
;-------------- do insert mode -----------------
		cmp.w	#79,(a3)
		bge.b	ok_cursor_pos
		move.l	ascII_ptr,a1
		move.l	a1,a2
		add.w	(a3),a1
		move.w	2(a3),d0
		mulu	#80,d0
		add.l	d0,a1
		add.l	d0,a2
		lea.l	80(a2),a2
		lea.l	-1(a2),a4
do_insert	move.b	-(a4),-(a2)
		cmp.l	a1,a4
		bne.b	do_insert
		move.b	d2,(a1)
		addq.w	#1,(a3)
		move.w	2(a3),d0
		bsr	print_line
		bra.b	ok_cursor_pos
;-----------------------------------------------
no_insert_mode
		movem.w	(a3),d0-d1
		bsr	print_char

		addq.w	#1,(a3)
		cmp.w	#80,(a3)
		blt.b	ok_cursor_pos
		clr.w	(a3)
		move.w	window_bot,d1
		cmp.w	2(a3),d1
		bgt.b	ok_cursor_h
		bsr	scroll_up
		bra.b	ok_cursor_pos
ok_cursor_h	addq.w	#1,2(a3)

ok_cursor_pos
		tst.b	(a0)
		bne.w	resnap

		sf	snap_buf

		move.l	(a7)+,a0

		bsr	set_cursor

no_normal_keys	rts

************************************************************
;-------------- called when RETURN is pressed --------------

command_line	movem.l	d0-d7/a0-a4,-(a7)
		lea.l	cursor_x,a3
		move.w	2(a3),d0		;read y pos
		subq.w	#1,d0
		move.l	ascII_ptr,a0
		mulu	#80,d0
		add.l	d0,a0
		lea.l	command,a1
		moveq	#80-1,d1
copy_command	move.b	(a0)+,(a1)+		;copy current line into
		dbf	d1,copy_command		;command

		sf	d4			;command repeat signal off
		lea.l	command,a0
seek_command	move.b	(a0)+,d0		;seek command first char
		beq.b	end_cmd_line
		cmp.b	#$20,d0
		beq.b	seek_command
		bra.b	ok_new_cmd

end_cmd_line	subq.w	#1,2(a3)		;if command line is empty
		move.l	last_cmd,a0		;then repeat last command
		addq.l	#1,a0
		st	d4			;signal in d4 for cmd repeat
ok_new_cmd
		subq.l	#1,a0
		lea.l	cmd_list,a2
		moveq	#0,d1			;command code offset cleared

.loop2		tst.b	(a2)			;search if command
		beq.b	end_cmd_list		;exists
		move.l	a2,a3
		move.l	a0,a1
.loop		move.b	(a3)+,d3
		beq.b	.ok_cmp
		move.b	(a1)+,d0
		bsr	upper_case
		cmp.b	d0,d3
		beq.b	.loop

.go1		lea.l	20(a2),a2		;next command in cmd_list
		bra.b	.loop2

.ok_cmp		move.l	8(a2),d1		;command code offset
		cmp.l	#cmd_x,d1
		beq.b	.nolast			;cant't repeat cmd_x
		move.l	a2,last_cmd
.nolast		move.l	a1,d2
		bra.b	.go1			;check if it's really this cmd
						;to get the longest cmd.

end_cmd_list	sf	break
		tst.l	d1
		beq.b	no_command

		tst.b	d4			;test if cmd repeat ?
		bne.b	.no_hist

		lea.l	history_cnt,a2
		move.w	(a2),d0
		lea.l	history,a0
		mulu	#80,d0
		add.l	d0,a0
		lea.l	command,a1
		moveq	#79-1,d0
.copy		move.b	(a1)+,(a0)+		;copy command in history
		dbf	d0,.copy

		addq.w	#1,(a2)
		and.w	#$f,(a2)		;maxi 16 history lines
		move.w	(a2),history_disp

.no_hist	st	no_curs
		move.l	d2,a0
		lea.l	cmd_list(pc),a5

		sf	disk_op
		sf	floppy_op

		move.l	a7,cmd_sp_save
		st	cmd_executed
		sf	cmd_crashed

		move.l	d1,-(a7)		;jmp (d1.l) exec. the cmd
		rts

no_command	lea.l	unknown_txt,a0
		bsr	print
		move.l	a7,cmd_sp_save

end_command	sf	cmd_executed
;try to recover from eventual command crash
		move.l	cmd_sp_save(pc),a7
		bsr	set_pic
		move.w	#$2000,sr

		bsr	end_disk

		tst.b	cmd_crashed
		beq.b	.nocrash
		lea.l	.crash_txt(pc),a0
		bsr	print
		lea.l	crash_stack,a0
		jsr	_ShowEntryReason

.nocrash
		tst.b	break			;ESC pressed during
		beq.b	.no_break		;command ?
		sf	break
		lea.l	break_txt,a0
		bsr	print
.no_break
		sf	no_curs

		movem.l	(a7)+,d0-d7/a0-a4
		rts

.crash_txt	dc.b "*** command crashed ***",$a,0

		cnop 0,4
cmd_sp_save	dc.l 0

************************************************************
		cnop 0,4

;-------------- cmds ordered by size -----------------------
;cmd format
;----------
;dc.b cmd name padded with 0 (8 bytes)
;dc.l ptr to code, ptr to line up code, ptr to line down code

cmd_list:	dc.b 'R',0,0,0,0,0,0,0
		dc.l cmd_r,0,0
		dc.b 'H',0,0,0,0,0,0,0
		dc.l cmd_h,cmdu_h,cmdd_h
		dc.b 'M',0,0,0,0,0,0,0
		dc.l cmd_h,cmdu_h,cmdd_h
		dc.b 'D',0,0,0,0,0,0,0
		dc.l cmd_a,cmdu_d,cmdd_d
		dc.b 'B',0,0,0,0,0,0,0
		dc.l cmd_b,0,0
		dc.b 'C',0,0,0,0,0,0,0
		dc.l cmd_c,0,0
		dc.b 'T',0,0,0,0,0,0,0
		dc.l cmd_t,0,0
		dc.b 'N',0,0,0,0,0,0,0
		dc.l cmd_n,cmdu_n,cmdd_n
		dc.b 'P',0,0,0,0,0,0,0
		dc.l cmd_p,0,0
		dc.b '?',0,0,0,0,0,0,0
		dc.l cmd_ev,0,0
		dc.b 'L',0,0,0,0,0,0,0
		dc.l cmd_l,0,0
		dc.b 'X',0,0,0,0,0,0,0
		dc.l cmd_x,0,0
		dc.b 'G',0,0,0,0,0,0,0
		dc.l cmd_g,0,0
		dc.b 'F',0,0,0,0,0,0,0
		dc.l cmd_f,0,0
		dc.b 'O',0,0,0,0,0,0,0
		dc.l cmd_o,0,0
		dc.b 'A',0,0,0,0,0,0,0
		dc.l cmd_a,0,0
		dc.b 'S',0,0,0,0,0,0,0
		dc.l cmd_s,0,0
		dc.b 'Q',0,0,0,0,0,0,0
		dc.l cmd_q,0,0
		dc.b 'E',0,0,0,0,0,0,0
		dc.l cmd_e,cmdu_e,cmdd_e

		dc.b 'FI',0,0,0,0,0,0
		dc.l cmd_fi,0,0
		dc.b 'RS',0,0,0,0,0,0
		dc.l cmd_disk1,0,0
		dc.b 'WS',0,0,0,0,0,0
		dc.l cmd_disk2,0,0
		dc.b 'CD',0,0,0,0,0,0
		dc.l cmd_cd,0,0
		dc.b 'DF',0,0,0,0,0,0
		dc.l cmd_df,0,0
		dc.b 'AF',0,0,0,0,0,0
		dc.l cmd_af,0,0
		dc.b 'TS',0,0,0,0,0,0
		dc.l cmd_ts,0,0
		dc.b 'TF',0,0,0,0,0,0
		dc.l cmd_tf,0,0
		dc.b 'SA',0,0,0,0,0,0
		dc.l cmd_sa,0,0
		dc.b 'LA',0,0,0,0,0,0
		dc.l cmd_la,0,0
		dc.b 'LS',0,0,0,0,0,0
		dc.l cmd_dir,0,0
		dc.b 'SP',0,0,0,0,0,0
		dc.l cmd_sp,0,0
		dc.b 'BB',0,0,0,0,0,0
		dc.l cmd_bb,0,0
		dc.b 'FS',0,0,0,0,0,0
		dc.l cmd_fs,0,0
		dc.b 'MW',0,0,0,0,0,0
		dc.l cmd_mw,0,0
		dc.b 'RD',0,0,0,0,0,0
		dc.l cmd_rd,0,0

		dc.b 'MWD',0,0,0,0,0
		dc.l cmd_mwd,0,0
		dc.b 'TSD',0,0,0,0,0
		dc.l cmd_tsd,0,0
 		dc.b 'FIF',0,0,0,0,0
		dc.l cmd_fif,0,0
		dc.b 'DIR',0,0,0,0,0
		dc.l cmd_dir,0,0
		dc.b 'PAL',0,0,0,0,0
		dc.l cmd_pal,0,0
		dc.b '31K',0,0,0,0,0
		dc.l cmd_31k,0,0
		dc.b 'LED',0,0,0,0,0
		dc.l cmd_led,0,0
		dc.b 'IDE',0,0,0,0,0
		dc.l cmd_ide,0,0
		dc.b 'DEL',0,0,0,0,0
		dc.l cmd_del,0,0
		dc.b 'PWD',0,0,0,0,0
		dc.l cmd_cd,0,0
		dc.b 'D2F',0,0,0,0,0
		dc.l cmd_d2f,0,0
		dc.b 'F2D',0,0,0,0,0
		dc.l cmd_f2d,0,0
		dc.b 'VER',0,0,0,0,0
		dc.l cmd_ver,0,0
		dc.b 'SAC',0,0,0,0,0
		dc.l cmd_sac,0,0
		dc.b 'COP',0,0,0,0,0
		dc.l cmd_cop,0,0

		dc.b 'NTSC',0,0,0,0
		dc.l cmd_ntsc,0,0
		dc.b 'TYPE',0,0,0,0
		dc.l cmd_type,0,0
		dc.b 'COPY',0,0,0,0
		dc.l cmd_copy,0,0
		dc.b 'PART',0,0,0,0
		dc.l cmd_part,0,0
		dc.b 'KILL',0,0,0,0
		dc.l cmd_kill,0,0

		dc.b 'DRIVE',0,0,0
		dc.l cmd_drive,0,0
		dc.b 'DEBUG',0,0,0
		dc.l cmd_debug,0,0
		dc.b 'MOTOR',0,0,0
		dc.l cmd_motor,0,0
		dc.b 'INTEL',0,0,0
		dc.l cmd_intel,0,0
		dc.b 'EXCEP',0,0,0
		dc.l cmd_excep,0,0
		dc.b 'MKDIR',0,0,0
		dc.l cmd_makedir,0,0
		dc.b 'CLEAR',0,0,0
		dc.l cmd_clear,0,0

		dc.b 'REBOOT',0,0
		dc.l cmd_reboot,0,0
		dc.b 'FORMAT',0,0
		dc.l cmd_format,0,0
		dc.b 'OUTPUT',0,0
		dc.l cmd_output,0,0
		dc.b 'SETMAP',0,0
		dc.l cmd_setmap,0,0
		dc.b 'HORNET',0,0
		dc.l cmd_hornet,0,0
		dc.b 'CHKDSK',0,0
		dc.l cmd_diskchk,0,0

		dc.b 'FORMATQ',0
		dc.l cmd_formatq,0,0
		dc.b 'DISKCHK',0
		dc.l cmd_diskchk,0,0
		dc.b 'MAKEDIR',0
		dc.l cmd_makedir,0,0
		dc.b 'HEXLOCK',0
		dc.l cmd_hexlock,0,0

	;whdload related commands
		dc.b 'WL',0,0,0,0,0,0
		dc.l cmd_wl,0,0
		dc.b 'WS',0,0,0,0,0,0
		dc.l cmd_ws,0,0
		dc.b 'WD',0,0,0,0,0,0
		dc.l cmd_wd,0,0
		dc.b 'WQ',0,0,0,0,0,0
		dc.l cmd_wq,0,0
		dc.b 'WPR',0,0,0,0,0
		dc.l cmd_wpr,0,0
		dc.b 'WPRW',0,0,0,0
		dc.l cmd_wprw,0,0
		dc.b 'WPW',0,0,0,0,0
		dc.l cmd_wpw,0,0
		dc.b 'WPSMC',0,0,0
		dc.l cmd_wpsmc,0,0

		dcb.b 8,0
		dc.l 0,0,0

****************************************************************

cmd_ver		lea.l	ver_txt(pc),a0
		bsr	print
		bra.w	end_command

ver_txt		dc.b "HRTmon v"
		version			;x.yz
		dc.b $a,0
		even

****************************************************************

cmd_hexlock
.1		move.b	(a0)+,d0
		cmp.b	#$20,d0
		beq.b	.1
		move.b	-1(a0),d0
		bsr	upper_case
		cmp.b	#'O',d0
		bne.b	.fail
		move.b	(a0),d0
		bsr	upper_case
		cmp.b	#'N',d0
		beq.b	.switchon
		cmp.b	#'F',d0
		bne.b	.fail

.switchoff	sf	hex_default
		lea.l	.offtext(pc),a0
		bra.b	.print

.switchon	st	hex_default
		lea.l	.ontext(pc),a0
		bra.b	.print

.fail		lea.l	.failtext(pc),a0
.print		bsr	print
		bra.w	end_command

.offtext	dc.b "Hex mode off ",$a,0
.ontext		dc.b "Hex mode on ",$a,0
.failtext	dc.b	"Valid arguments: on, off ",$a,0
		EVEN

****************************************************************

		include src/copper.s

****************************************************************
;-------------- add/remove/view memory watch -------------------

cmd_mw		moveq	#1,d1
		cmp.b	#'.',(a0)
		bne.b	.nosize
		addq.l	#1,a0
		move.b	(a0)+,d0
		bsr	upper_case
		cmp.b	#'B',d0
		beq.b	.nosize
		moveq	#2,d1
		cmp.b	#'W',d0
		beq.b	.nosize
		moveq	#4,d1
		cmp.b	#'L',d0
		beq.b	.nosize
		lea.l	.size_txt(pc),a0
		bsr	print
		bra.w	.end

.nosize		move.b	d1,.tmp_size
		bsr	evaluate
		bmi.w	illegal_addr
		beq.b	.gotaddr

;-------------- no param -> view all memory watch ---------

		lea.l	watch_size(pc),a2
		lea.l	watch_addr(pc),a1
.loop		move.b	(a2)+,d2
		beq.w	.end

		lea.l	.view_txt(pc),a0
		bsr	print
		move.l	(a1)+,d0
		moveq	#8,d1
		bsr	print_hex
		cmp.b	#1,d2
		bne.b	.nob
		lea.l	.viewb_txt(pc),a0
		bsr	print
		bra.b	.loop
.nob		cmp.b	#2,d2
		bne.b	.now
		lea.l	.vieww_txt(pc),a0
		bsr	print
		bra.b	.loop
.now		lea.l	.viewl_txt(pc),a0
		bsr	print
		bra.b	.loop

;-------------- set/remove memory watch -------------------

.gotaddr	move.l	d0,a4
		lea.l	watch_addr(pc),a0
		lea.l	watch_size(pc),a1
		lea.l	watch_orig(pc),a2
.loop2		addq.l	#4,a2
		tst.b	(a1)+
		beq.b	.noremove
		cmp.l	(a0)+,a4
		bne.b	.loop2
		subq.l	#1,a1
		subq.l	#4,a0
		subq.l	#4,a2
.pack		move.l	4(a0),(a0)+
		move.l	4(a2),(a2)+
		move.b	1(a1),(a1)+
		bne.b	.pack
		lea.l	.remove_txt(pc),a0
		bsr	print
		move.l	a4,d0
		moveq	#8,d1
		bsr	print_hexCR
		and.w	#$7fff,sr_reg
		bra.b	.end

;-------------- set memory watch --------------------------
.noremove	move.l	a4,d0
		btst	#0,d0
		beq.b	.okeven
		tst.w	proc_type
		bne.b	.okeven
		lea.l	.even_txt(pc),a0
		bsr	print
		bra.b	.end
.okeven
		bsr	reloc_pic
		lea.l	watch_size(pc),a0
		lea.l	watch_addr(pc),a1
		lea.l	watch_orig(pc),a2
		moveq	#MAX_WATCH-1,d0
.seek		tst.b	(a0)
		beq.b	.found_empty
		addq.l	#1,a0
		addq.l	#4,a1
		addq.l	#4,a2
		dbf	d0,.seek
		lea.l	.toomany_txt(pc),a0
		bsr	print
		bra.b	.end

.found_empty	move.b	.tmp_size(pc),(a0)
		move.l	a4,(a1)
		move.l	(a4),(a2)

		lea.l	.set_txt(pc),a0
		bsr	print
		moveq	#8,d1
		move.l	a4,d0
		bsr	print_hexCR

.end		bra.w	end_command

.tmp_size	dc.b 0

.size_txt	dc.b "Illegal size specifier ! (only .b .w .l allowed)",$a,0
.view_txt	dc.b "Memory watch at $",0
.viewb_txt	dc.b ".b",$a,0
.vieww_txt	dc.b ".w",$a,0
.viewl_txt	dc.b ".l",$a,0
.set_txt	dc.b "Memory watch set at $",0
.remove_txt	dc.b "Memory watch removed at $",0
.toomany_txt	dc.b "Too many memory watch !",$a,0
.even_txt	dc.b "Address must be even ! (only on 68000 processor)",$a,0
		even

MAX_WATCH = 16

		cnop 0,4
watch_addr	dcb.l MAX_WATCH+1,0	;address to watch
watch_orig	dcb.l MAX_WATCH+1,0	;original value in watched mem.
watch_size	dcb.b MAX_WATCH+1,0	;watch size (1,2,4, 0=none)

watch_flag	dc.b 0			;-1 when HRTmon entered by memory watch
		even
watch_trigger	dc.l 0			;address which triggered entrance
		even

****************************************************************
;-------------- init trace mode if memory watch exists ---------

init_mwatch	movem.l	a0-a3,-(a7)
		tst.b	watch_size
		beq.w	.nowatch

;-------------- init watch_orig ---------------
		lea.l	watch_size(pc),a0
		lea.l	watch_addr(pc),a1
		lea.l	watch_orig(pc),a2
.loop		tst.b	(a0)+
		beq.b	.end
		move.l	(a1)+,a3
		move.l	(a3),(a2)+
		bra.b	.loop
.end
		move.l	vbr_reg,a4
		or.w	#$8000,sr_reg
		move.l	$24(a4),old_trace
		move.l	#watch_entry,$24(a4)

.nowatch	movem.l	(a7)+,a0-a3
		rts

watch_entry	move.w	#$2700,sr
		movem.l	d0-d1/a0-a3,-(a7)
		lea.l	watch_size(pc),a0
		lea.l	watch_addr(pc),a1
		lea.l	watch_orig-4(pc),a2
		moveq	#0,d0
.loop		addq.l	#4,a2
		move.b	(a0)+,d0
		beq.b	.end
		move.l	(a1)+,a3
		subq.b	#1,d0
		bne.b	.nob
		move.b	(a3),d1
		cmp.b	(a2),d1
		beq.b	.loop
		bra.b	.diff
.nob		subq.b	#1,d0
		bne.b	.now
		move.w	(a3),d1
		cmp.w	(a2),d1
		beq.b	.loop
		bra.b	.diff
.now		move.l	(a3),d1
		cmp.l	(a2),d1
		beq.b	.loop

.diff		move.l	a3,watch_trigger
		move.l	vbr_reg,a0
		move.l	old_trace,$24(a0)
		movem.l	(a7)+,d0-d1/a0-a3
		and.w	#$7fff,(a7)
		st	watch_flag
		bra.w	monitor

.end		movem.l	(a7)+,d0-d1/a0-a3
		or.w	#$8000,(a7)
		rte

ShowWatchEntry	movem.l	d0-d1/a0,-(a7)
		tst.b	watch_flag
		beq.b	.exit
		sf	watch_flag
		lea.l	.txt(pc),a0
		bsr	print
		move.l	watch_trigger(pc),d0
		moveq	#8,d1
		bsr	print_hexCR
.exit		movem.l	(a7)+,d0-d1/a0
		rts

.txt		dc.b "Watched memory changed at $",0
		even

****************************************************************
;-------------- delete all memory watch  -----------------------

cmd_mwd		lea.l	watch_size(pc),a0
		moveq	#MAX_WATCH-1,d0
.clear		clr.b	(a0)+
		dbf	d0,.clear
		lea.l	.txt(pc),a0
		bsr	print
		bra.w	end_command

.txt		dc.b "All memory watch deleted",$a,0
		even

****************************************************************
;-------------- FS find string (not casesensitive) -------------

cmd_fs		bsr	evaluate
		bne.w	illegal_addr
		move.l	d0,find_start
		bsr	evaluate
		bne.w	illegal_addr
		move.l	d0,find_end
		movem.l	find_start,d0-d1
		cmp.l	d0,d1
		ble.w	illegal_addr

		lea.l	find_list,a1
		bsr	read_name
		tst.b	(a1)
		beq.w	illegal_string

.up		move.b	(a1),d0
		beq.b	.endlist
		bsr	upper_case
		move.b	d0,(a1)+
		bra.b	.up
.endlist
		movem.l	find_start,a0-a1

		lea.l	watch,a3
		move.l	a0,(a3)

.seek		tst.b	break
		bne.w	.endfs
		lea.l	find_list,a2
		move.b	(a2)+,d1
.next		cmp.l	a1,a0
		bge.w	.endfs
		move.l	a0,(a3)
		move.l	a0,a4
		RELOC_PIC 1
		move.b	(a4),d0
		addq.l	#1,a0
		bsr	upper_case
		cmp.b	d0,d1
		bne.w	.next

		move.l	a0,d7
.ok		move.b	(a2)+,d1
		beq.w	.found
		cmp.l	a1,a0
		bge.w	.endfs
		move.l	a0,a4
		RELOC_PIC 2
		addq.l	#1,a0
		move.b	(a4),d0
		bsr	upper_case
		cmp.b	d0,d1
		beq.w	.ok
		move.l	d7,a0
		bra.w	.seek

.found		move.l	d7,d0
		subq.l	#1,d0
		lea.l	ev_line,a0
		move.w	#$2020,8(a0)
		clr.b	10(a0)
		moveq	#8,d1
		bsr	conv_hex
		bsr	print
		move.l	d7,a0
		bra.w	.seek

.endfs
		tst.w	cursor_x
		beq.b	.no_CR
		lea.l	cr_txt,a0
		bsr	print
.no_CR
		bra.w	end_command

****************************************************************

cmd_clear
		lea.l	sureclear_txt(pc),a0
		bsr	print
		bsr	get_key
		lea.l	sure2_txt(pc),a0
		bsr	print
		cmp.b	#'y',d0
		bne.b	.no
		bsr	remove_pic
		lea.l	$0.w,a0
		moveq	#0,d0
		moveq	#-1,d1
.clear		rept 8
		move.l	d0,(a0)+
		endr
		dbf	d1,.clear
		bsr	set_pic
		move.l	$f80004,a0
		subq.l	#2,a0
		move.l	a0,pc_reg
		move.w	#$2700,sr_reg
		st	escape

.no		bra.w	end_command

sureclear_txt	dc.b "All memory will be cleared and the machine will reboot!",$a
		dc.b "(You will have to reinstall HRTmon)",$a
		dc.b "Are you sure you want to continue ? (y/n)",$d,0
sure2_txt	dc.b "                                         ",$d,0
		even

****************************************************************
;-------------- SETMAP change keymap ----------------------

cmd_setmap	lea.l	ev_line,a1
		bsr	read_name
		tst.b	(a1)			;keymap name given ?
		beq.w	.noset

		lea.l	ev_line,a0
.up		move.b	(a0),d0
		bsr	upper_case
		move.b	d0,(a0)+		;upper-case keymap name
		tst.b	(a0)
		bne.b	.up

		lea.l	ev_line,a0
		cmp.l	#"USA"*256,(a0)
		bne.b	.noUSAset
		move.l	#board3,d0
		move.l	#board4,d1
		move.l	#board4a,d2
		bra.b	.set
.noUSAset	cmp.w	#"CH",(a0)
		bne.b	.noCHset
		tst.b	2(a0)
		bne.b	.noCHset
		move.l	#board1,d0
		move.l	#board2,d1
		move.l	#board2a,d2
		bra.b	.set
.noCHset	cmp.w	#"D"*256,(a0)
		bne.b	.noDset
		move.l	#board5,d0
		move.l	#board6,d1
		move.l	#board6a,d2
		bra.b	.set
.noDset		cmp.w	#"F"*256,(a0)
		bne.b	.noFset
		move.l	#board7,d0
		move.l	#board8,d1
		move.l	#board8a,d2
		bra.b	.set
.noFset
		lea.l	.maperr_txt(pc),a0
		bsr	print
		bra.b	.noset

.set		movem.l	d0-d2,board_ptr

.noset		lea.l	.key1_txt(pc),a0
		bsr	print

		move.l	board_ptr,d0
		cmp.l	#board3,d0
		bne.b	.noUSA
		lea.l	.key2_txt(pc),a0
.noUSA		cmp.l	#board1,d0
		bne.b	.noCH
		lea.l	.key3_txt(pc),a0
.noCH		cmp.l	#board5,d0
		bne.b	.noD
		lea.l	.key4_txt(pc),a0
.noD		cmp.l	#board7,d0
		bne.b	.noF
		lea.l	.key5_txt(pc),a0
.noF		bsr	print

		bra.w	end_command

.maperr_txt	dc.b "Illegal keymap name !",$a,0
.key1_txt	dc.b "Actual keymap is : ",0
.key2_txt	dc.b "USA",$a,0
.key3_txt	dc.b "CH",$a,0
.key4_txt	dc.b "D",$a,0
.key5_txt	dc.b "F",$a,0
		even

;-------------- print exception vectors and names --------------

cmd_excep	moveq	#0,d2			;first vector
		sub.l	a2,a2			;first vector
		move.w	window_bot,d4
		sub.w	window_top,d4
		move.w	d4,d5
.loop		tst.b	break
		bne	.exit
		move.l	d2,d0
		lea	_exceptionnames,a0
		jsr	_DoStringNull
		move.l	d0,d3
		beq	.next

		lea	.t1,a0
		bsr	print
		move.l	(a2),d0
		moveq	#8,d1
		bsr	print_hex
		lea	.t2,a0
		bsr	print
		move.l	a2,d0
		moveq	#2,d1
		bsr	print_hex
		lea	.t3,a0
		bsr	print
		move.l	d3,a0
		bsr	print
		jsr	_PrintLn

		subq.w	#1,d4
		bne	.next
		move.w	d5,d4
		bsr	get_key

.next		addq.w	#1,d2
		addq.l	#4,a2
		cmp.w	#64,d2
		bne	.loop

.exit		bra.w	end_command

.t2		dc.b	" : "
.t1		dc.b	"$",0
.t3		dc.b	" ",0
		even

;-------------- disk to file -----------------------------------
;-------------- read a whole disk and save it as a file --------

cmd_d2f
		lea.l	ev_line,a1
		bsr	read_name
		tst.b	(a1)
		beq.w	illegal_name

		bsr	test_present
		bne.w	.error

		move.l	#ev_line,d1
		moveq	#-1,d2		;create new file
		bsr	open_file
		move.l	d0,d7
		beq.b	.error

		move.w	#-1,ts_size		;trainer buffer cleared

		moveq	#0,d5
		move.w	#160-1,d6
.next		move.l	#tmp_mem,a0
		move.l	d5,d0
		moveq	#11,d1
		bsr	read2
		bne.b	.error
		add.l	d1,d5
		move.l	d7,d1
		move.l	#tmp_mem,d2
		move.l	#512*11,d3
		bsr	write_file
		bne.b	.error
		tst.b	break
		bne.b	.close
		dbf	d6,.next

.close		move.l	d7,d1
		bsr	close_file
.error

		bra.w	end_command


;-------------- file to disk -----------------------------------
;-------------- read a file and write it to disk ---------------

cmd_f2d		lea.l	ev_line,a1
		bsr	read_name
		tst.b	(a1)
		beq.w	illegal_name

		bsr	test_present
		bne.w	.error

		move.l	#ev_line,d1
		moveq	#0,d2		;open old file
		bsr	open_file
		move.l	d0,d7
		beq.w	.error
		move.l	d7,a0
		cmp.l	#80*22*512,file_size(a0)
		beq.b	.oksize
		lea.l	sizeErr_txt(pc),a0
		bsr	print
		bra.b	.error

.oksize

		move.w	#-1,ts_size		;trainer buffer cleared

		moveq	#0,d5
		move.w	#160-1,d6
.next		move.l	d7,d1
		move.l	#tmp_mem,d2
		move.l	#512*11,d3
		bsr	read_file
		bne.b	.error
		move.l	#tmp_mem,a0
		move.l	d5,d0
		moveq	#11,d1
		bsr	write2
		bne.b	.error
		add.l	d1,d5
		tst.b	break
		bne.b	.close
		dbf	d6,.next

.close		move.l	d7,d1
		bsr	close_file
.error

		bra.w	end_command

sizeErr_txt	dc.b "Wrong file size !",$a,0
		even

;-------------- show custom registers --------------------------

cmd_e		bsr	evaluate
		bne.b	.no_offset
		cmp.l	#$200,d0
		bge.w	illegal_addr
		btst	#0,d0
		bne.w	illegal_addr
		move.l	d0,custom_offset
		move.l	d0,d3
		bsr	evaluate		;edit ?
		bne.b	.no_offset
		lea.l	custom,a3
		move.w	d0,(a3,d3.l)
		bra.w	end_command

.no_offset	move.l	custom_offset,d5
		moveq	#8-1,d7			;print 8 lines
.loop		cmp.l	#$200,d5
		bge.b	.noprint

		bsr	make_e_line

		addq.l	#2,d5
		lea.l	general_txt,a0
		bsr	print
		dbf	d7,.loop

.noprint	move.l	d5,custom_offset

		bra.w	end_command

;-> d5=offset
;<- general_txt filled with e_line
make_e_line	movem.l	d0-a4,-(a7)
		lea.l	custom_names,a2
		lea.l	custom,a3
		lea.l	general_txt,a0
		move.w	#"e ",(a0)+
		move.b	#"$",(a0)+
		moveq	#3,d1
		move.w	d5,d0
		bsr	conv_hex		;print offset
		add.l	d1,a0
		move.w	#" $",(a0)+
		move.w	(a3,d5.w),d0
		moveq	#4,d1
		bsr	conv_hex		;print register value
		add.l	d1,a0
		move.l	#"  ; ",(a0)+
		lsl.w	#2,d5
		lea.l	(a2,d5.w),a4
		move.l	(a4)+,(a0)+
		move.l	(a4)+,(a0)+
		move.w	#$0a00,(a0)+
		movem.l	(a7)+,d0-a4
		rts

;-------------- scroll up e_line -----------

cmdu_e		move.l	ascII_ptr,a0
		lea.l	80(a0),a0
		cmp.w	#'e ',(a0)	;last top line was an e_line ?
		bne.b	.noe
		addq.l	#2,a0
		bsr	evaluate	;get last offset
		subq.l	#2,d0
		bmi.b	.noe

		move.l	d0,d5
		bsr.b	make_e_line

		lea.l	general_txt,a0
		bsr	print
		subq.w	#1,(2,a3)	;cursor one line up

		moveq	#0,d0

.noe		rts

;-------------- scroll down e_line ---------

cmdd_e		move.l	ascII_ptr,a0
		move.l	d0,-(a7)
		move.w	window_bot,d0
		subq.w	#2,d0
		mulu	#80,d0
		add.l	d0,a0
		move.l	(a7)+,d0
		cmp.w	#'e ',(a0)	;last line was an e_line ?
		bne.b	.noe
		addq.l	#2,a0
		bsr	evaluate	;get last address
		addq.l	#2,d0
		cmp.l	#$200,d0
		bge.b	.noe

		move.l	d0,d5
		bsr	make_e_line

		addq.l	#2,d5
		move.l	d5,custom_offset

		subq.w	#1,(2,a3)		;cursor one line up
		lea.l	general_txt,a0
		bsr	print

		moveq	#0,d0

.noe		rts


;-------------- delete a file or an empty dir ------------------

cmd_del		lea.l	ev_line,a1
		bsr	read_name		;get source name
		tst.b	(a1)
		beq.w	illegal_name

		move.l	#ev_line,d1
		bsr	delete_file

		bra.w	end_command

;-------------- get partition info -----------------------------

cmd_part

		move.l	#floppy0,d0

		lea.l	part_txt(pc),a0
		bsr	print

.next		move.l	d0,a4


		lea.l	part_name(a4),a0
		lea.l	general_txt,a1
		move.l	a1,a2
		moveq	#8-1,d0
.fill		move.l	#"    ",(a2)+
		dbf	d0,.fill

		moveq	#0,d0
		move.b	(a0)+,d0
		bra.b	.godbf
.copy		move.b	(a0)+,(a1)+		;copy name
.godbf		dbf	d0,.copy
		lea.l	part2_txt(pc),a0
		bsr	print
		lea.l	general_txt,a0
		sf	16(a0)
		bsr	print			;print name

		lea.l	part3_txt(pc),a0
		cmp.l	#ide_device,part_device(a4)
		beq.b	.ide
		lea.l	part4_txt(pc),a0
.ide		bsr	print			;device

		move.l	part_first(a4),d0
		moveq	#8,d1
		bsr	print_dec

		lea.l	part5_txt(pc),a0
		bsr	print

		move.l	part_nbsec(a4),d0
		moveq	#8,d1
		bsr	print_decCR

		move.l	part_next(a4),d0
		bne.b	.next

		bra.w	end_command

part_txt
 dc.b "--- Name ----------- -- Device -- -- First Block ----- -- NbBlocks --------",$a,0

part2_txt	dc.b "     ",0
part5_txt	dcb.b 11,$20
		dc.b 0

part3_txt	dc.b "    IDE       ",0
part4_txt	dc.b "    Floppy    ",0
		even

;-------------- get info from IDE drives -----------------------

cmd_ide		tst.b	config_IDE
		bne.b	.goide
		lea.l	.noide_txt(pc),a0
		bsr	print
		bra.w	end_command
.goide		tst.b	config_elsat
		beq.b	.nocd32
		lea.l	.cd32_txt(pc),a0
		bsr	print
		bra.b	.cont
.nocd32		tst.b	config_A1200
		beq.b	.go4000
		lea.l	.a1200_txt(pc),a0
		bsr	print
		bra.b	.cont
.go4000		lea.l	.a4000_txt(pc),a0
		bsr	print

.cont		lea.l	secbuf,a0
		tst.w	IDE_info0
		beq.b	.nodrive0
		moveq	#0,d1		;drive no 0
		bsr	Read_ID
		bne.b	.nodrive0
		lea.l	ide1_txt(pc),a0
		bsr	print
		bsr	.print_info
		bra.b	.okdrive0
.nodrive0	lea.l	ide2_txt(pc),a0
		bsr	print

.okdrive0	lea.l	secbuf,a0
		tst.w	IDE_info1
		beq.b	.nodrive1
		moveq	#1,d1		;drive no 1
		bsr	Read_ID
		bne.b	.nodrive1
		lea.l	ide3_txt(pc),a0
		bsr	print
		bsr	.print_info
		bra.b	.okdrive1
.nodrive1	lea.l	ide4_txt(pc),a0
		bsr	print

.okdrive1	moveq	#0,d0
		moveq	#0,d1
		tst.w	IDE_info0
		beq.b	.no0
		lea.l	secbuf,a0
		bsr	Read_Block
.no0
		bra.w	end_command

.noide_txt	dc.b "No IDE interface",$a,0
.cd32_txt	dc.b "ELSAT CD32 Pro Module IDE interface",$a,0
.a1200_txt	dc.b "Gayle A1200 IDE interface",$a,0
.a4000_txt	dc.b "Gayle A4000 IDE interface",$a,0
		even

.print_info	lea.l	secbuf,a4
		lea.l	ide5_txt(pc),a0
		bsr	print
		lea.l	27*2(a4),a0
		move.w	#$0a00,46*2(a4)
		tst.w	(a0)
		bne.b	.okname
		lea.l	ide10_txt(pc),a0
.okname		bsr	print			;name

		lea.l	ide6_txt(pc),a0
		bsr	print
		lea.l	10*2(a4),a0
		move.w	#$0a00,19*2(a4)
		tst.w	(a0)
		bne.b	.okser
		lea.l	ide10_txt(pc),a0
.okser		cmp.b	#$20,(a0)+
		beq.b	.okser
		subq.l	#1,a0
		move.l	a0,a1
.seeke		tst.b	(a1)+
		bne.b	.seeke
		move.b	#$a,-2(a1)
		bsr	print			;serial

		lea.l	ide7_txt(pc),a0
		bsr	print
		move.w	1*2(a4),d0
		moveq	#4,d1
		bsr	print_decCR		;cylinders

		lea.l	ide8_txt(pc),a0
		bsr	print
		move.w	3*2(a4),d0
		moveq	#4,d1
		bsr	print_decCR		;heads

		lea.l	ide9_txt(pc),a0
		bsr	print
		move.w	6*2(a4),d0
		moveq	#4,d1
		bsr	print_decCR		;sectors

		lea.l	ide11_txt(pc),a0
		bsr	print
		moveq	#5,d1
		move.w	3*2(a4),d0
		mulu	6*2(a4),d0		;sec*heads
		mulu	1*2(a4),d0		;cyl*sec*heads
		moveq	#11,d1
		lsr.l	d1,d0
		moveq	#4,d1
		bsr	print_dec
		lea.l	ide12_txt(pc),a0
		bsr	print

		rts

ide1_txt	dc.b "-- Drive 0   : present",$a,0
ide2_txt	dc.b "-- Drive 0   : absent",$a,0
ide3_txt	dc.b "-- Drive 1   : present",$a,0
ide4_txt	dc.b "-- Drive 1   : absent",$a,0
ide5_txt	dc.b "   Name      : ",0
ide6_txt	dc.b "   Serial no : ",0
ide7_txt	dc.b "   Cylinders : ",0
ide8_txt	dc.b "   Heads     : ",0
ide9_txt	dc.b "   Sectors   : ",0
ide10_txt	dc.b "N/A",$a,0
ide11_txt	dc.b "   Capacity  : ",0
ide12_txt	dc.b "(MB)",$a,0
		even

;-------------- COPY --------------------------------------

cmd_copy
		lea.l	ev_line,a1
		bsr	read_name		;get source name
		tst.b	(a1)
		beq.w	illegal_name
		lea.l	general_txt,a1
		bsr	read_name		;get dest name
		tst.b	(a1)
		beq.w	illegal_name

		bsr	remove_pic

		moveq	#0,d6
		moveq	#0,d7
		move.l	#ev_line,d1
		moveq	#0,d2
		bsr	open_file
		move.l	d0,d6
		beq.w	.err
		move.l	#general_txt,d1
		moveq	#-1,d2
		bsr	open_file
		move.l	d0,d7
		bne.b	.okdest

		cmp.w	#FILEEXIST_ERR,drive_err
		bne.w	.err		;got only path without filename ?
		clr.w	drive_err
		lea.l	general_txt,a0
.seek		tst.b	(a0)+
		bne.b	.seek
		subq.l	#1,a0
		cmp.b	#':',-1(a0)
		beq.b	.noslash
		move.b	#'/',(a0)+
.noslash	move.l	d6,a1
		lea.l	file_name(a1),a1
		moveq	#0,d0
		move.b	(a1)+,d0
		bra.b	.godbf
.addname	move.b	(a1)+,(a0)+	;add filename to dest
.godbf		dbf	d0,.addname
		sf	(a0)

		move.l	#general_txt,d1
		moveq	#-1,d2
		bsr	open_file	;retry with filename
		move.l	d0,d7
		beq.b	.err
.okdest

		move.l	d6,a0
		move.l	file_size(a0),d5
		beq.b	.err

		move.w	#-1,ts_size		;trainer buffer cleared

.nextblock	move.l	#tmp_mem_size,d3
		cmp.l	d3,d5
		bge.b	.okd5
		move.l	d5,d3
.okd5		sub.l	d3,d5

		move.l	d6,d1
		move.l	#tmp_mem,d2
		bsr	read_file
		tst.l	d0
		bne.b	.err

		move.l	d7,d1
		bsr	write_file
		tst.l	d0
		bne.b	.err

		tst.l	d5
		bne.b	.nextblock

.err		move.l	d6,d1
		beq.b	.noclose1
		bsr	close_file
.noclose1	move.l	d7,d1
		beq.b	.noclose2
		bsr	close_file
.noclose2
		bsr	set_pic

		bra.w	end_command

;-------------- OUTPUT ------------------------------------

cmd_output	bsr	evaluate
		bne.b	.end
		tst.l	d0
		beq.b	.end
		move.l	d0,output_ptr
		move.l	d0,output_start
;		move.l	d0,watch2

		bra.b	.out

.end		move.l	output_ptr,d2
		move.l	output_start,d0
		clr.l	output_ptr
		clr.l	output_start

		lea.l	endout_txt,a0
		bsr	print
		moveq	#8,d1
		bsr	print_hex		;output_start
		lea.l	endout2_txt,a0
		bsr	print
		move.l	d2,d0			;output_ptr
		bsr	print_hexCR

.out		bra.w	end_command

endout_txt	dc.b "Output memory from $",0
endout2_txt	dc.b " to $",0
		cnop 0,4

;-------------- Q (compare memory) ------------------------

cmd_q		bsr	evaluate
		bne.w	illegal_addr
		move.l	d0,cmp_start
		bsr	evaluate
		bne.w	illegal_addr
		move.l	d0,cmp_end
		sub.l	cmp_start,d0
		ble.w	illegal_addr
		bsr	evaluate
		bne.w	illegal_addr
		move.l	d0,cmp_dest

		bsr	remove_pic
		moveq	#0,d0
		move.l	d0,$180(a6)

		move.l	cmp_start,a0
		move.l	cmp_dest,a1
		move.l	cmp_end,a2
		moveq	#-1,d0

.ok		cmp.l	a2,a0
		bge.b	.end_cmp
		cmpm.b	(a0)+,(a1)+
		beq.b	.ok
		move.l	a0,d0
		subq.l	#1,d0
.end_cmp
		bsr	set_pic

		moveq	#-1,d1
		cmp.l	d1,d0
		beq.b	.equal

		moveq	#8,d1
		bsr	print_hexCR
		bra.b	.out
.equal
		lea.l	equal_txt,a0
		bsr	print
.out
		bra.w	end_command

equal_txt	dc.b "Equal areas.",$a,0
		even

;-------------- A -----------------------------------------

cmd_a		move.l	a0,a3
		bsr	evaluate
		bne.b	.godisas
		btst	#0,d0
		bne.w	illegal_addr
		move.l	d0,a1
		move.l	a1,ass_addr
.skip		cmp.b	#$20,(a0)+
		beq.b	.skip
		subq.l	#1,a0
		tst.b	(a0)
		bne.b	.goas
.godisas	move.l	a3,a0
		bra.w	cmd_d

.goas		jsr	assemble
		tst.l	d0
		bne.w	illegal_syntax

.ok		lea.l	op68000,a0
		move.l	ass_addr,a4
		bsr	reloc_pic
		move.w	oplen,d0
.copy		move.w	(a0)+,(a4)+
		dbf	d0,.copy

		moveq	#8,d1
		move.l	ass_addr,d0
		moveq	#0,d2
		move.w	oplen,d2
		add.w	d2,d2
		addq.l	#2,d2
		add.l	d2,d0

		lea.l	new_ass(pc),a0
		bsr	print
		bsr	print_hex
		lea.l	new_ass2(pc),a0
		bsr	print
		bra.w	end_command

new_ass		dc.b "a $",0
new_ass2	dc.b " ",0
		cnop 0,4

;-------------- O -----------------------------------------
;fill memory
cmd_o		bsr	evaluate
		bne.w	illegal_addr
		move.l	d0,fill_start
		move.l	d0,d1
		bsr	evaluate
		bne.w	illegal_addr
		move.l	d0,fill_end
		sub.l	d1,d0
		ble.w	illegal_addr

		clr.w	eval_size		;.b par default
		bsr	evaluate

		bsr	remove_pic
		movem.l	fill_start,a0-a1
		move.w	eval_size,d1
		cmp.w	#3,d1
		beq.b	.fillL
		cmp.w	#1,d1
		beq.b	.fillW
.fillb		move.b	d0,(a0)+
		cmp.l	a1,a0
		blt.b	.fillb
		bra.b	.out
.fillW		move.w	d0,(a0)+
		cmp.l	a1,a0
		blt.b	.fillW
		bra.b	.out
.fillL		move.l	d0,(a0)+
		cmp.l	a1,a0
		blt.b	.fillL


.out		bsr	set_pic

		bra.w	end_command

;-------------- Switch led --------------------------------

cmd_led		bchg	#1,$bfe001
		bchg	#1,CIAA+0
		bra.w	end_command

;-------------- PAL/NTSC ----------------------------------

cmd_pal		clr.l	OldRaster
		move.w	#$20,custom+$1dc
		bra.w	end_command
cmd_ntsc	clr.l	OldRaster
		move.w	#0,custom+$1dc
		bra.w	end_command

cmd_31k		clr.l	OldRaster
		move.w	#$3a0,custom+$1dc
		bra.w	end_command

;-------------- S (SAVEFILE) ------------------------------

cmd_s
		lea.l	ev_line,a1
		bsr.w	read_name
		tst.b	(a1)
		beq.w	illegal_name
		bsr	evaluate
		bne.w	illegal_addr
		bne.w	illegal_addr
		move.l	d0,d4			;d4=save start
		bsr	evaluate
		bne.w	illegal_addr
		bne.w	illegal_addr
		move.l	d0,d3			;d3=save end
		move.l	d0,d1
		move.l	d4,d0
		sub.l	d4,d3			;d3=save len
		ble.w	illegal_addr		;end must be > start

		bsr	remove_pic

		move.l	#ev_line,d1		;ptr on filename
		moveq	#-1,d2			;mode create
		bsr	open_file
		move.l	d0,d7
		beq.b	.err

		move.l	d4,d2
		move.l	d7,d1
		bsr	write_file		;save mem

		move.l	d7,d1
		bsr	close_file

.err		bsr	set_pic

		bra.w	end_command


;-------------- MAKEDIR -----------------------------------

cmd_makedir	lea.l	ev_line,a1
		bsr.w	read_name
		tst.b	(a1)
		beq.w	illegal_name

		move.l	#ev_line,d1
		bsr	create_dir

		bra.w	end_command

;-------------- SA ----------------------------------------
;-------------- save the actual state in a file -----------

cmd_sa
		lea.l	ev_line,a1
		bsr.w	read_name
		tst.b	(a1)
		beq.w	illegal_name

		sub.l	a0,a0
		move.l	max_chip,a1
		bsr	search_cop

		move.w	drive,-(a7)
		moveq	#3,d4			;first drive SEL (drive0)
		lea.l	drive_present,a3
		moveq	#4-1,d5			;4 drives
.loopd		tst.b	(a3)+
		beq.b	.nodrive
		move.w	d4,drive		;get
		bsr	inittete		;floppy drive head pos
.nodrive	addq.w	#1,d4
		dbf	d5,.loopd
		move.w	(a7)+,drive

		bsr	remove_pic

		move.l	#ev_line,d1		;ptr on filename
		moveq	#-1,d2			;mode create
		bsr	open_file
		move.l	d0,d7
		beq.w	.err

		move.w	#-1,ts_size		;trainer buffer cleared

		lea.l	tmp_mem,a4
		move.l	#"ARSV",(a4)

		move.l	d7,d1
		move.l	a4,d2
		moveq	#4,d3
		bsr	write_file		;save header 'ARSV'
		bne.w	.err

		move.l	d7,d1
		move.l	#registres,d2
		move.l	#end_registres-registres,d3
		bsr	write_file		;save registers
		bne.w	.err

		move.l	d7,d1
		move.l	#custom,d2
		move.l	#$200,d3
		bsr	write_file		;save custom registers
		bne.w	.err

		move.l	d7,d1
		move.l	#palette,d2
		move.l	#256*4,d3
		bsr	write_file		;save palette
		bne.w	.err

		move.l	d7,d1
		move.l	#CIAA,d2
		moveq	#32,d3
		bsr	write_file		;save CIA registers
		bne.w	.err

		move.l	d7,d1
		move.l	#old_head,d2
		moveq	#4*2,d3
		bsr	write_file		;write head pos of drives
		bne.w	.err

		move.l	d7,a0
		move.l	file_part(a0),a0
		cmp.l	#floppy_device,part_device(a0)
		bne.w	.goHD

;-------------- floppy save routine ----------------
		move.l	d7,d1
		moveq	#0,d2
		move.l	#$CD000,d3
		bsr	write_file		;write data on Disk1
		bne.w	.err
		move.l	d7,d1
		bsr	close_file

		bsr	flush_fbuffer
		bsr	force_change

		bsr	set_pic
		lea.l	disk2_txt(pc),a0
		bsr	print
		bsr	get_key
		lea.l	diskclr_txt(pc),a0
		bsr	print
		bsr	remove_pic
		tst.b	break
		bne.w	.err

		move.l	#ev_line,d1		;ptr on filename
		moveq	#-1,d2			;mode create
		bsr	open_file
		move.l	d0,d7
		beq.w	.err

		lea.l	tmp_mem,a4
		move.l	#"ARS2",(a4)
		move.l	d7,d1
		move.l	a4,d2
		moveq	#4,d3
		bsr	write_file
		bne.w	.err

		move.l	d7,d1
		move.l	#$CD000,d2
		move.l	d2,d3
		bsr	write_file		;write data on Disk2
		bne.w	.err
		move.l	d7,d1
		bsr	close_file

		bsr	flush_fbuffer
		bsr	force_change

		bsr	set_pic
		lea.l	disk3_txt(pc),a0
		bsr	print
		bsr	get_key
		lea.l	diskclr_txt(pc),a0
		bsr	print
		bsr	remove_pic
		tst.b	break
		bne.b	.err

		move.l	#ev_line,d1		;ptr on filename
		moveq	#-1,d2			;mode create
		bsr	open_file
		move.l	d0,d7
		beq.b	.err

		lea.l	tmp_mem,a4
		move.l	#"ARS3",(a4)
		move.l	d7,d1
		move.l	a4,d2
		moveq	#4,d3
		bsr	write_file
		bne.b	.err

		move.l	d7,d1
		move.l	#$CD000*2,d2
		move.l	#$200000-$CD000*2,d3
		bsr	write_file		;write data on Disk3
		bne.b	.err
		bra.b	.endsave

.goHD		move.l	d7,d1
		moveq	#0,d2
		move.l	#$200000,d3
		bsr	write_file		;save all CHIPMEM
		bne.b	.err

.endsave	move.l	d7,d1
		bsr	close_file

.err		bsr	set_pic

		bra.w	end_command


disk1_txt	dc.b "Please insert disk 1 and press any key.",$d,0
disk2_txt	dc.b "Please insert disk 2 and press any key.",$d,0
disk3_txt	dc.b "Please insert disk 3 and press any key.",$d,0
diskclr_txt	dc.b "                                                   ",$d,0
		even


;-------------- SAC ---------------------------------------
;-------------- save the actual state in a file compressed

cmd_sac
		lea.l	ev_line,a1
		bsr.w	read_name
		tst.b	(a1)
		beq.w	illegal_name

		sub.l	a0,a0
		move.l	max_chip,a1
		bsr	search_cop

		move.w	drive,-(a7)
		moveq	#3,d4			;first drive SEL (drive0)
		lea.l	drive_present,a3
		moveq	#4-1,d5			;4 drives
.loopd		tst.b	(a3)+
		beq.b	.nodrive
		move.w	d4,drive		;get
		bsr	inittete		;floppy drive head pos
.nodrive	addq.w	#1,d4
		dbf	d5,.loopd
		move.w	(a7)+,drive

		bsr	remove_pic

		move.l	#ev_line,d1		;ptr on filename
		moveq	#-1,d2			;mode create
		bsr	open_file
		move.l	d0,d7
		beq.w	.err

		move.w	#-1,ts_size		;trainer buffer cleared

		lea.l	tmp_mem,a4
		move.l	#"ARSC",(a4)

		move.l	d7,d1
		move.l	a4,d2
		moveq	#4,d3
		bsr	write_file		;save header 'ARSC'
		bne.w	.err

		move.l	d7,d1
		move.l	#registres,d2
		move.l	#end_registres-registres,d3
		bsr	write_file		;save registers
		bne.w	.err

		move.l	d7,d1
		move.l	#custom,d2
		move.l	#$200,d3
		bsr	write_file		;save custom registers
		bne.w	.err

		move.l	d7,d1
		move.l	#palette,d2
		move.l	#256*4,d3
		bsr	write_file		;save palette
		bne.w	.err

		move.l	d7,d1
		move.l	#CIAA,d2
		moveq	#32,d3
		bsr	write_file		;save CIA registers
		bne.w	.err

		move.l	d7,d1
		move.l	#old_head,d2
		moveq	#4*2,d3
		bsr	write_file		;write head pos of drives
		bne.w	.err

		move.l	d7,a0
		move.l	file_part(a0),a0
		cmp.l	#floppy_device,part_device(a0)
		bne.w	.goHD

;-------------- floppy save routine ----------------

		move.l	d7,d1
		moveq	#0,d2
		move.l	#$4008,d3
		bsr	write_file		;write low-mem
		bne.w	.err

		lea.l	$4008.w,a0
		sub.l	a1,a1
		move.l	#$200000-$4008,d0
		bsr	pack
		move.l	d0,d6			;d6=packed size

		move.l	d6,d3
		move.l	#1730*512-$4008-$68e,d0	;size left on disk
		cmp.l	d0,d3
		blt.b	.okd3
		move.l	d0,d3
.okd3		sub.l	d3,d6
		move.l	d7,d1
		moveq	#0,d2
		bsr	write_file		;write data on 1st disk
		bne.w	.err

		move.l	d7,d1
		bsr	close_file

		tst.l	d6
		beq.w	.lowmem

;----------

		bsr	flush_fbuffer
		bsr	force_change

		bsr	set_pic
		lea.l	disk2_txt(pc),a0
		bsr	print
		bsr	get_key
		lea.l	diskclr_txt(pc),a0
		bsr	print
		bsr	remove_pic
		tst.b	break
		bne.w	.err

		move.l	#ev_line,d1		;ptr on filename
		moveq	#-1,d2			;mode create
		bsr	open_file
		move.l	d0,d7
		beq.w	.err

		lea.l	tmp_mem,a4
		move.l	#"ARC2",(a4)
		move.l	d7,d1
		move.l	a4,d2
		moveq	#4,d3
		bsr	write_file
		bne.w	.err

		move.l	d6,d3
		move.l	#1730*512-4,d0
		cmp.l	d0,d3
		blt.b	.okd3b
		move.l	d0,d3
.okd3b		sub.l	d3,d6
		move.l	d7,d1
		move.l	#1730*512-$4008-$68e,d2	;size saved on 1st disk
		bsr	write_file
		bne.w	.err

		move.l	d7,d1
		bsr	close_file
		tst.l	d6
		beq.w	.enddisk

;----------

		bsr	flush_fbuffer
		bsr	force_change

		bsr	set_pic
		lea.l	disk3_txt(pc),a0
		bsr	print
		bsr	get_key
		lea.l	diskclr_txt(pc),a0
		bsr	print
		bsr	remove_pic
		tst.b	break
		bne.w	.err

		move.l	#ev_line,d1		;ptr on filename
		moveq	#-1,d2			;mode create
		bsr	open_file
		move.l	d0,d7
		beq.w	.err

		lea.l	tmp_mem,a4
		move.l	#"ARC3",(a4)
		move.l	d7,d1
		move.l	a4,d2
		moveq	#4,d3
		bsr	write_file
		bne.w	.err

		move.l	d6,d3
		move.l	d7,d1
		move.l	#(1730*512-$4008-$68e)+(1730*512-4),d2
						;size saved on disks 1&2
		bsr	write_file
		bne.w	.err

.retry		move.l	d7,d1
		bsr	close_file

.enddisk	bsr	flush_fbuffer
		bsr	force_change
		clr.w	drive_err

		bsr	set_pic
		lea.l	disk1_txt(pc),a0
		bsr	print
		bsr	get_key
		lea.l	diskclr_txt(pc),a0
		bsr	print
		bsr	remove_pic
		tst.b	break
		bne.w	.err

.lowmem		move.l	#ev_line,d1
		moveq	#0,d2
		bsr	open_file
		move.l	d0,d7
		beq.b	.enddisk

		lea.l	tmp_mem,a4
		move.l	a4,d2
		moveq	#4,d3
		move.l	d7,d1
		bsr	read_file
		bne.b	.retry
		cmp.l	#"ARSC",(a4)
		bne.b	.retry

		move.l	d7,d1
		move.l	#$68e,d2
		moveq	#-1,d3
		bsr	seek_file

		sub.l	a0,a0
		lea.l	$4008.w,a1
		bsr	depack

		move.l	d7,d1
		moveq	#0,d2
		move.l	#$4008,d3
		bsr	read_file

		bra.b	.endsave

;-----------------------------------------------

.goHD		move.l	d7,d1
		moveq	#0,d2
		move.l	#$4008,d3
		bsr	write_file		;save low-mem
		bne.b	.err

		lea.l	$4008.w,a0
		lea.l	$0.w,a1
		move.l	#$200000-$4008,d0
		bsr	pack
		move.l	d0,d3
		move.l	d0,d5			;d5=packed size+header
		move.l	d7,d1
		moveq	#0,d2
		bsr	write_file		;save packed mem
		bne.b	.err

		lea.l	$0.w,a0
		lea.l	$4008.w,a1
		bsr	depack

		move.l	d7,d1
		move.l	d5,d2
		add.l	#$4008,d2
		neg.l	d2
		moveq	#0,d3
		bsr	seek_file

		move.l	d7,d1
		moveq	#0,d2
		move.l	#$4008,d3
		bsr	read_file

.endsave	move.l	d7,d1
		bsr	close_file

.err		bsr	set_pic

		bra.w	end_command


;-------------- LA ----------------------------------------
;-------------- load a saved game -------------------------

cmd_la		lea.l	ev_line,a1
		bsr.w	read_name
		tst.b	(a1)
		beq.w	illegal_name

		bsr	remove_pic

		move.l	#ev_line,d1		;ptr on filename
		moveq	#0,d2			;mode oldfile
		bsr	open_file
		move.l	d0,d7
		beq.w	.err

		lea.l	tmp_mem,a4

		move.w	#-1,ts_size		;trainer buffer cleared

		move.l	d7,d1
		move.l	a4,d2
		moveq	#4,d3
		bsr	read_file		;read header 'ARSV'
		bne.w	.err

		sf	packed
		cmp.l	#'ARSV',(a4)
		beq.b	.okarsv
		st	packed
		cmp.l	#'ARSC',(a4)
		beq.b	.okarsv
		lea.l	noarsv_txt(pc),a0
		bsr	print
		bra.w	.close
.okarsv
		move.l	d7,d1
		move.l	#registres,d2
		move.l	#end_registres-registres,d3
		bsr	read_file		;read registers
		bne.w	.err

		move.l	d7,d1
		move.l	#custom,d2
		move.l	#$200,d3
		bsr	read_file		;read custom registers
		bne.w	.err

		lea.l	custom,a1
		lea.l	$dff000,a2
		lea.l	move_list2(pc),a0
		move.w	#$100-1,d0
.loop		tst.b	(a0)+
		beq.b	.nomove
		move.w	(a1),(a2)		;reinit custom registers
.nomove		addq.l	#2,a1
		addq.l	#2,a2
		dbf	d0,.loop
		clr.w	$dff088
		move.w	#0,$dff106
		move.w	#0,$dff180

		move.l	d7,d1
		move.l	#palette,d2
		move.l	#256*4,d3
		bsr	read_file		;read palette
		bne.w	.err

		move.l	d7,d1
		move.l	#CIAA,d2
		moveq	#32,d3
		bsr	read_file		;read CIA registers
		bne.w	.err

		move.l	d7,d1
		move.l	a4,d2
		moveq	#8,d3
		bsr	read_file		;read drive headpos

		lea.l	old_head,a1
		move.l	a4,a2
		move.w	drive,-(a7)
		moveq	#3,d4			;first drive SEL (drive0)
		lea.l	drive_present,a3
		moveq	#4-1,d5			;4 drives
.loopd		tst.b	(a3)+
		beq.b	.nodrive
		move.w	d4,drive		;init
		bsr	inittete		;floppy drive head pos
		move.w	(a2),(a1)		;init old_head
.nodrive	addq.w	#1,d4
		addq.l	#2,a1
		addq.l	#2,a2
		dbf	d5,.loopd
		move.w	(a7)+,drive

		move.l	d7,a0
		move.l	file_part(a0),a0
		cmp.l	#floppy_device,part_device(a0)
		bne.w	.goHD

;-------------- floppy load --------------------

		tst.b	packed
		beq.w	.nopackedf

;-------------- packed floppy load ---------

		move.l	d7,d1
		move.l	#$4008,d2
		moveq	#0,d3
		bsr	seek_file		;skip low-mem

		move.l	d7,d1
		moveq	#0,d2
		move.l	#1730*512-$68e-$4008,d3
		bsr	read_file		;read 1st disk

		move.l	$0.w,d6
		bpl.b	.okposf
		neg.l	d6
.okposf		addq.l	#8,d6		;d6=packed size
		move.l	d1,a0
		move.l	file_size(a0),d0
		sub.l	#$68e+$4008,d0
		move.l	d0,d5
		sub.l	d0,d6
		beq.w	.lowmem

.retry2c	move.l	d7,d1
		bsr	close_file

.retry2bc	bsr	set_pic
		lea.l	disk2_txt(pc),a0
		bsr	print
		bsr	get_key
		lea.l	diskclr_txt(pc),a0
		bsr	print
		bsr	remove_pic
		tst.b	break
		bne.w	.err

		clr.w	drive_err
		move.l	#ev_line,d1		;ptr on filename
		moveq	#0,d2			;mode oldfile
		bsr	open_file
		move.l	d0,d7
		beq.b	.retry2bc

		lea.l	tmp_mem,a4
		move.l	d7,d1
		move.l	a4,d2
		moveq	#4,d3
		bsr	read_file
		bne.b	.retry2c
		cmp.l	#"ARC2",(a4)
		bne.b	.retry2c

		move.l	d7,d1
		move.l	d5,d2
		move.l	#1730*512-4,d3
		bsr	read_file		;read disk2

		move.l	d1,a0
		move.l	file_size(a0),d0
		subq.l	#4,d0
		sub.l	d0,d6
		beq.b	.enddisk
		add.l	d0,d5

.retry3c	move.l	d7,d1
		bsr	close_file

.retry3bc	bsr	set_pic
		lea.l	disk3_txt(pc),a0
		bsr	print
		bsr	get_key
		lea.l	diskclr_txt(pc),a0
		bsr	print
		bsr	remove_pic
		tst.b	break
		bne.w	.err

		clr.w	drive_err
		move.l	#ev_line,d1		;ptr on filename
		moveq	#0,d2			;mode oldfile
		bsr	open_file
		move.l	d0,d7
		beq.b	.retry3bc

		lea.l	tmp_mem,a4
		move.l	d7,d1
		move.l	a4,d2
		moveq	#4,d3
		bsr	read_file
		bne.b	.retry3c
		cmp.l	#"ARC3",(a4)
		bne.b	.retry3c

		move.l	d7,d1
		move.l	d5,d2
		move.l	#1730*512-4,d3
		bsr	read_file		;read disk3

.enddisk
		move.l	d7,d1
		bsr	close_file

	;ask disk1 and read low-mem

.retry1		bsr	set_pic
		lea.l	disk1_txt(pc),a0
		bsr	print
		bsr	get_key
		lea.l	diskclr_txt(pc),a0
		bsr	print
		bsr	remove_pic
		tst.b	break
		bne.w	.err

		clr.w	drive_err
		move.l	#ev_line,d1		;ptr on filename
		moveq	#0,d2			;mode oldfile
		bsr	open_file
		move.l	d0,d7
		beq.b	.retry1

		lea.l	tmp_mem,a4
		move.l	d7,d1
		move.l	a4,d2
		moveq	#4,d3
		bsr	read_file
		bne.b	.enddisk
		cmp.l	#"ARSC",(a4)
		bne.b	.enddisk

.lowmem		move.l	d7,d1
		move.l	#$68e,d2
		moveq	#-1,d3
		bsr	seek_file

		sub.l	a0,a0
		lea.l	$4008.w,a1
		bsr	depack

		move.l	d7,d1
		moveq	#0,d2
		move.l	#$4008,d3
		bsr	read_file		;read low-mem

		bra.w	.close

;-------------- non-packed floppy load -----

.nopackedf	move.l	d7,d1
		moveq	#0,d2
		move.l	#$CD000,d3
		bsr	read_file
		bne.w	.err

.retry2		move.l	d7,d1
		bsr	close_file

.retry2b	bsr	set_pic
		lea.l	disk2_txt(pc),a0
		bsr	print
		bsr	get_key
		lea.l	diskclr_txt(pc),a0
		bsr	print
		bsr	remove_pic
		tst.b	break
		bne.w	.err

		clr.w	drive_err
		move.l	#ev_line,d1		;ptr on filename
		moveq	#0,d2			;mode oldfile
		bsr	open_file
		move.l	d0,d7
		beq.b	.retry2b

		lea.l	tmp_mem,a4
		move.l	d7,d1
		move.l	a4,d2
		moveq	#4,d3
		bsr	read_file
		bne.b	.retry2
		cmp.l	#"ARS2",(a4)
		bne.b	.retry2

		move.l	d7,d1
		move.l	#$CD000,d2
		move.l	d2,d3
		bsr	read_file		;read data from disk2
		bne.w	.err

.retry3		move.l	d7,d1
		bsr	close_file

.retry3b	bsr	set_pic
		lea.l	disk3_txt(pc),a0
		bsr	print
		bsr	get_key
		lea.l	diskclr_txt(pc),a0
		bsr	print
		bsr	remove_pic
		tst.b	break
		bne.w	.err

		clr.w	drive_err
		move.l	#ev_line,d1		;ptr on filename
		moveq	#0,d2			;mode oldfile
		bsr	open_file
		move.l	d0,d7
		beq.b	.retry3b

		lea.l	tmp_mem,a4
		move.l	d7,d1
		move.l	a4,d2
		moveq	#4,d3
		bsr	read_file
		bne.b	.retry3
		cmp.l	#"ARS3",(a4)
		bne.b	.retry3

		move.l	d7,d1
		move.l	#2*$CD000,d2
		move.l	#$200000-2*$CD000,d3
		bsr	read_file		;read data from disk3
		bne.w	.err
		bra.w	.close

;------------------------------------------------

.goHD		tst.b	packed
		beq.b	.nopacked
		move.l	d7,d1
		move.l	#$4008,d2
		moveq	#0,d3
		bsr	seek_file		;skip low-mem
		move.l	d7,d1
		moveq	#0,d2
		moveq	#8,d3
		bsr	read_file		;read pack header
		bne.b	.err
		move.l	$0.w,d3			;get packed size
		bpl.b	.okpos
		neg.l	d3
.okpos		move.l	d7,d1
		moveq	#8,d2
		move.l	d3,d5
		bsr	read_file		;read packed data
		lea.l	$0.w,a0
		lea.l	$4008.w,a1
		bsr	depack

		move.l	d7,d1
		move.l	d5,d2
		add.l	#$4008+8,d2
		neg.l	d2
		moveq	#0,d3
		bsr	seek_file

		move.l	d7,d1
		moveq	#0,d2
		move.l	#$4008,d3
		bsr	read_file		;read low-mem area

		bra.b	.close

.nopacked	move.l	d7,d1
		moveq	#0,d2
		move.l	#$200000,d3
		bsr	read_file		;read all CHIPMEM
		bne.b	.err

.close		move.l	d7,d1
		bsr	close_file

.err		bsr	set_pic

		bra.w	end_command

noarsv_txt	dc.b "Not a HRTmon save file !",$a,0
		even

;-------------- list of registers to be initialised after la_cmd ----

move_list2	dcb.b $10,0		;$00-$1e
		dc.b -1,-1,-1		;$20-$24
		dc.b 0,0,0,0		;$26-$2c
		dcb.b 5,-1		;$2e-$36
		dcb.b 4,0		;$38-$3e
		dcb.b $c,-1		;$40-$56
		dc.b 0,0		;$58,$5a
		dc.b 0,0		;$5c,$5e
		dcb.b 4,-1		;$60-$66
		dcb.b 4,0		;$68-$6e
		dc.b -1,-1,-1		;$70-$74
		dcb.b 4,0		;$76-$7c
		dc.b -1			;$7e
		dc.b -1,-1		;$80,$82	;cop1
		dc.b -1,-1		;$84,$86	;cop2
		dc.b 0,0,0		;$88,$8a,$8c
		dc.b -1,-1,-1,-1	;$8e,$90,$92,$94
		dc.b 0,-1,0,0		;$96-$9c
		dcb.b $21,-1		;$9e-$de
		dcb.b $10,-1		;$e0-$fe
		dc.b 0,0,-1,0		;$100-$106
		dc.b -1,-1,0		;$108-$10c
		dc.b -1			;$10e
		dcb.b 8,0		;$110-$11e
		dcb.b $10,-1		;$120-$13e
		dcb.b $20,0		;$140-$17e
		dcb.b $20,0		;$180-$1be
		dcb.b $d,-1		;$1c0-$1d8
		dc.b 0			;$1da
		dc.b 0			;$1dc
		dc.b -1,-1,-1		;$1de-$1e2
		dc.b 0			;$1e4
		dcb.b 5,-1		;$1e6-$1ee
		dcb.b 6,0		;$1f0-$1fa
		dc.b 0			;$1fc
		dc.b 0			;$1fe

;-------------- MOTOR -------------------------------------

cmd_motor	bsr	motor_on
		bra.w	end_command

;----------------------------------------------------------

cmd_intel	lea.l	outside_txt(pc),a0
		bsr	print

		lea.l	cheat_cnt,a0
		tst.b	(a0)
		beq.b	.zero
		sf	(a0)
		bra.b	.out
.zero		addq.b	#1,(a0)
.out		bra.w	end_command

outside_txt	dc.b "OUTSIDE !",$a,0
		even

cmd_hornet	lea.l	hornet_txt(pc),a0
		bsr	print

		lea.l	cheat_cnt,a0
		cmp.b	#1,(a0)
		beq.b	.zero
		sf	(a0)
		bra.b	.out
.zero		addq.b	#1,(a0)
.out		bra.w	end_command


hornet_txt	dc.b "Illegal but nice command.",$a,0
		even

;-------------- TSD deep trainer start --------------------

cmd_tsd		st	ts_deep
		bra.b	cmd_ts_go

;-------------- TS trainer start --------------------------

cmd_ts		sf	ts_deep

cmd_ts_go	bsr	evaluate		;get start address
		bne.w	illegal_addr
		move.l	d0,ts_start
		bsr	evaluate		;get end address
		bne.w	illegal_addr
		move.l	d0,ts_end
		sub.l	ts_start,d0
		ble.w	illegal_addr

		movem.l	ts_start,d0-d1
		btst	#0,d0			;even address ?
		bne.w	illegal_addr
		btst	#0,d1			;even address ?
		bne.w	illegal_addr

		clr.w	eval_size		;.b par default
		bsr	evaluate		;get nb of lives in d0
		bne.w	illegal_val
 		move.w	eval_size,ts_size	;.b .w .l (0,1,2)

		lea.l	tmp_mem,a0
		bsr	live_scan

		bra.w	end_command

;--------------------------------------
;-> d0=nb of lives
;-> a0=start of trainer buffer in 'tmp_mem'

live_scan	move.l	ts_end,d1
		cmp.l	ts_start,d1
		beq.w	.noscan

		move.l	d0,d3
		tst.b	ts_deep
		beq.b	.nodeep
		addq.l	#1,d0			;d0=max val
		subq.l	#1,d3			;d3=min val
.nodeep

		move.w	ts_size,d1		;.b .w .l (0,1,2)
		beq.w	.go_b
		cmp.w	#1,d1
		bne.w	.go_l
		lea.l	tmp_mem+tmp_mem_size,a3
		subq.l	#4,a3			;for end signal
		move.l	ts_start,a1
		move.l	ts_end,a2
.seek1		move.l	a1,a4
		move.l	a1,watch
		RELOC_PIC 1
		cmp.w	(a4),d3
		bgt.b	.no_eg1
		cmp.w	(a4),d0
		blt.b	.no_eg1
		move.l	a1,(a0)+
		cmp.l	a3,a0
		bge.w	.end_seek
.no_eg1		addq.l	#2,a1
		tst.b	break
		bne.w	.end_seek
		cmp.l	a2,a1
		blt.w	.seek1
		bra.w	.end_seek

.go_l		lea.l	tmp_mem+tmp_mem_size,a3
		subq.l	#4,a3			;for end signal
		move.l	ts_start,a1
		move.l	ts_end,a2
.seek2		move.l	a1,a4
		move.l	a1,watch
		RELOC_PIC 2
		cmp.l	(a4),d3
		bgt.b	.no_eg2
		cmp.l	(a4),d0
		blt.b	.no_eg2
		move.l	a1,(a0)+
		cmp.l	a3,a0
		bge.w	.end_seek
.no_eg2		addq.l	#2,a1
		tst.b	break
		bne.w	.end_seek
		cmp.l	a2,a1
		blt.w	.seek2
		bra.w	.end_seek

.go_b		lea.l	tmp_mem+tmp_mem_size,a3
		subq.l	#4,a3			;for end signal
		move.l	ts_start,a1
		move.l	ts_end,a2
.seek3		move.l	a1,a4
		move.l	a1,watch
		RELOC_PIC 3
		cmp.b	(a4),d3
		bgt.b	.no_eg3
		cmp.b	(a4),d0
		blt.b	.no_eg3
		move.l	a1,(a0)+
		cmp.l	a3,a0
		bge.b	.end_seek
.no_eg3		addq.l	#1,a1
		tst.b	break
		bne.b	.end_seek
		cmp.l	a2,a1
		blt.w	.seek3

.end_seek	move.l	#-1,(a0)		;end signal

		moveq	#8,d1

		lea.l	ts_txt,a0
		bsr	print
		move.l	ts_start,d0
		bsr	print_hex
		lea.l	ts1_txt,a0
		bsr	print
		move.l	a1,d0
		move.l	a1,ts_start
		bsr	print_hexCR

.noscan		rts

ts_txt		dc.b "Scan range : $",0
ts1_txt		dc.b " - $",0
		even

;-------------- TF trainer find ---------------------------

cmd_tf
		bsr	evaluate		;read new nb of lives
		bne.w	illegal_val
		move.l	d0,d7			;new nb of lives=d7

		move.l	d7,d3
		move.l	d7,d4
		tst.b	ts_deep
		beq.b	.nodeep
		addq.l	#1,d3			;d3=max val
		subq.l	#1,d4			;d4=min val

.nodeep		lea.l	tmp_mem,a1
		move.l	a1,a2
		moveq	#-1,d2

		move.w	ts_size,d1
		bpl.b	.oksize
		lea.l	firstts_txt(pc),a0
		bsr	print
		bra.w	end_command
.oksize		beq.w	.go_b

		cmp.w	#1,d1
		bne.w	.go_l

		moveq	#8,d1
.seek1		tst.b	break
		bne.w	.end_seek
		cmp.l	(a1),d2
		beq.w	.end_seek
		move.l	(a1)+,a4
		RELOC_PIC 1
		cmp.w	(a4),d4
		bgt.w	.seek1
		cmp.w	(a4),d3
		blt.w	.seek1
		move.l	-4(a1),d0
		move.l	d0,(a2)+
		lea.l	tf_txt+1,a0
		bsr	conv_hex
		lea.l	tf_txt,a0
		bsr	print
		bra.w	.seek1

.go_l		moveq	#8,d1
.seek2		tst.b	break
		bne.w	.end_seek
		cmp.l	(a1),d2
		beq.w	.end_seek
		move.l	(a1)+,a4
		RELOC_PIC 2
		cmp.l	(a4),d4
		bgt.w	.seek2
		cmp.l	(a4),d3
		blt	.seek2
		move.l	-4(a1),d0
		move.l	d0,(a2)+
		lea.l	tf_txt+1,a0
		bsr	conv_hex
		lea.l	tf_txt,a0
		bsr	print
		bra.w	.seek2

.go_b		moveq	#8,d1
.seek3		tst.b	break
		bne.w	.end_seek
		cmp.l	(a1),d2
		beq.w	.end_seek
		move.l	(a1)+,a4
		RELOC_PIC 3
		cmp.b	(a4),d4
		bgt.w	.seek3
		cmp.b	(a4),d3
		blt.w	.seek3
		move.l	-4(a1),d0
		move.l	d0,(a2)+
		lea.l	tf_txt+1,a0
		bsr	conv_hex
		lea.l	tf_txt,a0
		bsr	print
		bra.w	.seek3

.end_seek	move.l	d2,(a2)		;set new end signal

		cmp.l	#tmp_mem+tmp_mem_size-4,a2
		bge.b	.noscan

		move.l	a2,a0		;tmp_mem buffer
		move.l	d7,d0		;nb of lives
		bsr	live_scan

.noscan		tst.w	cursor_x
		beq.b	.no_CR
		lea.l	cr_txt,a0
		bsr	print
.no_CR		bra.w	end_command

firstts_txt	dc.b "First use TS command !",$a,0
		even

;-------------- debug -------------------------------------

cmd_debug	bsr	remove_pic

		not.b	debug
		beq.w	.clear
		move.l	vbr_reg,a0
		move.l	$8(a0),exc8		;BUS
		move.l	$c(a0),excC		;Address
		move.l	$10(a0),exc10		;Illegal
		move.l	$14(a0),exc14		;Zero
		move.l	$28(a0),exc28		;LineA
		move.l	$2C(a0),exc2C		;LineF

		lea.l	debug_bus(pc),a1
		move.l	a1,$8(a0)
		lea.l	debug_addr(pc),a1
		move.l	a1,$C(a0)
		lea.l	debug_illegal(pc),a1
		move.l	a1,$10(a0)
		lea.l	debug_zero(pc),a1
		move.l	a1,$14(a0)
		lea.l	debug_linea(pc),a1
		move.l	a1,$28(a0)
		lea.l	debug_linef(pc),a1
		move.l	a1,$2c(a0)

		lea.l	debug1_txt,a0
		bsr	print
		bra.w	.okset

.clear		move.l	vbr_reg,a0
		lea.l	debug_bus(pc),a1
		cmp.l	$8(a0),a1
		bne.b	.no8
		move.l	exc8,$8(a0)
.no8		lea.l	debug_addr(pc),a1
		cmp.l	$C(a0),a1
		bne.b	.noC
		move.l	excC,$C(a0)
.noC		lea.l	debug_illegal(pc),a1
		cmp.l	$10(a0),a1
		bne.b	.no10
		move.l	exc10,$10(a0)
.no10		lea.l	debug_zero(pc),a1
		cmp.l	$14(a0),a1
		bne.b	.no14
		move.l	exc14,$14(a0)
.no14		lea.l	debug_linea(pc),a1
		cmp.l	$28(a0),a1
		bne.b	.no28
		move.l	exc28,$28(a0)
.no28		lea.l	debug_linef(pc),a1
		cmp.l	$2C(a0),a1
		bne.b	.no2C
		move.l	exc2C,$2C(a0)
.no2C
		lea.l	debug2_txt,a0
		bsr	print

.okset		bsr	set_pic
		bra.w	end_command

debug1_txt	dc.b "Debug mode on.",$a,0
debug2_txt	dc.b "Debug mode off.",$a,0
		even

debug_bus	move.b	#1,debug_entry
		jmp	monitor
debug_addr	move.b	#2,debug_entry
		jmp	monitor
debug_illegal	move.b	#3,debug_entry
		jmp	monitor
debug_zero	move.b	#4,debug_entry
		jmp	monitor
debug_linea	move.b	#5,debug_entry
		jmp	monitor
debug_linef	move.b	#6,debug_entry
		jmp	monitor

;debug_entry 1=bus,2=addr,3=illegal,4=zero,5=linea,6=linef

;-------------- print if entered from debug mode ----------

check_debug	movem.l	d0/a0,-(a7)
		moveq	#0,d0
		move.b	debug_entry,d0
		beq.b	.nodeb
		lsl.w	#2,d0
		move.l	debug_txt(pc,d0.w),a0
		bsr	print
		sf	debug_entry
.nodeb		movem.l	(a7)+,d0/a0
		rts

debug_txt	dc.l 0
		dc.l .bus_txt
		dc.l .addr_txt
		dc.l .illegal_txt
		dc.l .zero_txt
		dc.l .linea_txt
		dc.l .linef_txt

.bus_txt	dc.b "Debug mode: Bus error",$a,0
.addr_txt	dc.b "Debug mode: Illegal address",$a,0
.illegal_txt	dc.b "Debug mode: Illegal instruction",$a,0
.zero_txt	dc.b "Debug mode: Division by zero",$a,0
.linea_txt	dc.b "Debug mode: Line-A instruction",$a,0
.linef_txt	dc.b "Debug mode: Line-F instruction",$a,0
		even

;----------------------------------------------------------

;NODISK_ERR		equ 1
;TRACKCORRUPT_ERR	equ 2
;BADCHECKSUM_ERR	equ 3
;NOTDOS_ERR		equ 4
;WRITEPROTECT_ERR	equ 5
;CREATEFILE_ERR		equ 6
;FILENOTFOUND_ERR	equ 7
;DISKFULL_ERR		equ 8
;FILEEXIST_ERR		equ 9
;DEVICENOTFOUND_ERR	equ 10
;ILLEGALPATH_ERR	equ 11
;NOFFS_ERR		equ 12
;IDE_ERR		equ 13
;NOTEMPTY_ERR		equ 14

;check if disk cleanup needed

end_disk:	movem.l	d0-a6,-(a7)
		tst.b	disk_op
		beq.w	.no_disk_op
		sf	disk_op

		bsr	flush_fbuffer		;flush file buffers
		tst.b	floppy_op
		beq.b	.noflo
		sf	floppy_op
		bsr	motor_off
.noflo		clr.l	bitmap_part		;clear bitmap buffer

		clr.l	file_handle1		;free
		clr.l	file_handle2		;both file_handle

		lea.l	track_sector,a0
		moveq	#-1,d1
		move.l	d1,(a0)+
		move.l	d1,(a0)+
		move.l	d1,(a0)+
		move.w	d1,track_buffer_no

		move.w	drive_err,d0
		beq.w	.noerr
		clr.w	drive_err

		lea.l	no_disk_txt,a0		;1
		subq.w	#1,d0
		beq.w	.ok
		lea.l	corrupt_txt,a0		;2
		subq.w	#1,d0
		beq.w	.ok
		lea.l	bad_sum_txt,a0		;3
		subq.w	#1,d0
		beq.b	.ok
		lea.l	NotDOS_txt,a0		;4
		subq.w	#1,d0
		beq.b	.ok
		lea.l	WriteProt_txt,a0	;5
		subq.w	#1,d0
		beq.b	.ok
		lea.l	CreateFile_txt,a0	;6
		subq.w	#1,d0
		beq.b	.ok
		lea.l	FileNotFound_txt,a0	;7
		subq.w	#1,d0
		beq.b	.ok
		lea.l	DiskFull_txt,a0		;8
		subq.w	#1,d0
		beq.b	.ok
		lea.l	FileExists_txt,a0	;9
		subq.w	#1,d0
		beq.b	.ok
		lea.l	DeviceNotFound_txt,a0	;10
		subq.w	#1,d0
		beq.b	.ok
		lea.l	IllegalPath_txt,a0	;11
		subq.w	#1,d0
		beq.b	.ok
		lea.l	NoFFS_txt,a0		;12
		subq.w	#1,d0
		beq.b	.ok
		lea.l	IDEerr_txt,a0		;13
		subq.w	#1,d0
		beq.b	.ok
		lea.l	NotEmpty_txt,a0		;14


;		subq.w	#1,d0
;		beq.b	.ok
;		nop
.ok
		bsr	print

.noerr

.no_disk_op	movem.l	(a7)+,d0-a6
		rts

;-------------- type mem ------------------------

cmd_type	bsr evaluate
		bne.w	illegal_addr
		move.l	d0,a0
		bsr	print
		bra.w	end_command

;-------------- find ----------------------------

cmd_f		bsr	evaluate
		bne.w	illegal_addr
		move.l	d0,find_start
		bsr	evaluate
		bne.w	illegal_addr
		move.l	d0,find_end
		movem.l	find_start,d0-d1
		cmp.l	d0,d1
		ble.w	illegal_addr

		lea.l	find_list,a1
		moveq	#0,d1			;nb bytes
.loop		clr.w	eval_size
		bsr	evaluate
		bmi.w	illegal_val
		bgt.b	.end_find
		move.w	eval_size,d2
		move.w	d2,d3
.next		addq.w	#1,d1
		move.b	d0,(a1,d2.w)
		lsr.l	#8,d0
		dbf	d2,.next
		addq.w	#1,d3
		add.w	d3,a1
		cmp.l	#find_list+80,a1
		blt.b	.loop

.end_find	subq.w	#1,d1
		bmi.w	illegal_val
		movem.l	find_start,a0-a1
		cmp.l	a0,a1
		ble.w	illegal_addr


		lea.l	find_list,a2
		move.b	(a2)+,d0
.seek		cmp.l	a1,a0
		bge.w	.end_seek
		tst.b	break
		bne.w	.end_seek
		move.l	a0,watch
		move.l	a0,a4
		RELOC_PIC
		addq.l	#1,a0
		cmp.b	(a4)+,d0
		bne.w	.seek
		move.w	d1,d2
		move.l	a2,a3
		bra.b	.dbf
.comp		move.b	(a3)+,d3
		cmp.b	(a4)+,d3
		bne.w	.seek
.dbf		dbf	d2,.comp
		movem.l	d0-d1/a0,-(a7)
		move.l	a0,d0
		subq.l	#1,d0
		lea.l	ev_line,a0
		move.w	#$2020,8(a0)
		clr.b	10(a0)
		moveq	#8,d1
		bsr	conv_hex
		bsr	print
		movem.l	(a7)+,d0-d1/a0
		bra.w	.seek

.end_seek	tst.w	cursor_x
		beq.b	.no_CR
		lea.l	cr_txt,a0
		bsr	print
.no_CR
		bra.w	end_command

cr_txt		dc.b $a,0

;-------------- find instruction ----------------

cmd_fi		bsr	evaluate
		bne.w	illegal_addr
		and.w	#$fffe,d0
		move.l	d0,find_start
		bsr	evaluate
		bne.w	illegal_addr
		and.w	#$fffe,d0
		move.l	d0,find_end

		movem.l	find_start,d0-d1
		cmp.l	d0,d1
		ble.w	illegal_addr

		lea.l	find_list,a1
		bsr	read_name
		tst.b	(a1)
		beq.w	illegal_string

.re_jok		lea.l	find_list,a0
		cmp.b	#'*',(a0)	;remove jokers at begining of line
		bne.b	.lower
.shift		move.b	1(a0),(a0)+
		bne.b	.shift
		bra.b	.re_jok

.lower		move.b	(a0),d0
		beq.b	.end_list
		bsr	lower_case
		move.b	d0,(a0)+
		bra.b	.lower

.end_list	movem.l	find_start,a0-a1
		cmp.l	a0,a1
		ble.w	illegal_addr

		move.l	a1,d7
		lea.l	ev_line,a1
		move.l	a1,d6
		lea.l	find_list,a1
		move.l	a1,d5
		moveq	#$a,d4

.loop2		cmp.l	d7,a0
		bge.w	.end_fi
		tst.b	break
		bne.w	.end_fi

		move.l	a0,watch
		move.l	a0,-(a7)
		move.l	a0,a4
		bsr	reloc_pic
		move.l	d6,a0			;ev_line
		moveq	#%100,d0	;lower case, no d $address,indirect
		bsr	disassemble

		move.l	d5,a0			;find_list
		move.b	(a0)+,d0
		move.l	d6,a1			;ev_line
.seek		move.b	(a1)+,d1
		cmp.b	d4,d1
		beq.w	.no_fi
		cmp.b	d0,d1
		bne.b	.seek
		move.l	a0,a2
		move.l	a1,a3
.loop		move.b	(a2)+,d2
		beq.b	.ok
		move.b	(a3)+,d1
		cmp.b	d4,d1
		beq.b	.seek
		cmp.b	#'*',d2			;joker
		beq.b	.loop
		cmp.b	d2,d1
		beq.b	.loop
		bra.b	.seek

.ok
		move.l	(a7),a4
		RELOC_PIC
		move.l	d6,a0			;ev_line
		moveq	#%111,d0	;upper case, d $address, indirect
		bsr	disassemble

		move.l	d6,a0
		bsr	print			;print disas. line

.no_fi		move.l	(a7)+,a0
		addq.l	#2,a0
		bra.w	.loop2

.end_fi
		bra.w	end_command

;-------------- find instruction famicom -------------

cmd_fif		bsr	evaluate
		bne.w	illegal_addr
		move.l	d0,find_start
		bsr	evaluate
		bne.w	illegal_addr
		move.l	d0,find_end

		movem.l	find_start,d0-d1

		lea.l	find_list,a1
		bsr	read_name
		tst.b	(a1)
		beq.w	illegal_string

		lea.l	find_list,a0
.upper		move.b	(a0),d0
		beq.b	.end_list
		bsr	upper_case
		move.b	d0,(a0)+
		bra.b	.upper

.end_list	movem.l	find_start,a0-a1
		cmp.l	a0,a1
		ble.w	illegal_addr

		move.l	a1,d7
		lea.l	ev_line+10,a1
		move.l	a1,d6
		lea.l	find_list,a1
		move.l	a1,d5
		moveq	#$a,d4

.loop2		cmp.l	d7,a0
		bge.w	.end_fi
		tst.b	break
		bne.w	.end_fi

		move.l	a0,watch
		move.l	a0,-(a7)
		move.l	a0,a4
		RELOC_PIC
		move.l	d6,a0			;ev_line+10
		bsr	f_disassemble

		move.l	d5,a0			;find_list
		move.b	(a0)+,d0
		move.l	d6,a1			;ev_line+10
.seek		move.b	(a1)+,d1
		cmp.b	d4,d1
		beq.b	.no_fi
		cmp.b	d0,d1
		bne.b	.seek
		move.l	a0,a2
		move.l	a1,a3
.loop		move.b	(a2)+,d2
		beq.b	.ok
		move.b	(a3)+,d1
		cmp.b	d4,d1
		beq.b	.seek
		cmp.b	d2,d1
		beq.b	.loop
		bra.b	.seek

.ok		move.l	(a7),d0
		moveq	#8,d1
		lea.l	ev_line,a0
		move.b	#' ',9(a0)
		move.b	#' ',(a0)+
		bsr	conv_hex
		lea.l	ev_line,a0
		bsr	print
.no_fi		move.l	(a7)+,a0
		addq.l	#1,a0
		bra.w	.loop2

.end_fi
		bra.w	end_command

;-------------- change drive no -----------------
cmd_drive	bsr	evaluate
		bmi.w	illegal_val
		beq.b	.chg
		moveq	#0,d0
		move.w	drive,d0
		subq.w	#3,d0
.chg		moveq	#1,d1
		cmp.l	d1,d0
		bgt.w	illegal_val
		tst.l	d0
		bmi.w	illegal_val
		lea.l	drive_present,a0
		tst.b	(a0,d0.w)		;drive is present ?
		beq.w	illegal_val
		addq.w	#3,d0
		move.w	d0,drive
		subq.b	#3,d0
		lea.l	drive_txt,a0
		bsr	print
		moveq	#1,d1
		bsr	print_hexCR
		bra.w	end_command

;-------------- exit monitor --------------------

cmd_x		st	escape
		bra.w	end_command


;-------------- KILL --------------------------------------

cmd_kill	st	escape
		st	kill

		bra.w	end_command

;-------------- REBOOT ------------------------------------

cmd_reboot
		bsr	remove_pic

		move.l	$4.w,a0
		move.l	a0,d0
		lsr.l	#1,d0
		bcs.w	.nosafe
		lea.l	34(a0),a1
		moveq	#0,d0
		moveq	#$18-1,d1
.add		add.w	(a1)+,d0
		dbf	d1,.add
		not.w	d0
		cmp.w	82(a0),d0
		bne.w	.nosafe

		bsr	set_pic

		lea.l	surereboot_txt(pc),a0
		bsr	print
		bsr	get_key
		lea.l	sure2_txt(pc),a0
		bsr	print
		cmp.b	#'y',d0
		bne.b	.no

		bsr	remove_pic

		move.l	$4.w,a0
		clr.l	ColdCapture(a0)
		move.l	#cool-start+$48000,CoolCapture(a0)

		st	reboot

		lea.l	34(a0),a1
		moveq	#0,d0
		moveq	#$18-1,d1
.add2		add.w	(a1)+,d0
		dbf	d1,.add2
		not.w	d0
		move.w	d0,82(a0)

		move.l	$f80004,a0
		subq.l	#2,a0
		move.l	a0,pc_reg
		move.w	#$2700,sr_reg
		st	escape

		bsr	set_pic

.no		bra.w	end_command

.nosafe		lea.l	nosafe_txt(pc),a0
		bsr	print
		bra.w	end_command

nosafe_txt	dc.b "HRTmon can't reboot and stay resident !",$a
		dc.b "You have to perform this operation when ExecBase is still valid !",$a,0

surereboot_txt	dc.b "The machine will reboot!",$a
		dc.b "HRTmon will still be accessible to debug bootdisks",$a
		dc.b "Are you sure you want to continue ? (y/n)",$d,0
		even


cool		movem.l	d1-a6,-(a7)

		move.l	$4.w,a0
		clr.l	CoolCapture(a0)
		lea.l	34(a0),a1
		moveq	#0,d0
		moveq	#$18-1,d1
.add		add.w	(a1)+,d0
		dbf	d1,.add
		not.w	d0
		move.w	d0,82(a0)

		lea.l	$48000,a0
		lea.l	start,a1
		move.l	a1,a2
		move.l	#(end-start),d0
		lsr.l	#3,d0
.copy		move.l	(a0)+,(a1)+		;restore HRTmon to its place
		move.l	(a0)+,(a1)+
		dbf	d0,.copy

		move.l	$4.w,a6
		jsr	-636(a6)		;clear cache

		jmp	.next

.next		move.l	$4.w,a6
		lea.l	.super(pc),a5
		jsr	-30(a6)

		move.l	mon_size(pc),d0
		beq.b	.noalloc
		lea.l	start,a1
		jsr	-204(a6)
		tst.l	d0
		bne.b	.noalloc
		clr.l	mon_size		;alloc failed
.noalloc
		sf	entered


;.wait		move.w	$dff006,d0
;		and.w	#$ff,d0
;		move.w	#0,$dff106
;		move.w	d0,$dff180
;		btst	#6,$bfe001
;		bne.b	.wait

		movem.l	(a7)+,d1-a6
		moveq	#0,d0
		rts

	MC68030
.super		movec	VBR,a4
		move.l	a4,old_vbr
		lea.l	new_except,a0

		move.l	a0,a1
		lea.l	except_entry(pc),a2
		move.w	#$400/4-1,d1
.init		move.l	a2,(a1)+
		dbf	d1,.init

		suba.l	a1,a1
		move.w	#$400/4-1,d1
.copy2		move.l	(a4)+,(a1)+
		dbf	d1,.copy2

		move.l	$7c(a0),oldlev7
		move.l	#monitor,$7c(a0)
		movec	a0,VBR

		rte
	MC68000

;-------------- restart program at address ------

cmd_g		bsr	evaluate
		bmi.w	illegal_addr
		bne.b	.noaddr
		move.l	d0,pc_reg
.noaddr		st	escape
		bra.w	end_command

;-------------- Load AmigaDOS file --------------

cmd_l		lea.l	ev_line,a1
		bsr	read_name
		tst.b	(a1)
		beq.w	illegal_name

		bsr	evaluate
		bne.w	illegal_addr
		tst.l	d0
		bmi.w	illegal_addr
		move.l	d0,d4

		bsr	remove_pic

		move.l	#ev_line,d1
		moveq	#0,d2			;old file
		bsr	open_file
		move.l	d0,d7
		beq.b	.noopen
		move.l	d4,d2
		move.l	d7,a1
		move.l	file_size(a1),d3
		move.l	d3,d6
		move.l	d3,d1
		add.l	d2,d1			;end address
		move.l	d2,d0
		move.l	d7,d1
		bsr	read_file
.bad		move.l	d7,d1
		bsr	close_file

.noopen		bsr	set_pic

		tst.w	drive_err
		bne.b	.end

		lea.l	file_len_txt,a0
		bsr	print
		moveq	#8,d1
		move.l	d6,d0
		bsr	print_hexCR

.end		bra.w	end_command

;-------------- cd dir --------------------------

cmd_cd		lea.l	ev_line,a1
		bsr	read_name
		tst.b	(a1)
		beq.b	.nochg

		move.l	#ev_line,d1
		bsr	change_dir

.nochg		lea.l	ev_line,a0
		sf	(a0)
		move.l	a0,d1
		bsr	ExNext
		tst.l	d0
		bmi.b	.err

		move.l	d1,a3			;partition
		move.l	d2,d0			;parent block no
		bsr	print_path		;path

.err		bra.w	end_command

;-------------- dir -----------------------------

cmd_dir		lea.l	ev_line,a1
		bsr	read_name

		moveq	#0,d6			;nb files printed

		move.l	#ev_line,d1
		bsr	ExNext
		tst.l	d0
		bmi.w	.err

		move.l	d0,-(a7)
		move.l	d1,a3			;partition
		move.l	d2,d0			;parent block no
		bsr	print_path		;path
		move.l	(a7)+,d0

		tst.l	d0			;empty dir ?
		beq.w	.err

.loop		addq.l	#1,d6
		move.l	d0,a1
		lea.l	general_txt,a0
		move.b	#' ',d0
		move.l	a0,a2
		moveq	#31-1,d2
.fill		move.b	d0,(a2)+
		dbf	d2,.fill
		sf	(a2)

		lea.l	file_name(a1),a2
		moveq	#0,d0
		move.b	(a2)+,d0		;get file name size
		bra.b	.godbf
.copyn		move.b	(a2)+,(a0)+
.godbf		dbf	d0,.copyn
		lea.l	general_txt,a0
		bsr	print			;print filename

		tst.b	file_dir(a1)
		beq.b	.okfile
		lea.l	dir_txt,a0
		bsr	print
		bra.b	.okdir
.okfile		move.l	file_size(a1),d0
		moveq	#7,d1
		bsr	print_dec

.okdir		lea.l	spc_txt,a0
		bsr	print

		move.l	d6,d0		;nb files printed in d0
		move.w	screen_height,d1
		subq.w	#5,d1
		lsl.w	#1,d1
		divu	d1,d0		;pause after 46(PAL) files printed
		swap	d0
		tst.w	d0
		bne.b	.no46
		bsr	get_key		;wait for key pressed
.no46
		tst.b	break
		bne.b	.err

		move.l	a1,d1
		bsr	ExNext
		tst.l	d0
		bgt.w	.loop

.err		lsr.l	#1,d6
		bcc.b	.nocr
		lea.l	cr_txt,a0
		bsr	print

.nocr
		bra.w	end_command

dir_txt		dc.b "   (DIR)",0
spc_txt		dc.b " ",0
		even

;-------------- calculate BootBlock checksum --------------

cmd_bb		bsr	evaluate
		bne.w	illegal_addr
		move.l	d0,a0
		move.l	a0,a1
		clr.l	4(a1)

		moveq	#0,d0
		move.w	#512*2/4-1,d1
.add		add.l	(a0)+,d0
		bcc.b	.noc
		addq.l	#1,d0
.noc		dbf	d1,.add
		not.l	d0

		move.l	d0,4(a1)

		bra.w	end_command

;-------------- floppy access -------------------

cmd_disk1	move.b	#'R',d0
		bra.b	go_disk
cmd_disk2	move.b	#'W',d0
go_disk		move.b	d0,rwsec

		bsr	evaluate
		bne.w	illegal_addr
		move.l	d0,sec_addr
		bsr	evaluate
		bne.w	illegal_val
		tst.l	d0
		bmi.w	illegal_val
		cmp.l	#1759,d0
		bgt.w	illegal_val
		move.w	d0,strtsec
		move.l	d0,d1

		bsr	evaluate
		bne.w	illegal_val
		tst.l	d0
		bmi.w	illegal_val
		move.w	d0,nbsec
		add.l	d0,d1
		cmp.l	#1760,d1
		bgt.w	illegal_val

		move.l	sec_addr,d0
		move.w	nbsec,d1
		mulu	#512,d1
		add.l	d0,d1

		bsr	remove_pic

		bsr	test_present
		bne.b	.end_write

		move.l	sec_addr,a0
		movem.w	strtsec,d0-d1
		cmp.b	#'W',rwsec
		bne.b	.no_write

		bsr	write2

		bsr	force_change

		bra.b	.end_write

.no_write	bsr	read2
.end_write	bsr	set_pic
		bra.w	end_command

;----------------------------------------------------------
;-------------- test if a disk is present -----------------
;-------------- if disk changed then flush track_buffer ---
;-------------- used by non FileSystem commands (RS,WS...)-

;<- FLAGS 0=ok_present

test_present	movem.l	d0/a3,-(a7)
		lea.l	floppy0,a3
		cmp.w	#3,drive
		beq.b	.go0
		lea.l	floppy1,a3
.go0		moveq	#0,d0			;test cmd
		bsr	floppy_change
		tst.w	drive_err
		movem.l	(a7)+,d0/a3
		rts

;--------------------------------------------------------------------
;-------------- force change_disk to floppy selected in 'drive' -----

;-> drive

force_change	movem.l	d0/a2-a3,-(a7)
		lea.l	floppy0,a3
		cmp.w	#3,drive
		beq.b	.f0
		cmp.w	#4,drive
		bne.b	.end
		lea.l	floppy1,a3
.f0		move.l	part_device(a3),a2
		clr.l	part_filesystem(a3)
		moveq	#1,d0			;force change cmd
		jsr	CHANGE_CMD(a2)
		clr.l	bitmap_part		;clear bitmap buffer
.end		movem.l	(a7)+,d0/a2-a3
		rts

;-------------- disk check ----------------------

cmd_diskchk

		moveq	#0,d6
		move.w	#160-1,d7
.check		move.l	d6,d0
		lea.l	fbuf1,a0
		bsr	read_fsector
		tst.w	drive_err
		bne.b	.error
		tst.b	break
		bne.b	.error
		add.w	#11,d6
		dbf	d7,.check

		lea.l	.ok_txt(pc),a0
		bsr	print

.error
		bsr	force_change

		bra.w	end_command

.ok_txt		dc.b "Floppy disk is OK.",$a,0

;-------------- format disk ---------------------

fconfirm1_txt	dc.b "Format disk in drive ",0
fconfirm2_txt	dc.b " (y/n) ?",$d,0
		even

cmd_format	lea.l	ev_line,a1
		bsr.w	read_name

		lea.l	fconfirm1_txt,a0
		bsr	print
		move.w	drive,d0
		subq.w	#3,d0
		moveq	#1,d1
		bsr	print_dec
		lea.l	fconfirm2_txt,a0
		bsr	print

		bsr	get_key
		lea.l	diskclr_txt(pc),a0
		bsr	print
		cmp.b	#'y',d0
		bne.w	.endformat

		bsr	test_present
		bne.w	.endformat

		lea.l	$40000,a0		;dummy addr for format
		moveq	#0,d0
		moveq	#-1,d1			;format all disk
		moveq	#-1,d7			;write
		bsr	access

		tst.b	break
		bne.b	.break

		lea.l	format_block,a0
		lea.l	secbuf,a1
		moveq	#512/16-1,d0
.copy		move.l	(a0)+,(a1)+
		move.l	(a0)+,(a1)+
		move.l	(a0)+,(a1)+
		move.l	(a0)+,(a1)+
		dbf	d0,.copy

		lea.l	ev_line,a1
		tst.b	(a1)
		beq.w	.no_set
		move.l	a1,d1			;source name (ended by a zero)
		lea.l	secbuf+108*4,a2		;dest BCPL
		bsr	copy_name2
.no_set
		lea.l	secbuf,a0
		bsr	calc_sum
		move.w	#880,d0
		bsr	write_fsector
		lea.l	format_block+512,a0
		move.w	#881,d0
		bsr	write_fsector
		bsr	write_flush

.break		bsr	force_change

.endformat

		bra.w	end_command

;-------------- FORMATQ ------------------------
cmd_formatq	lea.l	ev_line,a1
		bsr.w	read_name

		lea.l	fconfirm1_txt,a0
		bsr	print
		move.w	drive,d0
		subq.w	#3,d0
		moveq	#1,d1
		bsr	print_dec
		lea.l	fconfirm2_txt,a0
		bsr	print

		bsr	get_key
		lea.l	diskclr_txt(pc),a0
		bsr	print
		cmp.b	#'y',d0
		bne.w	.endformat

		bsr	test_present
		bne.w	.endformat

		lea.l	format_block,a0
		lea.l	secbuf,a1
		moveq	#512/16-1,d0
.copy		move.l	(a0)+,(a1)+
		move.l	(a0)+,(a1)+
		move.l	(a0)+,(a1)+
		move.l	(a0)+,(a1)+
		dbf	d0,.copy

		lea.l	ev_line,a1
		tst.b	(a1)
		beq.b	.no_set
		move.l	a1,d1			;source name (ended by a zero)
		lea.l	secbuf+108*4,a2		;dest BCPL
		bsr	copy_name2
.no_set
		lea.l	secbuf,a0
		bsr	calc_sum
		move.w	#880,d0
		bsr	write_fsector
		lea.l	format_block+512,a0
		move.w	#881,d0
		bsr	write_fsector

		lea.l	secbuf,a0
		move.l	a0,a1
		moveq	#512/4-1,d0
.clr		clr.l	(a1)+
		dbf	d0,.clr
		move.l	#$444f5301,(a0)		;'DOS',1 (FFS)
		moveq	#0,d0
		bsr	write_fsector

		bsr	write_flush

		bsr	force_change

.endformat
		bra.w	end_command

;-------------- evaluate expression -------------
cmd_ev		bsr	evaluate
		bne.w	illegal_expr

		lea.l	ev_line,a0
		move.b	#'$',(a0)+
		moveq	#8,d1
		bsr	conv_hex
		addq.l	#8,a0
		moveq	#$20,d6
		move.b	d6,(a0)+

		moveq	#10,d1
		bsr	conv_dec

		move.b	d6,(a0)+
		move.b	#'"',(a0)+
		moveq	#0,d1
		lea.l	ascIIx,a1
		rol.l	#8,d0
		move.b	d0,d1
		move.b	(a1,d1.w),(a0)+
		rol.l	#8,d0
		move.b	d0,d1
		move.b	(a1,d1.w),(a0)+
		rol.l	#8,d0
		move.b	d0,d1
		move.b	(a1,d1.w),(a0)+
		rol.l	#8,d0
		move.b	d0,d1
		move.b	(a1,d1.w),(a0)+
		move.b	#'"',(a0)+

		move.b	d6,(a0)+

		move.b	#'%',(a0)+
		moveq	#4-1,d2
.loop3		moveq	#8-1,d1
.loop2		addx.l	d0,d0
		bcc.b	.zero
		move.b	#'1',(a0)+
		bra.b	.un
.zero		move.b	#'0',(a0)+
.un		dbf	d1,.loop2
		move.b	#'.',(a0)+
		dbf	d2,.loop3
		subq.l	#1,a0
		move.b	#$a,(a0)+
		clr.b	(a0)+

		lea.l	ev_line,a0
		bsr	print

		bra.w	end_command

;-------------- direct keyboard read (without IRQ) -------

;<- dkey

get_dkey	movem.l	d0-d2,-(a7)
		move.b	$bfed01,d0
		btst	#3,d0
		beq.b	no_dkey
		move.b	$bfec01,d0
		not.b	d0
		ror.b	#1,d0
		bset	#6,$bfee01
		cmp.b	#$78,d0
		beq.w	go_reset
		moveq	#8-1,d1
.wait		move.b	6(a6),d2
.ras		cmp.b	6(a6),d2
		beq.b	.ras
		dbf	d1,.wait
		bclr	#6,$bfee01
		move.b	d0,dkey
no_dkey		movem.l	(a7)+,d0-d2
		rts

;-------------- show registres (R) --------------
cmd_r
seek_reg	move.b	(a0)+,d0
		beq.w	end_chg_reg
		cmp.b	#$20,d0
		beq.b	seek_reg
		subq.l	#1,a0

		bsr	read_reg	;read registre (no of reg in d0)
		move.w	d0,d5		;save no registre in d5
		bmi.w	illegal_reg

seek_reg_val	move.b	(a0)+,d0
		beq.w	illegal_val
		cmp.b	#$20,d0
		beq.b	seek_reg_val
		subq.l	#1,a0

		bsr	evaluate
		bne.w	illegal_val

		cmp.w	#16,d5
		blt.b	ok_simple_reg
;-----------------------------------------------
		bne.b	no_pc_chg
		move.l	d0,pc_reg
		bra.b	end_chg_reg
;-----------------------------------------------
no_pc_chg	cmp.w	#19,d5
		bne.b	no_usp_chg
		move.l	d0,usp_reg
;-----------------------------------------------
no_usp_chg	cmp.w	#20,d5
		bne.b	no_ssp_chg
		move.l	d0,ssp_reg
;-----------------------------------------------
no_ssp_chg	cmp.w	#17,d5
		bne.b	no_sr_chg
		move.w	d0,sr_reg
		bra.b	end_chg_reg
;-----------------------------------------------
no_sr_chg	cmp.w	#18,d5
		bne.b	no_vbr_chg
		move.l	d0,vbr_reg
		bra.b	end_chg_reg
;-----------------------------------------------
no_vbr_chg	cmp.w	#21,d5
		bne.b	no_cacr_chg
		move.l	d0,cacr_reg
		bra.b	end_chg_reg
;-----------------------------------------------
no_cacr_chg	cmp.w	#22,d5
		bne.b	no_pcr_chg
		move.l	d0,msp_reg
		bra.b	end_chg_reg
;-----------------------------------------------
no_pcr_chg	cmp.w	#23,d5
		bne.b	no_busr_chg
		move.l	d0,caar_reg
		bra.b	end_chg_reg
;-----------------------------------------------
no_busr_chg
		bra.b	end_chg_reg

ok_simple_reg	lea.l	registres,a0
		lsl.w	#2,d5
		move.l	d0,(a0,d5.w)

end_chg_reg	bsr	print_reg
		bra.w	end_command

;-------------- print all registres -------------

print_reg	movem.l	d0-d1/a0-a1/a4,-(a7)
		moveq	#8,d1
		lea.l	registres,a1
		move.l	(a1)+,d0
		lea.l	data0_reg+3,a0
		bsr	conv_hex
		move.l	(a1)+,d0
		lea.l	data0_reg+3+9,a0
		bsr	conv_hex
		move.l	(a1)+,d0
		lea.l	data0_reg+3+18,a0
		bsr	conv_hex
		move.l	(a1)+,d0
		lea.l	data0_reg+3+27,a0
		bsr	conv_hex
		move.l	(a1)+,d0
		lea.l	data4_reg,a0
		bsr	conv_hex
		move.l	(a1)+,d0
		lea.l	data4_reg+9,a0
		bsr	conv_hex
		move.l	(a1)+,d0
		lea.l	data4_reg+18,a0
		bsr	conv_hex
		move.l	(a1)+,d0
		lea.l	data4_reg+27,a0
		bsr	conv_hex

		move.l	(a1)+,d0
		lea.l	addr0_reg+3,a0
		bsr	conv_hex
		move.l	(a1)+,d0
		lea.l	addr0_reg+3+9,a0
		bsr	conv_hex
		move.l	(a1)+,d0
		lea.l	addr0_reg+3+18,a0
		bsr	conv_hex
		move.l	(a1)+,d0
		lea.l	addr0_reg+3+27,a0
		bsr	conv_hex
		move.l	(a1)+,d0
		lea.l	addr4_reg,a0
		bsr	conv_hex
		move.l	(a1)+,d0
		lea.l	addr4_reg+9,a0
		bsr	conv_hex
		move.l	(a1)+,d0
		lea.l	addr4_reg+18,a0
		bsr	conv_hex
		move.l	(a1)+,d0
		lea.l	addr4_reg+27,a0
		bsr	conv_hex

		move.l	pc_reg,d0
		lea.l	pc_txt+3,a0
		bsr	conv_hex
		move.l	ssp_reg,d0
		lea.l	ssp_txt+4,a0
		bsr	conv_hex
		move.l	usp_reg,d0
		lea.l	usp_txt+4,a0
		bsr	conv_hex
		move.l	vbr_reg,d0
		lea.l	vbr_txt+4,a0
		bsr	conv_hex

		move.l	cacr_reg,d0
		lea.l	CACR_txt+5,a0
		bsr	conv_hex
		move.l	caar_reg,d0
		lea.l	CAAR_txt+5,a0
		bsr	conv_hex
		move.l	msp_reg,d0
		lea.l	MSP_txt+4,a0
		bsr	conv_hex
		move.l	isp_reg,d0
		lea.l	ISP_txt+4,a0
		bsr	conv_hex

		lea.l	sr_txt+3,a0
		moveq	#4,d1
		move.w	sr_reg,d0
		bsr	conv_hex

		move.l	a7_reg,a0
		and.w	#$2000,d0
		beq.b	no_superv_stack
		addq.l	#6,a0
		move.l	a0,a4
		bsr	reloc_pic
		move.w	(a4),d0			;stack frame
		lsr.w	#4,d0
		lsr.w	#8,d0
		add.w	d0,d0
		lea.l	frame,a4
		add.w	(a4,d0.w),a0
no_superv_stack	move.l	pc_reg,a4
		bsr	reloc_pic
		move.b	#'S',rts_txt+2
		cmp.w	#$4e73,(a4)		;cmp if RTE
		bne.b	no_rte
		addq.l	#2,a0			;jmp SR for RTE
		move.b	#'E',rts_txt+2
no_rte		move.l	a0,a4
		bsr	reloc_pic
		move.l	(a4),d0			;read RTS address
		lea.l	rts_txt+4,a0
		moveq	#8,d1
		bsr	conv_hex

		lea.l	registre_txt,a0
		bsr	print

		jsr	init_regBMON

		movem.l	(a7)+,d0-d1/a0-a1/a4
		rts

frame		dc.w 2,2,6,2,2,2,2,2,22,14,24,40,2,2,2 ;all stack frame size


********************************************************************
;-------------- print single instruction at dis_ptr  ---------------

;d0 returns instruction len

single_inst	movem.l	d1-d7/a0-a4,-(a7)

		move.l	dis_ptr,a4
		bsr	reloc_pic

		lea.l	general_txt,a0
		moveq	#7,d0		;upper case, d $address, indir. address
		bsr	disassemble
		lea.l	general_txt,a0
		bsr	print

		movem.l	(a7)+,d1-d7/a0-a4
		rts

***************************************************************
;-------------- hex dump (H) ---------------------

cmd_h		moveq	#0,d1
seek_para_h	move.b	(a0)+,d0
		beq.w	ok_h_cmd		;keep last address
		cmp.b	#$20,d0
		beq.b	seek_para_h
		subq.l	#1,a0

		bsr	evaluate		;returns address in d0
		bne.w	illegal_addr
		move.l	d0,hex_ptr

		move.l	d0,a3

.loop		move.b	(a0)+,d0
		beq.w	ok_h_cmd
		cmp.b	#"'",d0
		beq.b	ok_h_ascII
		cmp.b	#$20,d0
		beq.b	.loop
		subq.l	#1,a0
		st	label
		move.w	#1,eval_size	;set .W par default
		bsr	evaluate
		beq.b	.oky
		sf	label
		bra.w	illegal_val
.oky		sf	label
		move.l	a3,a4
		bsr	reloc_pic
		move.w	eval_size,d2
.next		move.b	d0,(a4,d2.w)
		lsr.l	#8,d0
		addq.l	#1,a3
		dbf	d2,.next
		st	d1
		bra.b	.loop

ok_h_ascII	tst.b	d1
		bne.b	ok_h_cmd
		move.l	hex_ptr,a3
.loop		move.b	(a0)+,d0
		beq.b	ok_h_cmd
		cmp.b	#"'",d0
		beq.b	ok_h_cmd
		move.l	a3,a4
		bsr	reloc_pic
		move.b	d0,(a4)
		st	d1
		addq.l	#1,a3
		bra.b	.loop

ok_h_cmd	tst.b	d1
		bne.w	end_command
		move.l	hex_ptr,a4		;a4=mem ptr
		move.l	a4,d0

		moveq	#8-1,d3			;print 8 lines
		moveq	#15,d5

.loop2		bsr	make_hex_line

		lea.l	16(a4),a4
		lea.l	hex_txt,a0
		bsr	print

		tst.b	break
		dbne	d3,.loop2

		move.l	a4,hex_ptr

		bra.w	end_command

;-------------- up arrow cmd ---------------

;-> d2 = scroll step (1 or SC_STEP)

cmdu_h		move.l	ascII_ptr,a0
		move.w	d2,d0
		mulu	#80,d0
		add.l	d0,a0
		cmp.w	#'h ',(a0)	;last top line was an hex dump ?
		bne.b	.noh
		addq.l	#2,a0
		bsr	evaluate	;get last address
		move.l	d2,d1
		lsl.l	#4,d1		;*16
		sub.l	d1,d0
		move.l	d0,a4

		subq.w	#1,d2
.print		bsr	make_hex_line
		lea.l	16(a4),a4
		lea.l	hex_txt,a0
		bsr	print
		dbf	d2,.print

		clr.w	2(a3)
;		lea.l	hex_txt2,a1
;		move.b	#$d,18(a1)
;		move.b	#$a,18(a1)

		moveq	#0,d0

.noh		rts

;-------------- down arrow cmd -------------

cmdd_h		move.l	ascII_ptr,a0
		move.w	window_bot,d0
		subq.w	#1,d0
		sub.w	d2,d0
		mulu	#80,d0
		add.l	d0,a0
		cmp.w	#'h ',(a0)	;last line was an hex dump ?
		bne.b	.noh
		addq.l	#2,a0
		bsr	evaluate	;get last address
		add.l	#16,d0
		move.l	d0,a4

		sub.w	d2,(2,a3)

		subq.w	#1,d2
.print		bsr	make_hex_line
		lea.l	hex_txt,a0
		bsr	print
		lea.l	16(a4),a4
		dbf	d2,.print

		move.l	a4,hex_ptr

		moveq	#0,d0

.noh		rts


;-> a4=address to dump
;<- hex_txt is filled with the dump text

make_hex_line	movem.l	d0-a4,-(a7)
		moveq	#$0f,d5

		move.l	a4,d0
		bsr	reloc_pic

		lea.l	ascIIx,a1
		lea.l	hex_list,a3

		lea.l	hex_txt+3,a0	;print address
		moveq	#8,d1
		bsr	conv_hex

		lea.l	hex_txt+3+8+2,a0		;long list
		lea.l	hex_txt2+1,a2		;ascII print
		moveq	#2,d1				;2 for conv_hex
		moveq	#0,d0
		moveq	#8-1,d6
.loopb		moveq	#2-1,d2
.loop		move.b	(a4)+,d0
		move.b	(a1,d0.w),(a2)+
		move.w	d0,d4
		and.w	d5,d4
		lsr.w	#4,d0
		move.b	(a3,d0.w),(a0)+
		move.b	(a3,d4.w),(a0)+
		dbf	d2,.loop
		addq.l	#2,a0
		dbf	d6,.loopb

		movem.l	(a7)+,d0-a4
		rts

***************************************************************
;-------------- ascII dump (n) ---------------------
cmd_n
seek_para_n	move.b	(a0)+,d0
		beq.b	ok_n_cmd		;keep last address
		cmp.b	#$20,d0
		beq.b	seek_para_n
		subq.l	#1,a0

		bsr	evaluate		;returns address in d0
		bne.w	illegal_addr
		move.l	d0,asc_ptr

ok_n_cmd	move.l	asc_ptr,a4		;a4=mem ptr
		move.l	a4,d0

		moveq	#8-1,d3			;print 8 lines

.loop2		moveq	#$a,d0
		bsr	make_ascII_line

		lea.l	general_txt,a0
		bsr	print
		lea.l	64(a4),a4

		tst.b	break
		dbne	d3,.loop2

		move.l	a4,asc_ptr

		bra.w	end_command

;-------------- scroll up ascII dump ----------------------

cmdu_n		move.l	ascII_ptr,a0
		move.w	d2,d0
		mulu	#80,d0
		add.l	d0,a0
		cmp.w	#'n ',(a0)	;last top line was an ascII dump ?
		bne.b	.non
		addq.l	#2,a0
		bsr	evaluate	;get last address
		move.l	d2,d1
		lsl.l	#6,d1		;*64
		sub.l	d1,d0
		move.l	d0,a4

		subq.w	#1,d2

.print		moveq	#$a,d0
		bsr	make_ascII_line

		lea.l	general_txt,a0
		bsr	print

		lea.l	64(a4),a4

		dbf	d2,.print

		clr.w	(2,a3)

		moveq	#0,d0

.non		rts

;-------------- scroll down ascII dump --------------------

cmdd_n		move.l	ascII_ptr,a0
		move.w	window_bot,d0
		subq.w	#1,d0
		sub.w	d2,d0
		mulu	#80,d0
		add.l	d0,a0
		cmp.w	#'n ',(a0)	;last line was an ascII dump ?
		bne.b	.non
		addq.l	#2,a0
		bsr	evaluate	;get last address
		add.l	#64,d0
		move.l	d0,a4

		sub.w	d2,(2,a3)

		subq.w	#1,d2

.print		moveq	#$a,d0
		bsr	make_ascII_line

		lea.l	general_txt,a0
		bsr	print

		lea.l	64(a4),a4

		dbf	d2,.print

		move.l	a4,asc_ptr


		moveq	#0,d0

.non		rts


;-> a4=mem ptr
;-> d0=end line ascII ($a or $d)
;<- general_txt filled with ascII line

make_ascII_line	movem.l	d0-a4,-(a7)
		move.b	d0,d7
		lea.l	ascIIx,a1
		move.l	a4,d0
		bsr	reloc_pic

		lea.l	general_txt,a0
		move.b	#'n',(a0)+
		move.b	#' ',(a0)+
		move.b	#'$',(a0)+
		moveq	#8,d1
		bsr	conv_hex

		lea.l	8(a0),a0
		move.b	#' ',(a0)+
		moveq	#0,d0
		moveq	#64-1,d2
.loop		move.b	(a4)+,d0
		move.b	(a1,d0.w),(a0)+
		dbf	d2,.loop

		move.b	d7,(a0)+
		sf	(a0)+

		movem.l	(a7)+,d0-a4
		rts

*********************************************************************
;check address accessed and if needed relocate the HRTmon bitmap mem.
;(bitmap is either at $20000 or at $60000)

;-> a4=address accessed

reloc_pic:
		cmp.l	#$40000,a4
		bcs.b	.low
;-------------- bitmap needs to be in $20000 ---------
		cmp.l	#$20000,pic_ptr
		beq.b	.ok_corr
		tst.b	pic_status
		bne.b	.pic_on1
		move.l	#$20000,pic_ptr
		bra.b	.ok_corr
.pic_on1	bsr	remove_pic
		move.l	#$20000,pic_ptr
		bsr	set_pic
		bra.b	.ok_corr

;-------------- bitmap needs to be in $60000 ---------
.low		cmp.l	#$60000,pic_ptr
		beq.b	.ok_corr
		tst.b	pic_status
		bne.b	.pic_on2
		move.l	#$60000,pic_ptr
		bra.b	.ok_corr
.pic_on2	bsr	remove_pic
		move.l	#$60000,pic_ptr
		bsr	set_pic

.ok_corr	rts


***************************************************
;-------------- trace mode ------------------------
cmd_t		move.b	(a0)+,d0
		beq.w	single_trace
		cmp.b	#$20,d0
		beq.b	cmd_t
		bsr	upper_case
		cmp.b	#'A',d0
		bne.b	no_trace_a
;-------------- trace till address reached -----
seek_tracea	move.b	(a0)+,d0
		beq.w	illegal_addr
		cmp.b	#$20,d0
		beq.b	seek_tracea
		subq.l	#1,a0
		bsr	evaluate
		bne.w	illegal_addr
		btst	#0,d0
		bne.w	illegal_addr
		move.l	d0,trace_address
		st	escape
		bra.w	end_command
;-------------- trace x steps -----------
no_trace_a	subq.l	#1,a0
		bsr	evaluate
		bne.w	illegal_val
		tst.l	d0
		beq.w	illegal_val
		cmp.l	#$10000,d0
		bge.w	illegal_val
init_trace	move.w	d0,trace_count
		st	escape
		bra.w	end_command

single_trace	moveq	#1,d0
		bra.b	init_trace

;---------------------------------------
trace_reach	move.l	a0,-(a7)
		lea.l	trace_count,a0
		subq.w	#1,(a0)
		beq.b	end_trace_mode
		move.l	(a7)+,a0
		or.w	#$8000,(a7)
		rte
end_trace_mode	move.l	vbr_reg,a0
		move.l	old_trace,$24(a0)
		move.l	(a7)+,a0
		and.w	#$7fff,(a7)
		bra.w	monitor
;---------------------------------------------
tracea_reach	move.l	d0,-(a7)
		move.l	trace_address,d0
		cmp.l	2+4(a7),d0
		beq.b	end_tracea
		move.l	(a7)+,d0
		or.w	#$8000,(a7)
		rte
end_tracea	move.l	(a7)+,d0
		move.l	a0,-(a7)
		move.l	vbr_reg,a0
		move.l	old_trace,$24(a0)
		lea.l	trace_address,a0
		move.l	#-1,(a0)
		move.l	(a7)+,a0
		and.w	#$7fff,(a7)
		bra.w	monitor

***************************************************
;-------------- copy memory -----------------------
;-------------- and exg (CE) memory ---------------

cmd_c		move.b	(a0),d0
		bsr	upper_case
		sf	exgc
		cmp.b	#'E',d0
		bne.b	no_exgc
		st	exgc
		addq.l	#1,a0

no_exgc		move.b	(a0)+,d0
		beq.w	illegal_addr
		cmp.b	#$20,d0
		beq.b	no_exgc
		subq.l	#1,a0
		bsr	evaluate
		bne.w	illegal_addr
		move.l	d0,copy_start

seek_end_copy	move.b	(a0)+,d0
		beq.w	illegal_addr
		cmp.b	#$20,d0
		beq.b	seek_end_copy
		subq.l	#1,a0

		bsr	evaluate
		bne.w	illegal_addr
		move.l	d0,copy_end

seek_dest_copy	move.b	(a0)+,d0
		beq.w	illegal_addr
		cmp.b	#$20,d0
		beq.b	seek_dest_copy
		subq.l	#1,a0

		bsr	evaluate
		bne.w	illegal_addr
		move.l	d0,copy_dest

		movem.l	copy_start,d0-d1
		sub.l	d0,d1				;length
		move.l	copy_dest,d0
		add.l	d0,d1
		movem.l	copy_start,d0-d1
		cmp.l	d0,d1
		ble.w	illegal_addr			;if end<start

		move.l	d0,a0
		move.l	d1,a2
		move.l	copy_dest,a1

		bsr	remove_pic

		moveq	#0,d0
		move.l	d0,$180(a6)

		tst.b	exgc
		beq.b	do_copy_mem

do_copye_mem	move.b	(a0),d0
		move.b	(a1),(a0)+
		move.b	d0,(a1)+
		cmp.l	a2,a0
		blt.b	do_copye_mem
		bra.b	end_copy

do_copy_mem	move.b	(a0)+,(a1)+
		cmp.l	a2,a0
		blt.b	do_copy_mem

end_copy	bsr	set_pic

		bra.w	end_command

***************************************************
;-------------- break point set -------------------
cmd_b		move.b	(a0)+,d0
		beq.w	illegal_addr
		bsr	upper_case
		moveq	#0,d7
		cmp.b	#'J',d0
		bne.w	no_jmp_break
		moveq	#-1,d7			;jsr break mode on
		bra.b	seek_break

no_jmp_break	cmp.b	#'D',d0
		bne.b	no_del_break
		lea.l	break_list,a0
		moveq	#-1,d0
do_clr_all_BP	tst.l	(a0)
		bmi.b	cleared_all_BP
		clr.l	(a0)
		move.w	d0,4(a0)
		move.l	d0,6(a0)
		lea.l	10(a0),a0
		bra.b	do_clr_all_BP
cleared_all_BP	lea.l	all_BP_txt,a0
		bsr	print
		bra.w	end_command
;-------------------------------------------

no_del_break	subq.l	#1,a0
seek_break	move.b	(a0)+,d0
		beq.w	print_break
		cmp.b	#$20,d0
		beq.b	seek_break
		subq.l	#1,a0

		bsr	evaluate
		bne.w	illegal_addr
		btst	#0,d0
		bne.w	illegal_addr
		move.l	d0,a2

;-------------- check if clear BP command ------------
		lea.l	break_list,a1
seek_clear_BP	tst.l	(a1)
		bmi.b	end_clear_BP
		cmp.l	(a1),a2
		beq.b	pre_clear_BP
		lea.l	10(a1),a1
		bra.b	seek_clear_BP
pre_clear_BP	cmp.w	#-1,4(a1)
		bne.w	do_clear_BP
		lea.l	10(a1),a1
		bra.b	seek_clear_BP

;-------------- add BP to list -----------------------

end_clear_BP	lea.l	break_list,a1
seek_free_break	tst.l	(a1)
		bmi.w	too_break
		tst.l	(a1)
		bne.b	break_used
		cmp.w	#-1,4(a1)
		beq.b	ok_break
break_used	lea.l	10(a1),a1
		bra.b	seek_free_break

ok_break	tst.l	d7
		beq.b	no_jsrb_set
		move.l	a2,(a1)			;save address
		move.l	a2,a4
		bsr	reloc_pic
		move.w	(a4),4(a1)		;save instr.
		move.l	2(a4),6(a1)		;save param.
		moveq	#8,d1
		move.l	a2,d0
		lea.l	BPJ_set_txt,a0
		bsr	print
		bsr	print_hexCR
		bra.b	ok_jsrb_set

no_jsrb_set	move.l	a2,(a1)
		move.l	a2,a4
		bsr	reloc_pic
		move.w	(a4),4(a1)		;save instr.

		moveq	#8,d1
		move.l	a2,d0
		lea.l	BP_set_txt,a0
		bsr	print
		bsr	print_hexCR

ok_jsrb_set
		bra.w	end_command
;-------------- do clear BP command -----------------------

do_clear_BP	move.w	4(a1),(a2)
		clr.l	(a1)
		move.l	6(a1),d0
		cmp.l	#-1,d0
		beq.b	no_jsrb_clr
		move.l	d0,2(a2)
no_jsrb_clr	move.l	#-1,6(a1)
		move.w	#-1,4(a1)
		moveq	#8,d1
		move.l	a2,d0
		lea.l	BP_clr_txt,a0
		bsr	print
		bsr	print_hexCR
		bra.w	end_command

;-------------- init break points when exiting HRTmon -----
init_break	movem.l	d0-d2/a0-a2,-(a7)
		sf	break_mode
		sf	BP_reach
		sf	BPJ_reach
		sf	BPatPC
		lea.l	break_list,a0
set_BP		tst.l	(a0)
		bmi.b	end_set_BP
		bne.b	set_break
		cmp.w	#-1,4(a0)
		bne.b	set_break
		lea.l	10(a0),a0
		bra.b	set_BP
set_break	cmp.l	#-1,6(a0)
		beq.b	no_BJSR_set
		move.l	(a0),a1
		move.w	#$4eb9,(a1)		;JSR
		lea.l	JSR_reach,a2
		move.l	a2,2(a1)
		bra.b	end_BJ_set
no_BJSR_set	move.l	(a0),a1
		cmp.l	pc_reg,a1
		bne.b	.notpc
		st	BPatPC
		move.w	#1,trace_count
		bra.b	end_BJ_set
.notpc		move.w	#$4afc,(a1)
		st	break_mode
end_BJ_set	lea.l	10(a0),a0
		bra.b	set_BP
end_set_BP	tst.b	break_mode
		beq.b	no_break_mode
		move.l	vbr_reg,a0
		move.l	$10(a0),illegal_except
		lea.l	break_reach,a2
		move.l	a2,$10(a0)
no_break_mode	movem.l	(a7)+,d0-d2/a0-a2
		rts

;-------------- remove all breakpoints from mem. -----
clear_break	movem.l	a0-a1,-(a7)
		lea.l	break_list,a0
do_clr_BP	tst.l	(a0)
		bmi.w	end_clear_BPa
		bne.b	ok_clr_BP
		cmp.w	#-1,4(a0)
		beq.b	no_clr_BP
ok_clr_BP	move.l	(a0),a1
		move.w	4(a0),(a1)
		cmp.l	#-1,6(a0)
		beq.b	no_clr_BP
		move.l	6(a0),2(a1)
no_clr_BP	lea.l	10(a0),a0
		bra.b	do_clr_BP
end_clear_BPa	movem.l	(a7)+,a0-a1
		rts

;----------------------------------------------

break_reach	movem.l	a0-a1/a5,-(a7)
		move.l	2+12(a7),a0		;fetch PC
		sf	BP_reach
		lea.l	break_list,a1
seek_BP_reach	tst.l	(a1)
		bmi.b	break_not_found
		bne.b	do_check_BP
		cmp.w	#-1,4(a1)
		beq.b	no_check_BP
do_check_BP	cmp.l	(a1),a0
		beq.b	found_break
no_check_BP	lea.l	10(a1),a1
		bra.b	seek_BP_reach

found_break	move.w	4(a1),(a0)		;restore old instr.
		clr.l	(a1)
		move.w	#-1,4(a1)		;clear old BP
		move.l	a0,Break_Address
		st	BP_reach
		movem.l	(a7)+,a0-a1/a5
		bra	monitor			;enter monitor

break_not_found	movem.l	(a7)+,a0-a1/a5
		move.l	illegal_except,-(a7)
		rts				;jmp old illegal

;----------------------------------------------------------

JSR_reach	movem.l	a2,-(a7)
		lea.l	JB_save,a2
		movem.l	a0-a1,(a2)
		movem.l	(a7)+,a2
		movem.l	(a7)+,a0
		lea.l	Break_Address,a1
		move.l	a0,(a1)
		move.l	vbr_reg,a1
		move.l	$80(a1),.oldtrap0
		lea.l	.JB_sup(pc),a0
		move.l	a0,$80(a1)
		trap	#0
.oldtrap0	dc.l	0
.JB_sup		move.l	.oldtrap0,$80(a1)
		move.l	Break_Address,a0	;fetch break_address
		subq.l	#6,a0			;sub len jsr address
		lea.l	BPJ_reach,a1
		sf	(a1)
		lea.l	break_list,a1
seek_BPJ_reach	tst.l	(a1)
		bmi.b	bj_not_found
		bne.b	do_check_BPJ
		cmp.w	#-1,4(a1)
		beq.b	no_check_BPJ
do_check_BPJ	cmp.l	(a1),a0
		beq.b	found_breakJ
no_check_BPJ	lea.l	10(a1),a1
		bra.b	seek_BPJ_reach

found_breakJ	move.w	4(a1),(a0)		;restore old instr.
		move.l	6(a1),2(a0)
		clr.l	(a1)
		move.w	#-1,4(a1)		;clear old BP
		move.l	#-1,6(a1)
		move.l	a0,Break_Address
		st	BPJ_reach
		movem.l	JB_save,a0-a1
		move.l	Break_Address,2(a7)
		bra.w	monitor		;enter monitor

bj_not_found	move.l	Break_Address,2(a7)
		movem.l	JB_save,a0-a1
		rte

;--------------------------------------------------
print_break	lea.l	break_list,a1
		moveq	#8,d1
do_break_all	tst.l	(a1)
		bmi.b	end_break_all
		bne.b	ok_break_all
		cmp.w	#-1,4(a1)
		beq.b	no_break_all
ok_break_all	tst.l	6(a1)
		bmi.b	no_Bjsr_prt
		move.l	(a1),d0
		lea.l	all_break2_txt,a0
		bra.b	ok_Bjsr_prt

no_Bjsr_prt	move.l	(a1),d0
		lea.l	all_break_txt,a0
ok_Bjsr_prt	bsr	print
		bsr	print_hexCR

no_break_all	lea.l	10(a1),a1
		bra.b	do_break_all

end_break_all	bra.w	end_command

***************************************************
;-------------- rn-disassemble (RD) -------------------
;syntax: rd 0 <addr> starts disassembling interleaved unencrypted blocks
;syntax: rd 1 <addr> starts disassembling encrypted area, old RN
;syntax: rd 2 <value> sets the decrypting value for rd 3
;syntax: rd 3 <addr> starts disassembling encrypted area, new RN inmidst
;syntax: rd <n> <addr> <offset> copies decrypted instructions to addr+offset

cmd_rd:		move.b	(a0)+,d0
		beq.b	ok_rd_cmd		;keep last address
		cmp.b	#$20,d0
		beq.b	cmd_rd
		subq.l	#1,a0

		move.l	#0,undecroffset

		bsr	evaluate
		bne.w	illegal_val
		cmp.w	#3,d0
		bhi.w	illegal_val
		move.w	d0,rd_mode

		cmp.w	#2,d0
		bne.s	.readrest
		bsr	evaluate
		bne.w	illegal_val
		move.l	d0,rd_mode3_val
		bra.w	end_command


.readrest	bsr	evaluate
		bne.w	illegal_addr
		btst	#0,d0			;address must be even
		bne.w	illegal_addr
		move.l	d0,dis_ptr

		bsr	evaluate
		bne.s	ok_rd_cmd
		move.l	d0,undecroffset

ok_rd_cmd	move.l	dis_ptr,d0		;a4 = ptr on memory
		and.w	#$fffe,d0		;even addr.
		move.l	d0,a4

;		moveq	#8-1,d7
next_rdis_line
		move.w	rd_mode,d0
		cmp.w	#1,d0
		bne.s	.nrdl1
		move.l	(A4),-(A7)
		move.l	-4(A4),d0
		not.l	d0
		swap	D0
		eor.l	d0,(A4)
		bra.s	.nrdl5

.nrdl1		cmp.w	#3,d0
		bne.s	.nrdl5
		move.l	(A4),-(A7)
		move.l	4(A4),-(A7)
		move.l	-4(A4),d0
		add.l	rd_mode3_val,d0
		eor.l	d0,(A4)
		eor.l	d0,4(A4)
		cmp.w	#$cf47,(A4)
		bne.s	.nrdl4
		move.w	#$4e92,(A4)
.nrdl4

.nrdl5		move.l	a4,-(a7)
		bsr	reloc_pic
		lea.l	general_txt,a0
		moveq	#%1101,d0		;upper case, rd $mode $address
		bsr	disassemble
		move.l	(a7)+,a4

		move.l	undecroffset,d7
		beq.s	.nocopy
		MOVEM.L	d0/a0/a1,-(A7)
		subq.w	#1,d0
		lea.l	(A4),a0
		lea.l	(A4,d7.l),a1
.copyinstr	move.b	(A0)+,(A1)+
		dbf	d0,.copyinstr
		MOVEM.L	(A7)+,d0/a0/a1
.nocopy
		moveq.l	#0,d7
		cmp.w	#$4afc,(A4)
		bne.s	.notogglemode
		bset.l	#$10,d7
.notogglemode	move.w	rd_mode,d7
		cmp.w	#1,d7
		bne.s	.nrdl2
		move.l	(A7)+,(A4)
		bra.s	.nrdl6

.nrdl2		cmp.w	#3,d7
		bne.s	.nrdl6
		move.l	(A7)+,4(A4)
		move.l	(A7)+,(A4)

.nrdl6		add.w	d0,a4			;add instr len

		btst.l	#$10,d7
		beq.s	.nrdl3
		bchg	#0,d7	
		move.w	d7,rd_mode

.nrdl3		lea.l	general_txt,a0
		bsr	print

;		tst.b	break
;		dbne	d7,next_rdis_line

		move.l	a4,dis_ptr

		bra.w	end_command

;-------------- disassemble (D) -------------------

cmd_d:		move.b	(a0)+,d0
		beq.b	ok_d_cmd		;keep last address
		cmp.b	#$20,d0
		beq.b	cmd_d
		subq.l	#1,a0

		bsr	evaluate
		bne.w	illegal_addr
		btst	#0,d0			;address must be even
		bne.w	illegal_addr
		move.l	d0,dis_ptr

ok_d_cmd	move.l	dis_ptr,d0		;a4 = ptr on memory
		and.w	#$fffe,d0		;even addr.
		move.l	d0,a4

		moveq	#8-1,d7
next_dis_line
		move.l	a4,-(a7)
		bsr	reloc_pic

		lea.l	general_txt,a0
		moveq	#7,d0			;upper case, d $address
		bsr	disassemble
		move.l	(a7)+,a4
		add.w	d0,a4			;add instr len

		lea.l	general_txt,a0
		bsr	print

		tst.b	break
		dbne	d7,next_dis_line

		move.l	a4,dis_ptr

		bra.w	end_command

;-------------- scroll up disassemble --------

cmdu_d		move.l	ascII_ptr,a0
		lea.l	80(a0),a0
		cmp.w	#'D ',(a0)	;last top line was a disassemble ?
		bne.w	.nod
		addq.l	#2,a0
		bsr	evaluate	;get last address
		move.l	d0,d6		;d6=address of next instr.
		move.l	d0,d5
		sub.l	#20,d5		;d5=address to test

.loop		move.l	d5,a4
		bsr	reloc_pic
		moveq	#7,d0
		lea.l	general_txt,a0
		bsr	disassemble
		add.l	d5,d0
		cmp.l	d0,d6		;right len of instr ?
		beq.b	.ok

		move.l	d0,d5
		cmp.l	d6,d5		;reach end of area to test
		blt.b	.loop

;---- couldn't find an instr. with the right length -> print 'dc.w'

		lea.l	general_txt,a0
		move.w	#"D ",(a0)+
		move.b	#"$",(a0)+
		move.l	d6,d0
		subq.l	#2,d0
		moveq	#8,d1
		bsr	conv_hex
		add.l	d1,a0
		move.b	#" ",(a0)+
		move.l	#"Dc.W",(a0)+
		move.w	#" $",(a0)+
		move.l	d6,a4
		bsr	reloc_pic
		move.w	(a4),d0
		moveq	#4,d1
		bsr	conv_hex
		add.l	d1,a0
		move.w	#$0a00,(a0)+
		bra.w	.ok

.ok		move.l	d5,d0

		lea.l	general_txt,a0
		bsr	print
		subq.w	#1,(2,a3)		;cursor one line up

		moveq	#0,d0

.nod		rts

.bad_addr	lea.l	illegal_addr_txt,a0
		bsr	print
		bra.b	.nod

;-------------- scroll down disassemble ---------

cmdd_d		move.l	ascII_ptr,a0
		move.l	d0,-(a7)
		move.w	window_bot,d0
		subq.w	#2,d0
		mulu	#80,d0
		add.l	d0,a0
		move.l	(a7)+,d0
		cmp.w	#'D ',(a0)	;last line was a disassemble ?
		bne.b	.nod
		addq.l	#2,a0
		bsr	evaluate	;get last address
		move.l	d0,d6
		move.l	d0,a4
		bsr	reloc_pic
		moveq	#0,d0
		lea.l	general_txt,a0
		bsr	disassemble	;calc last instr size
		add.l	d6,d0
		move.l	d0,a4
		move.l	d0,d6
		bsr	reloc_pic
		moveq	#7,d0
		lea.l	general_txt,a0
		bsr	disassemble
		add.l	d0,d6			;add instr. size
		move.l	d6,dis_ptr		;next instr. address

		subq.w	#1,(2,a3)		;cursor one line up
		lea.l	general_txt,a0
		bsr	print

		moveq	#0,d0

.nod		rts


		include src/65816.s

***********************************************************
;-------------- error routines ---------------------

illegal_syntax	lea.l	illegal_syntax_txt,a0
		bsr	print
		bra.w	end_command

illegal_val	lea.l	illegal_val_txt,a0
		bsr	print
		bra.w	end_command

illegal_reg	lea.l	illegal_reg_txt,a0
		bsr	print
		bra.w	end_command

illegal_addr	lea.l	illegal_addr_txt,a0
		bsr	print
		bra.w	end_command

illegal_name	lea.l	illegal_name_txt,a0
		bsr	print
		bra.w	end_command

illegal_string	lea.l	illegal_string_txt,a0
		bsr	print
		bra.w	end_command

illegal_expr	lea.l	illegal_expr_txt,a0
		bsr	print
		bra.w	end_command

too_break	lea.l	too_break_txt,a0
		bsr	print
		bra.w	end_command

not_found	lea.l	not_found_txt,a0
		bsr	print
		bra.w	end_command

*********************************************************
;d0=ascII
upper_case:	cmp.b	#'a',d0			;switch to upper case (d0)
		blt.b	.ok_upper
		cmp.b	#'z',d0
		bgt.b	.ok_upper
		add.b	#'A'-'a',d0
.ok_upper	rts

;d0=ascII
lower_case	cmp.b	#'A',d0			;switch to lower case (d0)
		blt.b	.ok_lower
		cmp.b	#'Z',d0
		bgt.b	.ok_lower
		add.b	#'a'-'A',d0
.ok_lower	rts

;d1=ascII
upper_case1	cmp.b	#'a',d1			;switch to upper case (d1)
		blt.b	.ok_upper
		cmp.b	#'z',d1
		bgt.b	.ok_upper
		add.b	#'A'-'a',d1
.ok_upper	rts

************************************************************
;read string at a0 and copy it in a1 (0=end of string)
;' ' can surround a string

read_name	movem.l	d0/a1,-(a7)
.loop		cmp.b	#$20,(a0)+
		beq.b	.loop
		subq.l	#1,a0
		cmp.b	#"'",(a0)
		bne.b	.colle
		addq.l	#1,a0
.loop2		move.b	(a0)+,d0
		beq.b	.err
		cmp.b	#"'",d0
		bne.b	.g1
		cmp.b	#"'",(a0)
		bne.b	.end_n
		addq.l	#1,a0
.g1		move.b	d0,(a1)+
		bra.b	.loop2
.end_n		clr.b	(a1)+
		movem.l	(a7)+,d0/a1
		rts

.colle		move.b	(a0)+,d0
		beq.b	.err
		cmp.b	#$20,d0
		beq.b	.end_n
		move.b	d0,(a1)+
		bra.b	.colle

.err		movem.l	(a7)+,d0/a1
		clr.b	(a1)
		rts

*************************************************************
;-> a0=ptr on ascII number
;<- returns number in d0

read_number	movem.l	d1-d3/a1,-(a7)
.spc		cmp.b	#$20,(a0)+
		beq.b	.spc
		subq.l	#1,a0
		st	ok_numb
		sf	minus
		cmp.b	#'-',(a0)
		bne.b	.no_minus
		st	minus
		addq.l	#1,a0
.no_minus	moveq	#0,d2
		moveq	#0,d0
		cmp.b	#'#',(a0)
		bne.b	no_dec_num

		addq.l	#1,a0		;jmp #

next_dec	move.b	(a0)+,d0
		cmp.b	#'0',d0
		blt.w	end_number
		cmp.b	#'9',d0
		bgt.w	end_number
		sub.b	#'0',d0

		add.l	d2,d2
		move.l	d2,d3
		lsl.l	#2,d2
		add.l	d3,d2		;mulu	#10,d2

		add.l	d0,d2
		sf	ok_numb
		bra.b	next_dec

;-------------- read binary number ------------------

no_dec_num	cmp.b	#'%',(a0)
		bne.b	no_bin_num
		addq.l	#1,a0

		moveq	#0,d2
.loop		move.b	(a0)+,d0
		sub.b	#'0',d0
		bmi.w	end_number
		cmp.b	#1,d0
		bgt.w	end_number
		add.l	d2,d2
		or.b	d0,d2
		sf	ok_numb
		bra.b	.loop

no_bin_num	cmp.b	#"'",(a0)
		bne.b	no_ascII_num
		addq.l	#1,a0
		moveq	#0,d2
.loop		move.b	(a0)+,d0
		beq.w	end_number
		cmp.b	#"'",d0
		bne.b	.no_end_ascII
		cmp.b	#"'",(a0)
		bne.b	end_ascII_num
		addq.l	#1,a0
.no_end_ascII	lsl.l	#8,d2
		move.b	d0,d2
		bra.b	.loop

end_ascII_num	addq.l	#1,a0
		sf	ok_numb
		bra.w	end_number

;-------------- read hexa number + label --------------------

no_ascII_num
		cmp.b	#'\',(a0)
		bne.b	.query_label
		addq.l	#1,a0
		bra.b	.do_label


.query_label	move.l	a0,a1
		tst.b	label	;test if label enabled
		bne.b	.no_label
.do_label
		bsr	check_label
		tst.l	d0
		beq.b	.no_label
		sf	ok_numb
		move.l	a4,-(a7)
		move.l	d0,a4
		bsr	reloc_pic
		move.l	(a4),d2
		move.l	(a7)+,a4
		addq.l	#1,a0
		bra.b	end_number

.no_label	move.l	a1,a0
		cmp.b	#'$',(a0)	;jmp $ for hex number
		bne.s	no_hex_num
		addq.l	#1,a0		;jmp $
next_hex
.ok_dollar	lea.l	hex_list,a1
		move.b	(a0)+,d0
		bsr	upper_case
		moveq	#$f,d1
seek_hex	cmp.b	(a1,d1.w),d0
		dbeq	d1,seek_hex
		tst.w	d1
		bmi.b	end_number
		lsl.l	#4,d2
		or.w	d1,d2
		sf	ok_numb
		bra.b	next_hex

no_hex_num	tst.b	hex_default
		beq.w	next_dec
		bra.b	next_hex


end_number	subq.l	#1,a0
		move.l	d2,d0
		tst.b	minus
		beq.b	.no_minus
		neg.l	d0
.no_minus	tst.b	ok_numb		;test if error
		movem.l	(a7)+,d1-d3/a1
		rts

check_label	movem.l	d1-d3/a1-a4,-(a7)
		move.l	a0,d3

		tst.b	hex_default
		beq.b	.all_labels
					;d0-a7 are only with \ labels in hexmode
.hex_mode	lea.l	label_list2(pc),a1
		move.b	-1(a0),d2
		cmp.b	#'\',d2
		bne.b	.allmode
.all_labels	lea.l	label_list(pc),a1

.allmode	moveq	#0,d2
.main		tst.b	(a1)
		beq.b	.end_list
		move.l	a0,a2
		move.l	a1,a3
		moveq	#8-1,d1
.do_cmp		move.b	(a2)+,d0
		bsr	upper_case
		cmp.b	(a3)+,d0
		bne.b	.no_egal
		tst.b	(a3)
		dbeq	d1,.do_cmp
		move.l	8(a1),d2
		move.l	a2,a4
.no_egal	lea.l	8+4(a1),a1
		bra.b	.main

.end_list	move.l	a4,a0			;get end of label
		move.b	(a0),d0
		beq.b	.ok_label
		bsr	upper_case
		lea.l	.symb_list,a1
.seek		cmp.b	(a1)+,d0
		beq.b	.ok_label
		tst.b	(a1)
		bne.b	.seek
		moveq	#0,d2
		move.l	d3,a0
.ok_label	move.l	d2,d0
		movem.l	(a7)+,d1-d3/a1-a4
		rts

;list of symbols which can follow a label.

.symb_list	dc.b $a,' ','+','-','*','/','&','|',')','.',']',0
		cnop 0,4

label_list
		dc.b "D0",0,0,0,0,0,0
		dc.l registres
		dc.b "D1",0,0,0,0,0,0
		dc.l registres+4
		dc.b "D2",0,0,0,0,0,0
		dc.l registres+2*4
		dc.b "D3",0,0,0,0,0,0
		dc.l registres+3*4
		dc.b "D4",0,0,0,0,0,0
		dc.l registres+4*4
		dc.b "D5",0,0,0,0,0,0
		dc.l registres+5*4
		dc.b "D6",0,0,0,0,0,0
		dc.l registres+6*4
		dc.b "D7",0,0,0,0,0,0
		dc.l registres+7*4

		dc.b "A0",0,0,0,0,0,0
		dc.l registres+8*4
		dc.b "A1",0,0,0,0,0,0
		dc.l registres+9*4
		dc.b "A2",0,0,0,0,0,0
		dc.l registres+10*4
		dc.b "A3",0,0,0,0,0,0
		dc.l registres+11*4
		dc.b "A4",0,0,0,0,0,0
		dc.l registres+12*4
		dc.b "A5",0,0,0,0,0,0
		dc.l registres+13*4
		dc.b "A6",0,0,0,0,0,0
		dc.l registres+14*4
		dc.b "A7",0,0,0,0,0,0
		dc.l registres+15*4

label_list2
		dc.b "PC",0,0,0,0,0,0
		dc.l pc_reg
		dc.b "VBR",0,0,0,0,0
		dc.l vbr_reg
		dc.b "CACR",0,0,0,0
		dc.l cacr_reg
		dc.b "CAAR",0,0,0,0
		dc.l caar_reg
		dc.b "PCR",0,0,0,0,0
		dc.l msp_reg
		dc.b "BUSR",0,0,0,0
		dc.l caar_reg
		dc.b "ISP",0,0,0,0,0
		dc.l isp_reg
		dc.b "MSP",0,0,0,0,0
		dc.l msp_reg
		dc.b "USP",0,0,0,0,0
		dc.l usp_reg

		dc.b "EXEC",0,0,0,0
		dc.l $4
		dc.b "LEV3",0,0,0,0
		dc.l $6c

		dc.b 0

		cnop 0,4

************************************************************

read_fact	cmp.b	#'(',(a0)
		bne.b	.no_parg
		addq.l	#1,a0
		bsr.w	read_expr
		cmp.b	#')',(a0)+
		bne.w	eval_error
		bra.b	end_fact
.no_parg	cmp.b	#'[',(a0)
		bne.b	.no_cro
		addq.l	#1,a0
		bsr.w	read_expr
		cmp.b	#']',(a0)+
		bne.w	eval_error
		move.l	d0,a4
		bsr	reloc_pic
		move.l	(a4),d0
		bra.b	end_fact
.no_cro		bsr	read_number
		bne.w	eval_error

end_fact	cmp.b	#'.',(a0)
		bne.b	.no_size
		addq.l	#1,a0			;jmp .
		move.l	d1,-(a7)
		move.b	(a0)+,d1
		bsr	upper_case1
		cmp.b	#'B',d1
		bne.b	.no_b
		ext.w	d0
		ext.l	d0
		clr.w	eval_size
		bra.b	.end_size
.no_b		cmp.b	#'W',d1
		bne.b	.no_w
		ext.l	d0
		move.w	#1,eval_size
		bra.b	.end_size
.no_w		cmp.b	#'L',d1
		bne.w	eval_error
		move.w	#3,eval_size
.end_size	move.l	(a7)+,d1
.no_size	rts


read_terme	move.l	d1,-(a7)
		bsr.w	read_fact
		move.l	d0,d1
.loop		cmp.b	#'*',(a0)
		bne.b	.no_fois
		addq.l	#1,a0
		bsr.w	read_fact
		bsr.w	muls32
		bra.b	.loop
.no_fois	cmp.b	#'/',(a0)
		bne.b	.no_div
		addq.l	#1,a0
		bsr.w	read_fact
		bsr.w	divs32
		bra.b	.loop
.no_div		move.l	d1,d0
		move.l	(a7)+,d1
		rts

read_expr	move.l	d1,-(a7)
		cmp.b	#'+',(a0)
		bne.b	.no_p
		addq.l	#1,a0
.no_p		cmp.b	#'-',(a0)
		bne.b	.no_m
		addq.l	#1,a0
		bsr.b	read_terme
		neg.l	d0
		bra.b	.ok_m
.no_m		bsr.b	read_terme
.ok_m		move.l	d0,d1
.loop		cmp.b	#'+',(a0)
		bne.b	.no_plus
		addq.l	#1,a0
		bsr.b	read_terme
		add.l	d0,d1
		bra.b	.loop
.no_plus	cmp.b	#'-',(a0)
		bne.b	.no_moins
		addq.l	#1,a0
		bsr.b	read_terme
		sub.l	d0,d1
		bra.b	.loop
.no_moins	move.l	d1,d0
		move.l	(a7)+,d1
		rts

;-> a0 =ptr on expr
;<- d0 value
;<- flag: zero=ok, negative=error, positive=no number

evaluate:	move.l	a7,exit_stack
.loop		cmp.b	#$20,(a0)+
		beq.b	.loop
		subq.l	#1,a0
		tst.b	(a0)
		beq.b	.no_eval
		bsr	read_expr
		tst.b	ok	;zero = ok
		rts
.no_eval	tst.b	ok2
		rts
eval_error	move.l	exit_stack,a7
		moveq	#-1,d0		;neg val = error
		rts

ok		dc.b 0
ok2		dc.b 1			;positive val = no number

;d0.l,d1.l
;d1.l=d0.l*d1.l
muls32		movem.l	d0/d2/a0,-(a7)
		lea.l	muls_data,a0
		movem.l	d0-d1,(a0)
		swap	d0
		mulu	d0,d1
		move.w	d1,d2
		movem.l	(a0),d0-d1
		swap	d1
		mulu	d0,d1
		add.w	d1,d2
		swap	d2
		clr.w	d2
		movem.l	(a0),d0-d1
		mulu	d0,d1
		add.l	d2,d1
		movem.l	(a7)+,d0/d2/a0
		rts


;d0.l,d1.l
;d1.l=d1.l/d0.l
divs32		movem.l	d0/d2-d4/a0,-(a7)
		exg	d0,d1
		sf	d4
		tst.l	d0
		bpl.b	.n1
		neg.l	d0
		not.b	d4
.n1		tst.l	d1
		bpl.b	.n2
		neg.l	d1
		not.b	d4
.n2		move.l	#$ffff,d3
		lea.l	divs_data,a0
		movem.l	d0-d1,(a0)
		moveq	#0,d2
		cmp.l	#$10000,d1
		bge.b	.div2

		tst.w	d1
		beq.w	eval_error

		swap	d0
		and.l	d3,d0
		divu	d1,d0
		move.w	d0,d2
		swap	d2
		clr.w	d2
		move.w	2(a0),d0
		divu	d1,d0
		and.l	d3,d0
		add.l	d0,d2
		bra.b	.ok1

.div2		lsr.l	#1,d0
		lsr.l	#1,d1
		cmp.l	#$10000,d1
		bge.b	.div2
		tst.w	d1
		beq.w	eval_error
		divu	d1,d0
		move.w	d0,d2
		and.l	d3,d2

.ok1		tst.b	d4
		beq.b	.n3
		neg.l	d2
.n3		move.l	d2,d1
		movem.l	(a7)+,d0/d2-d4/a0
		rts

************************************************************
;-> a0=ptr on ascII registre
;<- returns no of registre in d0 and end of ascII reg in a0

read_reg	movem.l	d1-d3/a1-a4,-(a7)
		move.l	a0,d3
		lea.l	reg_table,a1

		moveq	#-1,d2
.main		tst.b	(a1)
		beq.b	.end_list
		move.l	a0,a2
		move.l	a1,a3
		moveq	#4-1,d1
.do_cmp		move.b	(a2)+,d0
		bsr	upper_case
		cmp.b	(a3)+,d0
		bne.b	.no_egal
		tst.b	(a3)
		dbeq	d1,.do_cmp
		move.w	4(a1),d2
		move.l	a2,a4
.no_egal	lea.l	4+2(a1),a1
		bra.b	.main

.end_list	move.l	a4,a0			;get end of register
		move.w	d2,d0
		movem.l	(a7)+,d1-d3/a1-a4
		rts

reg_table
		dc.b "D0",0,0
		dc.w 0
		dc.b "D1",0,0
		dc.w 1
		dc.b "D2",0,0
		dc.w 2
		dc.b "D3",0,0
		dc.w 3
		dc.b "D4",0,0
		dc.w 4
		dc.b "D5",0,0
		dc.w 5
		dc.b "D6",0,0
		dc.w 6
		dc.b "D7",0,0
		dc.w 7
		dc.b "A0",0,0
		dc.w 8
		dc.b "A1",0,0
		dc.w 9
		dc.b "A2",0,0
		dc.w 10
		dc.b "A3",0,0
		dc.w 11
		dc.b "A4",0,0
		dc.w 12
		dc.b "A5",0,0
		dc.w 13
		dc.b "A6",0,0
		dc.w 14
		dc.b "A7",0,0
		dc.w 15
		dc.b "PC",0,0
		dc.w 16
		dc.b "SR",0,0
		dc.w 17
		dc.b "VBR",0
		dc.w 18
		dc.b "USP",0
		dc.w 19
		dc.b "SSP",0
		dc.w 20
		dc.b "CACR"
		dc.w 21
		dc.b "PCR",0
		dc.w 22
		dc.b "BUSR"
		dc.w 23
		dc.w 0


************************************************************
;-------------- Run-length packing -------------------------

;-> a0=ptr on source mem to pack
;-> a1=destination mem
;-> d0.w=size to pack
;<- d0.w=packed size

pack		movem.l	d1-d6/a0-a4,-(a7)
		move.w	#$100,$dff096		;disable bitplan DMA

		move.w	#0,$dff106
		move.l	a1,a4
		lea.l	8(a4),a2
		move.l	d0,4(a4)

		move.l	a0,a3
		move.l	d0,d3			;d3=original size

		move.l	d0,d5			;d5=nb of bytes still to pack

.loop		move.l	d5,d6
		cmp.l	#128,d6
		ble.b	.min
		moveq	#127,d6
		addq.l	#1,d6			;d6=128
.min
		move.l	d6,d0
		subq.l	#1,d0			;d6=1 ?
		bne.b	.no1
		move.b	(a0)+,(a2)+
		sf	(a2)+
		bra.b	.end
.no1
		move.b	(a0)+,d0
		move.b	d0,$dff180
		move.b	(a0)+,d1
		cmp.b	d0,d1
		bne.b	.diff

		moveq	#1,d4
		subq.l	#1,d6

.cntsame	addq.l	#1,d4
		subq.l	#1,d6
		beq.b	.out2
		cmp.b	(a0)+,d0
		beq.b	.cntsame
		subq.l	#1,a0
.out2
		sub.l	d4,d5
		neg.w	d4
		move.b	d0,(a2)+
		move.b	d4,(a2)+
		tst.l	d5
		beq.b	.end
		bra.b	.loop

;---------------------------------
.diff		lea.l	-2(a0),a1
		moveq	#0,d4
.cntdiff	move.b	d1,d0
		addq.l	#1,d4
		subq.l	#1,d6
		beq.b	.out
		move.b	(a0)+,d1
		cmp.b	d0,d1
		bne.b	.cntdiff
		subq.l	#1,a0

.out		subq.l	#1,a0
		sub.l	d4,d5
		subq.l	#1,d4
		move.w	d4,d0
.copy		move.b	(a1)+,(a2)+
		dbf	d4,.copy
		move.b	d0,(a2)+
		tst.l	d5			;end of block ?
		bne.b	.loop

.end		move.l	a2,d0
		lea.l	8(a4),a1
		sub.l	a1,d0			;d0=packed size
		move.l	d0,(a4)			;first long=packed size
		cmp.l	d0,d3
		bgt.b	.pack
		lea.l	8(a4),a1
		move.l	d3,d0
		neg.l	d0
		move.l	d0,(a4)
		move.l	d3,d0
		move.b	d3,d1
		lsr.l	#2,d3
.copyl		move.l	(a3)+,(a1)+		;copy unpacked area
		subq.l	#1,d3
		bne.b	.copyl
		and.w	#3,d1
		bra.b	.godbf
.copyb		move.b	(a3)+,(a1)+
.godbf		dbf	d1,.copyb
.pack
		addq.l	#8,d0			;for first long=size

		move.w	#0,$dff106
		move.w	#0,$dff180
		move.w	#$8100,$dff096		;enable bitplan DMA

		movem.l	(a7)+,d1-d6/a0-a4
		rts

;----------------------------------------------------------

;-> a0=ptr on packed mem
;-> a1=ptr on dest mem

depack		movem.l	d0-d1/a0-a2,-(a7)
		move.w	#0,$dff106
		move.w	#$100,$dff096		;disable bitplan DMA

		tst.l	(a0)
		bmi.b	.nopack
		movem.l	(a0)+,d0-d1
		move.l	a0,a2
		add.l	d0,a0
		add.l	d1,a1

.loop		cmp.l	a2,a0
		beq.b	.end
		moveq	#0,d0
		move.b	-(a0),d0
		bmi.b	.neg
.copy		move.b	-(a0),-(a1)
		dbf	d0,.copy
		move.b	(a1),$dff180
		bra.b	.loop
.neg		ext.w	d0
		not.w	d0
		move.b	-(a0),d1
		move.b	d1,$dff180
.fill		move.b	d1,-(a1)
		dbf	d0,.fill
		bra.b	.loop

.nopack		addq.l	#8,a0
		move.l	4(a0),d0
		move.b	d0,d1
		lsr.l	#2,d0
.copyl		move.l	(a0)+,(a1)+
		subq.l	#1,d0
		bne.b	.copyl
		and.w	#3,d1
		bra.b	.godbf
.copyb		move.b	(a0)+,(a1)+
.godbf		dbf	d1,.copyb

.end
		move.w	#0,$dff106
		move.w	#0,$dff180
		move.w	#$8100,$dff096		;enable bitplan DMA

		movem.l	(a7)+,d0-d1/a0-a2
		rts

************************************************************

;a0=ptr on text

print_curs	bsr	clear_cursor	;special print which clears cursor
		clr.w	cursor_x	;and set it to x=0
		bsr.b	print
		bsr	set_cursor
		rts


print:		movem.l	d0-d2/a0-a4,-(a7)
		move.l	output_ptr,d0
		beq.b	.noout
		move.l	a0,a1
		move.l	d0,a2
.copy		move.b	(a1)+,(a2)+
		bne.b	.copy
		subq.l	#1,a2
		move.l	a2,output_ptr
;		move.l	a2,watch2

.noout		move.w	window_bot,d2
		lea.l	cursor_x,a3
		move.w	2(a3),d0
		cmp.w	window_top,d0
		bge.b	.ok1
		move.w	window_top,d0
.ok1		cmp.w	window_bot,d0
		blt.b	.ok2
		move.w	window_bot,d0
.ok2		move.w	d0,2(a3)

		move.l	ascII_ptr,a2
		lea.l	print_cnt,a4
		clr.w	(a4)

print_next
go_print	tst.b	break
		bne.w	end_print
		move.w	2(a3),d0
		mulu	#80,d0
		lea.l	(a2,d0.l),a1
		add.w	(a3),a1

		move.b	(a0)+,d0
		beq.b	end_printl
		cmp.b	#$0a,d0
		beq.b	end_printl
		cmp.b	#9,d0
		bne.b	.no_tab
		move.w	(a3),d0
		and.w	#$fff8,d0
		addq.w	#8,d0
		move.w	d0,(a3)
		bra.b	go_print
.no_tab		cmp.b	#$0d,d0
		bne.b	.no_13
		clr.w	(a3)
		bra.b	go_print

.no_13		move.b	d0,(a1)

		addq.w	#1,(a3)
		cmp.w	#80,(a3)
		blt.b	.ok
		clr.w	(a3)
		move.w	2(a3),d0
		bsr	print_line
		addq.w	#1,(a4)
		cmp.w	(a4),d2
		bgt.b	.okp
		bsr.b	print_wait
.okp		cmp.w	2(a3),d2
		bgt.b	.ok_cr_prt
		subq.w	#1,2(a3)
		bsr	scroll_up
.ok_cr_prt	addq.w	#1,2(a3)
.ok
		bra.b	go_print

end_printl	tst.b	d0
		beq.b	end_print
;-------------- print CR -------------------
		move.w	2(a3),d0
		bsr	print_line
		addq.w	#1,(a4)
		cmp.w	(a4),d2
		bgt.b	.okp
		bsr.b	print_wait
.okp		clr.w	(a3)
		cmp.w	2(a3),d2
		bgt.b	.ok_cr_prt
		subq.w	#1,2(a3)
		bsr	scroll_up
.ok_cr_prt	addq.w	#1,2(a3)
		tst.b	(a0)
		beq.b	end_print_out
		bra.w	print_next
;-------------------------------------------
end_print	move.w	2(a3),d0
		bsr	print_line
end_print_out
		movem.l	(a7)+,d0-d2/a0-a4
		rts

print_wait	tst.b	no_print
		bne.b	.nowait
		clr.w	(a4)
		st	new_key
.wait		move.w	#15,time_cursor
		tst.b	new_key
		bne.b	.wait
.nowait		rts

************************************************************

no_sc_clr	dc.b 0
		even

scroll_up	move.l	d2,-(a7)
		moveq	#1,d2
		bsr.b	scroll_up2
		move.l	(a7)+,d2
		rts

;-> d2 = nb of lines

scroll_up2	movem.l	d0-d1/a0-a1,-(a7)
		move.l	ascII_ptr,a0
		move.w	window_top,d0
		mulu	#80,d0
		add.l	d0,a0
		move.w	d2,d0
		mulu	#80,d0
		lea.l	(a0,d0.l),a1
		move.w	window_bot,d0
		sub.w	window_top,d0
		sub.w	d2,d0
		addq.w	#1,d0
		bra.b	.go_dbf
.ascII_up	move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
.go_dbf		dbf	d0,.ascII_up
		move.l	#'    ',d0
		moveq	#5,d1
		mulu	d2,d1
		subq.w	#1,d1
.clear_up	move.l	d0,(a0)+
		move.l	d0,(a0)+
		move.l	d0,(a0)+
		move.l	d0,(a0)+
		dbf	d1,.clear_up

		tst.b	no_print
		bne.w	.noscroll

		move.l	pic_ptr,a0
		move.w	window_top,d0
		mulu	#80*h,d0
		add.l	d0,a0
		move.w	d2,d0
		mulu	#80*h,d0
		lea.l	(a0,d0.l),a1
		move.w	window_bot,d0
		sub.w	window_top,d0
		sub.w	d2,d0
		addq.w	#1,d0
		mulu	#h,d0
		bra.b	.go_dbf2
.pixel_up	move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
		move.l	(a1)+,(a0)+
.go_dbf2	dbf	d0,.pixel_up

		moveq	#0,d0
		tst.b	no_sc_clr
		beq.b	.goclr
		move.w	d2,d1
		subq.w	#1,d1
		mulu	#80*h,d1
		add.l	d1,a0
		moveq	#5*h-1,d1
		bra.b	.clear_pix_up
.goclr		move.w	d2,d1
		mulu	#5*h,d1
		subq.w	#1,d1
.clear_pix_up	move.l	d0,(a0)+
		move.l	d0,(a0)+
		move.l	d0,(a0)+
		move.l	d0,(a0)+
		dbf	d1,.clear_pix_up

.noscroll	movem.l	(a7)+,d0-d1/a0-a1
		rts

;-------------------------------------------

scroll_down	move.l	d2,-(a7)
		moveq	#1,d2
		bsr.b	scroll_down2
		move.l	(a7)+,d2
		rts

;-> d2 = nb of lines

scroll_down2	movem.l	d0-d1/a0-a1,-(a7)

		move.l	ascII_ptr,a0
		move.w	window_bot,d0
		mulu	#80,d0
		add.l	d0,a0
		move.l	a0,a1
		move.w	d2,d0
		subq.w	#1,d0
		mulu	#80,d0
		sub.l	d0,a1
		lea.l	80(a0),a0
		move.w	window_bot,d0
		sub.w	window_top,d0
		sub.w	d2,d0
		addq.w	#1,d0
		bra.b	.go_dbf
.ascII_down	move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
.go_dbf		dbf	d0,.ascII_down
		move.l	#'    ',d0
		move.w	d2,d1
		mulu	#5,d1
		subq.w	#1,d1
.clear_down	move.l	d0,-(a0)
		move.l	d0,-(a0)
		move.l	d0,-(a0)
		move.l	d0,-(a0)
		dbf	d1,.clear_down

		tst.b	no_print
		bne.w	.noscroll

		move.l	pic_ptr,a0
		move.w	window_bot,d0
		mulu	#80*h,d0
		add.l	d0,a0
		move.l	a0,a1
		move.w	d2,d0
		subq.w	#1,d0
		mulu	#80*h,d0
		sub.l	d0,a1
		lea.l	80*h(a0),a0

		move.w	window_bot,d0
		sub.w	window_top,d0
		sub.w	d2,d0
		addq.w	#1,d0
		mulu	#h,d0
		bra.b	.go_dbf2
.pixel_down	move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
		move.l	-(a1),-(a0)
.go_dbf2	dbf	d0,.pixel_down

		moveq	#0,d0
		tst.b	no_sc_clr
		beq.b	.goclr
		move.w	d2,d1
		subq.w	#1,d1
		mulu	#80*h,d1
		sub.l	d1,a0
		moveq	#5*h-1,d1
		bra.b	.clear_pix_down
.goclr		move.w	d2,d1
		mulu	#5*h,d1
		subq.w	#1,d1
.clear_pix_down	move.l	d0,-(a0)
		move.l	d0,-(a0)
		move.l	d0,-(a0)
		move.l	d0,-(a0)
		dbf	d1,.clear_pix_down

.noscroll	movem.l	(a7)+,d0-d1/a0-a1
		rts

************************************************************

;d0=no of line

print_line	movem.l	d0-d4/a0-a4,-(a7)
		tst.b	no_print
		bne.w	.no_print

		move.l	ascII_ptr,a0
		mulu	#80,d0
		add.l	d0,a0
		move.l	pic_ptr,a1
		mulu	#h,d0
		add.l	d0,a1

		lea.l	topaz2,a3
		lea.l	ascII_conv,a4
		moveq	#80-1,d4
		moveq	#0,d2
		moveq	#0,d0

.do_line_prt	move.b	(a0)+,d2

		move.b	(a4,d2.w),d0
		lea.l	(a3,d0.w),a2

		move.b	110(a2),80(a1)
		move.b	220(a2),160(a1)
		move.b	330(a2),240(a1)
		move.b	440(a2),320(a1)
		move.b	550(a2),400(a1)
		move.b	660(a2),480(a1)
		move.b	770(a2),560(a1)
		move.b	880(a2),640(a1)
		move.b	(a2),(a1)+

		dbf	d4,.do_line_prt

.stop_print	move.w	#15,time_cursor
		btst	#10-8,$dff016
		beq.b	.stop_print

.no_print	movem.l	(a7)+,d0-d4/a0-a4
		rts

;---------------------------------------------------

;d0=x pos
;d1=y pos
;d2=ascII char

print_char	movem.l	d0-d2/a1-a2,-(a7)
		move.l	ascII_ptr,a1
		add.w	d0,a1
		lsl.w	#4,d1
		move.w	d1,a2
		add.w	d1,d1
		add.w	d1,d1
		add.w	a2,d1			;d1*80
		move.b	d2,(a1,d1.w)

		and.w	#$ff,d2

		lea.l	ascII_conv,a2
		move.b	(a2,d2.w),d2
		lea.l	topaz2,a2
		lea.l	(a2,d2.w),a2

		move.l	pic_ptr,a1
		add.w	d0,a1
		mulu	#h,d1
		add.l	d1,a1
		move.b	(a2),(a1)
		move.b	110(a2),80(a1)
		move.b	220(a2),160(a1)
		move.b	330(a2),240(a1)
		move.b	440(a2),320(a1)
		move.b	550(a2),400(a1)
		move.b	660(a2),480(a1)
		move.b	770(a2),560(a1)
		move.b	880(a2),640(a1)
		movem.l	(a7)+,d0-d2/a1-a2
		rts


;d0=x pos
;d1=y pos
;d2=ascII char

;print on all screens

print_char2	movem.l	d0-d2/a1-a3,-(a7)
		lsl.w	#4,d1
		move.w	d1,a2
		add.w	d1,d1
		add.w	d1,d1
		add.w	a2,d1			;d1*80
		add.w	d0,d1
		lea.l	ascII_ptr+4,a1
.loop		tst.l	(a1)
		beq.b	.end_ascII
		move.l	(a1)+,a3
		move.b	d2,(a3,d1.w)
		bra.b	.loop

.end_ascII	and.w	#$ff,d2

		lea.l	ascII_conv,a2
		move.b	(a2,d2.w),d2
		lea.l	topaz2,a2
		lea.l	(a2,d2.w),a2

		sub.w	d0,d1

		move.l	pic_ptr,a1
		add.w	d0,a1
		mulu	#h,d1
		add.l	d1,a1
		move.b	(a2),(a1)
		move.b	110(a2),80(a1)
		move.b	220(a2),160(a1)
		move.b	330(a2),240(a1)
		move.b	440(a2),320(a1)
		move.b	550(a2),400(a1)
		move.b	660(a2),480(a1)
		move.b	770(a2),560(a1)
		move.b	880(a2),640(a1)
		movem.l	(a7)+,d0-d2/a1-a3
		rts

do_cursor:	bsr	clear_snap
		tst.b	no_curs
		bne.w	.no_cur

		movem.l	d0-a6,-(a7)
		lea.l	$dff000,a6
		bsr	init_mouse
		move.w	x_spr,d0
		bpl.b	.ok1
		clr.w	d0
.ok1		cmp.w	#640,d0
		blt.b	.ok2
		move.w	#639,d0
.ok2		move.w	d0,x_spr
		lsr	#3,d0
		move.w	d0,snap_x
		move.w	y_spr,d0
		bpl.b	.ok3
		clr.w	d0
.ok3		move.w	screen_height,d1
		mulu	#9,d1
		cmp.w	d1,d0
		blt.b	.ok4
		move.w	d1,d0
		subq.w	#1,d0
.ok4		move.w	d0,y_spr
		ext.l	d0
		divu	#9,d0
		move.w	d0,snap_y
		bsr	set_snap

		tst.b	nosnap
		bne.w	.nos
		tst.b	left
		beq.w	.noleft
		move.l	ascII_ptr,a0
		move.w	snap_y,d0
		mulu	#80,d0
		add.l	d0,a0
		move.w	snap_x,d0
		cmp.b	#$20,(a0,d0.w)
		beq.b	.noleft
.redeb		tst.w	d0
		beq.b	.debs
		cmp.b	#$20,-1(a0,d0.w)
		beq.b	.debs
		cmp.b	#';',-1(a0,d0.w)
		beq.b	.debs
		cmp.b	#'=',-1(a0,d0.w)
		beq.b	.debs
		cmp.b	#'(',-1(a0,d0.w)
		beq.b	.debs
		cmp.b	#',',-1(a0,d0.w)
		beq.b	.debs
		subq.w	#1,d0
		bra.b	.redeb
.debs
		lea.l	snap_buf,a1
.cops		move.b	(a0,d0.w),(a1)+
		addq.w	#1,d0
		cmp.w	#80,d0
		beq.b	.ends
		cmp.b	#',',(a0,d0.w)
		beq.b	.ends
		cmp.b	#';',(a0,d0.w)
		beq.b	.ends
		cmp.b	#')',(a0,d0.w)
		beq.b	.ends
		cmp.b	#$20,(a0,d0.w)
		bne.b	.cops
.ends		sf	(a1)

.noleft
.nos
		movem.l	(a7)+,d0-a6

		subq.w	#1,time_cursor
		bpl.b	no_cursor
		sf	nosnap
		move.w	#15,time_cursor
		bsr.b	set_cursor
.no_cur		rts

set_cursor:	movem.l	d0/a0,-(a7)
		move.l	pic_ptr,a0
		add.w	cursor_x,a0
		move.w	cursor_y,d0
		mulu	#80*h,d0
		add.l	d0,a0
		not.b	cursor_on
		tst.b	no_print
		bne.b	.no
		not.b	(a0)
		not.b	80(a0)
		not.b	160(a0)
		not.b	240(a0)
		not.b	320(a0)
		not.b	400(a0)
		not.b	480(a0)
		not.b	560(a0)
.no		movem.l	(a7)+,d0/a0
no_cursor	rts

clear_cursor:	movem.l	d0/a0,-(a7)
		st	nosnap
		bsr	clear_snap

		move.w	#15,time_cursor
		move.l	pic_ptr,a0
		add.w	cursor_x,a0
		move.w	cursor_y,d0
		mulu	#80*h,d0
		add.l	d0,a0
		tst.b	cursor_on
		beq.b	no_clear_curs
		tst.b	no_print
		bne.b	.no
		not.b	(a0)
		not.b	80(a0)
		not.b	160(a0)
		not.b	240(a0)
		not.b	320(a0)
		not.b	400(a0)
		not.b	480(a0)
		not.b	560(a0)
.no
no_clear_curs	sf	cursor_on
		movem.l	(a7)+,d0/a0
		rts

snap_x:		dc.w 40
snap_y:		dc.w 10
snap_buf:	dcb.b 84,0
snap:		dc.b 0
nosnap:		dc.b 0
		even

clear_snap:	movem.l	d0-d2/a0,-(a7)
		move.w	sr,d2
		move.w	#$2700,sr
		tst.b	snap
		beq.b	.snapoff
		sf	snap
		move.l	pic_ptr,a0
		add.w	snap_x,a0
		move.w	snap_y,d0
		mulu	#80*h,d0
		add.l	d0,a0
		move.b	#$55,d0
		move.b	#$aa,d1
		eor.b	d0,(a0)
		eor.b	d1,80(a0)
		eor.b	d0,160(a0)
		eor.b	d1,240(a0)
		eor.b	d0,320(a0)
		eor.b	d1,400(a0)
		eor.b	d0,480(a0)
		eor.b	d1,560(a0)
.snapoff	move.w	d2,sr
		movem.l	(a7)+,d0-d2/a0
		rts

set_snap:	movem.l	d0-d2/a0,-(a7)
		tst.b	nosnap
		bne.b	.nosnap
		move.w	sr,d2
		move.w	#$2700,sr
		tst.b	snap
		bne.b	.snapon
		st	snap
		move.l	pic_ptr,a0
		add.w	snap_x,a0
		move.w	snap_y,d0
		mulu	#80*h,d0
		add.l	d0,a0
		move.b	#$55,d0
		move.b	#$aa,d1
		eor.b	d0,(a0)
		eor.b	d1,80(a0)
		eor.b	d0,160(a0)
		eor.b	d1,240(a0)
		eor.b	d0,320(a0)
		eor.b	d1,400(a0)
		eor.b	d0,480(a0)
		eor.b	d1,560(a0)
.snapon		move.w	d2,sr
.nosnap		movem.l	(a7)+,d0-d2/a0
		rts

;-------------- Print an hex number -------------------

;-> d0=hex number
;-> d1=nb ascIIs (length of hex number)

print_hex	move.l	a0,-(a7)
		lea.l	general_txt,a0
		bsr.w	conv_hex
		sf	(a0,d1.w)		;set end CHAR
		bsr	print
		move.l	(a7)+,a0
		rts

;-------------- Print an hex number followed by a CR -

;-> d0=hex number
;-> d1=nb ascIIs (length of hex number)

print_hexCR	move.l	a0,-(a7)
		lea.l	general_txt,a0
		bsr.b	conv_hex
		move.b	#$a,(a0,d1.w)		;set CR CHAR
		sf	1(a0,d1.w)		;set end CHAR
		bsr	print
		move.l	(a7)+,a0
		rts

;-------------- Print a decimal number --------------------

;-> d0=dec number
;-> d1=nb ascIIs	;bit7=show zero

print_dec	movem.l	d0/a0,-(a7)
		lea.l	general_txt,a0
		bsr.b	conv_dec
		lea.l	general_txt,a0
		and.w	#$7f,d1
		sf	1(a0,d1.w)		;set end CHAR
		bsr	print
		movem.l	(a7)+,d1/a0
		rts

;-------------- Print a decimal number followed by a CR ---

;-> d0=dec number
;-> d1=nb ascIIs	;bit7=show zero

print_decCR	movem.l	d0/a0,-(a7)
		lea.l	general_txt,a0
		bsr.b	conv_dec
		lea.l	general_txt,a0
		and.w	#$7f,d1
		move.b	#$a,1(a0,d1.w)		;set CR CHAR
		sf	2(a0,d1.w)		;set end CHAR
		bsr	print
		movem.l	(a7)+,d1/a0
		rts

;-------------- Convert hex to ascII ------------------
;-> d0=hex number
;-> d1=nb ascIIs (length of hex number)
;-> a0=ptr on ascII destination buffer

conv_hex	movem.l	d0-d2/a1,-(a7)
		and.w	#$007f,d1		;clear bit 7
		lea.l	hex_list,a1
		subq.w	#1,d1
.do_conv_hex	move.w	d0,d2
		and.w	#$f,d2
		move.b	(a1,d2.w),(a0,d1.w)
		lsr.l	#4,d0
		dbf	d1,.do_conv_hex
		movem.l	(a7)+,d0-d2/a1
		rts

;-------------- Convert dec to ascII ------------------
;d0=dec number
;d1=nb ascIIs		bit7=show zero
;a0=ptr on ascII buffer

conv_dec:	movem.l	d0-d4/a1,-(a7)

		btst	#7,d1
		sne	d3
		and.w	#$003f,d1

		lea.l	dec_list,a1
		neg.w	d1
		add.w	#10,d1
		lsl.w	#2,d1
		add.w	d1,a1

		move.b	#' ',d4
		tst.l	d0
		bpl.b	.no_minus
		neg.l	d0
		move.b	#'-',d4

.no_minus	move.b	#' ',(a0)+
		move.l	d0,d2
.next		moveq	#0,d0
		move.l	(a1)+,d1
		beq.b	.end_dec
.loop		cmp.l	d2,d1
		bgt.b	.ok_dix
		sub.l	d1,d2
		addq.l	#1,d0
		bra.b	.loop
.ok_dix		subq.l	#1,d1
		bne.b	.no_last
		tst.b	d3
		bne.b	.no_last
		st	d3
		move.b	d4,-1(a0)
.no_last	tst.b	d3
		bne.b	.no_first
		st	d3
		tst.b	d0
		beq.b	.empty
		move.b	d4,-1(a0)
		bra.b	.no_first
.empty		sf	d3
		move.b	#' ',d0
		bra.b	.ok_spc
.no_first	cmp.b	#9,d0
		bls.b	.ok_dec
		moveq	#0,d0
.ok_dec		add.b	#'0',d0
.ok_spc		move.b	d0,(a0)+
		bra.b	.next

.end_dec	movem.l	(a7)+,d0-d4/a1
		rts

		cnop 0,4
dec_list	dc.l 1000000000
		dc.l 100000000
		dc.l 10000000
		dc.l 1000000
		dc.l 100000
		dc.l 10000
		dc.l 1000
		dc.l 100
		dc.l 10
		dc.l 1
		dc.l 0

**********************************************************

init_ascII	movem.l	d0-d2/a1-a3,-(a7)
		lea.l	ascII_conv,a1

		move.w	#256-1,d0
		moveq	#0,d1

loop_ia		lea.l	ascII,a2
		move.w	#end_ascII-ascII-1,d2
seek_ascII_i	cmp.b	(a2)+,d1
		dbeq	d2,seek_ascII_i

		lea.l	ascII+1,a3
		sub.l	a3,a2
		move.w	a2,d2
		move.b	d2,(a1)+
		addq.w	#1,d1
		dbf	d0,loop_ia

		movem.l	(a7)+,d0-d2/a1-a3
		rts

**************************************************************************

;save DMA disk buffer to backup_dma
;-> a0=buffer chip address

save_buffer	movem.l	d0-d1/a0-a1,-(a7)
		move.l	a0,buffer
		lea.l	backup_dma,a1
		move.w	#($1a00*2)/4-1,d1
.exg		move.l	(a0)+,(a1)+
		dbf	d1,.exg
		movem.l	(a7)+,d0-d1/a0-a1
		rts

rest_buffer	movem.l	d0-d1/a0-a1,-(a7)
		move.l	buffer,a1
		lea.l	backup_dma,a0
		move.w	#($1a00*2)/4-1,d1
.exg		move.l	(a0)+,(a1)+
		dbf	d1,.exg
		movem.l	(a7)+,d0-d1/a0-a1
		rts

***********************************************************

;-------------- read a single sector from floppy disk -----

;-> d0=no of sector to read
;-> a0=address to copy sector into

read_fsector	movem.l	d0-d7/a0-a4,-(a7)

		move.l	a0,a3

		moveq	#0,d2
		move.w	d0,d2
		divu	#11,d2			;d2=track_no of sector to read
		cmp.w	track_buffer_no,d2	;is track_buffer ok ?
		bne.b	.notsame

.okaccess	lea.l	track_sector,a0
		move.l	d2,d0
		swap	d0
		tst.b	(a0,d0.w)		;sector is in buffer ?
		bne.b	.notsame

		mulu	#512,d0			;offset in track_buffer
		lea.l	track_buffer,a0

		add.l	d0,a0
		moveq	#512/8-1,d0
.copysector	move.l	(a0)+,(a3)+
		move.l	(a0)+,(a3)+
		dbf	d0,.copysector
		bra.b	.okread

.notsame	bsr	write_flush
		move.w	d2,track_buffer_no
		move.w	d2,d0
		mulu	#11,d0
		lea.l	track_sector,a0
		clr.l	(a0)+			;all sectors
		clr.l	(a0)+			;are
		clr.l	(a0)+			;loaded
		lea.l	track_buffer,a0
		moveq	#11,d1			;read 11 sectors (one track)
		moveq	#0,d7			;read command
		bsr	access			;read the track in track_buffer
		beq.b	.okaccess		;no error ?

		moveq	#-1,d0
		move.w	d0,track_buffer_no	;an error occured
		lea.l	track_sector,a0
		move.l	d0,(a0)+		;all sectors
		move.l	d0,(a0)+		;are
		move.l	d0,(a0)+		;NOT loaded

.okread		tst.w	drive_err
		movem.l	(a7)+,d0-d7/a0-a4
		rts

;-------------- write a single sector to floppy disk ------

;-> d0=no of sector to write
;-> a0=address to copy sector from

write_fsector	movem.l	d0-d7/a0-a4,-(a7)

		move.l	a0,a3

		moveq	#0,d2
		move.w	d0,d2
		divu	#11,d2			;d2=track_no of sector to write
		cmp.w	track_buffer_no,d2	;is track_buffer ok ?
		beq.b	.oksame

.notsame	bsr.b	write_flush
		move.w	d2,track_buffer_no
		moveq	#-1,d0
		lea.l	track_sector,a0
		move.l	d0,(a0)+		;all sectors
		move.l	d0,(a0)+		;are
		move.l	d0,(a0)+		;NOT loaded

.oksame		move.l	d2,d0
		swap	d0
		lea.l	track_sector,a0
		sf	(a0,d0.w)		;set sector as loaded in buffer
		mulu	#512,d0
		lea.l	track_buffer,a0
		add.l	d0,a0
		moveq	#512/8-1,d0
.copysector	move.l	(a3)+,(a0)+
		move.l	(a3)+,(a0)+
		dbf	d0,.copysector

		st	flush_needed

		tst.w	drive_err
		movem.l	(a7)+,d0-d7/a0-a4
		rts

;-------------- flush write buffer for floppy disk --------

write_flush	movem.l	d0-d7/a0-a4,-(a7)

		tst.w	track_buffer_no
		bmi.w	.noflush

		tst.b	flush_needed
		beq.w	.noflush

.recheck	lea.l	track_sector,a0
		lea.l	11(a0),a1		;end of sectors list
		moveq	#-1,d2
		moveq	#11-1,d0
.seek		addq.l	#1,d2
		tst.b	(a0)+
		dbne	d0,.seek
		beq.b	.goflush

		moveq	#0,d1			;d1=nb sectors to read
		subq.l	#1,a0
.link		sf	(a0)+			;set sector as loaded
		addq.l	#1,d1
		cmp.l	a1,a0
		beq.b	.okread
		tst.b	(a0)
		bne.b	.link

.okread		cmp.w	#11,d1
		beq.b	.noflush
		lea.l	track_buffer,a0
		move.w	track_buffer_no,d0
		mulu	#11,d0
		add.w	d2,d0
		mulu	#512,d2
		add.l	d2,a0
		moveq	#0,d7			;read command
		bsr	access
		bne.b	.error
		bra.b	.recheck

.goflush	move.w	track_buffer_no,d0
		mulu	#11,d0
		lea.l	track_buffer,a0
		moveq	#11,d1			;write 11 sectors (one track)
		moveq	#-1,d7			;write command
		bsr	access
		beq.b	.noerr

.error		moveq	#-1,d0
		move.w	d0,track_buffer_no	;an error occured
		lea.l	track_sector,a0
		move.l	d0,(a0)+		;all sectors
		move.l	d0,(a0)+		;are
		move.l	d0,(a0)+		;NOT loaded

.noerr
.noflush	sf	flush_needed

		tst.w	drive_err
		movem.l	(a7)+,d0-d7/a0-a4
		rts

;-------------- write multiple sectors to floppy ----------
;-------------- exg pic memory and stop interrupts --------
;-> d0=first sector
;-> d1=nb of sector
;-> a0=address

write		movem.l	d0-d1/a0,-(a7)
		tst.w	d1
		beq.b	.nosave
		bsr	remove_pic
.loop		bsr	write_fsector
		addq.w	#1,d0
		lea.l	512(a0),a0
		subq.w	#1,d1
		bne.b	.loop
		bsr	write_flush
		bsr	set_pic
.nosave		tst.w	drive_err
		movem.l	(a7)+,d0-d1/a0
		rts

;-------------- read multiple sectors from floppy ---------
;-------------- exg pic memory and restore interrupts -----

;-> d0=first sector
;-> d1=nb of sector
;-> a0=address

read		movem.l	d0-d1/a0,-(a7)
		tst.w	d1
		beq.b	.noread
		bsr	remove_pic
.loop		bsr	read_fsector
		addq.w	#1,d0
		lea.l	512(a0),a0
		subq.w	#1,d1
		bne.b	.loop
		bsr	set_pic
.noread		tst.w	drive_err
		movem.l	(a7)+,d0-d1/a0
		rts

;-------------- write multiple sectors to floppy ----------

;-> d0=first sector
;-> d1=nb of sector
;-> a0=address

write2		movem.l	d0-d1/a0,-(a7)
		tst.w	d1
		beq.b	.nosave
.loop		bsr	write_fsector
		addq.w	#1,d0
		lea.l	512(a0),a0
		subq.w	#1,d1
		bne.b	.loop
		bsr	write_flush
.nosave		tst.w	drive_err
		movem.l	(a7)+,d0-d1/a0
		rts

;-------------- read multiple sectors from floppy ---------

;-> d0=first sector
;-> d1=nb of sector
;-> a0=address

read2		movem.l	d0-d1/a0,-(a7)
		tst.w	d1
		beq.b	.noread
.loop		bsr	read_fsector
		addq.w	#1,d0
		lea.l	512(a0),a0
		subq.w	#1,d1
		bne.b	.loop
.noread		tst.w	drive_err
		movem.l	(a7)+,d0-d1/a0
		rts

***********************************************
;d0.w=start sector (if write then d0 MUST be a multiple of 11)
;d1.w=nb sectors   (if write then d1 MUST be a multiple of 11)
;d7: -1=write 0=read
;a0=address to load/save

;if d1=-1 & d7=-1 -> format whole disk

access:		movem.l	d0-a6,-(a7)
		lea.l	$dff000,a6
		st	floppy_op

		ext.l	d0
		ext.l	d1

		move.l	a0,a4			;a4=address of actual sector

		lea.l	$40000,a0
		bsr	save_buffer

		sf	first_sauve
		tst.w	d1
		smi	format
		bpl.b	.no_format
		moveq	#0,d0
		move.w	#22*80,d1
		lea.l	track_sector,a0
		moveq	#-1,d5
		move.l	d5,(a0)+		;all sectors
		move.l	d5,(a0)+		;are NOT
		move.l	d5,(a0)+		;loaded in track_buffer
		move.w	d5,track_buffer_no
.no_format

		move.w	d1,sectpos		;nb sectors to read/write

		cmp.l	#-1,d7			;write requested ?
		seq	writes

		ext.l	d0
		move.l	d0,d5
		divu	#11,d5			;d5.w = start_track
		move.l	d5,d0
		swap	d0
		move.w	d0,startsec    ;nb sectors to skip in 1st track

		lea.l	$bfd100,a0
		move.w	#$8210,$96(a6)

		move.w	$10(a6),d0
		or.w	#$8000,d0
		move.w	d0,OldAdk
		move.w	#$7fff,$9e(a6)
		move.w	#$9100,$9e(a6)
		tst.b	writes
		bne.b	no_sync
		move.w	#$8400,$9e(a6)
no_sync:	move.w	#$4489,$7e(a6)

		bsr	motor_on		;turn drive motor ON
		tst.w	drive_err
		bne.w	fin
		tst.b	writes
		beq.b	.read
		btst	#3,$bfe001		;test if write protected
		bne.b	.read
		move.w	#WRITEPROTECT_ERR,drive_err
		bra.w	fin
.read
		bsr	inittete	;init head pos (track 0) if needed

		move.w	drive,d0
		bclr	d0,(a0)

		move.w	d5,d0
		bsr	settete			;move heads to 1st track

		sf	ctrl			;clear end signal

next:		movem.l	d0-d1/a0/a3,-(a7)	;check if there is a conflict
		move.l	buffer,d0		;between the DMA buffer and
		move.l	d0,d1			;the actual sector accessed
		add.l	#$1a00*2,d1
		lea.l	11*512(a4),a3
		cmp.l	d0,a3
		ble.b	.okbuf
		cmp.l	d1,a4
		bge.b	.okbuf
		bsr	rest_buffer
		move.l	#$50000,d1
		cmp.l	#$40000,d0
		beq.b	.go40
		move.l	#$40000,d1
.go40		move.l	d1,a0
		bsr	save_buffer
.okbuf		movem.l	(a7)+,d0-d1/a0/a3

		tst.b	writes
		beq.b	ok_read
		tst.w	startsec		;if write then 1st sector
		bne.b	fin			;must be a multiple of 11
		cmp.w	#11,sectpos		;write at least 11 sectors
		blt.b	fin
		bsr	sauve
		bra.b	ok_sauve
ok_read:	bsr	charge
		tst.w	drive_err
		bne.b	fin
ok_sauve:	tst.b	ctrl			;end of access ?
		bne.b	fin
		tst.b	break
		bne.b	fin

	;	btst	#5,$bfe001		;removed due to AT 1200 bug !
	;	beq.b	.okspeed
	;	move.w	#NODISK_ERR,drive_err
	;	bra.b	fin

.okspeed	movem.l	d0/a1,-(a7)
		lea.l	track,a1
		move.w	drive,d0
		add.w	d0,d0
		move.w	-3*2(a1,d0.w),d0	;get previous track pos
		addq.w	#1,d0			;step to next track or side
		bsr	settete
		movem.l	(a7)+,d0/a1
		bra.w	next

fin:		bsr	timer2
		bsr	rest_buffer

		move.w	drive,d0
		bset	d0,(a0)			;deselect drive
		bsr	timer1

		move.w	#$7fff,$9e(a6)
		move.w	OldAdk,$9e(a6)

		tst.w	drive_err
		movem.l	(a7)+,d0-a6
		rts

*******************************

motor_on:	movem.l	d0/a0,-(a7)
		lea.l	$bfd100,a0
		move.w	drive,d0
		or.b	#$80,(a0)
		bset	d0,(a0)
		bsr	timer1
		and.b	#$7f,(a0)
		bsr	timer1
		bclr	d0,(a0)
		bsr	timer1

		btst	d0,mot_on
		bne.b	.already_on

		move.b	#0,$bfe801

.motor_wait	cmp.b	#30,$bfe801		;wait > 500 ms
		bls.b	.motor_wait

		bset	d0,mot_on

.already_on	movem.l	(a7)+,d0/a0
		rts

;Old code removed due to AT 1200 bug !

;motor_wait:	cmp.b	#80,$bfe801
;		bhi.b	.no_mot_on
;		btst	#5,$bfe001
;		bne.b	motor_wait
;.out		movem.l	(a7)+,d0/a0
;		rts

;.no_mot_on	move.w	#NODISK_ERR,drive_err
;		bra.b	.out

*******************************

motor_off:	bsr.w	rest_head
		movem.l	d0/a0,-(a7)
		lea.l	$bfd100,a0
		move.w	drive,d0
		bset	d0,(a0)
		bsr	timer1
		or.b	#$80,(a0)
		bsr	timer1
		bclr	d0,(a0)
		bsr	timer1
		bset	d0,(a0)
		bsr	timer1
		bclr	d0,mot_on
		movem.l	(a7)+,d0/a0
		rts

*******************************

;>0.6 ms

timer1		movem.l	d0-d1/a6,-(a7)
		lea.l	$dff000,a6
		moveq	#10-1,d0
		cmp.b	#2,config_screen
		bne.b	.loop
		moveq	#10*2-1,d0
.loop		move.b	$6(a6),d1
.w1		cmp.b	$6(a6),d1
		beq.b	.w1
		dbf	d0,.loop
		movem.l	(a7)+,d0-d1/a6
		rts


;>3 ms

timer2		movem.l	d0-d1/a6,-(a7)
		lea.l	$dff000,a6
		moveq	#50-1,d0
		cmp.b	#2,config_screen
		bne.b	.loop
		moveq	#50*2-1,d0
.loop		move.b	$6(a6),d1
.w1		cmp.b	$6(a6),d1
		beq.b	.w1
		dbf	d0,.loop
		movem.l	(a7)+,d0-d1/a6
		rts

;>18 ms

timer3		movem.l	d0-d1/a6,-(a7)
		lea.l	$dff000,a6
		move.w	#284-1,d0
		cmp.b	#2,config_screen
		bne.b	.loop
		move.w	#284*2-1,d0
.loop		move.b	$6(a6),d1
.w1		cmp.b	$6(a6),d1
		beq.b	.w1
		dbf	d0,.loop
		movem.l	(a7)+,d0-d1/a6
		rts

*******************************

step_head:	bclr	#0,(a0)
		bsr	timer1
		bset	#0,(a0)
		bsr	timer2
		rts

*******************************

;-------------- check which drive is present ----

test_drive	movem.l	d0-d3/a0-a2,-(a7)
		lea.l	$bfd100,a0
		lea.l	$bfe001,a1
		move.b	#$ff,(a0)
		lea.l	drive_present,a2
		st	(a2)+			;DF0: always present

		moveq	#4,d0			;drive1 SEL

		moveq	#3-1,d3			;3 drives to test

.next_drive	and.b	#$7f,(a0)
		bsr	timer1
		bclr	d0,(a0)			;switch drive ON
		bsr	timer1
		bset	d0,(a0)
		bsr	timer1
		or.b	#$80,(a0)
		bclr	d0,(a0)			;switch drive OFF
		bsr	timer1

		moveq	#0,d1			;d1=drive ID
		moveq	#32-1,d2
.loop		add.l	d1,d1
		btst	#5,(a1)			;one bit of ID in RDY
		beq.b	.zero
		addq.l	#1,d1			;read drive ID
.zero		bset	d0,(a0)			;pulse SEL
		bsr	timer1
		bclr	d0,(a0)
		bsr	timer1
		dbf	d2,.loop

		bset	d0,(a0)			;deselect drive
		bsr	timer1

		tst.l	d1
		seq	(a2)+
		addq.w	#1,d0			;next drive
		dbf	d3,.next_drive

		movem.l	(a7)+,d0-d3/a0-a2
		rts

******************************

;-------------- init head pos -------------------

inittete:	movem.l	d0-d1/a0-a1,-(a7)
		lea.l	$bfd100,a0
		move.w	drive,d0
		move.w	d0,d1
		add.w	d1,d1
		btst	d0,init		;test if the selected drive was inited
		bne.b	.no_init
		bclr	d0,(a0)
		lea.l	old_head,a1
		clr.w	-3*2(a1,d1.w)	;save old position of head here
		bset	#1,(a0)
		bsr	timer1

.cont00		btst	#4,$bfe001
		beq.b	.track00
		bsr.w	step_head
		addq.w	#2,-3*2(a1,d1.w)
		bra.b	.cont00

.track00	lea.l	track,a1
		clr.w	-3*2(a1,d1.w)	;current head pos = 0
		bset	d0,init
		bset	d0,(a0)

.no_init	movem.l	(a7)+,d0-d1/a0-a1
		rts

rest_head	movem.l	d0-d1/a1,-(a7)
		move.w	drive,d0
		btst	d0,init
		beq.b	.no_rest
		lea.l	$bfd100,a0
		bclr	d0,(a0)
		bsr	timer1
		bclr	d0,init
		lea.l	old_head,a1
		add.w	d0,d0
		move.w	-3*2(a1,d0.w),d0
		bsr	timer1
		bsr	settete
		move.w	drive,d0
		lea.l	track,a1
		add.w	d0,d0
		move.w	#-1,-3*2(a1,d0.w)
		bset	d0,(a0)
.no_rest	movem.l	(a7)+,d0-d1/a1
		rts

*******************************

;d0=track to reach (0-159)

settete:	movem.l	d0-d4,-(a7)
		sf	d4
		bsr	timer1
		move.l	a1,-(a7)
		lea.l	track,a1
		move.w	drive,d1
		add.w	d1,d1
		move.w	-3*2(a1,d1.w),d1	;get old pos of heads
		move.l	(a7)+,a1
		move.w	d0,d3			;new pos
		asr.w	#1,d0
		asr.w	#1,d1
		cmp.w	d0,d1

		blt.b	moinstete
		bset	#1,(a0)			;set dir
		sub.w	d0,d1
		move.w	d1,d0
		moveq	#-2,d2
		bra.b	go_dbf

moinstete:	bclr	#1,(a0)			;set dir
		sub.w	d1,d0
		moveq	#2,d2
		bra.b	go_dbf

loop_set:	bsr.w	step_head
		st	d4

go_dbf:		dbf	d0,loop_set

		move.l	a1,-(a7)
		move.w	drive,d0
		lea.l	track,a1
		add.w	d0,d0
		move.w	d3,-3*2(a1,d0.w)	;set new pos of heads
		move.l	(a7)+,a1
		and.w	#1,d3
		bne.b	impair
		bset	#2,(a0)
		bra.b	pair
impair:		bclr	#2,(a0)
pair:		bsr	timer2

		tst.b	d4
		beq.b	.nomove
		bsr	timer3
.nomove
		movem.l	(a7)+,d0-d4
		rts

***************************************

charge:		movem.l	d0/d7/a3,-(a7)
		moveq	#5-1,d7
.loop		move.w	#$4000,$24(a6)
		bsr	timer1
		move.l	buffer,a3
		move.l	a3,$20(a6)
		move.w	#$9a00,d0		;len=$1a00*2=$3400
		move.w	#2,$9c(a6)
		move.w	d0,$24(a6)
		move.w	d0,$24(a6)

		move.b	#0,$bfe801

.waitdmad	cmp.b	#100,$bfe801		;wait no more than 2s
		bhi.b	.error
		move.w	$1e(a6),d0
		and.w	#2,d0
		beq.b	.waitdmad

		move.w	#$4000,$24(a6)

		bsr.b	dmfm
		beq.b	.out
		dbf	d7,.loop		;retry

.error		move.w	#TRACKCORRUPT_ERR,drive_err	;track corrupted
.out		movem.l	(a7)+,d0/d7/a3
		rts

***************************************

dmfm:		movem.l	d0-d7/a0-a2,-(a7)
		move.l	#$55555555,d7

		move.l	a4,a0				;save old status
		move.w	sectpos,oldsectpos
		move.w	startsec,oldstartsec

		moveq	#11-1,d4
		moveq	#0,d3
		move.l	a3,a1
		lea.l	$1a00*2(a1),a1

nextsector:	move.l	a3,a2

seek:		cmp.l	a1,a2
		bge.w	mfm_error
		cmp.w	#$4489,(a2)+
		bne.b	seek
		cmp.w	#$4489,(a2)
		beq.b	seek
		move.b	2(a2),D0
		move.b	6(a2),D1
		and.b	d7,d0
		and.b	d7,d1
		add.b	d0,d0
		or.b	d1,d0
		cmp.b	d0,d3
		beq.b	oksec
		lea.l	$43e(a2),a2
		bra.b	seek
oksec:		addq.l	#1,d3

		lea.l	$0028(a2),a2

		move.l	(a2)+,d0
		move.l	(a2)+,d1
		and.l	d7,d0
		and.l	d7,d1
		add.l	d0,d0
		or.l	d1,d0
		move.l	d0,d6

		move.l	a2,-(a7)
		lea.l	-$30(a2),a2

		moveq	#0,d0
		moveq	#$0a-1,d5
.nextcheck:	move.l	(a2)+,d1
		eor.l	d1,d0
		dbf	d5,.nextcheck

		move.l	(a7)+,a2

		and.l	d7,d0
		cmp.l	d6,d0		;test header checksum
		bne.w	mfm_error

		move.l	(a2)+,d0
		move.l	(a2)+,d1
		and.l	d7,d0
		and.l	d7,d1
		add.l	d0,d0
		or.l	d1,d0
		move.l	d0,d6

		move.l	a2,-(a7)

		moveq	#0,d0
		move.w	#$100/4-1,d5
.nextcheck2:	move.l	(a2)+,d1
		eor.l	d1,d0
		move.l	(a2)+,d1
		eor.l	d1,d0
		move.l	(a2)+,d1
		eor.l	d1,d0
		move.l	(a2)+,d1
		eor.l	d1,d0
		dbf	d5,.nextcheck2

		move.l	(a7)+,a2

		and.l	d7,d0
		cmp.l	d6,d0		;test data checksum
		bne.b	mfm_error

		tst.w	sectpos		;all sectors loaded ?
		beq.b	pasokload
		tst.w	startsec;have we reached the 1st sector to load ?
		beq.b	okloadsec
		subq.w	#1,startsec
		bra.b	pasokload

okloadsec:	moveq	#$80-1,d5
decodemfm:	move.l	$200(a2),d1
		move.l	(a2)+,d0
		and.l	d7,d0
		and.l	d7,d1
		add.l	d0,d0
		or.l	d1,d0
		move.l	d0,(a4)+
		dbf	d5,decodemfm

		subq.w	#1,sectpos	;nb sectors to load --
		bne.b	pasokload
		st	ctrl		;set end signal
		bra.b	reload

pasokload:	dbf	d4,nextsector

reload:		movem.l	(a7)+,d0-d7/a0-a2
		moveq	#0,d0			;ok
		rts

mfm_error:	move.w	oldsectpos,sectpos	;restore status
		move.w	oldstartsec,startsec
		move.l	a0,a4
		movem.l	(a7)+,d0-d7/a0-a2
		moveq	#-1,d0			;an error occured
		rts

***************************************************

sauve:
		bsr.b	code_mfm

		move.w	#$4000,$24(a6)
		bsr	timer2
		move.l	buffer,a3
		move.l	a3,$20(a6)
		move.w	#$c000+$220*11+$2a0,d0
		move.w	#2,$9c(a6)
		move.w	d0,$24(a6)
		move.w	d0,$24(a6)

waitdmad2:	move.w	$1e(a6),d0
		and.w	#2,d0
		beq.b	waitdmad2

		move.w	#$4000,$24(a6)

		bsr	timer2

		rts

***********************************
;Code track
;-> a4=data to be coded

code_mfm:	movem.l	a0-a3/d0-d7,-(a7)

		lea.l	track,a3
		move.w	drive,d0
		add.w	d0,d0
		move.w	-3*2(a3,d0.w),d0	;get track no

		move.l	buffer,a3

		move.l	#$55555555,d5
		move.l	d5,d6
		add.l	d6,d6			;d6=$aaaaaaaa

		tst.b	format
		beq.b	.noformat
		tst.b	first_sauve
		bne.b	.fast

.noformat	move.w	#$1a00*2/16-1,d1
.filla		move.l	d6,(a3)+
		move.l	d6,(a3)+
		move.l	d6,(a3)+
		move.l	d6,(a3)+
		dbf	d1,.filla

.fast		move.l	buffer,a3
		lea.l	$1a00*2-$440*11-$20(a3),a3

		bsr.b	do_track

		st	first_sauve
		movem.l	(a7)+,a0-a3/d0-d7
		rts

*******************************
;-> a3=ptr on dest track
;-> a4=ptr on data to write
;-> d0=no of track

do_track:	move.b	d0,header+1
		move.w	#$000b,header+2
		moveq	#11-1,d4
next_sec:	bsr.b	code_sector
		lea.l	$220*2(a3),a3		;go next sector
		add.w	#$00ff,header+2		;inc actual sec, dec sec left
		subq.w	#1,sectpos
		dbf	d4,next_sec

		tst.w	sectpos
		bne.b	.noend
		st	ctrl			;set end signal
.noend		rts

*******************************

code_sector	move.l	a3,a0
		addq.l	#4,a0
		move.l	#$44894489,(a0)+	;sync
		move.l	header,d0
		bsr.w	code

		lea.l	8(a3),a0
		moveq	#10,d1
		bsr.w	chk_sum
		bsr.b	code

		tst.b	format
		beq.b	.no_format
		lea.l	track_buffer,a4
		tst.b	first_sauve
		bne.b	.out
		moveq	#0,d0
		move.w	#512/16-1,d7
.clear		move.l	d0,(a4)+
		move.l	d0,(a4)+
		move.l	d0,(a4)+
		move.l	d0,(a4)+
		dbf	d7,.clear
		lea.l	track_buffer,a4
		move.l	#$444f5301,(a4)		;'DOS',1 = FFS
.no_format
		addq.l	#8,a0
		moveq	#$80-1,d7
.code_sec:	move.l	(a4)+,d0

		move.l	d0,d3
		lsr.l	#1,d0
		bsr.w	coder2

		lea.l	$200-4(a0),a0
		move.l	d3,d0
		bsr.w	coder2
		lea.l	-$200(a0),a0

		dbf	d7,.code_sec

		lea.l	$40(a3),a0
		move.w	#$100,d1
		bsr.w	chk_sum2
		lea.l	$38(a3),a0
		bsr.b	code
.out		rts

*******************************
; a0 = address du buffer code
; d0 = long word to code

code:		movem.l	d2/d3,-(a7)
		move.l	d0,d3
		lsr.l	#1,d0
		bsr.b	coder
		move.l	d3,d0
		bsr.b	coder2
		movem.l	(a7)+,d2/d3
		rts	
 
*******************************

coder:		and.l	d5,d0
		move.l	d0,d2
		eor.l	d5,d2
		move.l	d2,d1
		add.l	d2,d2
		lsr.l	#1,d1
		bset	#31,d1
		and.l	d2,d1
		or.l	d1,d0
		btst	#0,-1(a0)
		beq.b	pas_bit
		bclr	#31,d0
pas_bit:	move.l	d0,(a0)+
		rts

*******************************

chk_sum:	move.l	d2,-(a7)
		subq.w	#1,d1
		moveq	#0,d0
l_chks:		move.l	(a0)+,d2
		eor.l	d2,d0
		dbf	d1,l_chks
		and.l	d5,d0
		move.l	(a7)+,d2
		rts

chk_sum2:	move.l	d2,-(a7)
		subq.w	#1,d1
		moveq	#0,d0
l_chks2:	move.l	(a0)+,d2
		eor.l	d2,d0
		dbf	d1,l_chks2
		move.l	(a7)+,d2
		and.l	d5,d0
		rts

*******************************

coder2:		and.l	d5,d0
		move.l	d0,d2
		eor.l	d5,d2
		move.l	d2,d1
		add.l	d2,d2
		lsr.l	#1,d1
		bset	#31,d1
		and.l	d2,d1
		or.l	d1,d0
		btst	#0,-1(a0)
		beq.b	pas_bitx
		bclr	#31,d0
pas_bitx:	move.l	d0,(a0)+

		move.b	(a0),d1
		btst	#0,d0
		bne.b	bit0
		btst	#6,d1
		bne.b	bit6
		bset	#7,d1
		bra.b	go7

bit0:		bclr	#7,d1
go7:		move.b	d1,(a0)
bit6:		rts

**************************************************************************
**************************************************************************

;----------------------------------------------------------

NODISK_ERR		equ 1
TRACKCORRUPT_ERR	equ 2
BADCHECKSUM_ERR		equ 3
NOTDOS_ERR		equ 4
WRITEPROTECT_ERR	equ 5
CREATEFILE_ERR		equ 6
FILENOTFOUND_ERR	equ 7
DISKFULL_ERR		equ 8
FILEEXIST_ERR		equ 9
DEVICENOTFOUND_ERR	equ 10
ILLEGALPATH_ERR		equ 11
NOFFS_ERR		equ 12
IDE_ERR			equ 13
NOTEMPTY_ERR		equ 14

		STRUCTURE partition,0
		ULONG part_next		;ptr on next partition
		ULONG part_first	;first sector of this partition
		ULONG part_nbsec	;nb sectors in this partition
		ULONG part_device	;ptr on device
		UWORD part_unit		;unit number
		ULONG part_filesystem	;filesystem ID (DOS\0,....) 0=nodisk
		STRUCT part_name,32	;partition_name (BCPL) (upper case)
		STRUCT part_name2,32	;partition_name (BCPL) (original case)
		LABEL part_SIZEOF

		STRUCTURE file_handle,0
		ULONG file_part		;ptr on partition
		ULONG file_parent	;parent
		ULONG file_header
		ULONG file_extension
		ULONG file_extpos
		ULONG file_size
		ULONG file_seek
		STRUCT file_name,32	;1st byte=len of name (max 30) (BCPL)
		UBYTE file_written
		UBYTE file_dir		;0=it is a file, -1 it is a dir
					;(used by ExNext only)
		LABEL file_SIZEOF


;-------------- device is accesed by open,read,write,close,seek... cmd ------

READ_CMD equ 0
WRITE_CMD equ 4
UPDATE_CMD equ 8
MOTOFF_CMD equ 12
CHANGE_CMD equ 16
READM_CMD equ 20
WRITEM_CMD equ 24

	OPT_OFF
floppy_device	bra.w	floppy_read
		bra.w	floppy_write
		bra.w	floppy_update
		bra.w	floppy_motoff
		bra.w	floppy_change
		bra.w	floppy_readm
		bra.w	floppy_writem
	OPT_ON

;-> a3=ptr on part
;-> d0=no of first block to read
;-> d1=nb of block to read
;-> on stack=ptr on blocks

floppy_readm	movem.l	d0-d1/a0/a4,-(a7)
		lsl.w	#2,d1
		lea.l	4+4*4(a7,d1.w),a4
		lsr.w	#2,d1
		bra.b	.godbf
.loop		move.l	-(a4),a0
		bsr	floppy_read
		addq.l	#1,d0
.godbf		dbf	d1,.loop
		movem.l	(a7)+,d0-d1/a0/a4
		lsl.w	#2,d1
		move.l	(a7),(a7,d1.w)
		lea.l	(a7,d1.w),a7
		lsr.w	#2,d1
		rts

;-> a3=ptr on part
;-> d0=no of first block to write
;-> d1=nb of block to write
;-> on stack=ptr on blocks

floppy_writem	movem.l	d0-d1/a0/a4,-(a7)
		lsl.w	#2,d1
		lea.l	4+4*4(a7,d1.w),a4
		lsr.w	#2,d1
		bra.b	.godbf
.loop		move.l	-(a4),a0
		bsr	floppy_write
		addq.l	#1,d0
.godbf		dbf	d1,.loop
		movem.l	(a7)+,d0-d1/a0/a4
		lsl.w	#2,d1
		move.l	(a7),(a7,d1.w)
		lea.l	(a7,d1.w),a7
		lsr.w	#2,d1
		rts

;-> a3=ptr on part
;-> d0=no of block to read
;-> a0=buffer ptr

floppy_read	move.l	d0,-(a7)
		add.l	part_first(a3),d0
		cmp.w	#0,part_unit(a3)		;DF0 ?
		bne.b	.no0
		cmp.w	#3,drive
		beq.b	.read
		bsr	write_flush
		move.w	#3,drive
		move.l	d0,-(a7)
		moveq	#1,d0				;force floppy change
		bsr	floppy_change
		move.l	(a7)+,d0
		bra.b	.read

.no0		cmp.w	#1,part_unit(a3)		;DF1 ?
		bne.b	.no1
		cmp.w	#4,drive
		beq.b	.read
		bsr	write_flush
		move.w	#4,drive
		move.l	d0,-(a7)
		moveq	#1,d0				;force floppy change
		bsr	floppy_change
		move.l	(a7)+,d0
		bra.b	.read
.no1		bra.b	.err

.read		bsr	read_fsector
.err		move.l	(a7)+,d0
		rts

;-> a3=ptr on part
;-> d0=no of block to write
;-> a0=buffer ptr

floppy_write	move.l	d0,-(a7)

		bsr	signal_write

		add.l	part_first(a3),d0
		cmp.w	#0,part_unit(a3)		;DF0 ?
		bne.b	.no0
		cmp.w	#3,drive
		beq.b	.write
		bsr	write_flush
		move.w	#3,drive
		move.l	d0,-(a7)
		moveq	#1,d0				;force floppy change
		bsr	floppy_change
		move.l	(a7)+,d0
		bra.b	.write

.no0		cmp.w	#1,part_unit(a3)		;DF1 ?
		bne.b	.no1
		cmp.w	#4,drive
		beq.b	.write
		bsr	write_flush
		move.w	#4,drive
		move.l	d0,-(a7)
		moveq	#1,d0				;force floppy change
		bsr	floppy_change
		move.l	(a7)+,d0
		bra.b	.write
.no1		bra.b	.err

.write		bsr	write_fsector
.err		move.l	(a7)+,d0
		rts

;-> a3=ptr on part

floppy_update	bsr	write_flush
		rts

;-> a3=ptr on part

floppy_motoff	cmp.w	#0,part_unit(a3)		;DF0 ?
		bne.b	.no0
		cmp.w	#3,drive
		beq.b	.motoff
		bsr	write_flush
		move.w	#3,drive
		move.l	d0,-(a7)
		moveq	#1,d0				;force floppy change
		bsr	floppy_change
		move.l	(a7)+,d0
		bra.b	.motoff

.no0		cmp.w	#1,part_unit(a3)		;DF1 ?
		bne.b	.no1
		cmp.w	#4,drive
		beq.b	.motoff
		bsr	write_flush
		move.w	#4,drive
		move.l	d0,-(a7)
		moveq	#1,d0				;force floppy change
		bsr	floppy_change
		move.l	(a7)+,d0
		bra.b	.motoff
.no1		bra.b	.err

.motoff		bsr	motor_off
.err		rts

;-> a3=ptr on part
;-> d0=cmd 0=test if changed 1=force change
;<- d0 0=nochange -1=nodisk 1=newdisk

floppy_change	movem.l	a0,-(a7)
		tst.l	d0
		bne.w	.force
		cmp.w	#0,part_unit(a3)		;DF0 ?
		bne.b	.no0
		cmp.w	#3,drive
		beq.w	.gochg
		bsr	write_flush
		move.w	#3,drive
		movem.l	d0/a0,-(a7)
		lea.l	track_sector,a0
		moveq	#-1,d0
		move.l	d0,(a0)+
		move.l	d0,(a0)+
		move.l	d0,(a0)+
		move.w	d0,track_buffer_no
		movem.l	(a7)+,d0/a0
		bra.b	.gochg

.no0		cmp.w	#1,part_unit(a3)		;DF1 ?
		bne.b	.no1
		cmp.w	#4,drive
		beq.b	.gochg
		bsr	write_flush
		move.w	#4,drive
		movem.l	d0/a0,-(a7)
		lea.l	track_sector,a0
		moveq	#-1,d0
		move.l	d0,(a0)+
		move.l	d0,(a0)+
		move.l	d0,(a0)+
		move.w	d0,track_buffer_no
		movem.l	(a7)+,d0/a0
		bra.b	.gochg

.no1		bra.w	.err

.force		lea.l	track_sector,a0
		moveq	#-1,d0
		move.l	d0,(a0)+
		move.l	d0,(a0)+
		move.l	d0,(a0)+
		move.w	d0,track_buffer_no
		moveq	#1,d0			;new disk
		bra.b	.out

.gochg		lea.l	$bfd100,a0
		move.w	drive,d0
		bclr	d0,(a0)			;select drive
		bclr	#1,(a0)			;step dir -> 80
		bsr	timer1
		btst	#2,$bfe001		;/CHNG ?
		bne.b	.nochg

		bsr	step_head
		bset	#1,(a0)			;step dir -> 00
		bsr	timer1
		bsr	step_head
		bsr	timer1
		btst	#2,$bfe001
		bne.b	.newdisk
		bset	d0,(a0)			;deselect drive
		move.w	#NODISK_ERR,drive_err
		moveq	#-1,d0			;no disk
		bra.b	.out

.newdisk	bset	d0,(a0)			;deselect drive
		lea.l	track_sector,a0
		moveq	#-1,d0
		move.l	d0,(a0)+
		move.l	d0,(a0)+
		move.l	d0,(a0)+
		move.w	d0,track_buffer_no
		moveq	#1,d0			;new disk
		bra.b	.out

.nochg		bset	d0,(a0)			;deselect drive
		moveq	#0,d0			;no change
		bra.b	.out

.err		moveq	#1,d0
.out		movem.l	(a7)+,a0
		rts


	OPT_OFF
ide_device	bra.w	ide_read
		bra.w	ide_write
		bra.w	ide_update
		bra.w	ide_motoff
		bra.w	ide_change
		bra.w	ide_readm
		bra.w	ide_writem
	OPT_ON

;-> a3=ptr on part
;-> d0=no of first block to read
;-> d1=nb of block to read
;-> on stack=ptr on blocks

ide_readm	movem.l	d0-d1/a0/a4,-(a7)
		lsl.w	#2,d1
		lea.l	4+4*4(a7,d1.w),a4
		lsr.w	#2,d1
		bra.b	.godbf
.loop		move.l	-(a4),a0
		bsr	ide_read
		addq.l	#1,d0
.godbf		dbf	d1,.loop
		movem.l	(a7)+,d0-d1/a0/a4
		lsl.w	#2,d1
		move.l	(a7),(a7,d1.w)
		lea.l	(a7,d1.w),a7
		lsr.w	#2,d1
		rts

;-> a3=ptr on part
;-> d0=no of first block to write
;-> d1=nb of block to write
;-> on stack=ptr on blocks


ide_writem	movem.l	d0-d2/a0,-(a7)

		bsr	signal_write

		add.l	part_first(a3),d0

		lsl.w	#2,d1
		lea.l	4+4*4(a7,d1.w),a0
		lsr.w	#2,d1

		move.w	d1,d2
		move.w	part_unit(a3),d1
		bsr	WriteM_Block

		tst.l	d0
		beq.b	.noerr
.err		move.w	#IDE_ERR,drive_err
.noerr
		movem.l	(a7)+,d0-d2/a0
		add.w	d1,d1
		add.w	d1,d1
		move.l	(a7),(a7,d1.w)
		lea.l	(a7,d1.w),a7
		lsr.w	#2,d1
		rts

;-> a3 = part

signal_write	movem.l	d2/a0/a3,-(a7)
		lea.l	changed_disk,a0
		moveq	#16-1,d2
.seek		cmp.l	(a0)+,a3
		beq.b	.found
		dbf	d2,.seek

		lea.l	changed_disk,a0
.seek2		tst.l	(a0)+
		bne.b	.seek2
		move.l	a3,-4(a0)

.found		movem.l	(a7)+,d2/a0/a3
		rts


;-> d0=block no
;-> a0=address
;-> a3=part

ide_read	movem.l	d0-d1,-(a7)
		cmp.l	part_nbsec(a3),d0
		bge.b	.err
		add.l	part_first(a3),d0
		move.w	part_unit(a3),d1
		bsr	Read_Block
		tst.l	d0
		beq.b	.noerr
.err		move.w	#IDE_ERR,drive_err
.noerr		movem.l	(a7)+,d0-d1
		rts

;-> d0=block no
;-> a0=address
;-> a3=part

ide_write	movem.l	d0-d1,-(a7)

		bsr	signal_write

		cmp.l	part_nbsec(a3),d0
		bge.b	.err
		add.l	part_first(a3),d0
		move.w	part_unit(a3),d1
		bsr	Write_Block
		tst.l	d0
		beq.b	.noerr
.err		move.w	#IDE_ERR,drive_err
.noerr		movem.l	(a7)+,d0-d1
		rts

ide_update	rts
ide_motoff	rts
ide_change	moveq	#0,d0		;no change
		rts

;-------------- find the partitions on the HardDisks ------

find_part	movem.l	d0-a4,-(a7)

		lea.l	floppy0,a1
.seekend	move.l	(a1),d1
		beq.b	.endoflist
		move.l	d1,a1
		bra.b	.seekend

.endoflist	lea.l	secbuf,a0
		moveq	#0,d1
		bsr	Read_ID
		tst.l	d0
		bne.b	.nodrive0
		bsr	.search_part
.nodrive0
		tst.b	config_elsat
		bne.b	.nodrive1
		lea.l	secbuf,a0
		moveq	#1,d1
		bsr	Read_ID
		tst.l	d0
		bne.b	.nodrive1
		bsr	.search_part
.nodrive1
		lea.l	secbuf,a0
		moveq	#0,d1
		moveq	#0,d0
		tst.w	IDE_info0
		beq.b	.no0
		bsr	Read_Block
.no0
		movem.l	(a7)+,d0-a4
		rts

;-------------- search partitions in rigid disk blocks ----

;-> d1=drive no (unit)
;-> a1=last partition

.search_part
		moveq	#0,d6
.seek		lea.l	secbuf,a0
		move.l	d6,d0
		bsr	Read_Block	;read first RDSK
		cmp.l	#"RDSK",(a0)
		beq.b	.okrdsk
		addq.l	#1,d6
		cmp.l	#RDB_LOCATION_LIMIT,d6
		beq.w	.error
		bra.b	.seek
.okrdsk		move.l	rdb_CylBlocks(a0),d6
		move.l	rdb_PartitionList(a0),d0
		beq.w	.error
.read_next	moveq	#-1,d2
		cmp.l	d2,d0		;end of list ?
		beq.w	.error
		bsr	Read_Block	;read PART
		cmp.l	#"PART",(a0)
		bne.w	.error
		move.l	192(a0),d0
		lsr.l	#8,d0
		cmp.l	#"DOS",d0	;Amiga DOS filesystem ?
		bne.w	.next

		lea.l	part_SIZEOF(a1),a2
		cmp.l	#harddisk_end,a2	;max nb of part reached ?
		bge.w	.error

		move.l	a2,part_next(a1)
		move.l	a2,a1
		clr.l	part_next(a1)

		lea.l	pb_DriveName(a0),a2
		lea.l	part_name(a1),a3
		lea.l	part_name2(a1),a4
		clr.l	(a3)		;clear DF1: name if needed
		moveq	#0,d2
		move.b	(a2)+,d2
		move.b	d2,(a3)+	;copy name length
		move.b	d2,(a4)+
		bra.b	.godbf2
.copyn		move.b	(a2)+,d0
		move.b	d0,(a4)+
		bsr	upper_case
		move.b	d0,(a3)+	;copy Partition name
.godbf2		dbf	d2,.copyn

		move.l	$a4(a0),d0	;First cylinder
		mulu	d6,d0		;d0=first sector
		move.l	d0,part_first(a1)
		move.l	$a8(a0),d2	;last cylinder
		addq.w	#1,d2
		mulu	d6,d2		;last sector+1
		sub.l	d0,d2
		move.l	d2,part_nbsec(a1)
		move.w	d1,part_unit(a1)

		move.l	#ide_device,part_device(a1)

		lea.l	extension,a0
		bsr	Read_Block	;Read first sector
		move.l	(a0),part_filesystem(a1)
		lea.l	secbuf,a0

.next		move.l	pb_Next(a0),d0
		bne.w	.read_next

.error		rts

;----------------------------------------------------------

ide_base	dc.l 0		;$da2000 / $dd2020 / $EB8000 (A1200,A4000,CD32elsat)
ide_irq		dc.l 0		;$da9000 / $dd3020 / $0

ide_data	dc.w 0
ide_error	dc.w 0
ide_feature	dc.w 0
ide_secnt	dc.w 0
ide_secnb	dc.w 0
ide_cyllo	dc.w 0
ide_cylhi	dc.w 0
ide_dhead	dc.w 0
ide_status	dc.w 0
ide_command	dc.w 0
ide_altstat	dc.w 0

;$da8000+$1000 interrupt register

;-> a0=ID_Buffer
;-> d1=drive no (0/1)
;<- d0=error 0=ok -1=an error occured (No Drive)

Read_ID		movem.l	d1-d2/a0-a1/a3,-(a7)
		lea.l	IDE_info0,a1
		tst.w	d1
		beq.b	.okdrive0
		lea.l	IDE_info1,a1
.okdrive0
		move.l	ide_base(pc),a3

		move.b	#0,$bfe801
.wait		cmp.b	#80,$bfe801
		bhi.w	.error
		move.w	ide_status,d0
		tst.b	(a3,d0.w)
		bmi.b	.wait			;BUSY ?

		lsl.b	#4,d1
		and.b	#$10,d1			;Drive bit
		or.b	#$a0,d1
		move.w	ide_dhead,d0
		move.b	d1,(a3,d0.w)
		move.w	ide_command,d0
		move.b	#$EC,(a3,d0.w)		;Identify Drive cmd

		bsr	wait_irq2
		bne.b	.error

		move.w	ide_status,d0
		move.b	(a3,d0.w),d0
		btst	#0,d0
		bne.b	.error

		movem.l	a0/a1,-(a7)
		move.w	ide_data,d0
		lea.l	(a3,d0.w),a1
		move.w	#512/2-1,d0
.copy		move.w	(a1),d1
		ror.w	#8,d1			;LittleEndian to BigEndian
		move.w	d1,(a0)+
		dbf	d0,.copy
		movem.l	(a7)+,a0/a1

		move.w	1*2(a0),II_NbCyl(a1)
		move.w	3*2(a0),II_NbHeads(a1)
		move.w	6*2(a0),II_NbSecTrack(a1)
		moveq	#0,d1
		move.w	59*2(a0),d0
		btst	#8,d0			;setting valid ?
		beq.b	.nomult
		move.b	d0,d1
.nomult		move.w	d1,II_MultSize(a1)

		move.w	ide_status,d0
		move.b	(a3,d0.w),d0
		btst	#0,d0
		bne.b	.error
		moveq	#0,d0
		bra.b	.out
.error		moveq	#-1,d0
.out		movem.l	(a7)+,d1-d2/a0-a1/a3
		rts

;-> a3=ide_base
wait_irq	movem.l	d0/a0,-(a7)
		tst.l	ide_irq
		bne.b	.nocd32
		move.w	ide_altstat,d0
.busy		tst.b	(a3,d0.w)
		bmi.b	.busy
		move.w	ide_status,d0
		move.b	(a3,d0.w),d0		;ack int
		bra.b	.ok
.nocd32
.noirq		move.l	ide_irq(pc),a0
		move.b	(a0),d0
		bpl.b	.noirq
		tst.b	config_A1200
		bne.b	.go1200
		move.w	ide_status,d0
		move.b	(a3,d0.w),d0
		bra.b	.ok
.go1200		move.w	SR,-(a7)
		move.w	#$2700,sr
		move.w	ide_status,d0
		move.b	(a3,d0.w),d0
		move.b	(a0),d0
		andi.b	#3,d0
		or.b	#$7c,d0
		move.b	d0,(a0)
		move.w	(a7)+,SR
.ok
		movem.l	(a7)+,d0/a0
		rts


;same as wait_irq but doesn't wait more than 80 ticks
;-> a3=ide_base
;<- Z flag RESULT unset=ERROR set=OK

wait_irq2	movem.l	d0/a0,-(a7)

		tst.l	ide_irq
		bne.b	.nocd32
		move.b	#0,$bfe801
.loop		cmp.b	#80,$bfe801
		bhi.w	.err
		move.w	ide_altstat,d0
		move.b	(a3,d0.w),d0
		bpl.b	.ok2
		bra.b	.loop
.ok2		move.w	ide_status,d0
		move.b	(a3,d0.w),d0		;ack int
		btst	#0,d0
		bne.w	.err
		jmp	.ok

.nocd32
		move.b	#0,$bfe801
.noirq		cmp.b	#80,$bfe801
		bhi.b	.err
		move.l	ide_irq(pc),a0
		move.b	(a0),d0
		bpl.b	.noirq
		tst.b	config_A1200
		bne.b	.go1200
		move.w	ide_status,d0
		move.b	(a3,d0.w),d0
		bra.b	.ok
.go1200		move.w	SR,-(a7)
		move.w	#$2700,sr
		move.w	ide_status,d0
		move.b	(a3,d0.w),d0
		move.b	(a0),d0
		andi.b	#3,d0
		or.b	#$7c,d0
		move.b	d0,(a0)
		move.w	(a7)+,SR
.ok		moveq	#0,d0
		movem.l	(a7)+,d0/a0
		rts

.err		moveq	#-1,d0
		movem.l	(a7)+,d0/a0
		rts


;-> d0=block no
;-> d1=drive no (0/1)
;-> a0=dest address

Read_Block	movem.l	d1-d2/a0-a1/a3,-(a7)

		lea.l	IDE_info0,a1
		tst.w	d1
		beq.b	.okdrive0
		lea.l	IDE_info1,a1
.okdrive0
		tst.w	(a1)			;drive exists ?
		beq.w	.error

		move.l	ide_base(pc),a3

.wait		move.w	ide_status,d2
		tst.b	(a3,d2.w)
		bmi.b	.wait			;BUSY ?

		lsl.b	#4,d1
		and.b	#$10,d1			;Drive bit
		or.b	#$a0,d1

		move.w	II_NbHeads(a1),d2	;nb_heads
		mulu	II_NbSecTrack(a1),d2	;*nb_sectrack d2=nb sec/cyl
		tst.w	d2
		beq.w	.error
		divu	d2,d0			;d0=cyl no.
		cmp.w	II_NbCyl(a1),d0		;nb_cyl
		bge.w	.error

		move.w	ide_cyllo,d2
		move.b	d0,(a3,d2.w)
		lsr.w	#8,d0
		move.w	ide_cylhi,d2
		move.b	d0,(a3,d2.w)
		clr.w	d0
		swap	d0
		divu	II_NbSecTrack(a1),d0	;/nb_sectrack d0=no of head
		and.w	#%1111,d0
		or.b	d0,d1
		swap	d0
		cmp.w	II_NbSecTrack(a1),d0	;nb_sectrack
		bge.w	.error
		addq.b	#1,d0
		move.w	ide_secnb,d2
		move.b	d0,(a3,d2.w)		;start sector

		move.w	ide_dhead,d2
		move.b	d1,(a3,d2.w)
		move.w	ide_secnt,d2
		move.b	#1,(a3,d2.w)		;read 1 sector

		move.w	ide_command,d2
		move.b	#$20,(a3,d2.w)		;Read Drive cmd

		bsr	wait_irq

		move.w	ide_status,d0
		move.b	(a3,d0.w),d0
		btst	#0,d0
		bne.b	.error

		move.w	ide_data,d0
		lea.l	(a3,d0.w),a1
		move.w	#512/32-1,d0
.read		move.w	(a1),(a0)+
		move.w	(a1),(a0)+
		move.w	(a1),(a0)+
		move.w	(a1),(a0)+
		move.w	(a1),(a0)+
		move.w	(a1),(a0)+
		move.w	(a1),(a0)+
		move.w	(a1),(a0)+
		move.w	(a1),(a0)+
		move.w	(a1),(a0)+
		move.w	(a1),(a0)+
		move.w	(a1),(a0)+
		move.w	(a1),(a0)+
		move.w	(a1),(a0)+
		move.w	(a1),(a0)+
		move.w	(a1),(a0)+
		dbf	d0,.read

		move.w	ide_status,d0
		move.b	(a3,d0.w),d0
		btst	#0,d0
		bne.b	.error
		moveq	#0,d0
		bra.b	.out
.error		moveq	#-1,d0
.out		movem.l	(a7)+,d1-d2/a0-a1/a3
		rts


;-> d0=block no
;-> d1=drive no (0/1)
;-> a0=source address

Write_Block	movem.l	d1-d2/a0-a1/a3,-(a7)

		lea.l	IDE_info0,a1
		tst.w	d1
		beq.b	.okdrive0
		lea.l	IDE_info1,a1
.okdrive0
		tst.w	(a1)			;drive exists ?
		beq.w	.error

		move.l	ide_base(pc),a3

.wait		move.w	ide_status,d2
		tst.b	(a3,d2.w)
		bmi.b	.wait			;BUSY ?

		lsl.b	#4,d1
		and.b	#$10,d1			;Drive bit
		or.b	#$a0,d1

		move.w	II_NbHeads(a1),d2	;nb_heads
		mulu	II_NbSecTrack(a1),d2	;*nb_sectrack d2=nb sec/cyl
		divu	d2,d0			;d0=cyl no.
		tst.w	d2
		beq.w	.error
		cmp.w	II_NbCyl(a1),d0		;nb_cyl
		bge.w	.error

		move.w	ide_cyllo,d2
		move.b	d0,(a3,d2.w)
		lsr.w	#8,d0
		move.w	ide_cylhi,d2
		move.b	d0,(a3,d2.w)
		clr.w	d0
		swap	d0
		divu	II_NbSecTrack(a1),d0	;/nb_sectrack d0=no of head
		and.w	#%1111,d0
		or.b	d0,d1
		swap	d0
		cmp.w	II_NbSecTrack(a1),d0	;nb_sectrack
		bge.w	.error
		addq.b	#1,d0
		move.w	ide_secnb,d2
		move.b	d0,(a3,d2.w)		;start sector

		move.w	ide_dhead,d2
		move.b	d1,(a3,d2.w)
		move.w	ide_secnt,d2
		move.b	#1,(a3,d2.w)		;read 1 sector

		move.w	ide_command,d2
		move.b	#$30,(a3,d2.w)		;Write Drive cmd

.busy		move.w	ide_status,d0
		move.b	(a3,d0.w),d0
		bmi.b	.busy
		btst	#0,d0
		bne.b	.error
		btst	#3,d0			;DRQ ?
		beq.b	.busy

		move.w	ide_data,d0
		lea.l	(a3,d0.w),a1
		move.w	#512/32-1,d0
.write		move.w	(a0)+,(a1)
		move.w	(a0)+,(a1)
		move.w	(a0)+,(a1)
		move.w	(a0)+,(a1)
		move.w	(a0)+,(a1)
		move.w	(a0)+,(a1)
		move.w	(a0)+,(a1)
		move.w	(a0)+,(a1)
		move.w	(a0)+,(a1)
		move.w	(a0)+,(a1)
		move.w	(a0)+,(a1)
		move.w	(a0)+,(a1)
		move.w	(a0)+,(a1)
		move.w	(a0)+,(a1)
		move.w	(a0)+,(a1)
		move.w	(a0)+,(a1)
		dbf	d0,.write

		bsr	wait_irq

.busy2		move.w	ide_status,d0
		move.b	(a3,d0.w),d0
		btst	#0,d0
		bne.b	.error
		btst	#7,d0
		bne.b	.busy2
		moveq	#0,d0
		bra.b	.out
.error		moveq	#-1,d0
.out		movem.l	(a7)+,d1-d2/a0-a1/a3
		rts

;-> d0=block no
;-> d1=drive no (0/1)
;-> d2=nb ok blocks to write
;-> a0=ptr on top of stack of source address


WriteM_Block	movem.l	d1-d3/a0-a3,-(a7)

		lea.l	IDE_info0,a1
		tst.w	d1
		beq.b	.okdrive0
		lea.l	IDE_info1,a1
.okdrive0
		tst.w	d2
		beq.w	.error

		tst.w	(a1)			;drive exists ?
		beq.w	.error

		move.l	ide_base(pc),a3

.wait		move.w	ide_status,d3
		tst.b	(a3,d3.w)
		bmi.b	.wait			;BUSY ?

		lsl.b	#4,d1
		and.b	#$10,d1			;Drive bit
		or.b	#$a0,d1

		move.w	II_NbHeads(a1),d3	;nb_heads
		mulu	II_NbSecTrack(a1),d3	;*nb_sectrack d3=nb sec/cyl
		tst.w	d3
		beq.w	.error
		divu	d3,d0			;d0=cyl no.
		cmp.w	II_NbCyl(a1),d0		;nb_cyl
		bge.w	.error

		move.w	ide_cyllo,d3
		move.b	d0,(a3,d3.w)
		lsr.w	#8,d0
		move.w	ide_cylhi,d3
		move.b	d0,(a3,d3.w)
		clr.w	d0
		swap	d0
		divu	II_NbSecTrack(a1),d0	;/nb_sectrack d0=no of head
		and.w	#%1111,d0
		or.b	d0,d1
		swap	d0
		cmp.w	II_NbSecTrack(a1),d0	;nb_sectrack
		bge.w	.error
		addq.b	#1,d0
		move.w	ide_secnb,d3
		move.b	d0,(a3,d3.w)		;start sector

		move.w	ide_dhead,d3
		move.b	d1,(a3,d3.w)

		move.w	ide_secnt,d3
		move.b	d2,(a3,d3.w)		;write d2 sectors

		moveq	#$30,d0			;WriteSector
		move.w	ide_command,d3
		move.b	d0,(a3,d3.w)		;Drive cmd

		subq.w	#1,d2			;for dbf

.busy		move.w	ide_status,d0
		move.b	(a3,d0.w),d0
		bmi.b	.busy
		btst	#0,d0
		bne.b	.error
		btst	#3,d0			;DRQ ?
		beq.b	.busy

		move.l	-(a0),a2		;get next block address

		move.w	ide_data,d0
		lea.l	(a3,d0.w),a1

		move.w	#512/32-1,d0
.write		move.w	(a2)+,(a1)
		move.w	(a2)+,(a1)
		move.w	(a2)+,(a1)
		move.w	(a2)+,(a1)
		move.w	(a2)+,(a1)
		move.w	(a2)+,(a1)
		move.w	(a2)+,(a1)
		move.w	(a2)+,(a1)
		move.w	(a2)+,(a1)
		move.w	(a2)+,(a1)
		move.w	(a2)+,(a1)
		move.w	(a2)+,(a1)
		move.w	(a2)+,(a1)
		move.w	(a2)+,(a1)
		move.w	(a2)+,(a1)
		move.w	(a2)+,(a1)
		dbf	d0,.write

		bsr	wait_irq

		dbf	d2,.busy

.busy2		move.w	ide_status,d0
		move.b	(a3,d0.w),d0
		btst	#0,d0
		bne.b	.error
		btst	#7,d0
		bne.b	.busy2
		moveq	#0,d0
		bra.b	.out
.error		moveq	#-1,d0
.out		movem.l	(a7)+,d1-d3/a0-a3
		rts

****************************************************************

		include src/file.s
		include src/fbuffers.s
		include src/cmd_p.s
		include src/cmd_sp.s

**************************************************************************
;-------------- read the palette from custom registers -------------------

read_palette	movem.l	d0-d2/a0-a1,-(a7)
		tst.b	config_AGA
		beq.b	.noaga
		move.w	#$0124,$104(a6)		;set read mode
		lea.l	paletteL,a0
		move.w	#$200,d0
		moveq	#8-1,d1
.loop_bank	move.w	d0,$106(a6)
		lea.l	$180(a6),a1
		moveq	#32-1,d2
.color		move.w	(a1)+,(a0)+		;copy bank
		dbf	d2,.color
		add.w	#$2000,d0
		dbf	d1,.loop_bank

		lea.l	paletteH,a0
		moveq	#0,d0
		moveq	#8-1,d1
.loop_bank2	move.w	d0,$106(a6)
		lea.l	$180(a6),a1
		moveq	#32-1,d2
.color2		move.w	(a1)+,(a0)+		;copy bank
		dbf	d2,.color2
		add.w	#$2000,d0
		dbf	d1,.loop_bank2
		move.w	#$24,$104(a6)		;set write mode
.noaga		movem.l	(a7)+,d0-d2/a0-a1
		rts

restore_palette	movem.l	d0-d2/a0-a1,-(a7)
		tst.b	config_AGA
		beq.b	.noaga

		lea.l	paletteH,a0
		moveq	#0,d0
		moveq	#8-1,d1
.loop_bank2	move.w	d0,$106(a6)
		lea.l	$180(a6),a1
		moveq	#32-1,d2
.color2		move.w	(a0)+,(a1)+		;copy bank
		dbf	d2,.color2
		add.w	#$2000,d0
		dbf	d1,.loop_bank2

		lea.l	paletteL,a0
		move.w	#$200,d0
		moveq	#8-1,d1
.loop_bank	move.w	d0,$106(a6)
		lea.l	$180(a6),a1
		moveq	#32-1,d2
.color		move.w	(a0)+,(a1)+		;copy bank
		dbf	d2,.color
		add.w	#$2000,d0
		dbf	d1,.loop_bank

		move.w	custom+$10c,$10c(a6)	;for palette EOR

		move.w	#0,$106(a6)

.noaga		movem.l	(a7)+,d0-d2/a0-a1
		rts


**************************************************************************
;-------------- SCAN scan memory for samples -----------------------------

cmd_scan	jmp	end_command

**************************************************************************
*************** MMU routines *********************************************
**************************************************************************

;set a page mode
;-> a0 address
;-> mode 0=normal, 1=write protected, read/write protected
;<- d0 error (0=ok)


mmu_set_page:
		cmp.w	#4,proc_type
		blt.b	.err

;-------------- 68040/68060 MMU -----------
		nop

.err		moveq	#-1,d0
		rts

**************************************************************************
;--------------
; shows the reason because HrtMon was entered
; it will only output something if the reason is different than
; NMI and PORTS
; IN:	- a0 = stackframe
; OUT:	-

_ShowEntryReason
		movem.l	d0-a6,-(a7)
		moveq	#1,d0
		cmp.w	proc_type,d0
		bhs	.end
		moveq	#0,d0
		move.w	(6,a0),d0		;format
		cmp.w	#$68,d0			;keyboard
		beq	.end
		cmp.w	#$7c,d0			;NMI
		beq	.end
		lea	.str,a0
		bsr	print
		moveq	#4,d1
		bsr	print_hex
		lea	.spc,a0
		bsr	print
		and.w	#$fff,d0
		lsr.w	#2,d0
		lea	_exceptionnames,a0
		bsr	_DoStringNull
		move.l	d0,a0
		tst.l	d0
		bne	.known
		lea	.unknown,a0
.known		bsr	print
		bsr	_PrintLn
.end		movem.l	(a7)+,d0-a6
		rts

.str		dc.b	"$",0
.spc		dc.b	" ",0
.unknown	dc.b	"undefined entry into HrtMon",0
		even

;--------------
; prints a newline
; IN:	-
; OUT:	-

_PrintLn	lea	.lf,a0
		bra	print

.lf		dc.b	10,0

**************************************************************************
		
		include	src/whdload.s

**************************************************************************

	CNOP 0,4
		include src/disassemble.s
		include src/assemble.s

;make a copy of registers for disassemble function

init_regBMON	lea.l	regsBMON,a1
		lea.l	registres,a0
		moveq	#16-1,d0
.copy		move.l	(a0)+,(a1)+
		dbf	d0,.copy
		move.l	usp_reg,(a1)+
		move.l	isp_reg,(a1)+
		move.l	msp_reg,(a1)+
		move.l	vbr_reg,(a1)+
		move.l	cacr_reg,(a1)+
		move.l	caar_reg,(a1)+
		move.l	pc_reg,(a1)+
		moveq	#0,d0
		move.w	sr_reg,d0
		move.l	d0,(a1)+
		rts

************************************************************

		include src/keymaps.s

;----------------------------------------------------------------------------

ascII		dc.b "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz "
		dc.b "0123456789+",'"'
		dc.b "*ç%&/()=?'!$#-,;:[]<>@.éàèöäü",$5c,"|§°_¹²{} "
end_ascII

;--- special ascII -------
; '¹' = continous line
; '²' = small dot

;----------------------------------------------------------------------------

ascIIx		dcb.b $20,"²"
		dc.b ' !"£$%&',"'()*+,-./0123456789:;<=>?@"
		dc.b "ABCDEFGHIJKLMNOPQRSTUVWXYZ[",$5c,"]^_²"
		dc.b "abcdefghijklmnopqrstuvwxyz"
		dc.b "{|}²çüé²äà²ç²²è"
		dcb.b 256,"²"
		even

hex_list	dc.b "0123456789ABCDEF"

		cnop 0,4
ascII_conv	dcb.b 256,0


file_len_txt	dc.b "File length=$",0
BP_enter_txt	dc.b "Break Point reached at address $",0
BPJ_enter_txt	dc.b "JSR Break Point reached at address $",0
BP_set_txt	dc.b "Break Point set at address $",0
BPJ_set_txt	dc.b "JSR Break Point set at address $",0
BP_clr_txt	dc.b "Break Point removed at address $",0
all_break_txt	dc.b "Break Point at $",0
all_break2_txt	dc.b "JSR Break Point at $",0
all_BP_txt	dc.b "All Break Points cleared",$a,0
show_txt	dc.b "memory address : $",0
break_txt	dc.b "Break...",$a,0
drive_txt	dc.b "Active drive is now : ",0
no_disk_txt	dc.b "No disk in drive ...",$a,0
corrupt_txt	dc.b "Track corrupted ...",$a,0
bad_sum_txt	dc.b "Bad Block_Checksum ...",$a,0
NotDOS_txt	dc.b "Not a DOS disk ...",$a,0
WriteProt_txt	dc.b "Disk is write-protected ...",$a,0
Bitmap_txt	dc.b "Invalid Bitmap ...",$a,0

NotEmpty_txt		dc.b "Directory not empty ...",$a,0
IDEerr_txt		dc.b "IDE disk error ...",$a,0
NoFFS_txt		dc.b "Not an FFS or Int. FFS disk ...",$a
			dc.b "(OFS and DC-FFS not supported)",$a,0
IllegalPath_txt		dc.b "Illegal path ...",$a,0
DeviceNotFound_txt	dc.b "Device not found ...",$a,0
FileExists_txt		dc.b "File already exists ...",$a,0
DiskFull_txt		dc.b "Disk full ...",$a,0
FileNotFound_txt	dc.b "File not found ...",$a,0
CreateFile_txt		dc.b "Can't create file ...",$a,0


unknown_txt		dc.b "Unknown command...",$a,0
illegal_syntax_txt	dc.b "Illegal syntax...",$a,0
illegal_addr_txt	dc.b "Illegal address...",$a,0
illegal_reg_txt		dc.b "Illegal register...",$a,0
illegal_val_txt		dc.b "Illegal value...",$a,0
illegal_name_txt	dc.b "Illegal name...",$a,0
illegal_string_txt	dc.b "Illegal string...",$a,0
illegal_expr_txt	dc.b "Illegal expression...",$a,0
not_found_txt		dc.b "File not found...",$a,0
too_break_txt		dc.b "Too many break points...",$a,0
insert_off_txt		dc.b "off"
insert_on_txt		dc.b "on "

outerr_txt	dc.b "Wrong register number...",$a,0

		include src/help.s

;--------------------------------------------------------------------

topaz2		incbin "topaz3.raw"
		;880*10 Main Font

		cnop 0,4

***********************************************************


ascII_mac	macro
		dcb.b 80*(MAX_SCREEN-3),' '		;fill with spc.
		dc.b "¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹"
		dc.b "¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹¹"
		dc.b "HRTmon V"
		version
	IFND BARFLY
		dc.b " by Alain Malek                         "
		dc.b "                      "
	ELSE
		dc.b " by Alain Malek patched by Wepl at "
	DOSCMD	"WDate  >T:date"
	INCBIN	"T:date"
		dc.b "               "
	ENDC

		dc.b \1
		dc.b "Track:[00] Drive:[0] Address:[$00000000]"
		dc.b " Hrt:[$00000000] Fcpu:[65802]  Ins:[Off]"


		even
		endm

*****************************************************************************
;----------------------------------------
; names of exception/interrupt vectors
; for using with _DoString

_exceptionnames
.exlist		dc.w	2		;first
		dc.w	61		;last
		dc.l	0		;next list
		dc.w	.buserr-.exlist		;#2
		dc.w	.adderr-.exlist
		dc.w	.illinst-.exlist
		dc.w	.div-.exlist
		dc.w	.chk-.exlist
		dc.w	.trapv-.exlist
		dc.w	.priv-.exlist
		dc.w	.trace-.exlist
		dc.w	.linea-.exlist		;#10
		dc.w	.linef-.exlist		;#11
		dc.w	.emu-.exlist
		dc.w	.co-.exlist		;#13
		dc.w	.fmt-.exlist
		dc.w	.nii-.exlist
		dcb.w	8,0
		dc.w	.spi-.exlist		;#24
		dc.w	.au1-.exlist
		dc.w	.au2-.exlist
		dc.w	.au3-.exlist
		dc.w	.au4-.exlist
		dc.w	.au5-.exlist
		dc.w	.au6-.exlist
		dc.w	.nmi-.exlist
		dc.w	.t0-.exlist		;#32
		dc.w	.t1-.exlist
		dc.w	.t2-.exlist
		dc.w	.t3-.exlist
		dc.w	.t4-.exlist
		dc.w	.t5-.exlist
		dc.w	.t6-.exlist
		dc.w	.t7-.exlist
		dc.w	.t8-.exlist
		dc.w	.t9-.exlist
		dc.w	.t10-.exlist
		dc.w	.t11-.exlist
		dc.w	.t12-.exlist
		dc.w	.t13-.exlist
		dc.w	.t14-.exlist
		dc.w	.t15-.exlist
		dc.w	.fbra-.exlist
		dc.w	.fir-.exlist
		dc.w	.fdiv-.exlist
		dc.w	.fuf-.exlist
		dc.w	.foe-.exlist
		dc.w	.fof-.exlist
		dc.w	.fnan-.exlist
		dc.w	.fudt-.exlist
		dc.w	.mmucfg-.exlist
		dc.w	.51io-.exlist
		dc.w	.51alv-.exlist
		dc.w	0
		dc.w	.uea-.exlist		;#60
		dc.w	.ui-.exlist		;#61
.buserr		dc.b	"Access Fault",0
.adderr		dc.b	"Address Error",0
.illinst	dc.b	"Illegal Instruction",0
.div		dc.b	"Integer Divide by Zero",0
.chk		dc.b	"CHK,CHK2 Instruction",0
.trapv		dc.b	"TRAPV,TRAPcc,cpTRAPcc Instruction",0
.priv		dc.b	"Privilege Violation",0
.trace		dc.b	"Trace",0
.linea		dc.b	"Line 1010 Emulator",0
.linef		dc.b	"Line 1111 Emulator",0
.emu		dc.b	"Emulator Interrupt",0			;68060
.co		dc.b	"Coprocessor Protocol Violation",0	;68020/68030
.fmt		dc.b	"Stackframe Format Error",0
.nii		dc.b	"Uninitialized Interrupt",0
.spi		dc.b	"Spurious Interrupt",0
.au1		dc.b	"Level 1 Autovector (TBE/DSKBLK/SOFT)",0
.au2		dc.b	"Level 2 Autovector (CIA-A/EXT)",0
.au3		dc.b	"Level 3 Autovector (COPPER/VBLANK/BLITTER)",0
.au4		dc.b	"Level 4 Autovector (AUDIO0-3)",0
.au5		dc.b	"Level 5 Autovector (RBF/DSKSYNC)",0
.au6		dc.b	"Level 6 Autovector (CIA-B/EXT)",0
.nmi		dc.b	"NMI Autovector",0
.t0		dc.b	"TRAP #0",0
.t1		dc.b	"TRAP #1",0
.t2		dc.b	"TRAP #2",0
.t3		dc.b	"TRAP #3",0
.t4		dc.b	"TRAP #4",0
.t5		dc.b	"TRAP #5",0
.t6		dc.b	"TRAP #6",0
.t7		dc.b	"TRAP #7",0
.t8		dc.b	"TRAP #8",0
.t9		dc.b	"TRAP #9",0
.t10		dc.b	"TRAP #10",0
.t11		dc.b	"TRAP #11",0
.t12		dc.b	"TRAP #12",0
.t13		dc.b	"TRAP #13",0
.t14		dc.b	"TRAP #14",0
.t15		dc.b	"TRAP #15",0
.fbra		dc.b	"FP Branch or Set on Unordered Condition",0
.fir		dc.b	"FP Inexact Result",0
.fdiv		dc.b	"FP Divide by Zero",0
.fuf		dc.b	"FP Underflow",0
.foe		dc.b	"FP Operand Error",0
.fof		dc.b	"FP Overflow",0
.fnan		dc.b	"FP Signaling NAN",0
.fudt		dc.b	"FP Unimplemented Datatype",0		;68040
.mmucfg		dc.b	"MMU Configuration Error",0		;68030/68851
.51io		dc.b	"MMU Illegal Operation Error",0		;68851
.51alv		dc.b	"MMU Access Level Violation Error",0	;68851
.uea		dc.b	"Unimplemented Effective Address",0	;68060
.ui		dc.b	"Unimplemented Integer Instruction",0	;68060

		EVEN


*****************************************************************************
;----------------------------------------
; Berechnet String-Adresse über Zuordungstabelle
; input :	D0 = WORD   value
;		A0 = STRUCT Zuordnungstabelle
; output :	D0 = CPTR   string or NULL

_DoStringNull
.start		cmp.w	(a0),d0			;lower bound
		blt	.nextlist
		cmp.w	(2,a0),d0		;upper bound
		bgt	.nextlist
		move.w	d0,d1
		sub.w	(a0),d1			;index
		add.w	d1,d1			;because words
		move.w	(8,a0,d1.w),d1		;rptr
		beq	.nextlist
		add.w	d1,a0
		move.l	a0,d0
		rts

.nextlist	move.l	(4,a0),a0		;next list
		move.l	a0,d1
		bne	.start
		
		moveq	#0,d0
		rts

***********************************************************

		include src/format_block.s
		include src/custom_names.s

**************************************************************************
**************************************************************************
**************************************************************************

		cnop 0,16

inited		dc.l 0			;<>$12345678 then never entered
registres	dcb.l 14,0		;d0-d7,a0-a5
a6_reg		dc.l 0
a7_reg		dc.l 0
sr_reg		dc.w 0
pc_reg		dc.l 0
ssp_reg		dc.l 0
usp_reg		dc.l 0
vbr_reg		dc.l 0
cacr_reg	dc.l 0
caar_reg	dc.l 0		; or BUSR on 68060
isp_reg		dc.l 0
msp_reg		dc.l 0		; or PCR on 68060
tc_reg		dc.l 0
end_registres
itt0_reg	dc.l 0
itt1_reg	dc.l 0
dtt0_reg	dc.l 0
dtt1_reg	dc.l 0

		cnop 0,4

OldRaster	dc.l 0			;old pos of raster beam

proc_type	dc.w 2			;processor type 2=68020,...,6=68060
	;keyboard ptr with/without SHIFT, ALT
board_ptr	dc.l board3,board4,board4a

;-------------------------------------------

ascII_ptr	dc.l ascII_page1	;ptr on actual ascII page
		dc.l ascII_page1	;list of ptrs on all ascII pages
		dc.l ascII_page2
		dc.l ascII_Tracer
		dc.l 0			;end signal for multi-print

Reset_Flag	dc.w 0			;Copy of Action_GAL AND %100
trace_count	dc.w 0			;nb of steps to trace
escape		dc.b 0			;escape monitor signal
kill		dc.b 0			;Kill cmd done
reboot		dc.b 0			;Reboot cmd done
break_mode	dc.b 0			;0=no break points -1 BP set
BP_reach	dc.b 0			;entered from Illegal breakpoint
BPJ_reach	dc.b 0			;entered from JSR breakpoint
BPatPC		dc.b 0			;BP at PC -> need one trace step
trace_moni	dc.b 0			;Tracer mode ON/OFF
cheat		dc.b 0			;Can access Action-Replay area ?
cheat_cnt	dc.b 0			;counter to enable cheat
pic_status	dc.b 0			;0=low-mem ok. -1=pic in low-mem
VBL		dc.b 0			;set to -1 at each VBL
packed		dc.b 0			;used by LA_cmd
debug_entry	dc.b 0	;1=bus,2=addr,3=illegal,4=zero,5=linea,6=linef
		cnop 0,4


;-------------------------------------------
		cnop 0,4

last_cmd	dc.l cmd_list		;keep last command
hex_ptr:	dc.l 0			;ptr on last address shown as hexa
dis_ptr:	dc.l 0			;ptr on last addr disassembled
asc_ptr		dc.l 0			;same for ascII dump

time_repeat:	dc.w 0
time_cursor:	dc.w 0
cursor_x:	dc.w 0
cursor_y:	dc.w 0
old_cursor:	dc.l 0		;cursor_xy backup for Tracer
print_cnt	dc.w 0		;used in print procedure
ascII_num:	dcb.b 8,0
cursor_on:	dc.b 0		;actual cursor state (displayed or not)
no_curs:	dc.b 0		;disable cursor
key:		dc.b 0		;actual key
key_prev:	dc.b $7f	;previous key
shift_mode	dc.b 0		;0=no shift key pressed
ctrl_mode	dc.b 0		;0=no ctrl key pressed
alt_mode	dc.b 0		;0=no alt key pressed
amiga_mode	dc.b 0		;0=no amiga key pressed
insert_mode	dc.b 0		;0=no insert -1=insert mode on
break		dc.b 0		;break signal (ESC)
ascII_page	dc.b 0		;0=page1, -1=page2
		even
nb_keys		dc.w 0
key_buffer	dcb.b 64,0
new_key		dc.b 0
		even

history_cnt	dc.w 0		;last history
history_disp	dc.w 0		;next displayed history
history		rept	16
		dcb.b 80,0
		endr		;16 lines of history

command:	dcb.b 80+20,0		;the actual command line is here

window_top	dc.w 0		;top of actual window
window_bot	dc.w 0		;bottom of actual window (last line)
screen_height	dc.w 22		;nb max of lines (depending of resolution)


no_print	dc.b 0		;if -1 then print won't write in the picture
				;memory, only in the ascII_page

disk_op		dc.b 0		;-1 when a disk operation occures
floppy_op	dc.b 0		;-1 when floppy operation occured (needs motor_off)
cmd_executed	dc.b 0		;-1 when a command is executed
cmd_crashed	dc.b 0		;-1 if command crashed

		cnop 0,4

;-------------- strings which are modified ----------------
registre_txt
data0_reg	dc.b "D0=xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx "
data4_reg	dc.b "xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx",$a
addr0_reg	dc.b "A0=xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx "
addr4_reg	dc.b "xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx",$a
pc_txt		dc.b "PC=xxxxxxxx  "
ssp_txt		dc.b "SSP=xxxxxxxx "
usp_txt		dc.b "USP=xxxxxxxx "
vbr_txt		dc.b "VBR=xxxxxxxx "
sr_txt		dc.b "SR=xxxx "
rts_txt		dc.b "RTS=xxxxxxxx",$a
ISP_txt		dc.b "ISP=xxxxxxxx "
MSP_txt		dc.b "MSP=xxxxxxxx "
CACR_txt	dc.b "CACR=xxxxxxxx "
CAAR_txt	dc.b "CAAR=xxxxxxxx "
mc_txt		dc.b "MC68060",$a,0

hex_txt:
	dc.b "h $xxxxxxxx $xxxx $xxxx $xxxx $xxxx $xxxx $xxxx $xxxx $xxxx "
hex_txt2	dc.b "'                '",$a,0

		cnop 0,4
general_txt	dcb.b 80,0	;can be used by anyone to output a line
		dc.b $a,0
		even

ev_line		dcb.b 80,0	;evaluation output line (for any other use too)
		dc.b $a,0

;-------------- Tracer variables ---------------------
		cnop 0,4
tracer_old_rts	dc.l 0		;when skiping subroutine
tracer_addr	dcb.l MAX_SCREEN-10,-1	;address disassembled on each line
				;of the tracer

actual_pos	dc.w -1,-1
tracer_refresh	dc.b 0
trace_bsr	dc.b 0

;-------------- watch variables ----------------------
watch_txt	dcb.b 12," "

watch		dc.l 0			;the thing to watch, ptr or val

;-------------- output cmd variables -----------------
		cnop 0,4
output_start	dc.l 0
output_ptr	dc.l 0

		cnop 0,4

;-------------- Q (cmp mem) cmd variables ------------

cmp_start	dc.l 0
cmp_end		dc.l 0
cmp_dest	dc.l 0

;-------------- A (assemble) cmd variables -----------

ass_addr	dc.l 0		;address parameter
undecroffset	dc.l 0		;offset for undecrypted instruction
rd_mode3_val	dc.l 0		;additional value for decrypting in rn mode 3
rd_mode		dc.w 0		;rob-northen-disassemble-mode
		dc.w 0		;reserved

;-------------- AF cmd variables ---------------------

af_ptr		dc.l 0

;-------------- O (fill mem) cmd variables -----------

fill_start	dc.l 0
fill_end	dc.l 0

;-------------- E show custom registers --------------

custom_offset	dc.l 0

;-------------- trainer variables --------------------

ts_size		dc.w -1			;size used for searching .B .W .L
		cnop 0,4
ts_start	dc.l 0
ts_end		dc.l 0

tf_txt		dc.b "$xxxxxxxx ",0
ts_deep		dc.b 0			;deep trainer mode on/off
		cnop 0,4

;-------------- DEBUG cmd variables ------------------
		cnop 0,4

exc8		dc.l 0			;backup of
excC		dc.l 0			;exception
exc10		dc.l 0			;vectors
exc14		dc.l 0			;when
exc28		dc.l 0			;in
exc2C		dc.l 0			;debug mode

debug		dc.b 0			;debug mode ON/OFF

;-------------- F,FI,FIF cmds variable ---------------
		cnop 0,4

find_start	dc.l 0
find_end	dc.l 0
find_list	dcb.b 80,0
		dc.w 0

		cnop 0,4

;-------------- RS,WS cmds variables -----------------

sec_addr	dc.l 0

strtsec		dc.w 0
nbsec		dc.w 0

rwsec		dc.b 0		;'R' or 'W' (rs or ws cmd)
		cnop 0,4

;-------------- T,TA (trace) cmds variable -----------

old_trace:	dc.l 0		;old trace exception vector
trace_address:	dc.l -1		;used by TA

;-------------- C,CE (copy,exg mem) cmd variables ----

copy_start	dc.l 0
copy_end	dc.l 0
copy_dest	dc.l 0
exgc		dc.b 0			;copy or exg ?
		cnop 0,4

;-------------- variables used by read_number procedure ---

minus		dc.b 0
ok_numb		dc.b 0
label		dc.b 0			;disable label recognition
hex_default	dc.b 0			;if set, default are hex numbers
		cnop 0,4

;-------------- variables used for evaluation --------

exit_stack	dc.l 0			;to restore stack after an error
muls_data	dc.l 0,0		;used by muls32 procedure
divs_data	dc.l 0,0		;used by divs32 procedure
eval_size	dc.w 0			;size 0=.b 1=.w 3=.l
		cnop 0,4

;-------------- 65816 CPU variables ------------------

fami_mode	dc.b 0		;0=65802 -1=65816
fami_auto	dc.b 0		;automatic switch on/off   0/-1
		cnop 0,4

;-------------- breakpoint variables -----------------

illegal_except	dc.l 0		;old illegal exception vector for BP
Break_Address	dc.l 0		;keep address of BP when entering monitor
JB_save		dc.l 0,0	;used by JSR_reach

break_list	rept 20			;20 breakpoints max.

		dc.l 0			;break address
		dc.w -1			;old word
		dc.l -1			;old address for jsr
		endr

		dc.l -1			;end
		dc.w -1			;
		dc.l -1			;signals
		cnop 0,4

;-------------- variable used by get_dkey procedure --

dkey		dc.b 0,0		;direct key,olddkey
dkey_rep	dc.w 0			;direct key,repeat
dkey_shift	dc.b 0			;direct shift
		cnop 0,4

;-------------- Floppy disk variables ----------------

old_head	dc.w 0,0,0,0		;old pos of head (1 for each drive)

sectpos:	dc.w 0			;nb sectors to read in access
startsec:	dc.w 0			;offset of first sector in 1st track
oldsectpos:	dc.w 0
oldstartsec:	dc.w 0

drive_present	dc.b 0,0,0,0		;floppy drive present

drive:		dc.w 3			;selected floppy disk (3-6)
track:		dc.w 0,0,0,0		;actual pos of head (1 for each drive)
OldAdk:		dc.w 0
writes:		dc.b 0			;write floppy accesss ?
init:		dc.b 0			;head pos initied ? (bits 3-6)
mot_on:		dc.b 0			;motor on ? (bits 3-6)
ctrl:		dc.b 0			;end access signal

		cnop 0,4

header:		dc.l $ff00000b		;MFM header
format		dc.b 0
first_sauve	dc.b 0
		cnop 0,4

drive_err	dc.w 0

		cnop 0,4

buffer		dc.l 0			;address of DMA disk buffer

;-------------- IDE drive variables ------------------

		STRUCTURE IDE_info,0
		UWORD II_NbCyl
		UWORD II_NbHeads
		UWORD II_NbSecTrack
		UWORD II_MultSize		;multiple block size
		LABEL II_SIZEOF

IDE_info0	dc.w 0		;NbCyl
		dc.w 0		;NbHeads
		dc.w 0		;NbSecTrack
		dc.w 0		;MultSize

IDE_info1	dc.w 0		;NbCyl
		dc.w 0		;NbHeads
		dc.w 0		;NbSecTrack
		dc.w 0		;MultSize

		even

;-------------- part variables -----------------------

		cnop 0,4

floppy0:	dc.l 0
		dc.l 0
		dc.l 1760
		dc.l floppy_device
		dc.w 0			;unit
		dc.b 0,0,0,0		;for DOS\1,2,3,...
		dc.b 3,"DF0"
		dcb.b 32-4,0
		dcb.b 32,0
floppy1:	dc.l 0
		dc.l 0
		dc.l 1760
		dc.l floppy_device
		dc.w 1			;unit
		dc.b 0,0,0,0		;for DOS\1,2,3,...
		dc.b 3,"DF1"
		dcb.b 32-4,0
		dcb.b 32,0

		rept 16			;16 harddisk partition max
		dc.l 0
		dc.l 0
		dc.l 0
		dc.l 0
		dc.w 0			;unit
		dc.b 0,0,0,0		;for DOS\1,2,3,...
		dcb.b 32,0
		dcb.b 32,0
		endr
harddisk_end


;-------------- track buffer to speedup floppy access -----

track_buffer	dcb.b 512*11	;track buffer (11 sectors)
track_buffer_no	dc.w -1		;no of track in track_buffer (0-159)-1=empty
track_sector	dcb.b 12,-1	;-1 if sector of track_buffer isn't loaded
				;(12 values instead of 11 for easier clear)
flush_needed	dc.b 0
		cnop 0,4


;------------------------------------------------

ascII_page1	ascII_mac "Page_1"
ascII_page2	ascII_mac "Page_2"
ascII_Tracer	ascII_mac "Tracer"

;------------------------------------------------
		cnop 0,4

custom		dcb.w $100,0		;copy of custom $DFF000-$DFF1FE

CIAA		dcb.b 16,0
CIAB		dcb.b 16,0

palette
paletteH	dcb.w 8*32,0		;High bits
paletteL	dcb.w 8*32,0		;Low bits

;------------------------------------------------

fbuf1		rept nb_fbuf
		dcb.b 512,0		;file buffers
		endr

crash_stack	dcb.b $100,0

backup_vbr	dcb.b $400,0		;backup of vector table (before HRTmon installation)

backup_pic	dcb.b PICSIZE,0		;backup of pic mem
backup_dma	dcb.b $1a00*2,0		;backup of floppy disk dma buffer

;------------------------------------------------
		cnop 0,4

tmp_mem_size	equ $8000
tmp_mem		dcb.b tmp_mem_size,0

;tmp_mem is used by:
; - Trainer commands
; - copy command (copy a file)
; - d2f command (disk to file) (uses 512*11 first bytes)
; - f2d command (file to disk) (uses 512*11 first bytes)
; - sa command (save all)
; - la command (load all)
; - sp command (save picture)

;(!Trainer buffer can be erased by one of these commands!)

		dcb.b $800,0	;stack area
stack

end

;--------------------------------------------------------------------

;max floppy filesize=1730*512=885760

