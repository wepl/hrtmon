
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


**********************************************************
;-------------- assemble 65816 (AF) ----------------------

cmd_af		bsr	evaluate
		bne.w	illegal_addr
		move.l	d0,af_ptr
		move.l	d0,a4
		bsr	reloc_pic
.loop		move.b	(a0)+,d0
		beq.b	.end_line
		cmp.b	#$20,d0
		beq.b	.loop
		subq.l	#1,a0

		bsr	f_assemble
		tst.l	d0
		beq.b	.end_line		;error

		add.l	d0,af_ptr
		move.l	af_ptr,d0
		moveq	#8,d1
		lea.l	.nextAF_txt,a0
		bsr	print
		bsr	print_hex
		lea.l	.nextAF2_txt,a0
		bsr	print

.end_line
		bra.w	end_command

.nextAF_txt	dc.b "af $",0
.nextAF2_txt	dc.b " ",0
		even

**********************************************************
;-------------- disassemble 65816 (DF) -------------------

cmd_df		move.b	(a0)+,d0
		beq.b	ok_df_cmd		;keep last address
		cmp.b	#$20,d0
		beq.b	cmd_df
		subq.l	#1,a0

		bsr	evaluate
		bne.w	illegal_addr
		move.l	d0,dis_ptr

ok_df_cmd	move.l	dis_ptr,d0		;a4 = ptr on memory
		move.l	d0,a4

		moveq	#8-1,d7
next_disf_line	move.l	a4,d0
		lea.l	general_txt,a0		;print address
		move.b	#'d',(a0)+
		move.b	#'f',(a0)+
		move.b	#'$',(a0)+
		moveq	#8,d1
		bsr	conv_hex

		move.l	a4,-(a7)
		bsr	reloc_pic

		lea.l	general_txt+11,a0
		move.l	#'    ',d0
		move.l	d0,(a0)
		move.l	d0,4(a0)
		move.l	d0,8(a0)
		move.l	d0,12(a0)

		lea.l	general_txt+25,a0
		bsr	f_disassemble
		move.l	(a7)+,a4
		move.w	d0,d3			;copy instr. len

		lea.l	general_txt+12,a0
		moveq	#2,d1
.loop		move.b	(a4)+,d0
		bsr	conv_hex
		add.l	d1,a0
		move.b	#' ',(a0)+
		subq.w	#1,d3
		bne.b	.loop

		lea.l	general_txt,a0
		bsr	print

		tst.b	break
		dbne	d7,next_disf_line

		move.l	a4,dis_ptr

		bra.w	end_command

************************************************************
;a0=ascII source of instr.
;a4=memory ptr (corrected)
;returns instr. len in d0 0=error

f_assemble	movem.l	d1-d7/a1-a4,-(a7)

		move.b	(a0)+,d0
		bsr	upper_case
		lsl.l	#8,d0
		move.b	(a0)+,d0
		bsr	upper_case
		lsl.l	#8,d0
		move.b	(a0)+,d0
		bsr	upper_case
		lsl.l	#8,d0
		move.l	d0,d2			;d2=ASCII of instr.

		moveq	#18,d1			;addressing mode (18=rien)
.seek		move.b	(a0)+,d0
		beq.w	.end_line
		cmp.b	#$20,d0
		beq.b	.seek

		cmp.b	#'#',d0
		bne.b	.no_immediat
		bsr	evaluate
		bne.w	.error
		move.l	d0,d4
		moveq	#0,d1
		bra.w	.end_line

.no_immediat	cmp.b	#'A',d0
		bne.b	.no_accu
		moveq	#15,d1
		bra.w	.end_line

.no_accu	cmp.b	#'<',d0
		bne.b	.no_direct
		bsr	evaluate
		bne.w	.error
		move.l	d0,d4
		cmp.b	#',',(a0)+
		bne.b	.no_virg
		move.b	(a0)+,d0
		bsr	upper_case
		cmp.b	#'X',d0
		bne.b	.no_x
		moveq	#2,d1
		bra.w	.end_line
.no_x		cmp.b	#'Y',d0
		bne.b	.no_y
		moveq	#22,d1
		bra.w	.end_line
.no_y		cmp.b	#'S',d0
		bne.b	.no_s
		moveq	#13,d1
		bra.w	.end_line
.no_s		bra.w	.error
.no_virg	moveq	#1,d1
		bra.w	.end_line

.no_direct	cmp.b	#'!',d0
		bne.b	.no_abs
		bsr	evaluate
		bne.w	.error
		move.l	d0,d4
		cmp.b	#',',(a0)+
		bne.b	.no_virg2
		move.b	(a0)+,d0
		bsr	upper_case
		cmp.b	#'X',d0
		bne.b	.no_x2
		moveq	#9,d1
		bra.b	.end_line
.no_x2		cmp.b	#'Y',d0
		bne.b	.no_y2
		moveq	#10,d1
		bra.b	.end_line
.no_y2		bra.w	.error
.no_virg2	moveq	#8,d1
		bra.b	.end_line

.no_abs		cmp.b	#'>',d0
		bne.b	.no_absl
		bsr	evaluate
		bne.w	.error
		move.l	d0,d4
		cmp.b	#',',(a0)+
		bne.b	.no_virg3
		move.b	(a0)+,d0
		bsr	upper_case
		cmp.b	#'X',d0
		bne.b	.no_x3
		moveq	#12,d1
		bra.b	.end_line
.no_x3		bra.w	.error
.no_virg3	moveq	#11,d1
		bra.b	.end_line


.no_absl

		subq.l	#1,a0
		moveq	#17,d1
		bsr	evaluate
		bne.w	.error
		move.l	d0,d4

.end_line

		lea.l	fami_list-8,a3
.loop		addq.l	#8,a3
		tst.b	1(a3)
		bne.w	.end_seek
		cmp.l	2(a3),d2		;cmp instr.
		bne.b	.loop
		move.w	6(a3),d3
		and.w	#$ff,d3
		cmp.w	d3,d1			;cmp addr. mode
		bne.b	.loop

		move.b	(a3),(a4)		;copy opcode
		lea.l	fami_len,a3
		add.w	d1,d1
		moveq	#0,d5
		move.b	(a3,d1.w),d5
		cmp.b	#6,d5
		blt.b	.normal
		bne.b	.no_m6
		move.l	a4,d0
		addq.l	#2,d0
		move.l	d4,d3
		sub.l	d0,d3
		cmp.l	#127,d3
		bgt.w	.too_long
		cmp.l	#-128,d3
		blt.w	.too_long
		move.b	d3,1(a4)
		bra.b	.do_instr
.no_m6		move.l	a4,d0
		addq.l	#3,d0
		move.l	d4,d3
		sub.l	d0,d3
		cmp.l	#32767,d3
		bgt.w	.too_long
		cmp.l	#-32768,d3
		blt.w	.too_long
		move.b	d3,1(a4)
		lsr.w	#8,d3
		move.b	d3,2(a4)
		bra.b	.do_instr

.normal		cmp.b	#5,d5
		bne.b	.no_imm
		moveq	#1,d5			;8bit mode
		tst.b	fami_mode
		beq.b	.no_imm
		moveq	#2,d5			;16bit mode
.no_imm
		lea.l	1(a4),a1
		bra.b	.go_dbf
.sup		move.b	d4,(a1)+
		lsr.l	#8,d4
.go_dbf		dbf	d5,.sup

.do_instr	moveq	#0,d0
		move.b	1(a3,d1.w),d0		;read instr. len
		cmp.b	#7,d0
		bne.b	.ok
		moveq	#2,d0
		tst.b	fami_mode
		beq.b	.ok
		moveq	#3,d0
		bra.b	.ok

.end_seek	cmp.w	#17,d1
		bne.b	.no_17
		moveq	#19,d1
		bra.w	.end_line

.no_17		cmp.w	#0,d1
		bne.b	.no_00
		moveq	#16,d1
		bra.w	.end_line
.no_00

.error		lea.l	AFerror_txt,a0
		bsr	print
		moveq	#0,d0

.ok		movem.l	(a7)+,d1-d7/a1-a4
		rts

.too_long	lea.l	OutOfRange_txt,a0
		bsr	print
		moveq	#0,d0
		bra.w	.ok

OutOfRange_txt	dc.b "Out of range error ...",$a,0
AFerror_txt	dc.b "Illegal instruction ...",$a,0
		even

;< direct
;! absolu
;> absolu long

;	dc.b sup.bytes mode,instr.len
fami_len
	dc.b 1,2		;00 #$zz
	dc.b 1,2		;01 $zz
	dc.b 1,2		;02 $zz,x
	dc.b 1,2		;03 ($zz)
	dc.b 1,2		;04 ($zz,x)
	dc.b 1,2		;05 ($zz),y
	dc.b 1,2		;06 [$zz]
	dc.b 1,2		;07 [$zz],y
	dc.b 2,3		;08 $zzzz
	dc.b 2,3		;09 $zzzz,x
	dc.b 2,3		;10 $zzzz,y
	dc.b 3,4		;11 $zzzzzz
	dc.b 3,4		;12 $zzzzzz,x
	dc.b 1,2		;13 $zz,s
	dc.b 1,2		;14 ($zz,s),y
	dc.b 0,1		;15 A
	dc.b 5,7		;16 #$zz #$zzzz
	dc.b 6,2		;17 pc+2+next byte
	dc.b 0,1		;18 'rien'
	dc.b 7,3		;19 pc+3+next word
	dc.b 2,3		;20 ($zzzz)
	dc.b 2,3		;21 ($zzzz,x)
	dc.b 1,2		;22 $zz,y

;00 #$zz  8BitOnly
;01 $zz
;02 $zz,x
;03 ($zz)
;04 ($zz,x)
;05 ($zz),y
;06 [$zz]
;07 [$zz],y
;08 $zzzz
;09 $zzzz,x
;10 $zzzz,y
;11 $zzzzzz
;12 $zzzzzz,x
;13 $zz,s
;14 ($zz,s),y
;15 a
;16 #$zz #$zzzz
;17 pc+2+next byte
;18 'rien'
;19 pc+3+next word
;20 ($zzzz)
;21 ($zzzz,x)
;22 $zz,y

************************************************************

set_65802	movem.l	d0-d3/a0,-(a7)
		sf	fami_mode
		lea.l	f02_txt,a0
		moveq	#63,d0
		move.w	screen_height,d1
		subq.w	#1,d1
		moveq	#5-1,d3
.loop		move.b	(a0)+,d2
		bsr	print_char2
		addq.w	#1,d0
		dbf	d3,.loop
		movem.l	(a7)+,d0-d3/a0
		rts

set_65816	movem.l	d0-d3/a0,-(a7)
		st	fami_mode
		lea.l	f16_txt,a0
		moveq	#63,d0
		move.w	screen_height,d1
		subq.w	#1,d1
		moveq	#5-1,d3
.loop		move.b	(a0)+,d2
		bsr	print_char2
		addq.w	#1,d0
		dbf	d3,.loop
		movem.l	(a7)+,d0-d3/a0
		rts

f02_txt		dc.b "65802"
f16_txt		dc.b "65816"
		even

************************************************************
;65816 / 65802 disassembler

;a4=memory ptr (corrected)
;a0=ascII destination
;returns instr. len in d0

f_disassemble	movem.l	d1-d7/a1-a3,-(a7)

		move.l	#'    ',d0
		move.l	d0,(a0)
		move.l	d0,4(a0)

		moveq	#1,d7		;len of instruction

		move.b	(a4),d0
		lea.l	fami_list,a3
.seek		tst.b	1(a3)
		bne.b	.err
		cmp.b	(a3),d0
		beq.b	.found
		addq.l	#8,a3
		bra.b	.seek
.found
		bsr	do_famid
		bra.b	.ok

.err		move.l	#'??? ',(a0)+
		moveq	#1,d0
.ok		move.b	#$a,(a0)+
		clr.b	(a0)+
		move.l	d7,d0
		movem.l	(a7)+,d1-d7/a1-a3
		rts


do_famid	lea.l	2(a3),a1
		move.b	(a1)+,(a0)+		;copy instr. name
		move.b	(a1)+,(a0)+
		move.b	(a1)+,(a0)+
		move.b	#' ',(a0)+
		move.w	6(a3),d0
		and.w	#$ff,d0
		lsl.w	#2,d0
		jsr	.fmode(pc,d0.w)

;bit 15 a,x,y:8bit
;bit 14 a,x,y:16bit
		move.w	6(a3),d0
		btst	#15,d0
		beq.b	.no_spec1
		move.b	1(a4),d0
		and.b	#$20,d0
		beq.b	.no_spec1
		lea.l	.axy8,a1
.copy1		move.b	(a1)+,(a0)+
		bne.b	.copy1
		subq.l	#1,a0
		tst.b	fami_auto
		bne.b	.no_set
		bsr	set_65802
.no_set		rts
.no_spec1	btst	#14,d0
		beq.b	.no_spec2
		move.b	1(a4),d0
		and.b	#$20,d0
		beq.b	.no_spec2
		lea.l	.axy16,a1
.copy2		move.b	(a1)+,(a0)+
		bne.b	.copy2
		subq.l	#1,a0
		tst.b	fami_auto
		bne.b	.no_spec2
		bsr	set_65816
.no_spec2	rts

	OPT_OFF
.fmode		bra.w	fmode00
		bra.w	fmode01
		bra.w	fmode02
		bra.w	fmode03
		bra.w	fmode04
		bra.w	fmode05
		bra.w	fmode06
		bra.w	fmode07
		bra.w	fmode08
		bra.w	fmode09
		bra.w	fmode10
		bra.w	fmode11
		bra.w	fmode12
		bra.w	fmode13
		bra.w	fmode14
		bra.w	fmode15
		bra.w	fmode16
		bra.w	fmode17
		bra.w	fmode18
		bra.w	fmode19
		bra.w	fmode20
		bra.w	fmode21
		bra.w	fmode22
	OPT_ON

.axy8		dc.b "      A,X,Y:8bit",0
.axy16		dc.b "      A,X,Y:16bit",0
		cnop 0,4


;00 #$zz
fmode00		move.b	#'#',(a0)+
;01 $zz
fmode01
		move.b	#'$',(a0)+
		move.b	1(a4),d0
		addq.l	#1,d7
		moveq	#2,d1
		bsr	conv_hex
		add.l	d1,a0
		rts

;02 $zz,x
fmode02		move.b	#'$',(a0)+
		move.b	1(a4),d0
		addq.l	#1,d7
		moveq	#2,d1
		bsr	conv_hex
		add.l	d1,a0
		move.b	#',',(a0)+
		move.b	#'X',(a0)+
		rts

;03 ($zz)
fmode03		move.b	#'(',(a0)+
		move.b	#'$',(a0)+
		move.b	1(a4),d0
		addq.l	#1,d7
		moveq	#2,d1
		bsr	conv_hex
		add.l	d1,a0
		move.b	#')',(a0)+
		rts

;04 ($zz,x)
fmode04		move.b	#'(',(a0)+
		move.b	#'$',(a0)+
		move.b	1(a4),d0
		addq.l	#1,d7
		moveq	#2,d1
		bsr	conv_hex
		add.l	d1,a0
		move.b	#',',(a0)+
		move.b	#'X',(a0)+
		move.b	#')',(a0)+
		rts
;05 ($zz),y
fmode05		move.b	#'(',(a0)+
		move.b	#'$',(a0)+
		move.b	1(a4),d0
		addq.l	#1,d7
		moveq	#2,d1
		bsr	conv_hex
		add.l	d1,a0
		move.b	#')',(a0)+
		move.b	#',',(a0)+
		move.b	#'Y',(a0)+
		rts

;06 [$zz]
fmode06		move.b	#'[',(a0)+
		move.b	#'$',(a0)+
		move.b	1(a4),d0
		addq.l	#1,d7
		moveq	#2,d1
		bsr	conv_hex
		add.l	d1,a0
		move.b	#']',(a0)+
		rts

;07 [$zz],y
fmode07		move.b	#'[',(a0)+
		move.b	#'$',(a0)+
		move.b	1(a4),d0
		addq.l	#1,d7
		moveq	#2,d1
		bsr	conv_hex
		add.l	d1,a0
		move.b	#']',(a0)+
		move.b	#',',(a0)+
		move.b	#'Y',(a0)+
		rts

;08 $zzzz
fmode08		move.b	#'$',(a0)+
		move.b	2(a4),d0
		lsl.w	#8,d0
		move.b	1(a4),d0
		addq.l	#2,d7
		moveq	#4,d1
		bsr	conv_hex
		add.l	d1,a0
		rts

;09 $zzzz,x
fmode09		move.b	#'$',(a0)+
		move.b	2(a4),d0
		lsl.w	#8,d0
		move.b	1(a4),d0
		addq.l	#2,d7
		moveq	#4,d1
		bsr	conv_hex
		add.l	d1,a0
		move.b	#',',(a0)+
		move.b	#'X',(a0)+
		rts

;10 $zzzz,y
fmode10		move.b	#'$',(a0)+
		move.b	2(a4),d0
		lsl.w	#8,d0
		move.b	1(a4),d0
		addq.l	#2,d7
		moveq	#4,d1
		bsr	conv_hex
		add.l	d1,a0
		move.b	#',',(a0)+
		move.b	#'Y',(a0)+
		rts


;11 $zzzzzz
fmode11		move.b	#'$',(a0)+
		move.b	3(a4),d0
		lsl.l	#8,d0
		move.b	2(a4),d0
		lsl.l	#8,d0
		move.b	1(a4),d0
		addq.l	#3,d7
		moveq	#6,d1
		bsr	conv_hex
		add.l	d1,a0
		rts

;12 $zzzzzz,x
fmode12		move.b	#'$',(a0)+
		move.b	3(a4),d0
		lsl.l	#8,d0
		move.b	2(a4),d0
		lsl.l	#8,d0
		move.b	1(a4),d0
		addq.l	#3,d7
		moveq	#6,d1
		bsr	conv_hex
		add.l	d1,a0
		move.b	#',',(a0)+
		move.b	#'X',(a0)+
		rts

;13 $zz,s
fmode13
		move.b	#'$',(a0)+
		move.b	1(a4),d0
		addq.l	#1,d7
		moveq	#2,d1
		bsr	conv_hex
		add.l	d1,a0
		move.b	#',',(a0)+
		move.b	#'S',(a0)+
		rts

;14 ($zz,s),y
fmode14		move.b	#'(',(a0)+
		move.b	#'$',(a0)+
		move.b	1(a4),d0
		addq.l	#1,d7
		moveq	#2,d1
		bsr	conv_hex
		add.l	d1,a0
		move.b	#',',(a0)+
		move.b	#'S',(a0)+
		move.b	#')',(a0)+
		move.b	#',',(a0)+
		move.b	#'Y',(a0)+
		rts

;15 a
fmode15		move.b	#'A',(a0)+
		rts

;23 #$zz #$zzzz
fmode16		move.b	#'#',(a0)+
		move.b	#'$',(a0)+
		tst.b	fami_mode
		bne.b	.do_16
		move.b	1(a4),d0
		addq.l	#1,d7
		moveq	#2,d1
		bsr	conv_hex
		add.l	d1,a0
		rts

.do_16		move.b	2(a4),d0
		lsl.w	#8,d0
		move.b	1(a4),d0
		addq.l	#2,d7
		moveq	#4,d1
		bsr	conv_hex
		add.l	d1,a0
		rts


;17 pc+2+next byte
fmode17		move.l	a4,d0
		addq.l	#2,d0
		move.b	1(a4),d1
		addq.l	#1,d7
		ext.w	d1
		ext.l	d1
		add.l	d1,d0
		moveq	#6,d1
		bsr	conv_hex
		add.l	d1,a0
		rts

;18 'rien'
fmode18		rts

;19 pc+3+next word
fmode19		move.l	a4,d0
		addq.l	#3,d0
		move.b	2(a4),d1
		lsl.w	#8,d1
		move.b	1(a4),d1
		addq.l	#2,d7
		ext.l	d1
		add.l	d1,d0
		moveq	#6,d1
		bsr	conv_hex
		add.l	d1,a0
		rts

;20 ($zzzz)
fmode20		move.b	#'(',(a0)+
		move.b	#'$',(a0)+
		move.b	2(a4),d0
		lsl.w	#8,d0
		move.b	1(a4),d0
		addq.l	#2,d7
		moveq	#4,d1
		bsr	conv_hex
		add.l	d1,a0
		move.b	#')',(a0)+
		rts

;21 ($zzzz,x)
fmode21		move.b	#'(',(a0)+
		move.b	#'$',(a0)+
		move.b	2(a4),d0
		lsl.w	#8,d0
		move.b	1(a4),d0
		addq.l	#2,d7
		moveq	#4,d1
		bsr	conv_hex
		add.l	d1,a0
		move.b	#',',(a0)+
		move.b	#'X',(a0)+
		move.b	#')',(a0)+
		rts

;22 $zz,y
fmode22		move.b	#'$',(a0)+
		move.b	1(a4),d0
		addq.l	#1,d7
		moveq	#2,d1
		bsr	conv_hex
		add.l	d1,a0
		move.b	#',',(a0)+
		move.b	#'Y',(a0)+
		rts


fami_list	dc.b $e2,0		;byte of instruction,0
		dc.b 'SEP',0		;ascII of instruction
		dc.w 0+$8000		;addr. mode

		dc.b $69,0
		dc.b 'ADC',0
		dc.w 16

		dc.b $65,0
		dc.b 'ADC',0
		dc.w 01

		dc.b $75,0
		dc.b 'ADC',0
		dc.w 02

		dc.b $72,0
		dc.b 'ADC',0
		dc.w 03

		dc.b $61,0
		dc.b 'ADC',0
		dc.w 04

		dc.b $71,0
		dc.b 'ADC',0
		dc.w 05

		dc.b $67,0
		dc.b 'ADC',0
		dc.w 06

		dc.b $77,0
		dc.b 'ADC',0
		dc.w 07

		dc.b $6D,0
		dc.b 'ADC',0
		dc.w 08

		dc.b $7D,0
		dc.b 'ADC',0
		dc.w 09

		dc.b $79,0
		dc.b 'ADC',0
		dc.w 10

		dc.b $6F,0
		dc.b 'ADC',0
		dc.w 11

		dc.b $7F,0
		dc.b 'ADC',0
		dc.w 12

		dc.b $63,0
		dc.b 'ADC',0
		dc.w 13

		dc.b $73,0
		dc.b 'ADC',0
		dc.w 14

		dc.b $29,0
		dc.b 'AND',0
		dc.w 16

		dc.b $25,0
		dc.b 'AND',0
		dc.w 01

		dc.b $35,0
		dc.b 'AND',0
		dc.w 02

		dc.b $32,0
		dc.b 'AND',0
		dc.w 03

		dc.b $21,0
		dc.b 'AND',0
		dc.w 04

		dc.b $31,0
		dc.b 'AND',0
		dc.w 05

		dc.b $27,0
		dc.b 'AND',0
		dc.w 06

		dc.b $37,0
		dc.b 'AND',0
		dc.w 07

		dc.b $2D,0
		dc.b 'AND',0
		dc.w 08

		dc.b $3D,0
		dc.b 'AND',0
		dc.w 09

		dc.b $39,0
		dc.b 'AND',0
		dc.w 10

		dc.b $2F,0
		dc.b 'AND',0
		dc.w 11

		dc.b $3F,0
		dc.b 'AND',0
		dc.w 12

		dc.b $23,0
		dc.b 'AND',0
		dc.w 13

		dc.b $33,0
		dc.b 'AND',0
		dc.w 14

		dc.b $0A,0
		dc.b 'ASL',0
		dc.w 15

		dc.b $06,0
		dc.b 'ASL',0
		dc.w 01

		dc.b $16,0
		dc.b 'ASL',0
		dc.w 02

		dc.b $0E,0
		dc.b 'ASL',0
		dc.w 08

		dc.b $1E,0
		dc.b 'ASL',0
		dc.w 09

		dc.b $89,0
		dc.b 'BIT',0
		dc.w 16

		dc.b $24,0
		dc.b 'BIT',0
		dc.w 01

		dc.b $34,0
		dc.b 'BIT',0
		dc.w 02

		dc.b $2C,0
		dc.b 'BIT',0
		dc.w 08

		dc.b $3C,0
		dc.b 'BIT',0
		dc.w 09

		dc.b $C9,0
		dc.b 'CMP',0
		dc.w 16

		dc.b $C5,0
		dc.b 'CMP',0
		dc.w 01

		dc.b $D5,0
		dc.b 'CMP',0
		dc.w 02

		dc.b $D2,0
		dc.b 'CMP',0
		dc.w 03

		dc.b $C1,0
		dc.b 'CMP',0
		dc.w 04

		dc.b $D1,0
		dc.b 'CMP',0
		dc.w 05

		dc.b $C7,0
		dc.b 'CMP',0
		dc.w 06

		dc.b $D7,0
		dc.b 'CMP',0
		dc.w 07

		dc.b $CD,0
		dc.b 'CMP',0
		dc.w 08

		dc.b $DD,0
		dc.b 'CMP',0
		dc.w 09

		dc.b $D9,0
		dc.b 'CMP',0
		dc.w 10

		dc.b $CF,0
		dc.b 'CMP',0
		dc.w 11

		dc.b $DF,0
		dc.b 'CMP',0
		dc.w 12

		dc.b $C3,0
		dc.b 'CMP',0
		dc.w 13

		dc.b $D3,0
		dc.b 'CMP',0
		dc.w 14

		dc.b $E0,0
		dc.b 'CPX',0
		dc.w 16

		dc.b $E4,0
		dc.b 'CPX',0
		dc.w 01

		dc.b $EC,0
		dc.b 'CPX',0
		dc.w 08

		dc.b $C0,0
		dc.b 'CPY',0
		dc.w 16

		dc.b $C4,0
		dc.b 'CPY',0
		dc.w 01

		dc.b $CC,0
		dc.b 'CPY',0
		dc.w 08

		dc.b $3A,0
		dc.b 'DEC',0
		dc.w 15

		dc.b $C6,0
		dc.b 'DEC',0
		dc.w 01

		dc.b $D6,0
		dc.b 'DEC',0
		dc.w 02

		dc.b $CE,0
		dc.b 'DEC',0
		dc.w 08

		dc.b $DE,0
		dc.b 'DEC',0
		dc.w 09

		dc.b $49,0
		dc.b 'EOR',0
		dc.w 16

		dc.b $45,0
		dc.b 'EOR',0
		dc.w 01

		dc.b $55,0
		dc.b 'EOR',0
		dc.w 02

		dc.b $52,0
		dc.b 'EOR',0
		dc.w 03

		dc.b $41,0
		dc.b 'EOR',0
		dc.w 04

		dc.b $51,0
		dc.b 'EOR',0
		dc.w 05

		dc.b $47,0
		dc.b 'EOR',0
		dc.w 06

		dc.b $57,0
		dc.b 'EOR',0
		dc.w 07

		dc.b $4D,0
		dc.b 'EOR',0
		dc.w 08

		dc.b $5D,0
		dc.b 'EOR',0
		dc.w 09

		dc.b $59,0
		dc.b 'EOR',0
		dc.w 10

		dc.b $4F,0
		dc.b 'EOR',0
		dc.w 11

		dc.b $5F,0
		dc.b 'EOR',0
		dc.w 12

		dc.b $43,0
		dc.b 'EOR',0
		dc.w 13

		dc.b $53,0
		dc.b 'EOR',0
		dc.w 14

		dc.b $1A,0
		dc.b 'INC',0
		dc.w 15

		dc.b $E6,0
		dc.b 'INC',0
		dc.w 01

		dc.b $F6,0
		dc.b 'INC',0
		dc.w 02

		dc.b $EE,0
		dc.b 'INC',0
		dc.w 08

		dc.b $FE,0
		dc.b 'INC',0
		dc.w 09

		dc.b $A9,0
		dc.b 'LDA',0
		dc.w 16

		dc.b $A5,0
		dc.b 'LDA',0
		dc.w 01

		dc.b $B5,0
		dc.b 'LDA',0
		dc.w 02

		dc.b $B2,0
		dc.b 'LDA',0
		dc.w 03

		dc.b $A1,0
		dc.b 'LDA',0
		dc.w 04

		dc.b $B1,0
		dc.b 'LDA',0
		dc.w 05

		dc.b $A7,0
		dc.b 'LDA',0
		dc.w 06

		dc.b $B7,0
		dc.b 'LDA',0
		dc.w 07

		dc.b $AD,0
		dc.b 'LDA',0
		dc.w 08

		dc.b $BD,0
		dc.b 'LDA',0
		dc.w 09

		dc.b $B9,0
		dc.b 'LDA',0
		dc.w 10

		dc.b $AF,0
		dc.b 'LDA',0
		dc.w 11

		dc.b $BF,0
		dc.b 'LDA',0
		dc.w 12

		dc.b $A3,0
		dc.b 'LDA',0
		dc.w 13

		dc.b $B3,0
		dc.b 'LDA',0
		dc.w 14

		dc.b $A2,0
		dc.b 'LDX',0
		dc.w 16

		dc.b $A6,0
		dc.b 'LDX',0
		dc.w 01

		dc.b $B6,0
		dc.b 'LDX',0
		dc.w 22

		dc.b $AE,0
		dc.b 'LDX',0
		dc.w 08

		dc.b $BE,0
		dc.b 'LDX',0
		dc.w 10

		dc.b $A0,0
		dc.b 'LDY',0
		dc.w 16

		dc.b $A4,0
		dc.b 'LDY',0
		dc.w 01

		dc.b $B4,0
		dc.b 'LDY',0
		dc.w 02

		dc.b $AC,0
		dc.b 'LDY',0
		dc.w 08

		dc.b $BC,0
		dc.b 'LDY',0
		dc.w 09

		dc.b $46,0
		dc.b 'LSR',0
		dc.w 01

		dc.b $56,0
		dc.b 'LSR',0
		dc.w 02

		dc.b $4E,0
		dc.b 'LSR',0
		dc.w 08

		dc.b $5E,0
		dc.b 'LSR',0
		dc.w 09

		dc.b $4A,0
		dc.b 'LSR',0
		dc.w 15

		dc.b $09,0
		dc.b 'ORA',0
		dc.w 16

		dc.b $05,0
		dc.b 'ORA',0
		dc.w 01

		dc.b $15,0
		dc.b 'ORA',0
		dc.w 02

		dc.b $12,0
		dc.b 'ORA',0
		dc.w 03

		dc.b $01,0
		dc.b 'ORA',0
		dc.w 04

		dc.b $11,0
		dc.b 'ORA',0
		dc.w 05

		dc.b $07,0
		dc.b 'ORA',0
		dc.w 06

		dc.b $17,0
		dc.b 'ORA',0
		dc.w 07

		dc.b $0D,0
		dc.b 'ORA',0
		dc.w 08

		dc.b $1D,0
		dc.b 'ORA',0
		dc.w 09

		dc.b $19,0
		dc.b 'ORA',0
		dc.w 10

		dc.b $0F,0
		dc.b 'ORA',0
		dc.w 11

		dc.b $1F,0
		dc.b 'ORA',0
		dc.w 12

		dc.b $03,0
		dc.b 'ORA',0
		dc.w 13

		dc.b $13,0
		dc.b 'ORA',0
		dc.w 14

		dc.b $2A,0
		dc.b 'ROL',0
		dc.w 15

		dc.b $26,0
		dc.b 'ROL',0
		dc.w 01

		dc.b $36,0
		dc.b 'ROL',0
		dc.w 02

		dc.b $2E,0
		dc.b 'ROL',0
		dc.w 08

		dc.b $3E,0
		dc.b 'ROL',0
		dc.w 09

		dc.b $6A,0
		dc.b 'ROR',0
		dc.w 15

		dc.b $66,0
		dc.b 'ROR',0
		dc.w 01

		dc.b $76,0
		dc.b 'ROR',0
		dc.w 02

		dc.b $6E,0
		dc.b 'ROR',0
		dc.w 08

		dc.b $7E,0
		dc.b 'ROR',0
		dc.w 09

		dc.b $E9,0
		dc.b 'SBC',0
		dc.w 16

		dc.b $E5,0
		dc.b 'SBC',0
		dc.w 01

		dc.b $F5,0
		dc.b 'SBC',0
		dc.w 02

		dc.b $F2,0
		dc.b 'SBC',0
		dc.w 03

		dc.b $E1,0
		dc.b 'SBC',0
		dc.w 04

		dc.b $F1,0
		dc.b 'SBC',0
		dc.w 05

		dc.b $E7,0
		dc.b 'SBC',0
		dc.w 06

		dc.b $F7,0
		dc.b 'SBC',0
		dc.w 07

		dc.b $ED,0
		dc.b 'SBC',0
		dc.w 08

		dc.b $FD,0
		dc.b 'SBC',0
		dc.w 09

		dc.b $F9,0
		dc.b 'SBC',0
		dc.w 10

		dc.b $EF,0
		dc.b 'SBC',0
		dc.w 11

		dc.b $FF,0
		dc.b 'SBC',0
		dc.w 12

		dc.b $E3,0
		dc.b 'SBC',0
		dc.w 13

		dc.b $F3,0
		dc.b 'SBC',0
		dc.w 14

		dc.b $85,0
		dc.b 'STA',0
		dc.w 01

		dc.b $95,0
		dc.b 'STA',0
		dc.w 02

		dc.b $92,0
		dc.b 'STA',0
		dc.w 03

		dc.b $81,0
		dc.b 'STA',0
		dc.w 04

		dc.b $91,0
		dc.b 'STA',0
		dc.w 05

		dc.b $87,0
		dc.b 'STA',0
		dc.w 06

		dc.b $97,0
		dc.b 'STA',0
		dc.w 07

		dc.b $8D,0
		dc.b 'STA',0
		dc.w 08

		dc.b $9D,0
		dc.b 'STA',0
		dc.w 09

		dc.b $99,0
		dc.b 'STA',0
		dc.w 10

		dc.b $8F,0
		dc.b 'STA',0
		dc.w 11

		dc.b $9F,0
		dc.b 'STA',0
		dc.w 12

		dc.b $83,0
		dc.b 'STA',0
		dc.w 13

		dc.b $93,0
		dc.b 'STA',0
		dc.w 14

		dc.b $86,0
		dc.b 'STX',0
		dc.w 01

		dc.b $96,0
		dc.b 'STX',0
		dc.w 22

		dc.b $8E,0
		dc.b 'STX',0
		dc.w 08

		dc.b $84,0
		dc.b 'STY',0
		dc.w 01

		dc.b $94,0
		dc.b 'STY',0
		dc.w 02

		dc.b $8C,0
		dc.b 'STY',0
		dc.w 08

		dc.b $64,0
		dc.b 'STZ',0
		dc.w 01

		dc.b $74,0
		dc.b 'STZ',0
		dc.w 02

		dc.b $9C,0
		dc.b 'STZ',0
		dc.w 08

		dc.b $9E,0
		dc.b 'STZ',0
		dc.w 09

		dc.b $14,0
		dc.b 'TRB',0
		dc.w 01

		dc.b $1C,0
		dc.b 'TRB',0
		dc.w 08

		dc.b $04,0
		dc.b 'TSB',0
		dc.w 01

		dc.b $0C,0
		dc.b 'TSB',0
		dc.w 08

		dc.b $C2,0
		dc.b 'REP',0
		dc.w 00+$4000

		dc.b $E2,0
		dc.b 'SEP',0
		dc.w 0+$8000

		dc.b $90,0
		dc.b 'BCC',0
		dc.w 17

		dc.b $B0,0
		dc.b 'BCS',0
		dc.w 17

		dc.b $F0,0
		dc.b 'BEQ',0
		dc.w 17

		dc.b $30,0
		dc.b 'BMI',0
		dc.w 17

		dc.b $D0,0
		dc.b 'BNE',0
		dc.w 17

		dc.b $10,0
		dc.b 'BPL',0
		dc.w 17

		dc.b $80,0
		dc.b 'BRA',0
		dc.w 17

		dc.b $50,0
		dc.b 'BVC',0
		dc.w 17

		dc.b $70,0
		dc.b 'BVS',0
		dc.w 17

		dc.b $18,0
		dc.b 'CLC',0
		dc.w 18

		dc.b $D8,0
		dc.b 'CLD',0
		dc.w 18

		dc.b $58,0
		dc.b 'CLI',0
		dc.w 18

		dc.b $B8,0
		dc.b 'CLV',0
		dc.w 18

		dc.b $CA,0
		dc.b 'DEX',0
		dc.w 18

		dc.b $88,0
		dc.b 'DEY',0
		dc.w 18

		dc.b $E8,0
		dc.b 'INX',0
		dc.w 18

		dc.b $C8,0
		dc.b 'INY',0
		dc.w 18

		dc.b $EA,0
		dc.b 'NOP',0
		dc.w 18

		dc.b $F4,0
		dc.b 'PEA',0
		dc.w 08

		dc.b $D4,0
		dc.b 'PEI',0
		dc.w 03

		dc.b $62,0
		dc.b 'PER',0
		dc.w 19

		dc.b $48,0
		dc.b 'PHA',0
		dc.w 18

		dc.b $8B,0
		dc.b 'PHB',0
		dc.w 18

		dc.b $0B,0
		dc.b 'PHD',0
		dc.w 18

		dc.b $4B,0
		dc.b 'PHK',0
		dc.w 18

		dc.b $08,0
		dc.b 'PHP',0
		dc.w 18

		dc.b $DA,0
		dc.b 'PHX',0
		dc.w 18

		dc.b $5A,0
		dc.b 'PHY',0
		dc.w 18

		dc.b $68,0
		dc.b 'PLA',0
		dc.w 18

		dc.b $AB,0
		dc.b 'PLB',0
		dc.w 18

		dc.b $2B,0
		dc.b 'PLD',0
		dc.w 18

		dc.b $28,0
		dc.b 'PLP',0
		dc.w 18

		dc.b $FA,0
		dc.b 'PLX',0
		dc.w 18

		dc.b $7A,0
		dc.b 'PLY',0
		dc.w 18

		dc.b $38,0
		dc.b 'SEC',0
		dc.w 18

		dc.b $F8,0
		dc.b 'SED',0
		dc.w 18

		dc.b $78,0
		dc.b 'SEI',0
		dc.w 18

		dc.b $AA,0
		dc.b 'TAX',0
		dc.w 18

		dc.b $A8,0
		dc.b 'TAY',0
		dc.w 18

		dc.b $5B,0
		dc.b 'TCD',0
		dc.w 18

		dc.b $1B,0
		dc.b 'TCS',0
		dc.w 18

		dc.b $7B,0
		dc.b 'TDC',0
		dc.w 18

		dc.b $1C,0
		dc.b 'TRB',0
		dc.w 08

		dc.b $14,0
		dc.b 'TRB',0
		dc.w 01

		dc.b $0C,0
		dc.b 'TSB',0
		dc.w 08

		dc.b $04,0
		dc.b 'TSB',0
		dc.w 01

		dc.b $3B,0
		dc.b 'TSC',0
		dc.w 18

		dc.b $BA,0
		dc.b 'TSX',0
		dc.w 18

		dc.b $8A,0
		dc.b 'TXA',0
		dc.w 18

		dc.b $9A,0
		dc.b 'TXS',0
		dc.w 18

		dc.b $9B,0
		dc.b 'TXY',0
		dc.w 18

		dc.b $98,0
		dc.b 'TYA',0
		dc.w 18

		dc.b $BB,0
		dc.b 'TYX',0
		dc.w 18

		dc.b $FB,0
		dc.b 'XCE',0
		dc.w 18

		dc.b $00,0
		dc.b 'BRK',0
		dc.w 00

		dc.b $82,0
		dc.b 'BRL',0
		dc.w 19

		dc.b $02,0
		dc.b 'COP',0
		dc.w 00

		dc.b $DC,0
		dc.b 'JML',0
		dc.w 20

		dc.b $4C,0
		dc.b 'JMP',0
		dc.w 08

		dc.b $6C,0
		dc.b 'JMP',0
		dc.w 20

		dc.b $7C,0
		dc.b 'JMP',0
		dc.w 21

		dc.b $5C,0
		dc.b 'JMP',0
		dc.w 11

		dc.b $22,0
		dc.b 'JSL',0
		dc.w 11

		dc.b $20,0
		dc.b 'JSR',0
		dc.w 08

		dc.b $FC,0
		dc.b 'JSR',0
		dc.w 21

		dc.b $54,0
		dc.b 'MVN',0
		dc.w 08

		dc.b $44,0
		dc.b 'MVP',0
		dc.w 08

		dc.b $C2,0
		dc.b 'REP',0
		dc.w 00+$4000

		dc.b $40,0
		dc.b 'RTI',0
		dc.w 18

		dc.b $6B,0
		dc.b 'RTL',0
		dc.w 18

		dc.b $60,0
		dc.b 'RTS',0
		dc.w 18

		dc.b $E2,0
		dc.b 'SEP',0
		dc.w 00+$8000

		dc.b $DB,0
		dc.b 'STP',0
		dc.w 18

		dc.b $CB,0
		dc.b 'WAI',0
		dc.w 18

		dc.b $EB,0
		dc.b 'XBA',0
		dc.w 18

		dc.w -1

;00 #$zz
;01 $zz
;02 $zz,x
;03 ($zz)
;04 ($zz,x)
;05 ($zz),y
;06 [$zz]
;07 [$zz],y
;08 $zzzz
;09 $zzzz,x
;10 $zzzz,y
;11 $zzzzzz
;12 $zzzzzz,x
;13 $zz,s
;14 ($zz,s),y
;15 a
;16 #$zz #$zzzz
;17 pc+2+next byte
;18 'rien'
;19 pc+3+next word
;20 ($zzzz)
;21 ($zzzz,x)
;22 $zz,y

;bit 15 a,x,y:8bit
;bit 14 a,x,y:16bit

