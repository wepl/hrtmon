
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


;-------------- gfx ripper --------------------------------

cmd_p		bsr	evaluate		;read picno
		bmi.w	illegal_val		;illegal expression ?
		beq.b	.okpara			;got one param ?
		moveq	#0,d0			;default picno
.okpara		tst.l	d0
		bmi.w	illegal_val
		cmp.w	#500,d0
		bge.w	illegal_val

		jsr	remove_pic

		jsr	analyse_copper

		lea.l	custom,a3

		move.w	$100(a3),d0		;calc depth
		btst	#15,d0
		sne	hires
		btst	#11,d0
		sne	ham
		moveq	#8,d1			;from BPLCON0
		btst	#4,d0			;8 plan ?
		bne.b	.go8plan
		rol.w	#4,d0
		and.w	#%111,d0
		move.w	d0,d1
		bne.b	.go8plan		;at least 1 plan ?
		moveq	#1,d1
.go8plan	move.w	d1,depth

		cmp.w	#6,d1
		seq	ehb
		move.w	$104(a3),d0
		btst	#9,d0			; kill ehb ?
		beq.b	.nokehb
		sf	ehb
.nokehb
		movem.w	$92(a3),d0-d1		;DDFSTRT,DDFSTOP
		move.w	$1fc(a3),d2		;FMODE
		and.w	#%11,d2
		cmp.w	#3,d2
		bne.b	.nolimit
		move.w	#$c0,d3
		cmp.w	d3,d1
		ble.b	.nolimit
		move.w	d3,d1
.nolimit	sub.w	d0,d1			;calc width of pic
		lsr.w	#3,d1			;from these
		addq.w	#1,d1			;3 registers
		cmp.w	#3,d2
		bne.b	.nof3
		addq.w	#2,d1
		bra.b	.okfmode
.nof3		tst.w	d2
		beq.b	.okfmode
		addq.w	#1,d1
.okfmode	lsl.w	#4,d1
		tst.b	hires
		beq.b	.nohi
		lsl.w	#1,d1
.nohi		move.w	d1,width
		move.w	#256,height		;height is fixed to 256

		movem.l	$e0(a3),d0-d7
		movem.l	d0-d7,bitplan		;set default bitplan address

		move.w	$108(a3),d1		;calc. modulo
		move.w	$1fc(a3),d0		;from FMODE
;		tst.b	hires
;		bne.b	.okfmodeb
		and.w	#%11,d0
		cmp.w	#3,d0
		bne.b	.nof3b
		addq.w	#8,d1
		bra.b	.okfmodeb
.nof3b		tst.w	d0
		beq.b	.okfmodeb
		addq.w	#4,d1
.okfmodeb	move.w	width,d0		;and from width
		sub.w	#320,d0
		asr.w	#3,d0
		add.w	d0,d1
		move.w	d1,modulo

		tst.w	height
		bgt.b	.okh2
		move.w	#256,height
.okh2		tst.w	width
		bgt.b	.okw2
		move.w	#320,width
.okw2
		bsr	restore_palette

		move.w	#$7f7f,dkey

;-------------- main loop -----------------------

.raster		move.l	$4(a6),d0		;wait next VBL
		lsr.l	#1,d0
		lsr.w	#7,d0
		cmp.w	#2,d0
		bne.b	.raster

		movem.l	bitplan,d0-d7
		movem.l	d0-d7,$e0(a6)
		move.w	modulo,d0
		move.w	d0,$108(a6)
		move.w	d0,$10a(a6)

		move.w	#$38,$92(a6)
		move.w	#$d0,$94(a6)
		move.w	#0,$102(a6)
		move.w	#%1100,$1fc(a6)
		cmp.b	#1,config_screen
		bne.b	.nontsc0
		move.w	#$0,$1dc(a6)
		bra.b	.okmode0
.nontsc0	cmp.b	#2,config_screen
		bne.b	.nomulti0
		move.w	#$0,$1dc(a6)
		bra.b	.okmode0
.nomulti0	move.w	#$20,$1dc(a6)
.okmode0
		move.w	#$2c81,$8e(a6)
		moveq	#0,d0			;for $1e4
		move.w	#$2c,d1
		add.w	height,d1
		cmp.b	#1,config_screen
		bne.b	.nontsc
		move.w	#200+$2c,d2
		bra.b	.okmode
.nontsc		move.w	#256+$2c,d2
.okmode		cmp.w	d2,d1			;max displayed height ?
		ble.b	.okmaxv
		move.w	d2,d1
.okmaxv		cmp.w	#$100,d1
		blt.b	.noVE8
		or.w	#$0100,d0
.noVE8		move.w	#$81,d2
		add.w	width,d2
		cmp.w	#320+$81,d2		;max displayed width ?
		blt.b	.okmaxh
		move.w	#320+$81,d2
.okmaxh		cmp.w	#$100,d2
		blt.b	.noHE8
		or.w	#$2000,d0
.noHE8		lsl.w	#8,d1
		and.w	#$ff,d2
		or.w	d2,d1
		move.w	d1,$90(a6)
		move.w	d0,$1e4(a6)

		move.w	#$2,$106(a6)		;sprites outside
		move.w	spr_color,d0		;sprite color bank 0-f
		move.w	d0,d1
		lsl.w	#4,d1
		or.w	d1,d0
		move.w	custom+$10c,d1
		and.w	#$ff00,d1
		or.w	d1,d0
		move.w	d0,$10c(a6)

		move.w	depth,d0
		cmp.w	#8,d0
		bne.b	.no8
		move.w	#$0010,d0
		bra.b	.go100
.no8		lsl.w	#8,d0
		lsl.w	#4,d0
.go100		or.w	#$0001,d0
		tst.b	ham
		beq.b	.noham
		or.w	#$800,d0
.noham		move.w	d0,$100(a6)

		bsr	do_param

		bsr	get_dkey

		lea.l	dkey,a0
		move.b	(a0),d0
		cmp.b	1(a0),d0		;cmp with old dkey
		beq.b	.nonew			;for repeat mode
		move.w	#20,dkey_rep
		move.b	(a0),1(a0)
		bra.b	.newkey
.nonew		subq.w	#1,dkey_rep
		bpl.w	.raster
		clr.w	dkey_rep

.newkey
;-------------- L+R Shift ------------------
		cmp.b	#$60,d0
		beq.b	.shift
		cmp.b	#$61,d0
		bne.b	.noshift
.shift		st	dkey_shift
.noshift	cmp.b	#$60+$80,d0
		beq.b	.shift2
		cmp.b	#$61+$80,d0
		bne.b	.noshift2
.shift2		sf	dkey_shift
.noshift2
;-------------- f10/esc exit gfx-ripper ----
		cmp.b	#$45+$80,d0
		beq.b	.exit
		cmp.b	#$59,d0
		bne.b	.no_f10
.exit		move.b	#$7f,(a0)
		bra.w	.end_show
.no_f10
;-------------- + add 1 bitplan ------------
		cmp.b	#$0c,d0
		bne.b	.noplus
		cmp.w	#8,depth
		beq.b	.noplus
		addq.w	#1,depth
.noplus
;-------------- - sub 1 bitplan ------------
		cmp.b	#$0b,d0
		bne.b	.nominus
		cmp.w	#1,depth
		beq.b	.nominus
		subq.w	#1,depth
.nominus
;-------------- down move pic down ---------
		cmp.b	#$4d,d0
		bne.b	.nodown
		tst.b	set_height
		beq.b	.noset
		addq.w	#1,tmp_height
		sf	dkey_shift
.noset		moveq	#40,d1
		add.w	modulo,d1
		ext.l	d1
		tst.b	dkey_shift
		beq.b	.nos1
		muls	height,d1
.nos1		move.b	bitplan_lock,d2
		lea.l	bitplan,a1
		moveq	#8-1,d3
.plan		lsr.b	#1,d2
		bcs.b	.lock
		add.l	d1,(a1)
		move.l	max_chip,d5
		subq.l	#2,d5
		and.l	d5,(a1)
.lock		addq.l	#4,a1
		dbf	d3,.plan
.nodown
;-------------- up move pic up -------------
		cmp.b	#$4c,d0
		bne.b	.noup
		tst.b	set_height
		beq.b	.noset2
		tst.w	tmp_height
		beq.b	.noup
		subq.w	#1,tmp_height
		sf	dkey_shift
.noset2		moveq	#40,d1
		add.w	modulo,d1
		ext.l	d1
		tst.b	dkey_shift
		beq.b	.nos2
		muls	height,d1
.nos2		move.b	bitplan_lock,d2
		lea.l	bitplan,a1
		moveq	#8-1,d3
.plan2		lsr.b	#1,d2
		bcs.b	.lock2
		sub.l	d1,(a1)
		move.l	max_chip,d5
		subq.l	#2,d5
		and.l	d5,(a1)
.lock2		addq.l	#4,a1
		dbf	d3,.plan2
.noup
;-------------- right move pic right -------
		cmp.b	#$4e,d0
		bne.b	.noright
		moveq	#2,d1
		move.b	bitplan_lock,d2
		lea.l	bitplan,a1
		moveq	#8-1,d3
.plan3		lsr.b	#1,d2
		bcs.b	.lock3
		add.l	d1,(a1)
		move.l	max_chip,d5
		subq.l	#2,d5
		and.l	d5,(a1)
.lock3		addq.l	#4,a1
		dbf	d3,.plan3
.noright
;-------------- left move pic left ---------
		cmp.b	#$4f,d0
		bne.b	.noleft
		moveq	#2,d1
		move.b	bitplan_lock,d2
		lea.l	bitplan,a1
		moveq	#8-1,d3
.plan4		lsr.b	#1,d2
		bcs.b	.lock4
		sub.l	d1,(a1)
		move.l	max_chip,d5
		subq.l	#2,d5
		and.l	d5,(a1)
.lock4		addq.l	#4,a1
		dbf	d3,.plan4
.noleft
;-------------- 1-8 (un)lock bitplan -------
		cmp.b	#$01,d0
		blt.b	.nolock
		cmp.b	#$08,d0
		bgt.b	.nolock
		moveq	#0,d1
		move.b	d0,d1
		subq.b	#1,d1
		bchg	d1,bitplan_lock
		move.b	#$7f,(a0)
.nolock
;-------------- M INC(modulo) --------------
		cmp.b	#$37,d0
		bne.b	.nom
		addq.w	#2,modulo
.nom
;-------------- N DEC(modulo) --------------
		cmp.b	#$36,d0
		bne.b	.non
		subq.w	#2,modulo
.non
;-------------- , CLR(modulo) --------------
		cmp.b	#$38,d0
		bne.b	.novir
		clr.w	modulo
		move.b	#$7f,(a0)
.novir
;-------------- Q DEC(width) ---------------
		cmp.b	#$10,d0
		bne.b	.noq
		cmp.w	#16,width
		beq.b	.noq
		sub.w	#16,width
.noq
;-------------- W INC(width) ---------------
		cmp.b	#$11,d0
		bne.b	.now
		cmp.w	#2048,width
		beq.b	.now
		add.w	#16,width
.now
;-------------- A DEC(height) ---------------
		cmp.b	#$20,d0
		bne.b	.noa
		cmp.w	#1,height
		beq.b	.noa
		subq.w	#1,height
.noa
;-------------- S INC(height) ---------------
		cmp.b	#$21,d0
		bne.b	.nos
		cmp.w	#2048,height
		beq.b	.nos
		addq.w	#1,height
.nos
;-------------- R reset bitplans -----------
		cmp.b	#$13,d0
		bne.b	.nor
		lea.l	bitplan,a1
		move.l	(a1)+,d1
		moveq	#7-1,d2
.reset		move.l	d1,(a1)+
		dbf	d2,.reset
		move.b	#$7f,(a0)
.nor
;-------------- Del INC(spr_color) ---------
		cmp.b	#$46,d0
		bne.b	.noDel
		addq.w	#1,spr_color
		and.w	#$f,spr_color
.noDel
;-------------- Help sprite on/off ---------
		cmp.b	#$5f,d0
		bne.b	.noHelp
		bsr	flip_spr
.noHelp
;-------------- H HAM on/off ---------------
		cmp.b	#$25,d0
		bne.b	.noh
		not.b	ham
.noh
;-------------- E EHB on/off ---------------
		cmp.b	#$12,d0
		bne.b	.noe
		not.b	ehb
.noe
;-------------- F1 set picture height ------
		cmp.b	#$50,d0
		bne.b	.nof1
		not.b	set_height
		bne.b	.startset
		move.w	tmp_height,d1
		bgt.b	.oktmpheight
		moveq	#1,d1
.oktmpheight	move.w	d1,height
		lea.l	bitplan,a1
		lea.l	tmp_bitplan,a2
		moveq	#8-1,d1
.copyb2		move.l	(a2)+,(a1)+		;keep top of screen ptr
		dbf	d1,.copyb2
		bra.b	.nof1
.startset	clr.w	tmp_height
		lea.l	bitplan,a1
		lea.l	tmp_bitplan,a2
		moveq	#8-1,d1
.copyb		move.l	(a1)+,(a2)+		;keep top of screen ptr
		dbf	d1,.copyb
.nof1
;-------------- C (enable cheat sequence) --

		cmp.b	#$33,d0
		bne.b	.noc
		lea.l	cheat_cnt,a1
		cmp.b	#2,(a1)
		beq.b	.okcheat
		sf	(a1)
		bra.b	.noc
.okcheat	sf	(a1)
		not.b	cheat
.noc
		bra.w	.raster

;------------------------------------------------

.end_show
		move.b	#$7f,dkey
		move.w	#8,$9c(a6)
		clr.w	nb_keys		;flush keyboard buffer
		move.b	#$7f,key_prev

		sf	set_height

		tst.b	gfx_param	;sprite was ON ?
		beq.b	.noflip
		bsr	flip_spr	;yes
.noflip
		move.w	#$0120,$96(a6)	;sprites DMA and bitplan OFF
		jsr	set_pic
		move.w	#$8100,$96(a6)		;bitplan DMA on

		lea.l	plan_txt,a0
		bsr	print

		lea.l	bitplan,a1
		move.w	depth,d2
		subq.w	#1,d2
.pr		move.l	(a1)+,d0
		moveq	#8,d1
		bsr	print_hex
		move.b	#$20,d0
		bsr	.printchar
		dbf	d2,.pr
		move.b	#$a,d0
		bsr	.printchar

		lea.l	width_txt,a0
		bsr	print
		moveq	#0,d0
		move.w	width,d0
		moveq	#4,d1
		bsr	print_decCR

		lea.l	height_txt,a0
		bsr	print
		move.w	height,d0
		moveq	#4,d1
		bsr	print_decCR

		lea.l	depth_txt,a0
		bsr	print
		move.w	depth,d0
		moveq	#4,d1
		bsr	print_decCR

		st	p_used		;signal to allow sp command
		jmp	end_command

;-> d0=char
.printchar	move.l	a0,-(a7)
		lea.l	general_txt,a0
		move.b	d0,(a0)
		sf	1(a0)
		bsr	print
		move.l	(a7)+,a0
		rts

plan_txt	dc.b "Bitplans address : ",$a,0
width_txt	dc.b "Picture width  : ",0
height_txt	dc.b "Picture height : ",0
depth_txt	dc.b "Picture depth  : ",0

		cnop 0,4

**************************************************************************
;-------------- display parameters with sprite and mouse -----------------

do_param	tst.b	gfx_param
		beq.w	.noparam

		bsr	init_mouse
		bsr	init_spr
		lea.l	$120(a6),a0
		move.l	#0,(a0)+		;sprite 0
		move.l	#sprite_len,(a0)+	;sprite 1
		move.l	#empty_spr-sprite1,d0
		moveq	#6-1,d1
.clr		move.l	d0,(a0)+		;don't use sprites 2-7
		dbf	d1,.clr

		lea.l	($0+16).w,a0
		lea.l	bitplan,a1
		moveq	#4-1,d3
		moveq	#1,d0			;xpos
		moveq	#0,d1			;ypos
.nplan		move.l	(a1)+,d2
		bsr	SprPrintLong
		addq.w	#1,d1
		dbf	d3,.nplan

		move.l	#(sprite2+16-sprite1),a0
		moveq	#4-1,d3
		moveq	#1,d0			;xpos
		moveq	#0,d1			;ypos
.nplan2		move.l	(a1)+,d2
		bsr	SprPrintLong
		addq.w	#1,d1
		dbf	d3,.nplan2

		move.b	bitplan_lock,d3
		lea.l	($0+16).w,a0
		moveq	#0,d0
		moveq	#0,d1
		moveq	#4-1,d4
.lock1		moveq	#19,d2			;spc
		lsr.b	#1,d3
		bcc.b	.nolock1
		moveq	#18,d2			;L
.nolock1	bsr	print_spr_char
		addq.w	#1,d1
		dbf	d4,.lock1

		move.l	#(sprite2-sprite1+16),a0
		moveq	#0,d0
		moveq	#0,d1
		moveq	#4-1,d4
.lock2		moveq	#19,d2			;spc
		lsr.b	#1,d3
		bcc.b	.nolock2
		moveq	#18,d2			;L
.nolock2	bsr	print_spr_char
		addq.w	#1,d1
		dbf	d4,.lock2

		move.l	#(sprite2-sprite1+16),a0

		moveq	#10,d0
		moveq	#0,d1
		moveq	#16,d2			;W
		bsr	print_spr_char

		moveq	#12,d0
		moveq	#0,d1
		move.w	width,d2
		bsr	SprPrintDec

		moveq	#10,d0
		moveq	#1,d1
		moveq	#17,d2			;H
		bsr	print_spr_char

		moveq	#12,d0
		moveq	#1,d1
		move.w	height,d2
		tst.b	set_height
		beq.b	.noset
		move.w	tmp_height,d2
.noset		bsr	SprPrintDec

		moveq	#10,d0
		moveq	#2,d1
		moveq	#13,d2			;D
		bsr	print_spr_char

		moveq	#19,d2
		tst.b	ham
		beq.b	.noham
		moveq	#17,d2			;H
.noham		moveq	#10,d0
		moveq	#3,d1
		bsr	print_spr_char

		moveq	#19,d2
		tst.b	ehb
		beq.b	.noehb
		moveq	#$e,d2			;E
.noehb		moveq	#12,d0
		moveq	#3,d1
		bsr	print_spr_char


		moveq	#15,d0
		moveq	#2,d1
		move.w	depth,d2
		bsr	print_spr_char

.noparam	rts

;-> d0=xpos
;-> d1=ypos
;-> d2=long
;-> a0=sprite

SprPrintLong	movem.l	d0-d4,-(a7)
		move.l	d2,d3
		moveq	#8-1,d4
.loop		rol.l	#4,d3
		move.l	d3,d2
		and.w	#$f,d2
		bsr	print_spr_char
		addq.w	#1,d0
		dbf	d4,.loop
		movem.l	(a7)+,d0-d4
		rts

;-> d0=xpos
;-> d1=ypos
;-> d2=word
;-> a0=sprite

SprPrintWord	movem.l	d0-d4,-(a7)
		move.l	d2,d3
		moveq	#4-1,d4
.loop		rol.w	#4,d3
		move.w	d3,d2
		and.w	#$f,d2
		bsr	print_spr_char
		addq.w	#1,d0
		dbf	d4,.loop
		movem.l	(a7)+,d0-d4
		rts

;-> d0=xpos
;-> d1=ypos
;-> d2=number xxxx
;-> a0=sprite

SprPrintDec	movem.l	d0-d4,-(a7)
		move.l	d2,d3

		divu	#1000,d3
		move.w	d3,d2
		bsr	print_spr_char

		addq.w	#1,d0
		clr.w	d3
		swap	d3
		divu	#100,d3
		move.w	d3,d2
		bsr	print_spr_char

		addq.w	#1,d0
		clr.w	d3
		swap	d3
		divu	#10,d3
		move.w	d3,d2
		bsr	print_spr_char

		addq.w	#1,d0
		swap	d3
		move.w	d3,d2
		bsr	print_spr_char

		movem.l	(a7)+,d0-d4
		rts


;-> d0=x pos
;-> d1=y pos
;-> d2=char
;-> a0=top left of sprite (64 wide)

print_spr_char	movem.l	d0-d4/a0-a1,-(a7)
		ext.l	d0
		mulu	#6*16,d1
		add.l	d1,a0
		lea.l	sprfont,a1
		moveq	#5-1,d4
		move.w	d0,d3
		lsr.w	#1,d3
		add.w	d3,a0
.next		move.b	(a1,d2.w),d3
		move.b	(a0),d1
		btst	#0,d0
		beq.b	.noshift
		lsr.w	#4,d3
		and.b	#$f0,d1
		bra.b	.okshift
.noshift	and.b	#$0f,d1
.okshift	or.w	d3,d1
		move.b	d1,(a0)
		lea.l	16(a0),a0	;next line in sprite
		lea.l	160/8(a1),a1	;next line in font
		dbf	d4,.next
		movem.l	(a7)+,d0-d4/a0-a1
		rts

sprfont		incbin sprfont.raw
;"0123456789abcdefwhl "

init_mouse:
		btst	#6,$bfe001
		seq	left
		btst	#10-8,$16(a6)
		seq	right

		moveq	#0,d5
		tst.b	mouse_speed
		beq.b	pas_slow
		moveq	#1,d5

pas_slow:	move.w	$a(a6),d0
		move.w	d0,d1
		lsr.w	#8,d1		;y
		and.w	#$ff,d0		;x

		move.w	d0,d2
		sub.w	old_mx,d2
		cmp.w	#127,d2
		blt.b	mo_x1
		move.w	d0,d2
		sub.w	#256,d2
		sub.w	old_mx,d2
		asr.w	d5,d2
		add.w	d2,x_spr
		move.w	d0,old_mx
		bra.b	mo_x2

mo_x1:		cmp.w	#-127,d2
		bgt.b	mo_x3
		move.w	d0,d2
		add.w	#256,d2
		sub.w	old_mx,d2
		asr.w	d5,d2
		add.w	d2,x_spr
		move.w	d0,old_mx
		bra.b	mo_x2

mo_x3:		asr.w	d5,d2
		add.w	d2,x_spr
		move.w	d0,old_mx

mo_x2:		move.w	d1,d2
		sub.w	old_my,d2
		cmp.w	#127,d2
		blt.b	mo_y1
		move.w	d1,d2
		sub.w	#256,d2
		sub.w	old_my,d2
		asr.w	d5,d2
		add.w	d2,y_spr
		move.w	d1,old_my
		bra.b	mo_y2

mo_y1:		cmp.w	#-127,d2
		bgt.b	mo_y3
		move.w	d1,d2
		add.w	#256,d2
		sub.w	old_my,d2
		asr.w	d5,d2
		add.w	d2,y_spr
		move.w	d1,old_my
		bra.b	mo_y2

mo_y3:		asr.w	d5,d2
		add.w	d2,y_spr
		move.w	d1,old_my
mo_y2:		rts

;-----------------------------------------------

init_spr:
		moveq	#0,d1
		move.w	x_spr,d0
		tst.w	d0
		bpl.b	.so1
		clr.w	d0
		move.w	d0,x_spr
.so1		cmp.w	#320-80,d0
		blt.b	.so2
		move.w	#319-80,d0
		move.w	d0,x_spr
.so2
		lea.l	$0.w,a1
		move.w	x_spr,d0
		move.w	y_spr,d2
		bsr	set_sprpos

		move.l	#(sprite2-sprite1),a1
		move.w	x_spr,d0
		move.w	y_spr,d2
		add.w	#44,d0
		bsr	set_sprpos

		rts

set_sprpos	add.w	#$81-8,d0
		lsr.w	#1,d0
		bcc.b	.so0
		moveq	#1,d1
.so0		move.b	d0,1(a1)

		move.w	d2,d0
		bpl.b	.so3
		clr.w	d0
		clr.w	y_spr
.so3		cmp.w	#256,d0
		blt.b	.so4
		move.w	#255,d0
		move.w	d0,y_spr
.so4		add.w	#$2c-2,d0
		cmp.w	#256,d0
		blt.b	.ok_yspr1
		or.b	#%100,d1
.ok_yspr1	move.b	d0,(a1)
		add.w	#4*6,d0
		cmp.w	#256,d0
		blt.b	.ok_yspr2
		or.b	#%10,d1
.ok_yspr2	move.b	d0,8(a1)
		move.b	d1,9(a1)
		rts

flip_spr	movem.l	d0-d1/a0-a1,-(a7)
		move.w	#$24,$104(a6)		;sprites in front
		lea.l	$140(a6),a0
		moveq	#0,d0
		moveq	#$10-1,d1
.clr		move.l	d0,(a0)+		;clr spr data
		dbf	d1,.clr
		move.w	#$20,$96(a6)		;Sprite DMA OFF
		not.b	gfx_param
		beq.b	.nospr
		move.w	#$8020,$96(a6)		;Sprite DMA ON
.nospr
		lea.l	sprite1,a0
		lea.l	$0.w,a1
		move.w	#(sprite_len*2)/4-1,d1
.loop		move.l	(a0),d0
		move.l	(a1),(a0)+
		move.l	d0,(a1)+
		dbf	d1,.loop
		movem.l	(a7)+,d0-d1/a0-a1
		rts


		cnop 0,4

;-------------- picture editor variables (cmd P) -----

depth		dc.w 8
bitplan		dcb.l 8,0
bitplan_lock	dc.b %00000000		;1 bit for each bitplan
hires		dc.b 0			;0=low-res -1=high res
ham		dc.b 0			;0=HAM off -1=HAM on
ehb		dc.b 0			;0=EHB off -1=EHB on
gfx_param	dc.b 0
set_height	dc.b 0			;set height mode on/off
p_used		dc.b 0			;-1 if P command used (for SP)
		even
width		dc.w 320
height		dc.w 256
tmp_height	dc.w 0			;used by set height
tmp_bitplan	dcb.l 8,0		;keep top of screen for set height
modulo		dc.w 0

spr_color	dc.w 0			;no of bank used for sprite color
x_spr:		dc.w 0
y_spr:		dc.w 0
x_spr2:		dc.w 0
y_spr2:		dc.w 0
old_mx:		dc.w 0
old_my:		dc.w 0
left:		dc.b 0
right:		dc.b 0
mouse_speed:	dc.b 0

		cnop 0,4

;sprite for gfx-ripper info pannel (HELP key)

sprite1:	dc.w $3080,0,0,0,$4800,0,0,0
		rept 6*4
		dc.w 0,0,0,0,0,0,0,0
		endr
empty_spr	dc.w 0,0,0,0,0,0,0,0
		dc.w 0,0,0,0,0,0,0,0

sprite_len	equ (27*16)

sprite2:	dc.w $3080,0,0,0,$4800,0,0,0
		rept 6*4
		dc.w 0,0,0,0,0,0,0,0
		endr
		dc.w 0,0,0,0,0,0,0,0
		dc.w 0,0,0,0,0,0,0,0
end_sprite


