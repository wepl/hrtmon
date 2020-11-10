
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


;-------------- save the actual picture from the gfx-ripper --------------

cmd_sp
		lea.l	ev_line,a1
		bsr	read_name		;get filename
		tst.b	(a1)
		beq.w	illegal_name

		tst.b	p_used
		bne.b	.okp
		lea.l	firstusep_txt(pc),a0
		bsr	print
		jmp	end_command

.okp		jsr	remove_pic

		move.l	#ev_line,d1		;filename
		moveq	#-1,d2			;create mode
		bsr	open_file
		move.l	d0,d7			;filehandle
		beq.w	.error

;-------------- write header+BMHD ----------

		move.w	#-1,ts_size		;trainer buffer cleared

		lea.l	IFF_header,a0
		lea.l	tmp_mem,a1
		move.w	#(IFF_header_end-IFF_header),d0
		lsr.w	#1,d0
		subq.w	#1,d0
.copyh		move.w	(a0)+,(a1)+		;copy IFF_header
		dbf	d0,.copyh

		lea.l	tmp_mem,a0
		movem.w	width,d0-d1
		movem.w	d0-d1,IFF_header_bmhdstart-IFF_header(a0)
		movem.w	d0-d1,IFF_header_bmhdstart-IFF_header+16(a0)
		move.w	depth,d0
		move.b	d0,IFF_header_bmhdstart-IFF_header+8(a0)

		move.l	d7,d1
		move.l	#tmp_mem,d2
		move.l	#IFF_header_end-IFF_header,d3
		bsr	write_file		;write header

;-------------- write CAMG if HAM mode ----------

		tst.b	ham
		beq.b	.noham3
		lea.l	tmp_mem,a0
		move.l	#"CAMG",(a0)+
		move.l	#4,(a0)+
		move.l	#$800,(a0)+
		move.l	d7,d1
		move.l	#tmp_mem,d2
		move.l	#3*4,d3
		bsr	write_file		;write CAMG

		cmp.w	#8,depth		;HAM8 ?
		bne.b	.noham3
		lea.l	bitplan,a0
		movem.l	(a0),d0-d1		;rotate bitplans
		move.l	2*4(a0),(a0)
		move.l	3*4(a0),1*4(a0)
		move.l	4*4(a0),2*4(a0)
		move.l	5*4(a0),3*4(a0)
		move.l	6*4(a0),4*4(a0)
		move.l	7*4(a0),5*4(a0)
		movem.l	d0-d1,6*4(a0)
		bra.b	.okham
.noham3
		tst.b	ehb
		beq.b	.noehb
		cmp.w	#6,depth
		bne.b	.noehb
		move.l	#"CAMG",(a0)+
		move.l	#4,(a0)+
		move.l	#$80,(a0)+
		move.l	d7,d1
		move.l	#tmp_mem,d2
		move.l	#3*4,d3
		bsr	write_file		;write CAMG for halfbright
.noehb
.okham

;-------------- write CMAP+BODY header ----------

		lea.l	tmp_mem,a0
		move.w	depth,d0
		moveq	#1,d1
		lsl.w	d0,d1			;d1=nb colors
		move.l	#"CMAP",(a0)+
		move.w	d1,d2
		muls	#3,d2
		move.l	d2,(a0)+		;CMAP size

		move.l	d7,-(a7)
		lea.l	paletteH,a1
		lea.l	paletteL,a2
		moveq	#0,d7		;color no
		subq.w	#1,d1
.loopc		move.w	custom+$10c,d0
		lsr.w	#8,d0
		eor.w	d7,d0
		add.w	d0,d0
		move.w	(a1,d0.w),d2
		move.w	(a2,d0.w),d3
		move.w	d2,d6
		and.w	#$f,d6		;H blue
		lsl.w	#4,d6
		move.w	d3,d0
		and.w	#$f,d0		;L blue
		or.w	d0,d6		;d6=BLUE

		move.w	d2,d5
		and.w	#$f0,d5		;H green
		move.w	d3,d0
		and.w	#$f0,d0
		lsr.w	#4,d0		;L green
		or.w	d0,d5		;d5=GREEN

		move.w	d2,d4
		lsr.w	#4,d4
		and.w	#$f0,d4		;H red
		move.w	d3,d0
		lsr.w	#8,d0
		and.w	#$f,d0		;L red
		or.w	d0,d4		;d4=RED

		move.b	d4,(a0)+	;save RED
		move.b	d5,(a0)+	;save GREEN
		move.b	d6,(a0)+	;save BLUE

		addq.w	#1,d7
		dbf	d1,.loopc

		move.l	(a7)+,d7

		move.l	#"BODY",(a0)+
		movem.w	width,d0-d1
		lsr.w	#3,d0
		mulu	depth,d0
		mulu	d1,d0		;d0=BODY size
		move.l	d0,(a0)+

		move.l	d7,d1
		move.l	#tmp_mem,d2
		move.l	a0,d3
		sub.l	d2,d3
		bsr	write_file	;write CMAP + BODY header

;-------------- write bitplans --------

		movem.w	width,d4-d5
		lsr.l	#3,d4		;width in bytes
		sub.l	a1,a1		;offset
		subq.w	#1,d5
.next_line	lea.l	bitplan,a0
		move.w	depth,d6
		subq.w	#1,d6
.next_plan	move.l	(a0)+,d2
		add.l	a1,d2		;add offset
		move.l	d7,d1
		move.l	d4,d3
		bsr	write_file	;write 1 line from 1 plan
		dbf	d6,.next_plan
		lea.l	40(a1),a1	;inc offset to next line
		add.w	modulo,a1
		dbf	d5,.next_line

;-------------- close file ------------

		move.l	d7,d1
		moveq	#4,d2		;offset (FORM size)
		moveq	#-1,d3		;offset from beginning
		bsr	seek_file
		move.l	d7,a0
		move.l	file_size(a0),d0
		subq.l	#8,d0		;calc FORM size
		move.l	d0,tmp_mem
		move.l	d7,d1
		move.l	#tmp_mem,d2
		moveq	#4,d3
		bsr	write_file	;write FORM size

		move.l	d7,d1
		bsr	close_file

.error		jsr	set_pic
		move.w	#$2000,sr

		jmp	end_command

firstusep_txt	dc.b "First use P command !",$a,0
		even

		cnop 0,4

IFF_header	dc.b "FORM"
		dc.l 0			;FORM size
		dc.b "ILBM"
		dc.b "ANNO"
		dc.l .annoend-.annostart	;ANNO size (always even !)
.annostart	dc.b "File written by HRTmon v"
		version
.annoend
		dc.b "BMHD"
		dc.l IFF_header_bmhdend-IFF_header_bmhdstart	;BMHD size
IFF_header_bmhdstart	dc.w 0,0	;width,height
		dc.w 0,0		;x,y pos
		dc.b 0			;depth
		dc.b 0			;mask (none)
		dc.b 0			;compression mode (none)
		dc.b 0			;align
		dc.w 0			;transparent color
		dc.b $2c,$2c		;xAspect,yAspect
		dc.w 0,0		;pageWidth,pageHeight
IFF_header_bmhdend
IFF_header_end

