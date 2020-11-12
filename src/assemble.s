
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


;assembler from Beermon modified for HRTmon
;68000-68040/FPU/MMU

;-> a0=ptr on string
;-> a1=address where to assemble
;<- d0: 0=ok -1=error
;<- opcode in op68000 & len-1.w in oplen

;oplen		dc.w 0			;nb word for instr.-1
;op68000	dc.w 0,0,0,0,0		;assembled instr. here !!

;s		* only for test *
;		lea.l	linebufBMON,a0
;		lea.l	$1f0000,a1
;		bsr.w	assemble
;		rts
;linebufBMON	dc.b "lea ($1f0020,pc),a0"
;		dcb.b 80,0
;lineeofBMON	dc.b 0
;		even

	ifnd baseBMON
baseBMON
	endc

		ifnd s67BMON
s67BMON =$0000
s6BMON  =$0100
s8BMON  =$0200
stabBMON=$0300
snopBMON=$0400
s9a_BMON=$0500	;68020 cas
s9aBMON =$0600	;68020 chk2,cmp2
		endc

assemble:	movem.l	d1-d7/a0-a6,-(a7)

		lea.l	baseBMON,a6

		move.l	a1,paraBMON

		move.l	a0,a2
.seek		tst.b	(a2)+
		bne.b	.seek		;seek end of line
		subq.l	#1,a2
		move.l	a2,lineendBMON

		move.l a0,a5		;cmdadr!
nn3BMON:		cmp.l lineendBMON,a0
		bhi.b nn4BMON
		move.b (a0)+,d0
		cmp.b #"Z",d0
		bhi.b nn3BMON
		cmp.b #"A",d0
		bcs.b nn3BMON
		bset #5,d0			;go lower case
		move.b d0,-1(a0)
		bra.b nn3BMON
nn4BMON:		lea instrBMON(pc),a4
nn7BMON:
;	btst #0,paraBMON+3			;test if even address ?
;	bne.b nassq
;	cmp.b #"-",(a5)	;line?
;	beq.w ignline
		move.l a5,a0
		lea 6(a4),a1
nn6BMON:		move.b (a1),d0
		bclr #7,d0
		cmp.b (a0)+,d0
		bne.w nn5BMON
		btst #7,(a1)+
		beq.b nn6BMON

		lea op68000-2,a1
		clr.l lginstrBMON	;0:2byte instrBMON,2:4byte (mulx.l...)
		clr.w (a1)+		;oplen!
		clr.w (a1)
		move.w 4(a4),d0		;info
		and.w #$700,d0
		cmp.w #snopBMON,d0
		beq.w nnsnopBMON
		cmp.w #stabBMON,d0
				;bne.b nnstabBMON
		beq.w nn9BMON
nnstabBMON:		cmp.b #".",(a0)+
		bne.w nn5BMON
		cmp.w #s67BMON,d0
		bne.b nns67BMON
		sf imsizeBMON		;%00
		cmp.b #"b",(a0)+	;op68000:0
		beq.w nn9BMON
		addq.b #1,imsizeBMON	;%01
		move.w #%01000000,(a1)
		cmp.b #"w",-1(a0)
		beq.w nn9BMON
		addq.b #1,imsizeBMON	;%10
		move.w #%10000000,(a1)
		cmp.b #"l",-1(a0)
		beq.w nn9BMON	
nns67BMON:		cmp.w #s9a_BMON,d0			;68020 cas
		bne.b nns9a_BMON
		sf imsizeBMON		;%00
		move.w #%01000000000,(a1)	;68020
		cmp.b #"b",(a0)+	;op68000:0
		beq.w nn9BMON
		addq.b #1,imsizeBMON	;%01
		move.w #%10000000000,(a1)
		cmp.b #"w",-1(a0)
		beq.w nn9BMON
		addq.b #1,imsizeBMON	;%10
		move.w #%11000000000,(a1)
		cmp.b #"l",-1(a0)
		beq.w nn9BMON	
nns9a_BMON:		cmp.w #s9aBMON,d0			;68020 chk2,cmp2
		bne.b nns9aBMON
		sf imsizeBMON	;%00
		cmp.b #"b",(a0)+	;op68000:0
		beq.w nn9BMON
		addq.b #1,imsizeBMON	;%01
		move.w #%01000000000,(a1)
		cmp.b #"w",-1(a0)
		beq.w nn9BMON
		addq.b #1,imsizeBMON	;%10
		move.w #%10000000000,(a1)
		cmp.b #"l",-1(a0)
		beq.b nn9BMON	
nns9aBMON:		cmp.w #s6BMON,d0
		bne.b nns6BMON
		move.b #%01,imsizeBMON
		cmp.b #"w",(a0)+
		beq.b nn9BMON
		addq.b #1,imsizeBMON	;%10
		move.w #%1000000,(a1)
		cmp.b #"l",-1(a0)
		beq.b nn9BMON
nns6BMON:		cmp.w #s8BMON,d0
		bne.b nn5BMON		;ill instrtab or cmd
		move.b #%01,imsizeBMON
		cmp.b #"w",(a0)+
		beq.b nn9BMON
		addq.b #1,imsizeBMON	;%10
		move.w #%100000000,(a1)
		cmp.b #"l",-1(a0)
		bne.b nn5BMON
nn9BMON:		cmp.b #" ",(a0)
		bne.b nn5BMON
nn10BMON:		addq.w #1,a0
		cmp.l lineendBMON,a0
		bhi.b nn5BMON
		cmp.b #" ",(a0)
		beq.b nn10BMON

nnsnopBMON:		st d7
		move.l a0,a3
		move.w 4(a4),d0
		ext.w d0			 ;no a4,a5
		add.w d0,d0
		lea assoffsBMON(pc),a0
		move.w (a0,d0.w),d0
		lea assubsBMON(pc),a0

		jsr (a0,d0.w)	;a4:instrtabentry,a5:cmdtxt,a3:cnttxt
			;a6:notmodify!!
		tst.b d7	
		bne.b nn8BMON
nn5BMON:		sf d7
		tst.l (a4)
		beq.b nretryBMON		;unknown
		lea 14(a4),a4
		bra.w nn7BMON
nn8BMON:

		lea op68000,a0
		move.w (a0),d1
		or.w 2(a4),d1
		move.w d1,(a0)

		moveq	#0,d0		;ok!
ass_outBMON		movem.l	(a7)+,d1-d7/a0-a6
		rts

nretryBMON		moveq	#-1,d0		;unknown instruction
		bra.b	ass_outBMON

;---------------------------------------------------------------
		ifnd longtoaBMON

longtoaBMON:	movem.l d0-d2,-(a7)
		moveq #8-1,d2
lta0BMON:		rol.l #4,d0

		moveq #$0f,d1
		and.w d0,d1
		move.b l2asciiBMON(pc,d1.w),(a0)+
		dbf d2,lta0BMON
		movem.l (a7)+,d0-d2
		rts
l2asciiBMON:dc.b "0123456789abcdef"
;---------------------------------------------------------------
atolong_BMON:	cmp.b #"$",(a3)
		bne.b atolongBMON
		addq.w #1,a3
atolongBMON:	moveq #8-1,d2
		bra.b atob2BMON
atoword_BMON:	cmp.b #"$",(a3)
		bne.b atowordBMON
		addq.w #1,a3
atowordBMON:	moveq #4-1,d2
		bra.b atob2BMON
atobyte_BMON:	cmp.b #"$",(a3)
		bne.b atobyteBMON
		addq.w #1,a3
atobyteBMON:	moveq #2-1,d2
atob2BMON:		moveq #0,d0
atob1BMON:		move.b (a3),d1
		cmp.b #"f",d1
		bhi.b atob3BMON
		sub.b #"0",d1
		bcs.b atob3BMON
		cmp.b #9,d1
		bls.b atob0BMON
		cmp.b #"a"-"0",d1
		bcs.b atob3BMON
		sub.b #"a"-"0"-10,d1
atob0BMON:		lsl.l #4,d0
		or.b d1,d0
		addq.w #1,a3
		dbf d2,atob1BMON
atob3BMON:		tst.l d0
		rts
atodnibBMON:moveq #0,d0			;0-99
	move.b (a3),d1
	sub.b #"0",d1
	bcs.b atdnb0BMON
	cmp.b #9,d1
	bhi.b atdnb0BMON
	addq.w #1,a3
	move.b d1,d0
	move.b (a3),d1
	sub.b #"0",d1
	bcs.b atdnb0BMON
	cmp.b #9,d1
	bhi.b atdnb0BMON
	addq.w #1,a3
	mulu #10,d0
	add.b d1,d0
atdnb0BMON:	tst.b d0
	rts
atodlongBMON:
	moveq #0,d0
	moveq #0,d1
	moveq #10-1,d2
atol1BMON:	move.b (a3),d1
	sub.b #"0",d1
	bcs.b atodl0BMON
	cmp.b #9,d1
	bhi.b atodl0BMON
	addq.w #1,a3
	add.l d0,d0		;*2
	move.l d0,-(a7)
	lsl.l #2,d0		;*2*4
	add.l (a7)+,d0
	add.l d1,d0
	dbf d2,atol1BMON
atodl0BMON:	rts

		endc
;---------------------------------------------------------------
skipspcBMON:cmp.b #" ",(a0)
	bne.b fnspc0BMON
	addq.w #1,a0
	bra.b skipspcBMON
findspcBMON:cmp.b #" ",(a0)
	beq.b fnspc0BMON
	addq.w #1,a0
	bra.b findspcBMON
fnspc0BMON:	cmp.l lineendBMON,a0	;bhi.b eol
	rts
;---------------------------------------------------------------
	ifnd instrBMON

instrBMON:	dc.w $ffff,$4afc,snopBMON!xnoopsBMON
	dc.b "illega","l"+128,0
	dc.w $ffff,$4e70,snopBMON!xnoopsBMON
	dc.b "rese","t"+128,0,0,0
	dc.w $ffff,$4e71,snopBMON!xnoopsBMON
	dc.b "no","p"+128,0,0,0,0,0
	dc.w $ffff,$4e73,snopBMON!xnoopsBMON
	dc.b "rt","e"+128,0,0,0,0,0
	dc.w $ffff,$4e75,snopBMON!xnoopsBMON
	dc.b "rt","s"+128,0,0,0,0,0
	dc.w $ffff,$4e77,snopBMON!xnoopsBMON
	dc.b "rt","r"+128,0,0,0,0,0
	dc.w $ffff,$4afa,snopBMON!xnoopsBMON		;cpu32 only
	dc.b "bgn","d"+128,0,0,0,0

	dc.w $ffc0,$f200,snopBMON!xfpuBMON		;68881
	dc.b "f"+128,0,0,0,0,0,0,0
	dc.w $ffc0,$f200,stabBMON!xfmovelBMON		;68881
	dc.b "fmove.","l"+128,0
	dc.w $ffc0,$f200,stabBMON!xfmovemlBMON		;68881
	dc.b "fmovem.","l"+128
	dc.w $ffc0,$f200,stabBMON!xfmovemxBMON		;68881
	dc.b "fmovem.","x"+128
	dc.w $ffff,$f200,stabBMON!xfmovecrBMON		;68881
	dc.b "fmovec","r"+128,0
	dc.w $ffff,$f280,snopBMON!xfnopBMON		;68881
	dc.b "fno","p"+128,0,0,0,0
	dc.w $ffff,$f510,snopBMON!xnoopsBMON		;68040 only!
	dc.b "pflusha","n"+128
	dc.w $ffff,$f518,snopBMON!xnoopsBMON		;68040 only!
	dc.b "pflusha","!"+128
	dc.w $ffff,$f000,snopBMON!xpflushaBMON		;68020 order!
	dc.b "pflush","a"+128,0
	dc.w $ffff,$f000,stabBMON!xpflushsBMON		;68851 only order!
	dc.b "pflush","s"+128,0
	dc.w $ffc0,$f000,stabBMON!xpflushrBMON		;68851 only order!
	dc.b "pflush","r"+128,0
	dc.w $fff8,$f500,stabBMON!xpflush40BMON		;68040 only!
	dc.b "pflush","n"+128,0
	dc.w $fff8,$f508,stabBMON!xpflush40BMON		;68040 only!
	dc.b "pflus","h"+128,0,0
	dc.w $fff8,$f548,stabBMON!xpflush40BMON		;68040 only!
	dc.b "ptest","w"+128,0,0
	dc.w $fff8,$f568,stabBMON!xpflush40BMON		;68040 only!
	dc.b "ptest","r"+128,0,0

	dc.w $ffff,$f000,stabBMON!xpflushBMON		;68020 order!
	dc.b "pflus","h"+128,0,0
	dc.w $ffc0,$f000,stabBMON!xpvalidBMON		;68851 only
	dc.b "pvali","d"+128,0,0
		
	dc.w $ffff,$003c,stabBMON!xiwordtoccrBMON
	dc.b "or","i"+128,0,0,0,0,0
	dc.w $ffff,$023c,stabBMON!xiwordtoccrBMON
	dc.b "and","i"+128,0,0,0,0
	dc.w $ffff,$0a3c,stabBMON!xiwordtoccrBMON
	dc.b "eor","i"+128,0,0,0,0

	dc.w $ffff,$007c,stabBMON!xiwordtosrBMON
	dc.b "or","i"+128,0,0,0,0,0
	dc.w $ffff,$027c,stabBMON!xiwordtosrBMON
	dc.b "and","i"+128,0,0,0,0
	dc.w $ffff,$0a7c,stabBMON!xiwordtosrBMON
	dc.b "eor","i"+128,0,0,0,0

	dc.w $f1f8,$c140,stabBMON!xdx9btodx02BMON
	dc.b "ex","g"+128,0,0,0,0,0
	dc.w $f1f8,$c148,stabBMON!xax9btoax02BMON
	dc.b "ex","g"+128,0,0,0,0,0
	dc.w $f1f8,$c188,stabBMON!xdx9btoax02BMON
	dc.b "ex","g"+128,0,0,0,0,0

	dc.w $fff8,$4e58,stabBMON!xiunlkBMON
	dc.b "unl","k"+128,0,0,0,0
	dc.w $fff8,$4808,stabBMON!xilinklgBMON		;68020
	dc.b "link.","l"+128,0,0
	dc.w $fff8,$4e50,stabBMON!xilinkBMON
	dc.b "lin","k"+128,0,0,0,0

	dc.w $fff8,$4848,stabBMON!xbkptBMON		;68020
	dc.b "bkp","t"+128,0,0,0,0

	dc.w $f1f0,$8140,stabBMON!xpackBMON		;68020
	dc.b "pac","k"+128,0,0,0,0
	dc.w $f1f0,$8180,stabBMON!xpackBMON		;68020
	dc.b "unp","k"+128,0,0,0,0

	dc.w $fff0,$4e40,stabBMON!xitrapBMON
	dc.b "tra","p"+128,0,0,0,0

	dc.w $f100,$7000,stabBMON!ximoveqBMON
	dc.b "move","q"+128,0,0,0

	dc.w $ffff,$f800,stabBMON!xlpstopBMON		;cpu32 only
	dc.b "lpsto","p"+128,0,0	
	dc.w $ffff,$4e72,stabBMON!xiwordBMON
	dc.b "sto","p"+128,0,0,0,0
	dc.w $ffff,$4e74,stabBMON!xiwordBMON		;68010
	dc.b "rt","d"+128,0,0,0,0,0

	dc.w $fdff,$0cfc,snopBMON!xcas2BMON		;68020
	dc.b "cas2","."+128,0,0,0

	dc.w $fff8,$4840,stabBMON!xsrcdx02BMON
	dc.b "swa","p"+128,0,0,0,0
	dc.w $fff8,$49c0,stabBMON!xsrcdx02BMON		;68020
	dc.b "ext","b"+128,0,0,0,0
	dc.w $ffb8,$4880,s6BMON!xsrcdx02BMON
	dc.b "ex","t"+128,0,0,0,0,0

	dc.w $fff0,$06c0,stabBMON!xrx03BMON		;68020
	dc.b "rt","m"+128,0,0,0,0,0
;????
	dc.w $fffe,$f07a,snopBMON!xpccodesizeBMON	;68851 only order!
	dc.b "ptra","p"+128,0,0,0
	dc.w $ffff,$f07c,snopBMON!xpccodeBMON		;68851 only order!
	dc.b "ptra","p"+128,0,0,0
	dc.w $fffe,$f27a,snopBMON!xfccodesizeBMON	;68881 order!
	dc.b "ftra","p"+128,0,0,0
	dc.w $ffff,$f27c,snopBMON!xfccodeBMON		;68881 order!
	dc.b "ftra","p"+128,0,0,0
	dc.w $f0fe,$50fa,snopBMON!xccodesizeBMON	;68020 order!
	dc.b "tra","p"+128,0,0,0,0
	dc.w $f0ff,$50fc,snopBMON!xccodeBMON		;68020 order!
	dc.b "tra","p"+128,0,0,0,0
	dc.w $ffff,$4e76,snopBMON!xnoopsBMON		;order!
	dc.b "trap","v"+128,0,0,0
;????
	dc.w $ff3f,$f418,stabBMON!xidbcacBMON		;68040
	dc.b "cinv","a"+128,0,0,0
	dc.w $ff38,$f408,stabBMON!xidbindBMON		;68040
	dc.b "cinv","l"+128,0,0,0
	dc.w $ff38,$f410,stabBMON!xidbindBMON		;68040
	dc.b "cinv","p"+128,0,0,0
	dc.w $ff3f,$f438,stabBMON!xidbcacBMON		;68040
	dc.b "cpush","a"+128,0,0
	dc.w $ff38,$f428,stabBMON!xidbindBMON		;68040
	dc.b "cpush","l"+128,0,0
	dc.w $ff38,$f430,stabBMON!xidbindBMON		;68040
	dc.b "cpush","p"+128,0,0
	dc.w $fff8,$f620,stabBMON!xmov16BMON		;68040 order!
	dc.b "move1","6"+128,0,0
	dc.w $ffe0,$f600,stabBMON!xmov162BMON		;68040
	dc.b "move1","6"+128,0,0

	dc.w $f138,$b108,s67BMON!xicmpmBMON
	dc.b "cmp","m"+128,0,0,0,0

	dc.w $f1f0,$c100,stabBMON!xxbcdBMON
	dc.b "abc","d"+128,0,0,0,0
	dc.w $f1f0,$8100,stabBMON!xxbcdBMON
	dc.b "sbc","d"+128,0,0,0,0

	dc.w $f130,$d100,s67BMON!xsize67adsbxBMON
	dc.b "add","x"+128,0,0,0,0
	dc.w $f130,$9100,s67BMON!xsize67adsbxBMON
	dc.b "sub","x"+128,0,0,0,0

	dc.w $ffc0,$f000,stabBMON!xpmovefdBMON		;68030 only
	dc.b "pmovef","d"+128,0
	dc.w $ffc0,$f000,stabBMON!xpmoveBMON		;68020
	dc.b "pmov","e"+128,0,0,0
	dc.w $ffc0,$f000,snopBMON!xploadBMON		;68020
	dc.b "ploa","d"+128,0,0,0
	dc.w $ffc0,$f000,snopBMON!xptestBMON		;68020
	dc.b "ptes","t"+128,0,0,0

	dc.w $ffc0,$e8c0,stabBMON!xbfieldBMON		;68020
	dc.b "bfts","t"+128,0,0,0
	dc.w $ffc0,$eac0,stabBMON!xbfieldBMON		;68020
	dc.b "bfch","g"+128,0,0,0
	dc.w $ffc0,$ecc0,stabBMON!xbfieldBMON		;68020
	dc.b "bfcl","r"+128,0,0,0
	dc.w $ffc0,$eec0,stabBMON!xbfieldBMON		;68020
	dc.b "bfse","t"+128,0,0,0
	dc.w $ffc0,$e9c0,stabBMON!xbfieldtodxBMON	;68020
	dc.b "bfext","u"+128,0,0
	dc.w $ffc0,$ebc0,stabBMON!xbfieldtodxBMON	;68020
	dc.b "bfext","s"+128,0,0
	dc.w $ffc0,$edc0,stabBMON!xbfieldtodxBMON	;68020
	dc.b "bfff","o"+128,0,0,0
	dc.w $ffc0,$efc0,stabBMON!xdxtobfieldBMON	;68020
	dc.b "bfin","s"+128,0,0,0

	dc.w $ffc0,$06c0,stabBMON!xcallmBMON		;68020
	dc.b "call","m"+128,0,0,0
	
	dc.w $ffc0,$40c0,stabBMON!xsrtoeaBMON
	dc.b "mov","e"+128,0,0,0,0
	dc.w $ffc0,$42c0,stabBMON!xccrtoeaBMON		;68010
	dc.b "mov","e"+128,0,0,0,0
	dc.w $ffc0,$44c0,stabBMON!xeatoccrBMON
	dc.b "mov","e"+128,0,0,0,0
	dc.w $ffc0,$46c0,stabBMON!xeatosrBMON
	dc.b "mov","e"+128,0,0,0,0

	dc.w $fff0,$4e60,stabBMON!ximuspBMON
	dc.b "mov","e"+128,0,0,0,0

	dc.w $fffe,$4e7a,stabBMON!ximovecBMON		;68010
	dc.b "move","c"+128,0,0,0

	dc.w $fb80,$4880,s6BMON!ximovemBMON
	dc.b "move","m"+128,0,0,0

	dc.w $f138,$0108,s6BMON!xsize6movepBMON
	dc.b "move","p"+128,0,0,0

	dc.w $ffc0,$f800,snopBMON!xtblBMON		;cpu32 only
	dc.b "tb","l"+128,0,0,0,0,0

	dc.w $f1c0,$41c0,stabBMON!xeatoaxBMON
	dc.b "le","a"+128,0,0,0,0,0

	dc.w $ffff,$60ff,stabBMON!xlongbxxBMON		;68020
	dc.b "bra.","l"+128,0,0,0
	dc.w $ffff,$61ff,stabBMON!xlongbxxBMON		;68020
	dc.b "bsr.","l"+128,0,0,0
	dc.w $ffff,$6000,stabBMON!xwordbxxBMON
	dc.b "bra.","w"+128,0,0,0
	dc.w $ffff,$6100,stabBMON!xwordbxxBMON
	dc.b "bsr.","w"+128,0,0,0
	dc.w $ff00,$6000,stabBMON!xshortbxxBMON		;order !
	dc.b "bra.","b"+128,0,0,0
	dc.w $ff00,$6000,stabBMON!xshortbxxBMON		;order !
	dc.b "bra.","s"+128,0,0,0				;clone
	dc.w $ff00,$6100,stabBMON!xshortbxxBMON
	dc.b "bsr.","b"+128,0,0,0
	dc.w $ff00,$6100,stabBMON!xshortbxxBMON
	dc.b "bsr.","s"+128,0,0,0				;clone
	dc.w $ffb0,$f080,snopBMON!xbrapccBMON		;68851 only
	dc.b "p","b"+128,0,0,0,0,0,0
	dc.w $ffa0,$f280,snopBMON!xbrafccBMON		;68881
	dc.b "f","b"+128,0,0,0,0,0,0
	dc.w $f0ff,$60ff,snopBMON!xlongccBMON		;68020
	dc.b "b"+128,0,0,0,0,0,0,0
	dc.w $f0ff,$6000,snopBMON!xwordccBMON
	dc.b "b"+128,0,0,0,0,0,0,0
	dc.w $f000,$6000,snopBMON!xshortccBMON
	dc.b "b"+128,0,0,0,0,0,0,0

	dc.w $fff8,$f048,snopBMON!xpccodewordrelBMON	;68851 only
	dc.b "pd","b"+128,0,0,0,0,0
	dc.w $ffc0,$f040,snopBMON!xpccodeeaBMON		;68851 only
	dc.b "p","s"+128,0,0,0,0,0,0
	dc.w $fff8,$f248,snopBMON!xfccodewordrelBMON	;68881
	dc.b "fd","b"+128,0,0,0,0,0
	dc.w $ffc0,$f240,snopBMON!xfccodeeaBMON		;68881
	dc.b "f","s"+128,0,0,0,0,0,0

	dc.w $f0f8,$50c8,snopBMON!xccodewordrelBMON	;order !
	dc.b "d","b"+128,0,0,0,0,0,0
	dc.w $f0c0,$50c0,snopBMON!xccodeeaBMON
	dc.b "s"+128,0,0,0,0,0,0,0

	dc.w $ffc0,$4800,stabBMON!xsrceaBMON
	dc.b "nbc","d"+128,0,0,0,0
	dc.w $ffc0,$4840,stabBMON!xsrceaBMON
	dc.b "pe","a"+128,0,0,0,0,0
	dc.w $ffc0,$4ac0,stabBMON!xsrceaBMON
	dc.b "ta","s"+128,0,0,0,0,0
	dc.w $ffc0,$4e80,stabBMON!xsrceaBMON
	dc.b "js","r"+128,0,0,0,0,0
	dc.w $ffc0,$4ec0,stabBMON!xsrceaBMON
	dc.b "jm","p"+128,0,0,0,0,0
	dc.w $ffc0,$e0c0,stabBMON!xsrceaBMON
	dc.b "as","r"+128,0,0,0,0,0
	dc.w $ffc0,$e1c0,stabBMON!xsrceaBMON
	dc.b "as","l"+128,0,0,0,0,0
	dc.w $ffc0,$e2c0,stabBMON!xsrceaBMON
	dc.b "ls","r"+128,0,0,0,0,0
	dc.w $ffc0,$e3c0,stabBMON!xsrceaBMON
	dc.b "ls","l"+128,0,0,0,0,0
	dc.w $ffc0,$e4c0,stabBMON!xsrceaBMON
	dc.b "rox","r"+128,0,0,0,0
	dc.w $ffc0,$e5c0,stabBMON!xsrceaBMON
	dc.b "rox","l"+128,0,0,0,0
	dc.w $ffc0,$e6c0,stabBMON!xsrceaBMON
	dc.b "ro","r"+128,0,0,0,0,0
	dc.w $ffc0,$e7c0,stabBMON!xsrceaBMON
	dc.b "ro","l"+128,0,0,0,0,0
	dc.w $ffc0,$f100,stabBMON!xsrceaBMON		;68851 only
	dc.b "psav","e"+128,0,0,0
	dc.w $ffc0,$f140,stabBMON!xsrceaBMON		;68851 only
	dc.b "prestor","e"+128
	dc.w $ffc0,$f300,stabBMON!xsrceaBMON		;68881
	dc.b "fsav","e"+128,0,0,0
	dc.w $ffc0,$f340,stabBMON!xsrceaBMON		;68881
	dc.b "frestor","e"+128

	dc.w $ffc0,$4c40,stabBMON!xdivullBMON		;68020
	dc.b "divul.","l"+128,0
	dc.w $ffc0,$4c40,stabBMON!xdivsllBMON		;68020
	dc.b "divsl.","l"+128,0
	dc.w $ffc0,$4c40,stabBMON!xmululBMON		;68020
	dc.b "divu.","l"+128,0,0
	dc.w $ffc0,$4c40,stabBMON!xmulslBMON		;68020
	dc.b "divs.","l"+128,0,0
	dc.w $ffc0,$4c00,stabBMON!xmululBMON		;68020
	dc.b "mulu.","l"+128,0,0
	dc.w $ffc0,$4c00,stabBMON!xmulslBMON		;68020
	dc.b "muls.","l"+128,0,0

	dc.w $f1c0,$80c0,stabBMON!xeatodxBMON
	dc.b "div","u"+128,0,0,0,0
	dc.w $f1c0,$81c0,stabBMON!xeatodxBMON
	dc.b "div","s"+128,0,0,0,0
	dc.w $f1c0,$c0c0,stabBMON!xeatodxBMON
	dc.b "mul","u"+128,0,0,0,0
	dc.w $f1c0,$c1c0,stabBMON!xeatodxBMON
	dc.b "mul","s"+128,0,0,0,0
	dc.w $f1c0,$4180,stabBMON!xeatodxBMON
	dc.b "chk.","w"+128,0,0,0
	dc.w $f1c0,$4100,stabBMON!xea2todxBMON		;68020
	dc.b "chk.","l"+128,0,0,0

	dc.w $f0c0,$90c0,s8BMON!xsize8eatoax9bBMON
	dc.b "sub","a"+128,0,0,0,0
	dc.w $f0c0,$b0c0,s8BMON!xsize8eatoax9bBMON
	dc.b "cmp","a"+128,0,0,0,0
	dc.w $f0c0,$d0c0,s8BMON!xsize8eatoax9bBMON
	dc.b "add","a"+128,0,0,0,0

	dc.w $f100,$8000,s67BMON!xsize67eatodxBMON
	dc.b "o","r"+128,0,0,0,0,0,0
	dc.w $f100,$9000,s67BMON!xsize67eatodxBMON
	dc.b "su","b"+128,0,0,0,0,0
	dc.w $f100,$b000,s67BMON!xsize67eatodxBMON
	dc.b "cm","p"+128,0,0,0,0,0
	dc.w $f100,$c000,s67BMON!xsize67eatodxBMON
	dc.b "an","d"+128,0,0,0,0,0
	dc.w $f100,$d000,s67BMON!xsize67eatodxBMON
	dc.b "ad","d"+128,0,0,0,0,0

	dc.w $f100,$8100,s67BMON!xdxtoeaBMON
	dc.b "o","r"+128,0,0,0,0,0,0
	dc.w $f100,$9100,s67BMON!xdxtoeaBMON
	dc.b "su","b"+128,0,0,0,0,0
	dc.w $f100,$b100,s67BMON!xdxtoeaBMON
	dc.b "eo","r"+128,0,0,0,0,0
	dc.w $f100,$c100,s67BMON!xdxtoeaBMON
	dc.b "an","d"+128,0,0,0,0,0
	dc.w $f100,$d100,s67BMON!xdxtoeaBMON
	dc.b "ad","d"+128,0,0,0,0,0

	dc.w $ffc0,$0800,stabBMON!xinibtoeaBMON
	dc.b "bts","t"+128,0,0,0,0
	dc.w $ffc0,$0840,stabBMON!xinibtoeaBMON
	dc.b "bch","g"+128,0,0,0,0
	dc.w $ffc0,$0880,stabBMON!xinibtoeaBMON
	dc.b "bcl","r"+128,0,0,0,0
	dc.w $ffc0,$08c0,stabBMON!xinibtoeaBMON
	dc.b "bse","t"+128,0,0,0,0

	dc.w $f9c0,$08c0,s9a_BMON!xcasBMON		;68020
	dc.b "ca","s"+128,0,0,0,0,0
	dc.w $f9c0,$00c0,s9aBMON!xchk2BMON		;68020
	dc.b "chk","2"+128,0,0,0,0
	dc.w $f9c0,$00c0,s9aBMON!xcmp2BMON		;68020
	dc.b "cmp","2"+128,0,0,0,0

	dc.w $f100,$5000,s67BMON!xsize67i9btoeaBMON
	dc.b "add","q"+128,0,0,0,0
	dc.w $f100,$5100,s67BMON!xsize67i9btoeaBMON
	dc.b "sub","q"+128,0,0,0,0

	dc.w $f118,$e000,s67BMON!xsize67x9btodx02BMON
	dc.b "as","r"+128,0,0,0,0,0
	dc.w $f118,$e008,s67BMON!xsize67x9btodx02BMON
	dc.b "ls","r"+128,0,0,0,0,0
	dc.w $f118,$e010,s67BMON!xsize67x9btodx02BMON
	dc.b "rox","r"+128,0,0,0,0
	dc.w $f118,$e018,s67BMON!xsize67x9btodx02BMON
	dc.b "ro","r"+128,0,0,0,0,0
	dc.w $f118,$e100,s67BMON!xsize67x9btodx02BMON
	dc.b "as","l"+128,0,0,0,0,0
	dc.w $f118,$e108,s67BMON!xsize67x9btodx02BMON
	dc.b "ls","l"+128,0,0,0,0,0
	dc.w $f118,$e110,s67BMON!xsize67x9btodx02BMON
	dc.b "rox","l"+128,0,0,0,0
	dc.w $f118,$e118,s67BMON!xsize67x9btodx02BMON
	dc.b "ro","l"+128,0,0,0,0,0

	dc.w $ff00,$0000,s67BMON!xsize67ixtoeaBMON
	dc.b "or","i"+128,0,0,0,0,0
	dc.w $ff00,$0200,s67BMON!xsize67ixtoeaBMON
	dc.b "and","i"+128,0,0,0,0
	dc.w $ff00,$0400,s67BMON!xsize67ixtoeaBMON
	dc.b "sub","i"+128,0,0,0,0
	dc.w $ff00,$0600,s67BMON!xsize67ixtoeaBMON
	dc.b "add","i"+128,0,0,0,0
	dc.w $ff00,$0a00,s67BMON!xsize67ixtoeaBMON
	dc.b "eor","i"+128,0,0,0,0
	dc.w $ff00,$0c00,s67BMON!xsize67ixtoeaBMON
	dc.b "cmp","i"+128,0,0,0,0

	dc.w $f1c0,$0100,stabBMON!xdxtoeaBMON
	dc.b "bts","t"+128,0,0,0,0
	dc.w $f1c0,$0140,stabBMON!xdxtoeaBMON
	dc.b "bch","g"+128,0,0,0,0
	dc.w $f1c0,$0180,stabBMON!xdxtoeaBMON
	dc.b "bcl","r"+128,0,0,0,0
	dc.w $f1c0,$01c0,stabBMON!xdxtoeaBMON
	dc.b "bse","t"+128,0,0,0,0

	dc.w $ff00,$0e00,s67BMON!xmovesBMON		;68010
	dc.b "move","s"+128,0,0,0

	dc.w $ff00,$4000,s67BMON!xsrceaBMON
	dc.b "neg","x"+128,0,0,0,0
	dc.w $ff00,$4200,s67BMON!xsrceaBMON
	dc.b "cl","r"+128,0,0,0,0,0
	dc.w $ff00,$4400,s67BMON!xsrceaBMON
	dc.b "ne","g"+128,0,0,0,0,0
	dc.w $ff00,$4600,s67BMON!xsrceaBMON
	dc.b "no","t"+128,0,0,0,0,0
	dc.w $ff00,$4a00,s67BMON!xsrceaBMON
	dc.b "ts","t"+128,0,0,0,0,0

	dc.w $c000,$0000,snopBMON!ximoveBMON
	dc.b "move","."+128,0,0,0

	dc.w $f000,$a000,stabBMON!xdcwopBMON
	dc.b "line-","a"+128,0,0
	dc.w $f000,$f000,stabBMON!xdcwopBMON
	dc.b "line-","f"+128,0,0
	dc.w $0000,$0000,stabBMON!xdcwopBMON
	dc.b "dc.","w"+128,0,0,0,0

;---------------------------------------------------------------
xopfailBMON=0			;dummy
xnoopsBMON=1
xdcwopBMON=2
xeatoccrBMON=3
xeatosrBMON=4
xsrtoeaBMON=5
xwordbxxBMON=6
xwordccBMON=7
xshortbxxBMON=8
xshortccBMON=9
xeatoaxBMON=10
xeatodxBMON=11
xdxtoeaBMON=12
xiwordtoccrBMON=13
xiwordtosrBMON=14
xiwordBMON=15
xccodewordrelBMON=16
xinibtoeaBMON=17
xccodeeaBMON=18
xsrceaBMON=19
xsize67eatodxBMON=20
xsize8eatoax9bBMON=21
xsize67adsbxBMON=22
xsize67ixtoeaBMON=23
xsize67i9btoeaBMON=24
xsize67x9btodx02BMON=25
xitrapBMON=26
xsrcdx02BMON=27
ximuspBMON=28
ximoveBMON=29
ximoveqBMON=30
xicmpmBMON=31
xxbcdBMON=32
xsize6movepBMON=33
ximovemBMON=34
xdx9btodx02BMON=35
xax9btoax02BMON=36
xdx9btoax02BMON=37
xiunlkBMON=38
xilinkBMON=39
ximovecBMON=40
xccrtoeaBMON=41
xmovesBMON=42
xbfieldBMON=43
xbfieldtodxBMON=44
xdxtobfieldBMON=45
xbkptBMON=46
xlongbxxBMON=47
xlongccBMON=48
xcallmBMON=49
xcasBMON=50
xcas2BMON=51
xchk2BMON=52
xcmp2BMON=53
xrx03BMON=54
xccodeBMON=55
xccodesizeBMON=56
xilinklgBMON=57
xmululBMON=58
xmulslBMON=59
xdivullBMON=60
xdivsllBMON=61
xpackBMON=62
xea2todxBMON=63	;chk.l
xfccodeBMON=64
xfccodesizeBMON=65
xfccodewordrelBMON=66
xfccodeeaBMON=67
xfnopBMON=68
xfmovecrBMON=69
xpmoveBMON=70
xpmovefdBMON=71
xpflushaBMON=72
xpflushBMON=73
xploadBMON=74
xptestBMON=75
xidbcacBMON=76
xidbindBMON=77
xmov16BMON=78
xpccodeeaBMON=79
xpccodewordrelBMON=80
xpccodeBMON=81
xpccodesizeBMON=82
xbrapccBMON=83
xbrafccBMON=84
xpflushsBMON=85
xpflushrBMON=86
xpvalidBMON=87
xpflush40BMON=88
xmov162BMON=89
xlpstopBMON=90
xtblBMON=91
xfpuBMON=92
xfmovelBMON=93
xfmovemlBMON=94
xfmovemxBMON=95

		endc

;---------------------------------------------------------------
assoffsBMON:dc.w aopfailBMON,anoopsBMON,adcwopBMON,aeatoccrBMON,aeatosrBMON,asrtoeaBMON
	dc.w awordbxxBMON,awordccBMON
	dc.w ashortbxxBMON,ashortccBMON,aeatoaxBMON,aeatodxBMON,adxtoeaBMON,aiwordtoccrBMON
	dc.w aiwordtosrBMON,aiwordBMON,accodewordrelBMON,ainibtoeaBMON,accodeeaBMON,asrceaBMON
	dc.w asize67eatodxBMON,asize8eatoax9bBMON,asize67adsbxBMON,asize67ixtoeaBMON
	dc.w asize67i9btoeaBMON,asize67x9btodx02BMON,aitrapBMON,asrcdx02BMON,aimuspBMON
	dc.w aimoveBMON,aimoveqBMON,aicmpmBMON,axbcdBMON,asize6movepBMON
	dc.w aimovemBMON,adx9btodx02BMON,aax9btoax02BMON,adx9btoax02BMON
	dc.w aiunlkBMON,ailinkBMON,aimovecBMON,accrtoeaBMON,aimovesBMON,abfieldBMON,abfieldtodxBMON
	dc.w adxtobfieldBMON,abkptBMON,alongbxxBMON,alongccBMON,aicallmBMON,acasBMON,acas2BMON
	dc.w achk2BMON,acmp2BMON,artmBMON,atpccBMON,atpccsizeBMON,alinklgBMON,amululBMON,amulslBMON
	dc.w adivullBMON,adivsllBMON,apackBMON,aea2todxBMON,atpfccBMON,atpfccsizeBMON
	dc.w afccodewordrelBMON,afccodeeaBMON,afnopBMON,afmovecrBMON,apmoveBMON,apmovefdBMON
	dc.w apflushaBMON,apflushBMON,aploadBMON,aptestBMON,aidbcacBMON,aidbindBMON,amov16BMON
	dc.w apccodeeaBMON,apccodewordrelBMON,atppccBMON,atppccsizeBMON,abrapccBMON,abrafccBMON
	dc.w apflushsBMON,apflushrBMON,apvalidBMON,apflush40BMON,amov162BMON,alpstopBMON
	dc.w atblBMON,afpuBMON,afmovelBMON,afmovemlBMON,afmovemxBMON

;---------------------------------------------------------------
assubsBMON:
aopfailBMON=*-assubsBMON
anoopsBMON=*-assubsBMON
	rts
adcwopBMON=*-assubsBMON
	bsr.w atoword_BMON
	move.w d0,op68000
	rts
aeatoccrBMON=*-assubsBMON
	sf imsizeBMON
	bsr.w asseaBMON
	lea toccrtxtBMON(pc),a0
	bra.w cmptxtBMON
aeatosrBMON=*-assubsBMON
	move.b #1,imsizeBMON
	bsr.w asseaBMON
	lea tosrtxtBMON(pc),a0
	bra.w cmptxtBMON

accrtoeaBMON=*-assubsBMON
	lea ccrtxtBMON(pc),a0
	bra.b assreaBMON
asrtoeaBMON=*-assubsBMON
	lea srtxtBMON(pc),a0
assreaBMON:	bsr.w cmptxtBMON
	bra.w asstoeaBMON

abrapccBMON=*-assubsBMON
	bsr.w asspccodeBMON
	bra.b abraf0BMON
abrafccBMON=*-assubsBMON
	bsr.w assfccodeBMON
abraf0BMON:	or.w d0,op68000
	cmp.b #".",(a3)+
	bne.w failBMON
	cmp.b #"w",(a3)+
	beq.b awdcc0BMON
	bset #6,op68000+1	;long
	cmp.b #"l",-1(a3)
	beq.b algcc0BMON
	bra.b awdbxx1BMON		;failBMON
awordccBMON=*-assubsBMON
	bsr.w assccodeBMON
	cmp.b #".",(a3)+
	bne.b awdbxx1BMON		;failBMON
	cmp.b #"w",(a3)+	;bxx.w
	bne.b awdbxx1BMON		;failBMON
awdcc0BMON:	bsr.w asstabBMON
awordbxxBMON=*-assubsBMON
awdrelBMON:	bsr.w atolong_BMON
	subq.l #2,d0
	sub.l lginstrBMON,d0	;fdbx dx,lab...
	sub.l paraBMON,d0
	move.l d0,d1
	;beq.b awdbxx1BMON		;beq.w *+2
	bpl.b awdbxx0BMON
	not.l d1
awdbxx0BMON:cmp.l #$7fff,d1
	bhi.b awdbxx1BMON
	bra.w storewordBMON 
awdbxx1BMON:sf d7
	rts
alongccBMON=*-assubsBMON
	bsr.w assccodeBMON
	cmp.b #".",(a3)+
	bne.b awdbxx1BMON		;failBMON
	cmp.b #"l",(a3)+	;bxx.l
	bne.b awdbxx1BMON		;failBMON
algcc0BMON:	bsr.w asstabBMON
alongbxxBMON=*-assubsBMON
	bsr.w atolong_BMON
	subq.l #2,d0
	sub.l paraBMON,d0
	bra.w storelongBMON

ashortccBMON=*-assubsBMON
	bsr.w assccodeBMON
	cmp.b #".",(a3)+
	bne.b ashbxx1BMON		;failBMON
	moveq #"b",d0
	sub.b (a3)+,d0
	beq.b ashort0BMON
	cmp.b #"b"-"s",d0	;clone
	bne.b ashbxx1BMON		;failBMON
ashort0BMON:bsr.w asstabBMON
ashortbxxBMON=*-assubsBMON
ashbxxBMON:	bsr.w atolong_BMON
	subq.l #2,d0
	sub.l paraBMON,d0
	or.b d0,op68000+1
	beq.b ashbxx1BMON
	bpl.b ashbxx0BMON
	not.l d0
ashbxx0BMON:moveq #$7f,d1
	cmp.l d1,d0
	bls.b ashbxx2BMON
ashbxx1BMON:sf d7
ashbxx2BMON:rts

aeatoaxBMON=*-assubsBMON
	bsr.w asseaBMON
	bra.w asstoax9bBMON

aea2todxBMON=*-assubsBMON
	move.b #2,imsizeBMON
	bra.b aea2dxBMON
aeatodxBMON=*-assubsBMON
	move.b #1,imsizeBMON
aea2dxBMON:
asize67eatodxBMON=*-assubsBMON
	bsr.w asseaBMON
	bra.w asstodx9bBMON

adxtoeaBMON=*-assubsBMON
	bsr.w assdx9bBMON
	bra.w asstoeaBMON

aiwordtoccrBMON=*-assubsBMON
	cmp.b #"#",(a3)+
	bne.w failBMON
	bsr.w atobyte_BMON
	bsr.w storewordBMON
	lea toccrtxtBMON(pc),a0
	bra.w cmptxtBMON
aiwordtosrBMON=*-assubsBMON
	bsr.b aiwrdBMON
	lea tosrtxtBMON(pc),a0
	bra.w cmptxtBMON
aiwordBMON=*-assubsBMON
aiwrdBMON:	cmp.b #"#",(a3)+
	bne.w failBMON
	bsr.w atoword_BMON
	bra.w storewordBMON
alpstopBMON=*-assubsBMON
	move.w #$01c0,d0
	bsr.w storeinstrBMON
	bra.b aiwrdBMON
atblBMON=*-assubsBMON
	moveq #0,d0	;unsigned
	moveq #"u",d1
	sub.b (a3)+,d1
	beq.b atbluBMON
	add.b #"s"-"u",d1
	bne.w failBMON
	bset #11,d0	;signed
atbluBMON:	cmp.b #"n",(a3)
	bne.b atblnBMON
	bset #10,d0	;round
	addq.w #1,a3
atblnBMON:	cmp.b #".",(a3)+
	bne.w failBMON
	moveq #"b",d1
	sub.b (a3)+,d1
	beq.b atblbBMON
	bset #6,d0
	cmp.b #"b"-"w",d1
	beq.b atblbBMON
	cmp.b #"b"-"l",d1
	bne.w failBMON
	eor.b #%11000000,d0
atblbBMON:	bsr.w storeinstrBMON	
	bsr.w asstabBMON
	cmp.b #"d",(a3)+
	bne.b atbleaBMON
	bsr.w assxx02BMON
	cmp.b #":",(a3)+
	bne.w failBMON
	bsr.w assdn02BMON
	or.b d0,op68000+3
	bra.b atbldBMON	
atbleaBMON:	subq.w #1,a3
	bsr.w asseaBMON
	bset #0,op68000+2
atbldBMON:	cmp.b #",",(a3)+
	bne.w failBMON
	bsr.w assdn02BMON
	rol.w #4,d0
	or.b d0,op68000+2
	rts
afpuBMON=*-assubsBMON
	sf d7
	rts
afmovemxBMON=*-assubsBMON
	moveq #0,d0
	bsr.w storeinstrBMON
	cmp.b #"d",(a3)
	beq.b afmvmx0BMON		;dynamic mask
	cmp.b #"f",(a3)
	beq.b afmvmx0BMON
	bsr asseaBMON
	cmp.b #",",(a3)+
	bne.w failBMON
	bra assfprxBMON
afmvmx0BMON:bset #13-8,op68000+2
	bsr assfprxBMON
	bsr asstoeaBMON
	moveq #%111000,d0
	and.w op68000,d0
	beq.w failBMON		;dn
	cmp.b #%001000,d0	;an
	beq.w failBMON
	cmp.b #%111100,d0	;#
	beq.w failBMON
	cmp.b #%100000,d0	;-(an)?
	bne.b afmvmx1BMON
	move.b op68000+3,d1	;rl
	moveq #8-1,d0
afmvmx2BMON:lsr.b #1,d1
	roxl.b #1,d2
	dbf d0,afmvmx2BMON
	move.b d2,op68000+3
	bclr #12-8,op68000+2	;predec mode
afmvmx1BMON:rts	
	

afmovemlBMON=*-assubsBMON
	st d5
	bra.b afmovemBMON
afmovelBMON=*-assubsBMON
	sf d5			;fmovem flag
afmovemBMON:moveq #0,d4		;fpcr counter
	move.w #$8000,d0
	bsr.w storeinstrBMON
	move.b #%010,imsizeBMON	;long
	pea fpiar0BMON(pc)		;fpiar,An check
	cmp.b #"f",(a3)
	bne.b afpcr1BMON
	bset #13-8,op68000+2
	bsr.b afpcrBMON
	bra asstoeaBMON
afpcr1BMON:	bsr asseaBMON
	cmp.b #",",(a3)+
	bne.b fpfailBMON
afpcrBMON:	cmp.b #"f",(a3)+
	bne.b fpfailBMON
	cmp.b #"p",(a3)+
	bne.b fpfailBMON
	moveq #12,d2
	cmp.b #"c",(a3)
	beq.b afpcr0BMON
	moveq #11,d2
	cmp.b #"s",(a3)
	beq.b afpcr0BMON
	moveq #10,d2
	cmp.b #"i",(a3)+
	bne.b fpfailBMON
	cmp.b #"a",(a3)
	bne.b fpfailBMON
afpcr0BMON:	addq.w #1,a3
	cmp.b #"r",(a3)+
	bne.b fpfailBMON
	bset d2,op68000+2
	addq.b #1,d4		;fpcr counter
	cmp.b #"/",(a3)
	bne.b afpcr2BMON
	tst.b d5		;fmovem flag
	beq.b fpfailBMON	
	addq.w #1,a3
	bra.b afpcrBMON
afpcr2BMON:	rts
fpiar0BMON:	moveq #%111000,d0
	and.b op68000+1,d0	;ea field
	bne.b fpiar1BMON			;Dn?
	cmp.b #1,d4			;1 fpcr?
	bne.b fpfailBMON
fpiar1BMON:	subq.b #%001000,d0		;An
	bne.b fpcrokBMON
	cmp.b #1,d4			;1 fpcr?
	bne.b fpfailBMON
	btst #10-8,op68000+2	;fpiar?
	bne.b fpcrokBMON
fpfailBMON:	sf d7
fpcrokBMON:	rts

afmovecrBMON=*-assubsBMON
	cmp.b #"#",(a3)+
	bne.w failBMON
	bsr.w atobyte_BMON
	beq.b amfk0BMON
	cmp.b #$3f,d0
	bhi.w failBMON
	cmp.b #$0b,d0
	bcs.w failBMON
	cmp.b #$0f,d0
	bls.b amfk0BMON
	cmp.b #$30,d0
	bcs.w failBMON 
amfk0BMON:	or.w #$5c00,d0
	bsr.w storewordBMON
	bra.w asstofpx79BMON

assfprxBMON:move.w #$d000,d0	;postincr/cntrl mode
	cmp.b #"d",(a3)	;dynamic mask?
	bne.b arxmvm2BMON
	addq.w #1,a3
	move.b (a3)+,d0
	sub.b #"0",d0
	bcs.w arxfailBMON
	cmp.b #7,d0
	bhi.w arxfailBMON
	lsl.b #4,d0
	bset #11,d0	;dynamic
	bra.b arxmvm1BMON	
arxmvm2BMON:cmp.b #"f",(a3)+
	bne.b arxfailBMON
	cmp.b #"p",(a3)+
	bne.b arxfailBMON
	move.b (a3)+,d1
	sub.b #"0",d1
	bcs.b arxfailBMON
	cmp.b #7,d1
	bhi.b arxfailBMON
	moveq #7,d3
	sub.b d1,d3
	bset d3,d0
arxmvm4BMON:cmp.b #",",(a3)
	beq.b arxmvm1BMON
	cmp.b #"/",(a3)+
	beq.b arxmvm2BMON
	cmp.b #"-",-1(a3)
	bne.b arxmvm1BMON
	cmp.b #"f",(a3)+
	bne.b arxfailBMON
	cmp.b #"p",(a3)+
	bne.b arxfailBMON
	move.b (a3)+,d2
	sub.b #"0",d2
	bcs.b arxfailBMON
	cmp.b #7,d2
	bhi.b arxfailBMON
	cmp.b d1,d2
	bls.b arxfailBMON
arxmvm3BMON:addq.b #1,d1
	moveq #7,d3
	sub.b d1,d3
	bset d3,d0	
	cmp.b d1,d2
	bne.b arxmvm3BMON
	bra.b arxmvm4BMON
arxmvm1BMON:or.w d0,op68000+2
	rts
arxfailBMON:bra failBMON
	
apccodewordrelBMON=*-assubsBMON
	bsr.w asspccodeBMON
	bra.b accode1BMON
afccodewordrelBMON=*-assubsBMON
	bsr.w assfccodeBMON
accode1BMON:bsr.w storeinstrBMON	;instrBMON
	bra.b accode0BMON		;4bytes instrlen ok (oma bug)
accodewordrelBMON=*-assubsBMON
	bsr.w assccodeBMON
accode0BMON:bsr.w asstabBMON
	bsr.w assdx02BMON
	cmp.b #",",(a3)+
	beq.w awdrelBMON
	sf d7
	rts
ainibtoeaBMON=*-assubsBMON
	cmp.b #"#",(a3)+
	bne.w failBMON
	bsr.w atodnibBMON
	cmp.b #31,d0
	bhi.w failBMON
	bsr.w storeinstrBMON
	bra.w asstoeaBMON

apccodeeaBMON=*-assubsBMON
	bsr.w asspccodeBMON
	bra.b asrc1BMON
afccodeeaBMON=*-assubsBMON
	bsr.w assfccodeBMON
asrc1BMON:	bsr.w storeinstrBMON
	bra.b asrc0BMON
accodeeaBMON=*-assubsBMON
	bsr.w assccodeBMON
asrc0BMON:	bsr.w asstabBMON
asrceaBMON=*-assubsBMON
	bra.w asseaBMON

aimovesBMON=*-assubsBMON
	bsr.w storeinstrBMON	;space
	moveq #0,d3
	cmp.b #",",2(a3)	;xx,<ea>
	bne.b actlmvsBMON
	bset #11,d3
	bsr.w amvcregBMON
	addq.w #1,a3
	bsr.w asseaBMON
	bra.b aimvs0BMON
actlmvsBMON:bsr.w asseaBMON
	cmp.b #",",(a3)+
	bne.w failBMON
	bsr.w amvcregBMON
aimvs0BMON:	move.w d3,op68000+2
	rts

asize8eatoax9bBMON=*-assubsBMON
	bsr.w asseaBMON
	bra.w asstoax9bBMON

asize67ixtoeaBMON=*-assubsBMON
	cmp.b #"#",(a3)
	bne.w failBMON
	bsr.w asseaBMON		;#$xx.x
	bra.w asstoeaBMON		;clr previous ea !

asize67i9btoeaBMON=*-assubsBMON
	cmp.b #"#",(a3)+
	bne.w failBMON
	bsr.w atobyteBMON
	beq.w failBMON
	cmp.b #8,d0
	bhi.w failBMON	
	bne.b ai9b0BMON
	sf d0
ai9b0BMON:	add.b d0,d0
	or.b d0,op68000
	bra.w asstoeaBMON

asize67x9btodx02BMON=*-assubsBMON
	cmp.b #"#",(a3)+
	bne.b ashft0BMON
	bsr.w atobyteBMON
	beq.w failBMON
	cmp.b #8,d0
	bhi.w failBMON
	bne.b ashft1BMON
	sf d0
ashft1BMON:	add.b d0,d0
	or.b d0,op68000
	bra.w asstodx02BMON
ashft0BMON:	bset #5,op68000+1
	subq.w #1,a3
	bsr.w assdx9bBMON
	bra.w asstodx02BMON

aitrapBMON=*-assubsBMON
	cmp.b #"#",(a3)+
	bne.w failBMON
	bsr.w atodnibBMON
	cmp.b #15,d0
	bhi.w failBMON
	or.b d0,op68000+1
	rts
asrcdx02BMON=*-assubsBMON
	bra.w assdx02BMON
aimuspBMON=*-assubsBMON
	lea usptxtBMON(pc),a0
	cmp.b #"a",(a3)
	beq.b aimusp0BMON
	bset #3,op68000+1
	bsr.b aimusp1BMON
	cmp.b #",",(a3)+
	bra.w assax02BMON
aimusp0BMON:bsr.w assax02BMON
	cmp.b #",",(a3)+
	bne.w failBMON
aimusp1BMON:bra.w cmptxtBMON

apmovefdBMON=*-assubsBMON			;68030mmu only
	move.w #$0100,d0
	bsr.b apmvfdBMON
	move.w op68000+2,d0
	cmp.w #$4100,d0			;40:tc
	beq.w apfdokBMON
	eor.w #%0000100100000000,d0	;08:tt0,0c:tt1,48:srp,4c:crp
	and.w #%1011101111111111,d0
	bne.w failBMON
apfdokBMON:	rts
apmoveBMON=*-assubsBMON
	bsr.b apregBMON
	move.l a1,a3		;restore ptr
	tas d7			;failBMON?, ignore failBMON
	beq.b afromeaBMON
apmv2eaBMON:move.w #$200,d0
	bsr.w storeinstrBMON
	bsr.b apregBMON
	bra.w asstoeaBMON
afromeaBMON:moveq #0,d0
apmvfdBMON:	bsr.w storeinstrBMON
	bsr.w asseaBMON
	cmp.b #",",(a3)+
	bne.w failBMON
apregBMON:	lea pregsBMON(pc),a0
	move.l a3,a1
aprglpBMON:	move.b (a0)+,d1
	beq.w failBMON
aprgcmpBMON:moveq #$7f,d0
	and.b (a0),d0
	cmp.b (a3)+,d0
	bne.b aprgskpBMON
	tst.b (a0)+
	bpl.b aprgcmpBMON
	bra.b aprokBMON
aprgskpBMON:tst.b (a0)+
	bpl.b aprgskpBMON
	move.l a1,a3
	bra.b aprglpBMON
aprokBMON:	or.b d1,op68000+2
	rts
aidbcacBMON=*-assubsBMON
aidb3BMON:	move.b (a3)+,d1
	cmp.b #"c",(a3)+
	bne.b aidb2BMON
	moveq #4-1,d0
	lea idctxBMON(pc),a0
aidb1BMON:	cmp.b (a0,d0.w),d1
	beq.b aidb0BMON
	dbf d0,aidb1BMON
aidb2BMON:	bra.w failBMON
aidb0BMON:	lsl.w #6,d0
	or.w d0,op68000
	rts
aidbindBMON=*-assubsBMON
	bsr.b aidb3BMON
	cmp.b #",",(a3)+
	bne.w failBMON
apflush40BMON=*-assubsBMON
	cmp.b #"(",(a3)+
	bne.w failBMON
	bsr.w assax02BMON
	cmp.b #")",(a3)+
	bne.w failBMON
	rts

aimovecBMON=*-assubsBMON
	moveq #0,d3
	cmp.b #"t",(a3)		;movec tc, (68040)
	beq.b actlmvcBMON
	cmp.b #",",2(a3)	;xx,xxx(x)
	bne.b actlmvcBMON
	bset #0,op68000+1
	bsr.b amvcregBMON
	addq.w #1,a3	;cmp.b #",",(a3)+ bne.w failBMON
	bsr.b actrlmvBMON
	bra.b aimvc0BMON
actlmvcBMON:bsr.b actrlmvBMON
	cmp.b #",",(a3)+
	bne.w failBMON
	bsr.b amvcregBMON
aimvc0BMON:	move.w d3,d0
	bra.w storewordBMON
actrlmvBMON:lea cregsBMON-6(pc),a2
actrl1BMON:	addq.w #6,a2
	tst.b (a2)
	bmi.w actrl4BMON
	moveq #0,d1
actrl2BMON:	move.b 2(a2,d1.w),d0
	beq.b actrl0BMON
	cmp.b (a3,d1.w),d0
	bne.b actrl1BMON
	addq.b #1,d1
	cmp.b #4,d1
	bne.b actrl2BMON	
actrl0BMON:	or.w (a2),d3
	add.w d1,a3
	rts
actrl4BMON:	sf d7		;failBMON
	rts
amvcregBMON:move.b (a3)+,d0
	cmp.b #"d",d0
	beq.b mvcdxBMON
	bset #15,d3	;ax
	cmp.b #"a",d0
	bne.w actrl4BMON
mvcdxBMON:	bsr.w assrn02BMON
	ror.w #4,d0
	or.w d0,d3	;ax/dx
	rts

abfieldtodxBMON=*-assubsBMON
	bsr.b abfi3BMON
	cmp.b #",",(a3)+
	bne.w failBMON
	cmp.b #"d",(a3)+
	bne.w failBMON
	moveq #0,d3
	bsr.b mvcdxBMON
	or.w d3,op68000+2
	rts
adxtobfieldBMON=*-assubsBMON
	cmp.b #"d",(a3)+
	bne.w failBMON
	moveq #0,d3
	bsr.b mvcdxBMON
	cmp.b #",",(a3)+
	bne.w failBMON
	bsr.b abfi3BMON
	or.w d3,op68000+2
	rts
abfieldBMON=*-assubsBMON
abfi3BMON:	bsr.w storeinstrBMON
	bsr.w asseaBMON
	cmp.b #"{",(a3)+
	bne.w failBMON
	bsr.b abfi2BMON
	and.w #%111111,d0
	lsl.w #6,d0
	move.w d0,d2
	cmp.b #":",(a3)+
	bne.w failBMON
	bsr.b abfi2BMON
	and.w #%111111,d0
	or.w d2,d0
	cmp.b #"}",(a3)+
	bne.w failBMON
	move.w d0,op68000+2
	rts
abfi2BMON:	cmp.b #"d",(a3)
	beq.b abfi1BMON
	bsr.w atodnibBMON
	beq.w failBMON
	cmp.b #32,d0
	bhi.w failBMON
	bclr #5,d0	;bitfield32=0
	rts
abfi1BMON:	addq.w #1,a3
	move.b (a3)+,d0
	sub.b #"0",d0
	bcs.w failBMON
	cmp.b #7,d0
	bhi.w failBMON
	bset #5,d0
	rts
abkptBMON=*-assubsBMON
	cmp.b #"#",(a3)+
	bne.w failBMON
	bra.w assxx02BMON
apackBMON=*-assubsBMON
	cmp.b #"-",(a3)
	bne.b apadxBMON
	addq.w #1,a3
	cmp.b #"(",(a3)+
	bne.w failBMON
	bsr.w assax02BMON
	lea xbcdtxtBMON(pc),a0
	bsr.w cmptxtBMON
	bsr.w assax9bBMON
	cmp.b #")",(a3)+
	bne.w failBMON
	bset #3,op68000+1
	bra.b apad0BMON
apadxBMON:	bsr.w assdx02BMON
	bsr.w asstodx9bBMON
apad0BMON:	cmp.b #",",(a3)+
	bne.w failBMON
	bra.w aiwrdBMON
amov16BMON=*-assubsBMON
	cmp.b #"(",(a3)+
	bne.w failBMON
	bsr.w assax02BMON
	lea cmpmtxtBMON(pc),a0
	bsr.w cmptxtBMON
	bsr.w assan02BMON
	ror.w #4,d0
	or.w #$8000,d0
	bra.w storeinstrBMON
amov162BMON=*-assubsBMON
	cmp.b #"a",1(a3)	;(ax)
	bne.b amov0BMON
	cmp.b #"(",(a3)+
	bne.w failBMON
	bsr.w assax02BMON
	cmp.b #")",(a3)+
	bne.w failBMON
	moveq #%000000,d1
	cmp.b #"+",(a3)+
	beq.b amov1BMON
	subq.w #1,a3
	moveq #%010000,d1
amov1BMON:	cmp.b #",",(a3)+
	bne.w failBMON
	or.b d1,op68000+1
amov2BMON:	move.w op68000,-(a7)
	bsr.w asseaBMON
	moveq #%111111,d0
	and.w op68000,d0
	move.w (a7)+,op68000
	cmp.w #%111001,d0
	bne.w failBMON
	rts
amov0BMON:	bsr.b amov2BMON
	cmp.b #",",(a3)+
	bne.w failBMON
	cmp.b #"(",(a3)+
	bne.w failBMON
	bsr.w assax02BMON
	cmp.b #")",(a3)+
	bne.w failBMON
	moveq #%001000,d1
	cmp.b #"+",(a3)+
	beq.b amov3BMON
	moveq #%011000,d1
amov3BMON:	or.b d1,op68000+1
	rts

aimoveBMON=*-assubsBMON
	moveq #%01,d0
	sf imsizeBMON
	cmp.b #"b",(a3)
	beq.b aimov0BMON
	moveq #%11,d0
	addq.b #1,imsizeBMON
	cmp.b #"w",(a3)
	beq.b aimov0BMON
	moveq #%10,d0
	addq.b #1,imsizeBMON
	cmp.b #"l",(a3)
	bne.w failBMON
aimov0BMON:	lsl.b #4,d0
	or.b d0,op68000
	addq.w #1,a3
	bsr.w asstabBMON
	bsr.w asseaBMON
	move.w op68000,-(a7)
	bsr.w asstoeaBMON
	move.w op68000,d1
	move.w (a7)+,op68000
	move.w d1,d0
	and.w #%111,d0
	ror.w #7,d0
	or.w d0,op68000
	and.w #%111000,d1
	lsl.w #3,d1
	or.w d1,op68000
	rts
aimoveqBMON=*-assubsBMON
	sf imsizeBMON
	bsr.w asseaBMON
	move.w op68000,d0
	clr.l oplen
	and.w #%111111,d0	;#x
	cmp.w #%111100,d0
	bne.w failBMON
	move.b op68000+3,op68000+1
	bra.w asstodx9bBMON
aicallmBMON=*-assubsBMON
	cmp.b #"#",(a3)+
	bne.w failBMON
	bsr.w atobyte_BMON		;d0.l
	bsr.w storeinstrBMON
	bra.w asstoeaBMON

acasBMON=*-assubsBMON
	bsr.b assdn02BMON
	move.w d0,d2
	cmp.b #",",(a3)+
	bne.w failBMON
	bsr.b assdn02BMON
	lsl.w #6,d0
	or.w d2,d0
	bsr.w storeinstrBMON
	bra.w asstoeaBMON

assan02BMON:cmp.b #"a",(a3)+
	bne.w failBMON
	bra.b assrn02BMON
assdn02BMON:cmp.b #"d",(a3)+
	bne.w failBMON
assrn02BMON:move.b (a3)+,d0
	sub.b #"0",d0
	bcs.w failBMON
	cmp.b #7,d0
	bhi.w failBMON
	ext.w d0
	rts
acas2BMON=*-assubsBMON
	cmp.b #"w",(a3)
	beq.b aca0BMON	
	cmp.b #"l",(a3)
	bne.w failBMON
	bset #9-8,op68000
aca0BMON:	addq.w #1,a3
	bsr.w asstabBMON
	bsr.b assdn02BMON
	swap d0
	cmp.b #":",(a3)+
	bne.b cas2flBMON
	bsr.b assdn02BMON
	move.l d0,d1
	cmp.b #",",(a3)+
	bne.b cas2flBMON
	bsr.b assdn02BMON
	swap d0
	cmp.b #":",(a3)+
	bne.b cas2flBMON
	bsr.b assdn02BMON
	lsl.l #6,d0
	or.l d0,d1
	cmp.b #",",(a3)+
	bne.b cas2flBMON
	cmp.b #"(",(a3)+
	bne.b cas2flBMON
	moveq #0,d3
	bsr.w amvcregBMON
	cmp.b #")",(a3)+
	bne.b cas2flBMON
	cmp.b #":",(a3)+
	bne.b cas2flBMON
	cmp.b #"(",(a3)+
	bne.b cas2flBMON
	swap d3
	bsr.w amvcregBMON
	cmp.b #")",(a3)+
	bne.b cas2flBMON
	or.l d1,d3
	move.l d3,d0
	bra.w storelongBMON
cas2flBMON:	sf d7
	rts
achk2BMON=*-assubsBMON
	move.w #$0800,d3
	bra.b achkcmpBMON
acmp2BMON=*-assubsBMON
	moveq #0,d3
achkcmpBMON:bsr.w storeinstrBMON
	bsr.w asseaBMON
	cmp.b #",",(a3)+
	bne.b cas2flBMON
	bsr.w amvcregBMON	;or.w ?,d3
	move.w d3,op68000+2
	rts

artmBMON=*-assubsBMON
	moveq #0,d3
	bsr.w amvcregBMON
	rol.w #4,d3
	or.w d3,op68000	;0000ynnn
	rts

atppccsizeBMON=*-assubsBMON
	bsr.w asspccodeBMON
	bra.b atpc2BMON
atpfccsizeBMON=*-assubsBMON
	bsr.w assfccodeBMON
atpc2BMON:	bsr.w storeinstrBMON
	bra.b atpc1BMON
atpccsizeBMON=*-assubsBMON
	bsr.w assccodeBMON
atpc1BMON:	cmp.b #".",(a3)+
	bne.w failBMON
	move.b (a3)+,d0
	bsr.w asstabBMON
	cmp.b #"w",d0
	beq.w aiwrdBMON
	cmp.b #"l",d0
	bne.w failBMON
	bset #0,op68000+1
atpc0BMON:	cmp.b #"#",(a3)+
	bne.w failBMON
	bsr.w atolong_BMON
	bra.w storelongBMON
atpccBMON=*-assubsBMON
	bsr.w assccodeBMON
	cmp.b #" ",(a3)+
	bne.w failBMON
	rts

apvalidBMON=*-assubsBMON
	bsr.w assan02BMON
	or.w #$2c00,d0
	bsr.b afstoreBMON
	bra.w asstoeaBMON
atppccBMON=*-assubsBMON
	bsr.w asspccodeBMON
	bra.b afst0BMON
atpfccBMON=*-assubsBMON
	bsr.w assfccodeBMON
afst0BMON:	cmp.b #" ",(a3)+
	bne.w failBMON
afstoreBMON:bra.w storeinstrBMON

afnopBMON=*-assubsBMON
	moveq #0,d0		;4byte command
	bra.b afstoreBMON
apflushaBMON=*-assubsBMON
	move.w #$2400,d0
	bra.b afstoreBMON
apflushrBMON=*-assubsBMON
	move.w #$a000,d0
	bsr.b afstoreBMON
	bra.w asseaBMON
apflushsBMON=*-assubsBMON
	move.w #$3400,d3	
	bra.b apfl3BMON
apflushBMON=*-assubsBMON
	move.w #$3000,d3	
apfl3BMON:	move.b (a3)+,d2
	cmp.b #"f",(a3)
	bne.b apfl1BMON
	addq.w #1,a3
	cmp.b #"c",(a3)+
	bne.w failBMON
	moveq #%00000000,d1
	cmp.b #"s",d2
	beq.b apfl2BMON
	moveq #%00000001,d1
	cmp.b #"d",d2
	beq.b apfl2BMON
	bra.w failBMON
apfl1BMON:	moveq #%00010000,d1
	moveq #"#",d0
	sub.b d2,d0
	beq.b apfl0BMON
	cmp.b #"#"-"d",d0
	bne.w failBMON
	moveq #%00001000,d1
apfl0BMON:	bsr.w assrn02BMON
	or.w d0,d1
apfl2BMON:	cmp.b #",",(a3)+
	bne.w failBMON
	cmp.b #"#",(a3)+
	bne.w failBMON
	bsr.w assrn02BMON
	lsl.w #5,d0
	or.w d1,d0
	or.w d3,d0	;$3000|$3400
	bra.b afstoreBMON
aploadBMON=*-assubsBMON
	move.w #$2010,d1
apte0BMON:	moveq #"w",d0
	sub.b (a3)+,d0
	beq.b aplo0BMON
	bset #9,d1
	subq.b #"w"-"r",d0
	bne.w failBMON
aplo0BMON:	bsr.w asstabBMON
	cmp.b #"#",(a3)+
	bne.w failBMON
	bsr.w assrn02BMON
	or.w d1,d0
	bsr.w afstoreBMON	
	bra.w asstoeaBMON
aptestBMON=*-assubsBMON
	move.w #$8110,d1
	bsr.b apte0BMON
	cmp.b #",",(a3)+
	bne.w failBMON
	cmp.b #"#",(a3)+
	bne.w failBMON
	bsr.w assrn02BMON
	ror.w #6,d0
	beq.w failBMON	;level 0?
	move.w d0,d1
	cmp.b #",",(a3)+
	bne.w failBMON
	cmp.b #"a",(a3)+
	bne.w failBMON
	bsr.w assrn02BMON
	rol.w #5,d0
	or.w d1,d0
	or.w d0,op68000+2
	rts
	
adivsllBMON=*-assubsBMON
	move.w #$800,d0
	bra.b adivs0BMON
adivullBMON=*-assubsBMON
	moveq #0,d0
adivs0BMON:	bsr.w storeinstrBMON
	move.b #%10,imsizeBMON
	bsr.w asseaBMON
	cmp.b #",",(a3)+
	bne.w failBMON
	bsr.w assdn02BMON		;d0.w
	move.w d0,d3
	cmp.b #":",(a3)+
	bne.w failBMON
	bsr.w assdn02BMON
	cmp.b d0,d3
	beq.w failBMON
	bra.b notwoBMON	
amulslBMON=*-assubsBMON
	move.w #$800,d0
	bra.b amuls0BMON
amululBMON=*-assubsBMON
	moveq #0,d0
amuls0BMON:	bsr.w storeinstrBMON
	move.b #%10,imsizeBMON
	bsr.w asseaBMON
	cmp.b #",",(a3)+
	bne.w failBMON
	bsr.w assdn02BMON		;d0.w
	move.w d0,d3
	cmp.b #":",(a3)+
	bne.b notwoBMON
	bset #10,d3
	bsr.w assdn02BMON
notwoBMON:	ror.w #4,d0		;mulx.l ea,dm:dm=dn
	or.w d0,d3
	or.w d3,op68000+2
	rts

aicmpmBMON=*-assubsBMON
	cmp.b #"(",(a3)+
	bne.w failBMON
	bsr.w assax02BMON
	lea cmpmtxtBMON(pc),a0
	bsr.w cmptxtBMON
	bsr.w assax9bBMON
	cmp.b #")",(a3)+
	bne.w failBMON
	cmp.b #"+",(a3)+
	bne.w failBMON
	rts

asize67adsbxBMON=*-assubsBMON		;same!
axbcdBMON=*-assubsBMON
aasxbcdBMON:cmp.b #"-",(a3)+
	bne.b axbcd0BMON
	bset #3,op68000+1
	cmp.b #"(",(a3)+
	bne.b axbcd2BMON		;failBMON
	bsr.w assax02BMON
	lea xbcdtxtBMON(pc),a0
	bsr.w cmptxtBMON
	bsr.w assax9bBMON
	cmp.b #")",(a3)+
	beq.b axbcd1BMON		;no failBMON
axbcd2BMON:	sf d7
axbcd1BMON:	rts
axbcd0BMON:	subq.w #1,a3
	bsr.w assdx02BMON
	bra.w asstodx9bBMON

asize6movepBMON=*-assubsBMON
	cmp.b #"d",(a3)
	beq.b amovp0BMON
	bsr.w asseaBMON
	move.w op68000,d0
	bclr #5,op68000+1
	and.w #%111000,d0
	cmp.w #%101000,d0
	bne.w failBMON
	bra.w asstodx9bBMON
amovp0BMON:	bset #7,op68000+1
	bsr.w assdx9bBMON
	bsr.w asstoeaBMON
	move.w op68000,d0
	bclr #5,op68000+1
	and.w #%111000,d0
	cmp.w #%101000,d0
	bne.w failBMON
	rts
aimovemBMON=*-assubsBMON
	moveq #0,d0
	bsr storeinstrBMON
	cmp.b #"d",(a3)
	beq.b aimovm0BMON
	cmp.b #"a",(a3)
	beq.b aimovm0BMON
	bset #10-8,op68000
	bsr.w asseaBMON
	cmp.b #",",(a3)+
	bne.w failBMON
	bsr.b assrxBMON
	bra.b aimovm7BMON
aimovm0BMON:bsr.b assrxBMON
	bsr.w asstoeaBMON
aimovm7BMON:moveq #%111000,d0
	and.w op68000,d0
	cmp.w #%100000,d0	;-(an)
	bne.b aimovm8BMON
	move.w op68000+2,d1	;rl
	moveq #16-1,d0
aimovm9BMON:lsr.w #1,d1
	roxl.w #1,d2
	dbf d0,aimovm9BMON
	move.w d2,op68000+2
aimovm8BMON:rts
assrxBMON:	clr.w d0
aimovm2BMON:move.b 1(a3),d1
	sub.b #"0",d1
	bcs.w failBMON
	cmp.b #7,d1
	bhi.w failBMON
	cmp.b #"d",(a3)
	beq.b aimovm5BMON
	cmp.b #"a",(a3)
	bne.w failBMON
	addq.b #8,d1
aimovm5BMON:addq.w #2,a3
	bset d1,d0
aimovm4BMON:cmp.b #",",(a3)
	beq.b aimovm1BMON
	cmp.b #"/",(a3)+
	beq.b aimovm2BMON
	cmp.b #"-",-1(a3)
	bne.b aimovm1BMON
	move.b 1(a3),d2
	sub.b #"0",d2
	bcs.w failBMON
	cmp.b #7,d2
	bhi.w failBMON
	cmp.b #"d",(a3)
	beq.b aimovm6BMON
	cmp.b #"a",(a3)
	bne.w failBMON
	addq.b #8,d2
aimovm6BMON:addq.w #2,a3
	cmp.b d1,d2
	bls.w failBMON
aimovm3BMON:addq.b #1,d1
	bset d1,d0	
	cmp.b d1,d2
	bne.b aimovm3BMON
	bra.b aimovm4BMON
aimovm1BMON:move.w d0,op68000+2
	rts

adx9btodx02BMON=*-assubsBMON
	bsr.b assdx9bBMON
	bra.w asstodx02BMON
aax9btoax02BMON=*-assubsBMON
	bsr.b assax9bBMON
	bra.b asstoax02BMON
adx9btoax02BMON=*-assubsBMON
	bsr.b assdx9bBMON
	bra.b asstoax02BMON
aiunlkBMON=*-assubsBMON
	bra.b assax02BMON
alinklgBMON=*-assubsBMON
	bsr.b assax02BMON
	cmp.b #",",(a3)+
	bne.w failBMON
	bra.w atpc0BMON
ailinkBMON=*-assubsBMON
	bsr.b assax02BMON
	cmp.b #",",(a3)+
	bne.w failBMON
	bra.w aiwrdBMON

asstoax9bBMON:cmp.b #",",(a3)+
	bne.w failBMON
assax9bBMON:cmp.b #"a",(a3)+
	beq.b assxx9bBMON
	bra.w failBMON
asstodx9bBMON:cmp.b #",",(a3)+
	bne.w failBMON
assdx9bBMON:cmp.b #"d",(a3)+
	bne.w failBMON
assxx9bBMON:move.b (a3)+,d0
	sub.b #"0",d0
	bcs.w failBMON
	cmp.b #7,d0
	bhi.w failBMON	
	lsl.w #8,d0
	add.w d0,d0			;asl.w #1,d0
	or.w d0,op68000
	rts
asstoax02BMON:cmp.b #",",(a3)+
	bne.w failBMON
assax02BMON:cmp.b #"a",(a3)+
	bne.w failBMON
	bra.b assxx02BMON
asstodx02BMON:cmp.b #",",(a3)+
	bne.w failBMON
assdx02BMON:cmp.b #"d",(a3)+
	bne.w failBMON
assxx02BMON:move.b (a3)+,d0
	sub.b #"0",d0
	bcs.w failBMON
	cmp.b #7,d0
	bhi.w failBMON
	or.b d0,op68000+1
	rts

	ifnd failBMON
failBMON:	sf d7
	rts
	endc

asstofpx79BMON:
	cmp.b #",",(a3)+
	bne.w failBMON
assfpx79BMON:cmp.b #"f",(a3)+
	bne.w failBMON
	cmp.b #"p",(a3)+
	bne.w failBMON
	move.b (a3)+,d0
	sub.b #"0",d0
	bcs.w failBMON
	cmp.b #7,d0
	bhi.w failBMON
	ext.w d0
	lsl.w #7,d0
	or.w d0,op68000+2	;fpu instrBMON!
	rts
assccodeBMON:clr.w d0
	lea ccodetxtBMON(pc),a0
asscc0BMON:	move.b (a3),d1
	lsl.w #8,d1
	move.b 1(a3),d1
	tst.b 1(a0)		;"t",0;"f",0
	bne.b asscc2BMON
	sf d1
asscc2BMON:	cmp.w (a0)+,d1
	beq.b asscc1BMON
	add.w #$0100,d0
	cmp.w #$1000,d0
	bne.b asscc0BMON
	bra.w failBMON
asscc1BMON:	addq.w #1,a3
	tst.b d1		;"t",0;"f",0
	beq.b asscc3BMON
	addq.w #1,a3
asscc3BMON:	or.w d0,op68000
	rts

assfccodeBMON:
	moveq #124,d0		;(31-1)*4
assfcc2BMON:lea fccodetxtBMON(pc),a0
	lea (a0,d0.w),a0
	move.l a3,-(a7)
	moveq #4-1,d1
assfcc0BMON:cmpm.b (a3)+,(a0)+
	bne.b assfcc1BMON
	tst.b (a0)
	dbeq d1,assfcc0BMON
	cmp.b #"e",(a3)		;ngle preference
	beq.b assfcc1BMON
	addq.w #4,a7		;remove a3
	lsr.w #2,d0
	rts		
assfcc1BMON:move.l (a7)+,a3
	subq.w #4,d0
	bpl.b assfcc2BMON
	bra.w failBMON

asspccodeBMON:
	move.b (a3)+,d1
	lsl.w #8,d1
	move.b (a3)+,d1
	lea pccodetxtBMON(pc),a0
	moveq #30,d0		;(16-1)*2
asspcc0BMON:cmp.w (a0,d0.w),d1
	beq.b asspcc1BMON
	subq.w #2,d0
	bpl.b asspcc0BMON
	bra.w failBMON
asspcc1BMON:lsr.w #1,d0
	rts

cmpt0BMON:	cmp.b (a3)+,d0
	bne.w failBMON
cmptxtBMON:	move.b (a0)+,d0
	bclr #7,d0
	beq.b cmpt0BMON
	cmp.b (a3)+,d0
	bne.w failBMON
	rts

storeinstrBMON:addq.l #2,lginstrBMON
storewordBMON:
	lea op68000-2,a0
	move.w (a0),d1		;oplen!
	addq.w #1,(a0)+
	add.w d1,d1
	move.w d0,2(a0,d1.w)
	rts
storelongBMON:
	lea op68000-2,a0
	move.w (a0),d1		;oplen!
	addq.w #2,(a0)+
	add.w d1,d1
	move.l d0,2(a0,d1.w)
	rts

asstabBMON:	cmp.l lineendBMON,a3
	bhi.w failBMON
	cmp.b #" ",(a3)+
	beq.b asstabBMON
asstab0BMON:subq.w #1,a3
	rts	

asstoeaBMON:cmp.b #",",(a3)+
	bne.w failBMON
asseaBMON:	and.w #$ffc0,op68000
	cmp.b #"d",(a3)			;dx
	beq.w assdx02BMON
	or.w #$0008,op68000
	cmp.b #"a",(a3)			;ax
	beq.w assax02BMON
	and.w #$ffc0,op68000
	cmp.b #"(",(a3)			;(ax)|(ax)+
	bne.b assea0BMON
	cmp.b #"a",1(a3)
	bne.b assea0BMON
	addq.w #1,a3
	or.w #$0010,op68000
	bsr.w assax02BMON
	cmp.b #")",(a3)+
	bne.w failBMON
	cmp.b #"+",(a3)
	bne.b assea1BMON
	addq.w #1,a3
	or.w #$0018,op68000
assea1BMON:	rts
assea0BMON:	cmp.b #"-",(a3)		;-(ax)
	bne.b assea2BMON
	cmp.b #"(",1(a3)
	bne.w failBMON
	addq.w #2,a3
	or.w #$0020,op68000
	bsr.w assax02BMON
	cmp.b #")",(a3)+
	bne.w failBMON
	rts
assea2BMON:	cmp.b #"#",(a3)
	bne.w assea3BMON
	addq.w #1,a3
	or.w #$003c,op68000
	cmp.b #34,(a3)
	bne.b assea16BMON
	addq.w #1,a3
	moveq #0,d0
	moveq #5-1,d1
assea18BMON:cmp.b #34,(a3)+
	beq.b assea17BMON
	lsl.l #8,d0
	move.b -1(a3),d0
	dbf d1,assea18BMON
	bra.w failBMON
assea16BMON:cmp.b #"$",(a3)
	beq.b assea22BMON
	bsr.w atodlongBMON
	bra.b assea17BMON
assea22BMON:bsr.w atolong_BMON
assea17BMON:move.b imsizeBMON,d1
	beq.b assea13BMON

	cmp.b #%01,d1
	beq.b assea14BMON
	cmp.b #%10,d1
	bne.w failBMON
	swap d0
	bsr.b assea15BMON
	swap d0
	bra.b assea15BMON
assea13BMON:cmp.l #$ff,d0
	bhi.w failBMON
assea14BMON:cmp.l #$ffff,d0
	bhi.w failBMON
assea15BMON:bra.w storewordBMON	

assea3BMON:	cmp.b #"(",(a3)+	;(x.l),(x.w),(d,ax,rx.x)
	bne.w failBMON
	sf d2
	cmp.b #"-",(a3)		;sign?
	bne.b assea19BMON
	addq.w #1,a3
	st d2
assea19BMON:move.w d2,-(a7)
	cmp.b #"$",(a3)
	beq.b assea24BMON
	bsr.w atodlongBMON
	bra.b assea23BMON
assea24BMON:bsr.w atolong_BMON
assea23BMON:move.w (a7)+,d2
	cmp.b #",",(a3)		;d(	!(d,	?
	bne.w assea4BMON
	tst.b d2
	beq.b assea20BMON
	neg.l d0
assea20BMON:addq.w #1,a3
	cmp.b #"a",(a3)		;d(ax	!(d,ax
	bne.w assea5BMON
	move.l d0,-(a7)
	bsr.w assax02BMON
	move.l (a7)+,d0
	move.l d0,d1
	bpl.b assea21BMON
	neg.l d1
assea21BMON:cmp.b #")",(a3)+	;d(ax)	!(d,ax)
	bne.b assea6BMON
	or.w #$0028,op68000
	cmp.l #$ffff,d1		;abs(d0)
	bhi.w failBMON
	bra.w storewordBMON
assea6BMON:	cmp.b #",",-1(a3)	;	!(d,ax,rx.x)
	bne.w failBMON
	or.w #$0030,op68000
	cmp.l #$ff,d1		;abs(d0)
	bhi.w failBMON
assea9BMON:	and.w #$ff,d0
	cmp.b #"d",(a3)+
	beq.b assea7BMON
	cmp.b #"a",-1(a3)
	bne.w failBMON
	bset #15,d0
assea7BMON:	move.b (a3)+,d1
	sub.b #"0",d1
	bcs.w failBMON		
	cmp.b #7,d1
	bhi.w failBMON
	ext.w d1
	ror.w #4,d1
	or.w d1,d0
	cmp.b #".",(a3)+
	bne.w failBMON
	cmp.b #"w",(a3)+
	beq.b assea8BMON
	cmp.b #"l",-1(a3)
	bne.w failBMON
	bset #11,d0
assea8BMON:	cmp.b #"*",(a3)
	bne.b assea8bBMON
	addq.w #1,a3
	moveq #-"1",d1
	add.b (a3)+,d1
	beq.b assea8bBMON
	or.w #%01000000000,d0
	subq.b #1,d1
	beq.b assea8bBMON
	eor.w #%11000000000,d0
	subq.b #2,d1
	beq.b assea8bBMON
	eor.w #%01000000000,d0
	subq.b #4,d1
	bne.w failBMON		
assea8bBMON:cmp.b #")",(a3)+	;d(ax,rx.x)	!(d,ax,rx.x)
	bne.w failBMON
	bra.w storewordBMON
assea5BMON:	cmp.b #"p",(a3)+
	bne.w failBMON
	cmp.b #"c",(a3)+
	bne.w failBMON
	cmp.b #")",(a3)+		;d(pc)	!(d,pc)
	bne.b assea10BMON
	or.w #$003a,op68000
	subq.l #2,d0			;fixed instrBMON len
	sub.l lginstrBMON,d0		;extension (mulx.l,cas2...)
	sub.l paraBMON,d0
	bsr.w storewordBMON
	move.l d0,d1
	ext.l d1
	cmp.l d0,d1
	bne.w failBMON
	rts
assea10BMON:or.w #$003b,op68000
	subq.l #2,d0			;fixed instrBMON len
	sub.l lginstrBMON,d0		;extension (mulx.l,cas2...)
	sub.l paraBMON,d0
	move.b d0,d1
	ext.w d1
	ext.l d1
	cmp.l d1,d0
	bne.b assfailBMON
	cmp.b #",",-1(a3)
	beq.w assea9BMON			;d(pc,rx.x)	!(d,pc,rx.x)
assfailBMON:sf d7
	rts
assea4BMON:	cmp.b #")",(a3)+
	bne.w failBMON
	cmp.b #".",(a3)
	beq.b assea11BMON
	or.w #$0039,op68000
	swap d0
	bsr.w storewordBMON
	swap d0
	bra.w storewordBMON
assea11BMON:addq.w #1,a3
	cmp.b #"w",(a3)+	
	bne.w failBMON
	or.w #$0038,op68000
	cmp.l #$ffff,d0
	bhi.w failBMON
	bra.w storewordBMON

;---------------------------------------------------------------
	ifnd ccodetxtBMON

ccodetxtBMON:	dc.b "t",0,"f",0,"hilscccsneeqvcvsplmigeltgtle"	;even!
pccodetxtBMON:	dc.b "bsbclslcssscasacwswcisicgsgccscc"	;even!
fccodetxtBMON:	dc.l "f"<<24	;fpu condition codes
		dc.l "eq"<<16
		dc.l "ogt"<<8
		dc.l "oge"<<8
		dc.l "olt"<<8
		dc.l "ole"<<8
		dc.l "ogl"<<8
		dc.l "or"<<16
		dc.l "un"<<16
		dc.l "ueq"<<8
		dc.l "ugt"<<8
		dc.l "uge"<<8
		dc.l "ult"<<8
		dc.l "ule"<<8
		dc.l "ne"<<16
		dc.l "t"<<24
		dc.l "sf"<<16
		dc.l "seq"<<8
		dc.l "gt"<<16
		dc.l "ge"<<16
		dc.l "lt"<<16
		dc.l "le"<<16
		dc.l "gl"<<16
		dc.l "gle"<<8
		dc.l "ngle"
		dc.l "ngl"<<8
		dc.l "nle"<<8
		dc.l "nlt"<<8
		dc.l "nge"<<8
		dc.l "ngt"<<8
		dc.l "sne"<<8
		dc.l "st"<<16

fmconstBMON:dc.b "1","E"+$80,"P","i"+$80,"Log10(2",")"+$80,"e"+$80,"Log2(e",")"+$80
	dc.b "Log10(e",")"+$80,"0.","0"+$80,"ln(2",")"+$80,"ln(10",")"+$80
	dc.b "1"+$80,"1","0"+$80,"10","0"+$80

usptxtBMON:		dc.b "us","p"+$80
toccrtxtBMON:	dc.b ","
ccrtxtBMON:		dc.b "cc","r"+$80
tosrtxtBMON:	dc.b ","
srtxtBMON:		dc.b "s","r"+$80
xbcdtxtBMON:	dc.b "),-","("+$80
cmpmtxtBMON:	dc.b ")+,","("+$80
		even
cregsBMON:	dc.b $08,$08,"pcr",0	;68060
		dc.b $08,$07,"srp",0	;68040
		dc.b $08,$06,"urp",0	;68040
		dc.b $08,$05,"mmur"	;68040
		dc.b $08,$04,"isp",0
		dc.b $08,$03,"msp",0
		dc.b $08,$02,"caar"	;68020/30 only
		dc.b $08,$01,"vbr",0
		dc.b $08,$00,"usp",0
		dc.b $00,$08,"busr"	;68060
		dc.b $00,$07,"dtt1"	;68040
		dc.b $00,$06,"dtt0"	;68040
		dc.b $00,$05,"itt1"	;68040
		dc.b $00,$04,"itt0"	;68040
		dc.b $00,$03,"tc",0,0	;68040
		dc.b $00,$02,"cacr"
		dc.b $00,$01,"dfc",0
		dc.b $00,$00,"sfc",0
		dc.b $80
pregsBMON:	dc.b $08,"tt","0"+$80
		dc.b $0c,"tt","1"+$80
		dc.b $40,"t","c"+$80
		dc.b $44,"dr","p"+$80	;68851 only
		dc.b $48,"sr","p"+$80
		dc.b $4c,"cr","p"+$80
		dc.b $50,"ca","l"+$80	;68851 only
		dc.b $58,"sc","c"+$80	;68851 only
		dc.b $5c,"a","c"+$80	;68851 only
		dc.b $60,"mmus","r"+$80	;68030 mmusr = 68851 psr
		dc.b $60,"ps","r"+$80	;68030 mmusr = 68851 psr
		dc.b $64,"pcs","r"+$80	;68851 only
		dc.b $ac,"va","l"+$80,0	;68851 only

sizemovBMON:	dc.b "*blw",%11,%00,%10,%01
dcbtxtBMON:	dc.b "dc.b "!$80

idctxBMON:	dc.l "ndib"	;nc,dc,ic,bc

	endc


paraBMON	dc.l 0			;address

lineendBMON	dc.l 0

lginstrBMON	dc.l 0
oplen		dc.w 0			;nb word for instrBMON.
op68000		dc.w 0,0,0,0,0,0,0	;assembled instrBMON. here !!
		ifnd imsizeBMON
imsizeBMON	dc.b 0
		even
		endc

