;APS00000000000000000000000000000000000000000000000000000000000000000000000000000000

; $Id: disassemble.s 1.2 2000/11/24 19:51:31 jah Exp jah $

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


* Beermon Disassembler modified for HRTmon
* 68000-68040/FPU/MMU

;13.06.09	- display negative value in quick move instr. (moveq #$ff,dx -> moveq #-1,dx)
;		- disassemble immediate byte intr. even if the 1st data byte is not empty
;		  add/and/cmp/eor/move/ori.b #$??xx,<ea>
;		  this bug is done when code is generated with old asm prog (ie. seka: move.b #-1,dx)
;20.02.10	- null offset of bit field instruction will be "0" displayed instead of "32" now
;		- bchg/bclr/bset/btst #xx,<ea> , bit number is correctly displayed if value > 32
;		  it still disassembled if bit number > 255, but it adds "??" before it now

;;;HRTeval equ -1		;use evaluate routine from HRTmon ? (for assemble)
HRTeval equ 0		;use evaluate routine from HRTmon ? (for assemble)

;s
;	lea.l	$f800d2,a4
;	lea.l	destbuf(pc),a0
;	bsr	disassemble
;	rts
;
;destbuf		dcb.b 80,0
;		dc.b 0
;		even
;
;evaluate
*****************************************************************************
;-> a4=address to dis
;-> a0=ptr on dest string
;-> d0=%0000NRAU
;	U=1 upper case
;	A=1 print 'd $address'
;	R=1 print indirect address
;	N=1 print rd <rd_mode>.w $address
;<- d0=len of instr.

baseBMON

xcharsBMON equ 80



failBMON:	sf d7
	rts


disassemble:
	movem.l d1-d7/a0-a3/a5-a6,-(a7)	;yo, funky again...
	lea.l	baseBMON(pc),a6
	move.b	d0,modeBMON
	move.l	a4,a5
	move.l	a5,disaddrBMON
	move.l	a0,linebufBMON

	lea instrBMON(pc),a4	;a5:pc
	move.l a5,d6
dis2BMON:	moveq #xcharsBMON/4-1-1,d0		;redisBMON
	move.l	linebufBMON,a0
	move.l #"    ",d1	;fill linebufBMON
dis3BMON:	move.l d1,(a0)+
	dbf d0,dis3BMON
	move.b	#$a,(a0)+
	clr.b	(a0)+

	sf ea2ndBMON			;begin with 1st eaposBMON
	move.l d6,a5
	move.w (a5)+,d1		;instrBMON.w
dis0BMON:	move.w d1,d0
	and.w (a4)+,d0		;mask
	cmp.w (a4),d0		;instrframe
	lea 14-2(a4),a4
	bne.b dis0BMON		;scan instrBMON
	move.w 4-14(a4),d2		;info.w
	move.l d6,d0
	move.l	linebufBMON,a0
	btst	#1,modeBMON
	beq.b	.checkrd
	move.w #"d ",(a0)+
	move.b	#"$",(a0)+
	bsr.w longtoaBMON
	addq.w #1,a0
	bra.s	.noaddress

.checkrd
	btst	#3,modeBMON
	beq.s	.noaddress
	move.l #"rd $",(a0)+
	moveq.l	#0,d0
	move.w	rd_mode,d0
	bsr.w	bytetoaBMON
	move.w	#" $",(A0)+
	move.l	d6,d0
	bsr.w longtoaBMON
	addq.w #1,a0

.noaddress

	lea -14+6(a4),a1
dis1BMON:	move.b (a1)+,(a0)+
	bpl.b dis1BMON
	bclr #7,-1(a0)		;end

	st d7
	move.w d2,d4
	and.w #$ff00,d4		;sizeopt
	cmp.w #s67BMON,d4
	bne.b nos67BMON
	bsr.w size67BMON
nos67BMON:	cmp.w #s9a_BMON,d4
	bne.b nos9a_BMON
	bsr.w size9a_BMON
nos9a_BMON:	cmp.w #s9aBMON,d4
	bne.b nos9aBMON
	bsr.w size9aBMON
nos9aBMON:	cmp.w #s6BMON,d4
	bne.b nos6BMON
	bsr.w size6BMON
nos6BMON:	cmp.w #s8BMON,d4
	bne.b nos8BMON
	bsr.w size8BMON
nos8BMON:	cmp.w #stabBMON,d4
	bne.b nostabBMON
	bsr.w opsposBMON
nostabBMON:	;cmp.w #snopBMON,d4
	;bne.b nosnopBMON
nosnopBMON:
	lea disoffsBMON(pc),a1
	ext.w d2		;clr sizeopt
	add.w d2,d2
	move.w (a1,d2.w),d2
	move.l d6,-(a7)
	jsr dissubsBMON(pc,d2.w)		;no d7,a4
	move.l (a7)+,d6
	tst.b d7
	beq.w dis2BMON

	move.l	a5,a4				;next instr. addr in a4
	sub.l	disaddrBMON,a5
	move.l	a5,d0				;d0=len of instr.

	btst	#0,modeBMON		;upper case ?
	beq.b	.end
	move.l	linebufBMON,a0
.up	move.b	(a0)+,d1
	beq.b	.end
	cmp.b	#'a',d1
	blt.b	.up
	cmp.b	#'z',d1
	bgt.b	.up
	sub.b	#'a'-'A',d1
	move.b	d1,-1(a0)
	bra.b	.up
.end	movem.l (a7)+,d1-d7/a0-a3/a5-a6
	rts

dissubsBMON:
opfailBMON=*-dissubsBMON
	moveq #-1,d0
opf0BMON:	move.w d0,$dff180
	dbf d0,opf0BMON
noopsBMON=*-dissubsBMON
	rts

dcwopBMON=*-dissubsBMON
	move.w d1,d0
	bra.w wordtoa_BMON

eatoccrBMON=*-dissubsBMON
	sf imsizeBMON
	bsr.w doeaBMON
	bra.w optoccrBMON

eatosrBMON=*-dissubsBMON
	move.b #1,imsizeBMON
	bsr.w doeaBMON
	bra.w optosrBMON

ccrtoeaBMON=*-dissubsBMON
	bsr.w opccrBMON
	bra.b dissreaBMON
srtoeaBMON=*-dissubsBMON
	bsr.w opsrBMON
dissreaBMON:bra.w dotoeaBMON

fpufmt2BMON:dc.b "lsxpwdbp"
dfmvmemBMON:lea movetxtBMON+1(pc),a1	;fmove
	bsr txttobufBMON
	move.b #".",(a0)+
	move.w d6,d0
	rol.w #6,d0
	and.w #%111,d0
	move.b (fpufmt2BMON,pc,d0.w),(a0)+
	bsr opsposBMON
	moveq #%111000,d2
	and.w d1,d2
	bne.b no2dregBMON		;dn ?
	tst.b d0	;%000.l
	beq.b no2dregBMON
	subq.b #1,d0	;%001.sBMON
	beq.b no2dregBMON	
	subq.b #3,d0	;%100.w
	beq.b no2dregBMON
	subq.b #2,d0	;%110.b
	bne.b dmvfailBMON
no2dregBMON:cmp.b #%001000,d2	;an ?
	beq.b dmvfailBMON
	moveq #%01111111,d5
	and.w d6,d5		;k factor
	moveq #1,d3		;static k
	subq.b #%011,d0		;p{#k}
	beq.b dfmvk0BMON
	moveq #2,d3		;dynamic k
	subq.b #%111-%011,d0	;p{dn}
	beq.b dfmvk0BMON
	tst.b d5
	bne.b dmvfailBMON		;k factor>0 ?
	moveq #0,d3		;no k factor
dfmvk0BMON:	move.w d6,d0
	lsr.w #7,d0
	and.w #%111,d0
	bsr.w dfp5BMON		;fpn
	bsr dotoeaBMON
	subq.b #1,d3		;k factor ?
	bmi.b dfmvk1BMON
	move.b #"{",(a0)+
	tst.b d3		;static ?
	beq.b dfmvkp0BMON
	moveq #%1111,d0
	and.b d5,d0
	bne.b dmvfailBMON		;unused bitsBMON
	move.b #"d",(a0)+
	lsr.b #4,d5 
	bra.b dfmvkpBMON
dfmvkp0BMON:move.b #"#",(a0)+
	btst #6,d5
	beq.b dfmvkpBMON
	bset #7,d5
	neg.b d5
	move.b #"-",(a0)+
dfmvkpBMON:	move.l d5,d0
	bsr.w hextodBMON
	move.b #"}",(a0)+
dfmvk1BMON:	rts
dmvfailBMON:bra failBMON

dfpuBMON=*-dissubsBMON
	move.w (a5)+,d6		;fpu extension word
	move.w #$e000,d0
	and.w d6,d0
	cmp.w #$6000,d0
	beq.w dfmvmemBMON
	move.w #$a000,d0
	and.w d6,d0
	bne.b dffailBMON
	btst #14,d6		;r/m
	bne.b dfp0BMON
	tst.b d1		;ea field=0?
	bne.b dffailBMON
dfp0BMON:	lea fpuinstBMON(pc),a1
	moveq #%01111111,d0
	and.w d6,d0
dfp2BMON:	move.b (a1)+,d2
	bclr #7,d2
	sne d3		;0:fp0,ff:fp0,fp0
	moveq #%01111000,d4
	and.b d6,d4
	cmp.b #%00110000,d4	;sincosBMON
	seq d4
	beq.b dfp1BMON
	cmp.b d2,d0
	beq.b dfp1BMON
	addq.w #6,a1
	tst.b (a1)
	bne.b dfp2BMON
dffailBMON:	sf d7
	rts
fpufmtBMON:	dc.b "lsxpwdb*"
fpuimsBMON:	dc.b %010,%010,%110,%110,%001,%100,%000,-1
dfp1BMON:	bsr.w txttobufBMON
	move.b #".",(a0)+
	move.w d6,d0
	rol.w #6,d0
	and.w #%111,d0
	btst #14,d6
	beq.b dfp3BMON
	cmp.w #%111,d0
	beq.b dffailBMON
	move.b (fpufmtBMON,pc,d0),(a0)+
	move.b (fpuimsBMON,pc,d0.w),imsizeBMON
	bsr.w opsposBMON
	moveq #%111000,d2
	and.w d1,d2	;src:datareg?
	bne.b dfp8BMON
	tst.b d0	;%000.l
	beq.b dfp8BMON
	subq.b #1,d0	;%001.sBMON
	beq.b dfp8BMON	
	subq.b #3,d0	;%100.w
	beq.b dfp8BMON
	subq.b #2,d0	;%110.b
	bne.b dffailBMON
dfp8BMON:	cmp.b #%001000,d2	;an
	beq.b dffailBMON
	bsr.w doeaBMON	;imsizeBMON!!!
	st d0		;dest fpureg
	sf d3
	bra.b dfp6BMON
dfp3BMON:	move.b #"x",(a0)+
	bsr.w opsposBMON
	bsr.b dfp5BMON
dfp6BMON:	moveq #%01111111,d2
	and.w d6,d2
	cmp.b #%00111010,d2
	bne.b dftstBMON
	and.w #$0380,d6		;ftst:dst=0
	bne.w failBMON
	rts
dftstBMON:	tst.b d4
	beq.b dsncsBMON
	moveq #%111,d2
	and.w d6,d2
	bsr.b dfp7BMON
	move.b #":",(a0)+
	move.w d6,d0
	lsr.w #7,d0
	and.w #%111,d0
	bra.b dfp5BMON
dsncsBMON:	move.w d6,d2
	lsr.w #7,d2
	and.w #%111,d2
	tst.b d3
	bne.b dfp7BMON	;show fp0,fp0
	cmp.b d0,d2
	beq.b dfp4BMON
dfp7BMON:	move.w d2,d0
	move.b #",",(a0)+
dfp5BMON:	move.b #"f",(a0)+
	move.b #"p",(a0)+
	moveq #"0",d2
	add.b d0,d2
	move.b d2,(a0)+	
dfp4BMON:	rts			

dfmovemxBMON=*-dissubsBMON
	move.w #$c700,d0
	and.w (a5),d0
	cmp.w #$c000,d0		;fmovem fpx?
	bne.w failBMON
	move.w (a5)+,d3
	btst #13,d3
	bne.b dfmovm1BMON
	bsr doeaBMON
	move.b #",",(a0)+
	bra dofprxBMON
dfmovm1BMON:bsr dofprxBMON
	bra dotoeaBMON
dfmovemlBMON=*-dissubsBMON
	st d5
	bra.b dfmovemBMON

dfmovelBMON=*-dissubsBMON
	sf d5			;no fmovem
dfmovemBMON:move.w #$c3ff,d0
	and.w (a5),d0
	cmp.w #$8000,d0
	bne.w failBMON
	move.b #%010,imsizeBMON	;long
	move.w (a5)+,d6
	eor.w d0,d6
	btst #10,d6
	sne d4
	btst #11,d6
	sne d0
	add.b d0,d4
	btst #12,d6
	sne d0
	add.b d0,d4		;singleflag:0:0,-1:1,-2:2,-3:3
	beq.w failBMON		;0 fpcrBMON
	moveq #%111000,d0
	and.w d1,d0		;Dn?
	bne.b nofdregBMON
	cmp.b #-1,d4		;1 fpcrBMON?
	bne.w failBMON
nofdregBMON:subq.w #%001000,d0	;An?
	bne.b nofaregBMON
	cmp.b #-1,d4		;1 fpcrBMON?
	bne.w failBMON	
	btst #10,d6		;fpcrBMON=fpiarBMON?
	beq.w failBMON
nofaregBMON:bclr #13,d6
	beq.b tofpcrBMON
	bsr.b dofpcrBMON
	bra dotoeaBMON		
tofpcrBMON:	bsr doeaBMON
	move.b #",",(a0)+
dofpcrBMON:	move.b #"f",(a0)+
	move.b #"p",(a0)+
	moveq #"c",d0
	bclr #12,d6
	bne.b fpcr0BMON
	moveq #"s",d0
	bclr #11,d6
	bne.b fpcr0BMON
	move.b #"i",(a0)+
	moveq #"a",d0
	bclr #10,d6
	beq.w failBMON
fpcr0BMON:	move.b d0,(a0)+
	move.b #"r",(a0)+
	tst.w d6
	beq.b fpsinglBMON
	tst.b d5		;fmovem?
	beq.w failBMON
	move.b #"/",(a0)+
	bra.b dofpcrBMON
fpsinglBMON:rts

dfmovecrBMON=*-dissubsBMON
	move.w (a5),d0
	and.w #$fc40,d0
	cmp.w #$5c00,d0	
	bne.w failBMON
	moveq #$003f,d0
	and.w (a5),d0
	move.w d0,d1
	beq.b dfmk1xBMON
	cmp.b #$30,d0
	bcc.w dfmk3xBMON
	cmp.b #$0f,d0
	bhi.w failBMON
	cmp.b #$0b,d0
	bcs.w failBMON
	sub.b #$0b-1,d1
	bra.b dfmk1xBMON
dfmk3xBMON:	sub.b #$30-($0f-$0b)-2,d1
dfmk1xBMON:	move.b #"#",(a0)+
	bsr.w hextoa_BMON
	move.w (a5)+,d0
	bsr.w optofpx79BMON
	move.l	linebufBMON,a0
	lea.l	40(a0),a0
	move.b #";",(a0)+
	lea fmconstBMON(pc),a1
	cmp.b #10,d1	;Pi..100
	bls.b skipcrBMON
	bsr.b dfmk0BMON
	moveq #0,d0
	sub.b #9,d1	;1E4..1E4096
	bset d1,d0
	bra.w hextodBMON
skipcrBMON:	tst.b (a1)+
	bpl.b skipcrBMON
	dbf d1,skipcrBMON
dfmk0BMON:	bra.w txttobufBMON
	
dpccodewordrelBMON=*-dissubsBMON
	moveq #-16,d0		;$fff0 16 mmu ccodesBMON
	and.w (a5),d0
	bne.w failBMON
	move.w (a5)+,d0
	bsr.w dopccBMON
	bra.b dfcc0BMON
dfccodewordrelBMON=*-dissubsBMON
	moveq #-32,d0		;$ffe0 32 fpu ccodesBMON
	and.w (a5),d0
	bne.w failBMON
	move.w (a5)+,d0
	bsr.w dofccBMON
dfcc0BMON:	bsr.w opsposBMON
	bsr.w opdx02BMON
	move.b #",",(a0)+
	move.w (a5)+,d0
	lea -2(a5,d0.w),a1	;instrlen:4bytesBMON (oma bug)
	bra.b wbx2BMON
ccodewordrelBMON=*-dissubsBMON
	move.w d1,d0
	bsr.w doccBMON
wbx4BMON:	bsr.w opsposBMON
	bsr.w opdx02BMON
	move.b #",",(a0)+
	bra.b wbx3BMON
wordccBMON=*-dissubsBMON
	move.w d1,d0
	bsr.w doccBMON
wordcc0BMON:bsr.w chr2bufBMON
	dc.w ".w"		;bxx.w
	bsr.w opsposBMON
wordbxxBMON=*-dissubsBMON
wbx3BMON:	move.w (a5)+,d0
	lea -2(a5,d0.w),a1
wbx2BMON:	move.l a1,d0
	bra.w hextoa_BMON			;bra.w longtoa_BMON
longccBMON=*-dissubsBMON
	move.w d1,d0
	bsr.w doccBMON
longcc0BMON:bsr.w chr2bufBMON
	dc.w ".l"		;bxx.l
	bsr.w opsposBMON
longbxxBMON=*-dissubsBMON
	move.l (a5)+,d0
	lea -4(a5,d0.l),a1
	bra.b wbx2BMON
	
shortccBMON=*-dissubsBMON
	move.w d1,d0
	bsr.w doccBMON
	bsr.w chr2bufBMON
	dc.w ".b"
	bsr.w opsposBMON
shortbxxBMON=*-dissubsBMON
	move.b d1,d0
	ext.w d0
	lea (a5,d0.w),a1
	bra.b wbx2BMON

dbrapccBMON=*-dissubsBMON
	move.w d1,d0
	bsr.w dopccBMON
	bra.b dbra0BMON
dbrafccBMON=*-dissubsBMON
	move.w d1,d0
	bsr.w dofccBMON
dbra0BMON:	btst #6,d1
	beq.b wordcc0BMON
	bra.b longcc0BMON
		
eatoaxBMON=*-dissubsBMON
	bsr.w doeaBMON
	bra.w optoax9bBMON

ea2todxBMON=*-dissubsBMON
	move.b #2,imsizeBMON	;68020:chk.l #i.l,<ea>
	bra.b ea2dxBMON
eatodxBMON=*-dissubsBMON
	move.b #1,imsizeBMON
ea2dxBMON:
size67eatodxBMON=*-dissubsBMON
	bsr.w doeaBMON
	bra.w optodx9bBMON

dxtoeaBMON=*-dissubsBMON		;no im possible
	bsr.w opdx9bBMON
	move.w #$f1c0,d0
	and.w d1,d0
	cmp.w #$0100,d0		;btst: pcrel allowed
	beq.b dxtea0BMON
	bra.w dotoeaBMON

dxtea0BMON:	move.b #",",(a0)+
	bra.w doeaBMON

iwordtoccrBMON=*-dissubsBMON
	move.b #"#",(a0)+		;8/16?
	moveq #0,d0
	move.b (a5)+,d0
	bne.w failBMON
	move.b (a5)+,d0
	bsr.w hextoa_BMON			;bsr.w bytetoa_BMON
	bra.w optoccrBMON

iwordtosrBMON=*-dissubsBMON
	bsr.b iwrdBMON
	bra.w optosrBMON

toiwordBMON:move.b #",",(a0)+
iwordBMON=*-dissubsBMON
iwrdBMON:	move.b #"#",(a0)+
	moveq #0,d0
	move.w (a5)+,d0
	bra.w hextoa_BMON			;bra.w wordtoa_BMON

dlpstopBMON=*-dissubsBMON
	cmp.w #$01c0,(a5)+
	beq.b iwrdBMON
	bra.w failBMON  	
dtblBMON=*-dissubsBMON
	move.w (a5),d0
	and.w #$8238,d0
	bne.w failBMON
	move.w (a5),d0
	moveq #"s",d3
	btst #11,d0
	bne.b dtblsBMON
	moveq #"u",d3
dtblsBMON:	move.b d3,(a0)+
	btst #10,d0
	beq.b dtblrBMON
	move.b #"n",(a0)+
dtblrBMON:	bsr.w siz67d0BMON		;setsBMON imsizeBMON,del:d0
	move.w (a5)+,d3
	btst #8,d3		;datareg|eamode
	beq.b dtbldatBMON
	moveq #%111111,d0	
	and.w d3,d0
	bne.w failBMON	
	bsr.w doeaBMON
	bra.b dtblregBMON	
dtbldatBMON:bsr.w opdx02BMON
	move.b #":",(a0)+
	move.b #"d",(a0)+
	move.w d3,d0
	bsr.w op0xx02BMON	
dtblregBMON:move.w d3,d0
	rol.w #4,d0
	move.b #",",(a0)+
	move.b #"d",(a0)+
	bra.w op0xx02BMON

inibtoeaBMON=*-dissubsBMON
	move.b #"#",(a0)+
	moveq	#0,d3
	move.w (a5)+,d3
	move.w	#$ff00,d0
	and.w	d3,d0
	beq	.ok
	move.b	#"?",(a0)+
	move.b	#"?",(a0)+
	move.l	d3,d0
	bsr	bytetoa_BMON
	bra	.1
.ok	move.l	d3,d0
;	and.l #%11111,d0		;#0-31(7)  ;line removed
	bsr.w hextodBMON
.1	move.w #$ffc0,d0
	and.w d1,d0
	cmp.w #$0800,d0		;btst: pcrel allowed
	beq.b inibea0BMON
	bra.w dotoeaBMON

inibea0BMON:move.b #",",(a0)+
	bra.w doeaBMON
dpccodeeaBMON=*-dissubsBMON
	moveq #-16,d0
	and.w (a5),d0
	bne.w failBMON
	move.w (a5)+,d0
	bsr.w dopccBMON
	bra.b ccode0BMON
fccodeeaBMON=*-dissubsBMON
	moveq #-32,d0
	and.w (a5),d0
	bne.w failBMON
	move.w (a5)+,d0
	bsr.w dofccBMON
	bra.b ccode0BMON
ccodeeaBMON=*-dissubsBMON
	move.w d1,d0
	bsr.w doccBMON
ccode0BMON:	bsr.w opsposBMON
srceaBMON=*-dissubsBMON
	bra.w doeaBMON
imovesBMON=*-dissubsBMON
	move.w (a5)+,d3
	move.w #$07ff,d0
	and.w d3,d0
	bne.w failBMON
	bclr #11,d3
	bne.b mvsrxBMON
	bsr.w doeaBMON
	move.b #",",(a0)+
	bra.w mvcrxcfBMON
mvsrxBMON:	bsr.w mvcrxcfBMON
	bra.w dotoeaBMON		

size67ixtoeaBMON=*-dissubsBMON
	bsr.w immediateBMON
	bra.w dotoeaBMON

size67i9btoeaBMON=*-dissubsBMON
	bsr.w opi9bBMON
	bra.w dotoeaBMON

size67x9btodx02BMON=*-dissubsBMON
	btst #5,d1
	beq.b shi9bBMON
	bsr.w opdx9bBMON
	bra.b shdx9bBMON
shi9bBMON:	bsr.w opi9bBMON
shdx9bBMON:	bra.w optodx02BMON

itrapBMON=*-dissubsBMON
	move.b #"#",(a0)+
	move.w d1,d0
	bra.w nibtodBMON

srcdx02BMON=*-dissubsBMON
	bra.w opdx02BMON

imuspBMON=*-dissubsBMON
	btst #3,d1
	beq.b imusp0BMON
	bsr.w opuspBMON
	bra.w optoax02BMON
imusp0BMON:	bsr.w opax02BMON
	bra.w optouspBMON

dpmovefdBMON=*-dissubsBMON			;68030mmu only
	move.w (a5),d0
	cmp.w #$4100,d0			;40:tc
	beq.w dpmvfdBMON
	eor.w #%0000100100000000,d0	;08:tt0,0c:tt1,48:srp,4c:crp
	and.w #%1011101111111111,d0
	bne.w failBMON
	bra.b dpmvfdBMON	;bit9=0
dpmoveBMON=*-dissubsBMON
	move.w #$01ff,d0
	and.w (a5),d0
	bne.w failBMON
dpmvfdBMON:	move.w (a5)+,d3
	bclr #8,d3	;pmovefd bit
	bclr #9,d3
	bne.b pmvtoeaBMON
	bsr.w doeaBMON
	move.b #",",(a0)+
	bra.b pmvregBMON
pmvtoeaBMON:bsr.b pmvregBMON
	bra.w dotoeaBMON
pmvregBMON:	lea pregsBMON(pc),a1
	lsr.w #8,d3
pmvlpBMON:	move.b (a1)+,d0
	beq.w failBMON
	cmp.b d3,d0
	beq.b pmvfndBMON
pmvscanBMON:btst #7,(a1)+
	beq.b pmvscanBMON
	bra.b pmvlpBMON
pmvfndBMON:	bra.w txttobufBMON
didbcacBMON=*-dissubsBMON	;68040
didb0BMON:	lsr.w #6,d1
	moveq #3,d0
	and.w d1,d0
	move.b idctxBMON(pc,d0.w),(a0)+
	move.b #"c",(a0)+
	rts	
idctxBMON:	dc.l "ndib"	;nc,dc,ic,bc
didbindBMON=*-dissubsBMON
	move.w d1,-(a7)
	bsr.b didb0BMON
	move.w (a7)+,d1
	bra.w toaindirectBMON
dpflush40BMON=*-dissubsBMON		;pflush,ptestr,ptestw
	bra.w aindirectBMON

imovecBMON=*-dissubsBMON
	move.w (a5)+,d3
	btst #0,d1
	bne.b toctrlBMON
	bsr.b mvcregsBMON
	move.b #",",(a0)+
	bra.b mvcrxcfBMON
toctrlBMON:	bsr.b mvcrxcfBMON
	move.b #",",(a0)+
mvcregsBMON:lea cregsBMON(pc),a1
	move.w d3,d1
	and.w #$0fff,d1
nxtctrlBMON:cmp.w (a1),d1
	beq.b mvcctrlBMON
	tst.w (a1)
	addq.w #6,a1
	bne.b nxtctrlBMON	
	bra.w failBMON
mvcctrlBMON:addq.w #2,a1
	move.b (a1)+,(a0)+
	move.b (a1)+,(a0)+
	tst.b (a1)
	beq.b ctrl3chBMON
	move.b (a1)+,(a0)+
	tst.b (a1)
	beq.b ctrl3chBMON
	move.b (a1),(a0)+
ctrl3chBMON:rts
mvcrxcfBMON:move.w d3,d0		;movec,movesBMON
dbfiregBMON:and.w #$f000,d0		;bitfield
	rol.w #4,d0
dbytregBMON:add.b #"0",d0
	move.b #"a",(a0)+
	bclr #3,d0
	bne.b mvcaxBMON 
	addq.b #"d"-"a",-1(a0)
mvcaxBMON:	move.b d0,(a0)+
	rts
dbfieldtodxBMON=*-dissubsBMON
	move.w (a5)+,d2
	btst #15,d2
	bne.w failBMON
	bsr.b dbfi3BMON
	move.b #",",(a0)+
	move.w d2,d0
	bra.b dbfiregBMON
ddxtobfieldBMON=*-dissubsBMON
	move.w (a5),d0
	btst #15,d0
	bne.w failBMON
	bsr.b dbfiregBMON
	move.b #",",(a0)+
	move.w (a5)+,d2
	bra.b dbfi3BMON
dbfieldBMON=*-dissubsBMON
	move.w (a5)+,d2
	move.w d2,d0
	and.w #$f000,d0
	bne.w failBMON
dbfi3BMON:	bsr.w doeaBMON
	move.b #"{",(a0)+
	move.w d2,d0
	lsr.w #6,d0
	bsr.b dbfi2aBMON
	move.b #":",(a0)+
	move.w d2,d0
	bsr.b dbfi2BMON
	move.b #"}",(a0)+
	rts

dbfi2aBMON:	btst #11-6,d0		;bit field offset
	bne.b dbfi1BMON
	and.b #%11111,d0
	bra.w bytetodBMON	

dbfi2BMON:	btst #11-6,d0
	bne.b dbfi1BMON
	and.b #%11111,d0
	bne.b no32BMON
	moveq #32,d0
no32BMON:	bra.w bytetodBMON	
dbfi1BMON:	btst #10-6,d0		;y00xxxy00xxx
	bne.w failBMON
	btst #9-6,d0
	bne.w failBMON
	move.b #"d",(a0)+
	and.b #%111,d0
	add.b #"0",d0
	move.b d0,(a0)+
	rts
dbkptBMON=*-dissubsBMON
	move.b #"#",(a0)+
	bra.w opxx02BMON
dpackBMON=*-dissubsBMON
	btst #3,d1
	beq.b dpadxBMON
	bsr.w chr2bufBMON
	dc.w "-("
	bsr.w opax02BMON
	bsr.w chr4bufBMON
	dc.l "),-("
	bsr.w opax9bBMON
	move.b #")",(a0)+
	bra.b dpad0BMON
dpadxBMON:	bsr.w opdx02BMON
	bsr.w optodx9bBMON
dpad0BMON:	bra.w toiwordBMON
dmov16BMON=*-dissubsBMON
	move.w (a5),d2
	and.w #$8fff,d2	
	cmp.w #$8000,d2
	bne.w failBMON
	bsr.w aindirectpostBMON
	move.w (a5)+,d1
	rol.w #4,d1
	bra.w toaindirectpostBMON
dmov162BMON=*-dissubsBMON
	moveq #%010000,d2	;(ax)
	bclr #4,d1
	bne.b dmov1BMON
	moveq #%011000,d2	;(ax)+
dmov1BMON:	bclr #3,d1
	beq.b dmov0BMON
	bsr.b dmov2BMON
	or.w d2,d1
	bra.w dotoeaBMON
dmov0BMON:	or.w d2,d1
	bsr.w doeaBMON
	move.b #",",(a0)+
dmov2BMON:	bra.w abslongBMON
	
imoveBMON=*-dissubsBMON
	move.w d1,d0
	and.w #%0011000000000000,d0
	rol.w #4,d0
	bne.b imo0BMON
	sf d7
	rts
imo0BMON:	lea sizemovBMON(pc),a1
	move.b (a1,d0.w),(a0)+
	move.b 4(a1,d0.w),imsizeBMON

	bsr.w opsposBMON
	bsr.w doeaBMON

	move.w d1,d0
	rol.w #7,d0
	and.w #%111,d0
	move.w d1,d3
	lsr.w #3,d3
	and.w #%111000,d3
	or.w d3,d0

	move.w d0,d1
	bra.w dotoeaBMON

imoveqBMON=*-dissubsBMON
	move.b #"#",(a0)+
	moveq #0,d0
	move.b d1,d0
	bpl	.pos
	move.b	#"-",(a0)+
	neg.b	d0
.pos
	bsr.w hextoa_BMON			;bsr.w bytetoa_BMON
	bra.w optodx9bBMON	

icallmBMON=*-dissubsBMON
	tst.b (a5)
	bne.w failBMON
	move.w d1,-(a7)
	move.b #"#",(a0)+
	moveq #0,d0
	move.b (a5)+,d0
	bne.w failBMON
	move.b (a5)+,d0
	bsr.w hextoa_BMON			;bsr.w bytetoa_BMON
	move.w (a7)+,d1
	bra.w dotoeaBMON

dcasBMON=*-dissubsBMON
	bsr.w opsposBMON
	move.w (a5),d0
	and.w #$0e38,d0
	bne.w failBMON
	move.w (a5),d0
	move.b #"d",(a0)+
	bsr.w op0xx02BMON
	move.b #",",(a0)+
	move.w (a5)+,d0
	lsr.w #6,d0
	move.b #"d",(a0)+
	bsr.w op0xx02BMON
	bra.w dotoeaBMON

dcas2BMON=*-dissubsBMON
	moveq #"w",d2
	btst #9,d1
	beq.b dca0BMON
	moveq #"l",d2
dca0BMON:	move.b d2,(a0)+
	bsr.w opsposBMON
	move.l (a5),d2
	and.l #$0e380e38,d2
	bne.w failBMON
	move.l (a5),d2
	bsr.b casopBMON
	move.b #",",(a0)+
	lsr.l #6,d2
	bsr.b casopBMON
	bsr.w chr2bufBMON
	dc.w ",("
	move.w (a5)+,d0
	bsr.w dbfiregBMON
	bsr.w chr3bufBMON
	dc.l "):("<<8
	move.w (a5)+,d0
	bsr.w dbfiregBMON
	move.b #")",(a0)+
	rts	
casopBMON:	move.l d2,d0
	swap d0
	move.b #"d",(a0)+
	bsr.w op0xx02BMON
	move.w d2,d0
	bsr.w chr2bufBMON
	dc.w ":d"
	bra.w op0xx02BMON

dchk2BMON=*-dissubsBMON
	move.w (a5)+,d2
	btst #11,d2
	beq.w failBMON
	bra.b dchkcmpBMON
dcmp2BMON=*-dissubsBMON
	move.w (a5)+,d2
	btst #11,d2
	bne.w failBMON
dchkcmpBMON:bsr.w doeaBMON
	move.b #",",(a0)+
	move.w d2,d0
	bra.w dbfiregBMON

drtmBMON=*-dissubsBMON
	moveq #$0f,d0
	and.w d1,d0
	bra.w dbytregBMON

dtpccsizeBMON=*-dissubsBMON
	move.w d1,d0
	bsr.w doccBMON
dtpc2BMON:	move.b #".",(a0)+
	moveq #"w",d0
	btst #0,d1
	beq.b dtpc1BMON
	moveq #"l",d0
dtpc1BMON:	move.b d0,(a0)+
	bsr.w opsposBMON
	move.b #"#",(a0)+
	moveq #0,d0
	btst d0,d1
	bne.b dtpc0BMON
	move.w (a5)+,d0
	bra.w hextoa_BMON			;bra.w wordtoa_BMON
dtpc0BMON:	move.l (a5)+,d0
	bra.w hextoa_BMON			;bra.w longtoa_BMON
dtpccBMON=*-dissubsBMON
	move.w d1,d0
	bra.w doccBMON
dtpfccsizeBMON=*-dissubsBMON
	bsr.b dtpfcc0BMON
	bra.b dtpc2BMON
dtpfccBMON=*-dissubsBMON
dtpfcc0BMON:moveq #-32,d0		;$ffe0	;32 fpu ccodesBMON
	and.w (a5),d0
	bne.w failBMON
	move.w (a5)+,d0
	bra.w dofccBMON
dtppccsizeBMON=*-dissubsBMON
	bsr.b dtppcc0BMON
	bra.b dtpc2BMON
dtppccBMON=*-dissubsBMON
dtppcc0BMON:moveq #-16,d0		;$fff0 ;16 mmu ccodesBMON
	and.w (a5),d0
	bne.w failBMON
	move.w (a5)+,d0
	bra.w dopccBMON

dfnopBMON=*-dissubsBMON
	tst.w (a5)+		;4byte command
	seq d7
	rts
dpvalidBMON=*-dissubsBMON
	moveq #-8,d0
	and.w (a5),d0
	cmp.w #$2c00,d0
	bne.w failBMON
	move.w (a5)+,d0
	move.b #"a",(a0)+
	bsr.w op0xx02BMON
	bra.w dotoeaBMON

dpflushaBMON=*-dissubsBMON
	cmp.w #$2400,(a5)+
	seq d7
	rts
dpflushrBMON=*-dissubsBMON
	cmp.w #$a000,(a5)+
	bne.w failBMON
	bra.w doeaBMON
dpflushsBMON=*-dissubsBMON
	cmp.b #$34,(a5)
	beq.b dpfl4BMON
dpfl5BMON:	bra.w failBMON
dpflushBMON=*-dissubsBMON
	cmp.b #$30,(a5)
	bne.b dpfl5BMON
dpfl4BMON:	move.w (a5)+,d0
	moveq #%00011000,d1
	and.w d0,d1
	bne.b dpfl1BMON
	moveq #%00000111,d1
	moveq #"s",d2
	and.w d0,d1
	beq.b dpfl2BMON
	moveq #"d",d2
	subq.b #1,d1
	bne.w failBMON
dpfl2BMON:	move.b d2,(a0)+
	bsr.w chr2bufBMON
	dc.w "fc"
	bra.b dpfl3BMON
dpfl1BMON:	cmp.w #%00011000,d1	;illegal
	beq.w failBMON
	moveq #"#",d1
	bclr #3,d0	
	beq.b dpfl0BMON
	moveq #"d",d1
dpfl0BMON:	move.b d1,(a0)+
	bsr.w nibtoaBMON
dpfl3BMON:	bsr.w chr2bufBMON
	dc.w ",#"
	lsr.w #5,d0
	bra.w op0xx02BMON
dploadBMON=*-dissubsBMON
	move.w (a5),d0
	and.w #$fdf8,d0
	cmp.w #$2010,d0
	bne.w failBMON
dpte0BMON:	move.w (a5)+,d0
	move.b #"r",(a0)
	btst #9,d0
	bne.b dplo0BMON
	addq.b #"w"-"r",(a0)
dplo0BMON:	bsr.w opsposBMON
	move.b #"#",(a0)+
	bsr.w nibtoaBMON
	bra.w dotoeaBMON
dptestBMON=*-dissubsBMON
	move.w (a5),d0
	and.w #$e118,d0
	cmp.w #$8110,d0
	bne.w failBMON
	move.w (a5),d2
	bsr.b dpte0BMON
	bsr.w chr2bufBMON
	dc.w ",#"
	move.w d2,d0
	rol.w #6,d0
	moveq #%111,d2
	and.b d0,d2
	beq.w failBMON	;level 0
	bsr.w nibtoaBMON
	bsr.w chr2bufBMON
	dc.w ",a"
	rol.w #5,d0
	bclr #3,d0
	bra.w nibtoaBMON

ddivsllBMON=*-dissubsBMON
	move.w (a5)+,d2
	bchg #10,d2		;dn:dm mode,set g=1
	bne.w failBMON	
	bsr.b dmul5BMON
	bra.b ddivlBMON
ddivullBMON=*-dissubsBMON
	move.w (a5)+,d2
	bchg #10,d2		;dn:dm mode,set g=1
	bne.w failBMON	
	bsr.b dmul4BMON
ddivlBMON:	sub.b d2,d1
	and.b #%111,d1		;dm=dn
	beq.w failBMON
	rts
dmulslBMON=*-dissubsBMON
dmul3BMON:	move.w (a5)+,d2
dmul5BMON:	btst #11,d2
	beq.w failBMON
	bra.b dmul1BMON
dmululBMON=*-dissubsBMON
dmul2BMON:	move.w (a5)+,d2
dmul4BMON:	btst #11,d2
	bne.w failBMON
dmul1BMON:	move.w #$83f8,d0
	and.w d2,d0
	bne.w failBMON
	move.b #%10,imsizeBMON	;longwordsBMON
	bsr.w doeaBMON
	move.b #",",(a0)+
	move.w d2,d1
	btst #10,d2
	beq.b dmul0BMON
	bsr.w opdx02BMON
	move.b #":",(a0)+
	rol.w #4,d1
	bra.w opdx02BMON

dmul0BMON:	rol.w #4,d1
	bsr.w opdx02BMON
	sub.b d2,d1
	and.b #%111,d1		;mulxl ea,dn:dm=dn	
	bne.w failBMON
	rts

icmpmBMON=*-dissubsBMON
	bsr.w aindirectpostBMON
	rol.w #7,d1		;(ax)+
	bra.w toaindirectpostBMON

xbcdBMON=*-dissubsBMON
	bclr #14,d1
	bclr #3,d1		;dx
	beq.b xbcdxBMON	
	bset #5,d1		;-(ax)
	bset #14,d1
xbcdxBMON:	bsr.w doeaBMON		;no im possible
	rol.w #7,d1
	bra.w dotoeaBMON

size67adsbxBMON=*-dissubsBMON
	bclr #14,d1
	bclr #12,d1
	bclr #3,d1		;dx
	beq.b adsbxdxBMON
	bset #14,d1		;-(ax)
	bset #5,d1
adsbxdxBMON:bsr.w doeaBMON
	rol.w #7,d1
	bra.w dotoeaBMON	

size6movepBMON=*-dissubsBMON
	bset #5,d1		;d(ax)
	btst #7,d1
	beq.b mptodxBMON
	bsr.w opdx9bBMON
	bra.w dotoeaBMON
mptodxBMON:	bsr.w doeaBMON
	bra.w optodx9bBMON

imovemBMON=*-dissubsBMON
	move.w (a5)+,d3		;regbitsBMON
	btst #10,d1		;dir
	bne.b torxBMON
	bsr.w dorxBMON
	bra.w dotoeaBMON
torxBMON:	bsr.w doeaBMON
	move.b #",",(a0)+
	bra.w dorxBMON

dx9btodx02BMON=*-dissubsBMON
	bsr.w opdx9bBMON
	bra.w optodx02BMON
ax9btoax02BMON=*-dissubsBMON
	bsr.w opax9bBMON
	bra.w optoax02BMON
dx9btoax02BMON=*-dissubsBMON
	bsr.w opdx9bBMON
	bra.w optoax02BMON

dlinklgBMON=*-dissubsBMON
	bsr.b iunl0BMON
	bsr.w chr2bufBMON
	dc.w ",#"
	bra.w dtpc0BMON
ilinkBMON=*-dissubsBMON
	bsr.b iunl0BMON
	bra.w toiwordBMON
iunlkBMON=*-dissubsBMON
iunl0BMON:	bra.w opax02BMON

size8eatoax9bBMON=*-dissubsBMON
	bsr.w doeaBMON
	bra.w optoax9bBMON

size8BMON:	btst #8,d1
	beq.b siz60BMON
	bra.b siz61BMON
size6BMON:	btst #6,d1
	bne.b siz61BMON
siz60BMON:	moveq #"w",d0
	move.b #1,imsizeBMON
	bra.b siz62BMON
siz61BMON:	moveq #"l",d0
	move.b #2,imsizeBMON
siz62BMON:	move.b #".",(a0)+
	move.b d0,(a0)+
	bra.b opsposBMON
size9a_BMON:move.w d1,d0			;01.b,10.w,11.l,00.*
	lsr.w #3,d0
	sub.w #%01000000,d0	
	bra.b si9aBMON
size9aBMON:	move.w d1,d0			;00.b,01.w,10.l,11.*
	lsr.w #3,d0
	bra.b si9aBMON
size67BMON:	move.w d1,d0
siz67d0BMON:
si9aBMON:	and.w #%11000000,d0
	cmp.w #%11000000,d0
	beq.w failBMON
	lsr.w #6,d0
	move.b d0,imsizeBMON	;00.b,01.w,10.l,11.*
	move.b #".",(a0)+
	move.b sizetxtBMON(pc,d0.w),(a0)+
opsposBMON:
	move.b	#' ',(a0)+
	btst	#1,modeBMON
	beq.b	.notab
	move.l	linebufBMON,a0
	lea.l	23(a0),a0			;ftrapngle.l
.notab	rts
sizetxtBMON:dc.l "bwl*"

opi9bBMON:	move.b #"#",(a0)+
	move.w d1,d0
	and.w #%111000000000,d0
	rol.w #7,d0
	bne.b i9b0BMON
	moveq #8,d0
i9b0BMON:	bra.w nibtodBMON
optoax9bBMON:move.b #",",(a0)+
opax9bBMON:	move.b #"a",(a0)+
	bra.b opxxBMON
optodx9bBMON:move.b #",",(a0)+
opdx9bBMON:	move.b #"d",(a0)+
opxxBMON:	move.w d1,d0
	and.w #%111000000000,d0
	rol.w #7,d0
	add.b #"0",d0
	move.b d0,(a0)+
	rts
optoax02BMON:move.b #",",(a0)+
opax02BMON:	move.b #"a",(a0)+
	bra.b opxx02BMON
optodx02BMON:move.b #",",(a0)+
opdx02BMON:	move.b #"d",(a0)+
opxx02BMON:	move.w d1,d0
op0xx02BMON:and.w #%111,d0
	add.b #"0",d0
	move.b d0,(a0)+
	rts
optofpx79BMON:move.b #",",(a0)+
opfpx79BMON:bsr.b chr2bufBMON
	dc.w "fp"
	lsr.w #7,d0
	bra.b op0xx02BMON
optouspBMON:move.b #",",(a0)+
opuspBMON:	lea usptxtBMON(pc),a1
	bra.b txttobufBMON
optoccrBMON:move.b #",",(a0)+
opccrBMON:	lea ccrtxtBMON(pc),a1
	bra.b txttobufBMON
optosrBMON:	move.b #",",(a0)+
opsrBMON:	lea srtxtBMON(pc),a1
txttobufBMON:
	move.b (a1)+,(a0)
	bclr #7,(a0)+
	beq.b txttobufBMON
txtqBMON:	rts	
chr2bufBMON:move.l a1,-(a7)		;copiesBMON 2 chrsBMON below bsr chr2bufBMON
	move.l 4(a7),a1
	move.b (a1)+,(a0)+
	move.b (a1)+,(a0)+
	move.l (a7)+,a1
	addq.l #2,(a7)
	rts
chr3bufBMON:			;copiesBMON 3 chrsBMON below bsr chr3bufBMON
chr4bufBMON:move.l a1,-(a7)		;copiesBMON 4 chrsBMON below bsr chr4bufBMON
	move.l 4(a7),a1
	move.b (a1)+,(a0)+
	move.b (a1)+,(a0)+
	move.b (a1)+,(a0)+
	tst.b (a1)
	beq.b chr0BMON
	move.b (a1)+,(a0)+
chr0BMON:	move.l (a7)+,a1
	addq.l #4,(a7)
	rts

bcpltobufBMON:move.b (a1)+,d0
bcpl0BMON:	subq.b #1,d0
	bmi.b bcpl1BMON
	move.b (a1)+,(a0)+
	bra.b bcpl0BMON		
bcpl1BMON:	rts

dorxBMON:	move.w d1,d0
	and.w #%111000,d0
	cmp.w #%100000,d0	;-(ax)
	bne.b drx8BMON
	moveq #16-1,d2
drx9BMON:	lsr.w #1,d3
	roxl.w #1,d0
	dbf d2,drx9BMON	
	move.w d0,d3
drx8BMON:	sf d5
	clr.w d2
	moveq #"d",d0
	bsr.b drx0BMON
	moveq #"a",d0
drx0BMON:	moveq #8,d4
drx1BMON:	tst.w d4
	beq.b drx5BMON
	btst d2,d3
	bne.b drx2BMON
	addq.w #1,d2
	subq.w #1,d4
	bra.b drx1BMON
drx2BMON:	tas d5
	beq.b drx3BMON
	move.b #"/",(a0)+
drx3BMON:	move.b d0,(a0)+
	move.b d2,(a0)
	add.b #"0",(a0)
	bclr #3,(a0)+
	sf d6
drx4BMON:	addq.w #1,d2
	subq.w #1,d4
	beq.b drx6BMON
	btst d2,d3
	beq.b drx6BMON
	st d6
	bra.b drx4BMON
drx6BMON:	tst.b d6
	beq.b drx7BMON
	move.b #"-",(a0)+
	move.b d0,(a0)+
	move.b d2,(a0)
	add.b #"0"-1,(a0)
	bclr #3,(a0)+
drx7BMON:	tst.w d4
	beq.b drx5BMON
	subq.w #1,d4
	addq.w #1,d2
	bra.b drx1BMON
drx5BMON:	rts

dofprxBMON:	move.w d1,d0
	and.w #%111000,d0
	beq.w failBMON		;Dn
	cmp.w #%001000,d0	;An
	beq.w failBMON
	moveq #%01110000,d6
	and.l d3,d6		;posBMON
	cmp.b d3,d6
	beq.b dfprxaBMON
	moveq #-1,d6		;neg:bad dynamic mask
dfprxaBMON:	lsr.b #4,d6
	add.b #"0",d6		;dynamic mask
	cmp.w #%100000,d0	;-(ax)
	bne.b dfprx8BMON
	btst #13,d3	;dr field
	beq.w failBMON	;to FPx?
	btst #12,d3	;mode
	bne.w failBMON	;no predec mode?
	moveq #8-1,d2
dfprx9BMON:	lsr.b #1,d3
	roxl.b #1,d0
	dbf d2,dfprx9BMON	
	move.b d0,d3
	bra.b dfprx0BMON
dfprx8BMON:	btst #12,d3	;mode
	beq.w failBMON	;predec mode?
dfprx0BMON:	btst #11,d3
	beq.b dfpstatBMON
	tst.l d6
	bmi.w failBMON	;bad dynamic mask
	move.b #"d",(a0)+
	move.b d6,(a0)+
dfprx5BMON:	rts
dfpstatBMON:sf d0
	moveq #8-1,d2
dfprx1BMON:	tst.w d2
	bmi.b dfprx5BMON
	btst d2,d3
	bne.b dfprx2BMON
	subq.w #1,d2
	bra.b dfprx1BMON
dfprx2BMON:	tas d0
	beq.b dfprx3BMON
	move.b #"/",(a0)+
dfprx3BMON:	move.b #"f",(a0)+
	move.b #"p",(a0)+
	move.b #"7",(a0)
	sub.b d2,(a0)+
	sf d6
dfprx4BMON:	subq.w #1,d2
	bmi.b dfprx6BMON
	btst d2,d3
	beq.b dfprx6BMON
	st d6
	bra.b dfprx4BMON
dfprx6BMON:	tst.b d6
	beq.b dfprx7BMON
	move.b #"-",(a0)+
	move.b #"f",(a0)+
	move.b #"p",(a0)+
	move.b #"7"-1,(a0)
	sub.b d2,(a0)+
dfprx7BMON:	tst.w d2
	bmi.b dfprx5BMON
	subq.w #1,d2
	bra.b dfprx1BMON

doccBMON:	lea ccodetxtBMON(pc),a1
	and.w #%111100000000,d0
	lsr.w #7,d0
	lea (a1,d0.w),a1
	move.b (a1)+,(a0)+
	tst.b (a1)
	beq.b docc0BMON
	move.b (a1)+,(a0)+
docc0BMON:	rts
dopccBMON:	lea pccodetxtBMON(pc),a1
	and.w #%001111,d0
	add.w d0,d0
	lea (a1,d0.w),a1
	move.b (a1)+,(a0)+
	move.b (a1)+,(a0)+
	rts
dofccBMON:	lea fccodetxtBMON(pc),a1
	and.w #%011111,d0
	add.w d0,d0
	add.w d0,d0
	lea (a1,d0.w),a1
	move.b (a1)+,(a0)+	;1..4 charsBMON
	tst.b (a1)
	beq.b dofcc0BMON
	move.b (a1)+,(a0)+
	tst.b (a1)
	beq.b dofcc0BMON
	move.b (a1)+,(a0)+
	tst.b (a1)
	beq.b dofcc0BMON
	move.b (a1)+,(a0)+
dofcc0BMON:	rts



dotoeaBMON:	move.w d1,d0
	and.w #%111111,d0
	cmp.w #%111001,d0	;dest:(pc),d(pc),#xx invalid
	bls.b doto0BMON
	sf d7 
	rts
doto0BMON:	move.b #",",(a0)+

doeaBMON:	moveq #%111000,d0
	and.w d1,d0
	lsr.w #2,d0
	pea eatabBMON(pc)
	move.w eatabBMON(pc,d0.w),d0	;ext.l obsolete
	add.l d0,(a7)
	rts

eatabBMON:	dc.w ddirectBMON-eatabBMON
	dc.w adirectBMON-eatabBMON
	dc.w aindirectBMON-eatabBMON
	dc.w aindirectpostBMON-eatabBMON
	dc.w aindirectpreBMON-eatabBMON
	dc.w aindirectdispBMON-eatabBMON
	dc.w aindirectindexBMON-eatabBMON
	dc.w mode7BMON-eatabBMON
ddirectBMON:move.b #"d",(a0)+
	moveq #%000111,d0
	and.w d1,d0
	add.b #"0",d0
	move.b d0,(a0)+
	rts
adirectBMON:move.b #"a",(a0)+
	moveq #%000111,d0
	and.w d1,d0
	add.b #"0",d0
	move.b d0,(a0)+
	rts
toaindirectBMON:
	move.b #",",(a0)+
aindirectBMON:
	move.b #"(",(a0)+
	move.b #"a",(a0)+
	moveq #%000111,d0
	and.w d1,d0
	add.b #"0",d0
	move.b d0,(a0)+
	move.b #")",(a0)+
	bsr.w setbaseBMON
	bra.w eavalBMON
toaindirectpostBMON:
	move.b #",",(a0)+
aindirectpostBMON:
	move.b #"(",(a0)+
	move.b #"a",(a0)+
	moveq #%000111,d0
	and.w d1,d0
	add.b #"0",d0
	move.b d0,(a0)+
	move.b #")",(a0)+
	move.b #"+",(a0)+
	bsr.w setbaseBMON
	bra.w eavalBMON
aindirectpreBMON:
	move.b #"-",(a0)+
	move.b #"(",(a0)+
	moveq #%000111,d0
	and.w d1,d0
	add.b #"0",d0
	move.b #"a",(a0)+
	move.b d0,(a0)+
	move.b #")",(a0)+
	bsr.w setbaseBMON
	bra.w eavalBMON
aindirectdispBMON:
	move.b #"(",(a0)+
	move.w (a5),d0
	bsr.w swordtoa_BMON
	move.b #",",(a0)+
	move.b #"a",(a0)+
	moveq #%000111,d0
	and.w d1,d0
	add.b #"0",d0
	move.b d0,(a0)+
	move.b #")",(a0)+
	bsr.w setbaseBMON
	move.w (a5)+,d0
	ext.l d0
	add.l d0,baseregBMON
	bra.w eavalBMON
aindirectindexBMON:
	btst #0,(a5)
	bne.w indfullBMON
	bsr.w setbaseBMON
	move.b #"(",(a0)+
	move.w (a5),d0
	bsr.w sbytetoa_BMON
	ext.w d0
	ext.l d0
	add.l d0,baseregBMON
	move.b #",",(a0)+
	move.b #"a",(a0)+
	moveq #%000111,d0
	and.w d1,d0
	add.b #"0",d0
	move.b d0,(a0)+
	move.b #",",(a0)+
	moveq #"d",d0
	btst #7,(a5)
	beq.b ind0BMON
	moveq #"a",d0
ind0BMON:	move.b d0,(a0)+
	moveq #%01110000,d0
	and.b (a5),d0
	lsr.b #4,d0
	add.b #"0",d0
	move.b d0,(a0)+
	movem.l d1/a1,-(a7)
	move.w #%11110000,d0
	and.b (a5),d0
	lsr.b #2,d0
	lea regsBMON,a1
	move.l (a1,d0.w),d1
	move.b #".",(a0)+
	moveq #"l",d0
	btst #3,(a5)
	bne.b ind1BMON
	moveq #"w",d0
	ext.l d1
ind1BMON:	move.b d0,(a0)+
	moveq #%00000110,d0
	and.b (a5),d0
	beq.b ind2BMON
	lsr.b #1,d0
	move.b #"*",(a0)+
	move.b scaleBMON(pc,d0.w),(a0)+
ind2BMON:	lsl.l d0,d1
	add.l d1,baseregBMON
	move.b #")",(a0)+
	movem.l (a7)+,d1/a1
	addq.w #2,a5
	bra.w eavalBMON
scaleBMON:	dc.l "1248"
pcindfullBMON:
indfullBMON:moveq #%00001000,d0
	and.w (a5),d0
	bne.w reservedBMON		;failBMON
	moveq #%01000111,d0
	and.w (a5),d0
	cmp.b #%01000100,d0
	bcc.w reservedBMON
	cmp.b #%00000100,d0
	beq.w reservedBMON
	moveq #%00110000,d0
	and.w (a5),d0
	beq.w reservedBMON

	move.w d2,-(a7)
	move.w (a5)+,d2		;full format extension
	move.b #"(",(a0)+
	moveq #%00000111,d0
	and.w d2,d0
	beq.b nomemindBMON
	move.b #"[",(a0)+
nomemindBMON:		

	moveq #%111111,d0
	and.w d1,d0
	cmp.w #%111011,d0	;pc indirect?
	bne.b nopcindBMON

	moveq #%00110000,d0
	and.w d2,d0
	lsr.w #4,d0
	btst #7,d2
	beq.b nozpcBMON
	bsr.w displaceBMON
	cmp.b #"[",-1(a0)
	beq.b noidispBMON
	move.b #",",(a0)+
noidispBMON:move.b #"z",(a0)+
	bra.b dozpcBMON
nozpcBMON:	subq.b #1,d0
	beq.b dozpcBMON
	subq.b #1,d0
	beq.b nozpc1BMON
	move.l a5,d0
	add.l (a5)+,d0
	subq.l #2,d0
	bsr.w hextoa_BMON
;	move.b #".",(a0)+
;	move.b #"l",(a0)+
	bra.b nozpc0BMON
nozpc1BMON:	move.w (a5)+,d0
	ext.l d0
	add.l a5,d0
	subq.l #4,d0
	bsr.w hextoa_BMON
	move.b #".",(a0)+
	move.b #"w",(a0)+
nozpc0BMON:	move.b #",",(a0)+
dozpcBMON:	move.b #"p",(a0)+
	move.b #"c",(a0)+
	bra.b baseokBMON
nopcindBMON:moveq #%00110000,d0	;baseBMON displacement
	and.w d2,d0
	lsr.w #4,d0
	bsr.w displaceBMON
	moveq #%000111,d0
	and.w d1,d0
	btst #7,d2		;bsBMON
	bne.b basesuprBMON
	cmp.b #"[",-1(a0)
	beq.b noidis2BMON
	cmp.b #"(",-1(a0)
	beq.b noidis2BMON
	move.b #",",(a0)+
noidis2BMON:move.b #"a",(a0)+
	add.b #"0",d0
	move.b d0,(a0)+
	bra.b baseokBMON	
basesuprBMON:
	tst.b d0
	bne.w basfailBMON
baseokBMON:	
	btst #2,d2		;postindex?
	beq.b nopostBMON
	move.b #"]",(a0)+
nopostBMON:	btst #6,d2
	bne.b indsuprBMON
	cmp.b #"[",-1(a0)
	beq.b indpreBMON
	cmp.b #"(",-1(a0)
	beq.b indpreBMON
	move.b #",",(a0)+
indpreBMON:	moveq #"d",d0
	tst.w d2
	bpl.b dindxBMON
	moveq #"a",d0
dindxBMON:	move.b d0,(a0)+
	move.w #%0111000000000000,d0
	and.w d2,d0
	rol.w #4,d0
	add.b #"0",d0
	move.b d0,(a0)+
	move.b #".",(a0)+
	moveq #"l",d0
	btst #11,d2
	bne.b dind1BMON
	moveq #"w",d0
dind1BMON:	move.b d0,(a0)+
	move.w #%0000011000000000,d0
	and.w d2,d0
	beq.b dind2BMON
	rol.w #7,d0
	move.b #"*",(a0)+
	move.b scale3BMON(pc,d0.w),(a0)+
dind2BMON:	bra.b indokBMON
scale3BMON:	dc.l "1248"
indsuprBMON:move.w #%1111111000000000,d0
	and.w d2,d0
	bne.b indfailBMON
indokBMON:	moveq #%00000111,d0
	and.w d2,d0
	beq.b nopreBMON
	btst #2,d2
	bne.b nopreBMON 
	move.b #"]",(a0)+
nopreBMON:
	moveq #%00000011,d0	;outer displacement
	and.w d2,d0
	beq.b nomemind2BMON
	cmp.b #1,d0
	beq.b nokomaBMON
	move.b #",",(a0)+
nokomaBMON:	bsr.b displaceBMON
nomemind2BMON:	
	move.b #")",(a0)+
indquitBMON:move.w (a7)+,d2
	rts
indfailBMON:
basfailBMON:sf d7
	bra.b indquitBMON

displaceBMON:
	subq.b #1,d0
	beq.b nullouterBMON
	subq.b #1,d0
	beq.b wordouterBMON
	move.l (a5)+,d0
	bsr.w hextoa_BMON
;	move.b #".",(a0)+
;	move.b #"l",(a0)+
	rts
wordouterBMON:
	move.w (a5)+,d0		;upper word=0
	bsr.w hextoa_BMON
	move.b #".",(a0)+
	move.b #"w",(a0)+
nullouterBMON:
	rts

mode7BMON:	moveq #%000111,d0
	and.w d1,d0
	add.w d0,d0
	pea mod7tabBMON(pc)
	move.w mod7tabBMON(pc,d0.w),d0
	add.l d0,(a7)
	rts
mod7tabBMON:dc.w absshortBMON-mod7tabBMON
	dc.w abslongBMON-mod7tabBMON
	dc.w pcindirectBMON-mod7tabBMON
	dc.w pcindexBMON-mod7tabBMON
	dc.w immediateBMON-mod7tabBMON
	dc.w reservedBMON-mod7tabBMON
	dc.w reservedBMON-mod7tabBMON
	dc.w reservedBMON-mod7tabBMON

absshortBMON:
	move.b #"(",(a0)+
	moveq #0,d0
	move.w (a5)+,d0
	bpl	.pos
	move.b	#"-",(a0)+
	neg.w	d0
.pos	bsr.w hextoa_BMON
	move.b #")",(a0)+
	move.b #".",(a0)+
	move.b #"w",(a0)+
	rts
abslongBMON:move.b #"(",(a0)+
	move.l (a5)+,d0
	bsr.w hextoa_BMON
	move.b #")",(a0)+
	rts
pcindirectBMON:
	move.b #"(",(a0)+
	moveq #-2,d0
	add.w (a5)+,d0
	ext.l d0
	add.l a5,d0
	bsr.w hextoa_BMON
	move.b #",",(a0)+
	move.b #"p",(a0)+
	move.b #"c",(a0)+
	move.b #")",(a0)+
	rts
pcindexBMON:btst #0,(a5)
	bne.w pcindfullBMON
	move.b #"(",(a0)+
	move.w (a5),d0
	ext.w d0
	ext.l d0
	add.l a5,d0
	move.l d0,baseregBMON
	bsr.w hextoa_BMON
	move.b #",",(a0)+
	move.b #"p",(a0)+
	move.b #"c",(a0)+
	move.b #",",(a0)+
	moveq #"d",d0
	btst #7,(a5)
	beq.b pcind0BMON
	moveq #"a",d0
pcind0BMON:	move.b d0,(a0)+
	moveq #%01110000,d0
	and.b (a5),d0
	lsr.b #4,d0
	add.b #"0",d0
	move.b d0,(a0)+
	movem.l d1/a1,-(a7)
	move.w #%11110000,d0
	and.b (a5),d0
	lsr.b #2,d0
	lea regsBMON,a1
	move.l (a1,d0.w),d1
	move.b #".",(a0)+
	moveq #"l",d0
	btst #3,(a5)
	bne.b pcind1BMON
	moveq #"w",d0
	ext.l d1
pcind1BMON:	move.b d0,(a0)+
	moveq #%00000110,d0
	and.b (a5),d0
	beq.b pcind2BMON
	lsr.b #1,d0
	move.b #"*",(a0)+
	move.b scale2BMON(pc,d0.w),(a0)+
pcind2BMON:	lsl.l d0,d1
	add.l d1,baseregBMON
	move.b #")",(a0)+
	movem.l (a7)+,d1/a1
	addq.w #2,a5
	bra.w eavalBMON
scale2BMON:	dc.l "1248"

immediateBMON:
	move.b #"#",(a0)+
	moveq #0,d0
	move.w (a5)+,d0
	tst.b imsizeBMON
	bne.b imwordBMON
	cmp.w #$ff,d0
;	bhi.b reservedBMON		;failBMON
	bls.b	.ok
	move.b	#"?",(a0)+
	move.b	#"?",(a0)+
	and.w	#$ff,d0
.ok
	bra.w hextoa_BMON
imwordBMON:	cmp.b #%001,imsizeBMON
	bne.b imlongBMON
	bra.w hextoa_BMON
imlongBMON:	cmp.b #%010,imsizeBMON
	bne.b fpu4wrdBMON
	swap d0
	move.w (a5)+,d0
	bra.w hextoa_BMON
imresvdBMON:cmp.b #%011,imsizeBMON
	beq.b reservedBMON
fpu4wrdBMON:cmp.b #%100,imsizeBMON	;%1xx fpu imsizeBMON
	bne.b fpu5wrdBMON
fpu4w0BMON:	swap d0
	move.w (a5)+,d0
	bsr.w longtoa_BMON
fpu2w0BMON:	move.l (a5)+,d0
	bra.w longtoaBMON
fpu5wrdBMON:cmp.b #%101,imsizeBMON
	beq.b reservedBMON
fpu6wrdBMON:cmp.b #%110,imsizeBMON
	bne.b reservedBMON
	bsr.b fpu4w0BMON
	bra.b fpu2w0BMON

reservedBMON:
	sf d7
	rts

setbaseBMON:move.l a1,-(a7)
	lea regsBMON,a1
	moveq #%000111,d0
	and.w d1,d0
	lsl.w #2,d0
	move.l 8*4(a1,d0.w),baseregBMON	;contentsBMON An
	move.l (a7)+,a1
	rts

eavalBMON:
	btst	#2,modeBMON
	beq.b	eaval1BMON
	btst #2,dissignBMON
	beq.b eaval1BMON
	move.l a0,-(a7)
	move.l baseregBMON,d0
	move.l	linebufBMON,a0
	lea.l	54(a0),a0				;longest movem.l .... !
	move.b #";",(a0)+
	tst.b ea2ndBMON
	beq.b eaval0BMON
	lea 10(a0),a0
eaval0BMON:	bsr.w longtoa_BMON
	st ea2ndBMON
	move.l (a7)+,a0
eaval1BMON:	rts

sbytetoa_BMON:move.l d0,-(a7)
	btst #0,dissignBMON
	beq.b sby0BMON
	tst.b d0
	bpl.b sby0BMON
	neg.b d0	
	move.b #"-",(a0)+
sby0BMON:	and.l #$ff,d0
	btst #1,dissignBMON
	beq.b sby1BMON
	bsr.w hextodBMON
	bra.b sby2BMON
sby1BMON:	bsr.w hextoa_BMON			;bsr.w bytetoa_BMON
sby2BMON:	move.l (a7)+,d0
	rts

swordtoa_BMON:move.l d0,-(a7)
	btst #0,dissignBMON
	beq.b swo0BMON
	tst.w d0
	bpl.b swo0BMON
	neg.w d0
	move.b #"-",(a0)+	
swo0BMON:	and.l #$ffff,d0
	btst #1,dissignBMON
	beq.b swo1BMON
	bsr.w hextodBMON
	bra.b swo2BMON
swo1BMON:	bsr.w hextoa_BMON			;bsr.w wordtoa_BMON
swo2BMON:	move.l (a7)+,d0
	rts

;----------------------------------------------------------

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

	even
disoffsBMON:dc.w opfailBMON,noopsBMON,dcwopBMON,eatoccrBMON,eatosrBMON,srtoeaBMON,wordbxxBMON,wordccBMON
	dc.w shortbxxBMON,shortccBMON,eatoaxBMON,eatodxBMON,dxtoeaBMON,iwordtoccrBMON
	dc.w iwordtosrBMON,iwordBMON,ccodewordrelBMON,inibtoeaBMON,ccodeeaBMON,srceaBMON
	dc.w size67eatodxBMON,size8eatoax9bBMON,size67adsbxBMON,size67ixtoeaBMON
	dc.w size67i9btoeaBMON,size67x9btodx02BMON,itrapBMON,srcdx02BMON,imuspBMON
	dc.w imoveBMON,imoveqBMON,icmpmBMON,xbcdBMON,size6movepBMON
	dc.w imovemBMON,dx9btodx02BMON,ax9btoax02BMON,dx9btoax02BMON
	dc.w iunlkBMON,ilinkBMON,imovecBMON,ccrtoeaBMON,imovesBMON,dbfieldBMON,dbfieldtodxBMON
	dc.w ddxtobfieldBMON,dbkptBMON,longbxxBMON,longccBMON,icallmBMON,dcasBMON,dcas2BMON
	dc.w dchk2BMON,dcmp2BMON,drtmBMON,dtpccBMON,dtpccsizeBMON,dlinklgBMON,dmululBMON,dmulslBMON
	dc.w ddivullBMON,ddivsllBMON,dpackBMON,ea2todxBMON,dtpfccBMON,dtpfccsizeBMON
	dc.w dfccodewordrelBMON,fccodeeaBMON,dfnopBMON,dfmovecrBMON,dpmoveBMON,dpmovefdBMON
	dc.w dpflushaBMON,dpflushBMON,dploadBMON,dptestBMON,didbcacBMON,didbindBMON,dmov16BMON
	dc.w dpccodeeaBMON,dpccodewordrelBMON,dtppccBMON,dtppccsizeBMON,dbrapccBMON,dbrafccBMON
	dc.w dpflushsBMON,dpflushrBMON,dpvalidBMON,dpflush40BMON,dmov162BMON,dlpstopBMON
	dc.w dtblBMON,dfpuBMON,dfmovelBMON,dfmovemlBMON,dfmovemxBMON


;----------------------------------------------------------

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


s67BMON =$0000
s6BMON  =$0100
s8BMON  =$0200
stabBMON=$0300
snopBMON=$0400
s9a_BMON=$0500	;68020 casBMON
s9aBMON =$0600	;68020 chk2,cmp2

;----------------------------------------------------------

twolong_BMON:
	bsr.b longtoa_BMON
	move.b #"-",(a0)+
	move.l d1,d0
	bra.b longtoa_BMON
twoadr_BMON:bsr.b adrtoa_BMON
	move.b #"-",(a0)+
	move.l d1,d0
	bra.b adrtoa_BMON
nibtoa_BMON:move.b #"$",(a0)+
nibtoaBMON:	movem.l d0-d2,-(a7)
	moveq #1-1,d2
	ror.l #4,d0
	bra.b lta0BMON
bytetoa_BMON:move.b #"$",(a0)+
bytetoaBMON:movem.l d0-d2,-(a7)
	moveq #2-1,d2
	ror.l #8,d0
	bra.b lta0BMON
wordtoa_BMON:move.b #"$",(a0)+
wordtoaBMON:movem.l d0-d2,-(a7)
	moveq #4-1,d2
	swap d0
	bra.b lta0BMON
adrtoa_BMON:move.b #"$",(a0)+
adrtoaBMON:	movem.l d0-d2,-(a7)
	moveq #6-1,d2
	lsl.l #8,d0
	bra.b lta0BMON
longtoa_BMON:move.b #"$",(a0)+

longtoaBMON:movem.l d0-d2,-(a7)
	moveq #8-1,d2
lta0BMON:	rol.l #4,d0
	moveq #$0f,d1
	and.w d0,d1
	move.b l2asciiBMON(pc,d1.w),(a0)+
	dbf d2,lta0BMON
	movem.l (a7)+,d0-d2
	rts
l2asciiBMON:dc.b "0123456789abcdef"
hextoa_BMON:movem.l d0-d3,-(a7)
	moveq #9,d2
	cmp.l d2,d0
	bls.b hta3BMON
	move.b #"$",(a0)+
hta3BMON:	sf d3
	moveq #8-1,d2
hta0BMON:	rol.l #4,d0
	moveq #$0f,d1
	and.w d0,d1
	bne.b hta1BMON
	tst.w d2
	beq.b hta1BMON
	tst.b d3
	beq.b hta2BMON
hta1BMON:	st d3
	move.b l2asciiBMON(pc,d1.w),(a0)+
hta2BMON:	dbf d2,hta0BMON			
	movem.l (a7)+,d0-d3
	rts
wordtodBMON:move.l d0,-(a7)
	ext.l d0
	divu #1000,d0
	add.b #"0",d0
	move.b d0,(a0)+
	clr.w d0
	swap d0
	divu #100,d0
	add.b #"0",d0
	move.b d0,(a0)+
	swap d0
	bra.b btd0BMON
bytetodBMON:move.l d0,-(a7)
btd0BMON:	cmp.b #99,d0
	bls.b btd1BMON
	moveq #99,d0	
btd1BMON:	ext.w d0
	ext.l d0
	divu #10,d0
	bra.b ntd1BMON
nibtodBMON:	move.l d0,-(a7)
	moveq #%1111,d0
	and.l (a7),d0
	divu #10,d0
	beq.b ntd0BMON
ntd1BMON:	add.b #"0",d0
	move.b d0,(a0)+
ntd0BMON:	swap d0
	add.b #"0",d0
	move.b d0,(a0)+
	move.l (a7)+,d0
	rts
hextodBMON:	movem.l d0-d4/a1,-(a7)
	lea divtabBMON(pc),a1
	moveq #10-1,d1
	sf d4
hd0BMON:	moveq #"0",d2
	move.l (a1)+,d3
hd1BMON:	cmp.l d3,d0
	bcs.b hd2BMON
	st d4
	sub.l d3,d0
	addq.b #1,d2
	bra.b hd1BMON
hd2BMON:	tst.w d1
	beq.b hd4BMON
	tst.b d4
	beq.b hd3BMON
hd4BMON:	move.b d2,(a0)+
hd3BMON:	dbf d1,hd0BMON
	movem.l (a7)+,d0-d4/a1
	rts
divtabBMON:	dc.l 1000000000,100000000,10000000,1000000
	dc.l 100000,10000,1000,100,10,1

hextobBMON:	movem.l d0-d4,-(a7)
	sf d4
	bsr.b hb3BMON	
	bsr.b hb3BMON	
	bsr.b hb3BMON	
	st d4
	bsr.b hb3BMON	
	movem.l (a7)+,d0-d4
	rts
hb3BMON:	rol.l #8,d0
	move.b d0,d3
	bne.b hb2BMON
	tst.b d4
	beq.b hb4BMON
hb2BMON:	st d4
bytetobBMON:moveq #8-1,d2
hb0BMON:	moveq #"0",d1
	add.b d3,d3
	bcc.b hb1BMON
	moveq #"1",d1
hb1BMON:	move.b d1,(a0)+
	dbf d2,hb0BMON
	move.b #" ",(a0)+
hb4BMON:	rts

	ifne HRTeval

atolong_BMON
atolongBMON
atoword_BMON
atowordBMON
atobyte_BMON
atobyteBMON
atodnibBMON
atodlongBMON
		movem.l	d1-d7/a0-a2/a4-a6,-(a7)
		move.l	a3,a0
		bsr	evaluate
		move.l	a0,a3
		movem.l	(a7)+,d1-d7/a0-a2/a4-a6
		tst.l	d0
		rts

	else

atolong_BMON:cmp.b #"$",(a3)
	bne.b atolongBMON
	addq.w #1,a3
atolongBMON:moveq #8-1,d2
	bra.b atob2BMON
atoword_BMON:cmp.b #"$",(a3)
	bne.b atowordBMON
	addq.w #1,a3
atowordBMON:moveq #4-1,d2
	bra.b atob2BMON
atobyte_BMON:cmp.b #"$",(a3)
	bne.b atobyteBMON
	addq.w #1,a3
atobyteBMON:moveq #2-1,d2
atob2BMON:	moveq #0,d0
atob1BMON:	move.b (a3),d1
	cmp.b #"f",d1
	bhi.b atob3BMON
	sub.b #"0",d1
	bcs.b atob3BMON
	cmp.b #9,d1
	bls.b atob0BMON
	cmp.b #"a"-"0",d1
	bcs.b atob3BMON
	sub.b #"a"-"0"-10,d1
atob0BMON:	lsl.l #4,d0
	or.b d1,d0
	addq.w #1,a3
	dbf d2,atob1BMON
atob3BMON:	tst.l d0
	rts
atodnibBMON:moveq #0,d0		;0-99
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

;----------------------------------------------------------
fpuinstBMON:dc.b %10110000,"sinco","s"+128	;order!
	dc.b %00011000,"ab","s"+128,0,0,0
	dc.b %01011000,"sab","s"+128,0,0
	dc.b %01011100,"dab","s"+128,0,0
	dc.b %00011100,"aco","s"+128,0,0
	dc.b %10100010,"ad","d"+128,0,0,0
	dc.b %11100010,"sad","d"+128,0,0
	dc.b %11100110,"dad","d"+128,0,0
	dc.b %00001100,"asi","n"+128,0,0
	dc.b %00001010,"ata","n"+128,0,0
	dc.b %00001101,"atan","h"+128,0
	dc.b %10111000,"cm","p"+128,0,0,0
	dc.b %00011101,"co","s"+128,0,0,0
	dc.b %00011001,"cos","h"+128,0,0
	dc.b %10100000,"di","v"+128,0,0,0
	dc.b %11100000,"sdi","v"+128,0,0
	dc.b %11100100,"ddi","v"+128,0,0
	dc.b %00010000,"eto","x"+128,0,0
	dc.b %00011110,"getex","p"+128
	dc.b %00011111,"getma","n"+128
	dc.b %00000001,"in","t"+128,0,0,0
	dc.b %00000011,"intr","z"+128,0
	dc.b %00010101,"log1","0"+128,0
	dc.b %00010110,"log","2"+128,0,0
	dc.b %00010100,"log","n"+128,0,0
	dc.b %00000110,"lognp","1"+128	
	dc.b %10100001,"mo","d"+128,0,0,0
movetxtBMON:dc.b %10000000,"mov","e"+128,0,0
	dc.b %11000000,"smov","e"+128,0
	dc.b %11000100,"dmov","e"+128,0
	dc.b %10100011,"mu","l"+128,0,0,0
	dc.b %11100011,"smu","l"+128,0,0
	dc.b %11100111,"dmu","l"+128,0,0
	dc.b %00011010,"ne","g"+128,0,0,0
	dc.b %01011010,"sne","g"+128,0,0
	dc.b %01011110,"dne","g"+128,0,0
	dc.b %10100101,"re","m"+128,0,0,0
	dc.b %10100110,"scal","e"+128,0
	dc.b %10100100,"sgldi","v"+128
	dc.b %10100111,"sglmu","l"+128
	dc.b %00001110,"si","n"+128,0,0,0
	dc.b %00000010,"sin","h"+128,0,0
	dc.b %00000100,"sqr","t"+128,0,0
	dc.b %01000001,"ssqr","t"+128,0
	dc.b %01000101,"dsqr","t"+128,0
	dc.b %10101000,"su","b"+128,0,0,0
	dc.b %11101000,"ssu","b"+128,0,0
	dc.b %11101100,"dsu","b"+128,0,0
	dc.b %00001111,"ta","n"+128,0,0,0
	dc.b %00001001,"tan","h"+128,0,0
	dc.b %00010010,"tento","x"+128
	dc.b %00111010,"ts","t"+128,0,0,0
	dc.b %00010001,"twoto","x"+128
	dc.b 0	
	even	
;----------------------------------------------------------
ccodetxtBMON:	dc.b "t",0,"f",0,"hilscccsneeqvcvsplmigeltgtle"	;even!
pccodetxtBMON:	dc.b "bsbclslcssscasacwswcisicgsgccscc"	;even!
fccodetxtBMON:	dc.l "f"<<24	;fpu condition codesBMON
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
		even


;----------------------------------------------------------

linebufBMON		dc.l 0

modeBMON		dc.b 0

imsizeBMON:	dc.b 0			;%00.b,%01.w,%10.l,%11.*,%100.x
ea2ndBMON:	dc.b 0			;0:1st eavalue,ff:2nd eavalue
dissignBMON:dc.b %101 	;bit0:nosign/sign,bit1:hex/dez,bit2:no eavalBMON/eavalBMON
	even

baseregBMON		dc.l 0		;effective addressBMON accu
disaddrBMON		dc.l 0

;----------------------------------------------------------
regsBMON:	blk.l 16+6+1+1,0 ;regsBMON/usp/isp/msp/vbr/cacr/caar/pc/sr.w
vbregBMON=19*4+regsBMON
sfcdfcBMON:	dc.w 0			;00000xxx00000yyy
fpuregsBMON:blk.l 8*12,0		;fp0..7:80+16bit
fpcrBMON:	dc.l 0			;\
fpsrBMON:	dc.l 0			; )union
fpiarBMON:	dc.l 0			;/
mmutt0BMON:	dc.l 0			;\
mmutt1BMON:	dc.l 0			; \
mmutcBMON:	dc.l 0			;  )union
mmusrpBMON:	dc.l 0,0		;  )
mmucrpBMON:	dc.l 0,0		; /
mmusrBMON:	dc.w 0			;/
mmudrpBMON:	dc.l 0,0		;68851 only



