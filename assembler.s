
	XDEF _DoJSR
	XDEF _RemHRTmon
	XDEF _BootSum

;int BootSum(int *BootBlockAddress);

_BootSum	movem.l	d1-a6,-(a7)
		move.l	15*4(a7),a0	;get BootBlockAddress
		moveq	#0,d0
		move.w	#512*2/4-1,d1
.add		add.l	(a0)+,d0
		bcc.b	.noc
		addq.l	#1,d0
.noc		dbf	d1,.add
		not.l	d0
		movem.l	(a7)+,d1-a6
		rts

;void DoJSR(unsigned long address);

_DoJSR		movem.l	d0-a6,-(a7)
		move.l	16*4(a7),a0	;get parameter
		jsr	(a0)
		movem.l	(a7)+,d0-a6
		rts

;int RemHRTmon();
;Check if HRTmon already installed if YES remove it and FreeMem.
;return TRUE if HRTmon deinstalled
;return FALSE if HRTmon not located

_RemHRTmon	movem.l	d1-a6,-(a7)
		move.l	$4.w,a6
		lea.l	super(pc),a5
		jsr	-30(a6)
		tst.l	d7
		beq.b	.noHRT
		jsr	16(a4)			;remove
.noHRT		move.l	d7,d0
		movem.l	(a7)+,d1-a6
		rts

super		moveq	#0,d7
		move.l	$10.w,a3
		move.l	#.illegal,$10.w
	MC68010
		movec	VBR,a0
	MC68000
.return		move.l	$7c(a0),a0
		cmp.l	#'HRT!',-4(a0)
		bne.b	.noHRT
		moveq	#-1,d7
		move.l	-8(a0),a4
.noHRT		move.l	a3,$10.w
		rte


.illegal:	sub.l	a0,a0
		move.l	#.return,2(a7)
		rte
