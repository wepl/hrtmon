;
; this file contains all whdload related commands
;

 IFND WHDLOAD_I
TDREASON_OK		 = -1
resload_Abort		 =  4
resload_LoadFile	 =  8
resload_SaveFile	 = 12
resload_SetCACR		 = 16
resload_ListFiles	 = 20
resload_Decrunch	 = 24
resload_LoadFileDecrunch = 28
resload_FlushCache	 = 32
resload_GetFileSize	 = 36
resload_DiskLoad	 = 40
resload_DiskLoadDev	 = 44
resload_CRC16		 = 48
resload_Control		 = 52
resload_SaveFileOffset	 = 56
resload_ProtectRead	 = 60
resload_ProtectReadWrite = 64
resload_ProtectWrite	 = 68
resload_ProtectRemove	 = 72
resload_LoadFileOffset	 = 76
resload_Relocate	 = 80
resload_Delay		 = 84
resload_DeleteFile	 = 88
resload_ProtectSMC	 = 92
waitvb	MACRO
.1\@		btst	#0,($dff005)
		beq	.1\@
.2\@		btst	#0,($dff005)
		bne	.2\@
	ENDM
 ENDC

;---------------
; command WS -	save memory region using resload_SaveFile
;		(the same as command S but via WHDLoad)
;

cmd_ws		tst.l	(whd_base)
		beq	w_notinwhdload

		lea	ev_line,a1
		bsr	read_name
		tst.b	(a1)
		beq	illegal_name
		bsr	evaluate
		bne	illegal_addr
		move.l	d0,d4			;d4=save start
		bsr	evaluate
		bne	illegal_addr
		sub.l	d4,d0			;d0=save len
		ble	illegal_addr		;end must be > start

		jsr	remove_pic
		
		move.l	a1,a0			;name
		move.l	d4,a1			;address
		move.l	(whd_base),a2
		jsr	(resload_SaveFile,a2)

		jsr	set_pic
		
		tst.l	d0			;only for the case that
						;WHDL_NoError is not active
		beq	w_resloaderr
		bra	w_success

;---------------
; command WQ -	quit WHDLoad

cmd_wq		move.l	(whd_base),d0
		beq	w_notinwhdload

		sf	entered
		sf	no_curs
		bsr	clear_break

		pea	TDREASON_OK
		move.l	d0,a2
		jmp	(resload_Abort,a2)

;---------------
; command WD -	quit WHDLoad with debug

cmd_wd		move.l	(whd_base),d0
		beq	w_notinwhdload

		sf	entered
		sf	no_curs
		bsr	clear_break
		
		move.l	(ssp_reg),a1
		move.w	(6,a1),d1		;frame type
		and.w	#$f000,d1
		rol.w	#4,d1
		move.b	(.tab,pc,d1.w),d1
		sub.w	#12,a1
		move.l	a1,(ssp_reg)
.copy		move.w	(12,a1),(a1)+
		subq.w	#2,d1
		bne	.copy
		move.l	#TDREASON_DEBUG,(a1)+
		move.l	(pc_reg),(a1)+
		clr.w	(a1)+
		move.w	(sr_reg),(a1)

		add.l	#resload_Abort,d0
		move.l	d0,(pc_reg)

		st	escape
		jmp	end_command

.tab		dc.b	8	;#0 Four-Word
		dc.b	8	;#1 Throwaway Four-Word
		dc.b	12	;#2 Six-Word
		dc.b	12	;#3 68040 Floating-Point Post-Instruction
		dc.b	16	;#4 68040 Floating-Point Unimplemented
				;   68060 Eight-Word
		dc.b	0	;#5
		dc.b	0	;#6
		dc.b	$3c	;#7 68040 Access Error
		dc.b	29*2	;#8 68010 Bus and Address Error
		dc.b	20	;#9 68020/30 Coprocessor Mid-Instruction
		dc.b	$20	;#a 68020/30 Short Bus Cycle
		dc.b	$5c	;#b 68020/30 Long Bus Cycle
		dc.b	$18	;#c CPU32 Bus Error ...
		dc.b	0	;#d
		dc.b	0	;#e
		dc.b	0	;#f

;---------------
; command WPR -	protect memory region from reading

cmd_wpr		tst.l	(whd_base)
		beq	w_notinwhdload

		bsr	evaluate
		bne	illegal_addr
		move.l	d0,d4			;d4 = address

		bsr	evaluate
		bne	illegal_addr
		move.l	d0,d5			;d5 = length

		move.l	d5,d0
		move.l	d4,a0
		move.l	(whd_base),a2
		jsr	(resload_ProtectRead,a2)
		
		bra	w_success
		
;---------------
; command WPRW - protect memory region from reading and writing

cmd_wprw	tst.l	(whd_base)
		beq	w_notinwhdload

		bsr	evaluate
		bne	illegal_addr
		move.l	d0,d4			;d4 = address

		bsr	evaluate
		bne	illegal_addr
		move.l	d0,d5			;d5 = length

		move.l	d5,d0
		move.l	d4,a0
		move.l	(whd_base),a2
		jsr	(resload_ProtectReadWrite,a2)
		
		bra	w_success
		
;---------------
; command WPW -	protect memory region from writing

cmd_wpw		tst.l	(whd_base)
		beq	w_notinwhdload

		bsr	evaluate
		bne	illegal_addr
		move.l	d0,d4			;d4 = address

		bsr	evaluate
		bne	illegal_addr
		move.l	d0,d5			;d5 = length

		move.l	d5,d0
		move.l	d4,a0
		move.l	(whd_base),a2
		jsr	(resload_ProtectWrite,a2)
		
		bra	w_success
		
;---------------
; command WPSMC - snoop memory region for selfmodifying code

cmd_wpsmc	tst.l	(whd_base)
		beq	w_notinwhdload

		bsr	evaluate
		bne	illegal_addr
		move.l	d0,d4			;d4 = address

		bsr	evaluate
		bne	illegal_addr
		move.l	d0,d5			;d5 = length

		move.l	d5,d0
		move.l	d4,a0
		move.l	(whd_base),a2
		jsr	(resload_ProtectSMC,a2)
		
		bra	w_success
		
;---------------
		
w_success	lea	(w_txt_success,pc),a0
		bra	w_printreturn

w_notinwhdload	lea	(w_txt_notinwhdload,pc),a0
		bra	w_printreturn

w_resloaderr	lea	(w_txt_resloaderr,pc),a0

w_printreturn	pea	(end_command)
	;wait because strange keyboard handling
		moveq	#40,d0
.1		waitvb
		dbf	d0,.1
		bra	print

w_txt_success		dc.b	"success",10,0
w_txt_resloaderr	dc.b	"error, resload function failed",10,0
w_txt_notinwhdload	dc.b	"error, whdload not active or old whdload version",10,0
	EVEN

