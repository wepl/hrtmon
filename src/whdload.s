;
; this file contains all whdload related commands
;

TDREASON_OK		 = -1
TDREASON_DEBUG		 =  5
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
WHDLTAG_BUILD_GET	 = $88000013

;---------------
; stingray, 02-Dec-2015:
; Was "WL" but since the WS command had to be renamed to WSM
; I have renamed the "WL" command to "WLM" to have a consistent
; naming scheme.
; command WLM -	load a file using resload_LoadFile
;		(the same as command L but via WHDLoad)

cmd_wlm		tst.l	(whd_base)
		beq	w_notinwhdload

		lea	ev_line,a1
		bsr	read_name
		tst.b	(a1)
		move.l	a1,d3			;d3 = filename
		beq	illegal_name
		bsr	evaluate
		bne	illegal_addr
		move.l	d0,d4			;d4 = address
		bsr	evaluate
		beq	illegal_addr

		bsr	w_wait
		jsr	remove_pic

		move.l	d3,a0
		move.l	(whd_base),a2
		jsr	(resload_GetFileSize,a2)
		move.l	d0,d5			;d5 = length
		beq	.nf
		
		move.l	d3,a0			;name
		move.l	d4,a1			;address
		move.l	(whd_base),a2
		jsr	(resload_LoadFile,a2)
.nf
		jsr	set_pic

		tst.l	d5
		bne	.ok
		lea	(w_txt_nofile),a0
		bsr	print
		bra	w_end
.ok
		lea	(w_txt_file),a0
		move.l	d5,-(a7)
		movem.l	d3-d5,-(a7)
		move.l	a7,a1
		bsr	printf
		add	#16,a7
		bra	w_end

;---------------
; stingray, 02-Dec-2015:
; Was "WS" which made the originally built-in "ws" command
; (W.rite S.ectors) not work anymore, renamed to WSM!
; command WSM -	save memory region using resload_SaveFile
;		(the same as command S but via WHDLoad)

cmd_wsm		tst.l	(whd_base)
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

		bsr	w_wait
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
; key Alt+PrtSc - save current screen using resload_SaveFile

key_prtsc	movem.l	d0-d3/a0-a2,-(a7)

		tst.l	(whd_base)
		beq	.ret

		bsr	w_wait			;'PrtSc'
		bsr	w_wait			;'Alt'

		move.w	screen_height,d2
		sub.w	#3,d2
		move.w	d2,d3
		mulu	#81,d3
		
		move.l	ascII_ptr,a0
		sub.w	d3,a7
		move.l	a7,a2
		subq.w	#1,d2
.2		moveq	#80-1,d0
.1		move.b	(a0)+,(a2)+
		dbf	d0,.1
		move.b	#10,(a2)+
		dbf	d2,.2

		move.l	d3,d0			;length
		lea	(.prtsc),a0		;name
		move.l	a7,a1			;address
		move.l	(whd_base),a2
		jsr	(resload_SaveFile,a2)
		
		add.w	d3,a7

.ret		movem.l	(a7)+,d0-d3/a0-a2
		rts

.prtsc		dc.b	"hrtmon-screen.txt",0

;---------------
; command WQ -	quit WHDLoad

cmd_wq		move.l	(whd_base),d0
		beq	w_notinwhdload

		bsr	w_wait

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

		bsr	w_wait

		sf	entered
		sf	no_curs
		bsr	clear_break

		btst	#1,(sr_reg)
		beq	.usermode

.supermode	move.l	(ssp_reg),a1
		move.w	(6,a1),d1		;frame type
		and.w	#$f000,d1
		rol.w	#4,d1
		move.b	(.tab,pc,d1.w),d1	;frame length
		sub.w	#12,a1
		move.l	a1,(ssp_reg)
.copy		move.w	(12,a1),(a1)+
		subq.w	#2,d1
		bne	.copy
		bra	.setargs

.usermode	move.l	(usp_reg),a1
		sub.w	#12,a1
		move.l	a1,(usp_reg)

.setargs	move.l	#TDREASON_DEBUG,(a1)+
		move.l	(pc_reg),(a1)+
		clr.w	(a1)+
		move.w	(sr_reg),(a1)

		add.l	#resload_Abort,d0
		move.l	d0,(pc_reg)

		st	escape
		bra	w_end

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
; command WPD -	delete memory protection

cmd_wpd		tst.l	(whd_base)
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
		jsr	(resload_ProtectRemove,a2)
		
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
; command WI - show infos regarding WHDLoad

cmd_wi		tst.l	(whd_base)
		beq	w_notinwhdload

		clr.l	-(a7)
		clr.l	-(a7)
		pea	WHDLTAG_BUILD_GET
		move.l	a7,a0
		move.l	(whd_base),a2
		jsr	(resload_Control,a2)
		move.l	(whd_version),(a7)
		lea	(w_txt_whdload,pc),a0
		move.l	a7,a1
		bsr	printf
		add	#12,a7

		lea	(w_txt_chipmem,pc),a0
		lea	(max_chip),a1
		bsr	printf

	;if WHDLoad >= v16.6 print expmem
		move.l	(whd_expstrt),d0
		beq	.noexp
		move.l	(whd_expstop),d1
		move.l	d1,-(a7)
		sub.l	d0,(a7)
		move.l	(a7),-(a7)
		move.l	d1,-(a7)
		move.l	d0,-(a7)
		lea	(w_txt_expmem,pc),a0
		move.l	a7,a1
		bsr	printf
		add	#16,a7
.noexp
	;if WHDLoad >= v16.9 print slave
		move.l	(whd_slvstrt),d0
		beq	w_end
		move.l	(whd_slvstop),d1
		move.l	d1,-(a7)
		sub.l	d0,(a7)
		move.l	(a7),-(a7)
		move.l	d1,-(a7)
		move.l	d0,-(a7)
		lea	(w_txt_slvmem,pc),a0
		bsr	print
		lea	(w_txt_mem,pc),a0
		move.l	a7,a1
		bsr	printf
		add	#16,a7
		bra	w_end
		
;---------------
		
w_success	lea	(w_txt_success,pc),a0
		bra	w_printreturn

w_notinwhdload	lea	(w_txt_notinwhdload,pc),a0
		bra	w_printreturn

w_resloaderr	lea	(w_txt_resloaderr,pc),a0

w_printreturn	bsr	print
w_end		jmp	end_command

	;wait for key release to avoid further keypress detections
w_wait		btst	#0,(_ciaa+ciasdr)
		bne	w_wait
.1		btst	#0,$dff005
		beq	.1
.2		btst	#0,$dff005
		bne	.2
.3		btst	#0,$dff005
		beq	.3
		rts

w_txt_success		dc.b	"success",10,0
w_txt_resloaderr	dc.b	"error, resload function failed",10,0
w_txt_notinwhdload	dc.b	"error, whdload not active or old whdload version",10,0
w_txt_nofile		dc.b	"file not found (or length = 0)",10,0
w_txt_file		dc.b	"file '%s' loaded to $%lx length=$%lx=%ld",10,0
w_txt_whdload		dc.b	"WHDLoad v%d.%d.%ld",10,0
w_txt_chipmem		dc.b	"ChipMem - $%lx",10,0
w_txt_expmem		dc.b	"ExpMem"
w_txt_mem		dc.b	"  $%lx - $%lx = $%lx = %ld",10,0
w_txt_slvmem		dc.b	"Slave ",0
	EVEN

