
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

;block caching routines

nb_fbuf equ 8	;max nb of blocks cached

;------------- file_buffers ----------------------------------------------
;------------- LRU replacement algorithm ---------------------------------

;-> a3=partition
;-> d0=no of block to read
;-> a0=address of block

read_fbuffer	movem.l	d0-d1/a0-a3,-(a7)
		st	disk_op

		bsr	inc_age

		move.l	#fbuffer1,d1
.next		move.l	d1,a1
		tst.l	fbuf_block(a1)		;free ?
		bmi.b	.nothis
		cmp.l	fbuf_part(a1),a3	;same partition ?
		bne.b	.nothis
		cmp.l	fbuf_block(a1),d0	;same block no ?
		bne.b	.nothis
		clr.w	fbuf_age(a1)
		move.l	fbuf_ptr(a1),a2
		move.w	#512/8-1,d0
.read		move.l	(a2)+,(a0)+
		move.l	(a2)+,(a0)+
		dbf	d0,.read
		bra.b	.end
.nothis		move.l	fbuf_next(a1),d1
		bne.b	.next

		bsr	find_freefbuf

		move.l	a3,fbuf_part(a1)
		move.l	d0,fbuf_block(a1)
		sf	fbuf_written(a1)
		clr.w	fbuf_age(a1)
		move.l	part_device(a3),a2
		jsr	READ_CMD(a2)		;read block into dest
		move.l	fbuf_ptr(a1),a2
		move.w	#512/8-1,d0
.copy		move.l	(a0)+,(a2)+		;copy block into fbuf
		move.l	(a0)+,(a2)+
		dbf	d0,.copy

.end		movem.l	(a7)+,d0-d1/a0-a3
		rts

inc_age		movem.l	d1/a1,-(a7)
		move.l	#fbuffer1,d1
.loop		move.l	d1,a1
		tst.l	fbuf_block(a1)		;block used ?
		bmi.b	.free
		cmp.w	#$7fff,fbuf_age(a1)	;reached max val. ?
		beq.b	.free
		addq.w	#1,fbuf_age(a1)
.free		move.l	fbuf_next(a1),d1
		bne.b	.loop
		movem.l	(a7)+,d1/a1
		rts

;<- a1=ptr on fbuffer (write back one if needed)

find_freefbuf	movem.l	d0-d2/a0/a2-a3,-(a7)
		moveq	#-1,d0			;to find max age
		move.l	#fbuffer1,d1
.loop		move.l	d1,a1
		tst.l	fbuf_block(a1)
		bmi.w	.found
		cmp.w	fbuf_age(a1),d0		;older ?
		bgt.b	.notolder
		blt.b	.go
		tst.b	fbuf_written(a1);if same age => avoid to write-back
		bne.b	.notolder
.go		move.w	fbuf_age(a1),d0
		move.l	a1,a0
.notolder	move.l	fbuf_next(a1),d1
		bne.b	.loop

		move.l	a0,a1	;this is the fbuffer to kill (older)

		tst.b	fbuf_written(a1)	;need to write-back ?
		beq.b	.found

		move.l	fbuf_block(a1),d0
		move.l	fbuf_part(a1),a3
		moveq	#1,d1			;nb blocks to writem
		move.l	fbuf_ptr(a1),-(a7)	;block address on stack
		addq.l	#1,d0			;seek next block
		move.l	#fbuffer1,d2
.loop2		move.l	d2,a2
		tst.l	fbuf_block(a2)
		bmi.b	.no
		cmp.l	fbuf_block(a2),d0	;next block ?
		bne.b	.no
		cmp.l	fbuf_part(a2),a3	;same part ?
		bne.b	.no
		tst.b	fbuf_written(a2)	;need to write ?
		beq.b	.no
		addq.l	#1,d1			;write 1 more block
		addq.l	#1,d0			;seek next block
		move.l	fbuf_ptr(a2),-(a7)
		sf	fbuf_written(a2)
		move.l	#fbuffer1,d2
		bra.b	.loop2
.no		move.l	fbuf_next(a2),d2
		bne.b	.loop2

		sub.l	d1,d0
		move.l	part_device(a3),a2
		jsr	WRITEM_CMD(a2)		;write back block(s)

.found		movem.l	(a7)+,d0-d2/a0/a2-a3
		rts

;-> a3=partition
;-> d0=no of block to write
;-> a0=address of block

write_fbuffer	movem.l	d0-d1/a0-a3,-(a7)
		st	disk_op

		bsr	inc_age

		move.l	#fbuffer1,d1
.next		move.l	d1,a1
		tst.l	fbuf_block(a1)		;free ?
		bmi.b	.nothis
		cmp.l	fbuf_part(a1),a3	;same partition ?
		bne.b	.nothis
		cmp.l	fbuf_block(a1),d0	;same block no ?
		bne.b	.nothis
		clr.w	fbuf_age(a1)
		st	fbuf_written(a1)	;need to write-back
		move.l	fbuf_ptr(a1),a2
		move.w	#512/8-1,d0
.write		move.l	(a0)+,(a2)+
		move.l	(a0)+,(a2)+
		dbf	d0,.write
		bra.b	.end
.nothis		move.l	fbuf_next(a1),d1
		bne.b	.next

		bsr	find_freefbuf

		move.l	a3,fbuf_part(a1)
		move.l	d0,fbuf_block(a1)
		st	fbuf_written(a1)
		clr.w	fbuf_age(a1)
		move.l	fbuf_ptr(a1),a2
		move.w	#512/8-1,d0
.copy		move.l	(a0)+,(a2)+		;copy block into fbuf
		move.l	(a0)+,(a2)+
		dbf	d0,.copy

.end		movem.l	(a7)+,d0-d1/a0-a3
		rts


flush_fbuffer	movem.l	d0/a0-a3,-(a7)
		move.l	#fbuffer1,d0
.loop		move.l	d0,a1
		tst.l	fbuf_block(a1)		;block in buffer ?
		bmi.b	.free
		tst.b	fbuf_written(a1)	;need to update ?
		beq.b	.clear
		move.l	fbuf_part(a1),a3
		move.l	part_device(a3),a2
		move.l	fbuf_ptr(a1),a0
		move.l	fbuf_block(a1),d0
		jsr	WRITE_CMD(a2)		;write back to disk
		jsr	UPDATE_CMD(a2)
.clear		moveq	#-1,d0
		move.l	d0,fbuf_block(a1)	;set it to free
.free		move.l	fbuf_next(a1),d0
		bne.b	.loop
		movem.l	(a7)+,d0/a0-a3
		rts


init_fbuffer	movem.l	d0/a0-a2,-(a7)
		lea.l	fbuffer1,a0
		lea.l	fbuf1,a1
		move.w	#nb_fbuf-1,d0
		move.l	a0,a2
.loop		move.l	a0,(a2)
		clr.l	(a0)
		move.l	a1,fbuf_ptr(a0)
		clr.l	fbuf_part(a0)
		move.l	#-1,fbuf_block(a0)
		clr.w	fbuf_age(a0)
		sf	fbuf_written(a0)
		move.l	a0,a2
		lea.l	fbuf_SIZEOF(a0),a0
		lea.l	512(a1),a1
		dbf	d0,.loop
		movem.l	(a7)+,d0/a0-a2
		rts

		STRUCTURE fbuffers,0
		ULONG fbuf_next
		ULONG fbuf_ptr
		ULONG fbuf_part
		ULONG fbuf_block
		UWORD fbuf_age
		UBYTE fbuf_written
		UBYTE fbuf_align
		LABEL fbuf_SIZEOF

;-------------- block buffer to speedup file access -------

fbuffer1	rept nb_fbuf
		dc.l 0			;next
		dc.l 0			;ptr
		dc.l 0			;part
		dc.l -1			;block no
		dc.w 0			;age
		dc.b 0,0		;written, align
		endr


