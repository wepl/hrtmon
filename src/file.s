
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

;file access routines

;-----------------------------------------------------
;-------------- can open max 2 files -----------------

;-> d1=ptr on filename (ended by a zero)
;-> d2=mode 0=oldfile,-1=newfile,1=oldfile or olddir (used to delete a dir)
;<- d0=ptr on file_handle

open_file	movem.l	d1-d7/a0-a4,-(a7)
		st	disk_op
		move.l	d2,d7			;copy mode in d7
		move.l	d1,d6			;copy of full filename* in d6
		lea.l	file_handle1,a1
		tst.l	(a1)
		beq.b	.okfree
		lea.l	file_handle2,a1
		tst.l	(a1)
		beq.b	.okfree
		moveq	#0,d0
		bra.w	.error

.okfree		bsr	find_partition
		tst.l	d0
		beq.w	.error
		move.l	d0,a3
		move.l	d0,file_part(a1)
		move.l	d2,file_parent(a1)
		clr.l	file_seek(a1)
		clr.l	file_extpos(a1)
		sf	file_written(a1)
		sf	file_dir(a1)

		bsr	copy_name		;set BCPL name into filehandle

		move.l	part_device(a3),a2
		lea.l	extension,a0
		move.l	file_parent(a1),d0
		move.l	d0,d3
		bsr	read_fbuffer		;read ROOT or PARENT
		bsr	test_sum
		beq.b	.oksum
		moveq	#0,d0
		bra.w	.error
.oksum
		tst.l	d7			;oldfile or newfile ?
		bpl.w	.oldfile

;-------------- open new file --------------

		clr.l	file_size(a1)

		bsr	alloc_block		;find block for file_header
		bpl.b	.okhead
		move.w	#DISKFULL_ERR,drive_err
		moveq	#0,d0
		bra.w	.error
.okhead		move.l	d0,file_header(a1)
		move.l	d0,file_extension(a1)
		bsr	calc_hash
		move.l	d3,d6			;d6=place where to ins. in hash
		sf	d5			;signal for d6 found
.next2		lea.l	extension,a0
		move.l	(a0,d0.w),d0		;find end of hash list
		beq.b	.okfreeh
		tst.b	d5			;already found place where
		bne.b	.foundins		;to insert in hash list ?
		cmp.l	file_header(a1),d0
		bls.b	.foundins
		st	d5
		move.l	d3,d6			;d6=place where to ins. in hash
.foundins	move.l	d0,d3			;d3=no of actual loaded block
		bsr	read_fbuffer		;read next in hash list
		bsr	test_sum
		beq.b	.oksum2
		move.l	file_header(a1),d0
		bsr	free_block
		bsr	update_bitmap
		moveq	#0,d0
		bra.w	.error
.oksum2		lea.l	108*4(a0),a0
		bsr	cmp_filename		;file already exists ?
		bne.b	.different
		move.l	file_header(a1),d0
		bsr	free_block
		bsr	update_bitmap
		move.w	#FILEEXIST_ERR,drive_err
		moveq	#0,d0
		bra.w	.error
.different	move.w	#124*4,d0
		bra.b	.next2
.okfreeh	tst.b	d5
		beq.b	.loaded
		cmp.l	d3,d6			;block already loaded ?
		beq.b	.loaded
		move.l	d6,d0
		move.l	d0,d3
		bsr	read_fbuffer
.loaded		cmp.l	#1,127*4(a0)		;ROOT ?
		beq.b	.puthash
		move.l	1*4(a0),d0
		cmp.l	file_parent(a1),d0	;is it the PARENT
		bne.b	.putlist
.puthash	bsr	calc_hash
		move.l	(a0,d0.w),d6		;next in hash list
		move.l	file_header(a1),(a0,d0.w)
		bra.b	.savesec
.putlist	move.l	124*4(a0),d6		;next in hash list
		move.l	file_header(a1),124*4(a0)
.savesec	lea.l	extension,a0
		bsr	calc_sum
		move.l	d3,d0
		bsr	write_fbuffer		;write back the block
						;containing the ref to the file

;-------------- init the file header --

		lea.l	secbuf,a4
		moveq	#128-1,d0
.clr		clr.l	(a4)+			;clear whole block
		dbf	d0,.clr
		lea.l	secbuf,a0
		move.l	#2,(a0)			;Type 2 (T.SHORT)
		move.l	file_header(a1),1*4(a0)	;Header Key
		lea.l	108*4(a0),a4
		lea.l	file_name(a1),a0
		moveq	#8-1,d0
.copy_name	move.l	(a0)+,(a4)+		;copy filename
		dbf	d0,.copy_name
		lea.l	secbuf,a0
		move.l	file_parent(a1),125*4(a0)	;parent
		move.l	#-3,127*4(a0)		;sec.type (File-header)
		move.l	d6,124*4(a0)		;next in hash list

		bsr	calc_sum
		move.l	file_header(a1),d0
		bsr	write_fbuffer		;write new file header block

		bsr	update_bitmap

		bra.w	.okfile			;ok, return file_handle

;-------------- open old file --------------

.oldfile	bsr	calc_hash
.next		lea.l	extension,a0
		move.l	(a0,d0.w),d0
		bne.b	.oknext
		move.w	#FILENOTFOUND_ERR,drive_err
		moveq	#0,d0
		bra.b	.error
.oknext		bsr	read_fbuffer		;read next in hash list
		bsr	test_sum
		beq.b	.oksum3
		moveq	#0,d0
		bra.b	.error
.oksum3		lea.l	108*4(a0),a0		;filename
		bsr	cmp_filename
		beq.b	.found
		move.w	#124*4,d0
		bra.b	.next

.found		lea.l	extension,a0
		move.l	d0,file_header(a1)
		move.l	d0,file_extension(a1)
		move.l	81*4(a0),file_size(a1)	;copy file_size
		cmp.l	#-3,127*4(a0)		;is it a real file_header ?
		beq.b	.okfile
		tst.l	d7			;allow to open a dir ?
		beq.b	.nodir
		cmp.l	#2,127*4(a0)		;is it a directory ?
		beq.b	.okfile
.nodir		move.w	#FILENOTFOUND_ERR,drive_err
		moveq	#0,d0
		bra.b	.error

.okfile		move.l	a1,d0			;return file_handle

.error		tst.l	d0
		bne.b	.noerr
		clr.l	(a1)			;free file_handle
.noerr		movem.l	(a7)+,d1-d7/a0-a4
		tst.l	d0
		rts

;----------------------------------------------------------

;-> d1=ptr on file_handle
;<- d0=result 0=no error -1=error

close_file	movem.l	d1-d7/a0-a4,-(a7)
		tst.l	d1
		beq.b	.endclose
		move.l	d1,a1
		move.l	file_part(a1),a3
		move.l	part_device(a3),a2
		tst.b	file_written(a1)
		beq.b	.notwritten
		bsr	update_bitmap
		lea.l	secbuf,a0
		move.l	file_header(a1),d0
		bsr	read_fbuffer		;read fileheader
		move.l	file_size(a1),81*4(a0)	;update filesize
		tst.l	2*4(a0)			;any block present ?
		beq.b	.noblock
		move.l	77*4(a0),4*4(a0)	;copy first block no
.noblock	bsr	calc_sum
		bsr	write_fbuffer		;write correct fileheader

.notwritten	bsr	flush_fbuffer
		jsr	MOTOFF_CMD(a2)
		clr.l	(a1)			;Free file_handle
.endclose	moveq	#0,d0
		movem.l	(a7)+,d1-d7/a0-a4
		rts

;----------------------------------------------------------

;-> d1=ptr on file_handle
;-> d2=ptr on buffer
;-> d3=len to read
;<- d0=result 0=ok -1=error

read_file	movem.l	d1-d7/a0-a4,-(a7)
		move.l	d1,a1
		move.l	file_part(a1),a3
		move.l	part_device(a3),a2

		move.l	file_size(a1),d4
		sub.l	file_seek(a1),d4	;max val for len
		cmp.l	d4,d3
		ble.b	.oklen
		move.l	d4,d3			;truncate len
.oklen		tst.l	d3
		beq.w	.endread

		move.l	d2,a4

.recheck	move.l	file_extension(a1),d0
		lea.l	extension,a0
		bsr	read_fbuffer		;read extension block
		bsr	test_sum
		bne.w	.endread
		move.l	file_seek(a1),d0
		sub.l	file_extpos(a1),d0
		cmp.l	#72*512,d0
		blt.b	.inthisext
		move.l	126*4(a0),d0		;next extension block
		move.l	d0,file_extension(a1)
		add.l	#72*512,file_extpos(a1)
		bra.b	.recheck

.inthisext
		move.l	file_seek(a1),d4
		sub.l	file_extpos(a1),d4
		moveq	#9,d1
		lsr.l	d1,d4			;div 512
		neg.w	d4
		add.w	#71,d4
		lea.l	extension,a0
		lsl.w	#2,d4
		move.l	6*4(a0,d4.w),d0		;first data block
		lsr.w	#2,d4
		move.l	file_seek(a1),d1
		and.l	#$1ff,d1		;get offset in first block
		lea.l	secbuf,a0
		bsr	read_fbuffer		;read first data block
		move.l	#512,d5
		sub.l	d1,d5			;nb bytes in first block
		cmp.l	d3,d5
		bge.w	.lastread	;all bytes to read in this block ?

		lea.l	secbuf,a0
		add.l	d1,a0
		sub.l	d5,d3
		add.l	d5,file_seek(a1)
		subq.w	#1,d5
.copyf		move.b	(a0)+,(a4)+		;copy bytes from 1st block
		dbf	d5,.copyf

.nextdata	subq.w	#1,d4			;next block
		bpl.b	.okextension		;still in this extension ?
		lea.l	extension,a0
		move.l	126*4(a0),d0		;next extension
		move.l	d0,file_extension(a1)
		bsr	read_fbuffer		;read next extension block
		bsr	test_sum
		bne.b	.endread
		moveq	#71,d4
		add.l	#72*512,file_extpos(a1)

.okextension	lea.l	extension,a0
		lsl.w	#2,d4
		move.l	6*4(a0,d4.w),d0		;next data block
		lsr.w	#2,d4
		cmp.l	#512,d3			;nb bytes to read > 1 block ?
		blt.b	.readlast

		move.l	a4,a0
		bsr	read_fbuffer		;read data block
		lea.l	512(a4),a4
		add.l	#512,file_seek(a1)
		sub.l	#512,d3
		bne.b	.nextdata
		bra.b	.endread		;no more bytes to read

.readlast	lea.l	secbuf,a0
		bsr	read_fbuffer
		moveq	#0,d1			;offset in block=0

.lastread	lea.l	secbuf,a0
		add.l	d1,a0
		add.l	d3,file_seek(a1)
		subq.w	#1,d3
.copy		move.b	(a0)+,(a4)+
		dbf	d3,.copy

.endread	moveq	#0,d0
		tst.w	drive_err
		beq.b	.noerr
		moveq	#-1,d0
.noerr
		movem.l	(a7)+,d1-d7/a0-a4
		rts

;----------------------------------------------------------

;-> d1=ptr on file_handle
;-> d2=ptr on buffer
;-> d3=len to write
;<- d0=result 0=ok -1=error

write_file	movem.l	d1-d7/a0-a4,-(a7)

		move.l	d1,a1
		st	file_written(a1)
		move.l	file_part(a1),a3
		move.l	part_device(a3),a2

		move.l	file_size(a1),d0
		sub.l	file_seek(a1),d0	;nb bytes till end of file
		cmp.l	d0,d3
		ble.b	.noappend

		tst.l	d0
		beq.b	.noover
		bsr	overwrite		;overwrite till end of file
		tst.w	drive_err
		bne.b	.end
		add.l	d0,d2			;new buffer address
		sub.l	d0,d3			;nb bytes left to write
.noover		move.l	d3,d0
		bsr	append			;then append
		bra.b	.end

.noappend	move.l	d3,d0
		bsr	overwrite

.end		bsr	update_bitmap
		moveq	#0,d0
		tst.w	drive_err
		beq.b	.exit
		moveq	#-1,d0

.exit		movem.l	(a7)+,d1-d7/a0-a4
		rts

;-> a1=ptr on file_handle
;-> d0=nb bytes to overwrite
;-> d2=ptr on buffer
;-> a2=device
;-> a3=part

overwrite	movem.l	d0-a4,-(a7)
		move.l	d2,a4
		move.l	d0,d3

.recheck	move.l	file_extension(a1),d0
		lea.l	extension,a0
		bsr	read_fbuffer		;read extension block
		bsr	test_sum
		bne.w	.endwrite
		move.l	file_seek(a1),d0
		sub.l	file_extpos(a1),d0
		cmp.l	#72*512,d0
		blt.b	.inthisext
		move.l	126*4(a0),d0		;next extension block
		move.l	d0,file_extension(a1)
		add.l	#72*512,file_extpos(a1)
		bra.b	.recheck

.inthisext	move.l	file_seek(a1),d4
		sub.l	file_extpos(a1),d4
		moveq	#9,d1
		lsr.l	d1,d4			;div 512
		neg.w	d4
		add.w	#71,d4
		lea.l	extension,a0
		lsl.w	#2,d4
		move.l	6*4(a0,d4.w),d0		;first data block
		lsr.w	#2,d4
		move.l	file_seek(a1),d1
		and.l	#$1ff,d1		;get offset in first block
		lea.l	secbuf,a0
		bsr	read_fbuffer		;read first data block
		move.l	#512,d5
		sub.l	d1,d5			;nb bytes in first block
		cmp.l	d3,d5
		bge.w	.lastwrite		;all bytes to write in this block ?

		lea.l	secbuf,a0
		add.l	d1,a0
		sub.l	d5,d3
		add.l	d5,file_seek(a1)
		subq.w	#1,d5
.copyf		move.b	(a4)+,(a0)+		;copy bytes to 1st block
		dbf	d5,.copyf

		lea.l	secbuf,a0
		bsr	write_fbuffer		;write back first block

.nextdata	subq.w	#1,d4			;next block
		bpl.b	.okextension		;still in this extension ?
		lea.l	extension,a0
		move.l	126*4(a0),d0		;next extension
		move.l	d0,file_extension(a1)
		bsr	read_fbuffer		;read next extension block
		bsr	test_sum
		bne.b	.endwrite
		moveq	#71,d4
		add.l	#72*512,file_extpos(a1)

.okextension	lea.l	extension,a0
		lsl.w	#2,d4
		move.l	6*4(a0,d4.w),d0		;next data block
		lsr.w	#2,d4
		cmp.l	#512,d3			;nb bytes to write > 1 block ?
		blt.b	.writelast

		move.l	a4,a0
		bsr	write_fbuffer		;write data block
		lea.l	512(a4),a4
		sub.l	#512,d3
		add.l	#512,file_seek(a1)
		bne.b	.nextdata
		bra.b	.endwrite		;no more bytes to write

.writelast	lea.l	secbuf,a0
		bsr	read_fbuffer
		moveq	#0,d1			;offset in block=0

.lastwrite	lea.l	secbuf,a0
		add.l	d1,a0
		add.l	d3,file_seek(a1)
		subq.w	#1,d3
.copy		move.b	(a4)+,(a0)+
		dbf	d3,.copy

		lea.l	secbuf,a0
		bsr	write_fbuffer		;write back last block

.endwrite
		movem.l	(a7)+,d0-a4
		rts

;-> a1=ptr on file_handle
;-> d0=nb bytes to append
;-> d2=ptr on buffer
;-> a2=device
;-> a3=part

append		movem.l	d0-a4,-(a7)
		move.l	d2,a4
		move.l	d0,d3

.recheck	move.l	file_extension(a1),d0
		lea.l	extension,a0
		bsr	read_fbuffer		;read extension block
		bsr	test_sum
		bne.w	.error
		move.l	file_seek(a1),d0
		sub.l	file_extpos(a1),d0
		cmp.l	#72*512,d0
		blt.w	.inthisext
		move.l	126*4(a0),d0		;next extension block
		bne.b	.okhere			;exists ?

		bsr	alloc_block		;alloc new extension block
		bpl.b	.okalloc
		bsr	delete_fh
		move.w	#DISKFULL_ERR,drive_err
		bra.w	.error
.okalloc	lea.l	extension,a0
		move.l	d0,126*4(a0)		;put in extension block list
		move.l	d0,-(a7)
		bsr	calc_sum
		move.l	file_extension(a1),d0
		bsr	write_fbuffer		;write back extension block
		move.l	(a7)+,d0		;restore new extension block no
		lea.l	extension,a0
		moveq	#128-1,d1
.clrex		clr.l	(a0)+			;clear new extension block
		dbf	d1,.clrex
		lea.l	extension,a0
		move.l	#$10,(a0)			;T.LIST
		move.l	d0,1*4(a0)			;header key
		move.l	file_header(a1),125*4(a0)	;parent
		move.l	#-3,127*4(a0)			;sec.type
		bsr	calc_sum
		bsr	write_fbuffer		;write back new extension block

.okhere		move.l	d0,file_extension(a1)
		add.l	#72*512,file_extpos(a1)
		bra.w	.recheck

.inthisext	move.l	file_seek(a1),d4
		sub.l	file_extpos(a1),d4
		move.l	d4,d0
		moveq	#9,d1
		lsr.l	d1,d4			;div 512
		neg.w	d4
		add.w	#71,d4
		lea.l	extension,a0
		and.w	#$1ff,d0		;need to alloc a new block ?
		beq.w	.aligned
		lsl.w	#2,d4
		move.l	6*4(a0,d4.w),d0
		lsr.w	#2,d4
		lea.l	secbuf,a0
		bsr	read_fbuffer
		move.l	file_seek(a1),d1
		and.l	#$1ff,d1
		add.l	d1,a0
		neg.l	d1
		add.l	#512,d1			;nb bytes left in this block
		cmp.l	d3,d1
		ble.b	.noall
		move.l	d3,d1
.noall		add.l	d1,file_size(a1)
		add.l	d1,file_seek(a1)
		sub.l	d1,d3
		bra.b	.godbf
.copyb		move.b	(a4)+,(a0)+		;copy data in last block
.godbf		dbf	d1,.copyb
		lea.l	secbuf,a0
		bsr	write_fbuffer		;write back last block
		tst.l	d3
		beq.w	.endwrite

.nextwrite	subq.w	#1,d4
		bpl.w	.aligned
		bsr	alloc_block		;get new extension block
		bpl.b	.okalloc3
		lea.l	extension,a0
		move.l	file_extension(a1),d0
		bsr	calc_sum
		bsr	write_fbuffer		;write back last extension
		bsr	delete_fh
		move.w	#DISKFULL_ERR,drive_err
		bra.w	.error
.okalloc3	lea.l	extension,a0
		move.l	d0,126*4(a0)		;put in extension list
		move.l	d0,-(a7)
		bsr	calc_sum
		move.l	file_extension(a1),d0
		bsr	write_fbuffer		;write back old extension
		move.l	(a7)+,d0
		lea.l	extension,a0
		moveq	#128-1,d1
.clrex2		clr.l	(a0)+			;clear new extension block
		dbf	d1,.clrex2
		lea.l	extension,a0
		move.l	#$10,(a0)			;T.LIST
		move.l	d0,1*4(a0)			;header key
		move.l	file_header(a1),125*4(a0)	;parent
		move.l	#-3,127*4(a0)			;sec.type
		move.l	d0,file_extension(a1)
		add.l	#72*512,file_extpos(a1)
		bsr	calc_sum
		bsr	write_fbuffer		;write back new extension block
		moveq	#71,d4

.aligned	bsr	alloc_block		;get new data block
		bpl.b	.okalloc2
		lea.l	extension,a0
		move.l	file_extension(a1),d0
		bsr	calc_sum
		bsr	write_fbuffer		;write back last extension
		bsr	delete_fh
		move.w	#DISKFULL_ERR,drive_err
		bra.w	.error
.okalloc2	lea.l	extension,a0
		addq.l	#1,2*4(a0)		;1 block de plus dans la list
		lsl.w	#2,d4
		move.l	d0,6*4(a0,d4.w)
		lsr.w	#2,d4
		move.l	a4,a0
		cmp.l	#512,d3
		ble.b	.slow
		bsr	write_fbuffer		;write new data block
		bra.b	.okfast
.slow		bsr	write_fbuffer		;write new data block
.okfast
		move.l	#512,d0
		cmp.l	d0,d3
		ble.b	.last
		add.l	d0,a4
		sub.l	d0,d3			;reduce nb bytes left to write
		add.l	d0,file_seek(a1)
		add.l	d0,file_size(a1)
		bra.w	.nextwrite

.last		add.l	d3,file_size(a1)
		add.l	d3,file_seek(a1)

		lea.l	extension,a0
		move.l	file_extension(a1),d0
		bsr	calc_sum
		bsr	write_fbuffer		;write back last extension

.endwrite
.error
		movem.l	(a7)+,d0-a4
		rts

;----------------------------------------------------------

;-> d1=ptr on file_handle
;-> d2=seek offset
;-> d3=mode -1=beginning, 0=current, 1=end

seek_file	movem.l	d1-d7/a0-a4,-(a7)
		move.l	d1,a1
		tst.l	d3
		bpl.b	.nobegin

;-------------- beginning --------

		cmp.l	file_seek(a1),d2
		bge.b	.noback
		move.l	file_header(a1),file_extension(a1)
		clr.l	file_extpos(a1)
		move.l	d2,file_seek(a1)
		bra.b	.end
.noback		move.l	d2,file_seek(a1)
		bra.b	.end

;-------------- current ----------

.nobegin	tst.l	d3
		bne.b	.nocurrent
		tst.l	d2
		bpl.b	.okpos
		move.l	file_header(a1),file_extension(a1)
		clr.l	file_extpos(a1)
.okpos		add.l	d2,file_seek(a1)
		bra.b	.end

;-------------- end --------------

.nocurrent	move.l	file_size(a1),d1
		sub.l	d2,d1
		cmp.l	file_seek(a1),d1
		bge.b	.noback2
		move.l	file_header(a1),file_extension(a1)
		clr.l	file_extpos(a1)
.noback2	move.l	d1,file_seek(a1)

.end		move.l	file_seek(a1),d0
		cmp.l	file_size(a1),d0	;bigger than max val ?
		ble.b	.okmax
		move.l	file_size(a1),file_seek(a1)
.okmax		tst.l	file_seek(a1)		;negative ?
		bpl.b	.okmin
		clr.l	file_seek(a1)
.okmin
		movem.l	(a7)+,d1-d7/a0-a4
		rts

;-----------------------------------------------------

;-> d1=ptr on dirname (ended by a zero)

create_dir	movem.l	d0-d7/a0-a4,-(a7)
		st	disk_op
		move.l	d1,d6			;copy of full filename* in d6
		lea.l	file_handle1,a1
		tst.l	(a1)
		beq.b	.okfree
		lea.l	file_handle2,a1
		tst.l	(a1)
		beq.b	.okfree
		moveq	#0,d0
		bra.w	.error

.okfree		bsr	find_partition
		tst.l	d0
		beq.w	.error
		move.l	d0,a3
		move.l	d0,file_part(a1)
		move.l	d2,file_parent(a1)
		clr.l	file_seek(a1)
		clr.l	file_extpos(a1)
		sf	file_written(a1)

		bsr	copy_name		;set BCPL name into filehandle

		move.l	part_device(a3),a2
		lea.l	extension,a0
		move.l	file_parent(a1),d0
		move.l	d0,d3
		bsr	read_fbuffer		;read ROOT or DIR
		bsr	test_sum
		bne.w	.error

		clr.l	file_size(a1)

		bsr	alloc_block		;find block for dir_header
		bpl.b	.okhead
		move.w	#DISKFULL_ERR,drive_err
		moveq	#0,d0
		bra.w	.error
.okhead		move.l	d0,file_header(a1)
		move.l	d0,file_extension(a1)
		bsr	calc_hash
		move.l	d3,d6			;d6=place where to ins. in hash
		sf	d5			;signal for d6 found
.next2		lea.l	extension,a0
		move.l	(a0,d0.w),d0		;find end of hash list
		beq.b	.okfreeh
		tst.b	d5			;already found place where
		bne.b	.foundins		;to insert in hash list ?
		cmp.l	file_header(a1),d0
		bls.b	.foundins
		st	d5
		move.l	d3,d6			;d6=place where to ins. in hash
.foundins	move.l	d0,d3			;d3=no of actual loaded block
		bsr	read_fbuffer		;read next in hash list
		bsr	test_sum
		beq.b	.oksum2
		move.l	file_header(a1),d0
		bsr	free_block
		bsr	update_bitmap
		moveq	#0,d0
		bra.w	.error
.oksum2		lea.l	108*4(a0),a0
		bsr	cmp_filename		;file already exists ?
		bne.b	.different
		move.l	file_header(a1),d0
		bsr	free_block
		bsr	update_bitmap
		move.w	#FILEEXIST_ERR,drive_err
		moveq	#0,d0
		bra.w	.error
.different	move.w	#124*4,d0
		bra.b	.next2
.okfreeh	tst.b	d5
		beq.b	.loaded
		cmp.l	d3,d6			;block already loaded ?
		beq.b	.loaded
		move.l	d6,d0
		move.l	d0,d3
		bsr	read_fbuffer
.loaded		cmp.l	#1,127*4(a0)		;ROOT ?
		beq.b	.puthash
		move.l	1*4(a0),d0
		cmp.l	file_parent(a1),d0	;is it the PARENT
		bne.b	.putlist
.puthash	bsr	calc_hash
		move.l	(a0,d0.w),d6		;next in hash list
		move.l	file_header(a1),(a0,d0.w)
		bra.b	.savesec
.putlist	move.l	124*4(a0),d6		;next in hash list
		move.l	file_header(a1),124*4(a0)
.savesec	lea.l	extension,a0
		bsr	calc_sum
		move.l	d3,d0
		bsr	write_fbuffer		;write back the block
						;containing the ref to the dir

;-------------- init the dir block --

		lea.l	secbuf,a4
		moveq	#128-1,d0
.clr		clr.l	(a4)+			;clear whole block
		dbf	d0,.clr
		lea.l	secbuf,a0
		move.l	#2,(a0)			;Type 2 (T.SHORT)
		move.l	file_header(a1),1*4(a0)	;Header Key
		lea.l	108*4(a0),a4
		lea.l	file_name(a1),a0
		moveq	#8-1,d0
.copy_name	move.l	(a0)+,(a4)+		;copy dirname
		dbf	d0,.copy_name
		lea.l	secbuf,a0
		move.l	file_parent(a1),125*4(a0)	;parent
		move.l	#2,127*4(a0)		;sec.type (dir)
		move.l	d6,124*4(a0)		;next in hash list

		bsr	calc_sum
		move.l	file_header(a1),d0
		bsr	write_fbuffer		;write new dir block

		bsr	update_bitmap

		bsr	flush_fbuffer
		jsr	UPDATE_CMD(a2)
		jsr	MOTOFF_CMD(a2)

.error		clr.l	(a1)			;free file_handle
		movem.l	(a7)+,d0-d7/a0-a4
		rts

;----------------------------------------------------------
;-------------- change current directory (CD) -------------

;-> d1=ptr on new current path name

change_dir	movem.l	d0-d7/a0-a4,-(a7)
		st	disk_op

		move.l	d1,a0

.seekend	tst.b	(a0)+
		bne.b	.seekend
		subq.l	#2,a0			;go on last char
		cmp.l	d1,a0
		blt.b	.empty
		cmp.b	#'/',(a0)+
		beq.b	.already
		cmp.b	#':',-1(a0)		;device ?
		beq.b	.already
		move.b	#'/',(a0)+
.already	move.b	#'a',(a0)+	;dummy filename for find partition
		sf	(a0)+
.empty
		move.l	d1,d6			;copy of full filename* in d6
		lea.l	file_handle1,a1
		tst.l	(a1)
		beq.b	.okfree
		lea.l	file_handle2,a1
		tst.l	(a1)
		beq.b	.okfree
		moveq	#0,d0
		bra.w	.error

.okfree		bsr	find_partition
		tst.l	d0
		beq.b	.error
		movem.l	d0/d2,current_dir

		bsr	flush_fbuffer
		move.l	d0,a3
		move.l	part_device(a3),a2
		jsr	MOTOFF_CMD(a2)

.error		movem.l	(a7)+,d0-d7/a0-a4
		rts

;----------------------------------------------------------
;-------------- Print path --------------------------------

;-> d0=block no of actual dir
;-> a3=partition

print_path	movem.l	d0/a0-a4,-(a7)
		st	disk_op
		move.l	part_device(a3),a2
		lea.l	general_txt+80,a4
.loop		lea.l	secbuf,a0
		bsr	read_fbuffer
		cmp.l	#1,127*4(a0)		;ROOT ?
		beq.b	.root
		move.b	#'/',-(a4)
		bsr.b	.check_len
		bne.b	.out
		bsr	.put_name
		bsr.b	.check_len
		bne.b	.out
		move.l	125*4(a0),d0		;get parent
		bne.b	.loop
		bra.b	.err

.root		move.b	#':',-(a4)
		bsr.b	.check_len
		bne.b	.out
		lea.l	part_name(a3),a0
		bsr	.put_name2

.out		move.l	a4,a0
		bsr	print

.err		movem.l	(a7)+,d0/a0-a4
		rts


;put BCPL name from secbuf to -(a4)

.put_name	move.l	a0,-(a7)
		lea.l	secbuf+108*4,a0
		bsr	.put_name2
		move.l	(a7)+,a0
		rts

;put BCPL name from a0 to -(a4)

.put_name2	movem.l	d0/a0,-(a7)
		moveq	#0,d0
		move.b	(a0)+,d0		;name length
		add.l	d0,a0
		bra.b	.godbf
.copy		move.b	-(a0),-(a4)
		bsr.b	.check_len
		bne.b	.outn
.godbf		dbf	d0,.copy
.outn		movem.l	(a7)+,d0/a0
		rts

;-> a4
;<- FLAGS 0=len ok -1=full
.check_len	movem.l	d0/a4,-(a7)
		cmp.l	#general_txt+2,a4
		bne.b	.nofull
		move.l	#'....',(a4)
		moveq	#-1,d0
		bra.b	.outl
.nofull		moveq	#0,d0
.outl		movem.l	(a7)+,d0/a4
		rts

;----------------------------------------------------------
;-------------- Get Free Blocks  --------------------------

	IFEQ 1	; unused

;-> a3=ptr on partition

get_free_blocks
		st	disk_op
		move.l	part_nbsec(a3),d0
		lsr.l	#1,d0			;ROOT block no
		lea.l	secbuf,a0
		bsr	read_fbuffer		;read ROOT block
		bsr	test_sum
		beq.b	.oksum
		moveq	#0,d0
		bra.w	.error
.oksum
.error
		rts

	ENDC

;----------------------------------------------------------
;-------------- ExNext ------------------------------------

;-> d1=ptr on dir path to list or previous examined file_handle
;<- d0=file_handle on next file 0=no next -1=error
;<- d1=ptr on partition of dir (returned only on first call)
;<- d2=block no of parent (returned only on first call)

ExNext
		st	disk_op
		movem.l	d1-a4,-(a7)
		cmp.l	#file_handle1,d1
		beq.b	.next
		cmp.l	#file_handle2,d1
		bne.b	.first
.next
		move.l	d1,a1
		move.l	file_part(a1),a3
		move.l	part_device(a3),a2
		lea.l	secbuf,a0
		move.l	file_header(a1),d0
		bsr	read_fbuffer		;read previous file header
		bsr	test_sum
		bne.w	.error2
		move.l	124*4(a0),d0		;next in hash list
		bne.w	.found
		bsr	calc_hash
		lea.l	extension,a0		;dir block here
		lea.l	4(a0,d0.w),a0		;next in hash table
.seek2		cmp.l	#extension+78*4,a0	;end of hash table ?
		beq.w	.enddir
		move.l	(a0)+,d0
		beq.b	.seek2
		bra.w	.found

;-------------------------------------------

.first		move.l	d1,a0			;path name
.seekend	tst.b	(a0)+
		bne.b	.seekend
		subq.l	#2,a0			;go on last char
		cmp.l	d1,a0
		bge.b	.noempty
		move.l	d1,a0
		bra.b	.already
.noempty	cmp.b	#'/',(a0)+
		beq.b	.already
		cmp.b	#':',-1(a0)		;device ?
		beq.b	.already
		move.b	#'/',(a0)+
.already	move.b	#'a',(a0)+	;dummy filename for find partition
		sf	(a0)+

		lea.l	file_handle1,a1
		tst.l	(a1)
		beq.b	.okfree
		lea.l	file_handle2,a1
		tst.l	(a1)
		beq.b	.okfree
		moveq	#0,d0
		bra.w	.error

.okfree		bsr	find_partition
		tst.l	d0
		beq.w	.error
		move.l	d0,(a1)
		move.l	d0,(a7)			;return partition in d1
		move.l	d2,4(a7)		;return parent block no in d2
		move.l	d0,a3
		move.l	part_device(a3),a2
		move.l	d2,file_parent(a1)
		move.l	file_parent(a1),d0
		lea.l	extension,a0
		bsr	read_fbuffer		;read dir block
		bsr	test_sum
		bne.b	.error2
		lea.l	6*4(a0),a4		;start of hashtable
		moveq	#72-1,d1		;hashtable size
.seekfirst	move.l	(a4)+,d0		;find first file
		bne.b	.found
		dbf	d1,.seekfirst
.enddir		clr.l	(a1)
		moveq	#0,d0			;end of dir
		bra.b	.endx

.found		move.l	d0,file_header(a1)
		lea.l	secbuf,a0
		bsr	read_fbuffer		;read first file header
		bsr	test_sum
		bne.b	.error2
		move.l	81*4(a0),file_size(a1)
		lea.l	file_name(a1),a4
		cmp.l	#-3,127*4(a0)	;is it a file_header ? or a dir ?
		sne	file_dir(a1)
		lea.l	108*4(a0),a0
		moveq	#8-1,d0
.copyn		move.l	(a0)+,(a4)+		;copy BCPL filename
		dbf	d0,.copyn

		move.l	a1,d0
		bra.b	.endx

.error2		clr.l	(a1)			;free file_handle
.error		moveq	#-1,d0
		bra.b	.out

.endx		tst.w	drive_err
		bne.b	.error2

.out		movem.l	(a7)+,d1-a4
		rts

;----------------------------------------------------------

;<- d1=ptr on filename to delete (or empty directory)

delete_file	movem.l	d0-d2/a1,-(a7)
		moveq	#1,d2			;mode old file or old dir
		bsr	open_file
		tst.l	d0
		beq.b	.err
		move.l	d0,a1
		bsr	delete_fh
		clr.l	(a1)			;free file_handle
.err		movem.l	(a7)+,d0-d2/a1
		rts


;<- a1=file_handle

delete_fh	movem.l	d0-a4,-(a7)
		st	disk_op
		move.l	file_part(a1),a3
		move.l	part_device(a3),a2
		move.l	file_parent(a1),d0
		move.l	d0,d3
		lea.l	extension,a0
		bsr	read_fbuffer		;read PARENT dir
		bsr	test_sum
		bne.w	.error

		move.l	file_header(a1),d1

		bsr	calc_hash
.next		cmp.l	(a0,d0.w),d1
		beq.b	.found
		move.l	(a0,d0.w),d0
		beq.w	.error			;should never happen
		move.l	d0,d3
		bsr	read_fbuffer
		bsr	test_sum
		bne.w	.error
		move.w	#124*4,d0
		bra.b	.next
.found		move.w	d0,d2
		move.l	d1,d0
		lea.l	secbuf,a0
		bsr	read_fbuffer		;read file header
		bsr	test_sum
		bne.w	.error

		cmp.l	#1,127*4(a0)
		bne.b	.noroot
.notempty	move.w	#NOTEMPTY_ERR,drive_err
		bra.w	.error
.noroot		cmp.l	#2,127*4(a0)
		bne.b	.nodir
		lea.l	6*4(a0),a0
		moveq	#72-1,d0
.check		tst.l	(a0)+			;dir is empty ?
		bne.b	.notempty
		dbf	d0,.check
		lea.l	secbuf,a0
.nodir
		move.l	124*4(a0),d0		;next in hash chain
		lea.l	extension,a0
		move.l	d0,(a0,d2.w)
		move.l	d3,d0
		bsr	calc_sum
		bsr	write_fbuffer	;write back block before file_header

		lea.l	extension,a0
		move.l	file_header(a1),d0
		bsr	read_fbuffer		;file_header in extension
		bsr	test_sum
		bne.b	.error

		move.l	file_header(a1),d0
		bsr	free_block		;free file_header

.next_ext	lea.l	extension,a0
		move.l	2*4(a0),d1		;nb blocks
		lea.l	78*4(a0),a4
		bra.b	.godbf
.loop		move.l	-(a4),d0
		bsr	free_block
.godbf		dbf	d1,.loop
		move.l	126*4(a0),d0		;next extension ?
		beq.b	.end
		bsr	read_fbuffer		;read next extension
		bsr	test_sum
		bne.b	.error
		bsr	free_block		;free extension block
		bra.b	.next_ext

.end
.error		bsr	update_bitmap

		bsr	flush_fbuffer

		movem.l	(a7)+,d0-a4
		rts


;----------------------------------------------------------

;-> d1=filename
;<- d0=ptr on partition structure (0=error)
;<- d1=ptr on filename (without path)
;<- d2=dir key where the file is

find_partition	movem.l	d3-d7/a0-a4,-(a7)
		st	disk_op
		move.l	d1,d7

		move.l	current_dir,a3		;default partition
		move.l	current_dir+4,d6	;default dir

		lea.l	part_name(a3),a0
		lea.l	part_nametemp,a1
		moveq	#8-1,d0			;copy partition name
.copyn		move.l	(a0)+,(a1)+		;in case there isn't a device:
		dbf	d0,.copyn		;in the path

		move.l	d7,a0
.seek		tst.b	(a0)
		beq.w	.endofname
		cmp.b	#':',(a0)+		;seek device name (ex. HD0: )
		bne.b	.seek
		subq.l	#1,a0
		sub.l	d7,a0
		move.l	a0,d0
		cmp.l	#30,d0
		ble.b	.oksize
		moveq	#30,d0
.oksize		lea.l	part_nametemp,a1

		move.l	a1,a0
		moveq	#8-1,d1
.clr		clr.l	(a0)+			;clear part_nametemp
		dbf	d1,.clr

		move.b	d0,(a1)+		;write len of name
		move.l	d7,a0
		move.w	d0,d1
		bra.b	.godbf
.copyname	move.b	(a0)+,d0
		bsr	upper_case
		move.b	d0,(a1)+		;copy part_name
.godbf		dbf	d1,.copyname
		addq.l	#1,a0			;skip ':' char
		move.l	a0,d7

		lea.l	part_nametemp,a0

		lea.l	floppy0,a3		;find partition structure

.seek2		lea.l	part_name(a3),a1
		move.l	a0,a4
		moveq	#8-1,d0
.cmpname	cmpm.l	(a1)+,(a4)+
		bne.b	.notsame
		dbf	d0,.cmpname
		move.l	part_nbsec(a3),d6
		lsr.l	#1,d6			;ROOT block is default dir
		bra.b	.endofname
.notsame	move.l	part_next(a3),d0
		beq.b	.err
		move.l	d0,a3
		bra.b	.seek2
.err		move.w	#DEVICENOTFOUND_ERR,drive_err
		moveq	#0,d0
		bra.w	.end

;-------------- find directory -------------

.endofname	bsr	test_change
		tst.w	drive_err		;test if an error occured
		bne.b	.exit
		cmp.l	#$444f5301,part_filesystem(a3)	;DOS\1 FFS ?
		beq.b	.okFFS
		cmp.l	#$444f5303,part_filesystem(a3)	;DOS\3 FFS INTL ?
		beq.b	.okFFS
		move.w	#NOFFS_ERR,drive_err
.exit		moveq	#0,d0
		bra.w	.end
.okFFS		move.l	part_device(a3),a2
		move.l	d7,a0
.seekslash	tst.b	(a0)
		beq.w	.nomore
		cmp.b	#"/",(a0)+
		bne.b	.seekslash
		subq.l	#1,a0
		cmp.l	d7,a0
		bne.b	.noparent
		addq.l	#1,d7				;skip '/' char
		move.l	d6,d0
		lea.l	secbuf,a0
		bsr	read_fbuffer			;read actual dir
		bsr	test_sum
		bne.w	.error
		move.l	125*4(a0),d6			;parent
		bne.b	.okFFS				;found a parent ?
		move.w	#ILLEGALPATH_ERR,drive_err
		moveq	#0,d0
		bra.w	.end

.noparent	sub.l	d7,a0
		move.l	a0,d2				;dir_name len
		lea.l	part_nametemp,a1
		move.l	a1,a0
		moveq	#8-1,d1
.clr2		clr.l	(a1)+
		dbf	d1,.clr2
		move.b	d2,(a0)+			;set dir_name len
		move.l	d7,a1
		bra.b	.godbf2
.copydn		move.b	(a1)+,(a0)+			;copy dir_name
.godbf2		dbf	d2,.copydn
		move.l	a1,d7
		addq.l	#1,d7				;skip '/' char

		lea.l	secbuf,a0
		move.l	d6,d0
		bsr	read_fbuffer			;read actual dir
		bsr	test_sum
		bne.b	.error

		lea.l	part_nametemp,a0
		move.b	part_filesystem+3(a3),d0	;get DOSTYPE
		bsr	calc_hash2
.nextdir	lea.l	secbuf,a0
		move.l	(a0,d0.w),d6			;read hash ptr
		beq.b	.illegal
		move.l	d6,d0
		bsr	read_fbuffer			;read new dir
		bsr	test_sum
		bne.b	.error
		lea.l	part_nametemp,a0
		lea.l	secbuf+108*4,a1
		bsr	cmp_filename2
		beq.b	.okdirname
		move.w	#124*4,d0
		bra.b	.nextdir
.okdirname	cmp.l	#2,secbuf+127*4			;is it a dir ?
		beq.w	.okFFS
.illegal	move.w	#ILLEGALPATH_ERR,drive_err
.error		moveq	#0,d0				;not a dir, then error
		bra.b	.end

.nomore		move.l	d7,a0
		tst.b	(a0)				;empty filename ?
		bne.b	.okfilename
		move.w	#FILEEXIST_ERR,drive_err
		moveq	#0,d0
		bra.b	.end
.okfilename	move.l	a3,d0
		move.l	d7,d1
		move.l	d6,d2

.end		movem.l	(a7)+,d3-d7/a0-a4
		rts

;----------------------------------------------------------
;-------------- test if disk was changed and reinit -------
;-------------- partition structure -----------------------

;-> a3=ptr on partition

test_change	movem.l	d0/a0-a3,-(a7)
		st	disk_op
		move.l	part_device(a3),a2
		moveq	#0,d0			;test if changed cmd
		jsr	CHANGE_CMD(a2)
		tst.l	d0
		beq.b	.nochange
		bpl.b	.newdisk
		clr.l	part_filesystem(a3)	;no disk
		bra.b	.end
.newdisk	lea.l	secbuf,a0
		clr.l	(a0)
		moveq	#0,d0
		jsr	READ_CMD(a2)		;read boot-block
		move.l	secbuf,part_filesystem(a3)
		bra.b	.end
.nochange	tst.l	part_filesystem(a3)
		beq.b	.newdisk
.end		movem.l	(a7)+,d0/a0-a3
		rts

;----------------------------------------------------------
;-------------- copy name into file handle ----------------
;-------------- convert it into BCPL ----------------------

;-> d1=ptr on name ended by zero
;-> a1=file_handle

copy_name	move.l	a2,-(a7)
		lea.l	file_name(a1),a2
		rept 8
		clr.l	(a2)+
		endr
		lea.l	file_name(a1),a2
		bsr	copy_name2
		move.l	(a7)+,a2
		rts

;-> d1=ptr on name ended by zero
;-> a2=dest address

copy_name2	movem.l	d0-d2/a0-a3,-(a7)
		lea.l	1(a2),a3
		move.l	d1,a0
		moveq	#30-1,d0		;maxlen of filename
		moveq	#-1,d2
.copy		addq.w	#1,d2
		move.b	(a0)+,(a3)+
		dbeq	d0,.copy
		beq.b	.nomax
		moveq	#30,d2
.nomax		move.b	d2,(a2)			;write filename len
		movem.l	(a7)+,d0-d2/a0-a3
		rts

;----------------------------------------------------------

;-> a1=file_handle
;<- d0=hash val *4

calc_hash	movem.l	a0,-(a7)
		move.l	file_part(a1),a0
		move.b	part_filesystem+3(a0),d0	;get DOSTYPE
		lea.l	file_name(a1),a0
		bsr.b	calc_hash2
		movem.l	(a7)+,a0
		rts

;-> a0=ptr on BCPL name
;-> d0=DOSTYPE 1=FFS, 3=INTL FFS, 5=DCFS
;<- d0=hash val *4

calc_hash2	movem.l	d1-d3/a0-a1,-(a7)
		move.b	d0,d3			;d3=DOSTYPE
		moveq	#0,d0
		move.b	(a0)+,d0		;read len of name

		move.w	d0,d2
		subq.w	#1,d2
		move.w	d0,d1
		moveq	#0,d0
.loop2		mulu	#13,d1
		move.b	(a0)+,d0
		cmp.b	#3,d3			;check DOSTYPE
		blt.b	.nointl
		cmp.b	#$f7,d0
		beq.b	.okintl
		bsr.b	intl_conv
		bra.b	.okintl
.nointl		bsr	upper_case
.okintl		add.w	d0,d1
		and.w	#$7ff,d1
		dbf	d2,.loop2

		divu	#72,d1
		swap	d1
		addq.w	#6,d1
		move.w	d1,d0
		lsl.w	#2,d0
		movem.l	(a7)+,d1-d3/a0-a1
		rts

;-> d0=ascII char
intl_conv	movem.l	d1-d2,-(a7)
		move.b d0,d2
		move.l #"~`za",d1	;$7e607a61
		bclr #7,d0
		beq.b .hashcv1
		swap d1
.hashcv1	cmp.b d1,d0
		blt.b .hashcv2
		lsr.l #8,d1
		cmp.b d1,d0
		bgt.b .hashcv2
		bclr #5,d2
.hashcv2	move.b d2,d0
		movem.l	(a7)+,d1-d2
		rts

;----------------------------------------------------------

;-> a1=file_handle
;-> a0=ptr on BCPL name to compare
;<- FLAGS =result 0=same -1=different

cmp_filename	movem.l	a1,-(a7)
		lea.l	file_name(a1),a1
		bsr.b	cmp_filename2
		movem.l	(a7)+,a1
		rts

;-> a1=ptr on BCPL name1 to compare
;-> a0=ptr on BCPL name2 to compare
;<- FLAGS =result 0=same -1=different

cmp_filename2	movem.l	d0-d2/a0-a1,-(a7)
		move.b	(a1)+,d0
		cmp.b	(a0)+,d0
		bne.b	.err
		moveq	#0,d1
		move.b	d0,d1
		bra.b	.godbf
.loop		move.b	(a1)+,d0
		bsr	upper_case
		move.b	d0,d2
		move.b	(a0)+,d0
		bsr	upper_case
		cmp.b	d0,d2
		bne.b	.err
.godbf		dbf	d1,.loop
		moveq	#0,d0
		bra.b	.ok
.err		moveq	#-1,d0
.ok		movem.l	(a7)+,d0-d2/a0-a1
		rts

;---------------------------------------------------------------
;-------------- find a free block in the bitmap and set it -----

;-> a1=ptr on file_handle
;<- d0=free block no. -1=error

alloc_block	movem.l	d1-d7/a0-a4,-(a7)
		move.l	file_part(a1),a3
		move.l	part_device(a3),a2

		cmp.l	bitmap_part,a3
		beq.w	.okloaded

.search		bsr	update_bitmap
		move.l	a3,bitmap_part
		move.l	part_nbsec(a3),d0
		lsr.l	#1,d0			;ROOT block
		lea.l	secbuf,a0
		bsr	read_fbuffer		;read ROOT block

		move.w	#79*4,d4		;start of bitmap_block table
		move.w	#104*4,d5		;end of table
		move.l	#2,bitmap_offset

.next		bsr	update_bitmap
		lea.l	secbuf,a0
		move.l	(a0,d4.w),d0		;get bitmap block
		move.l	d0,bitmap_no
		lea.l	bitmap,a0
		bsr	read_fbuffer		;read bitmap block

		bsr	alloc_block2
		tst.l	d0
		bmi.b	.diskfull
		bne.b	.end
		add.l	#127*32,bitmap_offset
		addq.w	#4,d4			;next block in bitmap list
		cmp.w	d5,d4			;max val reached ?
		bne.b	.next
		lea.l	secbuf,a0
		move.l	(a0,d5.w),d0		;get next bitmap list no
		bsr	read_fbuffer
		move.w	#127*4,d5
		moveq	#0,d4
		bra.b	.next


.okloaded	bsr	alloc_block2
		tst.l	d0			;found a free block ?
		bmi.b	.diskfull
		beq.w	.search
.end
.diskfull	tst.l	d0
		movem.l	(a7)+,d1-d7/a0-a4
		rts

;-------------- find a free block in the actual bitmap block --------
;-------------- (called by alloc_block only) ------------------------

;-> a3=ptr on partition
;<- d0=no of free block, 0=no free block in bitmap block, -1=disk full

alloc_block2	movem.l	d1-d3/a0,-(a7)
		lea.l	bitmap+4,a0
		move.l	bitmap_offset,d0
		move.l	part_nbsec(a3),d3	;max limit

.seek2		move.l	(a0)+,d1
		bne.b	.deeper
		moveq	#32,d1
		add.l	d1,d0
		cmp.l	d3,d0
		bge.b	.full
		bra.b	.skip
.deeper		moveq	#32-1,d2
.seek		lsr.l	#1,d1
		bcs.b	.found
		addq.l	#1,d0
		cmp.l	d3,d0
		bge.b	.full
		dbf	d2,.seek
.skip		cmp.l	#bitmap+512,a0
		bne.b	.seek2
		moveq	#0,d0			;end of block reached
		bra.b	.end

.full		moveq	#-1,d0
		bra.b	.end

.found		move.l	d0,d1
		sub.l	bitmap_offset,d1
		move.l	d1,d2
		lsr.l	#5,d2
		lea.l	bitmap+4,a0
		lsl.l	#2,d2
		move.l	(a0,d2.l),d3
		and.l	#$1f,d1
		bclr	d1,d3			;alloc block in bitmap
		move.l	d3,(a0,d2.l)
		st	bitmap_modified
		tst.l	d0

.end		movem.l	(a7)+,d1-d3/a0
		rts

;---------------------------------------------------------------
;-------------- free a block in the bitmap ---------------------

;-> d0=no of block to free
;-> a1=ptr on file_handle

free_block	movem.l	d0-a4,-(a7)
		move.l	file_part(a1),a3
		move.l	part_device(a3),a2
		move.l	d0,d7

		cmp.l	bitmap_part,a3
		bne.b	.load

		move.l	d0,d7
		move.l	bitmap_offset,d0
		cmp.l	d0,d7			;is this block in the
		blt.b	.load			;actual bitmap block
		add.l	#127*32,d0
		cmp.l	d0,d7
		bge.b	.load

.free		move.l	d7,d1
		sub.l	bitmap_offset,d1
		move.l	d1,d2
		lsr.l	#5,d2
		lea.l	bitmap+4,a0
		lsl.l	#2,d2
		move.l	(a0,d2.l),d3
		and.l	#$1f,d1
		bset	d1,d3			;free block in bitmap
		move.l	d3,(a0,d2.l)
		bra.w	.end

.load		bsr	update_bitmap
		move.l	d7,d6
		divu	#127*32,d6		;d6=no of block to search
		ext.l	d6

		move.l	a3,bitmap_part
		move.l	part_nbsec(a3),d0
		lsr.l	#1,d0			;ROOT block
		lea.l	secbuf,a0
		bsr	read_fbuffer		;read ROOT block
		bsr	test_sum
		bne.b	.error

		move.w	#79*4,d4		;start of bitmap_block table
		move.w	#104*4,d5		;end of table
		move.l	#2,bitmap_offset

.nomax		tst.l	d6
		beq.b	.found
		add.l	#32*127,bitmap_offset
		subq.l	#1,d6
		addq.w	#4,d4
		cmp.w	d4,d5
		bne.b	.nomax
		lea.l	secbuf,a0
		move.l	(a0,d4.w),d0
		bsr	read_fbuffer		;read the bitmap block table
		moveq	#0,d4
		move.w	#127*4,d5
		bra.b	.nomax

.found		lea.l	secbuf,a0
		move.l	(a0,d4.w),d0
		move.l	d0,bitmap_no
		lea.l	bitmap,a0
		bsr	read_fbuffer		;read the bitmap block
		bsr	test_sum
		bne.b	.error
		bra.w	.free

.end		st	bitmap_modified

.error		movem.l	(a7)+,d0-a4
		rts

;----------------------------------------------------------
;-------------- write back the bitmap to disk -------------

update_bitmap	movem.l	d0/a0-a3,-(a7)
		tst.b	bitmap_modified
		beq.b	.nomodif
		sf	bitmap_modified
		move.l	bitmap_part,a3
		move.l	part_device(a3),a2
		lea.l	bitmap,a0
		bsr	calc_bitmapsum
		move.l	bitmap_no,d0
		bsr	write_fbuffer
.nomodif	movem.l	(a7)+,d0/a0-a3
		rts

;----------------------------------------------------------
;calculate a block checksum and write it in the block
;-> a0=ptr on block

calc_sum	movem.l	d0-d1/a0-a1,-(a7)
		move.l	a0,a1
		clr.l	5*4(a0)			;clear sum
		moveq	#0,d1
		moveq	#512/4-1,d0
.sum		add.l	(a0)+,d1
		dbf	d0,.sum
		neg.l	d1
		move.l	d1,5*4(a1)
		movem.l	(a7)+,d0-d1/a0-a1
		rts

;----------------------------------------------------------
;-------------- calcualte bitmap checksum and set it ------

;-> a0=ptr on bitmap block

calc_bitmapsum	movem.l	d0-d1/a0-a1,-(a7)
		move.l	a0,a1
		clr.l	(a0)			;clear sum
		moveq	#0,d1
		moveq	#512/4-1,d0
.sum		add.l	(a0)+,d1
		dbf	d0,.sum
		neg.l	d1
		move.l	d1,(a1)
		movem.l	(a7)+,d0-d1/a0-a1
		rts

;----------------------------------------------------------
;-------------- test the checksum of a block --------------

;-> a0=ptr on block to test
;<- FLAGS 0=sum ok -1=bad checksum

test_sum:	movem.l	d0-d1/a0,-(a7)
		moveq	#0,d0
		moveq	#512/16-1,d1
.sum		add.l	(a0)+,d0
		add.l	(a0)+,d0
		add.l	(a0)+,d0
		add.l	(a0)+,d0
		dbf	d1,.sum
		tst.l	d0
		beq.b	.ok
		move.w	#BADCHECKSUM_ERR,drive_err
		moveq	#-1,d0
.ok		movem.l	(a7)+,d0-d1/a0
		rts


		cnop 0,4

file_handle1	dc.l 0			;part
		dc.l 0			;parent
		dc.l 0			;header
		dc.l 0			;extension
		dc.l 0			;extension pos
		dc.l 0			;filesize
		dc.l 0			;seek pos
		dcb.b 32,0		;name
		dc.b 0			;written flag
		dc.b 0			;dir
		cnop 0,4

file_handle2	dc.l 0			;part
		dc.l 0			;parent
		dc.l 0			;header
		dc.l 0			;extension
		dc.l 0			;extension pos
		dc.l 0			;filesize
		dc.l 0			;seek pos
		dcb.b 32,0		;name
		dc.b 0			;written flag
		dc.b 0			;dir
		cnop 0,4

current_dir	dc.l floppy0		;ptr on partition
		dc.l 880		;ptr on dir key

part_nametemp	dcb.b 32,0

		cnop 0,4

extension	dcb.b 512,0		;buffers
secbuf		dcb.b 512,0		;used for
bitmap		dcb.b 512,0		;filesystem

bitmap_no	dc.l 0			;no of bitmap sector in bitmap buffer
bitmap_part	dc.l 0			;ptr on partition of bitmap buffer
bitmap_offset	dc.l 0			;no of the first block of the bitmap
bitmap_modified	dc.b 0			;0=no modif. -1=modified
		cnop 0,4

