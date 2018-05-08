#include "ti84pce.inc"
	.org userMem-2
	.db tExtTok,tAsm84CeCmp

#ifndef NDEBUG
	ld      a,(iy+mathprintFlags)
	res     mathprintEnabled,(iy+mathprintFlags)
	push    af
	call    _HomeUp
	call    _ClrLCDFull
#endif
	push    iy
	call	initMsd			; attempt to initialize msd
	jr	nc,initFailed
	call	fs_mount
initFailed:
	call    usbCleanup
	pop     iy
#ifndef NDEBUG
	pop     af
	ld      (iy+mathprintFlags),a
#endif
	ret

#include "debug.inc"
#include "host.inc"
#include "usb.asm"
#include "msd.asm"

#define ld_ehl_ix(xx) ld hl,(ix+xx) \ ld e,(ix+xx+3)
#define ld_ehl_iy(xx) ld hl,(iy+xx) \ ld e,(iy+xx+3)
#define ld_abc_ix(xx) ld bc,(ix+xx) \ ld a,(ix+xx+3)
#define ld_abc_iy(xx) ld bc,(iy+xx) \ ld a,(iy+xx+3)

#define ld_ix_ehl(xx) ld (ix+xx),hl \ ld (ix+xx+3),e
#define ld_iy_ehl(xx) ld (iy+xx),hl \ ld (iy+xx+3),e
#define ld_ix_abc(xx) ld (ix+xx),bc \ ld (ix+xx+3),a
#define ld_iy_abc(xx) ld (iy+xx),bc \ ld (iy+xx+3),a

; mount a logical drive
;  z = success
; nz = failure
fs_mount:
	xor	a,a
	sbc	hl,hl
	ld	(xferReadLba),hl
	ld	(xferReadLba+3),a		; zero lba
	call	scsiRequestDefaultReadSector	; read the mbr at sector 0
	call	fs_check
	cp	a,2				; 2 == no fat filesytem
	jr	z,noFatRecord
	or	a,a
	jr	z,goodFatRecord			; probably fdisk format here instead if a == 1
	ld	iy,xferReadDataDefault+446	; offset = mbr table
	ld	a,(iy+4)
	or	a,a
	jr	z,noFatRecord			; type of partition must exist
	lea	hl,iy+8				; partition offset in lba
	ld	de,fs_struct+fs_volbase
	call	util_cp32			; store as little-endian
	ld	de,xferReadLba			; scsi expects big-endian
	call	util_cp32_little_to_big
	call	scsiRequestDefaultReadSector	; get the fat record sector
	call	fs_check
	or	a,a
	jr	z,goodFatRecord			; ensure
noFatRecord:
	xor	a,a
	inc	a
	ret
goodFatRecord:
	ld	ix,xferReadDataDefault		; sector data
	ld	iy,fs_struct			; pointer to fat structure

	ld	a,(ix+42)
	or	a,a
	jr	nz,noFatRecord
	ld	a,(ix+43)
	or	a,a
	jr	nz,noFatRecord			; ensure only support for fat32 v0.0

	ld_ehl_ix(44)
	ld_iy_ehl(fs_dirbase)			; root directory start cluster
	debugCallHexBlock(fs_struct+fs_dirbase, 4)
	debugCall(debugNewLine)			; looks good

	ld_ehl_ix(36)				; euhl = number of sectors per fat
	ld_iy_ehl(fs_fatsize)			; fs_fasize = number of sectors per fat
	debugCallHexBlock(fs_struct+fs_fatsize, 4)
	debugCall(debugNewLine)			; looks good

	xor	a,a
	ld	bc,0
	ld	c,(ix+16)			; aubc = number of fats (2 usually)
	call	__lmulu				; euhl = fs_fasize * aubc
	ld_iy_ehl(fs_fasize)			; fs_fasize = number of sectors per fat
	debugCallHexBlock(fs_struct+fs_fasize, 4)
	debugCall(debugNewLine)			; looks good

	xor	a,a
	ld	bc,0
	ld	c,(ix+14)
	ld	b,(ix+15)			; nrsv: number of reserved sectors
	ld	(nrsv),bc
	call	__ladd				; sysect: nrsv + fasize
	ld	a,e
	push	hl
	pop	bc
	push	bc
	push	af
	ld_ehl_ix(32)				; tsect: total number of sectors on the volume
	call	__lsub				; tsect - sysect
	xor	a,a
	ld	bc,0
	ld	c,(ix+13)
	call	__ldivu				; nclst: (tsect - sysect) / fs_csize;
	ld	c,2
	call	__ladd				; num_fatent: nclst + 2
	ld_iy_ehl(fs_num_fatent)
	debugCallHexBlock(fs_struct+fs_num_fatent, 4)
	debugCall(debugNewLine)			; looks good

	ld_ehl_iy(fs_volbase)
nrsv =$+1
	ld	bc,0
	xor	a,a
	call	__ladd
	ld_iy_ehl(fs_fatbase)			; fatbase: volbase + nrsv
	debugCallHexBlock(fs_struct+fs_fatbase, 4)
	debugCall(debugNewLine)			; looks good

	ld_ehl_iy(fs_volbase)
	pop	af
	pop	bc				; aubc = sysect
	call	__ladd
	ld_iy_ehl(fs_database)			; database: volbase + sysect
	debugCallHexBlock(fs_struct+fs_database, 4)
	debugCall(debugNewLine)			; looks good

	xor	a,a
	ld	(fs_struct+fs_flag),a
	ret

; opens a file to the pointer
fs_open:
	ret

; check for valid fat record
fs_check:
	ld	de,$55 | ($aa << 8)		; magic = 0xAA55
	ld	bc,510				; offset = signature
	call	fs_offset_magic
	ld	a,2
	ret	nz
	ld	de,$46 | ($41 << 8)		; magic = FAT32
	ld	bc,82				; offset = fat32 check
	call	fs_offset_magic
	ld	a,1
	ret	nz
	dec	a
	ret

; bc = offset
; de = check
fs_offset_magic:
	ld	hl,xferReadDataDefault
	add	hl,bc
	ld	a,(hl)			; ensure record signature == 0xAA55
	cp	a,e
	ret	nz
	inc	hl
	ld	a,(hl)
	cp	a,d			; not a boot record
	ret

; move up to the parent directory
dir_rewind:
	ret

util_cp32:
	push	de
	push	hl
	ldi
	ldi
	ldi
	ldi
	pop	hl
	pop	de
	ret

util_cp32_little_to_big:
	inc	de
	inc	de
	inc	de
	ld	b,3
_:	ld	a,(hl)
	inc	hl
	ld	(de),a
	dec	de
	djnz	-_
	ret

util_zero:
	.db	0,0,0,0

; ---
; filesystem handling
; ---

fs_struct:
fs_type =$-fs_struct
	.db	0	; fat type
fs_flag =$-fs_struct
	.db	0	; file status flags
fs_csize =$-fs_struct
	.db	0,0	; number of sectors per cluster + padding
fs_fasize =$-fs_struct
	.db	0,0,0,0	; number of sectors per fat
fs_fatsize =$-fs_struct
	.db	0,0,0,0	; number of sectors per fat
fs_volbase =$-fs_struct
	.db	0,0,0,0	; location of fat base
fs_num_fatent =$-fs_struct
	.db	0,0,0,0	; number of fat entries (clusters + 2)
fs_fatbase =$-fs_struct
	.db	0,0,0,0	; fat start sector
fs_dirbase =$-fs_struct
	.db	0,0,0,0	; root directory start sector (clusters)
fs_database =$-fs_struct
	.db	0,0,0,0	; data start sector
fs_fptr =$-fs_struct
	.db	0,0,0,0	; file read/write pointer
fs_fsize =$-fs_struct
	.db	0,0,0,0	; file size
fs_org_clust =$-fs_struct
	.db	0,0,0,0	; file start cluster
fs_curr_clust =$-fs_struct
	.db	0,0,0,0	; file current cluster
fs_dsect =$-fs_struct
	.db	0,0,0,0	; file current data sector

; ---
; directory handling
; ---

dir_index:
	.dl	0	; current read/write index number
dir_sfn:
	.dl	0	; pointer to sfn (i/o) { file[8], ext[3], status[1] }
dir_sclust:
	.db	0,0,0,0	; table start cluster
dir_clust:
	.db	0,0,0,0	; current cluster
dir_sect:
	.db	0,0,0,0	; currect sector
