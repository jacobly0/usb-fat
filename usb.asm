initDevice:
	call    usbInit				; init usb
	call    devFirst
	ld	(device),iy			; use first device
	ld      hl,pixelShadow2			; get dev desc
	ld      de,reqDevDesc18
	call    setup
	ld      hl,pixelShadow2+18		; get langids
	ld      de,reqGetStrDesc
	call    setup
	ld      hl,pixelShadow2+18+2
	ld      de,reqGetStrDesc+4
	ldi
	ldi					; use first langid
	ld      hl,(pixelShadow2+8)		; get vendor
	ld      hl,(pixelShadow2+10)		; get product
	ld      hl,(pixelShadow2+12)		; get release
	ld      a,(pixelShadow2+14)		; get manufacturer
	ld      a,(pixelShadow2+15)		; get product
	ld      a,(pixelShadow2+16)		; get serial number
	ld	iy,(device)			; get configuration descriptor
	xor     a,a
	call    getConfigDescTotalLength	; total length of config
	ld      (reqConfDesc+wLength),hl
	push    hl
	ld      hl,pixelShadow2
	ld      de,reqConfDesc
	call    setup
	pop	bc
	ld      hl,pixelShadow2
	ld	(config_start),hl
	add	hl,bc
	ld	(config_end),hl
	ld	iy,(device)			; select config
	ld      ix,pixelShadow2
	call	devSelectConfig
	ret

setup:
	ld	iy,(device)
	call	controlDefaultTransfer
	jp	qhWait

; Input:
;  a = index
debugStrDesc:
#ifndef NDEBUG
	ld	hl, debugNewLine
	push	hl
#endif
	or      a,a
	jr      nz,_
	debugCallStr("<NONE>")
	ret
_:      ld      (reqGetStrDesc+2),a
	debugCallChar('\"')
	call	_
	debugCallStr('\"')
	ret
_:	ld      hl,pixelShadow2+18
	ld      de,reqGetStrDesc
	push    hl
	call    setup
	pop     hl
	ld      b,(hl)
	srl     b
_:      dec     b
	ret     z
	inc     hl
	inc     hl
	ld      de,(hl)
	ld      a,d
	or      a,a
	jr      nz,_
	ld      a,e
	cp      a,$20
	jr      c,_
	cp      a,$7F
	jr      nc,_
	cp      a,$5B
	jr      nz,++_
	ld      a,$C1
	jr      ++_
_:      ld      a,$D0
_:      debugCall(debugCharA)
	jr      ---_

reqDevDesc18:
	.db     $80,6,0,1,0,0,18,0
reqConfDesc:
	.db     $80,6,1,2,0,0,0,0,0
reqGetStrDesc:
	.db     $80,6,0,3,0,0,0,1
reqClearHalt:
	.db	2,1,0,0,0,0,0,0

device:
	.dl	0
config_start:
	.dl	0
config_end:
	.dl	0
