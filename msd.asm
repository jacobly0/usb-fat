xferCBWDataDefault equ pixelShadow2+1024
xferReadDataDefault equ pixelShadow2+2048

initMsd:
	call    initDevice
	call	findFirstMsdInterface
	call	c,findScsiEndpoints
	ret	nc
	call	msdReset
	call	msdMaxLun
	call	msdSetLun
	call	scsiInit
	scf
	ret

scsiInit:
	ld	hl,scsiInquiry
	call	scsiDefaultRequest
	ld	hl,scsiTestUnitReady
	call	scsiDefaultRequest
	ld	hl,scsiReadCapacity
	ld	de,lb_addr
	call	scsiRequest		; store the logical block address/size
	ld	hl,scsiModeSense6	; get the things?
	call	scsiDefaultRequest
	ld	hl,scsiReqestSense	; some drives die without this?
	call	scsiDefaultRequest
	ld	hl,scsiTestUnitReady	; test if ready again
	call	scsiDefaultRequest
	ld	hl,(block_size)
	ld	de,512
	or	a,a
	sbc	hl,de
	jp	nz,scsiFail		; ensure sector size is 512 bytes
	ret

; scsi format:
; <[1] in/out>,<[3] i/o length>,<[1] cdb length>,<[n] cdb>

scsiInquiry:
	.db	$80,$24,$00,$00,$06,$12,$00,$00,$00,$24,$00

scsiTestUnitReady:
	.db	$00,$00,$00,$00,$06,$00,$00,$00,$00,$00,$00

scsiModeSense6:
	.db	$80,$44,$00,$00,$06,$1a,$00,$3f,$00,$44,$00 ; 44 should be c0 eventually

scsiReqestSense:
	.db	$80,$1c,$00,$00,$06,$03,$00,$00,$00,00,$1c ; 1c should be 60 eventually

scsiReadCapacity:
	.db	$80,$08,$00,$00,$0a,$25,$00,$00,$00,$00,$00,$00,$00,$00,$00

scsiRead10:	; just requests a single 512 byte sector for kicks
	.db	$80,$00,$02,$00,$0a,$28,$00
xferReadLba:
	.db	$00,$00,$00,$00
scsiRead10GroupNum:
	.db	$00
scsiRead10Length:
	.db	$00,$01
scsiRead10Ctrl:
	.db	$00

; Input:
;  hl = ptr to xfer
;  de = ptr to storage
; Output;
;   z = success
;  nz = failure
scsiDefaultRequest:
	ld	de,xferCBWDataDefault
scsiRequest:
	call	scsiReq
	ret	z
	or	a,a			; csw packet status
	jp	z,scsiFail		; fail if anything other than csw status
					; we should perform recovery here?
	ret

; Input:
;  bc = offset in data
; Output:
;  hl -> requested data thing
scsiRequestDefaultReadSector:
	ld	bc,0
scsiRequestDefaultRead:
	ld	de,xferReadDataDefault
scsiRequestRead:
	push	bc
	push	de
	ld	hl,scsiRead10
	call	scsiRequest
	pop	hl
	pop	bc
	add	hl,bc
	ret

scsiRequestWrite:
	ret

scsiReq:
	ld	(xferCBWData),de
	ld	iy,packetCBW
	ld	a,(hl)
	ld	(iy+12),a		; direction flag
	push	af
	inc	hl
	ld	bc,(hl)			; get the length -- only use 3/4 bytes
	ld	(iy+8),bc		; i/o length
	push	bc
	inc	hl
	inc	hl
	inc	hl			; move past length
	ld	bc,0
	ld	c,(hl)			; cdb length
	inc	c
	lea	de,iy+14		; cdb location
	ldir
	ld	hl,(iy+4)		; increment tag
	inc	hl
	ld	(iy+4),hl
	push	iy			; iy -> packetCBW
	call	msdDeviceEpOut
	pop	de			; de -> packetCBW
	ld	bc,packetCBWLen
	call	bulkTransfer
	pop	bc
	pop	af
	or	a,a
	call	nz,msdDeviceEpIn
	call	z,msdDeviceEpOut
	sbc	hl,hl
	adc	hl,bc
	jr	z,msdCswAck		; no bulk out if 0 length req
xferCBWData =$+1
	ld	de,0
	call	bulkTransfer
msdCswAck:
	call	msdDeviceEpIn
	ld	de,packetCSW
	ld	bc,packetCSWLen
	call	bulkTransfer
	call	qhReap
	ld	hl,(packetCBW+4)
	ld	de,(packetCSW+4)		; check transfer tags
	xor	a,a
	sbc	hl,de
	ret	nz
	ld	hl,(packetCSW)
	ld	de,(packetCBW)
	xor	a,a
	sbc	hl,de
	ret	nz				; technically should check for 0x53 too... but meh, speedz
	ld	a,(packetCSW+12)
	or	a,a				; check for good status of transfer
	ret

scsiFail:
	debugCallLine("Transmission Error.")
	jp	debugAbort

; Input:
;  hl = ptr in config
;   a = type
; Output:
;  nc = not found
;   c = found
getDescriptor:
	push	de
	push	bc
	ld	de,(config_end)
	ld	bc,0
findDescriptor:
	or	a,a
	sbc	hl,de
	add	hl,de
	jr	nc,notFoundDescriptor
	push	hl
	pop	iy
	ld	c,(hl)				; length of descriptor
	cp	a,(iy+1)			; bDescriptorType == 4
	jr	z,foundDescriptor
	add	hl,bc
	jr	findDescriptor
foundDescriptor:
	scf
notFoundDescriptor:
	pop	bc
	pop	de
	ret

; Input:
;  hl = interface ptr
; Output:
;  nc = not found
;   c = found
findScsiEndpoints:
	ld	bc,6 | ($50 << 8)		; check scsi bot
	call	checkSubclassProtocol
	jr	nz,endpointFail
	push	hl
	pop	iy
	ld	b,(iy+4)
	ld	c,0
	ld	de,0
findScsiEndpointsLoop:
	ld	a,5				; endpoint descriptor type
	call	getDescriptor
	ret	nc
	ld	a,(iy+2)
	bit	7,a
	jr	z,endpointOut
	bit	0,c
	jr	nz,endpointSkip
	ld	(ep_in),a
	debugCallStr("In: ")
	debugCall(debugHexA)
	debugCall(debugNewLine)
	set	0,c
	jr	endpointSkip
endpointOut:
	bit	1,c
	jr	nz,endpointSkip
	ld	(ep_out),a
	debugCallStr("Out: ")
	debugCall(debugHexA)
	debugCall(debugNewLine)
	set	1,c
endpointSkip:
	ld	e,(iy+0)
	add	hl,de
	djnz	findScsiEndpointsLoop
	ld	a,c
	cp	a,2
	jr	z,endpointFail
	debugCallStr("Valid Endpoints.")
	debugCall(debugNewLine)
	scf
	ret
endpointFail:
	scf
	ccf
	ret

; Input:
;  none
; Output:
;  hl = interface ptr
;  nc = not found
;   c = found
findFirstMsdInterface:
	ld	hl,(config_start)
findMsdInterface:
	ld	a,4				; interface descriptor type
	call	getDescriptor
	ret	nc
	ld	a,(iy+0)			; bLength
	cp	a,9
	jr	c,findMsdInterface		; ensure interface length >= 9
	ld	a,(iy+5)
	cp	a,8				; bInterfaceClass == 8
	jr	nz,findMsdInterface
	debugCallStr("MSD Interface: ")
	ld	a,(iy+2)
	ld	(packetMSDReset+4),a		; store the interface number
	ld	(packetMSDMaxLUN+4),a		; store the interface number
	debugCall(debugHexA)
	debugCall(debugNewLine)
	scf
	ret

; Input:
;   c = subclass
;   b = protocol
checkSubclassProtocol:
	ld	a,(iy+6)
	cp	a,c				; scsi subclass
	ret	nz
	ld	a,(iy+7)
	cp	a,b				; bot protocol
	ret

msdSetLun:
	ld	a,(curr_lun)
	ld	(packetCBW+13),a
	ret
msdMaxLun:
	ld	hl,max_lun
	ld	de,packetMSDMaxLUN
	jp	setup
msdReset:
	or	a,a
	sbc	hl,hl
	ld	de,packetMSDReset
	jp	setup

; Output:
;  flags are unaffected
msdDeviceEpOut:
ep_out =$+1
	ld	a,0
	jr	+_
msdDeviceEpIn:
ep_in =$+1
	ld	a,0
_:	ld	iy,(device)
	ret

max_lun:
	.db	0
curr_lun:
	.db	0

lb_addr:
	.db	0
	.dl	0
	.db	0	; technically part of block size, but meh
block_size:
	.dl	0

packetMSDReset:
	.db	$21,$FF,$00,$00,$00,$00,$00,$00
packetMSDMaxLUN:
	.db	$A1,$FE,$00,$00,$00,$00,$01,$00

packetCSW:
	.db	$00,$00,$00,$00 ; signature
	.db	$00,$00,$00,$00 ; tag
	.db	$00,$00,$00,$00 ; residue
	.db	$00             ; status
packetCSWLen = $-packetCSW

packetCBW:
	.db	$55,$53,$42,$43	; signature
	.db	$00,$00,$00,$00 ; tag
	.db	$00,$00,$00,$00 ; length
	.db	$00             ; flags
	.db	$00		; lun
packetCBWCBD:
	.db	$00		; command block length
	.db	$00		; command block opcode
	.db	$00,$00,$00	; command block
	.db	$00,$00,$00,$00 ; command block
	.db	$00,$00,$00,$00 ; command block
	.db	$00,$00,$00,$00 ; command block
packetCBWLen = $-packetCBW
