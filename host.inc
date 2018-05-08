usbRangeLow		= $D13FD8
qhFreeList		= $D13FD8
qtdFreeList		= $D13FDB
setupPacket		= $D13FDE
unalloc0		= $D13FE6 ; 26 bytes
periodicList		= $D14000 ; 4K aligned, 1K
alignedPoolLow		= $D14400 ; 32b aligned
qhHead			= $D17702
alignedPoolHigh		= $D17740
unalloc1		= $D17740 ; 119 bytes
usbSillyState		= $D177B7 ; needs to not be $55 so nothing else gets clobbered
unalloc2		= $D177B8 ; 4 bytes
usbRangeHigh		= $D177BC

devEndpoints		= 0
devHubPorts		= devEndpoints+16
devAddr			= devHubPorts+1
devSpeed		= devAddr+1
devHubInfo		= devSpeed+1
devParent		= devHubInfo+2
devChildren		= devParent+3
devSibling		= devChildren+3
devSize			= devSibling+3

qtdNext			= 0
qtdAltNext		= qtdNext+4
bQtdAltNext		= 2
qtdStatus		= qtdAltNext+4
bQtdStatus		= 3
qtdPid			= qtdStatus+1
qtdLen			= qtdPid+1
qtdBufPtr		= qtdLen+2
qtdSize			= qtdBufPtr+20

bQtdStatusActive	= 7
bmQtdStatusActive	= 1<<bQtdStatusActive
bQtdStatusHalted	= 6
bmQtdStatusHalted	= 1<<bQtdStatusHalted
bQtdStatusBufErr	= 5
bmQtdStatusBufErr	= 1<<bQtdStatusBufErr
bQtdStatusBabble	= 4
bmQtdStatusBabble	= 1<<bQtdStatusBabble
bQtdStatusXactErr	= 3
bmQtdStatusXactErr	= 1<<bQtdStatusXactErr
bQtdStatusMissedUframe	= 2
bmQtdStatusMissedUframe	= 1<<bQtdStatusMissedUframe
bQtdStatusSplitState	= 1
bmQtdStatusSplitState	= 1<<bQtdStatusSplitState

qhOff			= 1<<1
qhNext			= -qhOff
qhAddr			= qhNext+4
qhEndPt			= qhAddr+1
qhMaxPktLen		= qhEndPt+1
qhSmask			= qhMaxPktLen+2
qhCmask			= qhSmask+1
qhHubAddr		= qhCmask+1
qhHubPort		= qhHubAddr+1
qhCurQtd		= qhHubPort+1
qhOverlay		= qhCurQtd+4
qhDir			= qhOverlay+qtdSize
qhType			= qhDir+1
qhPrev			= qhType+1
qhFirstQtd		= qhPrev+3
qhLastQtd		= qhFirstQtd+3
qhSize			= qhLastQtd+3

bmRequestType		= 0
bRequest		= 1
wValue			= 2
wIndex			= 4
wLength			= 6

reqTypeGetStat		= 0
reqTypeClrFeat		= 1
reqTypeSetFeat		= 3
reqTypeSetAddr		= 5
reqTypeGetDesc		= 6
reqTypeSetDesc		= 7
reqTypeGetConf		= 8
reqTypeSetConf		= 9
reqTypeGetIface		= 10
reqTypeSetIface		= 11
reqTypeSyncFrame	= 12

descTypeDev		= 1
descTypeConf		= 2
descTypeStr		= 3
descTypeIface		= 4
descTypeEndpt		= 5
descTypeDevQual		= 6
descTypeOtherConf	= 7
descTypeIfacePwr	= 8

bLength			= 0
bDescriptorType		= bLength+1

bcdUSB			= bDescriptorType+1
bDeviceClass		= bcdUSB+2
bDeviceSubClass		= bDeviceClass+1
bDeviceProtocol		= bDeviceSubClass+1
bMaxPacketSize0		= bDeviceProtocol+6

idVendor		= bMaxPacketSize0+1
idProduct		= idVendor+2
bcdDevice		= idProduct+2
iManufacturer		= bcdDevice+2
iProduct		= iManufacturer+1
iSerialNumber		= iProduct+1
bNumConfigurations	= iSerialNumber+1
descLenDev		= bNumConfigurations+1

bNumQualConfigurations	= bDeviceProtocol+1
bQualReserved		= bNumQualConfigurations+1
descLenDevQual		= bQualReserved+1

wTotalLength		= bDescriptorType+1
bNumInterfaces		= wTotalLength+2
bConfigurationValue	= bNumInterfaces+1
iConfiguration		= bConfigurationValue+1
bmInterfaceAttributes	= iConfiguration+1
bMaxPower		= bmInterfaceAttributes+1
descLenConf		= bMaxPower+1

bInterfaceNumber	= bDescriptorType+1
bAlternateSetting	= bInterfaceNumber+1
bNumEndpoints		= bAlternateSetting+1
bInterfaceClass		= bNumEndpoints+1
bInterfaceSubClass	= bInterfaceClass+1
bInterfaceProtocol	= bInterfaceSubClass+1
iInterface		= bInterfaceProtocol+1
descLenIface		= iInterface+1

bEndpointAddress	= bDescriptorType+1
bmEndpointAttributes	= bEndpointAddress+1
wMaxPacketSize		= bmEndpointAttributes+1
bInterval		= wMaxPacketSize+2
descLenEndpt		= bInterval+1

usbInit:
	ld	hl,mpTmrCtrl
	res	bTmr3Enable,(hl)
	xor	a,a
	ld	(usbSillyState),a
	debugCallLine("Waiting for plug a...")
_:	call	_usb_SelfPowered
	jr	nz,-_
	call	usbChipReset
	ld	hl,periodicList
	ld	(mpUsbPeriodicListBase),hl
	xor	a,a
	ld	b,a
	inc	a
_:	ld	(hl),a
	inc	hl
	inc	hl
	inc	hl
	inc	hl
	djnz	-_
;	ld	hl,alignedPoolLow
	ld	e,a
	ld	c,a
_:	ld	l,qhOff
	ld	(hl),bc
	push	hl
	pop	bc
	ld	a,64
_:	ld	l,a
	ld	(hl),de
	push	hl
	pop	de
	add	a,32
	jr	nz,-_
	ld	l,a
	inc	h
	ld	a,h
	cp	a,alignedPoolHigh>>8&$FF
	jr	nz,--_
	ld	(qhFreeList),bc
	ld	(qtdFreeList),de
	ld	ix,qhHead
;	lea	hl,ix+qhNext
	ld	(hl),ix
	ld	(ix+qhEndPt),1<<7
	ld	(ix+qhOverlay+qtdNext),1
	ld	(ix+qhOverlay+qtdAltNext),1
	ld	(ix+qhOverlay+qtdStatus),bmQtdStatusHalted
	ld	(ix+qhPrev),ix
	ld	(mpUsbAsyncListAddr),hl
	ld	hl,mpUsbCmd
	ld	(hl),(1<<5)|(1<<4)|(2<<2)|(1<<0)
;	jr	usbWaitForConnection

usbWaitForConnection:
	call	usbBusPower
	debugCallLine("Waiting for connection...")
	ld	hl,mpUsbPortSC
_:	bit	0,(hl)
	jr	z,-_
	call	usbBusReset
	ld	ix,ep0Desc
	lea	iy,ix+devRoot-ep0Desc
	xor	a,a
	ld	(reqSetAddr+wValue),a
	ld	(iy+devAddr),a
	ld	a,(mpUsbOtgCsr+2)
	and	a,3<<6
	rrca
	rrca
	ld	(iy+devSpeed),a
	call	qhInit
	ld	hl,reqSetAddr+wValue
	inc	(hl)
	call	pe,debugAbort
	ld	a,ixh
	ld	(iy+devEndpoints+0),a
	ld	a,(hl)
	ld	(iy+devAddr),a
	push	af
	call	qtdAlloc
	push	hl
	ld	de,reqGetDevDesc8
	call	controlTransferAdd
	ld	de,reqSetAddr
	call	controlTransferAdd
	debugCallLine("Transfers added...")
	call	qhWait
	call	z,debugAbort
	pop	iy
	pop	af
	ld	(ix+qhAddr),a
	ld	a,(iy+7)
	ld	(ix+qhMaxPktLen),a
	lea	hl,iy
	debugCallStr("Device descriptor received: ")
	debugCallHexBlockHL(8)
	debugCall(debugNewLine)
	jp	qtdFree

usbCleanup:
	ld	hl,mpUsbCmd
	ld	(hl),h
	ld	hl,usbRangeLow
	ld	bc,usbRangeHigh-usbRangeLow
	jp	_MemClear

usbChipReset:
	debugCallLine("Resetting chip...")
	ld	a,7
	ld	(mpUsbIdle),a
	ld	de,$2B0
	ld	hl,mpUsbDevCtrl+1
	ld	(hl),2
	dec	hl
	ld	(hl),$B0
;	jr	usbHostReset

usbHostReset:
	debugCallLine("Waiting for host to stop...")
	ld	hl,mpUsbCmd
	ld	(hl),h
	ld	l,usbSts+1
_:	bit	12-8,(hl)
	jr	z,-_
	debugCallLine("Waiting for host to reset...")
	ld	l,usbCmd
	ld	(hl),(2<<2)|(1<<1)
_:	bit	1,(hl)
	jr	nz,-_
	ret

usbBusPower:
	debugCallLine("Powering bus...")
	call	$21B70
	ld	hl,mpUsbOtgCsr
	res	5,(hl)
	set	4,(hl)
	ret

usbBusReset:
	debugCallLine("Resetting bus...")
	ld	hl,mpUsbPortSC+1
	set	0,(hl)
	call	_Delay10ms
	res	0,(hl)
_:	bit	0,(hl)
	jr	nz,-_
	ret

; Input:
;  ix = hub dev
; Output:
;  af = ?
;  bc = ?
;  hl = ?
;  iy = ?
hubPortsInit:
	ld	b,(ix+devHubPorts)
	lea	iy,ix-3
_:	call	qtdAlloc
	ld	(iy+devSibling),hl
	push	hl
	pop	iy
	push	bc
	ld	bc,devParent+1
	call	_MemClear
	ld	(hl),ix
	ld	(iy+devChildren),1
	pop	bc
	djnz	-_
	ld	(iy+devSibling),1
	ret

; Input:
;  iy = dev
; Output:
;  cf = done
;  iy = next dev
_:	scf
	bit	0,(iy+devParent)
	ret	nz
	ld	iy,(iy+devParent)
devNext:
	bit	0,(iy+devSibling)
	jr	nz,-_
_:	ld	iy,(iy+devSibling)
_:	or	a,a
	bit	0,(iy+devChildren)
	ret	nz
	lea	iy,iy-3
	jr	--_

; Output:
;  iy = first dev
devFirst:
	ld	iy,devRoot
	jr	-_

; Input:
;  ix = config desc
;  iy = dev
; Output:
;  a = ?
;  bc = ?
;  de = ?
;  hl = ?
;  ix = qh
devSelectConfig:
	ld	a,(ix+bDescriptorType)
	cp	a,2
	ret	nz
	ld	a,(ix+bLength)
	cp	a,9
	ret	c
	ld	a,(ix+bConfigurationValue)
	ld	de,reqSetConf+wValue
	ld	(de),a
	dec	de
	dec	de;reqSetConf
	push	ix
	push	iy
	call	controlDefaultTransfer
	pop	iy
	ex	(sp),ix
	call	_
	pop	ix
	call	c,debugAbort
	jp	qhWait
_:	or	a,a
	sbc	hl,hl
	ex	de,hl
	ld	hl,(ix+wTotalLength)
_:	ld	a,(ix+bLength)
	cp	a,2
	ret	c
	ld	e,a
	sbc.s	hl,de
	ret	c
	ld	a,(ix+bDescriptorType)
	cp	a,5
	jr	nz,+_
	ld	a,(ix+bLength)
	cp	a,7
	ret	c
	push	de
	push	hl
	push	ix
	call	qhInit
	pop	ix
	pop	hl
	pop	de
_:	ld	a,l
	or	a,h
	ret	z
	add	ix,de
	jr	--_

; Input:
;  a = index
;  iy = dev
; Output:
;  hl = desc size
getConfigDescTotalLength:
	ld	de,reqGetConfDesc4+wValue
	ld	(de),a
	dec	de
	dec	de;reqGetConfDesc4
	ld	hl,setupPacket
	call	controlDefaultTransfer
	call	qhWait
	ld	hl,(setupPacket+wTotalLength)
	inc	hl
	dec.s	hl
	ret

; Output:
;  f = ?
;  de = ?
;  hl = qh
qhAlloc:
	ld	hl,(qhFreeList)
	bit	0,l
	call	nz,debugAbort	; could malloc, but would require 255 + sizeof qh to properly align (though you would get free qtds I guess)
	ld	de,(hl)
	ld	(qhFreeList),de
	ret

; Input:
;  ix = qh
; Output:
;  zf = error
;  af = ?
;  bc = ?
;  de = ?
;  hl = ?
qhWait:
	ld	hl,(ix+qhFirstQtd)
_:;	debugCallStr("Waiting for transfer ")
;	debugCall(debugHexUHL)
;	debugCallLine(" to complete...")
	ld	de,(hl) ; FIXME: short packets!!!
	bit	0,e
	ret	nz
	set	bQtdStatus,l
_:	bit	7,(hl)
	jr	nz,-_
	ld	a,(hl)
	res	bQtdStatus,l
	call	qtdFree
	ex	de,hl
	ld	(ix+qhFirstQtd),hl
	bit	6,a
	jr	z,--_
	call	debugAbort

; Input:
;  ix = qh
; Output:
;  a = err bits
;  f = ?
;  bc = ?
;  de = ?
;  hl = ?
qhReap:
	ld	hl,(ix+qhFirstQtd)
	xor	a,a
	jr	++++_
_:	and	a,bmQtdStatusHalted|bmQtdStatusBufErr|bmQtdStatusBabble|bmQtdStatusXactErr|bmQtdStatusMissedUframe<<1
	jr	nz,++_
	set	bQtdStatus,l
_:	ld	a,(hl)
	rlca
	jr	c,-_
	res	bQtdStatus,l
_:	call	qtdFree
	ex	de,hl
	ld	(ix+qhFirstQtd),hl
_:;	debugCallStr("Waiting for transfer ")
;	debugCall(debugHexUHL)
;	debugCallLine(" to complete...")
	ld	de,(hl) ; FIXME: short packets!!!
	bit	0,e
	jr	z,----_
	ld	(ix+qhOverlay+qtdNext),hl ; flush any pending transactions
	ld	(ix+qhOverlay+qtdAltNext),hl
	ld	(ix+qhOverlay+qtdStatus),0 ; clear pipe halt
	ret

; Input:
;  ix = ep desc
;  iy = dev
; Output:
;  af = ?
;  bc = ?
;  de = ?
;  hl = ?
;  ix = qh
qhInit:
	call	qhAlloc
	ld	bc,(ix+bEndpointAddress)
	ld	de,(iy+devAddr)
	push	hl;qh
	inc	hl
	inc	hl;qh+qhAddr
	ld	(hl),e
	ld	a,c
	and	a,$f
	or	a,d
	bit	1,b;(ix+bmEndpointAttributes)
	jr	nz,_
	or	a,1<<6
_:	inc	hl;qh+qhEndPt
	ld	(hl),a
	ld	de,(ix+wMaxPacketSize)
	inc	hl;qh+qhMaxPktLen
	ld	(hl),e
	bit	5,a
	ld	a,b
	ld	e,4<<4
	jr	nz,_
	ld	e,1<<3
	rrca
_:	rrca
	jr	nc,_
	ld	e,0
_:	ld	a,d
	and	a,7
	or	a,e
	inc	hl;qh+qhMaxPktLen+1
	ld	(hl),a
	ld	de,(iy+devHubInfo-1)
	ld	a,b
	and	a,1
	inc	hl;qh+qhSmask
	ld	(hl),a
	rrca
	sbc	a,a
	and	a,7<<2
	ld	e,a
	inc	hl;qh+qhCmask
	ld	(hl),de
	ld	a,c
	and	a,$f
	add	a,iyl
	push	iy
	ld	iyl,a
	ld	a,(iy)
	ld	(iy),h
	pop	iy
	ld	h,a
	pop	ix;qh
	push	hl
	call	qtdInitDummy
	ld	(ix+qhOverlay+qtdNext),hl
	ld	(ix+qhOverlay+qtdAltNext),1
	ld	(ix+qhOverlay+qtdStatus),0
	ld	(ix+qhOverlay+qtdLen+1),0;dt
	ld	(ix+qhDir),bc
	ld	bc,qhHead
	ld	(ix+qhPrev),bc
	ld	(ix+qhFirstQtd),hl
	ld	(ix+qhLastQtd),hl
	ld	hl,(qhHead+qhNext)
	ld	(ix+qhNext),hl
	ld	l,qhOff+qhPrev
	ld	(hl),ix
	ld	(qhHead+qhNext),ix
	pop	de
	ld	a,d
	or	a,a
	ret	z
	ld	hl,mpUsbCmd
	set	6,(hl)
	ld	l,usbSts
_:	bit	5,(hl)
	jr	z,-_
	ex	de,hl
	ld	l,qhOff
;	jr	qhFree

; Input:
;  hl = qh
; Output:
;  bc = ?
qhFree:
	bit	0,l
	call	nz,debugAbort
	ld	bc,(qhFreeList)
	ld	(hl),bc
	ld	(qhFreeList),hl
	ret

; Output:
;  f = ?
;  hl = qtd
qtdAlloc:
	push	de
	ld	hl,(qtdFreeList)
	bit	0,l
	call	nz,debugAbort	; could malloc more I guess, or requisition a qh (would help dynamically fix the ratio)
	ld	de,(hl)
	ld	(qtdFreeList),de
	pop	de
	ret

; Input:
;  hl = qtd
; Output:
;  bc = ?
qtdFree:
	bit	0,l
	call	nz,debugAbort
	ld	bc,(qtdFreeList)
	ld	(hl),bc
	ld	(qtdFreeList),hl
	ret

; Output:
;  f = ?
;  hl = qtd
qtdInitDummy:
	call	qtdAlloc
	set	bQtdStatus,l
	ld	(hl),bmQtdStatusHalted
	res	bQtdStatus,l
	ld	(hl),1
	ret

; Input:
;  a = ep
;  iy = dev
; Output:
;  af = ?
;  ix = qh
;  iy = ?
epGetQh:
	and	a,$f
	or	a,iyl
	ld	iyl,a
	ld	a,(iy)
	ld	ix,qhHead
	ld	ixh,a
	ret

; Input:
;  a = length
;  b = type
;  hl = buf
;  iy = dev
; Output:
;
getDefaultDesc:
	ld	c,0

; Input:
;  a = length
;  b = type
;  c = index
;  hl = buf
;  iy = dev
; Output:
;
getDesc:
	ld	de,0

; Input:
;  a = length
;  b = type
;  c = index
;  de = language
;  hl = buf
;  iy = dev
; Output:
;
getStrDesc:
	push	hl;buf
	ld	hl,setupPacket+bmRequestType
	push	hl;setupPacket
	ld	(hl),$80
	inc	hl;setupPacket+bRequest
	ld	(hl),6
	inc	hl;setupPacket+wValue
	ld	(hl),bc
	inc	hl
	inc	hl;setupPacket+wIndex
	ld	(hl),de
	inc	hl
	inc	hl;setupPacket+wLength
	ld	(hl),a
	inc	hl
	ld	(hl),0
	pop	de;setupPacket
	pop	hl;buf
	call	controlDefaultTransfer
	jp	qhWait

; Input:
;  de = req
;  hl = buf
;  iy = dev
; Output:
;  af = ?
;  bc = ?
;  de = ?
;  hl = ?
;  ix = qh
;  iy = ?
controlDefaultTransfer:
	xor	a,a

; Input:
;  a = ep
;  de = req
;  hl = buf
;  iy = dev
; Output:
;  af = ?
;  bc = ?
;  de = ?
;  hl = ?
;  ix = qh
;  iy = ?
controlTransfer:
	call	epGetQh

; Input:
;  de = req
;  hl = buf
;  ix = qh
; Output:
;  af = ?
;  bc = ?
;  de = ?
;  hl = ?
;  iy = ?
controlTransferAdd:
	ld	a,(de)
	push	af
	push	hl
	ld	hl,wLength
	add	hl,de
	inc.s	bc
	ld	c,(hl)
	inc	hl
	ld	b,(hl)
	push	bc
	ld	bc,8
	ld	a,2
	call	_
	pop	bc
	pop	de
	pop	hl
	push	hl
	ld	a,b
	or	a,c
	ld	a,h
	call	nz,bulkTransferAddEnter
	pop	af
	rlca
	and	a,1
	xor	a,3<<2|1
	ld	bc,1<<15
_:	call	qtdInitDummy
	jr	qtdInit

_:	push	de
	add	hl,bc
	ld	bc,(ix+qhMaxPktLen)
	sla	c
	ld	a,b
	rla
	and	a,$f
	ld	d,a
	ld	e,c
	dec	de
	and	a,d
	ld	b,a
	ld	a,c
	and	a,e
	or	a,b
	jr	z,+++_
	push	hl
	ld	a,h
	ld	c,l
	sbc	hl,hl
	inc.s	de
	ld	b,16
_:	sla	c
	rla
	adc	hl,hl
	sbc	hl,de
	jr	nc,_
	add	hl,de
_:	djnz	--_
	ex	de,hl
	pop	hl
	or	a,a
	sbc	hl,de
	jr	++_
_:	ld	a,e
	cpl
	and	a,l
	ld	l,a
	ld	a,d
	cpl
	and	a,h
	ld	h,a
_:	pop	de
	pop	af
	push	hl
	pop	bc
	add	hl,de
	ex	(sp),hl
	sbc	hl,bc
	push	hl
	call	qtdInitDummy
	push	iy
	call	++_
	pop	iy
	pop	bc
	pop	de
	pop	hl
	jr	_

; Input:
;  a = ep
;  bc = len
;  de = buf
;  iy = dev
; Output:
;  af = ?
;  bc = ?
;  de = ?
;  hl = ?
;  ix = qh
;  iy = ?
bulkTransfer:
	call	epGetQh

; Input:
;  bc = len
;  de = buf
;  ix = qh
; Output:
;  af = ?
;  bc = ?
;  de = ?
;  hl = ?
;  iy = ?
bulkTransferAdd:
	ld	a,(ix+qhDir)
bulkTransferAddEnter:
	rlca
	and	a,1
	call	qtdInitDummy
	push	hl
	pop	iy
_:	push	bc
	push	af
	ld	a,d
	and	a,$f
	xor	a,$4f
	inc	a
	sbc	hl,hl
	ld	h,a
	ld	l,e
	sbc	hl,bc
	jr	c,------_
	pop	af
	pop	bc
	lea	hl,iy
_:	set	7,b
	.db	$fd

; Input:
;  a = ioc|cpage|cerr|pid
;  bc = dt|len
;  de = buf
;  hl = next qtd
;  ix = qh
;  hl/iy = alt next qtd
; Output:
;  af = ?
;  b = ?
;  de = ?
;  hl = ?
;  iy = qtd
qtdInit:
	push	hl
	ld	iy,(ix+qhLastQtd)
	ld	(ix+qhLastQtd),hl
	ld	(iy+qtdNext),hl
	pop	hl
	ld	(iy+qtdAltNext),hl
	ld	(iy+qtdPid),a
	ld	(iy+qtdLen),bc
	lea	hl,iy+qtdBufPtr
	ld	b,5
_:	ld	(hl),de
	ld	a,l
	add	a,4
	ld	l,a
	ld	a,d
	or	a,$f
	ld	d,a
	ld	e,$ff
	inc	de
	djnz	-_
	ld	(iy+qtdStatus),bmQtdStatusActive
	ret

reqGetDevDesc8:
	.db	$80,reqTypeGetDesc,0,descTypeDev,0,0,8,0
reqSetAddr:
	.db	$00,reqTypeSetAddr,0,0,0,0,0,0
reqGetConfDesc4:
	.db	$80,reqTypeGetDesc,0,descTypeConf,0,0,4,0
reqSetConf:
	.db	$00,reqTypeSetConf,0,0,0,0,0,0
ep0Desc:
	.db	descLenEndpt,descTypeEndpt,0,0,8,0,0
	.block	-$&15
devRoot:
	.block	devHubInfo
	.db	1<<7,1<<6
	.dl	1,1,1
