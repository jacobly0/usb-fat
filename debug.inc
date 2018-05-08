#ifdef NDEBUG
 #define debugCall(routine)
 #define debugCallChar(char)
 #define debugCallStr(src)
 #define debugCallLine(line)
 #define debugCallHexBlockBC(addr)
 #define debugCallHexBlockHL(size)
 #define debugCallHexBlock(addr,size)
debugAbort = 0
#else
 #define debugCall(routine) call routine
 #define debugCallChar(char) call debugChar \ .db char
 #define debugCallStr(str) call debugStr \ .db str,0
 #define debugCallLine(line) call debugStr \ .db line,0 \ call debugNewLine
 #define debugCallHexBlockBC(addr) call debugHexBlockBC \ .dl addr
 #define debugCallHexBlockHL(size) call debugHexBlockHL \ .db size
 #define debugCallHexBlock(addr,size) call debugHexBlock \ .dl addr \ .db size

debugAbort:
	debugCall(debugNewLine)
	debugCallStr("Abort called at ")
	ld	b,5
	jr	++_
_:	debugCallStr("<-")
_:	pop	hl
	debugCall(debugHexUHL)
	djnz	--_
	jr	$

debugHexBlockBC:
	ex	(sp),hl
	push	de
	push	bc
	push	af
	ld	de,(hl)
	inc	hl
	inc	hl
	inc	hl
_:	call	debugHex_DE_
	inc	de
	dec	bc
	ld	a,c
	or	a,b
	jr	nz,-_
	pop	af
	pop	bc
	pop	de
	ex	(sp),hl
	ret
debugHexBlockHL:
	ex	de,hl
	ex	(sp),hl
	push	de
	push	bc
	ld	b,(hl)
	inc	hl
_:	call	debugHex_DE_
	inc	de
	djnz	-_
	pop	bc
	pop	de
	ex	(sp),hl
	ex	de,hl
	ret
debugHexBlock:
	ex	(sp),hl
	push	de
	push	bc
	ld	de,(hl)
	inc	hl
	inc	hl
	inc	hl
	ld	b,(hl)
	inc	hl
_:	call	debugHex_DE_
	inc	de
	djnz	-_
	pop	bc
	pop	de
	ex	(sp),hl
	ret
debugHexBCU:
	push	af
	dec	sp
	push	bc
	jr	_
debugHexDEU:
	push	af
	dec	sp
	push	de
	jr	_
debugHexHLU:
	push	af
	dec	sp
	push	hl
_:	inc	sp
	pop	af
	jr	_
debugHexB:
	push	af
	ld	a,b
	jr	_
debugHexD:
	push	af
	ld	a,d
	jr	_
debugHexH:
	push	af
	ld	a,h
	jr	_
debugHexUBC:
	call	debugHexBCU
debugHexBC:
	call	debugHexB
debugHexC:
	push	af
	ld	a,c
	jr	_
debugHexUDE:
	call	debugHexDEU
debugHexDE:
	call	debugHexD
debugHexE:
	push	af
	ld	a,e
	jr	_
debugHexUHL:
	call	debugHexHLU
debugHexHL:
	call	debugHexH
debugHexL:
	push	af
	ld	a,l
	jr	_
debugHex_BC_:
	push	af
	ld	a,(bc)
	jr	_
debugHex_DE_:
	push	af
	ld	a,(de)
	jr	_
debugHex_HL_:
	push	af
	ld	a,(hl)
	jr	_
debugHexA:
	push	af
_:	push	iy
	ld	iy,flags
	push	af
	rrca
	rrca
	rrca
	rrca
	call	_
	pop	af
	call	_
	pop	iy
	pop	af
	ret
_:	or	a,$F0
	daa
	add	a,$A0
	adc	a,$40
	jp	_PutC

debugDecHL:
	push	af
	push	de
	push	hl
	push	iy
	ld	iy,flags
	call	_DispHL_s
	pop	iy
	pop	hl
	pop	de
	pop	af
	ret
debugDecUHL:
	push	af
	push	de
	push	hl
	push	iy
	ld	iy,flags
	call	_DispHL
	pop	iy
	pop	hl
	pop	de
	pop	af
	ret

debugCharA:
	push	iy
	ld	iy,flags
	call	_PutC
	pop	iy
	ret

debugChar:
	ex	(sp), hl
	push	af
	push	iy
	ld	iy,flags
	ld	a,(hl)
	inc	hl
	call	_PutC
	pop	iy
	pop	af
	ex	(sp), hl
	ret

debugStr:
	ex	(sp),hl
	push	af
	push	de
	push	iy
	ld	iy,flags
	call	_PutS
	pop	iy
	pop	de
	pop	af
	ex	(sp),hl
	ret

debugNewLine:
	push	iy
	ld	iy,flags
	call	_NewLine
	pop	iy
	ret

#endif
