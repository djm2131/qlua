;
; QLA_F3_V_meq_M_times_V(QLA_F3_ColorVector *aa, QLA_F3_ColorMatrix *bb, QLA_F3_ColorVector *cc)
;

global QLA_F3_V_meq_M_times_V
QLA_F3_V_meq_M_times_V:
	push		ebp
	mov		ebp,esp
	push		eax
	push		ebx
	push		ecx
	mov		eax,[ebp+8]	; *aa
	mov		ebx,[ebp+12]	; *bb
	mov		ecx,[ebp+16]	; *cc

	movups	xmm0,	[ecx]		;  b1i      b1r       b0i      b0r	<*(cc)>
	movups  xmm1,	[ebx]		; a01i     a01r      a00i     a00r	<*(bb)>
	movups  xmm2,	[ebx]		; a01i     a01r      a00i     a00r	<*(bb)>
	movlps  xmm4,	[ecx+16]	;  b0i      b0r       b2i      b2r	<*(((char *)cc)+16)>
	movups  xmm5,	[ebx+16]	; a10i     a10r      a02i     a02r	<*(((char *)bb)+16)>
	movups  xmm6,	[ebx+16]	; a10i     a10r      a02i     a02r	<*(((char *)bb)+16)>
	mulps   xmm1,	xmm0		; a01i*b1i a01r*b1r  a00i*b0i a00r*b0r
	movlhps	xmm4,	xmm0		; b0i      b0r       X        X
	shufps	xmm2,	xmm2,	0xB1	; a01r     a01i      a00r     a00i
	mulps	xmm2,	xmm0		; a01r*b1i a01i*b1r  a00r*b0i a00i*b0r
	movaps	xmm3,	xmm1		; a01i*b1i a01r*b1r  a00i*b0i a00r*b0r
	movlhps	xmm3,	xmm2		; a00r*b0i a00i*b0r  a00i*b0i a00r*b0r
	mulps	xmm5,	xmm4		; a10i*b0i a10r*b0r  a02i*b2i a02r*b2r
	shufps	xmm6,	xmm6,	0xB1	; a10r     a10i      a02r     a02i
	mulps	xmm6,	xmm4		; a10r*b0i a10i*b0r  a02r*b2i a02i*b2r
	movhlps	xmm2,	xmm1		; a01r*b1i a01i*b1r  a01i*b1i a01r*b1r
	addps	xmm3,	xmm2		; ??
	shufps	xmm4,	xmm4,	0x4E	; b2i      b2r       b0i      b0r
	movhlps	xmm4,	xmm0		; b2i      b2r       b1i      b1r
	movups	xmm1,	[ebx+32]	; a12i     a12r      a11i     a11r	<*(((char *)bb)+32)>
	mulps	xmm1,	xmm4		; a12i*b2i a12r*b2r  a11i*b1i a11r*b1r
	movups	xmm2,	[ebx+32]	; a12i     a12r      a11i     a11r	<*(((char *)bb)+32)>
	shufps	xmm2,	xmm2,	0xB1	; a12r     a12i      a11r     a11i
	mulps	xmm2,	xmm4		; a12r*b2i a12i*b2r  a11r*b1i a11i*b1r
	movaps	xmm7,	xmm5		; a10i*b0i a10r*b0r  a02i*b2i a02r*b2r
	movlhps	xmm5,	xmm6		; a02r*b2i a02i*b2r  a02i*b2i a02r*b2r
	movhlps	xmm6,	xmm7		; a10r*b0i a10i*b0r  a10i*b0i a10r*b0r
	movaps	xmm7,	xmm1		; a12i*b2i a12r*b2r  a11i*b1i a11r*b1r
	addps	xmm3,	xmm5		; a0r.bi   a0i.br    a0i.bi   a0r.br
	movlhps	xmm7,	xmm2		; a11r*b1i a11i*b1r  a11i*b1i a11r*b1r
	movhlps	xmm2,	xmm1		; a12r*b2i a12i*b2r  a12i*b2i a12r*b2r
	addps	xmm7,	xmm2		; ??
	shufps	xmm4,	xmm4,	0xBE	; b2r      b2i       b2i      b2r
	movups	xmm2,	[ebx+48]	; a21i     a21r      a20i     a20r	<*(((char *)bb)+48)>
	addps	xmm7,	xmm6		; a1r.bi   a1i.br    a1i.bi   a1r.br
	movaps	xmm1,	xmm3		; a0r.bi   a0i.br    a0i.bi   a0r.br
	shufps	xmm3,	xmm7,	0xDD	; a1r.bi   a1i.bi    a0r.bi   a0i.bi
	xorps	xmm3,	[sgn13]		; a1r.bi  -a1i.bi    a0r.bi  -a0i.bi	<_sse_sgn13>
	shufps	xmm1,	xmm7,	0x88	; a1i.br   a1r.br    a0i.br   a0r.br
	movups	xmm5,	[ebx+48]	; a21i     a21r      a20i     a20r	<*(((char *)bb)+48)>
	addps	xmm1,	xmm3		; ??
	movlps	xmm6,	[ebx+64]	; X        X         a22i     a22r	<*(((char *)bb)+64)>
	mulps	xmm2,	xmm0		; a21i*b1i a21r*b1r  a20i*b0i a20r*b0r
	shufps	xmm5,	xmm5,	0xB1	; a21r     a21i      a20r     a20i
	mulps	xmm5,	xmm0		; a21r*b1i a21i*b1r  a20r*b0i a20i*b0r
	movlhps	xmm6,	xmm6		; a22i     a22r      a22i     a22r
	mulps	xmm6,	xmm4		; a22i*b2r a22r*b2i  a22i*b2i a22r*b2r
	movaps	xmm0,	xmm2		; a21i*b1i a21r*b1r  a20i*b0i a20r*b0r
	movlhps	xmm2,	xmm5		; a20r*b0i a20i*b0r  a20i*b0i a20r*b0r
	movhlps	xmm5,	xmm0		; a21r*b1i a21i*b1r  a21i*b1i a21r*b1r
	addps	xmm6,	xmm2		; ??
	movups	xmm3,	[eax]		; ??					<*(aa)>
	addps	xmm6,	xmm5		; a2r.bi   a2i.br    a2i.bi   a2r.br
	movaps	xmm7,	xmm6		; a2r.bi   a2i.br    a2i.bi   a2r.br
	shufps	xmm7,	xmm7,	0x0D	; X        X         a2r.bi   a2i.bi
	subps	xmm3,	xmm1		; ??
	shufps	xmm6,	xmm6,	0x08	; X        X         a2i.br   a2r.br
	xorps	xmm7,	[sgn13]		; X        X         a2r.bi  -a2i.bi	<_sse_sgn13>	
	movlps	xmm0,	[eax+16]	; ??					<*(((char *)aa)+16)>
	addps	xmm7,	xmm6		; ??
	movups	[eax],	xmm3		; ??					<*(aa)>
	subps	xmm0,	xmm7		; ??
	movlps	[eax+16],xmm0		; ??					<*(((char *)aa)+16)>

	; *******************************************************************	

here:	pop	ecx
	pop	ebx
	pop	eax
	mov	esp,ebp
	pop	ebp
	ret

	align		16
sgn13:	dd		0x00000000
	dd		0x80000000
	dd		0x00000000
	dd		0x80000000
