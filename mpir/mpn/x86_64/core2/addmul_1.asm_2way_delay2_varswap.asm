dnl  mpn_addmul_1

dnl  Copyright 2010 The Code Cavern

dnl  This file is part of the MPIR Library.

dnl  The MPIR Library is free software; you can redistribute it and/or modify
dnl  it under the terms of the GNU Lesser General Public License as published
dnl  by the Free Software Foundation; either version 2.1 of the License, or (at
dnl  your option) any later version.

dnl  The MPIR Library is distributed in the hope that it will be useful, but
dnl  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
dnl  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
dnl  License for more details.

dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with the MPIR Library; see the file COPYING.LIB.  If not, write
dnl  to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
dnl  Boston, MA 02110-1301, USA.

include(`../config.m4')

C	(rdi,2)= not fully reduced remainder of (rsi,rdx) / divisor , and top limb <d
C	where (rcx,2)  contains B^i % divisor


ASM_START()
PROLOGUE(mpn_addmul_1)
cmp $2,%rdx
je two
jb one
push %rbx
push %r12
push %r13
push %r14
xor %r14,%r14
lea -16(%rdi,%rdx,8),%rdi
lea -16(%rsi,%rdx,8),%rsi
mov $4,%rbx
sub %rdx,%rbx
	
	
	#mul %rcx
	#add %r8,%r10
	#lea (%rax),%r8
	lea (%r14),%r8
	#mov -24(%rsi,%rbx,8),%rax
	#adc %r9,%r11
	#mov %r10,-48(%rdi,%rbx,8)
	#adc $0,%r12
	#lea (%rdx),%r9
	lea (%r14),%r9
	#mul %rcx
	#add %r11,%r13
	#adc %r12,%r8
	#lea (%rax),%r11
	lea (%r14),%r11
	mov -16(%rsi,%rbx,8),%rax
	#mov %r13,-40(%rdi,%rbx,8)
	#mov -32(%rdi,%rbx,8),%r10
	mov $0,%r10
	#adc $0,%r9
	#add $2,%rbx
	#mov -24(%rdi,%rbx,8),%r13
	mov $0,%r13
	#lea (%rdx),%r12
	lea (%r14),%r12
		
	mul %rcx
	add %r8,%r10
	lea (%rax),%r8
	mov -8(%rsi,%rbx,8),%rax
	adc %r9,%r11
	#mov %r10,-32(%rdi,%rbx,8)
	adc $0,%r12
	lea (%rdx),%r9
	mul %rcx
	add %r11,%r13
	adc %r12,%r8
	lea (%rax),%r11
	mov (%rsi,%rbx,8),%rax
	#mov %r13,-24(%rdi,%rbx,8)
	mov -16(%rdi,%rbx,8),%r10
	adc $0,%r9
	#add $2,%rbx
	mov -8(%rdi,%rbx,8),%r13
	lea (%rdx),%r12
cmp $0,%rbx
jge skiplp
ALIGN(16)
lp:	mul %rcx
	add %r8,%r10
	lea (%rax),%r8
	mov 8(%rsi,%rbx,8),%rax
	adc %r9,%r11
	mov %r10,-16(%rdi,%rbx,8)
	adc $0,%r12
	lea (%rdx),%r9
	mul %rcx
	add %r11,%r13
	adc %r12,%r8
	lea (%rax),%r11
	mov 16(%rsi,%rbx,8),%rax
	mov %r13,-8(%rdi,%rbx,8)
	mov (%rdi,%rbx,8),%r10
	adc $0,%r9
	add $2,%rbx
	mov 8-16(%rdi,%rbx,8),%r13
	lea (%rdx),%r12
	jnc lp
skiplp:	jne case0
case1:	mul %rcx
	add %r8,%r10
	lea (%rax),%r8
	mov 8(%rsi,%rbx,8),%rax
	adc %r9,%r11
	mov %r10,-16(%rdi,%rbx,8)
	adc $0,%r12
	lea (%rdx),%r9
	mul %rcx
	add %r11,%r13
	adc %r12,%r8
	lea (%rax),%r11
	#mov 16(%rsi,%rbx,8),%rax
	mov %r13,-8(%rdi,%rbx,8)
	mov (%rdi,%rbx,8),%r10
	adc $0,%r9
	#add $2,%rbx
	mov 8(%rdi,%rbx,8),%r13
	lea (%rdx),%r12

	#mul %rcx
	add %r8,%r10
	#lea (%rax),%r8
	lea (%r14),%r8
	#mov 8(%rsi,%rbx,8),%rax
	adc %r9,%r11
	mov %r10,(%rdi,%rbx,8)
	adc $0,%r12
	#lea (%rdx),%r9
	#lea (%r14),%r9
	#mul %rcx
	add %r11,%r13
	adc %r12,%r8
	#lea (%rax),%r11
	#mov 16(%rsi,%rbx,8),%rax
	mov %r13,8(%rdi,%rbx,8)
	#mov 16(%rdi,%rbx,8),%r10
	mov $0,%r10
	#adc $0,%r9
	#add $2,%rbx
	#mov 24(%rdi,%rbx,8),%r13
	#lea (%rdx),%r12

	#mul %rcx
	add %r8,%r10
	#lea (%rax),%r8
	#mov 8(%rsi,%rbx,8),%rax
	#adc %r9,%r11
	mov %r10,%rax
	pop %r14
	pop %r13
	pop %r12
	pop %rbx
	ret

case0:	mul %rcx
	add %r8,%r10
	lea (%rax),%r8
	#mov 8(%rsi,%rbx,8),%rax
	adc %r9,%r11
	mov %r10,-16(%rdi,%rbx,8)
	adc $0,%r12
	lea (%rdx),%r9
	#mul %rcx
	add %r11,%r13
	adc %r12,%r8
	#lea (%rax),%r11
	lea (%r14),%r11
	#mov 16(%rsi,%rbx,8),%rax
	mov %r13,-8(%rdi,%rbx,8)
	mov (%rdi,%rbx,8),%r10
	adc $0,%r9
	#add $2,%rbx
	#mov 8(%rdi,%rbx,8),%r13
	mov $0,%r13
	#lea (%rdx),%r12
	#lea (%r14),%r12

	#mul %rcx
	add %r8,%r10
	#lea (%rax),%r8
	#lea (%r14),$r8
	#mov 8(%rsi,%rbx,8),%rax
	adc %r9,%r11
	mov %r10,(%rdi,%rbx,8)
	#adc $0,%r12
	#lea (%rdx),%r9
	#lea (%r14),%r9
	#mul %rcx
	add %r11,%r13
	#adc %r12,%r8
	#lea (%rax),%r11
	#mov 16(%rsi,%rbx,8),%rax
	mov %r13,%rax
	#mov 16(%rdi,%rbx,8),%r10
	#adc $0,%r9
	#add $2,%rbx
	#mov 24(%rdi,%rbx,8),%r13
	#lea (%rdx),%r12

	#mul %rcx
	#add %r8,%r10
	#lea (%rax),%r8
	#mov 8(%rsi,%rbx,8),%rax
	#adc %r9,%r11
	#mov %r10,%rax
	pop %r14
	pop %r13
	pop %r12
	pop %rbx
	ret
one:	mov (%rsi),%rax
	mul %rcx
	add %rax,(%rdi)
	adc $0,%rdx
	mov %rdx,%rax
	ret
two:

mov (%rsi),%rax
mul %rcx
mov %rax,%r8
mov 8(%rsi),%rax
mov %rdx,%r9
xor %r10,%r10
mul %rcx
add %r8,(%rdi)
adc %rax,%r9
adc %rdx,%r10
add %r9,8(%rdi)
adc $0,%r10
mov %r10,%rax
ret
EPILOGUE()



