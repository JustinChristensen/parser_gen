	.text
	.file	"nfa.c"
	.file	1 "/home/wroathe/compilers/src/auto" "nfa.c"
	.file	2 "/home/wroathe/compilers/src/auto" "./result_types.h"
	.globl	noop_nfa                # -- Begin function noop_nfa
	.p2align	4, 0x90
	.type	noop_nfa,@function
noop_nfa:                               # @noop_nfa
.Lfunc_begin0:
	.loc	1 312 0                 # nfa.c:312:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
.Ltmp0:
	.loc	1 312 59 prologue_end   # nfa.c:312:59
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp1:
.Lfunc_end0:
	.size	noop_nfa, .Lfunc_end0-noop_nfa
	.cfi_endproc
                                        # -- End function
	.globl	do_empty_nfa            # -- Begin function do_empty_nfa
	.p2align	4, 0x90
	.type	do_empty_nfa,@function
do_empty_nfa:                           # @do_empty_nfa
.Lfunc_begin1:
	.loc	1 314 0                 # nfa.c:314:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$64, %rsp
	movq	%rdi, -8(%rbp)
.Ltmp2:
	.loc	1 315 14 prologue_end   # nfa.c:315:14
	movq	-8(%rbp), %rdi
	.loc	1 315 37 is_stmt 0      # nfa.c:315:37
	movq	-8(%rbp), %rsi
	.loc	1 315 23                # nfa.c:315:23
	leaq	-32(%rbp), %rax
	movq	%rdi, -40(%rbp)         # 8-byte Spill
	movq	%rax, %rdi
	callq	empty_machine
	movq	-40(%rbp), %rdi         # 8-byte Reload
	.loc	1 315 5                 # nfa.c:315:5
	leaq	-32(%rbp), %rax
	movq	(%rax), %rsi
	movq	%rsi, (%rsp)
	movq	8(%rax), %rsi
	movq	%rsi, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	smachine
	.loc	1 316 1 is_stmt 1       # nfa.c:316:1
	addq	$64, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp3:
.Lfunc_end1:
	.size	do_empty_nfa, .Lfunc_end1-do_empty_nfa
	.cfi_endproc
                                        # -- End function
	.globl	do_alt_nfa              # -- Begin function do_alt_nfa
	.p2align	4, 0x90
	.type	do_alt_nfa,@function
do_alt_nfa:                             # @do_alt_nfa
.Lfunc_begin2:
	.loc	1 318 0                 # nfa.c:318:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$128, %rsp
	leaq	16(%rbp), %rax
	movq	%rdi, -8(%rbp)
.Ltmp4:
	.loc	1 319 14 prologue_end   # nfa.c:319:14
	movq	-8(%rbp), %rdi
	.loc	1 319 35 is_stmt 0      # nfa.c:319:35
	movq	-8(%rbp), %rsi
	.loc	1 319 64                # nfa.c:319:64
	movq	-8(%rbp), %rcx
	.loc	1 319 55                # nfa.c:319:55
	leaq	-56(%rbp), %rdx
	movq	%rdi, -64(%rbp)         # 8-byte Spill
	movq	%rdx, %rdi
	movq	%rsi, -72(%rbp)         # 8-byte Spill
	movq	%rcx, %rsi
	movq	%rax, -80(%rbp)         # 8-byte Spill
	callq	gmachine
	.loc	1 319 23                # nfa.c:319:23
	leaq	-32(%rbp), %rdi
	movq	-72(%rbp), %rsi         # 8-byte Reload
	movq	-80(%rbp), %rax         # 8-byte Reload
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rcx
	movq	%rcx, 16(%rsp)
	leaq	-56(%rbp), %rcx
	movq	(%rcx), %rdx
	movq	%rdx, 24(%rsp)
	movq	8(%rcx), %rdx
	movq	%rdx, 32(%rsp)
	movq	16(%rcx), %rcx
	movq	%rcx, 40(%rsp)
	callq	alt_machine
	movq	-64(%rbp), %rdi         # 8-byte Reload
	.loc	1 319 5                 # nfa.c:319:5
	leaq	-32(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	smachine
	.loc	1 320 1 is_stmt 1       # nfa.c:320:1
	addq	$128, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp5:
.Lfunc_end2:
	.size	do_alt_nfa, .Lfunc_end2-do_alt_nfa
	.cfi_endproc
                                        # -- End function
	.globl	do_cat_nfa              # -- Begin function do_cat_nfa
	.p2align	4, 0x90
	.type	do_cat_nfa,@function
do_cat_nfa:                             # @do_cat_nfa
.Lfunc_begin3:
	.loc	1 322 0                 # nfa.c:322:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$128, %rsp
	leaq	16(%rbp), %rax
	movq	%rdi, -8(%rbp)
.Ltmp6:
	.loc	1 323 14 prologue_end   # nfa.c:323:14
	movq	-8(%rbp), %rdi
	.loc	1 323 55 is_stmt 0      # nfa.c:323:55
	movq	-8(%rbp), %rsi
	.loc	1 323 46                # nfa.c:323:46
	leaq	-56(%rbp), %rcx
	movq	%rdi, -64(%rbp)         # 8-byte Spill
	movq	%rcx, %rdi
	movq	%rax, -72(%rbp)         # 8-byte Spill
	callq	gmachine
	.loc	1 323 23                # nfa.c:323:23
	leaq	-32(%rbp), %rdi
	movq	-72(%rbp), %rax         # 8-byte Reload
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rcx
	movq	%rcx, 16(%rsp)
	leaq	-56(%rbp), %rcx
	movq	(%rcx), %rsi
	movq	%rsi, 24(%rsp)
	movq	8(%rcx), %rsi
	movq	%rsi, 32(%rsp)
	movq	16(%rcx), %rcx
	movq	%rcx, 40(%rsp)
	callq	cat_machine
	movq	-64(%rbp), %rdi         # 8-byte Reload
	.loc	1 323 5                 # nfa.c:323:5
	leaq	-32(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	smachine
	.loc	1 324 1 is_stmt 1       # nfa.c:324:1
	addq	$128, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp7:
.Lfunc_end3:
	.size	do_cat_nfa, .Lfunc_end3-do_cat_nfa
	.cfi_endproc
                                        # -- End function
	.globl	do_dotall_nfa           # -- Begin function do_dotall_nfa
	.p2align	4, 0x90
	.type	do_dotall_nfa,@function
do_dotall_nfa:                          # @do_dotall_nfa
.Lfunc_begin4:
	.loc	1 326 0                 # nfa.c:326:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$64, %rsp
	movq	%rdi, -8(%rbp)
.Ltmp8:
	.loc	1 327 14 prologue_end   # nfa.c:327:14
	movq	-8(%rbp), %rdi
	.loc	1 327 38 is_stmt 0      # nfa.c:327:38
	movq	-8(%rbp), %rsi
	.loc	1 327 23                # nfa.c:327:23
	leaq	-32(%rbp), %rax
	movq	%rdi, -40(%rbp)         # 8-byte Spill
	movq	%rax, %rdi
	callq	dotall_machine
	movq	-40(%rbp), %rdi         # 8-byte Reload
	.loc	1 327 5                 # nfa.c:327:5
	leaq	-32(%rbp), %rax
	movq	(%rax), %rsi
	movq	%rsi, (%rsp)
	movq	8(%rax), %rsi
	movq	%rsi, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	smachine
	.loc	1 328 1 is_stmt 1       # nfa.c:328:1
	addq	$64, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp9:
.Lfunc_end4:
	.size	do_dotall_nfa, .Lfunc_end4-do_dotall_nfa
	.cfi_endproc
                                        # -- End function
	.globl	do_symbol_nfa           # -- Begin function do_symbol_nfa
	.p2align	4, 0x90
	.type	do_symbol_nfa,@function
do_symbol_nfa:                          # @do_symbol_nfa
.Lfunc_begin5:
	.loc	1 330 0                 # nfa.c:330:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$64, %rsp
	leaq	16(%rbp), %rax
	movq	%rdi, -8(%rbp)
.Ltmp10:
	.loc	1 331 14 prologue_end   # nfa.c:331:14
	movq	-8(%rbp), %rdi
	.loc	1 331 38 is_stmt 0      # nfa.c:331:38
	movq	-8(%rbp), %rsi
	.loc	1 331 23                # nfa.c:331:23
	leaq	-32(%rbp), %rcx
	movq	%rdi, -40(%rbp)         # 8-byte Spill
	movq	%rcx, %rdi
	movsbl	(%rax), %edx
	callq	symbol_machine
	movq	-40(%rbp), %rdi         # 8-byte Reload
	.loc	1 331 5                 # nfa.c:331:5
	leaq	-32(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	smachine
	.loc	1 332 1 is_stmt 1       # nfa.c:332:1
	addq	$64, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp11:
.Lfunc_end5:
	.size	do_symbol_nfa, .Lfunc_end5-do_symbol_nfa
	.cfi_endproc
                                        # -- End function
	.globl	do_star_nfa             # -- Begin function do_star_nfa
	.p2align	4, 0x90
	.type	do_star_nfa,@function
do_star_nfa:                            # @do_star_nfa
.Lfunc_begin6:
	.loc	1 334 0                 # nfa.c:334:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	movq	%rdi, -8(%rbp)
.Ltmp12:
	.loc	1 335 14 prologue_end   # nfa.c:335:14
	movq	-8(%rbp), %rdi
	.loc	1 335 39 is_stmt 0      # nfa.c:335:39
	movq	-8(%rbp), %rsi
	.loc	1 335 57                # nfa.c:335:57
	movq	-8(%rbp), %rax
	.loc	1 335 48                # nfa.c:335:48
	leaq	-56(%rbp), %rcx
	movq	%rdi, -64(%rbp)         # 8-byte Spill
	movq	%rcx, %rdi
	movq	%rsi, -72(%rbp)         # 8-byte Spill
	movq	%rax, %rsi
	callq	gmachine
	.loc	1 335 23                # nfa.c:335:23
	leaq	-32(%rbp), %rdi
	movq	-72(%rbp), %rsi         # 8-byte Reload
	leaq	-56(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	closure_machine
	movq	-64(%rbp), %rdi         # 8-byte Reload
	.loc	1 335 5                 # nfa.c:335:5
	leaq	-32(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	smachine
	.loc	1 336 1 is_stmt 1       # nfa.c:336:1
	addq	$96, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp13:
.Lfunc_end6:
	.size	do_star_nfa, .Lfunc_end6-do_star_nfa
	.cfi_endproc
                                        # -- End function
	.globl	do_plus_nfa             # -- Begin function do_plus_nfa
	.p2align	4, 0x90
	.type	do_plus_nfa,@function
do_plus_nfa:                            # @do_plus_nfa
.Lfunc_begin7:
	.loc	1 338 0                 # nfa.c:338:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	movq	%rdi, -8(%rbp)
.Ltmp14:
	.loc	1 339 14 prologue_end   # nfa.c:339:14
	movq	-8(%rbp), %rdi
	.loc	1 339 42 is_stmt 0      # nfa.c:339:42
	movq	-8(%rbp), %rsi
	.loc	1 339 60                # nfa.c:339:60
	movq	-8(%rbp), %rax
	.loc	1 339 51                # nfa.c:339:51
	leaq	-56(%rbp), %rcx
	movq	%rdi, -64(%rbp)         # 8-byte Spill
	movq	%rcx, %rdi
	movq	%rsi, -72(%rbp)         # 8-byte Spill
	movq	%rax, %rsi
	callq	gmachine
	.loc	1 339 23                # nfa.c:339:23
	leaq	-32(%rbp), %rdi
	movq	-72(%rbp), %rsi         # 8-byte Reload
	leaq	-56(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	posclosure_machine
	movq	-64(%rbp), %rdi         # 8-byte Reload
	.loc	1 339 5                 # nfa.c:339:5
	leaq	-32(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	smachine
	.loc	1 340 1 is_stmt 1       # nfa.c:340:1
	addq	$96, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp15:
.Lfunc_end7:
	.size	do_plus_nfa, .Lfunc_end7-do_plus_nfa
	.cfi_endproc
                                        # -- End function
	.globl	do_optional_nfa         # -- Begin function do_optional_nfa
	.p2align	4, 0x90
	.type	do_optional_nfa,@function
do_optional_nfa:                        # @do_optional_nfa
.Lfunc_begin8:
	.loc	1 342 0                 # nfa.c:342:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	movq	%rdi, -8(%rbp)
.Ltmp16:
	.loc	1 343 14 prologue_end   # nfa.c:343:14
	movq	-8(%rbp), %rdi
	.loc	1 343 40 is_stmt 0      # nfa.c:343:40
	movq	-8(%rbp), %rsi
	.loc	1 343 58                # nfa.c:343:58
	movq	-8(%rbp), %rax
	.loc	1 343 49                # nfa.c:343:49
	leaq	-56(%rbp), %rcx
	movq	%rdi, -64(%rbp)         # 8-byte Spill
	movq	%rcx, %rdi
	movq	%rsi, -72(%rbp)         # 8-byte Spill
	movq	%rax, %rsi
	callq	gmachine
	.loc	1 343 23                # nfa.c:343:23
	leaq	-32(%rbp), %rdi
	movq	-72(%rbp), %rsi         # 8-byte Reload
	leaq	-56(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	optional_machine
	movq	-64(%rbp), %rdi         # 8-byte Reload
	.loc	1 343 5                 # nfa.c:343:5
	leaq	-32(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	smachine
	.loc	1 344 1 is_stmt 1       # nfa.c:344:1
	addq	$96, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp17:
.Lfunc_end8:
	.size	do_optional_nfa, .Lfunc_end8-do_optional_nfa
	.cfi_endproc
                                        # -- End function
	.globl	nfa_context             # -- Begin function nfa_context
	.p2align	4, 0x90
	.type	nfa_context,@function
nfa_context:                            # @nfa_context
.Lfunc_begin9:
	.loc	1 26 0                  # nfa.c:26:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, %rax
	movq	%rsi, -8(%rbp)
.Ltmp18:
	.loc	1 28 21 prologue_end    # nfa.c:28:21
	movq	-8(%rbp), %rsi
	.loc	1 27 33                 # nfa.c:27:33
	movq	%rsi, (%rdi)
	movq	$0, 8(%rdi)
	xorps	%xmm0, %xmm0
	movups	%xmm0, 16(%rdi)
	movq	$0, 32(%rdi)
	movb	$0, 40(%rdi)
	xorl	%ecx, %ecx
	movb	%cl, %dl
	movq	%rax, -32(%rbp)         # 8-byte Spill
	.loc	1 31 20                 # nfa.c:31:20
	movb	%dl, %al
	movq	%rdi, -40(%rbp)         # 8-byte Spill
	callq	nullperr
	movl	%edx, -16(%rbp)
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	-40(%rbp), %rsi         # 8-byte Reload
	movq	%rax, 44(%rsi)
	movl	-16(%rbp), %ecx
	movl	%ecx, 52(%rsi)
	movq	-32(%rbp), %rax         # 8-byte Reload
	.loc	1 27 5                  # nfa.c:27:5
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp19:
.Lfunc_end9:
	.size	nfa_context, .Lfunc_end9-nfa_context
	.cfi_endproc
                                        # -- End function
	.globl	accepting_state         # -- Begin function accepting_state
	.p2align	4, 0x90
	.type	accepting_state,@function
accepting_state:                        # @accepting_state
.Lfunc_begin10:
	.loc	1 35 0                  # nfa.c:35:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, %rax
	xorl	%esi, %esi
.Ltmp20:
	.loc	1 36 12 prologue_end    # nfa.c:36:12
	movq	%rdi, %rcx
	movq	%rdi, -8(%rbp)          # 8-byte Spill
	movq	%rcx, %rdi
	movl	$24, %edx
	movq	%rax, -16(%rbp)         # 8-byte Spill
	callq	memset
	movq	-8(%rbp), %rax          # 8-byte Reload
	.loc	1 36 31 is_stmt 0       # nfa.c:36:31
	movl	$0, (%rax)
	movq	-16(%rbp), %rax         # 8-byte Reload
	.loc	1 36 5                  # nfa.c:36:5
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp21:
.Lfunc_end10:
	.size	accepting_state, .Lfunc_end10-accepting_state
	.cfi_endproc
                                        # -- End function
	.globl	epsilon_state           # -- Begin function epsilon_state
	.p2align	4, 0x90
	.type	epsilon_state,@function
epsilon_state:                          # @epsilon_state
.Lfunc_begin11:
	.loc	1 41 0 is_stmt 1        # nfa.c:41:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, %rax
	movq	%rsi, -8(%rbp)
.Ltmp22:
	.loc	1 42 31 prologue_end    # nfa.c:42:31
	movl	$1, (%rdi)
	movl	$0, 4(%rdi)
	.loc	1 44 17                 # nfa.c:44:17
	movq	-8(%rbp), %rsi
	.loc	1 44 9 is_stmt 0        # nfa.c:44:9
	movq	%rsi, 8(%rdi)
	movb	$0, 16(%rdi)
	.loc	1 42 5 is_stmt 1        # nfa.c:42:5
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp23:
.Lfunc_end11:
	.size	epsilon_state, .Lfunc_end11-epsilon_state
	.cfi_endproc
                                        # -- End function
	.globl	dotall_state            # -- Begin function dotall_state
	.p2align	4, 0x90
	.type	dotall_state,@function
dotall_state:                           # @dotall_state
.Lfunc_begin12:
	.loc	1 48 0                  # nfa.c:48:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, %rax
	movq	%rsi, -8(%rbp)
.Ltmp24:
	.loc	1 49 31 prologue_end    # nfa.c:49:31
	movl	$4, (%rdi)
	movl	$0, 4(%rdi)
	.loc	1 51 17                 # nfa.c:51:17
	movq	-8(%rbp), %rsi
	.loc	1 51 9 is_stmt 0        # nfa.c:51:9
	movq	%rsi, 8(%rdi)
	movb	$0, 16(%rdi)
	.loc	1 49 5 is_stmt 1        # nfa.c:49:5
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp25:
.Lfunc_end12:
	.size	dotall_state, .Lfunc_end12-dotall_state
	.cfi_endproc
                                        # -- End function
	.globl	branch_state            # -- Begin function branch_state
	.p2align	4, 0x90
	.type	branch_state,@function
branch_state:                           # @branch_state
.Lfunc_begin13:
	.loc	1 55 0                  # nfa.c:55:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, %rax
	movq	%rsi, -8(%rbp)
	movq	%rdx, -16(%rbp)
.Ltmp26:
	.loc	1 56 31 prologue_end    # nfa.c:56:31
	movl	$2, (%rdi)
	movl	$0, 4(%rdi)
	.loc	1 58 17                 # nfa.c:58:17
	movq	-8(%rbp), %rdx
	.loc	1 58 9 is_stmt 0        # nfa.c:58:9
	movq	%rdx, 8(%rdi)
	.loc	1 59 18 is_stmt 1       # nfa.c:59:18
	movq	-16(%rbp), %rdx
	.loc	1 58 9                  # nfa.c:58:9
	movq	%rdx, 16(%rdi)
	.loc	1 56 5                  # nfa.c:56:5
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp27:
.Lfunc_end13:
	.size	branch_state, .Lfunc_end13-branch_state
	.cfi_endproc
                                        # -- End function
	.globl	symbol_state            # -- Begin function symbol_state
	.p2align	4, 0x90
	.type	symbol_state,@function
symbol_state:                           # @symbol_state
.Lfunc_begin14:
	.loc	1 63 0                  # nfa.c:63:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movb	%sil, %al
	movq	%rdi, %rcx
	xorl	%esi, %esi
	movb	%al, -1(%rbp)
.Ltmp28:
	.loc	1 64 12 prologue_end    # nfa.c:64:12
	movq	%rdi, %rdx
	movq	%rdi, -16(%rbp)         # 8-byte Spill
	movq	%rdx, %rdi
	movl	$24, %edx
	movq	%rcx, -24(%rbp)         # 8-byte Spill
	callq	memset
	movq	-16(%rbp), %rcx         # 8-byte Reload
	.loc	1 64 31 is_stmt 0       # nfa.c:64:31
	movl	$3, (%rcx)
	.loc	1 66 19 is_stmt 1       # nfa.c:66:19
	movb	-1(%rbp), %al
	.loc	1 66 9 is_stmt 0        # nfa.c:66:9
	movb	%al, 16(%rcx)
	movq	-24(%rbp), %rax         # 8-byte Reload
	.loc	1 64 5 is_stmt 1        # nfa.c:64:5
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp29:
.Lfunc_end14:
	.size	symbol_state, .Lfunc_end14-symbol_state
	.cfi_endproc
                                        # -- End function
	.globl	setst                   # -- Begin function setst
	.p2align	4, 0x90
	.type	setst,@function
setst:                                  # @setst
.Lfunc_begin15:
	.loc	1 71 0                  # nfa.c:71:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	leaq	16(%rbp), %rax
	movq	%rdi, -8(%rbp)
.Ltmp30:
	.loc	1 72 19 prologue_end    # nfa.c:72:19
	movl	uid, %ecx
	movl	%ecx, %edx
	addl	$1, %edx
	movl	%edx, uid
	.loc	1 72 14 is_stmt 0       # nfa.c:72:14
	movl	%ecx, 4(%rax)
	.loc	1 73 6 is_stmt 1        # nfa.c:73:6
	movq	-8(%rbp), %rdi
	.loc	1 73 15 is_stmt 0       # nfa.c:73:15
	movq	(%rdi), %rdi
	.loc	1 73 26                 # nfa.c:73:26
	movq	(%rax), %rsi
	movq	%rsi, (%rdi)
	movq	8(%rax), %rsi
	movq	%rsi, 8(%rdi)
	movq	16(%rax), %rax
	movq	%rax, 16(%rdi)
	.loc	1 74 5 is_stmt 1        # nfa.c:74:5
	movq	-8(%rbp), %rax
	.loc	1 74 23 is_stmt 0       # nfa.c:74:23
	movq	8(%rax), %rsi
	addq	$1, %rsi
	movq	%rsi, 8(%rax)
	.loc	1 75 12 is_stmt 1       # nfa.c:75:12
	movq	-8(%rbp), %rax
	.loc	1 75 29 is_stmt 0       # nfa.c:75:29
	movq	(%rax), %rsi
	movq	%rsi, %rdi
	addq	$24, %rdi
	movq	%rdi, (%rax)
	.loc	1 75 5                  # nfa.c:75:5
	movq	%rsi, %rax
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp31:
.Lfunc_end15:
	.size	setst, .Lfunc_end15-setst
	.cfi_endproc
                                        # -- End function
	.globl	point                   # -- Begin function point
	.p2align	4, 0x90
	.type	point,@function
point:                                  # @point
.Lfunc_begin16:
	.loc	1 78 0 is_stmt 1        # nfa.c:78:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
.Ltmp32:
	.loc	1 79 20 prologue_end    # nfa.c:79:20
	movq	-16(%rbp), %rdx
	.loc	1 79 5 is_stmt 0        # nfa.c:79:5
	movq	-8(%rbp), %rsi
	.loc	1 79 18                 # nfa.c:79:18
	movq	%rdx, 8(%rsi)
	.loc	1 80 21 is_stmt 1       # nfa.c:80:21
	movq	-24(%rbp), %rdx
	.loc	1 80 5 is_stmt 0        # nfa.c:80:5
	movq	-8(%rbp), %rsi
	.loc	1 80 19                 # nfa.c:80:19
	movq	%rdx, 16(%rsi)
	.loc	1 81 1 is_stmt 1        # nfa.c:81:1
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp33:
.Lfunc_end16:
	.size	point, .Lfunc_end16-point
	.cfi_endproc
                                        # -- End function
	.globl	patch                   # -- Begin function patch
	.p2align	4, 0x90
	.type	patch,@function
patch:                                  # @patch
.Lfunc_begin17:
	.loc	1 83 0                  # nfa.c:83:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	leaq	16(%rbp), %rax
	movq	%rdi, -8(%rbp)
.Ltmp34:
	.loc	1 84 20 prologue_end    # nfa.c:84:20
	movq	-8(%rbp), %rdi
	.loc	1 84 14 is_stmt 0       # nfa.c:84:14
	movq	8(%rax), %rcx
	.loc	1 84 18                 # nfa.c:84:18
	movq	%rdi, (%rcx)
.Ltmp35:
	.loc	1 85 9 is_stmt 1        # nfa.c:85:9
	cmpq	$0, 16(%rax)
	movq	%rax, -16(%rbp)         # 8-byte Spill
.Ltmp36:
	.loc	1 85 9 is_stmt 0        # nfa.c:85:9
	je	.LBB17_2
# %bb.1:
	.loc	1 0 9                   # nfa.c:0:9
	movq	-16(%rbp), %rax         # 8-byte Reload
.Ltmp37:
	.loc	1 85 48                 # nfa.c:85:48
	movq	8(%rax), %rcx
	.loc	1 85 39                 # nfa.c:85:39
	movq	(%rcx), %rcx
	.loc	1 85 32                 # nfa.c:85:32
	movq	16(%rax), %rdx
	.loc	1 85 37                 # nfa.c:85:37
	movq	%rcx, (%rdx)
.Ltmp38:
.LBB17_2:
	.loc	1 86 1 is_stmt 1        # nfa.c:86:1
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp39:
.Lfunc_end17:
	.size	patch, .Lfunc_end17-patch
	.cfi_endproc
                                        # -- End function
	.globl	smachine                # -- Begin function smachine
	.p2align	4, 0x90
	.type	smachine,@function
smachine:                               # @smachine
.Lfunc_begin18:
	.loc	1 88 0                  # nfa.c:88:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	leaq	16(%rbp), %rax
	movq	%rdi, -8(%rbp)
.Ltmp40:
	.loc	1 89 5 prologue_end     # nfa.c:89:5
	movq	-8(%rbp), %rdi
	.loc	1 89 20 is_stmt 0       # nfa.c:89:20
	movq	(%rax), %rcx
	movq	%rcx, 16(%rdi)
	movq	8(%rax), %rcx
	movq	%rcx, 24(%rdi)
	movq	16(%rax), %rax
	movq	%rax, 32(%rdi)
	.loc	1 90 1 is_stmt 1        # nfa.c:90:1
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp41:
.Lfunc_end18:
	.size	smachine, .Lfunc_end18-smachine
	.cfi_endproc
                                        # -- End function
	.globl	gmachine                # -- Begin function gmachine
	.p2align	4, 0x90
	.type	gmachine,@function
gmachine:                               # @gmachine
.Lfunc_begin19:
	.loc	1 92 0                  # nfa.c:92:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, %rax
	movq	%rsi, -8(%rbp)
.Ltmp42:
	.loc	1 93 12 prologue_end    # nfa.c:93:12
	movq	-8(%rbp), %rsi
	.loc	1 93 21 is_stmt 0       # nfa.c:93:21
	movq	16(%rsi), %rcx
	movq	%rcx, (%rdi)
	movq	24(%rsi), %rcx
	movq	%rcx, 8(%rdi)
	movq	32(%rsi), %rcx
	movq	%rcx, 16(%rdi)
	.loc	1 93 5                  # nfa.c:93:5
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp43:
.Lfunc_end19:
	.size	gmachine, .Lfunc_end19-gmachine
	.cfi_endproc
                                        # -- End function
	.globl	nfa_to_rval             # -- Begin function nfa_to_rval
	.p2align	4, 0x90
	.type	nfa_to_rval,@function
nfa_to_rval:                            # @nfa_to_rval
.Lfunc_begin20:
	.loc	1 96 0 is_stmt 1        # nfa.c:96:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, %rax
	movq	%rsi, -8(%rbp)
.Ltmp44:
	.loc	1 97 44 prologue_end    # nfa.c:97:44
	movq	-8(%rbp), %rsi
	movq	%rax, -16(%rbp)         # 8-byte Spill
	.loc	1 97 35 is_stmt 0       # nfa.c:97:35
	callq	gmachine
	movq	-16(%rbp), %rax         # 8-byte Reload
	.loc	1 97 5                  # nfa.c:97:5
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp45:
.Lfunc_end20:
	.size	nfa_to_rval, .Lfunc_end20-nfa_to_rval
	.cfi_endproc
                                        # -- End function
	.globl	empty_machine           # -- Begin function empty_machine
	.p2align	4, 0x90
	.type	empty_machine,@function
empty_machine:                          # @empty_machine
.Lfunc_begin21:
	.loc	1 100 0 is_stmt 1       # nfa.c:100:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$80, %rsp
	movq	%rdi, %rax
	xorl	%ecx, %ecx
	movl	%ecx, %edx
	movq	%rsi, -8(%rbp)
.Ltmp46:
	#DEBUG_VALUE: empty_machine:machine <- [$rdi+0]
	.loc	1 105 27 prologue_end   # nfa.c:105:27
	movq	-8(%rbp), %rsi
	.loc	1 105 36 is_stmt 0      # nfa.c:105:36
	leaq	-32(%rbp), %r8
	movq	%rdi, -40(%rbp)         # 8-byte Spill
.Ltmp47:
	#DEBUG_VALUE: empty_machine:machine <- [DW_OP_constu 40, DW_OP_minus, DW_OP_deref] [$rbp+0]
	movq	%r8, %rdi
	movq	%rsi, -48(%rbp)         # 8-byte Spill
	movq	%rdx, %rsi
	movq	%rax, -56(%rbp)         # 8-byte Spill
	callq	epsilon_state
	movq	-48(%rbp), %rdi         # 8-byte Reload
	.loc	1 105 21                # nfa.c:105:21
	leaq	-32(%rbp), %rax
	movq	(%rax), %rdx
	movq	%rdx, (%rsp)
	movq	8(%rax), %rdx
	movq	%rdx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	setst
	xorl	%ecx, %ecx
	movl	%ecx, %edx
	movq	-40(%rbp), %rsi         # 8-byte Reload
	.loc	1 105 19                # nfa.c:105:19
	movq	%rax, (%rsi)
	.loc	1 106 30 is_stmt 1      # nfa.c:106:30
	movq	(%rsi), %rax
	.loc	1 106 37 is_stmt 0      # nfa.c:106:37
	addq	$8, %rax
	.loc	1 106 5                 # nfa.c:106:5
	movq	%rsi, %rdi
	movq	%rax, %rsi
	callq	point
	movq	-56(%rbp), %rax         # 8-byte Reload
	.loc	1 107 5 is_stmt 1       # nfa.c:107:5
	addq	$80, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp48:
.Lfunc_end21:
	.size	empty_machine, .Lfunc_end21-empty_machine
	.cfi_endproc
                                        # -- End function
	.globl	dotall_machine          # -- Begin function dotall_machine
	.p2align	4, 0x90
	.type	dotall_machine,@function
dotall_machine:                         # @dotall_machine
.Lfunc_begin22:
	.loc	1 110 0                 # nfa.c:110:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$80, %rsp
	movq	%rdi, %rax
	xorl	%ecx, %ecx
	movl	%ecx, %edx
	movq	%rsi, -8(%rbp)
.Ltmp49:
	#DEBUG_VALUE: dotall_machine:machine <- [$rdi+0]
	.loc	1 115 27 prologue_end   # nfa.c:115:27
	movq	-8(%rbp), %rsi
	.loc	1 115 36 is_stmt 0      # nfa.c:115:36
	leaq	-32(%rbp), %r8
	movq	%rdi, -40(%rbp)         # 8-byte Spill
.Ltmp50:
	#DEBUG_VALUE: dotall_machine:machine <- [DW_OP_constu 40, DW_OP_minus, DW_OP_deref] [$rbp+0]
	movq	%r8, %rdi
	movq	%rsi, -48(%rbp)         # 8-byte Spill
	movq	%rdx, %rsi
	movq	%rax, -56(%rbp)         # 8-byte Spill
	callq	dotall_state
	movq	-48(%rbp), %rdi         # 8-byte Reload
	.loc	1 115 21                # nfa.c:115:21
	leaq	-32(%rbp), %rax
	movq	(%rax), %rdx
	movq	%rdx, (%rsp)
	movq	8(%rax), %rdx
	movq	%rdx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	setst
	xorl	%ecx, %ecx
	movl	%ecx, %edx
	movq	-40(%rbp), %rsi         # 8-byte Reload
	.loc	1 115 19                # nfa.c:115:19
	movq	%rax, (%rsi)
	.loc	1 116 30 is_stmt 1      # nfa.c:116:30
	movq	(%rsi), %rax
	.loc	1 116 37 is_stmt 0      # nfa.c:116:37
	addq	$8, %rax
	.loc	1 116 5                 # nfa.c:116:5
	movq	%rsi, %rdi
	movq	%rax, %rsi
	callq	point
	movq	-56(%rbp), %rax         # 8-byte Reload
	.loc	1 117 5 is_stmt 1       # nfa.c:117:5
	addq	$80, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp51:
.Lfunc_end22:
	.size	dotall_machine, .Lfunc_end22-dotall_machine
	.cfi_endproc
                                        # -- End function
	.globl	symbol_machine          # -- Begin function symbol_machine
	.p2align	4, 0x90
	.type	symbol_machine,@function
symbol_machine:                         # @symbol_machine
.Lfunc_begin23:
	.loc	1 120 0                 # nfa.c:120:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	movb	%dl, %al
	movq	%rdi, %rcx
	movq	%rsi, -8(%rbp)
	movb	%al, -9(%rbp)
.Ltmp52:
	#DEBUG_VALUE: symbol_machine:machine <- [$rdi+0]
	.loc	1 125 27 prologue_end   # nfa.c:125:27
	movq	-8(%rbp), %rsi
	.loc	1 125 36 is_stmt 0      # nfa.c:125:36
	leaq	-40(%rbp), %r8
	movq	%rdi, -48(%rbp)         # 8-byte Spill
.Ltmp53:
	#DEBUG_VALUE: symbol_machine:machine <- [DW_OP_constu 48, DW_OP_minus, DW_OP_deref] [$rbp+0]
	movq	%r8, %rdi
	movsbl	-9(%rbp), %edx
	movq	%rsi, -56(%rbp)         # 8-byte Spill
	movl	%edx, %esi
	movq	%rcx, -64(%rbp)         # 8-byte Spill
	callq	symbol_state
	movq	-56(%rbp), %rdi         # 8-byte Reload
	.loc	1 125 21                # nfa.c:125:21
	leaq	-40(%rbp), %rcx
	movq	(%rcx), %r8
	movq	%r8, (%rsp)
	movq	8(%rcx), %r8
	movq	%r8, 8(%rsp)
	movq	16(%rcx), %rcx
	movq	%rcx, 16(%rsp)
	callq	setst
	xorl	%edx, %edx
                                        # kill: def $rdx killed $edx
	movq	-48(%rbp), %rcx         # 8-byte Reload
	.loc	1 125 19                # nfa.c:125:19
	movq	%rax, (%rcx)
	.loc	1 126 30 is_stmt 1      # nfa.c:126:30
	movq	(%rcx), %rax
	.loc	1 126 37 is_stmt 0      # nfa.c:126:37
	addq	$8, %rax
	.loc	1 126 5                 # nfa.c:126:5
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	point
	movq	-64(%rbp), %rax         # 8-byte Reload
	.loc	1 127 5 is_stmt 1       # nfa.c:127:5
	addq	$96, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp54:
.Lfunc_end23:
	.size	symbol_machine, .Lfunc_end23-symbol_machine
	.cfi_endproc
                                        # -- End function
	.globl	alt_machine             # -- Begin function alt_machine
	.p2align	4, 0x90
	.type	alt_machine,@function
alt_machine:                            # @alt_machine
.Lfunc_begin24:
	.loc	1 130 0                 # nfa.c:130:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$176, %rsp
	movq	%rdi, %rax
	leaq	40(%rbp), %rcx
	leaq	16(%rbp), %rdx
	movq	%rsi, -8(%rbp)
.Ltmp55:
	#DEBUG_VALUE: alt_machine:machine <- [$rdi+0]
	.loc	1 135 27 prologue_end   # nfa.c:135:27
	movq	-8(%rbp), %rsi
	.loc	1 135 54 is_stmt 0      # nfa.c:135:54
	movq	(%rdx), %r8
	.loc	1 135 67                # nfa.c:135:67
	movq	(%rcx), %r9
	.loc	1 135 36                # nfa.c:135:36
	leaq	-32(%rbp), %r10
	movq	%rdi, -88(%rbp)         # 8-byte Spill
.Ltmp56:
	#DEBUG_VALUE: alt_machine:machine <- [DW_OP_constu 88, DW_OP_minus, DW_OP_deref] [$rbp+0]
	movq	%r10, %rdi
	movq	%rsi, -96(%rbp)         # 8-byte Spill
	movq	%r8, %rsi
	movq	%rdx, -104(%rbp)        # 8-byte Spill
	movq	%r9, %rdx
	movq	%rax, -112(%rbp)        # 8-byte Spill
	movq	%rcx, -120(%rbp)        # 8-byte Spill
	callq	branch_state
	movq	-96(%rbp), %rdi         # 8-byte Reload
	.loc	1 135 21                # nfa.c:135:21
	leaq	-32(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	setst
	xorl	%r11d, %r11d
	movl	%r11d, %esi
	movq	-88(%rbp), %rcx         # 8-byte Reload
	.loc	1 135 19                # nfa.c:135:19
	movq	%rax, (%rcx)
	.loc	1 136 23 is_stmt 1      # nfa.c:136:23
	movq	-8(%rbp), %rdi
	.loc	1 136 32 is_stmt 0      # nfa.c:136:32
	leaq	-56(%rbp), %rax
	movq	%rdi, -128(%rbp)        # 8-byte Spill
	movq	%rax, %rdi
	callq	epsilon_state
	movq	-128(%rbp), %rdi        # 8-byte Reload
	.loc	1 136 17                # nfa.c:136:17
	leaq	-56(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	setst
	movq	-104(%rbp), %rcx        # 8-byte Reload
	.loc	1 136 5                 # nfa.c:136:5
	movq	(%rcx), %rdx
	movq	%rdx, (%rsp)
	movq	8(%rcx), %rdx
	movq	%rdx, 8(%rsp)
	movq	16(%rcx), %rdx
	movq	%rdx, 16(%rsp)
	movq	%rax, %rdi
	callq	patch
	xorl	%r11d, %r11d
	movl	%r11d, %esi
	.loc	1 137 24 is_stmt 1      # nfa.c:137:24
	movq	-8(%rbp), %rdi
	.loc	1 137 33 is_stmt 0      # nfa.c:137:33
	leaq	-80(%rbp), %rax
	movq	%rdi, -136(%rbp)        # 8-byte Spill
	movq	%rax, %rdi
	callq	epsilon_state
	movq	-136(%rbp), %rdi        # 8-byte Reload
	.loc	1 137 18                # nfa.c:137:18
	leaq	-80(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	setst
	movq	-120(%rbp), %rcx        # 8-byte Reload
	.loc	1 137 5                 # nfa.c:137:5
	movq	(%rcx), %rdx
	movq	%rdx, (%rsp)
	movq	8(%rcx), %rdx
	movq	%rdx, 8(%rsp)
	movq	16(%rcx), %rdx
	movq	%rdx, 16(%rsp)
	movq	%rax, %rdi
	callq	patch
	movq	-104(%rbp), %rax        # 8-byte Reload
	.loc	1 138 29 is_stmt 1      # nfa.c:138:29
	movq	8(%rax), %rcx
	.loc	1 138 23 is_stmt 0      # nfa.c:138:23
	movq	(%rcx), %rcx
	.loc	1 138 35                # nfa.c:138:35
	addq	$8, %rcx
	movq	-120(%rbp), %rdx        # 8-byte Reload
	.loc	1 138 50                # nfa.c:138:50
	movq	8(%rdx), %rsi
	.loc	1 138 43                # nfa.c:138:43
	movq	(%rsi), %rsi
	.loc	1 138 56                # nfa.c:138:56
	addq	$8, %rsi
	movq	-88(%rbp), %rdi         # 8-byte Reload
	movq	%rsi, -144(%rbp)        # 8-byte Spill
	.loc	1 138 5                 # nfa.c:138:5
	movq	%rcx, %rsi
	movq	-144(%rbp), %rdx        # 8-byte Reload
	callq	point
	movq	-112(%rbp), %rax        # 8-byte Reload
	.loc	1 139 5 is_stmt 1       # nfa.c:139:5
	addq	$176, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp57:
.Lfunc_end24:
	.size	alt_machine, .Lfunc_end24-alt_machine
	.cfi_endproc
                                        # -- End function
	.globl	cat_machine             # -- Begin function cat_machine
	.p2align	4, 0x90
	.type	cat_machine,@function
cat_machine:                            # @cat_machine
.Lfunc_begin25:
	.loc	1 142 0                 # nfa.c:142:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$64, %rsp
	movq	%rdi, %rax
	leaq	40(%rbp), %rcx
	leaq	16(%rbp), %rdx
.Ltmp58:
	#DEBUG_VALUE: cat_machine:machine <- [$rdi+0]
	.loc	1 147 25 prologue_end   # nfa.c:147:25
	movq	(%rcx), %rsi
	.loc	1 147 5 is_stmt 0       # nfa.c:147:5
	movq	(%rdx), %r8
	movq	%r8, (%rsp)
	movq	8(%rdx), %r8
	movq	%r8, 8(%rsp)
	movq	16(%rdx), %r8
	movq	%r8, 16(%rsp)
	movq	%rdi, -8(%rbp)          # 8-byte Spill
.Ltmp59:
	#DEBUG_VALUE: cat_machine:machine <- [DW_OP_constu 8, DW_OP_minus, DW_OP_deref] [$rbp+0]
	movq	%rsi, %rdi
	movq	%rax, -16(%rbp)         # 8-byte Spill
	movq	%rcx, -24(%rbp)         # 8-byte Spill
	movq	%rdx, -32(%rbp)         # 8-byte Spill
	callq	patch
	movq	-32(%rbp), %rax         # 8-byte Reload
	.loc	1 148 27 is_stmt 1      # nfa.c:148:27
	movq	(%rax), %rcx
	movq	-8(%rbp), %rdx          # 8-byte Reload
	.loc	1 148 19 is_stmt 0      # nfa.c:148:19
	movq	%rcx, (%rdx)
	movq	-24(%rbp), %rcx         # 8-byte Reload
	.loc	1 149 28 is_stmt 1      # nfa.c:149:28
	movq	8(%rcx), %rsi
	.loc	1 149 40 is_stmt 0      # nfa.c:149:40
	movq	16(%rcx), %rdx
	movq	-8(%rbp), %rdi          # 8-byte Reload
	.loc	1 149 5                 # nfa.c:149:5
	callq	point
	movq	-16(%rbp), %rax         # 8-byte Reload
	.loc	1 150 5 is_stmt 1       # nfa.c:150:5
	addq	$64, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp60:
.Lfunc_end25:
	.size	cat_machine, .Lfunc_end25-cat_machine
	.cfi_endproc
                                        # -- End function
	.globl	posclosure_machine      # -- Begin function posclosure_machine
	.p2align	4, 0x90
	.type	posclosure_machine,@function
posclosure_machine:                     # @posclosure_machine
.Lfunc_begin26:
	.loc	1 153 0                 # nfa.c:153:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	movq	%rdi, %rax
	leaq	16(%rbp), %rcx
	xorl	%edx, %edx
                                        # kill: def $rdx killed $edx
	movq	%rsi, -8(%rbp)
.Ltmp61:
	#DEBUG_VALUE: posclosure_machine:machine <- [$rdi+0]
	.loc	1 158 27 prologue_end   # nfa.c:158:27
	movq	(%rcx), %rsi
	.loc	1 158 19 is_stmt 0      # nfa.c:158:19
	movq	%rsi, (%rdi)
	.loc	1 159 24 is_stmt 1      # nfa.c:159:24
	movq	-8(%rbp), %rsi
	.loc	1 159 52 is_stmt 0      # nfa.c:159:52
	movq	(%rcx), %r8
	.loc	1 159 33                # nfa.c:159:33
	leaq	-32(%rbp), %r9
	movq	%rdi, -40(%rbp)         # 8-byte Spill
.Ltmp62:
	#DEBUG_VALUE: posclosure_machine:machine <- [DW_OP_constu 40, DW_OP_minus, DW_OP_deref] [$rbp+0]
	movq	%r9, %rdi
	movq	%rsi, -48(%rbp)         # 8-byte Spill
	movq	%r8, %rsi
	movq	%rax, -56(%rbp)         # 8-byte Spill
	movq	%rcx, -64(%rbp)         # 8-byte Spill
	callq	branch_state
	movq	-48(%rbp), %rdi         # 8-byte Reload
	.loc	1 159 18                # nfa.c:159:18
	leaq	-32(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	setst
	movq	-64(%rbp), %rcx         # 8-byte Reload
	.loc	1 159 5                 # nfa.c:159:5
	movq	(%rcx), %rdx
	movq	%rdx, (%rsp)
	movq	8(%rcx), %rdx
	movq	%rdx, 8(%rsp)
	movq	16(%rcx), %rdx
	movq	%rdx, 16(%rsp)
	movq	%rax, %rdi
	callq	patch
	xorl	%r10d, %r10d
	movl	%r10d, %edx
	movq	-64(%rbp), %rax         # 8-byte Reload
	.loc	1 160 30 is_stmt 1      # nfa.c:160:30
	movq	8(%rax), %rcx
	.loc	1 160 23 is_stmt 0      # nfa.c:160:23
	movq	(%rcx), %rcx
	.loc	1 160 36                # nfa.c:160:36
	addq	$8, %rcx
	addq	$8, %rcx
	movq	-40(%rbp), %rdi         # 8-byte Reload
	.loc	1 160 5                 # nfa.c:160:5
	movq	%rcx, %rsi
	callq	point
	movq	-56(%rbp), %rax         # 8-byte Reload
	.loc	1 161 5 is_stmt 1       # nfa.c:161:5
	addq	$96, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp63:
.Lfunc_end26:
	.size	posclosure_machine, .Lfunc_end26-posclosure_machine
	.cfi_endproc
                                        # -- End function
	.globl	optional_machine        # -- Begin function optional_machine
	.p2align	4, 0x90
	.type	optional_machine,@function
optional_machine:                       # @optional_machine
.Lfunc_begin27:
	.loc	1 164 0                 # nfa.c:164:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$128, %rsp
	movq	%rdi, %rax
	leaq	16(%rbp), %rcx
	xorl	%edx, %edx
	movl	%edx, %r8d
	movq	%rsi, -8(%rbp)
.Ltmp64:
	#DEBUG_VALUE: optional_machine:machine <- [$rdi+0]
	.loc	1 169 24 prologue_end   # nfa.c:169:24
	movq	-8(%rbp), %rsi
	.loc	1 169 33 is_stmt 0      # nfa.c:169:33
	leaq	-32(%rbp), %r9
	movq	%rdi, -64(%rbp)         # 8-byte Spill
.Ltmp65:
	#DEBUG_VALUE: optional_machine:machine <- [DW_OP_constu 64, DW_OP_minus, DW_OP_deref] [$rbp+0]
	movq	%r9, %rdi
	movq	%rsi, -72(%rbp)         # 8-byte Spill
	movq	%r8, %rsi
	movq	%rax, -80(%rbp)         # 8-byte Spill
	movq	%rcx, -88(%rbp)         # 8-byte Spill
	callq	epsilon_state
	movq	-72(%rbp), %rdi         # 8-byte Reload
	.loc	1 169 18                # nfa.c:169:18
	leaq	-32(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	setst
	movq	-88(%rbp), %rcx         # 8-byte Reload
	.loc	1 169 5                 # nfa.c:169:5
	movq	(%rcx), %rsi
	movq	%rsi, (%rsp)
	movq	8(%rcx), %rsi
	movq	%rsi, 8(%rsp)
	movq	16(%rcx), %rsi
	movq	%rsi, 16(%rsp)
	movq	%rax, %rdi
	callq	patch
	xorl	%edx, %edx
                                        # kill: def $rdx killed $edx
	.loc	1 170 27 is_stmt 1      # nfa.c:170:27
	movq	-8(%rbp), %rdi
	movq	-88(%rbp), %rax         # 8-byte Reload
	.loc	1 170 55 is_stmt 0      # nfa.c:170:55
	movq	(%rax), %rsi
	.loc	1 170 36                # nfa.c:170:36
	leaq	-56(%rbp), %rcx
	movq	%rdi, -96(%rbp)         # 8-byte Spill
	movq	%rcx, %rdi
	callq	branch_state
	movq	-96(%rbp), %rdi         # 8-byte Reload
	.loc	1 170 21                # nfa.c:170:21
	leaq	-56(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	setst
	movq	-64(%rbp), %rcx         # 8-byte Reload
	.loc	1 170 19                # nfa.c:170:19
	movq	%rax, (%rcx)
	movq	-88(%rbp), %rax         # 8-byte Reload
	.loc	1 171 30 is_stmt 1      # nfa.c:171:30
	movq	8(%rax), %rdx
	.loc	1 171 23 is_stmt 0      # nfa.c:171:23
	movq	(%rdx), %rdx
	.loc	1 171 36                # nfa.c:171:36
	addq	$8, %rdx
	.loc	1 171 51                # nfa.c:171:51
	movq	(%rcx), %rsi
	.loc	1 171 58                # nfa.c:171:58
	addq	$8, %rsi
	addq	$8, %rsi
	.loc	1 171 5                 # nfa.c:171:5
	movq	%rcx, %rdi
	movq	%rsi, -104(%rbp)        # 8-byte Spill
	movq	%rdx, %rsi
	movq	-104(%rbp), %rdx        # 8-byte Reload
	callq	point
	movq	-80(%rbp), %rax         # 8-byte Reload
	.loc	1 172 5 is_stmt 1       # nfa.c:172:5
	addq	$128, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp66:
.Lfunc_end27:
	.size	optional_machine, .Lfunc_end27-optional_machine
	.cfi_endproc
                                        # -- End function
	.globl	closure_machine         # -- Begin function closure_machine
	.p2align	4, 0x90
	.type	closure_machine,@function
closure_machine:                        # @closure_machine
.Lfunc_begin28:
	.loc	1 175 0                 # nfa.c:175:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$80, %rsp
	movq	%rdi, %rax
	leaq	16(%rbp), %rcx
	movq	%rsi, -8(%rbp)
.Ltmp67:
	.loc	1 179 29 prologue_end   # nfa.c:179:29
	movq	-8(%rbp), %rsi
	.loc	1 179 57 is_stmt 0      # nfa.c:179:57
	movq	-8(%rbp), %rdx
	.loc	1 179 38                # nfa.c:179:38
	leaq	-32(%rbp), %r8
	movq	%rdi, -40(%rbp)         # 8-byte Spill
	movq	%r8, %rdi
	movq	%rsi, -48(%rbp)         # 8-byte Spill
	movq	%rdx, %rsi
	movq	(%rcx), %rdx
	movq	%rdx, (%rsp)
	movq	8(%rcx), %rdx
	movq	%rdx, 8(%rsp)
	movq	16(%rcx), %rcx
	movq	%rcx, 16(%rsp)
	movq	%rax, -56(%rbp)         # 8-byte Spill
	callq	posclosure_machine
	movq	-40(%rbp), %rdi         # 8-byte Reload
	movq	-48(%rbp), %rsi         # 8-byte Reload
	.loc	1 179 12                # nfa.c:179:12
	leaq	-32(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	optional_machine
	movq	-56(%rbp), %rax         # 8-byte Reload
	.loc	1 179 5                 # nfa.c:179:5
	addq	$80, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp68:
.Lfunc_end28:
	.size	closure_machine, .Lfunc_end28-closure_machine
	.cfi_endproc
                                        # -- End function
	.globl	nfa_regex               # -- Begin function nfa_regex
	.p2align	4, 0x90
	.type	nfa_regex,@function
nfa_regex:                              # @nfa_regex
.Lfunc_begin29:
	.loc	1 182 0 is_stmt 1       # nfa.c:182:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$256, %rsp              # imm = 0x100
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
.Ltmp69:
	.loc	1 183 5 prologue_end    # nfa.c:183:5
	cmpq	$0, -16(%rbp)
.Ltmp70:
	.loc	1 183 5 is_stmt 0       # nfa.c:183:5
	je	.LBB29_2
# %bb.1:
	jmp	.LBB29_3
.LBB29_2:
.Ltmp71:
	.loc	1 183 5                 # nfa.c:183:5
	movabsq	$.L.str, %rdi
	movabsq	$.L.str.1, %rsi
	movl	$183, %edx
	movabsq	$.L__PRETTY_FUNCTION__.nfa_regex, %rcx
	callq	__assert_fail
.Ltmp72:
.LBB29_3:
	.loc	1 185 24 is_stmt 1      # nfa.c:185:24
	movq	-16(%rbp), %rdi
	.loc	1 185 10 is_stmt 0      # nfa.c:185:10
	callq	has_nfa_error
.Ltmp73:
	.loc	1 185 9                 # nfa.c:185:9
	testb	$1, %al
	jne	.LBB29_12
# %bb.4:
.Ltmp74:
	.loc	1 186 40 is_stmt 1      # nfa.c:186:40
	movq	-16(%rbp), %rsi
	.loc	1 186 31 is_stmt 0      # nfa.c:186:31
	leaq	-40(%rbp), %rdi
.Ltmp75:
	#DEBUG_VALUE: lmachine <- [$rdi+0]
	callq	gmachine
.Ltmp76:
	.loc	1 0 31                  # nfa.c:0:31
	movabsq	$nfa_to_rval, %rsi
	.loc	1 187 55 is_stmt 1      # nfa.c:187:55
	movq	-8(%rbp), %rdi
	.loc	1 187 62 is_stmt 0      # nfa.c:187:62
	movq	-16(%rbp), %rax
	.loc	1 187 41                # nfa.c:187:41
	leaq	-112(%rbp), %rcx
.Ltmp77:
	#DEBUG_VALUE: pcontext <- [$rcx+0]
	.loc	1 0 41                  # nfa.c:0:41
	movq	%rdi, -208(%rbp)        # 8-byte Spill
	.loc	1 187 41                # nfa.c:187:41
	movq	%rcx, %rdi
	movq	-208(%rbp), %rcx        # 8-byte Reload
.Ltmp78:
	.loc	1 0 41                  # nfa.c:0:41
	movq	%rsi, -216(%rbp)        # 8-byte Spill
	.loc	1 187 41                # nfa.c:187:41
	movq	%rcx, %rsi
	movq	%rax, %rdx
	movq	-216(%rbp), %rcx        # 8-byte Reload
	movabsq	$nfa_actions, %r8
	callq	parse_context
.Ltmp79:
	.loc	1 191 13 is_stmt 1      # nfa.c:191:13
	cmpq	$0, -32(%rbp)
.Ltmp80:
	.loc	1 191 13 is_stmt 0      # nfa.c:191:13
	je	.LBB29_6
# %bb.5:
.Ltmp81:
	.loc	1 192 13 is_stmt 1      # nfa.c:192:13
	movq	-16(%rbp), %rax
	.loc	1 192 30 is_stmt 0      # nfa.c:192:30
	movq	(%rax), %rcx
	addq	$-24, %rcx
	movq	%rcx, (%rax)
	.loc	1 193 13 is_stmt 1      # nfa.c:193:13
	movq	-16(%rbp), %rax
	.loc	1 193 31 is_stmt 0      # nfa.c:193:31
	movq	8(%rax), %rcx
	addq	$-1, %rcx
	movq	%rcx, 8(%rax)
	.loc	1 194 16 is_stmt 1      # nfa.c:194:16
	movl	uid, %edx
	addl	$-1, %edx
	movl	%edx, uid
.Ltmp82:
.LBB29_6:
	.loc	1 197 14                # nfa.c:197:14
	leaq	-112(%rbp), %rdi
	callq	parse_regex
.Ltmp83:
	.loc	1 197 13 is_stmt 0      # nfa.c:197:13
	testb	$1, %al
	jne	.LBB29_8
# %bb.7:
.Ltmp84:
	.loc	1 198 43 is_stmt 1      # nfa.c:198:43
	movb	-56(%rbp), %al
	.loc	1 198 13 is_stmt 0      # nfa.c:198:13
	movq	-16(%rbp), %rcx
	.loc	1 198 32                # nfa.c:198:32
	andb	$1, %al
	movb	%al, 40(%rcx)
	.loc	1 199 13 is_stmt 1      # nfa.c:199:13
	movq	-16(%rbp), %rcx
	.loc	1 199 60 is_stmt 0      # nfa.c:199:60
	movq	-52(%rbp), %rdx
	movq	%rdx, -128(%rbp)
	movl	-44(%rbp), %esi
	movl	%esi, -120(%rbp)
	.loc	1 199 30                # nfa.c:199:30
	movq	-128(%rbp), %rdx
	movq	%rdx, 44(%rcx)
	movl	-120(%rbp), %esi
	movl	%esi, 52(%rcx)
	.loc	1 200 9 is_stmt 1       # nfa.c:200:9
	jmp	.LBB29_11
.Ltmp85:
.LBB29_8:
	.loc	1 201 17                # nfa.c:201:17
	cmpq	$0, -32(%rbp)
.Ltmp86:
	.loc	1 201 17 is_stmt 0      # nfa.c:201:17
	je	.LBB29_10
# %bb.9:
.Ltmp87:
	.loc	1 201 42                # nfa.c:201:42
	movq	-16(%rbp), %rdi
	.loc	1 201 64                # nfa.c:201:64
	movq	-40(%rbp), %rax
	movq	%rax, -152(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -144(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -136(%rbp)
	.loc	1 201 31                # nfa.c:201:31
	leaq	-152(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	do_alt_nfa
.Ltmp88:
.LBB29_10:
	.loc	1 202 28 is_stmt 1      # nfa.c:202:28
	movq	-16(%rbp), %rsi
	.loc	1 202 19 is_stmt 0      # nfa.c:202:19
	leaq	-176(%rbp), %rdi
	callq	gmachine
	.loc	1 202 44                # nfa.c:202:44
	movq	-16(%rbp), %rdi
	.loc	1 202 53                # nfa.c:202:53
	leaq	-200(%rbp), %rsi
	movq	%rdi, -224(%rbp)        # 8-byte Spill
	movq	%rsi, %rdi
	callq	accepting_state
	movq	-224(%rbp), %rdi        # 8-byte Reload
	.loc	1 202 38                # nfa.c:202:38
	leaq	-200(%rbp), %rsi
	movq	(%rsi), %rax
	movq	%rax, (%rsp)
	movq	8(%rsi), %rax
	movq	%rax, 8(%rsp)
	movq	16(%rsi), %rax
	movq	%rax, 16(%rsp)
	callq	setst
	.loc	1 202 13                # nfa.c:202:13
	leaq	-176(%rbp), %rsi
	movq	(%rsi), %rdi
	movq	%rdi, (%rsp)
	movq	8(%rsi), %rdi
	movq	%rdi, 8(%rsp)
	movq	16(%rsi), %rsi
	movq	%rsi, 16(%rsp)
	movq	%rax, %rdi
	callq	patch
.Ltmp89:
.LBB29_11:
	.loc	1 204 5 is_stmt 1       # nfa.c:204:5
	jmp	.LBB29_12
.Ltmp90:
.LBB29_12:
	.loc	1 206 12                # nfa.c:206:12
	movq	-16(%rbp), %rax
	.loc	1 206 5 is_stmt 0       # nfa.c:206:5
	addq	$256, %rsp              # imm = 0x100
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp91:
.Lfunc_end29:
	.size	nfa_regex, .Lfunc_end29-nfa_regex
	.cfi_endproc
                                        # -- End function
	.globl	has_nfa_error           # -- Begin function has_nfa_error
	.p2align	4, 0x90
	.type	has_nfa_error,@function
has_nfa_error:                          # @has_nfa_error
.Lfunc_begin30:
	.loc	1 209 0 is_stmt 1       # nfa.c:209:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
.Ltmp92:
	.loc	1 210 12 prologue_end   # nfa.c:210:12
	movq	-8(%rbp), %rdi
	.loc	1 210 21 is_stmt 0      # nfa.c:210:21
	movb	40(%rdi), %al
	.loc	1 210 5                 # nfa.c:210:5
	andb	$1, %al
	movzbl	%al, %eax
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp93:
.Lfunc_end30:
	.size	has_nfa_error, .Lfunc_end30-has_nfa_error
	.cfi_endproc
                                        # -- End function
	.globl	nfa_error               # -- Begin function nfa_error
	.p2align	4, 0x90
	.type	nfa_error,@function
nfa_error:                              # @nfa_error
.Lfunc_begin31:
	.loc	1 213 0 is_stmt 1       # nfa.c:213:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, -24(%rbp)
.Ltmp94:
	.loc	1 214 12 prologue_end   # nfa.c:214:12
	movq	-24(%rbp), %rdi
	.loc	1 214 21 is_stmt 0      # nfa.c:214:21
	movl	52(%rdi), %eax
	movl	%eax, -8(%rbp)
	movq	44(%rdi), %rdi
	movq	%rdi, -16(%rbp)
	.loc	1 214 5                 # nfa.c:214:5
	movl	-8(%rbp), %eax
	movl	%eax, -32(%rbp)
	movq	-16(%rbp), %rdi
	movq	%rdi, -40(%rbp)
	movq	-40(%rbp), %rax
	movl	-32(%rbp), %edx
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp95:
.Lfunc_end31:
	.size	nfa_error, .Lfunc_end31-nfa_error
	.cfi_endproc
                                        # -- End function
	.globl	print_nfa_error         # -- Begin function print_nfa_error
	.p2align	4, 0x90
	.type	print_nfa_error,@function
print_nfa_error:                        # @print_nfa_error
.Lfunc_begin32:
	.loc	1 218 0 is_stmt 1       # nfa.c:218:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -32(%rbp)
	movl	%esi, -24(%rbp)
	movq	-32(%rbp), %rdi
	movq	%rdi, -16(%rbp)
	movl	-24(%rbp), %esi
	movl	%esi, -8(%rbp)
.Ltmp96:
	.loc	1 219 5 prologue_end    # nfa.c:219:5
	movq	-16(%rbp), %rdi
	movq	%rdi, -48(%rbp)
	movl	-8(%rbp), %esi
	movl	%esi, -40(%rbp)
	movq	-48(%rbp), %rdi
	movl	-40(%rbp), %esi
	callq	print_parse_error
	.loc	1 220 1                 # nfa.c:220:1
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp97:
.Lfunc_end32:
	.size	print_nfa_error, .Lfunc_end32-print_nfa_error
	.cfi_endproc
                                        # -- End function
	.globl	free_nfa_context        # -- Begin function free_nfa_context
	.p2align	4, 0x90
	.type	free_nfa_context,@function
free_nfa_context:                       # @free_nfa_context
.Lfunc_begin33:
	.loc	1 222 0                 # nfa.c:222:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
.Ltmp98:
	.loc	1 223 10 prologue_end   # nfa.c:223:10
	movq	-8(%rbp), %rdi
	.loc	1 223 19 is_stmt 0      # nfa.c:223:19
	movq	(%rdi), %rdi
	.loc	1 223 5                 # nfa.c:223:5
	callq	free
	.loc	1 224 10 is_stmt 1      # nfa.c:224:10
	movq	-8(%rbp), %rdi
	.loc	1 224 5 is_stmt 0       # nfa.c:224:5
	callq	free
	.loc	1 225 1 is_stmt 1       # nfa.c:225:1
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp99:
.Lfunc_end33:
	.size	free_nfa_context, .Lfunc_end33-free_nfa_context
	.cfi_endproc
                                        # -- End function
	.globl	eps_closure             # -- Begin function eps_closure
	.p2align	4, 0x90
	.type	eps_closure,@function
eps_closure:                            # @eps_closure
.Lfunc_begin34:
	.loc	1 227 0                 # nfa.c:227:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
.Ltmp100:
	.loc	1 228 9 prologue_end    # nfa.c:228:9
	movq	-24(%rbp), %rdx
	.loc	1 228 20 is_stmt 0      # nfa.c:228:20
	movq	-16(%rbp), %rsi
	.loc	1 228 9                 # nfa.c:228:9
	movslq	4(%rsi), %rsi
.Ltmp101:
	.loc	1 228 9                 # nfa.c:228:9
	testb	$1, (%rdx,%rsi)
	je	.LBB34_2
# %bb.1:
.Ltmp102:
	.loc	1 228 32                # nfa.c:228:32
	jmp	.LBB34_6
.Ltmp103:
.LBB34_2:
	.loc	1 230 10 is_stmt 1      # nfa.c:230:10
	movq	-8(%rbp), %rdi
	.loc	1 230 19 is_stmt 0      # nfa.c:230:19
	movq	-16(%rbp), %rsi
	.loc	1 230 5                 # nfa.c:230:5
	callq	push
	.loc	1 231 5 is_stmt 1       # nfa.c:231:5
	movq	-24(%rbp), %rsi
	.loc	1 231 16 is_stmt 0      # nfa.c:231:16
	movq	-16(%rbp), %rdi
	.loc	1 231 23                # nfa.c:231:23
	movslq	4(%rdi), %rdi
	.loc	1 231 27                # nfa.c:231:27
	movb	$1, (%rsi,%rdi)
	.loc	1 233 13 is_stmt 1      # nfa.c:233:13
	movq	-16(%rbp), %rsi
	.loc	1 233 20 is_stmt 0      # nfa.c:233:20
	movl	(%rsi), %ecx
	movl	%ecx, %esi
	movq	%rsi, %rdi
	subq	$4, %rdi
	movq	%rax, -32(%rbp)         # 8-byte Spill
	movq	%rsi, -40(%rbp)         # 8-byte Spill
	movq	%rdi, -48(%rbp)         # 8-byte Spill
	.loc	1 233 5                 # nfa.c:233:5
	ja	.LBB34_6
# %bb.7:
	.loc	1 0 5                   # nfa.c:0:5
	movq	-40(%rbp), %rax         # 8-byte Reload
	movq	.LJTI34_0(,%rax,8), %rcx
	jmpq	*%rcx
.LBB34_3:
.Ltmp104:
	.loc	1 235 25 is_stmt 1      # nfa.c:235:25
	movq	-8(%rbp), %rdi
	.loc	1 235 34 is_stmt 0      # nfa.c:235:34
	movq	-16(%rbp), %rax
	.loc	1 235 41                # nfa.c:235:41
	movq	8(%rax), %rsi
	.loc	1 235 47                # nfa.c:235:47
	movq	-24(%rbp), %rdx
	.loc	1 235 13                # nfa.c:235:13
	callq	eps_closure
	.loc	1 236 13 is_stmt 1      # nfa.c:236:13
	jmp	.LBB34_6
.LBB34_4:
	.loc	1 238 25                # nfa.c:238:25
	movq	-8(%rbp), %rdi
	.loc	1 238 34 is_stmt 0      # nfa.c:238:34
	movq	-16(%rbp), %rax
	.loc	1 238 41                # nfa.c:238:41
	movq	8(%rax), %rsi
	.loc	1 238 47                # nfa.c:238:47
	movq	-24(%rbp), %rdx
	.loc	1 238 13                # nfa.c:238:13
	callq	eps_closure
	.loc	1 239 25 is_stmt 1      # nfa.c:239:25
	movq	-8(%rbp), %rdi
	.loc	1 239 34 is_stmt 0      # nfa.c:239:34
	movq	-16(%rbp), %rax
	.loc	1 239 41                # nfa.c:239:41
	movq	16(%rax), %rsi
	.loc	1 239 48                # nfa.c:239:48
	movq	-24(%rbp), %rdx
	.loc	1 239 13                # nfa.c:239:13
	callq	eps_closure
	.loc	1 240 13 is_stmt 1      # nfa.c:240:13
	jmp	.LBB34_6
.LBB34_5:
	.loc	1 244 13                # nfa.c:244:13
	jmp	.LBB34_6
.Ltmp105:
.LBB34_6:
	.loc	1 246 1                 # nfa.c:246:1
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp106:
.Lfunc_end34:
	.size	eps_closure, .Lfunc_end34-eps_closure
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.p2align	3
.LJTI34_0:
	.quad	.LBB34_5
	.quad	.LBB34_3
	.quad	.LBB34_4
	.quad	.LBB34_5
	.quad	.LBB34_5
                                        # -- End function
	.text
	.globl	move                    # -- Begin function move
	.p2align	4, 0x90
	.type	move,@function
move:                                   # @move
.Lfunc_begin35:
	.loc	1 248 0                 # nfa.c:248:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$64, %rsp
	movb	%dl, %al
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movb	%al, -17(%rbp)
	movq	%rcx, -32(%rbp)
.LBB35_1:                               # =>This Inner Loop Header: Depth=1
.Ltmp107:
	.loc	1 251 24 prologue_end   # nfa.c:251:24
	movq	-16(%rbp), %rdi
	.loc	1 251 20 is_stmt 0      # nfa.c:251:20
	callq	pop
	xorl	%ecx, %ecx
	movb	%cl, %dl
	.loc	1 251 18                # nfa.c:251:18
	movq	%rax, -40(%rbp)
	cmpq	$0, %rax
	movb	%dl, -49(%rbp)          # 1-byte Spill
	.loc	1 251 34                # nfa.c:251:34
	je	.LBB35_3
# %bb.2:                                #   in Loop: Header=BB35_1 Depth=1
	.loc	1 251 52                # nfa.c:251:52
	movq	-40(%rbp), %rdi
	.loc	1 251 46                # nfa.c:251:46
	callq	value
	.loc	1 251 44                # nfa.c:251:44
	movq	%rax, -48(%rbp)
	.loc	1 251 34                # nfa.c:251:34
	cmpq	$0, %rax
	setne	%cl
	movb	%cl, -49(%rbp)          # 1-byte Spill
.LBB35_3:                               #   in Loop: Header=BB35_1 Depth=1
	.loc	1 0 34                  # nfa.c:0:34
	movb	-49(%rbp), %al          # 1-byte Reload
	.loc	1 251 5                 # nfa.c:251:5
	testb	$1, %al
	jne	.LBB35_4
	jmp	.LBB35_9
.LBB35_4:                               #   in Loop: Header=BB35_1 Depth=1
.Ltmp108:
	.loc	1 252 14 is_stmt 1      # nfa.c:252:14
	movq	-48(%rbp), %rax
	.loc	1 252 26 is_stmt 0      # nfa.c:252:26
	cmpl	$3, (%rax)
	.loc	1 252 42                # nfa.c:252:42
	jne	.LBB35_6
# %bb.5:                                #   in Loop: Header=BB35_1 Depth=1
	.loc	1 252 45                # nfa.c:252:45
	movq	-48(%rbp), %rax
	movsbl	16(%rax), %ecx
	.loc	1 252 62                # nfa.c:252:62
	movsbl	-17(%rbp), %edx
	.loc	1 252 59                # nfa.c:252:59
	cmpl	%edx, %ecx
	.loc	1 252 65                # nfa.c:252:65
	je	.LBB35_7
.LBB35_6:                               #   in Loop: Header=BB35_1 Depth=1
	.loc	1 252 68                # nfa.c:252:68
	movq	-48(%rbp), %rax
	.loc	1 252 80                # nfa.c:252:80
	cmpl	$4, (%rax)
.Ltmp109:
	.loc	1 252 13                # nfa.c:252:13
	jne	.LBB35_8
.LBB35_7:                               #   in Loop: Header=BB35_1 Depth=1
.Ltmp110:
	.loc	1 253 25 is_stmt 1      # nfa.c:253:25
	movq	-8(%rbp), %rdi
	.loc	1 253 34 is_stmt 0      # nfa.c:253:34
	movq	-48(%rbp), %rax
	.loc	1 253 41                # nfa.c:253:41
	movq	8(%rax), %rsi
	.loc	1 253 47                # nfa.c:253:47
	movq	-32(%rbp), %rdx
	.loc	1 253 13                # nfa.c:253:13
	callq	eps_closure
.Ltmp111:
.LBB35_8:                               #   in Loop: Header=BB35_1 Depth=1
	.loc	1 0 13                  # nfa.c:0:13
	xorl	%eax, %eax
	movl	%eax, %esi
	.loc	1 256 19 is_stmt 1      # nfa.c:256:19
	movq	-40(%rbp), %rdi
	.loc	1 256 9 is_stmt 0       # nfa.c:256:9
	callq	free_node
.Ltmp112:
	.loc	1 251 5 is_stmt 1       # nfa.c:251:5
	jmp	.LBB35_1
.LBB35_9:
	.loc	1 258 1                 # nfa.c:258:1
	addq	$64, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp113:
.Lfunc_end35:
	.size	move, .Lfunc_end35-move
	.cfi_endproc
                                        # -- End function
	.globl	accepts                 # -- Begin function accepts
	.p2align	4, 0x90
	.type	accepts,@function
accepts:                                # @accepts
.Lfunc_begin36:
	.loc	1 260 0                 # nfa.c:260:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -16(%rbp)
	movq	%rsi, -24(%rbp)
.Ltmp114:
	.loc	1 264 22 prologue_end   # nfa.c:264:22
	movq	-16(%rbp), %rdi
	.loc	1 264 17 is_stmt 0      # nfa.c:264:17
	callq	head
	.loc	1 264 15                # nfa.c:264:15
	movq	%rax, -32(%rbp)
.LBB36_1:                               # =>This Inner Loop Header: Depth=1
	.loc	1 264 5                 # nfa.c:264:5
	cmpq	$0, -32(%rbp)
	je	.LBB36_6
# %bb.2:                                #   in Loop: Header=BB36_1 Depth=1
.Ltmp115:
	.loc	1 265 23 is_stmt 1      # nfa.c:265:23
	movq	-32(%rbp), %rdi
	.loc	1 265 17 is_stmt 0      # nfa.c:265:17
	callq	value
	.loc	1 265 15                # nfa.c:265:15
	movq	%rax, -48(%rbp)
.Ltmp116:
	.loc	1 267 13 is_stmt 1      # nfa.c:267:13
	movq	-48(%rbp), %rax
	.loc	1 267 19 is_stmt 0      # nfa.c:267:19
	cmpq	-24(%rbp), %rax
.Ltmp117:
	.loc	1 267 13                # nfa.c:267:13
	jne	.LBB36_4
# %bb.3:
.Ltmp118:
	.loc	1 268 13 is_stmt 1      # nfa.c:268:13
	movb	$1, -1(%rbp)
	jmp	.LBB36_7
.Ltmp119:
.LBB36_4:                               #   in Loop: Header=BB36_1 Depth=1
	.loc	1 271 16                # nfa.c:271:16
	movq	-32(%rbp), %rax
	.loc	1 271 22 is_stmt 0      # nfa.c:271:22
	movq	(%rax), %rax
	.loc	1 271 14                # nfa.c:271:14
	movq	%rax, -40(%rbp)
.Ltmp120:
# %bb.5:                                #   in Loop: Header=BB36_1 Depth=1
	.loc	1 264 45 is_stmt 1      # nfa.c:264:45
	movq	-40(%rbp), %rax
	.loc	1 264 43 is_stmt 0      # nfa.c:264:43
	movq	%rax, -32(%rbp)
	.loc	1 264 5                 # nfa.c:264:5
	jmp	.LBB36_1
.Ltmp121:
.LBB36_6:
	.loc	1 274 5 is_stmt 1       # nfa.c:274:5
	movb	$0, -1(%rbp)
.LBB36_7:
	.loc	1 275 1                 # nfa.c:275:1
	movb	-1(%rbp), %al
	andb	$1, %al
	movzbl	%al, %eax
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp122:
.Lfunc_end36:
	.size	accepts, .Lfunc_end36-accepts
	.cfi_endproc
                                        # -- End function
	.globl	nfa_match               # -- Begin function nfa_match
	.p2align	4, 0x90
	.type	nfa_match,@function
nfa_match:                              # @nfa_match
.Lfunc_begin37:
	.loc	1 277 0                 # nfa.c:277:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
.Ltmp123:
	.loc	1 278 22 prologue_end   # nfa.c:278:22
	movq	-16(%rbp), %rsi
	.loc	1 278 31 is_stmt 0      # nfa.c:278:31
	movq	16(%rsi), %rdi
	movq	%rdi, -40(%rbp)
	movq	24(%rsi), %rdi
	movq	%rdi, -32(%rbp)
	movq	32(%rsi), %rsi
	movq	%rsi, -24(%rbp)
	.loc	1 279 24 is_stmt 1      # nfa.c:279:24
	movq	-16(%rbp), %rsi
	.loc	1 279 33 is_stmt 0      # nfa.c:279:33
	movq	8(%rsi), %rsi
	.loc	1 279 12                # nfa.c:279:12
	movq	%rsi, -48(%rbp)
	.loc	1 280 31 is_stmt 1      # nfa.c:280:31
	movq	-48(%rbp), %rdi
	.loc	1 280 24 is_stmt 0      # nfa.c:280:24
	movl	$1, %esi
	callq	calloc
	.loc	1 280 11                # nfa.c:280:11
	movq	%rax, -56(%rbp)
	.loc	1 281 28 is_stmt 1      # nfa.c:281:28
	movb	$0, %al
	callq	init_list
	.loc	1 281 18 is_stmt 0      # nfa.c:281:18
	movq	%rax, -64(%rbp)
	.loc	1 282 28 is_stmt 1      # nfa.c:282:28
	movb	$0, %al
	callq	init_list
	.loc	1 282 18 is_stmt 0      # nfa.c:282:18
	movq	%rax, -72(%rbp)
	.loc	1 284 17 is_stmt 1      # nfa.c:284:17
	movq	-64(%rbp), %rdi
	.loc	1 284 30 is_stmt 0      # nfa.c:284:30
	movq	-40(%rbp), %rsi
	.loc	1 284 37                # nfa.c:284:37
	movq	-56(%rbp), %rdx
	.loc	1 284 5                 # nfa.c:284:5
	callq	eps_closure
.LBB37_1:                               # =>This Inner Loop Header: Depth=1
	.loc	1 292 21 is_stmt 1      # nfa.c:292:21
	movq	-8(%rbp), %rax
	movq	%rax, %rcx
	addq	$1, %rcx
	movq	%rcx, -8(%rbp)
	.loc	1 292 17 is_stmt 0      # nfa.c:292:17
	movb	(%rax), %dl
	.loc	1 292 15                # nfa.c:292:15
	movb	%dl, -73(%rbp)
	.loc	1 292 12                # nfa.c:292:12
	movsbl	%dl, %esi
	.loc	1 292 25                # nfa.c:292:25
	cmpl	$0, %esi
	.loc	1 292 5                 # nfa.c:292:5
	je	.LBB37_3
# %bb.2:                                #   in Loop: Header=BB37_1 Depth=1
	.loc	1 0 5                   # nfa.c:0:5
	xorl	%esi, %esi
.Ltmp124:
	.loc	1 293 16 is_stmt 1      # nfa.c:293:16
	movq	-56(%rbp), %rdi
	.loc	1 293 35 is_stmt 0      # nfa.c:293:35
	movq	-48(%rbp), %rdx
	.loc	1 293 9                 # nfa.c:293:9
	callq	memset
	.loc	1 294 14 is_stmt 1      # nfa.c:294:14
	movq	-72(%rbp), %rdi
	.loc	1 294 23 is_stmt 0      # nfa.c:294:23
	movq	-64(%rbp), %rsi
	.loc	1 294 32                # nfa.c:294:32
	movb	-73(%rbp), %al
	.loc	1 294 35                # nfa.c:294:35
	movq	-56(%rbp), %rcx
	.loc	1 294 9                 # nfa.c:294:9
	movsbl	%al, %edx
	callq	move
	.loc	1 295 13 is_stmt 1      # nfa.c:295:13
	movq	-64(%rbp), %rcx
	.loc	1 295 11 is_stmt 0      # nfa.c:295:11
	movq	%rcx, -88(%rbp)
	.loc	1 296 19 is_stmt 1      # nfa.c:296:19
	movq	-72(%rbp), %rcx
	.loc	1 296 17 is_stmt 0      # nfa.c:296:17
	movq	%rcx, -64(%rbp)
	.loc	1 297 19 is_stmt 1      # nfa.c:297:19
	movq	-88(%rbp), %rcx
	.loc	1 297 17 is_stmt 0      # nfa.c:297:17
	movq	%rcx, -72(%rbp)
.Ltmp125:
	.loc	1 292 5 is_stmt 1       # nfa.c:292:5
	jmp	.LBB37_1
.LBB37_3:
	.loc	1 303 27                # nfa.c:303:27
	movq	-64(%rbp), %rdi
	.loc	1 303 41 is_stmt 0      # nfa.c:303:41
	movq	-32(%rbp), %rax
	.loc	1 303 36                # nfa.c:303:36
	movq	(%rax), %rsi
	.loc	1 303 19                # nfa.c:303:19
	callq	accepts
	.loc	1 303 10                # nfa.c:303:10
	andb	$1, %al
	movb	%al, -89(%rbp)
	.loc	1 305 10 is_stmt 1      # nfa.c:305:10
	movq	-56(%rbp), %rdi
	.loc	1 305 5 is_stmt 0       # nfa.c:305:5
	callq	free
	xorl	%ecx, %ecx
	movl	%ecx, %esi
	.loc	1 306 15 is_stmt 1      # nfa.c:306:15
	movq	-72(%rbp), %rdi
	.loc	1 306 5 is_stmt 0       # nfa.c:306:5
	callq	free_list
	xorl	%ecx, %ecx
	movl	%ecx, %esi
	.loc	1 307 15 is_stmt 1      # nfa.c:307:15
	movq	-64(%rbp), %rdi
	.loc	1 307 5 is_stmt 0       # nfa.c:307:5
	callq	free_list
	.loc	1 309 12 is_stmt 1      # nfa.c:309:12
	movb	-89(%rbp), %al
	.loc	1 309 5 is_stmt 0       # nfa.c:309:5
	andb	$1, %al
	movzbl	%al, %eax
	addq	$96, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp126:
.Lfunc_end37:
	.size	nfa_match, .Lfunc_end37-nfa_match
	.cfi_endproc
                                        # -- End function
	.globl	print_nfa_states        # -- Begin function print_nfa_states
	.p2align	4, 0x90
	.type	print_nfa_states,@function
print_nfa_states:                       # @print_nfa_states
.Lfunc_begin38:
	.loc	1 346 0 is_stmt 1       # nfa.c:346:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
.Ltmp127:
	.loc	1 349 16 prologue_end   # nfa.c:349:16
	movq	-8(%rbp), %rdi
	.loc	1 349 10 is_stmt 0      # nfa.c:349:10
	callq	empty
.Ltmp128:
	.loc	1 349 9                 # nfa.c:349:9
	testb	$1, %al
	jne	.LBB38_6
# %bb.1:
.Ltmp129:
	.loc	1 350 9 is_stmt 1       # nfa.c:350:9
	movabsq	$.L.str.2, %rdi
	movb	$0, %al
	callq	printf
	.loc	1 351 21                # nfa.c:351:21
	movq	-8(%rbp), %rdi
	movl	%eax, -20(%rbp)         # 4-byte Spill
	.loc	1 351 16 is_stmt 0      # nfa.c:351:16
	callq	head
	.loc	1 351 14                # nfa.c:351:14
	movq	%rax, -16(%rbp)
	.loc	1 352 27 is_stmt 1      # nfa.c:352:27
	movq	-16(%rbp), %rdi
	.loc	1 352 21 is_stmt 0      # nfa.c:352:21
	callq	value
	.loc	1 352 9                 # nfa.c:352:9
	movq	%rax, %rdi
	callq	print_state
.Ltmp130:
	.loc	1 353 21 is_stmt 1      # nfa.c:353:21
	movq	-16(%rbp), %rax
	.loc	1 353 27 is_stmt 0      # nfa.c:353:27
	movq	(%rax), %rax
	.loc	1 353 19                # nfa.c:353:19
	movq	%rax, -16(%rbp)
.LBB38_2:                               # =>This Inner Loop Header: Depth=1
	.loc	1 353 9                 # nfa.c:353:9
	cmpq	$0, -16(%rbp)
	je	.LBB38_5
# %bb.3:                                #   in Loop: Header=BB38_2 Depth=1
.Ltmp131:
	.loc	1 354 13 is_stmt 1      # nfa.c:354:13
	movabsq	$.L.str.3, %rdi
	movb	$0, %al
	callq	printf
	.loc	1 355 31                # nfa.c:355:31
	movq	-16(%rbp), %rdi
	movl	%eax, -24(%rbp)         # 4-byte Spill
	.loc	1 355 25 is_stmt 0      # nfa.c:355:25
	callq	value
	.loc	1 355 13                # nfa.c:355:13
	movq	%rax, %rdi
	callq	print_state
.Ltmp132:
# %bb.4:                                #   in Loop: Header=BB38_2 Depth=1
	.loc	1 353 46 is_stmt 1      # nfa.c:353:46
	movq	-16(%rbp), %rax
	.loc	1 353 52 is_stmt 0      # nfa.c:353:52
	movq	(%rax), %rax
	.loc	1 353 44                # nfa.c:353:44
	movq	%rax, -16(%rbp)
	.loc	1 353 9                 # nfa.c:353:9
	jmp	.LBB38_2
.Ltmp133:
.LBB38_5:
	.loc	1 357 9 is_stmt 1       # nfa.c:357:9
	movabsq	$.L.str.4, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -28(%rbp)         # 4-byte Spill
	.loc	1 358 5                 # nfa.c:358:5
	jmp	.LBB38_7
.Ltmp134:
.LBB38_6:
	.loc	1 359 9                 # nfa.c:359:9
	movabsq	$.L.str.5, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -32(%rbp)         # 4-byte Spill
.Ltmp135:
.LBB38_7:
	.loc	1 362 5                 # nfa.c:362:5
	movabsq	$.L.str.6, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -36(%rbp)         # 4-byte Spill
	.loc	1 363 1                 # nfa.c:363:1
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp136:
.Lfunc_end38:
	.size	print_nfa_states, .Lfunc_end38-print_nfa_states
	.cfi_endproc
                                        # -- End function
	.globl	print_state             # -- Begin function print_state
	.p2align	4, 0x90
	.type	print_state,@function
print_state:                            # @print_state
.Lfunc_begin39:
	.loc	1 365 0                 # nfa.c:365:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$64, %rsp
	movq	%rdi, -8(%rbp)
.Ltmp137:
	.loc	1 366 21 prologue_end   # nfa.c:366:21
	movq	-8(%rbp), %rdi
	.loc	1 366 28 is_stmt 0      # nfa.c:366:28
	movl	4(%rdi), %esi
	.loc	1 366 5                 # nfa.c:366:5
	movl	$.L.str.7, %edi
	xorl	%eax, %eax
	movb	%al, %cl
	movb	%cl, %al
	callq	printf
	.loc	1 367 13 is_stmt 1      # nfa.c:367:13
	movq	-8(%rbp), %rdi
	.loc	1 367 20 is_stmt 0      # nfa.c:367:20
	movl	(%rdi), %esi
	movl	%esi, %edi
	movq	%rdi, %rdx
	subq	$4, %rdx
	movl	%eax, -12(%rbp)         # 4-byte Spill
	movq	%rdi, -24(%rbp)         # 8-byte Spill
	movq	%rdx, -32(%rbp)         # 8-byte Spill
	.loc	1 367 5                 # nfa.c:367:5
	ja	.LBB39_6
# %bb.7:
	.loc	1 0 5                   # nfa.c:0:5
	movq	-24(%rbp), %rax         # 8-byte Reload
	movq	.LJTI39_0(,%rax,8), %rcx
	jmpq	*%rcx
.LBB39_1:
.Ltmp138:
	.loc	1 369 13 is_stmt 1      # nfa.c:369:13
	movabsq	$.L.str.8, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -36(%rbp)         # 4-byte Spill
	.loc	1 370 13                # nfa.c:370:13
	jmp	.LBB39_6
.LBB39_2:
	.loc	1 372 31                # nfa.c:372:31
	movq	-8(%rbp), %rax
	.loc	1 372 38 is_stmt 0      # nfa.c:372:38
	movq	8(%rax), %rsi
	.loc	1 372 13                # nfa.c:372:13
	movabsq	$.L.str.9, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -40(%rbp)         # 4-byte Spill
	.loc	1 373 13 is_stmt 1      # nfa.c:373:13
	jmp	.LBB39_6
.LBB39_3:
	.loc	1 375 34                # nfa.c:375:34
	movq	-8(%rbp), %rax
	.loc	1 375 41 is_stmt 0      # nfa.c:375:41
	movq	8(%rax), %rsi
	.loc	1 375 13                # nfa.c:375:13
	movabsq	$.L.str.10, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -44(%rbp)         # 4-byte Spill
	.loc	1 376 13 is_stmt 1      # nfa.c:376:13
	jmp	.LBB39_6
.LBB39_4:
	.loc	1 378 38                # nfa.c:378:38
	movq	-8(%rbp), %rax
	.loc	1 378 45 is_stmt 0      # nfa.c:378:45
	movq	8(%rax), %rsi
	.loc	1 378 51                # nfa.c:378:51
	movq	-8(%rbp), %rax
	.loc	1 378 58                # nfa.c:378:58
	movq	16(%rax), %rdx
	.loc	1 378 13                # nfa.c:378:13
	movabsq	$.L.str.11, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -48(%rbp)         # 4-byte Spill
	.loc	1 379 13 is_stmt 1      # nfa.c:379:13
	jmp	.LBB39_6
.LBB39_5:
	.loc	1 381 38                # nfa.c:381:38
	movq	-8(%rbp), %rax
	.loc	1 381 45 is_stmt 0      # nfa.c:381:45
	movq	8(%rax), %rsi
	.loc	1 381 51                # nfa.c:381:51
	movq	-8(%rbp), %rax
	movsbl	16(%rax), %edx
	.loc	1 381 13                # nfa.c:381:13
	movabsq	$.L.str.12, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -52(%rbp)         # 4-byte Spill
.Ltmp139:
.LBB39_6:
	.loc	1 384 5 is_stmt 1       # nfa.c:384:5
	movabsq	$.L.str.13, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -56(%rbp)         # 4-byte Spill
	.loc	1 385 1                 # nfa.c:385:1
	addq	$64, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp140:
.Lfunc_end39:
	.size	print_state, .Lfunc_end39-print_state
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.p2align	3
.LJTI39_0:
	.quad	.LBB39_1
	.quad	.LBB39_2
	.quad	.LBB39_4
	.quad	.LBB39_5
	.quad	.LBB39_3
                                        # -- End function
	.text
	.globl	print_state_table       # -- Begin function print_state_table
	.p2align	4, 0x90
	.type	print_state_table,@function
print_state_table:                      # @print_state_table
.Lfunc_begin40:
	.loc	1 387 0                 # nfa.c:387:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
.LBB40_1:                               # =>This Inner Loop Header: Depth=1
.Ltmp141:
	.loc	1 388 12 prologue_end   # nfa.c:388:12
	movq	-8(%rbp), %rax
	.loc	1 388 18 is_stmt 0      # nfa.c:388:18
	cmpq	-16(%rbp), %rax
	.loc	1 388 5                 # nfa.c:388:5
	je	.LBB40_3
# %bb.2:                                #   in Loop: Header=BB40_1 Depth=1
.Ltmp142:
	.loc	1 389 24 is_stmt 1      # nfa.c:389:24
	movq	-8(%rbp), %rsi
	.loc	1 389 9 is_stmt 0       # nfa.c:389:9
	movabsq	$.L.str.14, %rdi
	movb	$0, %al
	callq	printf
	.loc	1 390 21 is_stmt 1      # nfa.c:390:21
	movq	-8(%rbp), %rdi
	movl	%eax, -20(%rbp)         # 4-byte Spill
	.loc	1 390 9 is_stmt 0       # nfa.c:390:9
	callq	print_state
	.loc	1 391 9 is_stmt 1       # nfa.c:391:9
	movabsq	$.L.str.6, %rdi
	movb	$0, %al
	callq	printf
	.loc	1 392 14                # nfa.c:392:14
	movq	-8(%rbp), %rsi
	addq	$24, %rsi
	movq	%rsi, -8(%rbp)
	movl	%eax, -24(%rbp)         # 4-byte Spill
.Ltmp143:
	.loc	1 388 5                 # nfa.c:388:5
	jmp	.LBB40_1
.LBB40_3:
	.loc	1 394 1                 # nfa.c:394:1
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp144:
.Lfunc_end40:
	.size	print_state_table, .Lfunc_end40-print_state_table
	.cfi_endproc
                                        # -- End function
	.type	uid,@object             # @uid
	.bss
	.globl	uid
	.p2align	2
uid:
	.long	0                       # 0x0
	.size	uid, 4

	.type	nfa_actions,@object     # @nfa_actions
	.data
	.globl	nfa_actions
	.p2align	4
nfa_actions:
	.quad	noop_nfa
	.quad	do_empty_nfa
	.quad	do_alt_nfa
	.quad	do_cat_nfa
	.quad	noop_nfa
	.quad	do_dotall_nfa
	.quad	do_symbol_nfa
	.quad	do_star_nfa
	.quad	do_plus_nfa
	.quad	do_optional_nfa
	.size	nfa_actions, 80

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"context != NULL"
	.size	.L.str, 16

	.type	.L.str.1,@object        # @.str.1
.L.str.1:
	.asciz	"nfa.c"
	.size	.L.str.1, 6

	.type	.L__PRETTY_FUNCTION__.nfa_regex,@object # @__PRETTY_FUNCTION__.nfa_regex
.L__PRETTY_FUNCTION__.nfa_regex:
	.asciz	"struct nfa_context *nfa_regex(char *, struct nfa_context *)"
	.size	.L__PRETTY_FUNCTION__.nfa_regex, 60

	.type	.L.str.2,@object        # @.str.2
.L.str.2:
	.asciz	"{"
	.size	.L.str.2, 2

	.type	.L.str.3,@object        # @.str.3
.L.str.3:
	.asciz	", "
	.size	.L.str.3, 3

	.type	.L.str.4,@object        # @.str.4
.L.str.4:
	.asciz	"}"
	.size	.L.str.4, 2

	.type	.L.str.5,@object        # @.str.5
.L.str.5:
	.asciz	"empty"
	.size	.L.str.5, 6

	.type	.L.str.6,@object        # @.str.6
.L.str.6:
	.asciz	"\n"
	.size	.L.str.6, 2

	.type	.L.str.7,@object        # @.str.7
.L.str.7:
	.asciz	"(%d, "
	.size	.L.str.7, 6

	.type	.L.str.8,@object        # @.str.8
.L.str.8:
	.asciz	"accept"
	.size	.L.str.8, 7

	.type	.L.str.9,@object        # @.str.9
.L.str.9:
	.asciz	"eps, %p"
	.size	.L.str.9, 8

	.type	.L.str.10,@object       # @.str.10
.L.str.10:
	.asciz	"dotall, %p"
	.size	.L.str.10, 11

	.type	.L.str.11,@object       # @.str.11
.L.str.11:
	.asciz	"branch, %p, %p"
	.size	.L.str.11, 15

	.type	.L.str.12,@object       # @.str.12
.L.str.12:
	.asciz	"symbol, %p, %c"
	.size	.L.str.12, 15

	.type	.L.str.13,@object       # @.str.13
.L.str.13:
	.asciz	")"
	.size	.L.str.13, 2

	.type	.L.str.14,@object       # @.str.14
.L.str.14:
	.asciz	"%p: "
	.size	.L.str.14, 5

	.file	3 "/home/wroathe/compilers/src/auto" "./nfa.h"
	.file	4 "/usr/lib/llvm-8/lib/clang/8.0.0/include" "stddef.h"
	.file	5 "/home/wroathe/compilers/src/auto" "./parser.h"
	.file	6 "/home/wroathe/compilers/src/auto" "../../base/base/list.h"
	.section	.debug_str,"MS",@progbits,1
.Linfo_string0:
	.asciz	"clang version 8.0.0-3~ubuntu18.04.1 (tags/RELEASE_800/final)" # string offset=0
.Linfo_string1:
	.asciz	"nfa.c"                 # string offset=61
.Linfo_string2:
	.asciz	"/home/wroathe/compilers/src/auto" # string offset=67
.Linfo_string3:
	.asciz	"uid"                   # string offset=100
.Linfo_string4:
	.asciz	"int"                   # string offset=104
.Linfo_string5:
	.asciz	"nfa_actions"           # string offset=108
.Linfo_string6:
	.asciz	"_"                     # string offset=120
.Linfo_string7:
	.asciz	"expr"                  # string offset=122
.Linfo_string8:
	.asciz	"type"                  # string offset=127
.Linfo_string9:
	.asciz	"unsigned int"          # string offset=132
.Linfo_string10:
	.asciz	"NULL_EXPR"             # string offset=145
.Linfo_string11:
	.asciz	"EMPTY_EXPR"            # string offset=155
.Linfo_string12:
	.asciz	"DOTALL_EXPR"           # string offset=166
.Linfo_string13:
	.asciz	"ALT_EXPR"              # string offset=178
.Linfo_string14:
	.asciz	"CAT_EXPR"              # string offset=187
.Linfo_string15:
	.asciz	"STAR_EXPR"             # string offset=196
.Linfo_string16:
	.asciz	"PLUS_EXPR"             # string offset=206
.Linfo_string17:
	.asciz	"OPTIONAL_EXPR"         # string offset=216
.Linfo_string18:
	.asciz	"SUB_EXPR"              # string offset=230
.Linfo_string19:
	.asciz	"SYMBOL_EXPR"           # string offset=239
.Linfo_string20:
	.asciz	"expr_type"             # string offset=251
.Linfo_string21:
	.asciz	"lexpr"                 # string offset=261
.Linfo_string22:
	.asciz	"rexpr"                 # string offset=267
.Linfo_string23:
	.asciz	"symbol"                # string offset=273
.Linfo_string24:
	.asciz	"char"                  # string offset=280
.Linfo_string25:
	.asciz	"mach"                  # string offset=285
.Linfo_string26:
	.asciz	"start"                 # string offset=290
.Linfo_string27:
	.asciz	"ACCEPTING_STATE"       # string offset=296
.Linfo_string28:
	.asciz	"EPSILON_STATE"         # string offset=312
.Linfo_string29:
	.asciz	"BRANCH_STATE"          # string offset=326
.Linfo_string30:
	.asciz	"SYMBOL_STATE"          # string offset=339
.Linfo_string31:
	.asciz	"DOTALL_STATE"          # string offset=352
.Linfo_string32:
	.asciz	"nfa_state_type"        # string offset=365
.Linfo_string33:
	.asciz	"id"                    # string offset=380
.Linfo_string34:
	.asciz	"next"                  # string offset=383
.Linfo_string35:
	.asciz	"left"                  # string offset=388
.Linfo_string36:
	.asciz	"right"                 # string offset=393
.Linfo_string37:
	.asciz	"nfa_state"             # string offset=399
.Linfo_string38:
	.asciz	"end"                   # string offset=409
.Linfo_string39:
	.asciz	"end1"                  # string offset=413
.Linfo_string40:
	.asciz	"nfa"                   # string offset=418
.Linfo_string41:
	.asciz	"sym"                   # string offset=422
.Linfo_string42:
	.asciz	"rval"                  # string offset=426
.Linfo_string43:
	.asciz	"__ARRAY_SIZE_TYPE__"   # string offset=431
.Linfo_string44:
	.asciz	"noop_nfa"              # string offset=451
.Linfo_string45:
	.asciz	"do_empty_nfa"          # string offset=460
.Linfo_string46:
	.asciz	"do_alt_nfa"            # string offset=473
.Linfo_string47:
	.asciz	"do_cat_nfa"            # string offset=484
.Linfo_string48:
	.asciz	"do_dotall_nfa"         # string offset=495
.Linfo_string49:
	.asciz	"do_symbol_nfa"         # string offset=509
.Linfo_string50:
	.asciz	"do_star_nfa"           # string offset=523
.Linfo_string51:
	.asciz	"do_plus_nfa"           # string offset=535
.Linfo_string52:
	.asciz	"do_optional_nfa"       # string offset=547
.Linfo_string53:
	.asciz	"nfa_context"           # string offset=563
.Linfo_string54:
	.asciz	"statebuf"              # string offset=575
.Linfo_string55:
	.asciz	"numstates"             # string offset=584
.Linfo_string56:
	.asciz	"long unsigned int"     # string offset=594
.Linfo_string57:
	.asciz	"size_t"                # string offset=612
.Linfo_string58:
	.asciz	"has_error"             # string offset=619
.Linfo_string59:
	.asciz	"_Bool"                 # string offset=629
.Linfo_string60:
	.asciz	"error"                 # string offset=635
.Linfo_string61:
	.asciz	"perror"                # string offset=641
.Linfo_string62:
	.asciz	"token_col"             # string offset=648
.Linfo_string63:
	.asciz	"expected"              # string offset=658
.Linfo_string64:
	.asciz	"actual"                # string offset=667
.Linfo_string65:
	.asciz	"parse_error"           # string offset=674
.Linfo_string66:
	.asciz	"nfa_error"             # string offset=686
.Linfo_string67:
	.asciz	"accepting_state"       # string offset=696
.Linfo_string68:
	.asciz	"epsilon_state"         # string offset=712
.Linfo_string69:
	.asciz	"dotall_state"          # string offset=726
.Linfo_string70:
	.asciz	"branch_state"          # string offset=739
.Linfo_string71:
	.asciz	"symbol_state"          # string offset=752
.Linfo_string72:
	.asciz	"setst"                 # string offset=765
.Linfo_string73:
	.asciz	"point"                 # string offset=771
.Linfo_string74:
	.asciz	"patch"                 # string offset=777
.Linfo_string75:
	.asciz	"smachine"              # string offset=783
.Linfo_string76:
	.asciz	"gmachine"              # string offset=792
.Linfo_string77:
	.asciz	"nfa_to_rval"           # string offset=801
.Linfo_string78:
	.asciz	"empty_machine"         # string offset=813
.Linfo_string79:
	.asciz	"dotall_machine"        # string offset=827
.Linfo_string80:
	.asciz	"symbol_machine"        # string offset=842
.Linfo_string81:
	.asciz	"alt_machine"           # string offset=857
.Linfo_string82:
	.asciz	"cat_machine"           # string offset=869
.Linfo_string83:
	.asciz	"posclosure_machine"    # string offset=881
.Linfo_string84:
	.asciz	"optional_machine"      # string offset=900
.Linfo_string85:
	.asciz	"closure_machine"       # string offset=917
.Linfo_string86:
	.asciz	"nfa_regex"             # string offset=933
.Linfo_string87:
	.asciz	"has_nfa_error"         # string offset=943
.Linfo_string88:
	.asciz	"print_nfa_error"       # string offset=957
.Linfo_string89:
	.asciz	"free_nfa_context"      # string offset=973
.Linfo_string90:
	.asciz	"eps_closure"           # string offset=990
.Linfo_string91:
	.asciz	"move"                  # string offset=1002
.Linfo_string92:
	.asciz	"accepts"               # string offset=1007
.Linfo_string93:
	.asciz	"nfa_match"             # string offset=1015
.Linfo_string94:
	.asciz	"print_nfa_states"      # string offset=1025
.Linfo_string95:
	.asciz	"print_state"           # string offset=1042
.Linfo_string96:
	.asciz	"print_state_table"     # string offset=1054
.Linfo_string97:
	.asciz	"context"               # string offset=1072
.Linfo_string98:
	.asciz	"lnfa"                  # string offset=1080
.Linfo_string99:
	.asciz	"state"                 # string offset=1085
.Linfo_string100:
	.asciz	"machine"               # string offset=1091
.Linfo_string101:
	.asciz	"first"                 # string offset=1099
.Linfo_string102:
	.asciz	"second"                # string offset=1105
.Linfo_string103:
	.asciz	"inner"                 # string offset=1112
.Linfo_string104:
	.asciz	"regex"                 # string offset=1118
.Linfo_string105:
	.asciz	"lmachine"              # string offset=1124
.Linfo_string106:
	.asciz	"pcontext"              # string offset=1133
.Linfo_string107:
	.asciz	"result_context"        # string offset=1142
.Linfo_string108:
	.asciz	"actions"               # string offset=1157
.Linfo_string109:
	.asciz	"getval"                # string offset=1165
.Linfo_string110:
	.asciz	"scan_context"          # string offset=1172
.Linfo_string111:
	.asciz	"input"                 # string offset=1185
.Linfo_string112:
	.asciz	"input_col"             # string offset=1191
.Linfo_string113:
	.asciz	"token"                 # string offset=1201
.Linfo_string114:
	.asciz	"lookahead"             # string offset=1207
.Linfo_string115:
	.asciz	"lookahead_col"         # string offset=1217
.Linfo_string116:
	.asciz	"parse_context"         # string offset=1231
.Linfo_string117:
	.asciz	"nstates"               # string offset=1245
.Linfo_string118:
	.asciz	"head"                  # string offset=1253
.Linfo_string119:
	.asciz	"val"                   # string offset=1258
.Linfo_string120:
	.asciz	"node"                  # string offset=1262
.Linfo_string121:
	.asciz	"last"                  # string offset=1267
.Linfo_string122:
	.asciz	"list"                  # string offset=1272
.Linfo_string123:
	.asciz	"already_on"            # string offset=1277
.Linfo_string124:
	.asciz	"cstates"               # string offset=1288
.Linfo_string125:
	.asciz	"c"                     # string offset=1296
.Linfo_string126:
	.asciz	"accept"                # string offset=1298
.Linfo_string127:
	.asciz	"str"                   # string offset=1305
.Linfo_string128:
	.asciz	"t"                     # string offset=1309
.Linfo_string129:
	.asciz	"result"                # string offset=1311
	.section	.debug_loc,"",@progbits
.Ldebug_loc0:
	.quad	.Ltmp46-.Lfunc_begin0
	.quad	.Ltmp47-.Lfunc_begin0
	.short	2                       # Loc expr size
	.byte	117                     # DW_OP_breg5
	.byte	0                       # 0
	.quad	.Ltmp47-.Lfunc_begin0
	.quad	.Lfunc_end21-.Lfunc_begin0
	.short	3                       # Loc expr size
	.byte	118                     # DW_OP_breg6
	.byte	88                      # -40
	.byte	6                       # DW_OP_deref
	.quad	0
	.quad	0
.Ldebug_loc1:
	.quad	.Ltmp49-.Lfunc_begin0
	.quad	.Ltmp50-.Lfunc_begin0
	.short	2                       # Loc expr size
	.byte	117                     # DW_OP_breg5
	.byte	0                       # 0
	.quad	.Ltmp50-.Lfunc_begin0
	.quad	.Lfunc_end22-.Lfunc_begin0
	.short	3                       # Loc expr size
	.byte	118                     # DW_OP_breg6
	.byte	88                      # -40
	.byte	6                       # DW_OP_deref
	.quad	0
	.quad	0
.Ldebug_loc2:
	.quad	.Ltmp52-.Lfunc_begin0
	.quad	.Ltmp53-.Lfunc_begin0
	.short	2                       # Loc expr size
	.byte	117                     # DW_OP_breg5
	.byte	0                       # 0
	.quad	.Ltmp53-.Lfunc_begin0
	.quad	.Lfunc_end23-.Lfunc_begin0
	.short	3                       # Loc expr size
	.byte	118                     # DW_OP_breg6
	.byte	80                      # -48
	.byte	6                       # DW_OP_deref
	.quad	0
	.quad	0
.Ldebug_loc3:
	.quad	.Ltmp55-.Lfunc_begin0
	.quad	.Ltmp56-.Lfunc_begin0
	.short	2                       # Loc expr size
	.byte	117                     # DW_OP_breg5
	.byte	0                       # 0
	.quad	.Ltmp56-.Lfunc_begin0
	.quad	.Lfunc_end24-.Lfunc_begin0
	.short	4                       # Loc expr size
	.byte	118                     # DW_OP_breg6
	.byte	168                     # -88
	.byte	127                     # 
	.byte	6                       # DW_OP_deref
	.quad	0
	.quad	0
.Ldebug_loc4:
	.quad	.Ltmp58-.Lfunc_begin0
	.quad	.Ltmp59-.Lfunc_begin0
	.short	2                       # Loc expr size
	.byte	117                     # DW_OP_breg5
	.byte	0                       # 0
	.quad	.Ltmp59-.Lfunc_begin0
	.quad	.Lfunc_end25-.Lfunc_begin0
	.short	3                       # Loc expr size
	.byte	118                     # DW_OP_breg6
	.byte	120                     # -8
	.byte	6                       # DW_OP_deref
	.quad	0
	.quad	0
.Ldebug_loc5:
	.quad	.Ltmp61-.Lfunc_begin0
	.quad	.Ltmp62-.Lfunc_begin0
	.short	2                       # Loc expr size
	.byte	117                     # DW_OP_breg5
	.byte	0                       # 0
	.quad	.Ltmp62-.Lfunc_begin0
	.quad	.Lfunc_end26-.Lfunc_begin0
	.short	3                       # Loc expr size
	.byte	118                     # DW_OP_breg6
	.byte	88                      # -40
	.byte	6                       # DW_OP_deref
	.quad	0
	.quad	0
.Ldebug_loc6:
	.quad	.Ltmp64-.Lfunc_begin0
	.quad	.Ltmp65-.Lfunc_begin0
	.short	2                       # Loc expr size
	.byte	117                     # DW_OP_breg5
	.byte	0                       # 0
	.quad	.Ltmp65-.Lfunc_begin0
	.quad	.Lfunc_end27-.Lfunc_begin0
	.short	3                       # Loc expr size
	.byte	118                     # DW_OP_breg6
	.byte	64                      # -64
	.byte	6                       # DW_OP_deref
	.quad	0
	.quad	0
	.section	.debug_abbrev,"",@progbits
	.byte	1                       # Abbreviation Code
	.byte	17                      # DW_TAG_compile_unit
	.byte	1                       # DW_CHILDREN_yes
	.byte	37                      # DW_AT_producer
	.byte	14                      # DW_FORM_strp
	.byte	19                      # DW_AT_language
	.byte	5                       # DW_FORM_data2
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	16                      # DW_AT_stmt_list
	.byte	23                      # DW_FORM_sec_offset
	.byte	27                      # DW_AT_comp_dir
	.byte	14                      # DW_FORM_strp
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	6                       # DW_FORM_data4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	2                       # Abbreviation Code
	.byte	52                      # DW_TAG_variable
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	63                      # DW_AT_external
	.byte	25                      # DW_FORM_flag_present
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	2                       # DW_AT_location
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	3                       # Abbreviation Code
	.byte	36                      # DW_TAG_base_type
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	62                      # DW_AT_encoding
	.byte	11                      # DW_FORM_data1
	.byte	11                      # DW_AT_byte_size
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	4                       # Abbreviation Code
	.byte	1                       # DW_TAG_array_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	5                       # Abbreviation Code
	.byte	33                      # DW_TAG_subrange_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	55                      # DW_AT_count
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	6                       # Abbreviation Code
	.byte	15                      # DW_TAG_pointer_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	7                       # Abbreviation Code
	.byte	21                      # DW_TAG_subroutine_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	8                       # Abbreviation Code
	.byte	5                       # DW_TAG_formal_parameter
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	9                       # Abbreviation Code
	.byte	15                      # DW_TAG_pointer_type
	.byte	0                       # DW_CHILDREN_no
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	10                      # Abbreviation Code
	.byte	23                      # DW_TAG_union_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	11                      # DW_AT_byte_size
	.byte	11                      # DW_FORM_data1
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	11                      # Abbreviation Code
	.byte	13                      # DW_TAG_member
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	56                      # DW_AT_data_member_location
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	12                      # Abbreviation Code
	.byte	19                      # DW_TAG_structure_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	11                      # DW_AT_byte_size
	.byte	11                      # DW_FORM_data1
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	13                      # Abbreviation Code
	.byte	13                      # DW_TAG_member
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	56                      # DW_AT_data_member_location
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	14                      # Abbreviation Code
	.byte	23                      # DW_TAG_union_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	11                      # DW_AT_byte_size
	.byte	11                      # DW_FORM_data1
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	15                      # Abbreviation Code
	.byte	19                      # DW_TAG_structure_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	11                      # DW_AT_byte_size
	.byte	11                      # DW_FORM_data1
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	16                      # Abbreviation Code
	.byte	4                       # DW_TAG_enumeration_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	11                      # DW_AT_byte_size
	.byte	11                      # DW_FORM_data1
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	17                      # Abbreviation Code
	.byte	40                      # DW_TAG_enumerator
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	28                      # DW_AT_const_value
	.byte	15                      # DW_FORM_udata
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	18                      # Abbreviation Code
	.byte	36                      # DW_TAG_base_type
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	11                      # DW_AT_byte_size
	.byte	11                      # DW_FORM_data1
	.byte	62                      # DW_AT_encoding
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	19                      # Abbreviation Code
	.byte	21                      # DW_TAG_subroutine_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	20                      # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	6                       # DW_FORM_data4
	.byte	64                      # DW_AT_frame_base
	.byte	24                      # DW_FORM_exprloc
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	63                      # DW_AT_external
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	21                      # Abbreviation Code
	.byte	5                       # DW_TAG_formal_parameter
	.byte	0                       # DW_CHILDREN_no
	.byte	2                       # DW_AT_location
	.byte	24                      # DW_FORM_exprloc
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	22                      # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	6                       # DW_FORM_data4
	.byte	64                      # DW_AT_frame_base
	.byte	24                      # DW_FORM_exprloc
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	63                      # DW_AT_external
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	23                      # Abbreviation Code
	.byte	5                       # DW_TAG_formal_parameter
	.byte	0                       # DW_CHILDREN_no
	.byte	2                       # DW_AT_location
	.byte	24                      # DW_FORM_exprloc
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	24                      # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	0                       # DW_CHILDREN_no
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	6                       # DW_FORM_data4
	.byte	64                      # DW_AT_frame_base
	.byte	24                      # DW_FORM_exprloc
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	63                      # DW_AT_external
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	25                      # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	6                       # DW_FORM_data4
	.byte	64                      # DW_AT_frame_base
	.byte	24                      # DW_FORM_exprloc
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	63                      # DW_AT_external
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	26                      # Abbreviation Code
	.byte	52                      # DW_TAG_variable
	.byte	0                       # DW_CHILDREN_no
	.byte	2                       # DW_AT_location
	.byte	23                      # DW_FORM_sec_offset
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	27                      # Abbreviation Code
	.byte	11                      # DW_TAG_lexical_block
	.byte	1                       # DW_CHILDREN_yes
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	6                       # DW_FORM_data4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	28                      # Abbreviation Code
	.byte	52                      # DW_TAG_variable
	.byte	0                       # DW_CHILDREN_no
	.byte	2                       # DW_AT_location
	.byte	24                      # DW_FORM_exprloc
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	29                      # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	6                       # DW_FORM_data4
	.byte	64                      # DW_AT_frame_base
	.byte	24                      # DW_FORM_exprloc
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	63                      # DW_AT_external
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	30                      # Abbreviation Code
	.byte	52                      # DW_TAG_variable
	.byte	0                       # DW_CHILDREN_no
	.byte	2                       # DW_AT_location
	.byte	24                      # DW_FORM_exprloc
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	31                      # Abbreviation Code
	.byte	22                      # DW_TAG_typedef
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	0                       # EOM(3)
	.section	.debug_info,"",@progbits
.Lcu_begin0:
	.long	.Ldebug_info_end0-.Ldebug_info_start0 # Length of Unit
.Ldebug_info_start0:
	.short	4                       # DWARF version number
	.long	.debug_abbrev           # Offset Into Abbrev. Section
	.byte	8                       # Address Size (in bytes)
	.byte	1                       # Abbrev [1] 0xb:0xd64 DW_TAG_compile_unit
	.long	.Linfo_string0          # DW_AT_producer
	.short	12                      # DW_AT_language
	.long	.Linfo_string1          # DW_AT_name
	.long	.Lline_table_start0     # DW_AT_stmt_list
	.long	.Linfo_string2          # DW_AT_comp_dir
	.quad	.Lfunc_begin0           # DW_AT_low_pc
	.long	.Lfunc_end40-.Lfunc_begin0 # DW_AT_high_pc
	.byte	2                       # Abbrev [2] 0x2a:0x15 DW_TAG_variable
	.long	.Linfo_string3          # DW_AT_name
	.long	63                      # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.byte	11                      # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	uid
	.byte	3                       # Abbrev [3] 0x3f:0x7 DW_TAG_base_type
	.long	.Linfo_string4          # DW_AT_name
	.byte	5                       # DW_AT_encoding
	.byte	4                       # DW_AT_byte_size
	.byte	2                       # Abbrev [2] 0x46:0x15 DW_TAG_variable
	.long	.Linfo_string5          # DW_AT_name
	.long	91                      # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.byte	13                      # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	nfa_actions
	.byte	4                       # Abbrev [4] 0x5b:0xc DW_TAG_array_type
	.long	103                     # DW_AT_type
	.byte	5                       # Abbrev [5] 0x60:0x6 DW_TAG_subrange_type
	.long	609                     # DW_AT_type
	.byte	10                      # DW_AT_count
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x67:0x5 DW_TAG_pointer_type
	.long	108                     # DW_AT_type
	.byte	7                       # Abbrev [7] 0x6c:0xc DW_TAG_subroutine_type
                                        # DW_AT_prototyped
	.byte	8                       # Abbrev [8] 0x6d:0x5 DW_TAG_formal_parameter
	.long	120                     # DW_AT_type
	.byte	8                       # Abbrev [8] 0x72:0x5 DW_TAG_formal_parameter
	.long	121                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	9                       # Abbrev [9] 0x78:0x1 DW_TAG_pointer_type
	.byte	10                      # Abbrev [10] 0x79:0x39 DW_TAG_union_type
	.long	.Linfo_string42         # DW_AT_name
	.byte	24                      # DW_AT_byte_size
	.byte	2                       # DW_AT_decl_file
	.byte	56                      # DW_AT_decl_line
	.byte	11                      # Abbrev [11] 0x81:0xc DW_TAG_member
	.long	.Linfo_string6          # DW_AT_name
	.long	120                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	57                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0x8d:0xc DW_TAG_member
	.long	.Linfo_string7          # DW_AT_name
	.long	178                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	58                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0x99:0xc DW_TAG_member
	.long	.Linfo_string25         # DW_AT_name
	.long	391                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	59                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xa5:0xc DW_TAG_member
	.long	.Linfo_string41         # DW_AT_name
	.long	384                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	60                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0xb2:0x5 DW_TAG_pointer_type
	.long	183                     # DW_AT_type
	.byte	12                      # Abbrev [12] 0xb7:0x79 DW_TAG_structure_type
	.long	.Linfo_string7          # DW_AT_name
	.byte	24                      # DW_AT_byte_size
	.byte	2                       # DW_AT_decl_file
	.byte	17                      # DW_AT_decl_line
	.byte	11                      # Abbrev [11] 0xbf:0xc DW_TAG_member
	.long	.Linfo_string8          # DW_AT_name
	.long	304                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	18                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	13                      # Abbrev [13] 0xcb:0x8 DW_TAG_member
	.long	211                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	19                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	14                      # Abbrev [14] 0xd3:0x5c DW_TAG_union_type
	.byte	16                      # DW_AT_byte_size
	.byte	2                       # DW_AT_decl_file
	.byte	19                      # DW_AT_decl_line
	.byte	13                      # Abbrev [13] 0xd7:0x8 DW_TAG_member
	.long	223                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	21                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	15                      # Abbrev [15] 0xdf:0x1d DW_TAG_structure_type
	.byte	16                      # DW_AT_byte_size
	.byte	2                       # DW_AT_decl_file
	.byte	21                      # DW_AT_decl_line
	.byte	11                      # Abbrev [11] 0xe3:0xc DW_TAG_member
	.long	.Linfo_string21         # DW_AT_name
	.long	178                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	21                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xef:0xc DW_TAG_member
	.long	.Linfo_string22         # DW_AT_name
	.long	178                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	21                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	13                      # Abbrev [13] 0xfc:0x8 DW_TAG_member
	.long	260                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	23                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	15                      # Abbrev [15] 0x104:0x11 DW_TAG_structure_type
	.byte	8                       # DW_AT_byte_size
	.byte	2                       # DW_AT_decl_file
	.byte	23                      # DW_AT_decl_line
	.byte	11                      # Abbrev [11] 0x108:0xc DW_TAG_member
	.long	.Linfo_string7          # DW_AT_name
	.long	178                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	23                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	13                      # Abbrev [13] 0x115:0x8 DW_TAG_member
	.long	285                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	25                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	15                      # Abbrev [15] 0x11d:0x11 DW_TAG_structure_type
	.byte	1                       # DW_AT_byte_size
	.byte	2                       # DW_AT_decl_file
	.byte	25                      # DW_AT_decl_line
	.byte	11                      # Abbrev [11] 0x121:0xc DW_TAG_member
	.long	.Linfo_string23         # DW_AT_name
	.long	384                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	25                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	16                      # Abbrev [16] 0x130:0x49 DW_TAG_enumeration_type
	.long	377                     # DW_AT_type
	.long	.Linfo_string20         # DW_AT_name
	.byte	4                       # DW_AT_byte_size
	.byte	2                       # DW_AT_decl_file
	.byte	4                       # DW_AT_decl_line
	.byte	17                      # Abbrev [17] 0x13c:0x6 DW_TAG_enumerator
	.long	.Linfo_string10         # DW_AT_name
	.byte	0                       # DW_AT_const_value
	.byte	17                      # Abbrev [17] 0x142:0x6 DW_TAG_enumerator
	.long	.Linfo_string11         # DW_AT_name
	.byte	1                       # DW_AT_const_value
	.byte	17                      # Abbrev [17] 0x148:0x6 DW_TAG_enumerator
	.long	.Linfo_string12         # DW_AT_name
	.byte	2                       # DW_AT_const_value
	.byte	17                      # Abbrev [17] 0x14e:0x6 DW_TAG_enumerator
	.long	.Linfo_string13         # DW_AT_name
	.byte	3                       # DW_AT_const_value
	.byte	17                      # Abbrev [17] 0x154:0x6 DW_TAG_enumerator
	.long	.Linfo_string14         # DW_AT_name
	.byte	4                       # DW_AT_const_value
	.byte	17                      # Abbrev [17] 0x15a:0x6 DW_TAG_enumerator
	.long	.Linfo_string15         # DW_AT_name
	.byte	5                       # DW_AT_const_value
	.byte	17                      # Abbrev [17] 0x160:0x6 DW_TAG_enumerator
	.long	.Linfo_string16         # DW_AT_name
	.byte	6                       # DW_AT_const_value
	.byte	17                      # Abbrev [17] 0x166:0x6 DW_TAG_enumerator
	.long	.Linfo_string17         # DW_AT_name
	.byte	7                       # DW_AT_const_value
	.byte	17                      # Abbrev [17] 0x16c:0x6 DW_TAG_enumerator
	.long	.Linfo_string18         # DW_AT_name
	.byte	8                       # DW_AT_const_value
	.byte	17                      # Abbrev [17] 0x172:0x6 DW_TAG_enumerator
	.long	.Linfo_string19         # DW_AT_name
	.byte	9                       # DW_AT_const_value
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x179:0x7 DW_TAG_base_type
	.long	.Linfo_string9          # DW_AT_name
	.byte	7                       # DW_AT_encoding
	.byte	4                       # DW_AT_byte_size
	.byte	3                       # Abbrev [3] 0x180:0x7 DW_TAG_base_type
	.long	.Linfo_string24         # DW_AT_name
	.byte	6                       # DW_AT_encoding
	.byte	1                       # DW_AT_byte_size
	.byte	12                      # Abbrev [12] 0x187:0x2d DW_TAG_structure_type
	.long	.Linfo_string40         # DW_AT_name
	.byte	24                      # DW_AT_byte_size
	.byte	2                       # DW_AT_decl_file
	.byte	50                      # DW_AT_decl_line
	.byte	11                      # Abbrev [11] 0x18f:0xc DW_TAG_member
	.long	.Linfo_string26         # DW_AT_name
	.long	436                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	51                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0x19b:0xc DW_TAG_member
	.long	.Linfo_string38         # DW_AT_name
	.long	604                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	52                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0x1a7:0xc DW_TAG_member
	.long	.Linfo_string39         # DW_AT_name
	.long	604                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	53                      # DW_AT_decl_line
	.byte	16                      # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x1b4:0x5 DW_TAG_pointer_type
	.long	441                     # DW_AT_type
	.byte	12                      # Abbrev [12] 0x1b9:0x78 DW_TAG_structure_type
	.long	.Linfo_string37         # DW_AT_name
	.byte	24                      # DW_AT_byte_size
	.byte	2                       # DW_AT_decl_file
	.byte	38                      # DW_AT_decl_line
	.byte	11                      # Abbrev [11] 0x1c1:0xc DW_TAG_member
	.long	.Linfo_string8          # DW_AT_name
	.long	561                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	39                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0x1cd:0xc DW_TAG_member
	.long	.Linfo_string33         # DW_AT_name
	.long	63                      # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	40                      # DW_AT_decl_line
	.byte	4                       # DW_AT_data_member_location
	.byte	13                      # Abbrev [13] 0x1d9:0x8 DW_TAG_member
	.long	481                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	41                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	14                      # Abbrev [14] 0x1e1:0x4f DW_TAG_union_type
	.byte	16                      # DW_AT_byte_size
	.byte	2                       # DW_AT_decl_file
	.byte	41                      # DW_AT_decl_line
	.byte	13                      # Abbrev [13] 0x1e5:0x8 DW_TAG_member
	.long	493                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	43                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	15                      # Abbrev [15] 0x1ed:0x1d DW_TAG_structure_type
	.byte	16                      # DW_AT_byte_size
	.byte	2                       # DW_AT_decl_file
	.byte	43                      # DW_AT_decl_line
	.byte	11                      # Abbrev [11] 0x1f1:0xc DW_TAG_member
	.long	.Linfo_string34         # DW_AT_name
	.long	436                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	43                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0x1fd:0xc DW_TAG_member
	.long	.Linfo_string23         # DW_AT_name
	.long	384                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	43                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	13                      # Abbrev [13] 0x20a:0x8 DW_TAG_member
	.long	530                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	45                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	15                      # Abbrev [15] 0x212:0x1d DW_TAG_structure_type
	.byte	16                      # DW_AT_byte_size
	.byte	2                       # DW_AT_decl_file
	.byte	45                      # DW_AT_decl_line
	.byte	11                      # Abbrev [11] 0x216:0xc DW_TAG_member
	.long	.Linfo_string35         # DW_AT_name
	.long	436                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	45                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0x222:0xc DW_TAG_member
	.long	.Linfo_string36         # DW_AT_name
	.long	436                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	45                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	16                      # Abbrev [16] 0x231:0x2b DW_TAG_enumeration_type
	.long	377                     # DW_AT_type
	.long	.Linfo_string32         # DW_AT_name
	.byte	4                       # DW_AT_byte_size
	.byte	2                       # DW_AT_decl_file
	.byte	30                      # DW_AT_decl_line
	.byte	17                      # Abbrev [17] 0x23d:0x6 DW_TAG_enumerator
	.long	.Linfo_string27         # DW_AT_name
	.byte	0                       # DW_AT_const_value
	.byte	17                      # Abbrev [17] 0x243:0x6 DW_TAG_enumerator
	.long	.Linfo_string28         # DW_AT_name
	.byte	1                       # DW_AT_const_value
	.byte	17                      # Abbrev [17] 0x249:0x6 DW_TAG_enumerator
	.long	.Linfo_string29         # DW_AT_name
	.byte	2                       # DW_AT_const_value
	.byte	17                      # Abbrev [17] 0x24f:0x6 DW_TAG_enumerator
	.long	.Linfo_string30         # DW_AT_name
	.byte	3                       # DW_AT_const_value
	.byte	17                      # Abbrev [17] 0x255:0x6 DW_TAG_enumerator
	.long	.Linfo_string31         # DW_AT_name
	.byte	4                       # DW_AT_const_value
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x25c:0x5 DW_TAG_pointer_type
	.long	436                     # DW_AT_type
	.byte	18                      # Abbrev [18] 0x261:0x7 DW_TAG_base_type
	.long	.Linfo_string43         # DW_AT_name
	.byte	8                       # DW_AT_byte_size
	.byte	7                       # DW_AT_encoding
	.byte	6                       # Abbrev [6] 0x268:0x5 DW_TAG_pointer_type
	.long	621                     # DW_AT_type
	.byte	19                      # Abbrev [19] 0x26d:0xb DW_TAG_subroutine_type
	.long	121                     # DW_AT_type
                                        # DW_AT_prototyped
	.byte	8                       # Abbrev [8] 0x272:0x5 DW_TAG_formal_parameter
	.long	120                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0x278:0x35 DW_TAG_subprogram
	.quad	.Lfunc_begin0           # DW_AT_low_pc
	.long	.Lfunc_end0-.Lfunc_begin0 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string44         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	312                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	21                      # Abbrev [21] 0x28e:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	312                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	21                      # Abbrev [21] 0x29d:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string6          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	312                     # DW_AT_decl_line
	.long	121                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0x2ad:0x35 DW_TAG_subprogram
	.quad	.Lfunc_begin1           # DW_AT_low_pc
	.long	.Lfunc_end1-.Lfunc_begin1 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string45         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	314                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	21                      # Abbrev [21] 0x2c3:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	314                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	21                      # Abbrev [21] 0x2d2:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string6          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	314                     # DW_AT_decl_line
	.long	121                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0x2e2:0x35 DW_TAG_subprogram
	.quad	.Lfunc_begin2           # DW_AT_low_pc
	.long	.Lfunc_end2-.Lfunc_begin2 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string46         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	318                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	21                      # Abbrev [21] 0x2f8:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	318                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	21                      # Abbrev [21] 0x307:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string98         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	318                     # DW_AT_decl_line
	.long	121                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0x317:0x35 DW_TAG_subprogram
	.quad	.Lfunc_begin3           # DW_AT_low_pc
	.long	.Lfunc_end3-.Lfunc_begin3 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string47         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	322                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	21                      # Abbrev [21] 0x32d:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	322                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	21                      # Abbrev [21] 0x33c:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string98         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	322                     # DW_AT_decl_line
	.long	121                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0x34c:0x35 DW_TAG_subprogram
	.quad	.Lfunc_begin4           # DW_AT_low_pc
	.long	.Lfunc_end4-.Lfunc_begin4 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string48         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	326                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	21                      # Abbrev [21] 0x362:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	326                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	21                      # Abbrev [21] 0x371:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string6          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	326                     # DW_AT_decl_line
	.long	121                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0x381:0x35 DW_TAG_subprogram
	.quad	.Lfunc_begin5           # DW_AT_low_pc
	.long	.Lfunc_end5-.Lfunc_begin5 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string49         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	330                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	21                      # Abbrev [21] 0x397:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	330                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	21                      # Abbrev [21] 0x3a6:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string41         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	330                     # DW_AT_decl_line
	.long	121                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0x3b6:0x35 DW_TAG_subprogram
	.quad	.Lfunc_begin6           # DW_AT_low_pc
	.long	.Lfunc_end6-.Lfunc_begin6 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string50         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	334                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	21                      # Abbrev [21] 0x3cc:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	334                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	21                      # Abbrev [21] 0x3db:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string6          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	334                     # DW_AT_decl_line
	.long	121                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0x3eb:0x35 DW_TAG_subprogram
	.quad	.Lfunc_begin7           # DW_AT_low_pc
	.long	.Lfunc_end7-.Lfunc_begin7 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string51         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	338                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	21                      # Abbrev [21] 0x401:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	338                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	21                      # Abbrev [21] 0x410:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string6          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	338                     # DW_AT_decl_line
	.long	121                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0x420:0x35 DW_TAG_subprogram
	.quad	.Lfunc_begin8           # DW_AT_low_pc
	.long	.Lfunc_end8-.Lfunc_begin8 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string52         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	342                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	21                      # Abbrev [21] 0x436:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	342                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	21                      # Abbrev [21] 0x445:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string6          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	342                     # DW_AT_decl_line
	.long	121                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x455:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin9           # DW_AT_low_pc
	.long	.Lfunc_end9-.Lfunc_begin9 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string53         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	26                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	3015                    # DW_AT_type
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x46e:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string54         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	26                      # DW_AT_decl_line
	.long	436                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	24                      # Abbrev [24] 0x47d:0x19 DW_TAG_subprogram
	.quad	.Lfunc_begin10          # DW_AT_low_pc
	.long	.Lfunc_end10-.Lfunc_begin10 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string67         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	35                      # DW_AT_decl_line
	.long	441                     # DW_AT_type
                                        # DW_AT_external
	.byte	22                      # Abbrev [22] 0x496:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin11          # DW_AT_low_pc
	.long	.Lfunc_end11-.Lfunc_begin11 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string68         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	41                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	441                     # DW_AT_type
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x4af:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string34         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	41                      # DW_AT_decl_line
	.long	436                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x4be:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin12          # DW_AT_low_pc
	.long	.Lfunc_end12-.Lfunc_begin12 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string69         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	48                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	441                     # DW_AT_type
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x4d7:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string34         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	48                      # DW_AT_decl_line
	.long	436                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x4e6:0x36 DW_TAG_subprogram
	.quad	.Lfunc_begin13          # DW_AT_low_pc
	.long	.Lfunc_end13-.Lfunc_begin13 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string70         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	55                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	441                     # DW_AT_type
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x4ff:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string35         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	55                      # DW_AT_decl_line
	.long	436                     # DW_AT_type
	.byte	23                      # Abbrev [23] 0x50d:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string36         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	55                      # DW_AT_decl_line
	.long	436                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x51c:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin14          # DW_AT_low_pc
	.long	.Lfunc_end14-.Lfunc_begin14 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string71         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	63                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	441                     # DW_AT_type
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x535:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	127
	.long	.Linfo_string23         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	63                      # DW_AT_decl_line
	.long	384                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x544:0x36 DW_TAG_subprogram
	.quad	.Lfunc_begin15          # DW_AT_low_pc
	.long	.Lfunc_end15-.Lfunc_begin15 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string72         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	71                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	436                     # DW_AT_type
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x55d:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	71                      # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x56b:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string99         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	71                      # DW_AT_decl_line
	.long	441                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0x57a:0x40 DW_TAG_subprogram
	.quad	.Lfunc_begin16          # DW_AT_low_pc
	.long	.Lfunc_end16-.Lfunc_begin16 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string73         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	78                      # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x58f:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string100        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	78                      # DW_AT_decl_line
	.long	3180                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x59d:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string38         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	78                      # DW_AT_decl_line
	.long	604                     # DW_AT_type
	.byte	23                      # Abbrev [23] 0x5ab:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.long	.Linfo_string39         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	78                      # DW_AT_decl_line
	.long	604                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0x5ba:0x32 DW_TAG_subprogram
	.quad	.Lfunc_begin17          # DW_AT_low_pc
	.long	.Lfunc_end17-.Lfunc_begin17 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string74         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	83                      # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x5cf:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string100        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	83                      # DW_AT_decl_line
	.long	391                     # DW_AT_type
	.byte	23                      # Abbrev [23] 0x5dd:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string99         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	83                      # DW_AT_decl_line
	.long	436                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0x5ec:0x32 DW_TAG_subprogram
	.quad	.Lfunc_begin18          # DW_AT_low_pc
	.long	.Lfunc_end18-.Lfunc_begin18 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string75         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	88                      # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x601:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	88                      # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x60f:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string100        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	88                      # DW_AT_decl_line
	.long	391                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x61e:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin19          # DW_AT_low_pc
	.long	.Lfunc_end19-.Lfunc_begin19 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string76         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	92                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	391                     # DW_AT_type
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x637:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	92                      # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x646:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin20          # DW_AT_low_pc
	.long	.Lfunc_end20-.Lfunc_begin20 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string77         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	96                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	121                     # DW_AT_type
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x65f:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	96                      # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x66e:0x37 DW_TAG_subprogram
	.quad	.Lfunc_begin21          # DW_AT_low_pc
	.long	.Lfunc_end21-.Lfunc_begin21 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string78         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	100                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	391                     # DW_AT_type
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x687:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	100                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	26                      # Abbrev [26] 0x695:0xf DW_TAG_variable
	.long	.Ldebug_loc0            # DW_AT_location
	.long	.Linfo_string100        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	101                     # DW_AT_decl_line
	.long	391                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x6a5:0x37 DW_TAG_subprogram
	.quad	.Lfunc_begin22          # DW_AT_low_pc
	.long	.Lfunc_end22-.Lfunc_begin22 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string79         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	110                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	391                     # DW_AT_type
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x6be:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	110                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	26                      # Abbrev [26] 0x6cc:0xf DW_TAG_variable
	.long	.Ldebug_loc1            # DW_AT_location
	.long	.Linfo_string100        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	111                     # DW_AT_decl_line
	.long	391                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x6dc:0x45 DW_TAG_subprogram
	.quad	.Lfunc_begin23          # DW_AT_low_pc
	.long	.Lfunc_end23-.Lfunc_begin23 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string80         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	120                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	391                     # DW_AT_type
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x6f5:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	120                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x703:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	119
	.long	.Linfo_string23         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	120                     # DW_AT_decl_line
	.long	384                     # DW_AT_type
	.byte	26                      # Abbrev [26] 0x711:0xf DW_TAG_variable
	.long	.Ldebug_loc2            # DW_AT_location
	.long	.Linfo_string100        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	121                     # DW_AT_decl_line
	.long	391                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x721:0x53 DW_TAG_subprogram
	.quad	.Lfunc_begin24          # DW_AT_low_pc
	.long	.Lfunc_end24-.Lfunc_begin24 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string81         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	130                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	391                     # DW_AT_type
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x73a:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	130                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x748:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string35         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	130                     # DW_AT_decl_line
	.long	391                     # DW_AT_type
	.byte	23                      # Abbrev [23] 0x756:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	40
	.long	.Linfo_string36         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	130                     # DW_AT_decl_line
	.long	391                     # DW_AT_type
	.byte	26                      # Abbrev [26] 0x764:0xf DW_TAG_variable
	.long	.Ldebug_loc3            # DW_AT_location
	.long	.Linfo_string100        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	131                     # DW_AT_decl_line
	.long	391                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x774:0x45 DW_TAG_subprogram
	.quad	.Lfunc_begin25          # DW_AT_low_pc
	.long	.Lfunc_end25-.Lfunc_begin25 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string82         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	142                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	391                     # DW_AT_type
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x78d:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string101        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	142                     # DW_AT_decl_line
	.long	391                     # DW_AT_type
	.byte	23                      # Abbrev [23] 0x79b:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	40
	.long	.Linfo_string102        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	142                     # DW_AT_decl_line
	.long	391                     # DW_AT_type
	.byte	26                      # Abbrev [26] 0x7a9:0xf DW_TAG_variable
	.long	.Ldebug_loc4            # DW_AT_location
	.long	.Linfo_string100        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	143                     # DW_AT_decl_line
	.long	391                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x7b9:0x45 DW_TAG_subprogram
	.quad	.Lfunc_begin26          # DW_AT_low_pc
	.long	.Lfunc_end26-.Lfunc_begin26 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string83         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	153                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	391                     # DW_AT_type
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x7d2:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	153                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x7e0:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string103        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	153                     # DW_AT_decl_line
	.long	391                     # DW_AT_type
	.byte	26                      # Abbrev [26] 0x7ee:0xf DW_TAG_variable
	.long	.Ldebug_loc5            # DW_AT_location
	.long	.Linfo_string100        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	154                     # DW_AT_decl_line
	.long	391                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x7fe:0x45 DW_TAG_subprogram
	.quad	.Lfunc_begin27          # DW_AT_low_pc
	.long	.Lfunc_end27-.Lfunc_begin27 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string84         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	164                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	391                     # DW_AT_type
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x817:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	164                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x825:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string103        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	164                     # DW_AT_decl_line
	.long	391                     # DW_AT_type
	.byte	26                      # Abbrev [26] 0x833:0xf DW_TAG_variable
	.long	.Ldebug_loc6            # DW_AT_location
	.long	.Linfo_string100        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	165                     # DW_AT_decl_line
	.long	391                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x843:0x36 DW_TAG_subprogram
	.quad	.Lfunc_begin28          # DW_AT_low_pc
	.long	.Lfunc_end28-.Lfunc_begin28 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string85         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	175                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	391                     # DW_AT_type
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x85c:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	175                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x86a:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string103        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	175                     # DW_AT_decl_line
	.long	391                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x879:0x61 DW_TAG_subprogram
	.quad	.Lfunc_begin29          # DW_AT_low_pc
	.long	.Lfunc_end29-.Lfunc_begin29 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string86         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	182                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	3175                    # DW_AT_type
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x892:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string104        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	182                     # DW_AT_decl_line
	.long	3185                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x8a0:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	182                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	27                      # Abbrev [27] 0x8ae:0x2b DW_TAG_lexical_block
	.quad	.Ltmp74                 # DW_AT_low_pc
	.long	.Ltmp90-.Ltmp74         # DW_AT_high_pc
	.byte	28                      # Abbrev [28] 0x8bb:0xe DW_TAG_variable
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.long	.Linfo_string105        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	186                     # DW_AT_decl_line
	.long	391                     # DW_AT_type
	.byte	28                      # Abbrev [28] 0x8c9:0xf DW_TAG_variable
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\220\177"
	.long	.Linfo_string106        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	187                     # DW_AT_decl_line
	.long	3190                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x8da:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin30          # DW_AT_low_pc
	.long	.Lfunc_end30-.Lfunc_begin30 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string87         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	209                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	3102                    # DW_AT_type
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x8f3:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	209                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x902:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin31          # DW_AT_low_pc
	.long	.Lfunc_end31-.Lfunc_begin31 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string66         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	213                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	3109                    # DW_AT_type
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x91b:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	213                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0x92a:0x24 DW_TAG_subprogram
	.quad	.Lfunc_begin32          # DW_AT_low_pc
	.long	.Lfunc_end32-.Lfunc_begin32 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string88         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	218                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x93f:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string60         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	218                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0x94e:0x24 DW_TAG_subprogram
	.quad	.Lfunc_begin33          # DW_AT_low_pc
	.long	.Lfunc_end33-.Lfunc_begin33 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string89         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	222                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x963:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	222                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0x972:0x40 DW_TAG_subprogram
	.quad	.Lfunc_begin34          # DW_AT_low_pc
	.long	.Lfunc_end34-.Lfunc_begin34 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string90         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	227                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x987:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string117        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	227                     # DW_AT_decl_line
	.long	3357                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x995:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string99         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	227                     # DW_AT_decl_line
	.long	436                     # DW_AT_type
	.byte	23                      # Abbrev [23] 0x9a3:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.long	.Linfo_string123        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	227                     # DW_AT_decl_line
	.long	3433                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0x9b2:0x6a DW_TAG_subprogram
	.quad	.Lfunc_begin35          # DW_AT_low_pc
	.long	.Lfunc_end35-.Lfunc_begin35 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string91         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	248                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x9c7:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string117        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	248                     # DW_AT_decl_line
	.long	3357                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x9d5:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string124        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	248                     # DW_AT_decl_line
	.long	3357                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x9e3:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	111
	.long	.Linfo_string125        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	248                     # DW_AT_decl_line
	.long	384                     # DW_AT_type
	.byte	23                      # Abbrev [23] 0x9f1:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.long	.Linfo_string123        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	248                     # DW_AT_decl_line
	.long	3433                    # DW_AT_type
	.byte	28                      # Abbrev [28] 0x9ff:0xe DW_TAG_variable
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.long	.Linfo_string120        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	249                     # DW_AT_decl_line
	.long	3395                    # DW_AT_type
	.byte	28                      # Abbrev [28] 0xa0d:0xe DW_TAG_variable
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.long	.Linfo_string99         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	250                     # DW_AT_decl_line
	.long	436                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	29                      # Abbrev [29] 0xa1c:0x66 DW_TAG_subprogram
	.quad	.Lfunc_begin36          # DW_AT_low_pc
	.long	.Lfunc_end36-.Lfunc_begin36 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string92         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	260                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	3102                    # DW_AT_type
                                        # DW_AT_external
	.byte	21                      # Abbrev [21] 0xa36:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string124        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	260                     # DW_AT_decl_line
	.long	3357                    # DW_AT_type
	.byte	21                      # Abbrev [21] 0xa45:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.long	.Linfo_string126        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	260                     # DW_AT_decl_line
	.long	436                     # DW_AT_type
	.byte	30                      # Abbrev [30] 0xa54:0xf DW_TAG_variable
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.long	.Linfo_string120        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	261                     # DW_AT_decl_line
	.long	3395                    # DW_AT_type
	.byte	30                      # Abbrev [30] 0xa63:0xf DW_TAG_variable
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.long	.Linfo_string34         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	261                     # DW_AT_decl_line
	.long	3395                    # DW_AT_type
	.byte	30                      # Abbrev [30] 0xa72:0xf DW_TAG_variable
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.long	.Linfo_string99         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	262                     # DW_AT_decl_line
	.long	436                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	29                      # Abbrev [29] 0xa82:0xb5 DW_TAG_subprogram
	.quad	.Lfunc_begin37          # DW_AT_low_pc
	.long	.Lfunc_end37-.Lfunc_begin37 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string93         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	277                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	3102                    # DW_AT_type
                                        # DW_AT_external
	.byte	21                      # Abbrev [21] 0xa9c:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string127        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	277                     # DW_AT_decl_line
	.long	3185                    # DW_AT_type
	.byte	21                      # Abbrev [21] 0xaab:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	277                     # DW_AT_decl_line
	.long	3175                    # DW_AT_type
	.byte	30                      # Abbrev [30] 0xaba:0xf DW_TAG_variable
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.long	.Linfo_string40         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	278                     # DW_AT_decl_line
	.long	391                     # DW_AT_type
	.byte	30                      # Abbrev [30] 0xac9:0xf DW_TAG_variable
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.long	.Linfo_string55         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	279                     # DW_AT_decl_line
	.long	3084                    # DW_AT_type
	.byte	30                      # Abbrev [30] 0xad8:0xf DW_TAG_variable
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.long	.Linfo_string123        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	280                     # DW_AT_decl_line
	.long	3433                    # DW_AT_type
	.byte	30                      # Abbrev [30] 0xae7:0xf DW_TAG_variable
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.long	.Linfo_string124        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	281                     # DW_AT_decl_line
	.long	3357                    # DW_AT_type
	.byte	30                      # Abbrev [30] 0xaf6:0x10 DW_TAG_variable
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270\177"
	.long	.Linfo_string117        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	282                     # DW_AT_decl_line
	.long	3357                    # DW_AT_type
	.byte	30                      # Abbrev [30] 0xb06:0x10 DW_TAG_variable
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\267\177"
	.long	.Linfo_string125        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	290                     # DW_AT_decl_line
	.long	384                     # DW_AT_type
	.byte	30                      # Abbrev [30] 0xb16:0x10 DW_TAG_variable
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\250\177"
	.long	.Linfo_string128        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	291                     # DW_AT_decl_line
	.long	3357                    # DW_AT_type
	.byte	30                      # Abbrev [30] 0xb26:0x10 DW_TAG_variable
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\247\177"
	.long	.Linfo_string129        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	303                     # DW_AT_decl_line
	.long	3102                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0xb37:0x35 DW_TAG_subprogram
	.quad	.Lfunc_begin38          # DW_AT_low_pc
	.long	.Lfunc_end38-.Lfunc_begin38 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string94         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	346                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	21                      # Abbrev [21] 0xb4d:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string124        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	346                     # DW_AT_decl_line
	.long	3357                    # DW_AT_type
	.byte	30                      # Abbrev [30] 0xb5c:0xf DW_TAG_variable
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string120        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	347                     # DW_AT_decl_line
	.long	3395                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0xb6c:0x26 DW_TAG_subprogram
	.quad	.Lfunc_begin39          # DW_AT_low_pc
	.long	.Lfunc_end39-.Lfunc_begin39 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string95         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	365                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	21                      # Abbrev [21] 0xb82:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string99         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	365                     # DW_AT_decl_line
	.long	436                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0xb92:0x35 DW_TAG_subprogram
	.quad	.Lfunc_begin40          # DW_AT_low_pc
	.long	.Lfunc_end40-.Lfunc_begin40 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string96         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	387                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	21                      # Abbrev [21] 0xba8:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string26         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	387                     # DW_AT_decl_line
	.long	436                     # DW_AT_type
	.byte	21                      # Abbrev [21] 0xbb7:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string38         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	387                     # DW_AT_decl_line
	.long	436                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0xbc7:0x45 DW_TAG_structure_type
	.long	.Linfo_string53         # DW_AT_name
	.byte	56                      # DW_AT_byte_size
	.byte	3                       # DW_AT_decl_file
	.byte	28                      # DW_AT_decl_line
	.byte	11                      # Abbrev [11] 0xbcf:0xc DW_TAG_member
	.long	.Linfo_string54         # DW_AT_name
	.long	436                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	29                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xbdb:0xc DW_TAG_member
	.long	.Linfo_string55         # DW_AT_name
	.long	3084                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	30                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xbe7:0xc DW_TAG_member
	.long	.Linfo_string40         # DW_AT_name
	.long	391                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	31                      # DW_AT_decl_line
	.byte	16                      # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xbf3:0xc DW_TAG_member
	.long	.Linfo_string58         # DW_AT_name
	.long	3102                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	32                      # DW_AT_decl_line
	.byte	40                      # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xbff:0xc DW_TAG_member
	.long	.Linfo_string60         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	33                      # DW_AT_decl_line
	.byte	44                      # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	31                      # Abbrev [31] 0xc0c:0xb DW_TAG_typedef
	.long	3095                    # DW_AT_type
	.long	.Linfo_string57         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	62                      # DW_AT_decl_line
	.byte	3                       # Abbrev [3] 0xc17:0x7 DW_TAG_base_type
	.long	.Linfo_string56         # DW_AT_name
	.byte	7                       # DW_AT_encoding
	.byte	8                       # DW_AT_byte_size
	.byte	3                       # Abbrev [3] 0xc1e:0x7 DW_TAG_base_type
	.long	.Linfo_string59         # DW_AT_name
	.byte	2                       # DW_AT_encoding
	.byte	1                       # DW_AT_byte_size
	.byte	12                      # Abbrev [12] 0xc25:0x15 DW_TAG_structure_type
	.long	.Linfo_string66         # DW_AT_name
	.byte	12                      # DW_AT_byte_size
	.byte	3                       # DW_AT_decl_file
	.byte	24                      # DW_AT_decl_line
	.byte	11                      # Abbrev [11] 0xc2d:0xc DW_TAG_member
	.long	.Linfo_string61         # DW_AT_name
	.long	3130                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	25                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0xc3a:0x2d DW_TAG_structure_type
	.long	.Linfo_string65         # DW_AT_name
	.byte	12                      # DW_AT_byte_size
	.byte	5                       # DW_AT_decl_file
	.byte	69                      # DW_AT_decl_line
	.byte	11                      # Abbrev [11] 0xc42:0xc DW_TAG_member
	.long	.Linfo_string62         # DW_AT_name
	.long	63                      # DW_AT_type
	.byte	5                       # DW_AT_decl_file
	.byte	70                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xc4e:0xc DW_TAG_member
	.long	.Linfo_string63         # DW_AT_name
	.long	63                      # DW_AT_type
	.byte	5                       # DW_AT_decl_file
	.byte	71                      # DW_AT_decl_line
	.byte	4                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xc5a:0xc DW_TAG_member
	.long	.Linfo_string64         # DW_AT_name
	.long	63                      # DW_AT_type
	.byte	5                       # DW_AT_decl_file
	.byte	72                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0xc67:0x5 DW_TAG_pointer_type
	.long	3015                    # DW_AT_type
	.byte	6                       # Abbrev [6] 0xc6c:0x5 DW_TAG_pointer_type
	.long	391                     # DW_AT_type
	.byte	6                       # Abbrev [6] 0xc71:0x5 DW_TAG_pointer_type
	.long	384                     # DW_AT_type
	.byte	12                      # Abbrev [12] 0xc76:0x69 DW_TAG_structure_type
	.long	.Linfo_string116        # DW_AT_name
	.byte	72                      # DW_AT_byte_size
	.byte	5                       # DW_AT_decl_file
	.byte	75                      # DW_AT_decl_line
	.byte	11                      # Abbrev [11] 0xc7e:0xc DW_TAG_member
	.long	.Linfo_string107        # DW_AT_name
	.long	120                     # DW_AT_type
	.byte	5                       # DW_AT_decl_file
	.byte	76                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xc8a:0xc DW_TAG_member
	.long	.Linfo_string108        # DW_AT_name
	.long	3295                    # DW_AT_type
	.byte	5                       # DW_AT_decl_file
	.byte	77                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xc96:0xc DW_TAG_member
	.long	.Linfo_string109        # DW_AT_name
	.long	616                     # DW_AT_type
	.byte	5                       # DW_AT_decl_file
	.byte	78                      # DW_AT_decl_line
	.byte	16                      # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xca2:0xc DW_TAG_member
	.long	.Linfo_string110        # DW_AT_name
	.long	3300                    # DW_AT_type
	.byte	5                       # DW_AT_decl_file
	.byte	79                      # DW_AT_decl_line
	.byte	24                      # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xcae:0xc DW_TAG_member
	.long	.Linfo_string114        # DW_AT_name
	.long	63                      # DW_AT_type
	.byte	5                       # DW_AT_decl_file
	.byte	80                      # DW_AT_decl_line
	.byte	48                      # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xcba:0xc DW_TAG_member
	.long	.Linfo_string115        # DW_AT_name
	.long	63                      # DW_AT_type
	.byte	5                       # DW_AT_decl_file
	.byte	81                      # DW_AT_decl_line
	.byte	52                      # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xcc6:0xc DW_TAG_member
	.long	.Linfo_string58         # DW_AT_name
	.long	3102                    # DW_AT_type
	.byte	5                       # DW_AT_decl_file
	.byte	82                      # DW_AT_decl_line
	.byte	56                      # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xcd2:0xc DW_TAG_member
	.long	.Linfo_string60         # DW_AT_name
	.long	3130                    # DW_AT_type
	.byte	5                       # DW_AT_decl_file
	.byte	83                      # DW_AT_decl_line
	.byte	60                      # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0xcdf:0x5 DW_TAG_pointer_type
	.long	103                     # DW_AT_type
	.byte	12                      # Abbrev [12] 0xce4:0x39 DW_TAG_structure_type
	.long	.Linfo_string110        # DW_AT_name
	.byte	24                      # DW_AT_byte_size
	.byte	5                       # DW_AT_decl_file
	.byte	62                      # DW_AT_decl_line
	.byte	11                      # Abbrev [11] 0xcec:0xc DW_TAG_member
	.long	.Linfo_string111        # DW_AT_name
	.long	3185                    # DW_AT_type
	.byte	5                       # DW_AT_decl_file
	.byte	63                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xcf8:0xc DW_TAG_member
	.long	.Linfo_string112        # DW_AT_name
	.long	63                      # DW_AT_type
	.byte	5                       # DW_AT_decl_file
	.byte	64                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xd04:0xc DW_TAG_member
	.long	.Linfo_string113        # DW_AT_name
	.long	63                      # DW_AT_type
	.byte	5                       # DW_AT_decl_file
	.byte	65                      # DW_AT_decl_line
	.byte	12                      # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xd10:0xc DW_TAG_member
	.long	.Linfo_string62         # DW_AT_name
	.long	63                      # DW_AT_type
	.byte	5                       # DW_AT_decl_file
	.byte	66                      # DW_AT_decl_line
	.byte	16                      # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0xd1d:0x5 DW_TAG_pointer_type
	.long	3362                    # DW_AT_type
	.byte	12                      # Abbrev [12] 0xd22:0x21 DW_TAG_structure_type
	.long	.Linfo_string122        # DW_AT_name
	.byte	16                      # DW_AT_byte_size
	.byte	6                       # DW_AT_decl_file
	.byte	8                       # DW_AT_decl_line
	.byte	11                      # Abbrev [11] 0xd2a:0xc DW_TAG_member
	.long	.Linfo_string118        # DW_AT_name
	.long	3395                    # DW_AT_type
	.byte	6                       # DW_AT_decl_file
	.byte	9                       # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xd36:0xc DW_TAG_member
	.long	.Linfo_string121        # DW_AT_name
	.long	3395                    # DW_AT_type
	.byte	6                       # DW_AT_decl_file
	.byte	10                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0xd43:0x5 DW_TAG_pointer_type
	.long	3400                    # DW_AT_type
	.byte	12                      # Abbrev [12] 0xd48:0x21 DW_TAG_structure_type
	.long	.Linfo_string120        # DW_AT_name
	.byte	16                      # DW_AT_byte_size
	.byte	6                       # DW_AT_decl_file
	.byte	13                      # DW_AT_decl_line
	.byte	11                      # Abbrev [11] 0xd50:0xc DW_TAG_member
	.long	.Linfo_string34         # DW_AT_name
	.long	3395                    # DW_AT_type
	.byte	6                       # DW_AT_decl_file
	.byte	14                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xd5c:0xc DW_TAG_member
	.long	.Linfo_string119        # DW_AT_name
	.long	120                     # DW_AT_type
	.byte	6                       # DW_AT_decl_file
	.byte	15                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0xd69:0x5 DW_TAG_pointer_type
	.long	3102                    # DW_AT_type
	.byte	0                       # End Of Children Mark
.Ldebug_info_end0:
	.section	.debug_macinfo,"",@progbits
	.byte	0                       # End Of Macro List Mark

	.ident	"clang version 8.0.0-3~ubuntu18.04.1 (tags/RELEASE_800/final)"
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym noop_nfa
	.addrsig_sym do_empty_nfa
	.addrsig_sym do_alt_nfa
	.addrsig_sym do_cat_nfa
	.addrsig_sym do_dotall_nfa
	.addrsig_sym do_symbol_nfa
	.addrsig_sym do_star_nfa
	.addrsig_sym do_plus_nfa
	.addrsig_sym do_optional_nfa
	.addrsig_sym nullperr
	.addrsig_sym accepting_state
	.addrsig_sym epsilon_state
	.addrsig_sym dotall_state
	.addrsig_sym branch_state
	.addrsig_sym symbol_state
	.addrsig_sym setst
	.addrsig_sym point
	.addrsig_sym patch
	.addrsig_sym smachine
	.addrsig_sym gmachine
	.addrsig_sym nfa_to_rval
	.addrsig_sym empty_machine
	.addrsig_sym dotall_machine
	.addrsig_sym symbol_machine
	.addrsig_sym alt_machine
	.addrsig_sym cat_machine
	.addrsig_sym posclosure_machine
	.addrsig_sym optional_machine
	.addrsig_sym closure_machine
	.addrsig_sym __assert_fail
	.addrsig_sym has_nfa_error
	.addrsig_sym parse_context
	.addrsig_sym parse_regex
	.addrsig_sym print_parse_error
	.addrsig_sym free
	.addrsig_sym eps_closure
	.addrsig_sym push
	.addrsig_sym move
	.addrsig_sym pop
	.addrsig_sym value
	.addrsig_sym free_node
	.addrsig_sym accepts
	.addrsig_sym head
	.addrsig_sym calloc
	.addrsig_sym init_list
	.addrsig_sym free_list
	.addrsig_sym empty
	.addrsig_sym printf
	.addrsig_sym print_state
	.addrsig_sym uid
	.addrsig_sym nfa_actions
	.section	.debug_line,"",@progbits
.Lline_table_start0:
