	.text
	.file	"parser.c"
	.file	1 "/home/wroathe/compilers/src/auto" "./result_types.h"
	.file	2 "/usr/include" "ctype.h"
	.file	3 "/home/wroathe/compilers/src/auto" "./parser.h"
	.globl	scan_context            # -- Begin function scan_context
	.p2align	4, 0x90
	.type	scan_context,@function
scan_context:                           # @scan_context
.Lfunc_begin0:
	.file	4 "/home/wroathe/compilers/src/auto" "parser.c"
	.loc	4 9 0                   # parser.c:9:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, %rax
	movq	%rsi, -8(%rbp)
.Ltmp0:
	.loc	4 11 18 prologue_end    # parser.c:11:18
	movq	-8(%rbp), %rsi
	.loc	4 10 34                 # parser.c:10:34
	movq	%rsi, (%rdi)
	movl	$1, 8(%rdi)
	movl	$0, 12(%rdi)
	movl	$1, 16(%rdi)
	.loc	4 10 5 is_stmt 0        # parser.c:10:5
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp1:
.Lfunc_end0:
	.size	scan_context, .Lfunc_end0-scan_context
	.cfi_endproc
                                        # -- End function
	.globl	consume                 # -- Begin function consume
	.p2align	4, 0x90
	.type	consume,@function
consume:                                # @consume
.Lfunc_begin1:
	.loc	4 18 0 is_stmt 1        # parser.c:18:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movb	%sil, %al
	movq	%rdi, %rcx
	leaq	16(%rbp), %rdx
	movb	%al, -1(%rbp)
	movq	%rcx, -16(%rbp)         # 8-byte Spill
	movq	%rdx, -24(%rbp)         # 8-byte Spill
	movq	%rdi, -32(%rbp)         # 8-byte Spill
.LBB1_1:                                # =>This Inner Loop Header: Depth=1
	movq	-24(%rbp), %rax         # 8-byte Reload
.Ltmp2:
	.loc	4 19 21 prologue_end    # parser.c:19:21
	movq	(%rax), %rcx
	.loc	4 19 12 is_stmt 0       # parser.c:19:12
	movsbl	(%rcx), %edx
	.loc	4 19 30                 # parser.c:19:30
	movsbl	-1(%rbp), %esi
	.loc	4 19 27                 # parser.c:19:27
	cmpl	%esi, %edx
	.loc	4 19 32                 # parser.c:19:32
	movb	$1, %dil
	movb	%dil, -33(%rbp)         # 1-byte Spill
	je	.LBB1_3
# %bb.2:                                #   in Loop: Header=BB1_1 Depth=1
	.loc	4 19 35                 # parser.c:19:35
	callq	__ctype_b_loc
	movq	(%rax), %rax
	movq	-24(%rbp), %rcx         # 8-byte Reload
	movq	(%rcx), %rdx
	movsbl	(%rdx), %esi
	movslq	%esi, %rdx
	movzwl	(%rax,%rdx,2), %esi
	andl	$1, %esi
	.loc	4 19 32                 # parser.c:19:32
	cmpl	$0, %esi
	setne	%dil
	movb	%dil, -33(%rbp)         # 1-byte Spill
.LBB1_3:                                #   in Loop: Header=BB1_1 Depth=1
	.loc	4 0 32                  # parser.c:0:32
	movb	-33(%rbp), %al          # 1-byte Reload
	.loc	4 19 5                  # parser.c:19:5
	testb	$1, %al
	jne	.LBB1_4
	jmp	.LBB1_5
.LBB1_4:                                #   in Loop: Header=BB1_1 Depth=1
	.loc	4 0 5                   # parser.c:0:5
	movq	-24(%rbp), %rax         # 8-byte Reload
.Ltmp3:
	.loc	4 20 22 is_stmt 1       # parser.c:20:22
	movq	(%rax), %rcx
	addq	$1, %rcx
	movq	%rcx, (%rax)
	.loc	4 21 26                 # parser.c:21:26
	movl	8(%rax), %edx
	addl	$1, %edx
	movl	%edx, 8(%rax)
.Ltmp4:
	.loc	4 19 5                  # parser.c:19:5
	jmp	.LBB1_1
.LBB1_5:
	.loc	4 0 5 is_stmt 0         # parser.c:0:5
	movq	-24(%rbp), %rax         # 8-byte Reload
	.loc	4 24 12 is_stmt 1       # parser.c:24:12
	movq	(%rax), %rcx
	movq	-32(%rbp), %rdx         # 8-byte Reload
	movq	%rcx, (%rdx)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rdx)
	movq	16(%rax), %rcx
	movq	%rcx, 16(%rdx)
	movq	-16(%rbp), %rax         # 8-byte Reload
	.loc	4 24 5 is_stmt 0        # parser.c:24:5
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp5:
.Lfunc_end1:
	.size	consume, .Lfunc_end1-consume
	.cfi_endproc
                                        # -- End function
	.globl	scan                    # -- Begin function scan
	.p2align	4, 0x90
	.type	scan,@function
scan:                                   # @scan
.Lfunc_begin2:
	.loc	4 27 0 is_stmt 1        # parser.c:27:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$144, %rsp
	movq	%rdi, %rax
	leaq	16(%rbp), %rcx
	movq	%rax, -80(%rbp)         # 8-byte Spill
	movq	%rcx, -88(%rbp)         # 8-byte Spill
	movq	%rdi, -96(%rbp)         # 8-byte Spill
.LBB2_1:                                # =>This Inner Loop Header: Depth=1
.Ltmp6:
	.loc	4 28 12 prologue_end    # parser.c:28:12
	callq	__ctype_b_loc
	movq	(%rax), %rax
	movq	-88(%rbp), %rcx         # 8-byte Reload
	movq	(%rcx), %rdx
	movsbl	(%rdx), %esi
	movslq	%esi, %rdx
	movzwl	(%rax,%rdx,2), %esi
	andl	$1, %esi
	.loc	4 28 5 is_stmt 0        # parser.c:28:5
	cmpl	$0, %esi
	je	.LBB2_3
# %bb.2:                                #   in Loop: Header=BB2_1 Depth=1
	.loc	4 0 5                   # parser.c:0:5
	movq	-88(%rbp), %rax         # 8-byte Reload
.Ltmp7:
	.loc	4 29 26 is_stmt 1       # parser.c:29:26
	movl	8(%rax), %ecx
	addl	$1, %ecx
	movl	%ecx, 8(%rax)
	.loc	4 30 22                 # parser.c:30:22
	movq	(%rax), %rdx
	addq	$1, %rdx
	movq	%rdx, (%rax)
.Ltmp8:
	.loc	4 28 5                  # parser.c:28:5
	jmp	.LBB2_1
.LBB2_3:
	.loc	4 0 5 is_stmt 0         # parser.c:0:5
	movq	-88(%rbp), %rax         # 8-byte Reload
	.loc	4 33 33 is_stmt 1       # parser.c:33:33
	movl	8(%rax), %ecx
	.loc	4 33 23 is_stmt 0       # parser.c:33:23
	movl	%ecx, 16(%rax)
.Ltmp9:
	.loc	4 39 18 is_stmt 1       # parser.c:39:18
	movq	(%rax), %rdx
	.loc	4 39 9 is_stmt 0        # parser.c:39:9
	movsbl	(%rdx), %ecx
	.loc	4 39 24                 # parser.c:39:24
	cmpl	$42, %ecx
.Ltmp10:
	.loc	4 39 9                  # parser.c:39:9
	jne	.LBB2_5
# %bb.4:
.Ltmp11:
	.loc	4 40 19 is_stmt 1       # parser.c:40:19
	leaq	-24(%rbp), %rdi
	movq	-88(%rbp), %rax         # 8-byte Reload
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rcx
	movq	%rcx, 16(%rsp)
	movl	$42, %esi
	callq	consume
	movq	-24(%rbp), %rax
	movq	-88(%rbp), %rcx         # 8-byte Reload
	movq	%rax, (%rcx)
	movq	-16(%rbp), %rax
	movq	%rax, 8(%rcx)
	movq	-8(%rbp), %rax
	movq	%rax, 16(%rcx)
	.loc	4 41 23                 # parser.c:41:23
	movl	$-214, 12(%rcx)
	.loc	4 42 5                  # parser.c:42:5
	jmp	.LBB2_20
.Ltmp12:
.LBB2_5:
	.loc	4 0 5 is_stmt 0         # parser.c:0:5
	movq	-88(%rbp), %rax         # 8-byte Reload
.Ltmp13:
	.loc	4 42 25                 # parser.c:42:25
	movq	(%rax), %rcx
	.loc	4 42 16                 # parser.c:42:16
	movsbl	(%rcx), %edx
	.loc	4 42 31                 # parser.c:42:31
	cmpl	$43, %edx
.Ltmp14:
	.loc	4 42 16                 # parser.c:42:16
	jne	.LBB2_7
# %bb.6:
.Ltmp15:
	.loc	4 43 19 is_stmt 1       # parser.c:43:19
	leaq	-48(%rbp), %rdi
	movq	-88(%rbp), %rax         # 8-byte Reload
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rcx
	movq	%rcx, 16(%rsp)
	movl	$43, %esi
	callq	consume
	movq	-48(%rbp), %rax
	movq	-88(%rbp), %rcx         # 8-byte Reload
	movq	%rax, (%rcx)
	movq	-40(%rbp), %rax
	movq	%rax, 8(%rcx)
	movq	-32(%rbp), %rax
	movq	%rax, 16(%rcx)
	.loc	4 44 23                 # parser.c:44:23
	movl	$-213, 12(%rcx)
	.loc	4 45 5                  # parser.c:45:5
	jmp	.LBB2_19
.Ltmp16:
.LBB2_7:
	.loc	4 0 5 is_stmt 0         # parser.c:0:5
	movq	-88(%rbp), %rax         # 8-byte Reload
.Ltmp17:
	.loc	4 45 25                 # parser.c:45:25
	movq	(%rax), %rcx
	.loc	4 45 16                 # parser.c:45:16
	movsbl	(%rcx), %edx
	.loc	4 45 31                 # parser.c:45:31
	cmpl	$63, %edx
.Ltmp18:
	.loc	4 45 16                 # parser.c:45:16
	jne	.LBB2_9
# %bb.8:
.Ltmp19:
	.loc	4 46 19 is_stmt 1       # parser.c:46:19
	leaq	-72(%rbp), %rdi
	movq	-88(%rbp), %rax         # 8-byte Reload
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rcx
	movq	%rcx, 16(%rsp)
	movl	$63, %esi
	callq	consume
	movq	-72(%rbp), %rax
	movq	-88(%rbp), %rcx         # 8-byte Reload
	movq	%rax, (%rcx)
	movq	-64(%rbp), %rax
	movq	%rax, 8(%rcx)
	movq	-56(%rbp), %rax
	movq	%rax, 16(%rcx)
	.loc	4 47 23                 # parser.c:47:23
	movl	$-193, 12(%rcx)
	.loc	4 48 5                  # parser.c:48:5
	jmp	.LBB2_18
.Ltmp20:
.LBB2_9:
	.loc	4 0 5 is_stmt 0         # parser.c:0:5
	movq	-88(%rbp), %rax         # 8-byte Reload
.Ltmp21:
	.loc	4 49 26 is_stmt 1       # parser.c:49:26
	movq	(%rax), %rcx
	.loc	4 49 17 is_stmt 0       # parser.c:49:17
	movsbl	(%rcx), %edx
	.loc	4 49 9                  # parser.c:49:9
	movl	%edx, %esi
	subl	$40, %esi
	movl	%edx, -100(%rbp)        # 4-byte Spill
	movl	%esi, -104(%rbp)        # 4-byte Spill
	je	.LBB2_12
	jmp	.LBB2_21
.LBB2_21:
	.loc	4 0 9                   # parser.c:0:9
	movl	-100(%rbp), %eax        # 4-byte Reload
	.loc	4 49 9                  # parser.c:49:9
	subl	$41, %eax
	movl	%eax, -108(%rbp)        # 4-byte Spill
	je	.LBB2_13
	jmp	.LBB2_22
.LBB2_22:
	.loc	4 0 9                   # parser.c:0:9
	movl	-100(%rbp), %eax        # 4-byte Reload
	.loc	4 49 9                  # parser.c:49:9
	subl	$46, %eax
	movl	%eax, -112(%rbp)        # 4-byte Spill
	je	.LBB2_10
	jmp	.LBB2_23
.LBB2_23:
	.loc	4 0 9                   # parser.c:0:9
	movl	-100(%rbp), %eax        # 4-byte Reload
	.loc	4 49 9                  # parser.c:49:9
	subl	$124, %eax
	movl	%eax, -116(%rbp)        # 4-byte Spill
	je	.LBB2_11
	jmp	.LBB2_14
.LBB2_10:
	.loc	4 0 9                   # parser.c:0:9
	movq	-88(%rbp), %rax         # 8-byte Reload
.Ltmp22:
	.loc	4 51 31 is_stmt 1       # parser.c:51:31
	movl	$-210, 12(%rax)
	.loc	4 52 17                 # parser.c:52:17
	jmp	.LBB2_17
.LBB2_11:
	.loc	4 0 17 is_stmt 0        # parser.c:0:17
	movq	-88(%rbp), %rax         # 8-byte Reload
	.loc	4 54 31 is_stmt 1       # parser.c:54:31
	movl	$-132, 12(%rax)
	.loc	4 55 17                 # parser.c:55:17
	jmp	.LBB2_17
.LBB2_12:
	.loc	4 0 17 is_stmt 0        # parser.c:0:17
	movq	-88(%rbp), %rax         # 8-byte Reload
	.loc	4 57 31 is_stmt 1       # parser.c:57:31
	movl	$-216, 12(%rax)
	.loc	4 58 17                 # parser.c:58:17
	jmp	.LBB2_17
.LBB2_13:
	.loc	4 0 17 is_stmt 0        # parser.c:0:17
	movq	-88(%rbp), %rax         # 8-byte Reload
	.loc	4 60 31 is_stmt 1       # parser.c:60:31
	movl	$-215, 12(%rax)
	.loc	4 61 17                 # parser.c:61:17
	jmp	.LBB2_17
.LBB2_14:
	.loc	4 0 17 is_stmt 0        # parser.c:0:17
	movq	-88(%rbp), %rax         # 8-byte Reload
.Ltmp23:
	.loc	4 63 30 is_stmt 1       # parser.c:63:30
	movq	(%rax), %rcx
	.loc	4 63 21 is_stmt 0       # parser.c:63:21
	movsbl	(%rcx), %edx
	.loc	4 63 36                 # parser.c:63:36
	cmpl	$92, %edx
.Ltmp24:
	.loc	4 63 21                 # parser.c:63:21
	jne	.LBB2_16
# %bb.15:
	.loc	4 0 21                  # parser.c:0:21
	movq	-88(%rbp), %rax         # 8-byte Reload
.Ltmp25:
	.loc	4 64 34 is_stmt 1       # parser.c:64:34
	movq	(%rax), %rcx
	addq	$1, %rcx
	movq	%rcx, (%rax)
	.loc	4 65 38                 # parser.c:65:38
	movl	8(%rax), %edx
	addl	$1, %edx
	movl	%edx, 8(%rax)
.Ltmp26:
.LBB2_16:
	.loc	4 0 38 is_stmt 0        # parser.c:0:38
	movq	-88(%rbp), %rax         # 8-byte Reload
	.loc	4 68 42 is_stmt 1       # parser.c:68:42
	movq	(%rax), %rcx
	.loc	4 68 33 is_stmt 0       # parser.c:68:33
	movsbl	(%rcx), %edx
	.loc	4 68 31                 # parser.c:68:31
	movl	%edx, 12(%rax)
.Ltmp27:
.LBB2_17:
	.loc	4 0 31                  # parser.c:0:31
	movq	-88(%rbp), %rax         # 8-byte Reload
	.loc	4 72 22 is_stmt 1       # parser.c:72:22
	movq	(%rax), %rcx
	addq	$1, %rcx
	movq	%rcx, (%rax)
	.loc	4 73 26                 # parser.c:73:26
	movl	8(%rax), %edx
	addl	$1, %edx
	movl	%edx, 8(%rax)
.Ltmp28:
.LBB2_18:
	.loc	4 0 26 is_stmt 0        # parser.c:0:26
	jmp	.LBB2_19
.LBB2_19:
	jmp	.LBB2_20
.LBB2_20:
	movq	-88(%rbp), %rax         # 8-byte Reload
	.loc	4 76 12 is_stmt 1       # parser.c:76:12
	movq	(%rax), %rcx
	movq	-96(%rbp), %rdx         # 8-byte Reload
	movq	%rcx, (%rdx)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rdx)
	movq	16(%rax), %rcx
	movq	%rcx, 16(%rdx)
	movq	-80(%rbp), %rax         # 8-byte Reload
	.loc	4 76 5 is_stmt 0        # parser.c:76:5
	addq	$144, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp29:
.Lfunc_end2:
	.size	scan, .Lfunc_end2-scan
	.cfi_endproc
                                        # -- End function
	.globl	token                   # -- Begin function token
	.p2align	4, 0x90
	.type	token,@function
token:                                  # @token
.Lfunc_begin3:
	.loc	4 79 0 is_stmt 1        # parser.c:79:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	leaq	16(%rbp), %rax
.Ltmp30:
	.loc	4 80 20 prologue_end    # parser.c:80:20
	movl	12(%rax), %eax
	.loc	4 80 5 is_stmt 0        # parser.c:80:5
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp31:
.Lfunc_end3:
	.size	token, .Lfunc_end3-token
	.cfi_endproc
                                        # -- End function
	.globl	token_col               # -- Begin function token_col
	.p2align	4, 0x90
	.type	token_col,@function
token_col:                              # @token_col
.Lfunc_begin4:
	.loc	4 83 0 is_stmt 1        # parser.c:83:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	leaq	16(%rbp), %rax
.Ltmp32:
	.loc	4 84 20 prologue_end    # parser.c:84:20
	movl	16(%rax), %eax
	.loc	4 84 5 is_stmt 0        # parser.c:84:5
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp33:
.Lfunc_end4:
	.size	token_col, .Lfunc_end4-token_col
	.cfi_endproc
                                        # -- End function
	.globl	getval                  # -- Begin function getval
	.p2align	4, 0x90
	.type	getval,@function
getval:                                 # @getval
.Lfunc_begin5:
	.loc	4 87 0 is_stmt 1        # parser.c:87:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, %rax
	movq	%rsi, -8(%rbp)
.Ltmp34:
	.loc	4 88 14 prologue_end    # parser.c:88:14
	movq	-8(%rbp), %rsi
	.loc	4 88 23 is_stmt 0       # parser.c:88:23
	movq	16(%rsi), %rsi
	.loc	4 88 31                 # parser.c:88:31
	movq	-8(%rbp), %rcx
	.loc	4 88 40                 # parser.c:88:40
	movq	(%rcx), %rcx
	movq	%rsi, -16(%rbp)         # 8-byte Spill
	.loc	4 88 12                 # parser.c:88:12
	movq	%rcx, %rsi
	movq	-16(%rbp), %rcx         # 8-byte Reload
	movq	%rax, -24(%rbp)         # 8-byte Spill
	callq	*%rcx
	movq	-24(%rbp), %rax         # 8-byte Reload
	.loc	4 88 5                  # parser.c:88:5
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp35:
.Lfunc_end5:
	.size	getval, .Lfunc_end5-getval
	.cfi_endproc
                                        # -- End function
	.globl	do_action               # -- Begin function do_action
	.p2align	4, 0x90
	.type	do_action,@function
do_action:                              # @do_action
.Lfunc_begin6:
	.loc	4 91 0 is_stmt 1        # parser.c:91:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	leaq	16(%rbp), %rax
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
.Ltmp36:
	.loc	4 92 7 prologue_end     # parser.c:92:7
	movq	-8(%rbp), %rdi
	.loc	4 92 16 is_stmt 0       # parser.c:92:16
	movq	8(%rdi), %rdi
	.loc	4 92 7                  # parser.c:92:7
	movl	-12(%rbp), %esi
	movl	%esi, %ecx
	movq	(%rdi,%rcx,8), %rcx
	.loc	4 92 33                 # parser.c:92:33
	movq	-8(%rbp), %rdi
	.loc	4 92 42                 # parser.c:92:42
	movq	(%rdi), %rdi
	.loc	4 92 5                  # parser.c:92:5
	movq	(%rax), %rdx
	movq	%rdx, (%rsp)
	movq	8(%rax), %rdx
	movq	%rdx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	*%rcx
	.loc	4 93 1 is_stmt 1        # parser.c:93:1
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp37:
.Lfunc_end6:
	.size	do_action, .Lfunc_end6-do_action
	.cfi_endproc
                                        # -- End function
	.globl	parse_context           # -- Begin function parse_context
	.p2align	4, 0x90
	.type	parse_context,@function
parse_context:                          # @parse_context
.Lfunc_begin7:
	.loc	4 100 0                 # parser.c:100:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$144, %rsp
	movq	%rdi, %rax
.Ltmp38:
	#DEBUG_VALUE: parse_context:context <- [$rdi+0]
	movq	%rsi, -8(%rbp)
	movq	%rdx, -16(%rbp)
	movq	%rcx, -24(%rbp)
	movq	%r8, -32(%rbp)
.Ltmp39:
	.loc	4 101 5 prologue_end    # parser.c:101:5
	cmpq	$0, -8(%rbp)
	movq	%rax, -104(%rbp)        # 8-byte Spill
	movq	%rdi, -112(%rbp)        # 8-byte Spill
.Ltmp40:
	#DEBUG_VALUE: parse_context:context <- [DW_OP_constu 112, DW_OP_minus, DW_OP_deref] [$rbp+0]
	.loc	4 101 5 is_stmt 0       # parser.c:101:5
	je	.LBB7_2
# %bb.1:
	#DEBUG_VALUE: parse_context:context <- [DW_OP_constu 112, DW_OP_minus, DW_OP_deref] [$rbp+0]
	jmp	.LBB7_3
.LBB7_2:
	#DEBUG_VALUE: parse_context:context <- [DW_OP_constu 112, DW_OP_minus, DW_OP_deref] [$rbp+0]
.Ltmp41:
	.loc	4 101 5                 # parser.c:101:5
	movabsq	$.L.str, %rdi
	movabsq	$.L.str.1, %rsi
	movl	$101, %edx
	movabsq	$.L__PRETTY_FUNCTION__.parse_context, %rcx
	callq	__assert_fail
.Ltmp42:
.LBB7_3:
	#DEBUG_VALUE: parse_context:context <- [DW_OP_constu 112, DW_OP_minus, DW_OP_deref] [$rbp+0]
	.loc	4 102 5 is_stmt 1       # parser.c:102:5
	cmpq	$0, -16(%rbp)
.Ltmp43:
	.loc	4 102 5 is_stmt 0       # parser.c:102:5
	je	.LBB7_5
# %bb.4:
	#DEBUG_VALUE: parse_context:context <- [DW_OP_constu 112, DW_OP_minus, DW_OP_deref] [$rbp+0]
	jmp	.LBB7_6
.LBB7_5:
	#DEBUG_VALUE: parse_context:context <- [DW_OP_constu 112, DW_OP_minus, DW_OP_deref] [$rbp+0]
.Ltmp44:
	.loc	4 102 5                 # parser.c:102:5
	movabsq	$.L.str.2, %rdi
	movabsq	$.L.str.1, %rsi
	movl	$102, %edx
	movabsq	$.L__PRETTY_FUNCTION__.parse_context, %rcx
	callq	__assert_fail
.Ltmp45:
.LBB7_6:
	#DEBUG_VALUE: parse_context:context <- [DW_OP_constu 112, DW_OP_minus, DW_OP_deref] [$rbp+0]
	.loc	4 103 5 is_stmt 1       # parser.c:103:5
	cmpq	$0, -32(%rbp)
.Ltmp46:
	.loc	4 103 5 is_stmt 0       # parser.c:103:5
	je	.LBB7_8
# %bb.7:
	#DEBUG_VALUE: parse_context:context <- [DW_OP_constu 112, DW_OP_minus, DW_OP_deref] [$rbp+0]
	jmp	.LBB7_9
.LBB7_8:
	#DEBUG_VALUE: parse_context:context <- [DW_OP_constu 112, DW_OP_minus, DW_OP_deref] [$rbp+0]
.Ltmp47:
	.loc	4 103 5                 # parser.c:103:5
	movabsq	$.L.str.3, %rdi
	movabsq	$.L.str.1, %rsi
	movl	$103, %edx
	movabsq	$.L__PRETTY_FUNCTION__.parse_context, %rcx
	callq	__assert_fail
.Ltmp48:
.LBB7_9:
	#DEBUG_VALUE: parse_context:context <- [DW_OP_constu 112, DW_OP_minus, DW_OP_deref] [$rbp+0]
	.loc	4 104 5 is_stmt 1       # parser.c:104:5
	cmpq	$0, -24(%rbp)
.Ltmp49:
	.loc	4 104 5 is_stmt 0       # parser.c:104:5
	je	.LBB7_11
# %bb.10:
	#DEBUG_VALUE: parse_context:context <- [DW_OP_constu 112, DW_OP_minus, DW_OP_deref] [$rbp+0]
	jmp	.LBB7_12
.LBB7_11:
	#DEBUG_VALUE: parse_context:context <- [DW_OP_constu 112, DW_OP_minus, DW_OP_deref] [$rbp+0]
.Ltmp50:
	.loc	4 104 5                 # parser.c:104:5
	movabsq	$.L.str.4, %rdi
	movabsq	$.L.str.1, %rsi
	movl	$104, %edx
	movabsq	$.L__PRETTY_FUNCTION__.parse_context, %rcx
	callq	__assert_fail
.Ltmp51:
.LBB7_12:
	#DEBUG_VALUE: parse_context:context <- [DW_OP_constu 112, DW_OP_minus, DW_OP_deref] [$rbp+0]
	.loc	4 106 54 is_stmt 1      # parser.c:106:54
	movq	-8(%rbp), %rsi
	leaq	-80(%rbp), %rdi
	.loc	4 106 41 is_stmt 0      # parser.c:106:41
	callq	scan_context
	.loc	4 106 36                # parser.c:106:36
	movq	-64(%rbp), %rsi
	movq	%rsp, %rdi
	movq	%rsi, 16(%rdi)
	movups	-80(%rbp), %xmm0
	movups	%xmm0, (%rdi)
	leaq	-56(%rbp), %rdi
	callq	scan
	.loc	4 110 27 is_stmt 1      # parser.c:110:27
	movq	-16(%rbp), %rsi
	movq	-112(%rbp), %rdi        # 8-byte Reload
	.loc	4 108 36                # parser.c:108:36
	movq	%rsi, (%rdi)
	.loc	4 111 20                # parser.c:111:20
	movq	-32(%rbp), %rsi
	.loc	4 108 36                # parser.c:108:36
	movq	%rsi, 8(%rdi)
	.loc	4 112 19                # parser.c:112:19
	movq	-24(%rbp), %rsi
	.loc	4 108 36                # parser.c:108:36
	movq	%rsi, 16(%rdi)
	.loc	4 109 25                # parser.c:109:25
	movq	-40(%rbp), %rsi
	movq	%rsi, 40(%rdi)
	movups	-56(%rbp), %xmm0
	movups	%xmm0, 24(%rdi)
	.loc	4 113 22                # parser.c:113:22
	movq	-40(%rbp), %rsi
	movq	%rsp, %rax
	movq	%rsi, 16(%rax)
	movups	-56(%rbp), %xmm0
	movups	%xmm0, (%rax)
	callq	token
	movq	-112(%rbp), %rsi        # 8-byte Reload
	.loc	4 108 36                # parser.c:108:36
	movl	%eax, 48(%rsi)
	.loc	4 114 26                # parser.c:114:26
	movq	-40(%rbp), %rdi
	movq	%rsp, %rcx
	movq	%rdi, 16(%rcx)
	movups	-56(%rbp), %xmm0
	movups	%xmm0, (%rcx)
	callq	token_col
	movq	-112(%rbp), %rcx        # 8-byte Reload
	.loc	4 108 36                # parser.c:108:36
	movl	%eax, 52(%rcx)
	movb	$0, 56(%rcx)
	.loc	4 116 18                # parser.c:116:18
	callq	nullperr
	movl	%edx, -88(%rbp)
	movq	%rax, -96(%rbp)
	movq	-96(%rbp), %rax
	movq	-112(%rbp), %rcx        # 8-byte Reload
	movq	%rax, 60(%rcx)
	movl	-88(%rbp), %edx
	movl	%edx, 68(%rcx)
	movq	-104(%rbp), %rax        # 8-byte Reload
	.loc	4 119 5                 # parser.c:119:5
	addq	$144, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp52:
.Lfunc_end7:
	.size	parse_context, .Lfunc_end7-parse_context
	.cfi_endproc
                                        # -- End function
	.globl	peek                    # -- Begin function peek
	.p2align	4, 0x90
	.type	peek,@function
peek:                                   # @peek
.Lfunc_begin8:
	.loc	4 122 0                 # parser.c:122:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	%rdx, -24(%rbp)
.Ltmp53:
	.loc	4 123 13 prologue_end   # parser.c:123:13
	cmpq	$0, -24(%rbp)
	.loc	4 123 16 is_stmt 0      # parser.c:123:16
	je	.LBB8_2
# %bb.1:
	.loc	4 123 21                # parser.c:123:21
	movq	-24(%rbp), %rax
	.loc	4 123 25                # parser.c:123:25
	movq	-8(%rbp), %rcx
	.loc	4 123 34                # parser.c:123:34
	movl	48(%rcx), %edi
	.loc	4 123 19                # parser.c:123:19
	callq	*%rax
	cmpl	$0, %eax
	.loc	4 123 46                # parser.c:123:46
	movb	$1, %dl
	movb	%dl, -25(%rbp)          # 1-byte Spill
	jne	.LBB8_3
.LBB8_2:
	.loc	4 123 49                # parser.c:123:49
	movq	-8(%rbp), %rax
	.loc	4 123 58                # parser.c:123:58
	movl	48(%rax), %ecx
	.loc	4 123 68                # parser.c:123:68
	cmpl	-12(%rbp), %ecx
	sete	%dl
	movb	%dl, -25(%rbp)          # 1-byte Spill
.LBB8_3:
	.loc	4 0 68                  # parser.c:0:68
	movb	-25(%rbp), %al          # 1-byte Reload
	.loc	4 123 5                 # parser.c:123:5
	andb	$1, %al
	movzbl	%al, %eax
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp54:
.Lfunc_end8:
	.size	peek, .Lfunc_end8-peek
	.cfi_endproc
                                        # -- End function
	.globl	expect                  # -- Begin function expect
	.p2align	4, 0x90
	.type	expect,@function
expect:                                 # @expect
.Lfunc_begin9:
	.loc	4 126 0 is_stmt 1       # parser.c:126:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	movq	%rdi, -16(%rbp)
	movl	%esi, -20(%rbp)
	movq	%rdx, -32(%rbp)
.Ltmp55:
	.loc	4 127 14 prologue_end   # parser.c:127:14
	movq	-16(%rbp), %rdi
	.loc	4 127 23 is_stmt 0      # parser.c:127:23
	movl	-20(%rbp), %esi
	.loc	4 127 33                # parser.c:127:33
	movq	-32(%rbp), %rdx
	.loc	4 127 9                 # parser.c:127:9
	callq	peek
.Ltmp56:
	.loc	4 127 9                 # parser.c:127:9
	testb	$1, %al
	jne	.LBB9_1
	jmp	.LBB9_2
.LBB9_1:
.Ltmp57:
	.loc	4 133 49 is_stmt 1      # parser.c:133:49
	movq	-16(%rbp), %rax
	.loc	4 133 58 is_stmt 0      # parser.c:133:58
	addq	$24, %rax
	.loc	4 133 44                # parser.c:133:44
	leaq	-56(%rbp), %rdi
.Ltmp58:
	#DEBUG_VALUE: scan_context <- [$rdi+0]
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	scan
.Ltmp59:
	.loc	4 134 9 is_stmt 1       # parser.c:134:9
	movq	-16(%rbp), %rax
	.loc	4 134 33 is_stmt 0      # parser.c:134:33
	movq	-56(%rbp), %rcx
	movq	%rcx, 24(%rax)
	movq	-48(%rbp), %rcx
	movq	%rcx, 32(%rax)
	movq	-40(%rbp), %rcx
	movq	%rcx, 40(%rax)
	.loc	4 135 30 is_stmt 1      # parser.c:135:30
	leaq	-56(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	token
	.loc	4 135 9 is_stmt 0       # parser.c:135:9
	movq	-16(%rbp), %rcx
	.loc	4 135 28                # parser.c:135:28
	movl	%eax, 48(%rcx)
	.loc	4 136 34 is_stmt 1      # parser.c:136:34
	leaq	-56(%rbp), %rcx
	movq	(%rcx), %rdi
	movq	%rdi, (%rsp)
	movq	8(%rcx), %rdi
	movq	%rdi, 8(%rsp)
	movq	16(%rcx), %rcx
	movq	%rcx, 16(%rsp)
	callq	token_col
	.loc	4 136 9 is_stmt 0       # parser.c:136:9
	movq	-16(%rbp), %rcx
	.loc	4 136 32                # parser.c:136:32
	movl	%eax, 52(%rcx)
	.loc	4 138 9 is_stmt 1       # parser.c:138:9
	movb	$1, -1(%rbp)
	jmp	.LBB9_3
.Ltmp60:
.LBB9_2:
	.loc	4 145 9                 # parser.c:145:9
	movq	-16(%rbp), %rax
	.loc	4 145 28 is_stmt 0      # parser.c:145:28
	movb	$1, 56(%rax)
	.loc	4 146 9 is_stmt 1       # parser.c:146:9
	movq	-16(%rbp), %rax
	.loc	4 148 26                # parser.c:148:26
	movq	-16(%rbp), %rcx
	.loc	4 148 35 is_stmt 0      # parser.c:148:35
	movl	52(%rcx), %edx
	.loc	4 146 47 is_stmt 1      # parser.c:146:47
	movl	%edx, -72(%rbp)
	.loc	4 149 25                # parser.c:149:25
	movl	-20(%rbp), %edx
	.loc	4 146 47                # parser.c:146:47
	movl	%edx, -68(%rbp)
	.loc	4 147 23                # parser.c:147:23
	movq	-16(%rbp), %rcx
	.loc	4 147 32 is_stmt 0      # parser.c:147:32
	movl	48(%rcx), %edx
	.loc	4 146 47 is_stmt 1      # parser.c:146:47
	movl	%edx, -64(%rbp)
	.loc	4 146 26 is_stmt 0      # parser.c:146:26
	movq	-72(%rbp), %rcx
	movq	%rcx, 60(%rax)
	movl	-64(%rbp), %edx
	movl	%edx, 68(%rax)
	.loc	4 152 9 is_stmt 1       # parser.c:152:9
	movb	$0, -1(%rbp)
.Ltmp61:
.LBB9_3:
	.loc	4 154 1                 # parser.c:154:1
	movb	-1(%rbp), %al
	andb	$1, %al
	movzbl	%al, %eax
	addq	$96, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp62:
.Lfunc_end9:
	.size	expect, .Lfunc_end9-expect
	.cfi_endproc
                                        # -- End function
	.globl	parse_regex             # -- Begin function parse_regex
	.p2align	4, 0x90
	.type	parse_regex,@function
parse_regex:                            # @parse_regex
.Lfunc_begin10:
	.loc	4 156 0                 # parser.c:156:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -16(%rbp)
.Ltmp63:
	.loc	4 157 20 prologue_end   # parser.c:157:20
	movq	-16(%rbp), %rdi
	.loc	4 157 9 is_stmt 0       # parser.c:157:9
	callq	parse_expr
	.loc	4 157 29                # parser.c:157:29
	testb	$1, %al
	jne	.LBB10_1
	jmp	.LBB10_3
.LBB10_1:
	.loc	4 0 29                  # parser.c:0:29
	xorl	%esi, %esi
	xorl	%eax, %eax
	movl	%eax, %edx
	.loc	4 157 39                # parser.c:157:39
	movq	-16(%rbp), %rdi
	.loc	4 157 32                # parser.c:157:32
	callq	expect
.Ltmp64:
	.loc	4 157 9                 # parser.c:157:9
	testb	$1, %al
	jne	.LBB10_2
	jmp	.LBB10_3
.LBB10_2:
	.loc	4 0 9                   # parser.c:0:9
	xorl	%esi, %esi
	movabsq	$NULLRVAL, %rax
.Ltmp65:
	.loc	4 158 19 is_stmt 1      # parser.c:158:19
	movq	-16(%rbp), %rdi
	.loc	4 158 9 is_stmt 0       # parser.c:158:9
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	do_action
	.loc	4 159 9 is_stmt 1       # parser.c:159:9
	movb	$1, -1(%rbp)
	jmp	.LBB10_4
.Ltmp66:
.LBB10_3:
	.loc	4 162 5                 # parser.c:162:5
	movb	$0, -1(%rbp)
.LBB10_4:
	.loc	4 163 1                 # parser.c:163:1
	movb	-1(%rbp), %al
	andb	$1, %al
	movzbl	%al, %eax
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp67:
.Lfunc_end10:
	.size	parse_regex, .Lfunc_end10-parse_regex
	.cfi_endproc
                                        # -- End function
	.globl	parse_expr              # -- Begin function parse_expr
	.p2align	4, 0x90
	.type	parse_expr,@function
parse_expr:                             # @parse_expr
.Lfunc_begin11:
	.loc	4 165 0                 # parser.c:165:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$80, %rsp
	movq	%rdi, -8(%rbp)
.Ltmp68:
	.loc	4 166 15 prologue_end   # parser.c:166:15
	movq	-8(%rbp), %rdi
	.loc	4 166 31 is_stmt 0      # parser.c:166:31
	movq	-8(%rbp), %rsi
	.loc	4 166 24                # parser.c:166:24
	leaq	-32(%rbp), %rax
	movq	%rdi, -40(%rbp)         # 8-byte Spill
	movq	%rax, %rdi
	callq	getval
	movq	-40(%rbp), %rdi         # 8-byte Reload
	.loc	4 166 5                 # parser.c:166:5
	leaq	-32(%rbp), %rax
	movq	(%rax), %rsi
	movq	%rsi, (%rsp)
	movq	8(%rax), %rsi
	movq	%rsi, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	parse_alt
	.loc	4 167 5 is_stmt 1       # parser.c:167:5
	movb	$1, %cl
	andb	$1, %cl
	movzbl	%cl, %edx
	movb	%al, -41(%rbp)          # 1-byte Spill
	movl	%edx, %eax
	addq	$80, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp69:
.Lfunc_end11:
	.size	parse_expr, .Lfunc_end11-parse_expr
	.cfi_endproc
                                        # -- End function
	.globl	parse_alt               # -- Begin function parse_alt
	.p2align	4, 0x90
	.type	parse_alt,@function
parse_alt:                              # @parse_alt
.Lfunc_begin12:
	.loc	4 170 0                 # parser.c:170:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$80, %rsp
	leaq	16(%rbp), %rax
	movq	%rdi, -16(%rbp)
.Ltmp70:
	.loc	4 171 19 prologue_end   # parser.c:171:19
	movq	-16(%rbp), %rdi
	.loc	4 171 9 is_stmt 0       # parser.c:171:9
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rcx
	movq	%rcx, 16(%rsp)
	movq	%rax, -48(%rbp)         # 8-byte Spill
	callq	parse_cat
.Ltmp71:
	.loc	4 171 9                 # parser.c:171:9
	testb	$1, %al
	jne	.LBB12_1
	jmp	.LBB12_8
.LBB12_1:
.Ltmp72:
	.loc	4 172 9 is_stmt 1       # parser.c:172:9
	jmp	.LBB12_2
.LBB12_2:                               # =>This Inner Loop Header: Depth=1
.Ltmp73:
	.loc	4 173 27                # parser.c:173:27
	movq	-16(%rbp), %rsi
	.loc	4 173 20 is_stmt 0      # parser.c:173:20
	leaq	-40(%rbp), %rdi
	callq	getval
	xorl	%eax, %eax
	movl	%eax, %edx
	movq	-40(%rbp), %rsi
	movq	-48(%rbp), %rdi         # 8-byte Reload
	movq	%rsi, (%rdi)
	movq	-32(%rbp), %rsi
	movq	%rsi, 8(%rdi)
	movq	-24(%rbp), %rsi
	movq	%rsi, 16(%rdi)
.Ltmp74:
	.loc	4 175 22 is_stmt 1      # parser.c:175:22
	movq	-16(%rbp), %rdi
	.loc	4 175 17 is_stmt 0      # parser.c:175:17
	movl	$4294967164, %esi       # imm = 0xFFFFFF7C
	callq	peek
	.loc	4 175 42                # parser.c:175:42
	testb	$1, %al
	jne	.LBB12_3
	jmp	.LBB12_6
.LBB12_3:                               #   in Loop: Header=BB12_2 Depth=1
	.loc	4 0 42                  # parser.c:0:42
	xorl	%eax, %eax
	movl	%eax, %edx
	.loc	4 176 24 is_stmt 1      # parser.c:176:24
	movq	-16(%rbp), %rdi
	.loc	4 176 17 is_stmt 0      # parser.c:176:17
	movl	$4294967164, %esi       # imm = 0xFFFFFF7C
	callq	expect
	.loc	4 176 44                # parser.c:176:44
	testb	$1, %al
	jne	.LBB12_4
	jmp	.LBB12_6
.LBB12_4:                               #   in Loop: Header=BB12_2 Depth=1
	.loc	4 177 28 is_stmt 1      # parser.c:177:28
	movq	-16(%rbp), %rdi
	.loc	4 177 17 is_stmt 0      # parser.c:177:17
	callq	parse_expr
.Ltmp75:
	.loc	4 175 17 is_stmt 1      # parser.c:175:17
	testb	$1, %al
	jne	.LBB12_5
	jmp	.LBB12_6
.LBB12_5:                               #   in Loop: Header=BB12_2 Depth=1
.Ltmp76:
	.loc	4 178 27                # parser.c:178:27
	movq	-16(%rbp), %rdi
	.loc	4 178 17 is_stmt 0      # parser.c:178:17
	movl	$2, %esi
	movq	-48(%rbp), %rax         # 8-byte Reload
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rcx
	movq	%rcx, 16(%rsp)
	callq	do_action
	.loc	4 179 17 is_stmt 1      # parser.c:179:17
	jmp	.LBB12_2
.Ltmp77:
.LBB12_6:
	.loc	4 182 13                # parser.c:182:13
	jmp	.LBB12_7
.Ltmp78:
.LBB12_7:
	.loc	4 185 9                 # parser.c:185:9
	movb	$1, -1(%rbp)
	jmp	.LBB12_9
.Ltmp79:
.LBB12_8:
	.loc	4 188 5                 # parser.c:188:5
	movb	$0, -1(%rbp)
.LBB12_9:
	.loc	4 189 1                 # parser.c:189:1
	movb	-1(%rbp), %al
	andb	$1, %al
	movzbl	%al, %eax
	addq	$80, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp80:
.Lfunc_end12:
	.size	parse_alt, .Lfunc_end12-parse_alt
	.cfi_endproc
                                        # -- End function
	.globl	parse_cat               # -- Begin function parse_cat
	.p2align	4, 0x90
	.type	parse_cat,@function
parse_cat:                              # @parse_cat
.Lfunc_begin13:
	.loc	4 191 0                 # parser.c:191:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	leaq	16(%rbp), %rax
	movabsq	$NULLRVAL, %rcx
	movq	%rdi, -16(%rbp)
.Ltmp81:
	.loc	4 192 15 prologue_end   # parser.c:192:15
	movq	-16(%rbp), %rdi
	.loc	4 192 5 is_stmt 0       # parser.c:192:5
	movl	$1, %esi
	movq	(%rcx), %rdx
	movq	%rdx, (%rsp)
	movq	8(%rcx), %rdx
	movq	%rdx, 8(%rsp)
	movq	16(%rcx), %rcx
	movq	%rcx, 16(%rsp)
	movq	%rax, -72(%rbp)         # 8-byte Spill
	callq	do_action
	.loc	4 193 31 is_stmt 1      # parser.c:193:31
	movq	-16(%rbp), %rsi
	.loc	4 193 24 is_stmt 0      # parser.c:193:24
	leaq	-40(%rbp), %rdi
.Ltmp82:
	#DEBUG_VALUE: parse_cat:empty <- [$rdi+0]
	callq	getval
.Ltmp83:
	.loc	4 195 22 is_stmt 1      # parser.c:195:22
	movq	-16(%rbp), %rdi
	.loc	4 195 9 is_stmt 0       # parser.c:195:9
	callq	parse_factor
.Ltmp84:
	.loc	4 195 9                 # parser.c:195:9
	testb	$1, %al
	jne	.LBB13_1
	jmp	.LBB13_6
.LBB13_1:
.Ltmp85:
	.loc	4 196 9 is_stmt 1       # parser.c:196:9
	jmp	.LBB13_2
.LBB13_2:                               # =>This Inner Loop Header: Depth=1
.Ltmp86:
	.loc	4 197 27                # parser.c:197:27
	movq	-16(%rbp), %rsi
	.loc	4 197 20 is_stmt 0      # parser.c:197:20
	leaq	-64(%rbp), %rdi
	callq	getval
	movq	-64(%rbp), %rsi
	movq	-72(%rbp), %rdi         # 8-byte Reload
	movq	%rsi, (%rdi)
	movq	-56(%rbp), %rsi
	movq	%rsi, 8(%rdi)
	movq	-48(%rbp), %rsi
	movq	%rsi, 16(%rdi)
.Ltmp87:
	.loc	4 199 30 is_stmt 1      # parser.c:199:30
	movq	-16(%rbp), %rdi
	.loc	4 199 17 is_stmt 0      # parser.c:199:17
	callq	parse_factor
.Ltmp88:
	.loc	4 199 17                # parser.c:199:17
	testb	$1, %al
	jne	.LBB13_3
	jmp	.LBB13_4
.LBB13_3:                               #   in Loop: Header=BB13_2 Depth=1
.Ltmp89:
	.loc	4 200 27 is_stmt 1      # parser.c:200:27
	movq	-16(%rbp), %rdi
	.loc	4 200 17 is_stmt 0      # parser.c:200:17
	movl	$3, %esi
	movq	-72(%rbp), %rax         # 8-byte Reload
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rcx
	movq	%rcx, 16(%rsp)
	callq	do_action
	.loc	4 201 17 is_stmt 1      # parser.c:201:17
	jmp	.LBB13_2
.Ltmp90:
.LBB13_4:
	.loc	4 204 13                # parser.c:204:13
	jmp	.LBB13_5
.Ltmp91:
.LBB13_5:
	.loc	4 207 19                # parser.c:207:19
	movq	-16(%rbp), %rdi
	.loc	4 207 9 is_stmt 0       # parser.c:207:9
	movl	$3, %esi
	leaq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	do_action
	.loc	4 209 9 is_stmt 1       # parser.c:209:9
	movb	$1, -1(%rbp)
	jmp	.LBB13_7
.Ltmp92:
.LBB13_6:
	.loc	4 212 5                 # parser.c:212:5
	movb	$0, -1(%rbp)
.LBB13_7:
	.loc	4 213 1                 # parser.c:213:1
	movb	-1(%rbp), %al
	andb	$1, %al
	movzbl	%al, %eax
	addq	$96, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp93:
.Lfunc_end13:
	.size	parse_cat, .Lfunc_end13-parse_cat
	.cfi_endproc
                                        # -- End function
	.globl	parse_factor            # -- Begin function parse_factor
	.p2align	4, 0x90
	.type	parse_factor,@function
parse_factor:                           # @parse_factor
.Lfunc_begin14:
	.loc	4 215 0                 # parser.c:215:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$80, %rsp
	xorl	%eax, %eax
	movl	%eax, %edx
	movq	%rdi, -16(%rbp)
.Ltmp94:
	.loc	4 216 10 prologue_end   # parser.c:216:10
	movb	$0, -17(%rbp)
.Ltmp95:
	.loc	4 218 14                # parser.c:218:14
	movq	-16(%rbp), %rdi
	.loc	4 218 9 is_stmt 0       # parser.c:218:9
	movl	$4294967080, %esi       # imm = 0xFFFFFF28
	callq	peek
	.loc	4 218 37                # parser.c:218:37
	testb	$1, %al
	jne	.LBB14_1
	jmp	.LBB14_5
.LBB14_1:
	.loc	4 0 37                  # parser.c:0:37
	xorl	%eax, %eax
	movl	%eax, %edx
	.loc	4 219 16 is_stmt 1      # parser.c:219:16
	movq	-16(%rbp), %rdi
	.loc	4 219 9 is_stmt 0       # parser.c:219:9
	movl	$4294967080, %esi       # imm = 0xFFFFFF28
	callq	expect
	.loc	4 219 39                # parser.c:219:39
	testb	$1, %al
	jne	.LBB14_2
	jmp	.LBB14_5
.LBB14_2:
	.loc	4 220 20 is_stmt 1      # parser.c:220:20
	movq	-16(%rbp), %rdi
	.loc	4 220 9 is_stmt 0       # parser.c:220:9
	callq	parse_expr
	.loc	4 220 29                # parser.c:220:29
	testb	$1, %al
	jne	.LBB14_3
	jmp	.LBB14_5
.LBB14_3:
	.loc	4 0 29                  # parser.c:0:29
	xorl	%eax, %eax
	movl	%eax, %edx
	.loc	4 220 39                # parser.c:220:39
	movq	-16(%rbp), %rdi
	.loc	4 220 32                # parser.c:220:32
	movl	$4294967081, %esi       # imm = 0xFFFFFF29
	callq	expect
.Ltmp96:
	.loc	4 218 9 is_stmt 1       # parser.c:218:9
	testb	$1, %al
	jne	.LBB14_4
	jmp	.LBB14_5
.LBB14_4:
	.loc	4 0 9 is_stmt 0         # parser.c:0:9
	movabsq	$NULLRVAL, %rax
.Ltmp97:
	.loc	4 221 19 is_stmt 1      # parser.c:221:19
	movq	-16(%rbp), %rdi
	.loc	4 221 9 is_stmt 0       # parser.c:221:9
	movl	$4, %esi
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	do_action
	.loc	4 222 18 is_stmt 1      # parser.c:222:18
	movb	$1, -17(%rbp)
	.loc	4 223 5                 # parser.c:223:5
	jmp	.LBB14_11
.Ltmp98:
.LBB14_5:
	.loc	4 0 5 is_stmt 0         # parser.c:0:5
	xorl	%eax, %eax
	movl	%eax, %edx
.Ltmp99:
	.loc	4 223 21                # parser.c:223:21
	movq	-16(%rbp), %rdi
	.loc	4 223 16                # parser.c:223:16
	movl	$4294967086, %esi       # imm = 0xFFFFFF2E
	callq	peek
.Ltmp100:
	.loc	4 223 16                # parser.c:223:16
	testb	$1, %al
	jne	.LBB14_6
	jmp	.LBB14_7
.LBB14_6:
	.loc	4 0 16                  # parser.c:0:16
	xorl	%eax, %eax
	movl	%eax, %edx
.Ltmp101:
	.loc	4 224 16 is_stmt 1      # parser.c:224:16
	movq	-16(%rbp), %rdi
	.loc	4 224 9 is_stmt 0       # parser.c:224:9
	movl	$4294967086, %esi       # imm = 0xFFFFFF2E
	callq	expect
	movabsq	$NULLRVAL, %rdx
	.loc	4 225 19 is_stmt 1      # parser.c:225:19
	movq	-16(%rbp), %rdi
	.loc	4 225 9 is_stmt 0       # parser.c:225:9
	movl	$5, %esi
	movq	(%rdx), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rdx), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rdx), %rcx
	movq	%rcx, 16(%rsp)
	movb	%al, -49(%rbp)          # 1-byte Spill
	callq	do_action
	.loc	4 226 18 is_stmt 1      # parser.c:226:18
	movb	$1, -17(%rbp)
	.loc	4 227 5                 # parser.c:227:5
	jmp	.LBB14_10
.Ltmp102:
.LBB14_7:
	.loc	4 227 21 is_stmt 0      # parser.c:227:21
	movq	-16(%rbp), %rdi
	.loc	4 227 16                # parser.c:227:16
	movl	$4294967295, %esi       # imm = 0xFFFFFFFF
	movabsq	$is_symbol, %rdx
	callq	peek
.Ltmp103:
	.loc	4 227 16                # parser.c:227:16
	testb	$1, %al
	jne	.LBB14_8
	jmp	.LBB14_9
.LBB14_8:
	.loc	4 0 16                  # parser.c:0:16
	xorl	%esi, %esi
.Ltmp104:
	.loc	4 228 20 is_stmt 1      # parser.c:228:20
	leaq	-48(%rbp), %rax
.Ltmp105:
	#DEBUG_VALUE: sym <- [$rax+0]
	movq	%rax, %rdi
	movl	$24, %edx
	callq	memset
.Ltmp106:
	.loc	4 228 45 is_stmt 0      # parser.c:228:45
	movq	-16(%rbp), %rdi
	.loc	4 228 35                # parser.c:228:35
	callq	lookahead
	movb	%al, %cl
	.loc	4 228 26                # parser.c:228:26
	movb	%cl, -48(%rbp)
	.loc	4 229 16 is_stmt 1      # parser.c:229:16
	movq	-16(%rbp), %rdi
	.loc	4 229 9 is_stmt 0       # parser.c:229:9
	movl	$4294967295, %esi       # imm = 0xFFFFFFFF
	movabsq	$is_symbol, %rdx
	callq	expect
	.loc	4 230 19 is_stmt 1      # parser.c:230:19
	movq	-16(%rbp), %rdi
	.loc	4 230 9 is_stmt 0       # parser.c:230:9
	movl	$6, %esi
	leaq	-48(%rbp), %rdx
	movq	(%rdx), %r8
	movq	%r8, (%rsp)
	movq	8(%rdx), %r8
	movq	%r8, 8(%rsp)
	movq	16(%rdx), %rdx
	movq	%rdx, 16(%rsp)
	movb	%al, -50(%rbp)          # 1-byte Spill
	callq	do_action
	.loc	4 231 18 is_stmt 1      # parser.c:231:18
	movb	$1, -17(%rbp)
.Ltmp107:
.LBB14_9:
	.loc	4 0 18 is_stmt 0        # parser.c:0:18
	jmp	.LBB14_10
.LBB14_10:
	jmp	.LBB14_11
.LBB14_11:
	.loc	4 234 9 is_stmt 1       # parser.c:234:9
	testb	$1, -17(%rbp)
	je	.LBB14_26
# %bb.12:
.Ltmp108:
	.loc	4 235 9                 # parser.c:235:9
	jmp	.LBB14_13
.LBB14_13:                              # =>This Inner Loop Header: Depth=1
	.loc	4 0 9 is_stmt 0         # parser.c:0:9
	xorl	%eax, %eax
	movl	%eax, %edx
.Ltmp109:
	.loc	4 236 22 is_stmt 1      # parser.c:236:22
	movq	-16(%rbp), %rdi
	.loc	4 236 17 is_stmt 0      # parser.c:236:17
	movl	$4294967082, %esi       # imm = 0xFFFFFF2A
	callq	peek
	.loc	4 236 43                # parser.c:236:43
	testb	$1, %al
	jne	.LBB14_14
	jmp	.LBB14_16
.LBB14_14:                              #   in Loop: Header=BB14_13 Depth=1
	.loc	4 0 43                  # parser.c:0:43
	xorl	%eax, %eax
	movl	%eax, %edx
	.loc	4 236 53                # parser.c:236:53
	movq	-16(%rbp), %rdi
	.loc	4 236 46                # parser.c:236:46
	movl	$4294967082, %esi       # imm = 0xFFFFFF2A
	callq	expect
.Ltmp110:
	.loc	4 236 17                # parser.c:236:17
	testb	$1, %al
	jne	.LBB14_15
	jmp	.LBB14_16
.LBB14_15:                              #   in Loop: Header=BB14_13 Depth=1
	.loc	4 0 17                  # parser.c:0:17
	movabsq	$NULLRVAL, %rax
.Ltmp111:
	.loc	4 237 27 is_stmt 1      # parser.c:237:27
	movq	-16(%rbp), %rdi
	.loc	4 237 17 is_stmt 0      # parser.c:237:17
	movl	$7, %esi
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	do_action
	.loc	4 238 17 is_stmt 1      # parser.c:238:17
	jmp	.LBB14_13
.Ltmp112:
.LBB14_16:                              #   in Loop: Header=BB14_13 Depth=1
	.loc	4 0 17 is_stmt 0        # parser.c:0:17
	xorl	%eax, %eax
	movl	%eax, %edx
.Ltmp113:
	.loc	4 239 29 is_stmt 1      # parser.c:239:29
	movq	-16(%rbp), %rdi
	.loc	4 239 24 is_stmt 0      # parser.c:239:24
	movl	$4294967083, %esi       # imm = 0xFFFFFF2B
	callq	peek
	.loc	4 239 50                # parser.c:239:50
	testb	$1, %al
	jne	.LBB14_17
	jmp	.LBB14_19
.LBB14_17:                              #   in Loop: Header=BB14_13 Depth=1
	.loc	4 0 50                  # parser.c:0:50
	xorl	%eax, %eax
	movl	%eax, %edx
	.loc	4 239 60                # parser.c:239:60
	movq	-16(%rbp), %rdi
	.loc	4 239 53                # parser.c:239:53
	movl	$4294967083, %esi       # imm = 0xFFFFFF2B
	callq	expect
.Ltmp114:
	.loc	4 239 24                # parser.c:239:24
	testb	$1, %al
	jne	.LBB14_18
	jmp	.LBB14_19
.LBB14_18:                              #   in Loop: Header=BB14_13 Depth=1
	.loc	4 0 24                  # parser.c:0:24
	movabsq	$NULLRVAL, %rax
.Ltmp115:
	.loc	4 240 27 is_stmt 1      # parser.c:240:27
	movq	-16(%rbp), %rdi
	.loc	4 240 17 is_stmt 0      # parser.c:240:17
	movl	$8, %esi
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	do_action
	.loc	4 241 17 is_stmt 1      # parser.c:241:17
	jmp	.LBB14_13
.Ltmp116:
.LBB14_19:                              #   in Loop: Header=BB14_13 Depth=1
	.loc	4 0 17 is_stmt 0        # parser.c:0:17
	xorl	%eax, %eax
	movl	%eax, %edx
.Ltmp117:
	.loc	4 242 29 is_stmt 1      # parser.c:242:29
	movq	-16(%rbp), %rdi
	.loc	4 242 24 is_stmt 0      # parser.c:242:24
	movl	$4294967103, %esi       # imm = 0xFFFFFF3F
	callq	peek
	.loc	4 242 54                # parser.c:242:54
	testb	$1, %al
	jne	.LBB14_20
	jmp	.LBB14_22
.LBB14_20:                              #   in Loop: Header=BB14_13 Depth=1
	.loc	4 0 54                  # parser.c:0:54
	xorl	%eax, %eax
	movl	%eax, %edx
	.loc	4 242 64                # parser.c:242:64
	movq	-16(%rbp), %rdi
	.loc	4 242 57                # parser.c:242:57
	movl	$4294967103, %esi       # imm = 0xFFFFFF3F
	callq	expect
.Ltmp118:
	.loc	4 242 24                # parser.c:242:24
	testb	$1, %al
	jne	.LBB14_21
	jmp	.LBB14_22
.LBB14_21:                              #   in Loop: Header=BB14_13 Depth=1
	.loc	4 0 24                  # parser.c:0:24
	movabsq	$NULLRVAL, %rax
.Ltmp119:
	.loc	4 243 27 is_stmt 1      # parser.c:243:27
	movq	-16(%rbp), %rdi
	.loc	4 243 17 is_stmt 0      # parser.c:243:17
	movl	$9, %esi
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	do_action
	.loc	4 244 17 is_stmt 1      # parser.c:244:17
	jmp	.LBB14_13
.Ltmp120:
.LBB14_22:
	.loc	4 0 17 is_stmt 0        # parser.c:0:17
	jmp	.LBB14_23
.LBB14_23:
	jmp	.LBB14_24
.LBB14_24:
	.loc	4 247 13 is_stmt 1      # parser.c:247:13
	jmp	.LBB14_25
.Ltmp121:
.LBB14_25:
	.loc	4 250 9                 # parser.c:250:9
	movb	$1, -1(%rbp)
	jmp	.LBB14_27
.Ltmp122:
.LBB14_26:
	.loc	4 253 5                 # parser.c:253:5
	movb	$0, -1(%rbp)
.LBB14_27:
	.loc	4 254 1                 # parser.c:254:1
	movb	-1(%rbp), %al
	andb	$1, %al
	movzbl	%al, %eax
	addq	$80, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp123:
.Lfunc_end14:
	.size	parse_factor, .Lfunc_end14-parse_factor
	.cfi_endproc
                                        # -- End function
	.globl	is_symbol               # -- Begin function is_symbol
	.p2align	4, 0x90
	.type	is_symbol,@function
is_symbol:                              # @is_symbol
.Lfunc_begin15:
	.loc	4 256 0                 # parser.c:256:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
.Ltmp124:
	.loc	4 257 12 prologue_end   # parser.c:257:12
	callq	__ctype_b_loc
	movq	(%rax), %rax
	movslq	-4(%rbp), %rcx
	movzwl	(%rax,%rcx,2), %edi
	andl	$16384, %edi            # imm = 0x4000
	.loc	4 257 5 is_stmt 0       # parser.c:257:5
	movl	%edi, %eax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp125:
.Lfunc_end15:
	.size	is_symbol, .Lfunc_end15-is_symbol
	.cfi_endproc
                                        # -- End function
	.globl	lookahead               # -- Begin function lookahead
	.p2align	4, 0x90
	.type	lookahead,@function
lookahead:                              # @lookahead
.Lfunc_begin16:
	.loc	4 260 0 is_stmt 1       # parser.c:260:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
.Ltmp126:
	.loc	4 261 12 prologue_end   # parser.c:261:12
	movq	-8(%rbp), %rdi
	.loc	4 261 21 is_stmt 0      # parser.c:261:21
	movl	48(%rdi), %eax
	.loc	4 261 5                 # parser.c:261:5
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp127:
.Lfunc_end16:
	.size	lookahead, .Lfunc_end16-lookahead
	.cfi_endproc
                                        # -- End function
	.globl	has_parse_error         # -- Begin function has_parse_error
	.p2align	4, 0x90
	.type	has_parse_error,@function
has_parse_error:                        # @has_parse_error
.Lfunc_begin17:
	.loc	4 264 0 is_stmt 1       # parser.c:264:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
.Ltmp128:
	.loc	4 265 12 prologue_end   # parser.c:265:12
	movq	-8(%rbp), %rdi
	.loc	4 265 21 is_stmt 0      # parser.c:265:21
	movb	56(%rdi), %al
	.loc	4 265 5                 # parser.c:265:5
	andb	$1, %al
	movzbl	%al, %eax
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp129:
.Lfunc_end17:
	.size	has_parse_error, .Lfunc_end17-has_parse_error
	.cfi_endproc
                                        # -- End function
	.globl	nullperr                # -- Begin function nullperr
	.p2align	4, 0x90
	.type	nullperr,@function
nullperr:                               # @nullperr
.Lfunc_begin18:
	.loc	4 268 0 is_stmt 1       # parser.c:268:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
.Ltmp130:
	.loc	4 269 33 prologue_end   # parser.c:269:33
	movl	$0, -16(%rbp)
	movl	$0, -12(%rbp)
	movl	$0, -8(%rbp)
	.loc	4 269 5 is_stmt 0       # parser.c:269:5
	movl	-8(%rbp), %eax
	movl	%eax, -24(%rbp)
	movq	-16(%rbp), %rcx
	movq	%rcx, -32(%rbp)
	movq	-32(%rbp), %rax
	movl	-24(%rbp), %edx
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp131:
.Lfunc_end18:
	.size	nullperr, .Lfunc_end18-nullperr
	.cfi_endproc
                                        # -- End function
	.globl	parse_error             # -- Begin function parse_error
	.p2align	4, 0x90
	.type	parse_error,@function
parse_error:                            # @parse_error
.Lfunc_begin19:
	.loc	4 272 0 is_stmt 1       # parser.c:272:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, -24(%rbp)
.Ltmp132:
	.loc	4 273 12 prologue_end   # parser.c:273:12
	movq	-24(%rbp), %rdi
	.loc	4 273 21 is_stmt 0      # parser.c:273:21
	movl	68(%rdi), %eax
	movl	%eax, -8(%rbp)
	movq	60(%rdi), %rdi
	movq	%rdi, -16(%rbp)
	.loc	4 273 5                 # parser.c:273:5
	movl	-8(%rbp), %eax
	movl	%eax, -32(%rbp)
	movq	-16(%rbp), %rdi
	movq	%rdi, -40(%rbp)
	movq	-40(%rbp), %rax
	movl	-32(%rbp), %edx
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp133:
.Lfunc_end19:
	.size	parse_error, .Lfunc_end19-parse_error
	.cfi_endproc
                                        # -- End function
	.globl	lexeme_for              # -- Begin function lexeme_for
	.p2align	4, 0x90
	.type	lexeme_for,@function
lexeme_for:                             # @lexeme_for
.Lfunc_begin20:
	.loc	4 276 0 is_stmt 1       # parser.c:276:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
.Ltmp134:
	.loc	4 277 13 prologue_end   # parser.c:277:13
	movl	-12(%rbp), %esi
	.loc	4 277 5 is_stmt 0       # parser.c:277:5
	movl	%esi, %eax
	subl	$-216, %eax
	movl	%esi, -16(%rbp)         # 4-byte Spill
	movl	%eax, -20(%rbp)         # 4-byte Spill
	je	.LBB20_6
	jmp	.LBB20_12
.LBB20_12:
	.loc	4 0 5                   # parser.c:0:5
	movl	-16(%rbp), %eax         # 4-byte Reload
	.loc	4 277 5                 # parser.c:277:5
	subl	$-215, %eax
	movl	%eax, -24(%rbp)         # 4-byte Spill
	je	.LBB20_7
	jmp	.LBB20_13
.LBB20_13:
	.loc	4 0 5                   # parser.c:0:5
	movl	-16(%rbp), %eax         # 4-byte Reload
	.loc	4 277 5                 # parser.c:277:5
	subl	$-214, %eax
	movl	%eax, -28(%rbp)         # 4-byte Spill
	je	.LBB20_2
	jmp	.LBB20_14
.LBB20_14:
	.loc	4 0 5                   # parser.c:0:5
	movl	-16(%rbp), %eax         # 4-byte Reload
	.loc	4 277 5                 # parser.c:277:5
	subl	$-213, %eax
	movl	%eax, -32(%rbp)         # 4-byte Spill
	je	.LBB20_3
	jmp	.LBB20_15
.LBB20_15:
	.loc	4 0 5                   # parser.c:0:5
	movl	-16(%rbp), %eax         # 4-byte Reload
	.loc	4 277 5                 # parser.c:277:5
	subl	$-210, %eax
	movl	%eax, -36(%rbp)         # 4-byte Spill
	je	.LBB20_5
	jmp	.LBB20_16
.LBB20_16:
	.loc	4 0 5                   # parser.c:0:5
	movl	-16(%rbp), %eax         # 4-byte Reload
	.loc	4 277 5                 # parser.c:277:5
	subl	$-193, %eax
	movl	%eax, -40(%rbp)         # 4-byte Spill
	je	.LBB20_4
	jmp	.LBB20_17
.LBB20_17:
	.loc	4 0 5                   # parser.c:0:5
	movl	-16(%rbp), %eax         # 4-byte Reload
	.loc	4 277 5                 # parser.c:277:5
	subl	$-132, %eax
	movl	%eax, -44(%rbp)         # 4-byte Spill
	je	.LBB20_1
	jmp	.LBB20_18
.LBB20_18:
	.loc	4 0 5                   # parser.c:0:5
	movl	-16(%rbp), %eax         # 4-byte Reload
	.loc	4 277 5                 # parser.c:277:5
	subl	$-1, %eax
	movl	%eax, -48(%rbp)         # 4-byte Spill
	je	.LBB20_8
	jmp	.LBB20_19
.LBB20_19:
	.loc	4 0 5                   # parser.c:0:5
	movl	-16(%rbp), %eax         # 4-byte Reload
	.loc	4 277 5                 # parser.c:277:5
	testl	%eax, %eax
	je	.LBB20_9
	jmp	.LBB20_10
.LBB20_1:
.Ltmp135:
	.loc	4 278 32 is_stmt 1      # parser.c:278:32
	movabsq	$.L.str.5, %rax
	movq	%rax, -8(%rbp)
	.loc	4 278 39 is_stmt 0      # parser.c:278:39
	jmp	.LBB20_11
.LBB20_2:
	.loc	4 279 32 is_stmt 1      # parser.c:279:32
	movabsq	$.L.str.6, %rax
	movq	%rax, -8(%rbp)
	.loc	4 279 39 is_stmt 0      # parser.c:279:39
	jmp	.LBB20_11
.LBB20_3:
	.loc	4 280 32 is_stmt 1      # parser.c:280:32
	movabsq	$.L.str.7, %rax
	movq	%rax, -8(%rbp)
	.loc	4 280 39 is_stmt 0      # parser.c:280:39
	jmp	.LBB20_11
.LBB20_4:
	.loc	4 281 32 is_stmt 1      # parser.c:281:32
	movabsq	$.L.str.8, %rax
	movq	%rax, -8(%rbp)
	.loc	4 281 39 is_stmt 0      # parser.c:281:39
	jmp	.LBB20_11
.LBB20_5:
	.loc	4 282 32 is_stmt 1      # parser.c:282:32
	movabsq	$.L.str.9, %rax
	movq	%rax, -8(%rbp)
	.loc	4 282 39 is_stmt 0      # parser.c:282:39
	jmp	.LBB20_11
.LBB20_6:
	.loc	4 283 32 is_stmt 1      # parser.c:283:32
	movabsq	$.L.str.10, %rax
	movq	%rax, -8(%rbp)
	.loc	4 283 39 is_stmt 0      # parser.c:283:39
	jmp	.LBB20_11
.LBB20_7:
	.loc	4 284 32 is_stmt 1      # parser.c:284:32
	movabsq	$.L.str.11, %rax
	movq	%rax, -8(%rbp)
	.loc	4 284 39 is_stmt 0      # parser.c:284:39
	jmp	.LBB20_11
.LBB20_8:
	.loc	4 285 32 is_stmt 1      # parser.c:285:32
	movabsq	$.L.str.12, %rax
	movq	%rax, -8(%rbp)
	.loc	4 285 44 is_stmt 0      # parser.c:285:44
	jmp	.LBB20_11
.LBB20_9:
	.loc	4 286 32 is_stmt 1      # parser.c:286:32
	movabsq	$.L.str.13, %rax
	movq	%rax, -8(%rbp)
	.loc	4 286 41 is_stmt 0      # parser.c:286:41
	jmp	.LBB20_11
.LBB20_10:
	.loc	4 288 25 is_stmt 1      # parser.c:288:25
	movl	-12(%rbp), %eax
	movb	%al, %cl
	.loc	4 288 13 is_stmt 0      # parser.c:288:13
	movq	-8(%rbp), %rdx
	.loc	4 288 23                # parser.c:288:23
	movb	%cl, (%rdx)
.Ltmp136:
.LBB20_11:
	.loc	4 292 12 is_stmt 1      # parser.c:292:12
	movq	-8(%rbp), %rax
	.loc	4 292 5 is_stmt 0       # parser.c:292:5
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp137:
.Lfunc_end20:
	.size	lexeme_for, .Lfunc_end20-lexeme_for
	.cfi_endproc
                                        # -- End function
	.globl	print_parse_error       # -- Begin function print_parse_error
	.p2align	4, 0x90
	.type	print_parse_error,@function
print_parse_error:                      # @print_parse_error
.Lfunc_begin21:
	.loc	4 297 0 is_stmt 1       # parser.c:297:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$80, %rsp
	leaq	-34(%rbp), %rax
	xorl	%ecx, %ecx
	movq	%rdi, -32(%rbp)
	movl	%esi, -24(%rbp)
	movq	-32(%rbp), %rdi
	movq	%rdi, -16(%rbp)
	movl	-24(%rbp), %esi
	movl	%esi, -8(%rbp)
.Ltmp138:
	#DEBUG_VALUE: print_parse_error:symbuf <- [$rax+0]
	.loc	4 298 10 prologue_end   # parser.c:298:10
	movq	%rax, %rdi
	movl	%ecx, %esi
	movl	$2, %edx
	movq	%rax, -48(%rbp)         # 8-byte Spill
.Ltmp139:
	#DEBUG_VALUE: print_parse_error:symbuf <- [DW_OP_constu 48, DW_OP_minus, DW_OP_deref] [$rbp+0]
	callq	memset
	.loc	4 300 13                # parser.c:300:13
	movq	stderr, %rdi
	.loc	4 301 34                # parser.c:301:34
	movl	-8(%rbp), %esi
	movq	-48(%rbp), %rax         # 8-byte Reload
	movq	%rdi, -56(%rbp)         # 8-byte Spill
	.loc	4 301 9 is_stmt 0       # parser.c:301:9
	movq	%rax, %rdi
	callq	lexeme_for
	leaq	-34(%rbp), %rdi
	.loc	4 302 34 is_stmt 1      # parser.c:302:34
	movl	-12(%rbp), %esi
	movq	%rax, -64(%rbp)         # 8-byte Spill
	.loc	4 302 9 is_stmt 0       # parser.c:302:9
	callq	lexeme_for
	.loc	4 303 15 is_stmt 1      # parser.c:303:15
	movl	-16(%rbp), %r8d
	movq	-56(%rbp), %rdi         # 8-byte Reload
	.loc	4 300 5                 # parser.c:300:5
	movabsq	$.L.str.14, %rsi
	movq	-64(%rbp), %rdx         # 8-byte Reload
	movq	%rax, %rcx
	movb	$0, %al
	callq	fprintf
	movl	%eax, -68(%rbp)         # 4-byte Spill
	.loc	4 304 1                 # parser.c:304:1
	addq	$80, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp140:
.Lfunc_end21:
	.size	print_parse_error, .Lfunc_end21-print_parse_error
	.cfi_endproc
                                        # -- End function
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"input != NULL"
	.size	.L.str, 14

	.type	.L.str.1,@object        # @.str.1
.L.str.1:
	.asciz	"parser.c"
	.size	.L.str.1, 9

	.type	.L__PRETTY_FUNCTION__.parse_context,@object # @__PRETTY_FUNCTION__.parse_context
.L__PRETTY_FUNCTION__.parse_context:
	.asciz	"struct parse_context parse_context(char *, void *, union rval (*)(void *), void (**)(void *, union rval))"
	.size	.L__PRETTY_FUNCTION__.parse_context, 106

	.type	.L.str.2,@object        # @.str.2
.L.str.2:
	.asciz	"result_context != NULL"
	.size	.L.str.2, 23

	.type	.L.str.3,@object        # @.str.3
.L.str.3:
	.asciz	"actions != NULL"
	.size	.L.str.3, 16

	.type	.L.str.4,@object        # @.str.4
.L.str.4:
	.asciz	"getval != NULL"
	.size	.L.str.4, 15

	.type	.L.str.5,@object        # @.str.5
.L.str.5:
	.asciz	"|"
	.size	.L.str.5, 2

	.type	.L.str.6,@object        # @.str.6
.L.str.6:
	.asciz	"*"
	.size	.L.str.6, 2

	.type	.L.str.7,@object        # @.str.7
.L.str.7:
	.asciz	"+"
	.size	.L.str.7, 2

	.type	.L.str.8,@object        # @.str.8
.L.str.8:
	.asciz	"?"
	.size	.L.str.8, 2

	.type	.L.str.9,@object        # @.str.9
.L.str.9:
	.asciz	"."
	.size	.L.str.9, 2

	.type	.L.str.10,@object       # @.str.10
.L.str.10:
	.asciz	"("
	.size	.L.str.10, 2

	.type	.L.str.11,@object       # @.str.11
.L.str.11:
	.asciz	")"
	.size	.L.str.11, 2

	.type	.L.str.12,@object       # @.str.12
.L.str.12:
	.asciz	"symbol"
	.size	.L.str.12, 7

	.type	.L.str.13,@object       # @.str.13
.L.str.13:
	.asciz	"eof"
	.size	.L.str.13, 4

	.type	.L.str.14,@object       # @.str.14
.L.str.14:
	.asciz	"| Parse Error\n|\n| Got: %s\n| Expected: %s\n|\n| At Column: %d\n|\n"
	.size	.L.str.14, 62

	.type	NULLRVAL,@object        # @NULLRVAL
	.section	.rodata,"a",@progbits
	.p2align	3
NULLRVAL:
	.quad	0
	.zero	16
	.size	NULLRVAL, 24

	.section	.debug_str,"MS",@progbits,1
.Linfo_string0:
	.asciz	"clang version 8.0.0-3~ubuntu18.04.1 (tags/RELEASE_800/final)" # string offset=0
.Linfo_string1:
	.asciz	"parser.c"              # string offset=61
.Linfo_string2:
	.asciz	"/home/wroathe/compilers/src/auto" # string offset=70
.Linfo_string3:
	.asciz	"NULLRVAL"              # string offset=103
.Linfo_string4:
	.asciz	"_"                     # string offset=112
.Linfo_string5:
	.asciz	"expr"                  # string offset=114
.Linfo_string6:
	.asciz	"type"                  # string offset=119
.Linfo_string7:
	.asciz	"unsigned int"          # string offset=124
.Linfo_string8:
	.asciz	"NULL_EXPR"             # string offset=137
.Linfo_string9:
	.asciz	"EMPTY_EXPR"            # string offset=147
.Linfo_string10:
	.asciz	"DOTALL_EXPR"           # string offset=158
.Linfo_string11:
	.asciz	"ALT_EXPR"              # string offset=170
.Linfo_string12:
	.asciz	"CAT_EXPR"              # string offset=179
.Linfo_string13:
	.asciz	"STAR_EXPR"             # string offset=188
.Linfo_string14:
	.asciz	"PLUS_EXPR"             # string offset=198
.Linfo_string15:
	.asciz	"OPTIONAL_EXPR"         # string offset=208
.Linfo_string16:
	.asciz	"SUB_EXPR"              # string offset=222
.Linfo_string17:
	.asciz	"SYMBOL_EXPR"           # string offset=231
.Linfo_string18:
	.asciz	"expr_type"             # string offset=243
.Linfo_string19:
	.asciz	"lexpr"                 # string offset=253
.Linfo_string20:
	.asciz	"rexpr"                 # string offset=259
.Linfo_string21:
	.asciz	"symbol"                # string offset=265
.Linfo_string22:
	.asciz	"char"                  # string offset=272
.Linfo_string23:
	.asciz	"mach"                  # string offset=277
.Linfo_string24:
	.asciz	"start"                 # string offset=282
.Linfo_string25:
	.asciz	"ACCEPTING_STATE"       # string offset=288
.Linfo_string26:
	.asciz	"EPSILON_STATE"         # string offset=304
.Linfo_string27:
	.asciz	"BRANCH_STATE"          # string offset=318
.Linfo_string28:
	.asciz	"SYMBOL_STATE"          # string offset=331
.Linfo_string29:
	.asciz	"DOTALL_STATE"          # string offset=344
.Linfo_string30:
	.asciz	"nfa_state_type"        # string offset=357
.Linfo_string31:
	.asciz	"id"                    # string offset=372
.Linfo_string32:
	.asciz	"int"                   # string offset=375
.Linfo_string33:
	.asciz	"next"                  # string offset=379
.Linfo_string34:
	.asciz	"left"                  # string offset=384
.Linfo_string35:
	.asciz	"right"                 # string offset=389
.Linfo_string36:
	.asciz	"nfa_state"             # string offset=395
.Linfo_string37:
	.asciz	"end"                   # string offset=405
.Linfo_string38:
	.asciz	"end1"                  # string offset=409
.Linfo_string39:
	.asciz	"nfa"                   # string offset=414
.Linfo_string40:
	.asciz	"sym"                   # string offset=418
.Linfo_string41:
	.asciz	"rval"                  # string offset=422
.Linfo_string42:
	.asciz	"_ISupper"              # string offset=427
.Linfo_string43:
	.asciz	"_ISlower"              # string offset=436
.Linfo_string44:
	.asciz	"_ISalpha"              # string offset=445
.Linfo_string45:
	.asciz	"_ISdigit"              # string offset=454
.Linfo_string46:
	.asciz	"_ISxdigit"             # string offset=463
.Linfo_string47:
	.asciz	"_ISspace"              # string offset=473
.Linfo_string48:
	.asciz	"_ISprint"              # string offset=482
.Linfo_string49:
	.asciz	"_ISgraph"              # string offset=491
.Linfo_string50:
	.asciz	"_ISblank"              # string offset=500
.Linfo_string51:
	.asciz	"_IScntrl"              # string offset=509
.Linfo_string52:
	.asciz	"_ISpunct"              # string offset=518
.Linfo_string53:
	.asciz	"_ISalnum"              # string offset=527
.Linfo_string54:
	.asciz	"SYMBOL"                # string offset=536
.Linfo_string55:
	.asciz	"ALT"                   # string offset=543
.Linfo_string56:
	.asciz	"STAR"                  # string offset=547
.Linfo_string57:
	.asciz	"PLUS"                  # string offset=552
.Linfo_string58:
	.asciz	"OPTIONAL"              # string offset=557
.Linfo_string59:
	.asciz	"DOTALL"                # string offset=566
.Linfo_string60:
	.asciz	"LPAREN"                # string offset=573
.Linfo_string61:
	.asciz	"RPAREN"                # string offset=580
.Linfo_string62:
	.asciz	"token_type"            # string offset=587
.Linfo_string63:
	.asciz	"DO_REGEX"              # string offset=598
.Linfo_string64:
	.asciz	"DO_EMPTY"              # string offset=607
.Linfo_string65:
	.asciz	"DO_ALT"                # string offset=616
.Linfo_string66:
	.asciz	"DO_CAT"                # string offset=623
.Linfo_string67:
	.asciz	"DO_SUB"                # string offset=630
.Linfo_string68:
	.asciz	"DO_DOTALL"             # string offset=637
.Linfo_string69:
	.asciz	"DO_SYMBOL"             # string offset=647
.Linfo_string70:
	.asciz	"DO_STAR"               # string offset=657
.Linfo_string71:
	.asciz	"DO_PLUS"               # string offset=665
.Linfo_string72:
	.asciz	"DO_OPTIONAL"           # string offset=673
.Linfo_string73:
	.asciz	"action_type"           # string offset=685
.Linfo_string74:
	.asciz	"unsigned short"        # string offset=697
.Linfo_string75:
	.asciz	"scan_context"          # string offset=712
.Linfo_string76:
	.asciz	"input"                 # string offset=725
.Linfo_string77:
	.asciz	"input_col"             # string offset=731
.Linfo_string78:
	.asciz	"token"                 # string offset=741
.Linfo_string79:
	.asciz	"token_col"             # string offset=747
.Linfo_string80:
	.asciz	"consume"               # string offset=757
.Linfo_string81:
	.asciz	"scan"                  # string offset=765
.Linfo_string82:
	.asciz	"getval"                # string offset=770
.Linfo_string83:
	.asciz	"do_action"             # string offset=777
.Linfo_string84:
	.asciz	"parse_context"         # string offset=787
.Linfo_string85:
	.asciz	"result_context"        # string offset=801
.Linfo_string86:
	.asciz	"actions"               # string offset=816
.Linfo_string87:
	.asciz	"lookahead"             # string offset=824
.Linfo_string88:
	.asciz	"lookahead_col"         # string offset=834
.Linfo_string89:
	.asciz	"has_error"             # string offset=848
.Linfo_string90:
	.asciz	"_Bool"                 # string offset=858
.Linfo_string91:
	.asciz	"error"                 # string offset=864
.Linfo_string92:
	.asciz	"expected"              # string offset=870
.Linfo_string93:
	.asciz	"actual"                # string offset=879
.Linfo_string94:
	.asciz	"parse_error"           # string offset=886
.Linfo_string95:
	.asciz	"peek"                  # string offset=898
.Linfo_string96:
	.asciz	"expect"                # string offset=903
.Linfo_string97:
	.asciz	"parse_regex"           # string offset=910
.Linfo_string98:
	.asciz	"parse_expr"            # string offset=922
.Linfo_string99:
	.asciz	"parse_alt"             # string offset=933
.Linfo_string100:
	.asciz	"parse_cat"             # string offset=943
.Linfo_string101:
	.asciz	"parse_factor"          # string offset=953
.Linfo_string102:
	.asciz	"is_symbol"             # string offset=966
.Linfo_string103:
	.asciz	"has_parse_error"       # string offset=976
.Linfo_string104:
	.asciz	"nullperr"              # string offset=992
.Linfo_string105:
	.asciz	"lexeme_for"            # string offset=1001
.Linfo_string106:
	.asciz	"print_parse_error"     # string offset=1012
.Linfo_string107:
	.asciz	"context"               # string offset=1030
.Linfo_string108:
	.asciz	"c"                     # string offset=1038
.Linfo_string109:
	.asciz	"action"                # string offset=1040
.Linfo_string110:
	.asciz	"lval"                  # string offset=1047
.Linfo_string111:
	.asciz	"scontext"              # string offset=1052
.Linfo_string112:
	.asciz	"is"                    # string offset=1061
.Linfo_string113:
	.asciz	"empty"                 # string offset=1064
.Linfo_string114:
	.asciz	"has_head"              # string offset=1070
.Linfo_string115:
	.asciz	"symbuf"                # string offset=1079
.Linfo_string116:
	.asciz	"__ARRAY_SIZE_TYPE__"   # string offset=1086
	.section	.debug_loc,"",@progbits
.Ldebug_loc0:
	.quad	.Ltmp38-.Lfunc_begin0
	.quad	.Ltmp40-.Lfunc_begin0
	.short	2                       # Loc expr size
	.byte	117                     # DW_OP_breg5
	.byte	0                       # 0
	.quad	.Ltmp40-.Lfunc_begin0
	.quad	.Lfunc_end7-.Lfunc_begin0
	.short	4                       # Loc expr size
	.byte	118                     # DW_OP_breg6
	.byte	144                     # -112
	.byte	127                     # 
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
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	2                       # DW_AT_location
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	3                       # Abbreviation Code
	.byte	38                      # DW_TAG_const_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	4                       # Abbreviation Code
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
	.byte	5                       # Abbreviation Code
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
	.byte	6                       # Abbreviation Code
	.byte	15                      # DW_TAG_pointer_type
	.byte	0                       # DW_CHILDREN_no
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	7                       # Abbreviation Code
	.byte	15                      # DW_TAG_pointer_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	8                       # Abbreviation Code
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
	.byte	9                       # Abbreviation Code
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
	.byte	10                      # Abbreviation Code
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
	.byte	11                      # Abbreviation Code
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
	.byte	12                      # Abbreviation Code
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
	.byte	13                      # Abbreviation Code
	.byte	40                      # DW_TAG_enumerator
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	28                      # DW_AT_const_value
	.byte	15                      # DW_FORM_udata
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	14                      # Abbreviation Code
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
	.byte	15                      # Abbreviation Code
	.byte	4                       # DW_TAG_enumeration_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	11                      # DW_AT_byte_size
	.byte	11                      # DW_FORM_data1
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	16                      # Abbreviation Code
	.byte	40                      # DW_TAG_enumerator
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	28                      # DW_AT_const_value
	.byte	13                      # DW_FORM_sdata
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	17                      # Abbreviation Code
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
	.byte	18                      # Abbreviation Code
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
	.byte	19                      # Abbreviation Code
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
	.byte	20                      # Abbreviation Code
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
	.byte	21                      # Abbreviation Code
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
	.byte	22                      # Abbreviation Code
	.byte	11                      # DW_TAG_lexical_block
	.byte	1                       # DW_CHILDREN_yes
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	6                       # DW_FORM_data4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	23                      # Abbreviation Code
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
	.byte	24                      # Abbreviation Code
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
	.byte	25                      # Abbreviation Code
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
	.byte	5                       # DW_FORM_data2
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	63                      # DW_AT_external
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	26                      # Abbreviation Code
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
	.byte	27                      # Abbreviation Code
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
	.byte	28                      # Abbreviation Code
	.byte	21                      # DW_TAG_subroutine_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	29                      # Abbreviation Code
	.byte	5                       # DW_TAG_formal_parameter
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	30                      # Abbreviation Code
	.byte	21                      # DW_TAG_subroutine_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	31                      # Abbreviation Code
	.byte	1                       # DW_TAG_array_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	32                      # Abbreviation Code
	.byte	33                      # DW_TAG_subrange_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	55                      # DW_AT_count
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	33                      # Abbreviation Code
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
	.byte	0                       # EOM(3)
	.section	.debug_info,"",@progbits
.Lcu_begin0:
	.long	.Ldebug_info_end0-.Ldebug_info_start0 # Length of Unit
.Ldebug_info_start0:
	.short	4                       # DWARF version number
	.long	.debug_abbrev           # Offset Into Abbrev. Section
	.byte	8                       # Address Size (in bytes)
	.byte	1                       # Abbrev [1] 0xb:0x8df DW_TAG_compile_unit
	.long	.Linfo_string0          # DW_AT_producer
	.short	12                      # DW_AT_language
	.long	.Linfo_string1          # DW_AT_name
	.long	.Lline_table_start0     # DW_AT_stmt_list
	.long	.Linfo_string2          # DW_AT_comp_dir
	.quad	.Lfunc_begin0           # DW_AT_low_pc
	.long	.Lfunc_end21-.Lfunc_begin0 # DW_AT_high_pc
	.byte	2                       # Abbrev [2] 0x2a:0x15 DW_TAG_variable
	.long	.Linfo_string3          # DW_AT_name
	.long	63                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	63                      # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	NULLRVAL
	.byte	3                       # Abbrev [3] 0x3f:0x5 DW_TAG_const_type
	.long	68                      # DW_AT_type
	.byte	4                       # Abbrev [4] 0x44:0x39 DW_TAG_union_type
	.long	.Linfo_string41         # DW_AT_name
	.byte	24                      # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	56                      # DW_AT_decl_line
	.byte	5                       # Abbrev [5] 0x4c:0xc DW_TAG_member
	.long	.Linfo_string4          # DW_AT_name
	.long	125                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	57                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0x58:0xc DW_TAG_member
	.long	.Linfo_string5          # DW_AT_name
	.long	126                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	58                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0x64:0xc DW_TAG_member
	.long	.Linfo_string23         # DW_AT_name
	.long	339                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	59                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0x70:0xc DW_TAG_member
	.long	.Linfo_string40         # DW_AT_name
	.long	332                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	60                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x7d:0x1 DW_TAG_pointer_type
	.byte	7                       # Abbrev [7] 0x7e:0x5 DW_TAG_pointer_type
	.long	131                     # DW_AT_type
	.byte	8                       # Abbrev [8] 0x83:0x79 DW_TAG_structure_type
	.long	.Linfo_string5          # DW_AT_name
	.byte	24                      # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	17                      # DW_AT_decl_line
	.byte	5                       # Abbrev [5] 0x8b:0xc DW_TAG_member
	.long	.Linfo_string6          # DW_AT_name
	.long	252                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	18                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	9                       # Abbrev [9] 0x97:0x8 DW_TAG_member
	.long	159                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	19                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	10                      # Abbrev [10] 0x9f:0x5c DW_TAG_union_type
	.byte	16                      # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	19                      # DW_AT_decl_line
	.byte	9                       # Abbrev [9] 0xa3:0x8 DW_TAG_member
	.long	171                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	21                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xab:0x1d DW_TAG_structure_type
	.byte	16                      # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	21                      # DW_AT_decl_line
	.byte	5                       # Abbrev [5] 0xaf:0xc DW_TAG_member
	.long	.Linfo_string19         # DW_AT_name
	.long	126                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	21                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0xbb:0xc DW_TAG_member
	.long	.Linfo_string20         # DW_AT_name
	.long	126                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	21                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	9                       # Abbrev [9] 0xc8:0x8 DW_TAG_member
	.long	208                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	23                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xd0:0x11 DW_TAG_structure_type
	.byte	8                       # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	23                      # DW_AT_decl_line
	.byte	5                       # Abbrev [5] 0xd4:0xc DW_TAG_member
	.long	.Linfo_string5          # DW_AT_name
	.long	126                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	23                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	9                       # Abbrev [9] 0xe1:0x8 DW_TAG_member
	.long	233                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	25                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0xe9:0x11 DW_TAG_structure_type
	.byte	1                       # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	25                      # DW_AT_decl_line
	.byte	5                       # Abbrev [5] 0xed:0xc DW_TAG_member
	.long	.Linfo_string21         # DW_AT_name
	.long	332                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	25                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0xfc:0x49 DW_TAG_enumeration_type
	.long	325                     # DW_AT_type
	.long	.Linfo_string18         # DW_AT_name
	.byte	4                       # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	4                       # DW_AT_decl_line
	.byte	13                      # Abbrev [13] 0x108:0x6 DW_TAG_enumerator
	.long	.Linfo_string8          # DW_AT_name
	.byte	0                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x10e:0x6 DW_TAG_enumerator
	.long	.Linfo_string9          # DW_AT_name
	.byte	1                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x114:0x6 DW_TAG_enumerator
	.long	.Linfo_string10         # DW_AT_name
	.byte	2                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x11a:0x6 DW_TAG_enumerator
	.long	.Linfo_string11         # DW_AT_name
	.byte	3                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x120:0x6 DW_TAG_enumerator
	.long	.Linfo_string12         # DW_AT_name
	.byte	4                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x126:0x6 DW_TAG_enumerator
	.long	.Linfo_string13         # DW_AT_name
	.byte	5                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x12c:0x6 DW_TAG_enumerator
	.long	.Linfo_string14         # DW_AT_name
	.byte	6                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x132:0x6 DW_TAG_enumerator
	.long	.Linfo_string15         # DW_AT_name
	.byte	7                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x138:0x6 DW_TAG_enumerator
	.long	.Linfo_string16         # DW_AT_name
	.byte	8                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x13e:0x6 DW_TAG_enumerator
	.long	.Linfo_string17         # DW_AT_name
	.byte	9                       # DW_AT_const_value
	.byte	0                       # End Of Children Mark
	.byte	14                      # Abbrev [14] 0x145:0x7 DW_TAG_base_type
	.long	.Linfo_string7          # DW_AT_name
	.byte	7                       # DW_AT_encoding
	.byte	4                       # DW_AT_byte_size
	.byte	14                      # Abbrev [14] 0x14c:0x7 DW_TAG_base_type
	.long	.Linfo_string22         # DW_AT_name
	.byte	6                       # DW_AT_encoding
	.byte	1                       # DW_AT_byte_size
	.byte	8                       # Abbrev [8] 0x153:0x2d DW_TAG_structure_type
	.long	.Linfo_string39         # DW_AT_name
	.byte	24                      # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	50                      # DW_AT_decl_line
	.byte	5                       # Abbrev [5] 0x15b:0xc DW_TAG_member
	.long	.Linfo_string24         # DW_AT_name
	.long	384                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	51                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0x167:0xc DW_TAG_member
	.long	.Linfo_string37         # DW_AT_name
	.long	559                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	52                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0x173:0xc DW_TAG_member
	.long	.Linfo_string38         # DW_AT_name
	.long	559                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	53                      # DW_AT_decl_line
	.byte	16                      # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	7                       # Abbrev [7] 0x180:0x5 DW_TAG_pointer_type
	.long	389                     # DW_AT_type
	.byte	8                       # Abbrev [8] 0x185:0x78 DW_TAG_structure_type
	.long	.Linfo_string36         # DW_AT_name
	.byte	24                      # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	38                      # DW_AT_decl_line
	.byte	5                       # Abbrev [5] 0x18d:0xc DW_TAG_member
	.long	.Linfo_string6          # DW_AT_name
	.long	509                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	39                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0x199:0xc DW_TAG_member
	.long	.Linfo_string31         # DW_AT_name
	.long	552                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	40                      # DW_AT_decl_line
	.byte	4                       # DW_AT_data_member_location
	.byte	9                       # Abbrev [9] 0x1a5:0x8 DW_TAG_member
	.long	429                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	41                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	10                      # Abbrev [10] 0x1ad:0x4f DW_TAG_union_type
	.byte	16                      # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	41                      # DW_AT_decl_line
	.byte	9                       # Abbrev [9] 0x1b1:0x8 DW_TAG_member
	.long	441                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	43                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0x1b9:0x1d DW_TAG_structure_type
	.byte	16                      # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	43                      # DW_AT_decl_line
	.byte	5                       # Abbrev [5] 0x1bd:0xc DW_TAG_member
	.long	.Linfo_string33         # DW_AT_name
	.long	384                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	43                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0x1c9:0xc DW_TAG_member
	.long	.Linfo_string21         # DW_AT_name
	.long	332                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	43                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	9                       # Abbrev [9] 0x1d6:0x8 DW_TAG_member
	.long	478                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	45                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	11                      # Abbrev [11] 0x1de:0x1d DW_TAG_structure_type
	.byte	16                      # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	45                      # DW_AT_decl_line
	.byte	5                       # Abbrev [5] 0x1e2:0xc DW_TAG_member
	.long	.Linfo_string34         # DW_AT_name
	.long	384                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	45                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0x1ee:0xc DW_TAG_member
	.long	.Linfo_string35         # DW_AT_name
	.long	384                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	45                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x1fd:0x2b DW_TAG_enumeration_type
	.long	325                     # DW_AT_type
	.long	.Linfo_string30         # DW_AT_name
	.byte	4                       # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	30                      # DW_AT_decl_line
	.byte	13                      # Abbrev [13] 0x209:0x6 DW_TAG_enumerator
	.long	.Linfo_string25         # DW_AT_name
	.byte	0                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x20f:0x6 DW_TAG_enumerator
	.long	.Linfo_string26         # DW_AT_name
	.byte	1                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x215:0x6 DW_TAG_enumerator
	.long	.Linfo_string27         # DW_AT_name
	.byte	2                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x21b:0x6 DW_TAG_enumerator
	.long	.Linfo_string28         # DW_AT_name
	.byte	3                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x221:0x6 DW_TAG_enumerator
	.long	.Linfo_string29         # DW_AT_name
	.byte	4                       # DW_AT_const_value
	.byte	0                       # End Of Children Mark
	.byte	14                      # Abbrev [14] 0x228:0x7 DW_TAG_base_type
	.long	.Linfo_string32         # DW_AT_name
	.byte	5                       # DW_AT_encoding
	.byte	4                       # DW_AT_byte_size
	.byte	7                       # Abbrev [7] 0x22f:0x5 DW_TAG_pointer_type
	.long	384                     # DW_AT_type
	.byte	15                      # Abbrev [15] 0x234:0x5b DW_TAG_enumeration_type
	.long	325                     # DW_AT_type
	.byte	4                       # DW_AT_byte_size
	.byte	2                       # DW_AT_decl_file
	.byte	46                      # DW_AT_decl_line
	.byte	13                      # Abbrev [13] 0x23c:0x7 DW_TAG_enumerator
	.long	.Linfo_string42         # DW_AT_name
	.ascii	"\200\002"              # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x243:0x7 DW_TAG_enumerator
	.long	.Linfo_string43         # DW_AT_name
	.ascii	"\200\004"              # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x24a:0x7 DW_TAG_enumerator
	.long	.Linfo_string44         # DW_AT_name
	.ascii	"\200\b"                # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x251:0x7 DW_TAG_enumerator
	.long	.Linfo_string45         # DW_AT_name
	.ascii	"\200\020"              # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x258:0x7 DW_TAG_enumerator
	.long	.Linfo_string46         # DW_AT_name
	.ascii	"\200 "                 # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x25f:0x7 DW_TAG_enumerator
	.long	.Linfo_string47         # DW_AT_name
	.ascii	"\200@"                 # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x266:0x8 DW_TAG_enumerator
	.long	.Linfo_string48         # DW_AT_name
	.ascii	"\200\200\001"          # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x26e:0x8 DW_TAG_enumerator
	.long	.Linfo_string49         # DW_AT_name
	.ascii	"\200\200\002"          # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x276:0x6 DW_TAG_enumerator
	.long	.Linfo_string50         # DW_AT_name
	.byte	1                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x27c:0x6 DW_TAG_enumerator
	.long	.Linfo_string51         # DW_AT_name
	.byte	2                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x282:0x6 DW_TAG_enumerator
	.long	.Linfo_string52         # DW_AT_name
	.byte	4                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x288:0x6 DW_TAG_enumerator
	.long	.Linfo_string53         # DW_AT_name
	.byte	8                       # DW_AT_const_value
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x28f:0x44 DW_TAG_enumeration_type
	.long	552                     # DW_AT_type
	.long	.Linfo_string62         # DW_AT_name
	.byte	4                       # DW_AT_byte_size
	.byte	3                       # DW_AT_decl_file
	.byte	36                      # DW_AT_decl_line
	.byte	16                      # Abbrev [16] 0x29b:0x6 DW_TAG_enumerator
	.long	.Linfo_string54         # DW_AT_name
	.byte	127                     # DW_AT_const_value
	.byte	16                      # Abbrev [16] 0x2a1:0x7 DW_TAG_enumerator
	.long	.Linfo_string55         # DW_AT_name
	.ascii	"\374~"                 # DW_AT_const_value
	.byte	16                      # Abbrev [16] 0x2a8:0x7 DW_TAG_enumerator
	.long	.Linfo_string56         # DW_AT_name
	.ascii	"\252~"                 # DW_AT_const_value
	.byte	16                      # Abbrev [16] 0x2af:0x7 DW_TAG_enumerator
	.long	.Linfo_string57         # DW_AT_name
	.ascii	"\253~"                 # DW_AT_const_value
	.byte	16                      # Abbrev [16] 0x2b6:0x7 DW_TAG_enumerator
	.long	.Linfo_string58         # DW_AT_name
	.ascii	"\277~"                 # DW_AT_const_value
	.byte	16                      # Abbrev [16] 0x2bd:0x7 DW_TAG_enumerator
	.long	.Linfo_string59         # DW_AT_name
	.ascii	"\256~"                 # DW_AT_const_value
	.byte	16                      # Abbrev [16] 0x2c4:0x7 DW_TAG_enumerator
	.long	.Linfo_string60         # DW_AT_name
	.ascii	"\250~"                 # DW_AT_const_value
	.byte	16                      # Abbrev [16] 0x2cb:0x7 DW_TAG_enumerator
	.long	.Linfo_string61         # DW_AT_name
	.ascii	"\251~"                 # DW_AT_const_value
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x2d3:0x49 DW_TAG_enumeration_type
	.long	325                     # DW_AT_type
	.long	.Linfo_string73         # DW_AT_name
	.byte	4                       # DW_AT_byte_size
	.byte	3                       # DW_AT_decl_file
	.byte	47                      # DW_AT_decl_line
	.byte	13                      # Abbrev [13] 0x2df:0x6 DW_TAG_enumerator
	.long	.Linfo_string63         # DW_AT_name
	.byte	0                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x2e5:0x6 DW_TAG_enumerator
	.long	.Linfo_string64         # DW_AT_name
	.byte	1                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x2eb:0x6 DW_TAG_enumerator
	.long	.Linfo_string65         # DW_AT_name
	.byte	2                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x2f1:0x6 DW_TAG_enumerator
	.long	.Linfo_string66         # DW_AT_name
	.byte	3                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x2f7:0x6 DW_TAG_enumerator
	.long	.Linfo_string67         # DW_AT_name
	.byte	4                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x2fd:0x6 DW_TAG_enumerator
	.long	.Linfo_string68         # DW_AT_name
	.byte	5                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x303:0x6 DW_TAG_enumerator
	.long	.Linfo_string69         # DW_AT_name
	.byte	6                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x309:0x6 DW_TAG_enumerator
	.long	.Linfo_string70         # DW_AT_name
	.byte	7                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x30f:0x6 DW_TAG_enumerator
	.long	.Linfo_string71         # DW_AT_name
	.byte	8                       # DW_AT_const_value
	.byte	13                      # Abbrev [13] 0x315:0x6 DW_TAG_enumerator
	.long	.Linfo_string72         # DW_AT_name
	.byte	9                       # DW_AT_const_value
	.byte	0                       # End Of Children Mark
	.byte	14                      # Abbrev [14] 0x31c:0x7 DW_TAG_base_type
	.long	.Linfo_string74         # DW_AT_name
	.byte	7                       # DW_AT_encoding
	.byte	2                       # DW_AT_byte_size
	.byte	17                      # Abbrev [17] 0x323:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin0           # DW_AT_low_pc
	.long	.Lfunc_end0-.Lfunc_begin0 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string75         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	9                       # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	1984                    # DW_AT_type
                                        # DW_AT_external
	.byte	18                      # Abbrev [18] 0x33c:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string76         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	9                       # DW_AT_decl_line
	.long	2041                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	17                      # Abbrev [17] 0x34b:0x36 DW_TAG_subprogram
	.quad	.Lfunc_begin1           # DW_AT_low_pc
	.long	.Lfunc_end1-.Lfunc_begin1 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string80         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	18                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	1984                    # DW_AT_type
                                        # DW_AT_external
	.byte	18                      # Abbrev [18] 0x364:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string107        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	18                      # DW_AT_decl_line
	.long	1984                    # DW_AT_type
	.byte	18                      # Abbrev [18] 0x372:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	127
	.long	.Linfo_string108        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	18                      # DW_AT_decl_line
	.long	332                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	17                      # Abbrev [17] 0x381:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin2           # DW_AT_low_pc
	.long	.Lfunc_end2-.Lfunc_begin2 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string81         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	27                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	1984                    # DW_AT_type
                                        # DW_AT_external
	.byte	18                      # Abbrev [18] 0x39a:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string107        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	27                      # DW_AT_decl_line
	.long	1984                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	17                      # Abbrev [17] 0x3a9:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin3           # DW_AT_low_pc
	.long	.Lfunc_end3-.Lfunc_begin3 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string78         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	79                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	552                     # DW_AT_type
                                        # DW_AT_external
	.byte	18                      # Abbrev [18] 0x3c2:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string107        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	79                      # DW_AT_decl_line
	.long	1984                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	17                      # Abbrev [17] 0x3d1:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin4           # DW_AT_low_pc
	.long	.Lfunc_end4-.Lfunc_begin4 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string79         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	83                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	552                     # DW_AT_type
                                        # DW_AT_external
	.byte	18                      # Abbrev [18] 0x3ea:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string107        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	83                      # DW_AT_decl_line
	.long	1984                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	17                      # Abbrev [17] 0x3f9:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin5           # DW_AT_low_pc
	.long	.Lfunc_end5-.Lfunc_begin5 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string82         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	87                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	68                      # DW_AT_type
                                        # DW_AT_external
	.byte	18                      # Abbrev [18] 0x412:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string107        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	87                      # DW_AT_decl_line
	.long	2241                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x421:0x40 DW_TAG_subprogram
	.quad	.Lfunc_begin6           # DW_AT_low_pc
	.long	.Lfunc_end6-.Lfunc_begin6 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string83         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	91                      # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	18                      # Abbrev [18] 0x436:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string107        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	91                      # DW_AT_decl_line
	.long	2241                    # DW_AT_type
	.byte	18                      # Abbrev [18] 0x444:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.long	.Linfo_string109        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	91                      # DW_AT_decl_line
	.long	723                     # DW_AT_type
	.byte	18                      # Abbrev [18] 0x452:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string110        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	91                      # DW_AT_decl_line
	.long	68                      # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	17                      # Abbrev [17] 0x461:0x6f DW_TAG_subprogram
	.quad	.Lfunc_begin7           # DW_AT_low_pc
	.long	.Lfunc_end7-.Lfunc_begin7 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string84         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	95                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	2046                    # DW_AT_type
                                        # DW_AT_external
	.byte	18                      # Abbrev [18] 0x47a:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string76         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	96                      # DW_AT_decl_line
	.long	2041                    # DW_AT_type
	.byte	18                      # Abbrev [18] 0x488:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string85         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	97                      # DW_AT_decl_line
	.long	125                     # DW_AT_type
	.byte	18                      # Abbrev [18] 0x496:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.long	.Linfo_string82         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	98                      # DW_AT_decl_line
	.long	2173                    # DW_AT_type
	.byte	18                      # Abbrev [18] 0x4a4:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.long	.Linfo_string86         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	99                      # DW_AT_decl_line
	.long	2151                    # DW_AT_type
	.byte	20                      # Abbrev [20] 0x4b2:0xe DW_TAG_variable
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.long	.Linfo_string111        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	106                     # DW_AT_decl_line
	.long	1984                    # DW_AT_type
	.byte	21                      # Abbrev [21] 0x4c0:0xf DW_TAG_variable
	.long	.Ldebug_loc0            # DW_AT_location
	.long	.Linfo_string107        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	108                     # DW_AT_decl_line
	.long	2046                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	17                      # Abbrev [17] 0x4d0:0x44 DW_TAG_subprogram
	.quad	.Lfunc_begin8           # DW_AT_low_pc
	.long	.Lfunc_end8-.Lfunc_begin8 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string95         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	122                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	2189                    # DW_AT_type
                                        # DW_AT_external
	.byte	18                      # Abbrev [18] 0x4e9:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string107        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	122                     # DW_AT_decl_line
	.long	2241                    # DW_AT_type
	.byte	18                      # Abbrev [18] 0x4f7:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.long	.Linfo_string92         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	122                     # DW_AT_decl_line
	.long	552                     # DW_AT_type
	.byte	18                      # Abbrev [18] 0x505:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.long	.Linfo_string112        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	122                     # DW_AT_decl_line
	.long	2246                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	17                      # Abbrev [17] 0x514:0x60 DW_TAG_subprogram
	.quad	.Lfunc_begin9           # DW_AT_low_pc
	.long	.Lfunc_end9-.Lfunc_begin9 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string96         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	126                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	2189                    # DW_AT_type
                                        # DW_AT_external
	.byte	18                      # Abbrev [18] 0x52d:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string107        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	126                     # DW_AT_decl_line
	.long	2241                    # DW_AT_type
	.byte	18                      # Abbrev [18] 0x53b:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.long	.Linfo_string92         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	126                     # DW_AT_decl_line
	.long	552                     # DW_AT_type
	.byte	18                      # Abbrev [18] 0x549:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.long	.Linfo_string112        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	126                     # DW_AT_decl_line
	.long	2246                    # DW_AT_type
	.byte	22                      # Abbrev [22] 0x557:0x1c DW_TAG_lexical_block
	.quad	.Ltmp57                 # DW_AT_low_pc
	.long	.Ltmp60-.Ltmp57         # DW_AT_high_pc
	.byte	20                      # Abbrev [20] 0x564:0xe DW_TAG_variable
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.long	.Linfo_string75         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	133                     # DW_AT_decl_line
	.long	1984                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	17                      # Abbrev [17] 0x574:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin10          # DW_AT_low_pc
	.long	.Lfunc_end10-.Lfunc_begin10 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string97         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	156                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	2189                    # DW_AT_type
                                        # DW_AT_external
	.byte	18                      # Abbrev [18] 0x58d:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string107        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	156                     # DW_AT_decl_line
	.long	2241                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	17                      # Abbrev [17] 0x59c:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin11          # DW_AT_low_pc
	.long	.Lfunc_end11-.Lfunc_begin11 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string98         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	165                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	2189                    # DW_AT_type
                                        # DW_AT_external
	.byte	18                      # Abbrev [18] 0x5b5:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string107        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	165                     # DW_AT_decl_line
	.long	2241                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	17                      # Abbrev [17] 0x5c4:0x36 DW_TAG_subprogram
	.quad	.Lfunc_begin12          # DW_AT_low_pc
	.long	.Lfunc_end12-.Lfunc_begin12 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string99         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	170                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	2189                    # DW_AT_type
                                        # DW_AT_external
	.byte	18                      # Abbrev [18] 0x5dd:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string107        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	170                     # DW_AT_decl_line
	.long	2241                    # DW_AT_type
	.byte	18                      # Abbrev [18] 0x5eb:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string110        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	170                     # DW_AT_decl_line
	.long	68                      # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	17                      # Abbrev [17] 0x5fa:0x44 DW_TAG_subprogram
	.quad	.Lfunc_begin13          # DW_AT_low_pc
	.long	.Lfunc_end13-.Lfunc_begin13 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string100        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	191                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	2189                    # DW_AT_type
                                        # DW_AT_external
	.byte	18                      # Abbrev [18] 0x613:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string107        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	191                     # DW_AT_decl_line
	.long	2241                    # DW_AT_type
	.byte	18                      # Abbrev [18] 0x621:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string110        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	191                     # DW_AT_decl_line
	.long	68                      # DW_AT_type
	.byte	20                      # Abbrev [20] 0x62f:0xe DW_TAG_variable
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.long	.Linfo_string113        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	193                     # DW_AT_decl_line
	.long	68                      # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	17                      # Abbrev [17] 0x63e:0x52 DW_TAG_subprogram
	.quad	.Lfunc_begin14          # DW_AT_low_pc
	.long	.Lfunc_end14-.Lfunc_begin14 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string101        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	215                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	2189                    # DW_AT_type
                                        # DW_AT_external
	.byte	18                      # Abbrev [18] 0x657:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string107        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	215                     # DW_AT_decl_line
	.long	2241                    # DW_AT_type
	.byte	20                      # Abbrev [20] 0x665:0xe DW_TAG_variable
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	111
	.long	.Linfo_string114        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	216                     # DW_AT_decl_line
	.long	2189                    # DW_AT_type
	.byte	22                      # Abbrev [22] 0x673:0x1c DW_TAG_lexical_block
	.quad	.Ltmp104                # DW_AT_low_pc
	.long	.Ltmp107-.Ltmp104       # DW_AT_high_pc
	.byte	20                      # Abbrev [20] 0x680:0xe DW_TAG_variable
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.long	.Linfo_string40         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.byte	228                     # DW_AT_decl_line
	.long	68                      # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	23                      # Abbrev [23] 0x690:0x2a DW_TAG_subprogram
	.quad	.Lfunc_begin15          # DW_AT_low_pc
	.long	.Lfunc_end15-.Lfunc_begin15 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string102        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.short	256                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	552                     # DW_AT_type
                                        # DW_AT_external
	.byte	24                      # Abbrev [24] 0x6aa:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	124
	.long	.Linfo_string108        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.short	256                     # DW_AT_decl_line
	.long	552                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	23                      # Abbrev [23] 0x6ba:0x2a DW_TAG_subprogram
	.quad	.Lfunc_begin16          # DW_AT_low_pc
	.long	.Lfunc_end16-.Lfunc_begin16 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string87         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.short	260                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	552                     # DW_AT_type
                                        # DW_AT_external
	.byte	24                      # Abbrev [24] 0x6d4:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string107        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.short	260                     # DW_AT_decl_line
	.long	2241                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	23                      # Abbrev [23] 0x6e4:0x2a DW_TAG_subprogram
	.quad	.Lfunc_begin17          # DW_AT_low_pc
	.long	.Lfunc_end17-.Lfunc_begin17 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string103        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.short	264                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	2189                    # DW_AT_type
                                        # DW_AT_external
	.byte	24                      # Abbrev [24] 0x6fe:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string107        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.short	264                     # DW_AT_decl_line
	.long	2241                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0x70e:0x1a DW_TAG_subprogram
	.quad	.Lfunc_begin18          # DW_AT_low_pc
	.long	.Lfunc_end18-.Lfunc_begin18 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string104        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.short	268                     # DW_AT_decl_line
	.long	2196                    # DW_AT_type
                                        # DW_AT_external
	.byte	23                      # Abbrev [23] 0x728:0x2a DW_TAG_subprogram
	.quad	.Lfunc_begin19          # DW_AT_low_pc
	.long	.Lfunc_end19-.Lfunc_begin19 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string94         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.short	272                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	2196                    # DW_AT_type
                                        # DW_AT_external
	.byte	24                      # Abbrev [24] 0x742:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.long	.Linfo_string107        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.short	272                     # DW_AT_decl_line
	.long	2241                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	23                      # Abbrev [23] 0x752:0x39 DW_TAG_subprogram
	.quad	.Lfunc_begin20          # DW_AT_low_pc
	.long	.Lfunc_end20-.Lfunc_begin20 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string105        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.short	276                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	2041                    # DW_AT_type
                                        # DW_AT_external
	.byte	24                      # Abbrev [24] 0x76c:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string115        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.short	276                     # DW_AT_decl_line
	.long	2041                    # DW_AT_type
	.byte	24                      # Abbrev [24] 0x77b:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.long	.Linfo_string78         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.short	276                     # DW_AT_decl_line
	.long	552                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	26                      # Abbrev [26] 0x78b:0x35 DW_TAG_subprogram
	.quad	.Lfunc_begin21          # DW_AT_low_pc
	.long	.Lfunc_end21-.Lfunc_begin21 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string106        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.short	297                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	24                      # Abbrev [24] 0x7a1:0xf DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string91         # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.short	297                     # DW_AT_decl_line
	.long	2196                    # DW_AT_type
	.byte	27                      # Abbrev [27] 0x7b0:0xf DW_TAG_variable
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	94
	.long	.Linfo_string115        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.short	298                     # DW_AT_decl_line
	.long	2262                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	8                       # Abbrev [8] 0x7c0:0x39 DW_TAG_structure_type
	.long	.Linfo_string75         # DW_AT_name
	.byte	24                      # DW_AT_byte_size
	.byte	3                       # DW_AT_decl_file
	.byte	62                      # DW_AT_decl_line
	.byte	5                       # Abbrev [5] 0x7c8:0xc DW_TAG_member
	.long	.Linfo_string76         # DW_AT_name
	.long	2041                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	63                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0x7d4:0xc DW_TAG_member
	.long	.Linfo_string77         # DW_AT_name
	.long	552                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	64                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0x7e0:0xc DW_TAG_member
	.long	.Linfo_string78         # DW_AT_name
	.long	552                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	65                      # DW_AT_decl_line
	.byte	12                      # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0x7ec:0xc DW_TAG_member
	.long	.Linfo_string79         # DW_AT_name
	.long	552                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	66                      # DW_AT_decl_line
	.byte	16                      # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	7                       # Abbrev [7] 0x7f9:0x5 DW_TAG_pointer_type
	.long	332                     # DW_AT_type
	.byte	8                       # Abbrev [8] 0x7fe:0x69 DW_TAG_structure_type
	.long	.Linfo_string84         # DW_AT_name
	.byte	72                      # DW_AT_byte_size
	.byte	3                       # DW_AT_decl_file
	.byte	75                      # DW_AT_decl_line
	.byte	5                       # Abbrev [5] 0x806:0xc DW_TAG_member
	.long	.Linfo_string85         # DW_AT_name
	.long	125                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	76                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0x812:0xc DW_TAG_member
	.long	.Linfo_string86         # DW_AT_name
	.long	2151                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	77                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0x81e:0xc DW_TAG_member
	.long	.Linfo_string82         # DW_AT_name
	.long	2173                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	78                      # DW_AT_decl_line
	.byte	16                      # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0x82a:0xc DW_TAG_member
	.long	.Linfo_string75         # DW_AT_name
	.long	1984                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	79                      # DW_AT_decl_line
	.byte	24                      # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0x836:0xc DW_TAG_member
	.long	.Linfo_string87         # DW_AT_name
	.long	552                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	80                      # DW_AT_decl_line
	.byte	48                      # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0x842:0xc DW_TAG_member
	.long	.Linfo_string88         # DW_AT_name
	.long	552                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	81                      # DW_AT_decl_line
	.byte	52                      # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0x84e:0xc DW_TAG_member
	.long	.Linfo_string89         # DW_AT_name
	.long	2189                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	82                      # DW_AT_decl_line
	.byte	56                      # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0x85a:0xc DW_TAG_member
	.long	.Linfo_string91         # DW_AT_name
	.long	2196                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	83                      # DW_AT_decl_line
	.byte	60                      # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	7                       # Abbrev [7] 0x867:0x5 DW_TAG_pointer_type
	.long	2156                    # DW_AT_type
	.byte	7                       # Abbrev [7] 0x86c:0x5 DW_TAG_pointer_type
	.long	2161                    # DW_AT_type
	.byte	28                      # Abbrev [28] 0x871:0xc DW_TAG_subroutine_type
                                        # DW_AT_prototyped
	.byte	29                      # Abbrev [29] 0x872:0x5 DW_TAG_formal_parameter
	.long	125                     # DW_AT_type
	.byte	29                      # Abbrev [29] 0x877:0x5 DW_TAG_formal_parameter
	.long	68                      # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	7                       # Abbrev [7] 0x87d:0x5 DW_TAG_pointer_type
	.long	2178                    # DW_AT_type
	.byte	30                      # Abbrev [30] 0x882:0xb DW_TAG_subroutine_type
	.long	68                      # DW_AT_type
                                        # DW_AT_prototyped
	.byte	29                      # Abbrev [29] 0x887:0x5 DW_TAG_formal_parameter
	.long	125                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	14                      # Abbrev [14] 0x88d:0x7 DW_TAG_base_type
	.long	.Linfo_string90         # DW_AT_name
	.byte	2                       # DW_AT_encoding
	.byte	1                       # DW_AT_byte_size
	.byte	8                       # Abbrev [8] 0x894:0x2d DW_TAG_structure_type
	.long	.Linfo_string94         # DW_AT_name
	.byte	12                      # DW_AT_byte_size
	.byte	3                       # DW_AT_decl_file
	.byte	69                      # DW_AT_decl_line
	.byte	5                       # Abbrev [5] 0x89c:0xc DW_TAG_member
	.long	.Linfo_string79         # DW_AT_name
	.long	552                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	70                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0x8a8:0xc DW_TAG_member
	.long	.Linfo_string92         # DW_AT_name
	.long	552                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	71                      # DW_AT_decl_line
	.byte	4                       # DW_AT_data_member_location
	.byte	5                       # Abbrev [5] 0x8b4:0xc DW_TAG_member
	.long	.Linfo_string93         # DW_AT_name
	.long	552                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	72                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	7                       # Abbrev [7] 0x8c1:0x5 DW_TAG_pointer_type
	.long	2046                    # DW_AT_type
	.byte	7                       # Abbrev [7] 0x8c6:0x5 DW_TAG_pointer_type
	.long	2251                    # DW_AT_type
	.byte	30                      # Abbrev [30] 0x8cb:0xb DW_TAG_subroutine_type
	.long	552                     # DW_AT_type
                                        # DW_AT_prototyped
	.byte	29                      # Abbrev [29] 0x8d0:0x5 DW_TAG_formal_parameter
	.long	552                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	31                      # Abbrev [31] 0x8d6:0xc DW_TAG_array_type
	.long	332                     # DW_AT_type
	.byte	32                      # Abbrev [32] 0x8db:0x6 DW_TAG_subrange_type
	.long	2274                    # DW_AT_type
	.byte	2                       # DW_AT_count
	.byte	0                       # End Of Children Mark
	.byte	33                      # Abbrev [33] 0x8e2:0x7 DW_TAG_base_type
	.long	.Linfo_string116        # DW_AT_name
	.byte	8                       # DW_AT_byte_size
	.byte	7                       # DW_AT_encoding
	.byte	0                       # End Of Children Mark
.Ldebug_info_end0:
	.section	.debug_macinfo,"",@progbits
	.byte	0                       # End Of Macro List Mark

	.ident	"clang version 8.0.0-3~ubuntu18.04.1 (tags/RELEASE_800/final)"
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym scan_context
	.addrsig_sym consume
	.addrsig_sym __ctype_b_loc
	.addrsig_sym scan
	.addrsig_sym token
	.addrsig_sym token_col
	.addrsig_sym getval
	.addrsig_sym do_action
	.addrsig_sym __assert_fail
	.addrsig_sym peek
	.addrsig_sym expect
	.addrsig_sym parse_expr
	.addrsig_sym parse_alt
	.addrsig_sym parse_cat
	.addrsig_sym parse_factor
	.addrsig_sym is_symbol
	.addrsig_sym lookahead
	.addrsig_sym nullperr
	.addrsig_sym lexeme_for
	.addrsig_sym fprintf
	.addrsig_sym stderr
	.addrsig_sym NULLRVAL
	.section	.debug_line,"",@progbits
.Lline_table_start0:
