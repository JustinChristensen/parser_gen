	.text
	.file	"print_ast.c"
	.file	1 "/home/wroathe/compilers/src/auto" "./result_types.h"
	.file	2 "/home/wroathe/compilers/src/auto" "./ast.h"
	.globl	print_expr              # -- Begin function print_expr
	.p2align	4, 0x90
	.type	print_expr,@function
print_expr:                             # @print_expr
.Lfunc_begin0:
	.file	3 "/home/wroathe/compilers/src/auto" "print_ast.c"
	.loc	3 12 0                  # print_ast.c:12:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	movq	%rdi, -8(%rbp)
.Ltmp0:
	.loc	3 15 18 prologue_end    # print_ast.c:15:18
	movl	$5000, %edi             # imm = 0x1388
	movl	$8, %esi
	callq	calloc
	.loc	3 15 16 is_stmt 0       # print_ast.c:15:16
	movq	%rax, -24(%rbp)
	.loc	3 15 11                 # print_ast.c:15:11
	movq	%rax, -16(%rbp)
.Ltmp1:
	.loc	3 16 5 is_stmt 1        # print_ast.c:16:5
	cmpq	$0, -16(%rbp)
.Ltmp2:
	.loc	3 16 5 is_stmt 0        # print_ast.c:16:5
	je	.LBB0_2
# %bb.1:
	jmp	.LBB0_3
.LBB0_2:
.Ltmp3:
	.loc	3 16 5                  # print_ast.c:16:5
	movabsq	$.L.str, %rdi
	movabsq	$.L.str.1, %rsi
	movl	$16, %edx
	movabsq	$.L__PRETTY_FUNCTION__.print_expr, %rcx
	callq	__assert_fail
.Ltmp4:
.LBB0_3:
	.loc	3 18 13 is_stmt 1       # print_ast.c:18:13
	movq	-8(%rbp), %rax
	.loc	3 18 8 is_stmt 0        # print_ast.c:18:8
	movq	-24(%rbp), %rcx
	movq	%rcx, %rdx
	addq	$8, %rdx
	movq	%rdx, -24(%rbp)
	.loc	3 18 11                 # print_ast.c:18:11
	movq	%rax, (%rcx)
	.loc	3 20 9 is_stmt 1        # print_ast.c:20:9
	movl	$0, -28(%rbp)
.LBB0_4:                                # =>This Inner Loop Header: Depth=1
	.loc	3 21 12                 # print_ast.c:21:12
	movq	-24(%rbp), %rax
	.loc	3 21 15 is_stmt 0       # print_ast.c:21:15
	cmpq	-16(%rbp), %rax
	.loc	3 21 5                  # print_ast.c:21:5
	jbe	.LBB0_20
# %bb.5:                                #   in Loop: Header=BB0_4 Depth=1
.Ltmp5:
	.loc	3 22 17 is_stmt 1       # print_ast.c:22:17
	movq	-24(%rbp), %rax
	movq	%rax, %rcx
	addq	$-8, %rcx
	movq	%rcx, -24(%rbp)
	.loc	3 22 16 is_stmt 0       # print_ast.c:22:16
	movq	-8(%rax), %rax
	.loc	3 22 14                 # print_ast.c:22:14
	movq	%rax, -8(%rbp)
.Ltmp6:
	.loc	3 24 18 is_stmt 1       # print_ast.c:24:18
	cmpq	$0, -8(%rbp)
.Ltmp7:
	.loc	3 24 13 is_stmt 0       # print_ast.c:24:13
	jne	.LBB0_7
# %bb.6:                                #   in Loop: Header=BB0_4 Depth=1
.Ltmp8:
	.loc	3 25 25 is_stmt 1       # print_ast.c:25:25
	movl	-28(%rbp), %eax
	addl	$-1, %eax
	movl	%eax, -28(%rbp)
	.loc	3 26 9                  # print_ast.c:26:9
	jmp	.LBB0_19
.Ltmp9:
.LBB0_7:                                #   in Loop: Header=BB0_4 Depth=1
	.loc	3 27 21                 # print_ast.c:27:21
	movq	-8(%rbp), %rax
	.loc	3 27 27 is_stmt 0       # print_ast.c:27:27
	movl	(%rax), %ecx
	movl	%ecx, %eax
	movq	%rax, %rdx
	subq	$9, %rdx
	movq	%rax, -40(%rbp)         # 8-byte Spill
	movq	%rdx, -48(%rbp)         # 8-byte Spill
	.loc	3 27 13                 # print_ast.c:27:13
	ja	.LBB0_18
# %bb.21:                               #   in Loop: Header=BB0_4 Depth=1
	.loc	3 0 13                  # print_ast.c:0:13
	movq	-40(%rbp), %rax         # 8-byte Reload
	movq	.LJTI0_0(,%rax,8), %rcx
	jmpq	*%rcx
.LBB0_8:                                #   in Loop: Header=BB0_4 Depth=1
.Ltmp10:
	.loc	3 29 29 is_stmt 1       # print_ast.c:29:29
	movq	stderr, %rdi
	.loc	3 29 21 is_stmt 0       # print_ast.c:29:21
	movabsq	$.L.str.2, %rsi
	movb	$0, %al
	callq	fprintf
	movl	%eax, -52(%rbp)         # 4-byte Spill
	.loc	3 30 21 is_stmt 1       # print_ast.c:30:21
	jmp	.LBB0_18
.LBB0_9:                                #   in Loop: Header=BB0_4 Depth=1
	.loc	3 32 28                 # print_ast.c:32:28
	movl	-28(%rbp), %edi
	.loc	3 32 21 is_stmt 0       # print_ast.c:32:21
	callq	indent
	.loc	3 33 21 is_stmt 1       # print_ast.c:33:21
	movabsq	$.L.str.3, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -56(%rbp)         # 4-byte Spill
	.loc	3 34 21                 # print_ast.c:34:21
	jmp	.LBB0_18
.LBB0_10:                               #   in Loop: Header=BB0_4 Depth=1
	.loc	3 36 28                 # print_ast.c:36:28
	movl	-28(%rbp), %edi
	.loc	3 36 21 is_stmt 0       # print_ast.c:36:21
	callq	indent
	.loc	3 37 21 is_stmt 1       # print_ast.c:37:21
	movabsq	$.L.str.4, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -60(%rbp)         # 4-byte Spill
	.loc	3 38 21                 # print_ast.c:38:21
	jmp	.LBB0_18
.LBB0_11:                               #   in Loop: Header=BB0_4 Depth=1
	.loc	3 40 28                 # print_ast.c:40:28
	movl	-28(%rbp), %edi
	.loc	3 40 21 is_stmt 0       # print_ast.c:40:21
	callq	indent
	.loc	3 41 21 is_stmt 1       # print_ast.c:41:21
	movabsq	$.L.str.5, %rdi
	movb	$0, %al
	callq	printf
	.loc	3 42 33                 # print_ast.c:42:33
	movl	-28(%rbp), %ecx
	addl	$1, %ecx
	movl	%ecx, -28(%rbp)
	.loc	3 43 24                 # print_ast.c:43:24
	movq	-24(%rbp), %rdi
	movq	%rdi, %rdx
	addq	$8, %rdx
	movq	%rdx, -24(%rbp)
	.loc	3 43 27 is_stmt 0       # print_ast.c:43:27
	movq	$0, (%rdi)
	.loc	3 44 29 is_stmt 1       # print_ast.c:44:29
	movq	-8(%rbp), %rdx
	.loc	3 44 35 is_stmt 0       # print_ast.c:44:35
	movq	16(%rdx), %rdx
	.loc	3 44 24                 # print_ast.c:44:24
	movq	-24(%rbp), %rdi
	movq	%rdi, %rsi
	addq	$8, %rsi
	movq	%rsi, -24(%rbp)
	.loc	3 44 27                 # print_ast.c:44:27
	movq	%rdx, (%rdi)
	.loc	3 45 29 is_stmt 1       # print_ast.c:45:29
	movq	-8(%rbp), %rdx
	.loc	3 45 35 is_stmt 0       # print_ast.c:45:35
	movq	8(%rdx), %rdx
	.loc	3 45 24                 # print_ast.c:45:24
	movq	-24(%rbp), %rsi
	movq	%rsi, %rdi
	addq	$8, %rdi
	movq	%rdi, -24(%rbp)
	.loc	3 45 27                 # print_ast.c:45:27
	movq	%rdx, (%rsi)
	movl	%eax, -64(%rbp)         # 4-byte Spill
	.loc	3 46 21 is_stmt 1       # print_ast.c:46:21
	jmp	.LBB0_18
.LBB0_12:                               #   in Loop: Header=BB0_4 Depth=1
	.loc	3 48 28                 # print_ast.c:48:28
	movl	-28(%rbp), %edi
	.loc	3 48 21 is_stmt 0       # print_ast.c:48:21
	callq	indent
	.loc	3 49 21 is_stmt 1       # print_ast.c:49:21
	movabsq	$.L.str.6, %rdi
	movb	$0, %al
	callq	printf
	.loc	3 50 33                 # print_ast.c:50:33
	movl	-28(%rbp), %ecx
	addl	$1, %ecx
	movl	%ecx, -28(%rbp)
	.loc	3 51 24                 # print_ast.c:51:24
	movq	-24(%rbp), %rdi
	movq	%rdi, %rdx
	addq	$8, %rdx
	movq	%rdx, -24(%rbp)
	.loc	3 51 27 is_stmt 0       # print_ast.c:51:27
	movq	$0, (%rdi)
	.loc	3 52 29 is_stmt 1       # print_ast.c:52:29
	movq	-8(%rbp), %rdx
	.loc	3 52 35 is_stmt 0       # print_ast.c:52:35
	movq	16(%rdx), %rdx
	.loc	3 52 24                 # print_ast.c:52:24
	movq	-24(%rbp), %rdi
	movq	%rdi, %rsi
	addq	$8, %rsi
	movq	%rsi, -24(%rbp)
	.loc	3 52 27                 # print_ast.c:52:27
	movq	%rdx, (%rdi)
	.loc	3 53 29 is_stmt 1       # print_ast.c:53:29
	movq	-8(%rbp), %rdx
	.loc	3 53 35 is_stmt 0       # print_ast.c:53:35
	movq	8(%rdx), %rdx
	.loc	3 53 24                 # print_ast.c:53:24
	movq	-24(%rbp), %rsi
	movq	%rsi, %rdi
	addq	$8, %rdi
	movq	%rdi, -24(%rbp)
	.loc	3 53 27                 # print_ast.c:53:27
	movq	%rdx, (%rsi)
	movl	%eax, -68(%rbp)         # 4-byte Spill
	.loc	3 54 21 is_stmt 1       # print_ast.c:54:21
	jmp	.LBB0_18
.LBB0_13:                               #   in Loop: Header=BB0_4 Depth=1
	.loc	3 56 28                 # print_ast.c:56:28
	movl	-28(%rbp), %edi
	.loc	3 56 21 is_stmt 0       # print_ast.c:56:21
	callq	indent
	.loc	3 57 21 is_stmt 1       # print_ast.c:57:21
	movabsq	$.L.str.7, %rdi
	movb	$0, %al
	callq	printf
	.loc	3 58 33                 # print_ast.c:58:33
	movl	-28(%rbp), %ecx
	addl	$1, %ecx
	movl	%ecx, -28(%rbp)
	.loc	3 59 24                 # print_ast.c:59:24
	movq	-24(%rbp), %rdi
	movq	%rdi, %rdx
	addq	$8, %rdx
	movq	%rdx, -24(%rbp)
	.loc	3 59 27 is_stmt 0       # print_ast.c:59:27
	movq	$0, (%rdi)
	.loc	3 60 29 is_stmt 1       # print_ast.c:60:29
	movq	-8(%rbp), %rdx
	.loc	3 60 35 is_stmt 0       # print_ast.c:60:35
	movq	8(%rdx), %rdx
	.loc	3 60 24                 # print_ast.c:60:24
	movq	-24(%rbp), %rdi
	movq	%rdi, %rsi
	addq	$8, %rsi
	movq	%rsi, -24(%rbp)
	.loc	3 60 27                 # print_ast.c:60:27
	movq	%rdx, (%rdi)
	movl	%eax, -72(%rbp)         # 4-byte Spill
	.loc	3 61 21 is_stmt 1       # print_ast.c:61:21
	jmp	.LBB0_18
.LBB0_14:                               #   in Loop: Header=BB0_4 Depth=1
	.loc	3 63 28                 # print_ast.c:63:28
	movl	-28(%rbp), %edi
	.loc	3 63 21 is_stmt 0       # print_ast.c:63:21
	callq	indent
	.loc	3 64 21 is_stmt 1       # print_ast.c:64:21
	movabsq	$.L.str.6, %rdi
	movb	$0, %al
	callq	printf
	.loc	3 65 33                 # print_ast.c:65:33
	movl	-28(%rbp), %ecx
	addl	$1, %ecx
	movl	%ecx, -28(%rbp)
	.loc	3 66 24                 # print_ast.c:66:24
	movq	-24(%rbp), %rdi
	movq	%rdi, %rdx
	addq	$8, %rdx
	movq	%rdx, -24(%rbp)
	.loc	3 66 27 is_stmt 0       # print_ast.c:66:27
	movq	$0, (%rdi)
	.loc	3 67 29 is_stmt 1       # print_ast.c:67:29
	movq	-8(%rbp), %rdx
	.loc	3 67 35 is_stmt 0       # print_ast.c:67:35
	movq	8(%rdx), %rdx
	.loc	3 67 24                 # print_ast.c:67:24
	movq	-24(%rbp), %rdi
	movq	%rdi, %rsi
	addq	$8, %rsi
	movq	%rsi, -24(%rbp)
	.loc	3 67 27                 # print_ast.c:67:27
	movq	%rdx, (%rdi)
	movl	%eax, -76(%rbp)         # 4-byte Spill
	.loc	3 68 21 is_stmt 1       # print_ast.c:68:21
	jmp	.LBB0_18
.LBB0_15:                               #   in Loop: Header=BB0_4 Depth=1
	.loc	3 70 28                 # print_ast.c:70:28
	movl	-28(%rbp), %edi
	.loc	3 70 21 is_stmt 0       # print_ast.c:70:21
	callq	indent
	.loc	3 71 21 is_stmt 1       # print_ast.c:71:21
	movabsq	$.L.str.8, %rdi
	movb	$0, %al
	callq	printf
	.loc	3 72 33                 # print_ast.c:72:33
	movl	-28(%rbp), %ecx
	addl	$1, %ecx
	movl	%ecx, -28(%rbp)
	.loc	3 73 24                 # print_ast.c:73:24
	movq	-24(%rbp), %rdi
	movq	%rdi, %rdx
	addq	$8, %rdx
	movq	%rdx, -24(%rbp)
	.loc	3 73 27 is_stmt 0       # print_ast.c:73:27
	movq	$0, (%rdi)
	.loc	3 74 29 is_stmt 1       # print_ast.c:74:29
	movq	-8(%rbp), %rdx
	.loc	3 74 35 is_stmt 0       # print_ast.c:74:35
	movq	8(%rdx), %rdx
	.loc	3 74 24                 # print_ast.c:74:24
	movq	-24(%rbp), %rdi
	movq	%rdi, %rsi
	addq	$8, %rsi
	movq	%rsi, -24(%rbp)
	.loc	3 74 27                 # print_ast.c:74:27
	movq	%rdx, (%rdi)
	movl	%eax, -80(%rbp)         # 4-byte Spill
	.loc	3 75 21 is_stmt 1       # print_ast.c:75:21
	jmp	.LBB0_18
.LBB0_16:                               #   in Loop: Header=BB0_4 Depth=1
	.loc	3 77 28                 # print_ast.c:77:28
	movl	-28(%rbp), %edi
	.loc	3 77 21 is_stmt 0       # print_ast.c:77:21
	callq	indent
	.loc	3 78 21 is_stmt 1       # print_ast.c:78:21
	movabsq	$.L.str.9, %rdi
	movb	$0, %al
	callq	printf
	.loc	3 79 33                 # print_ast.c:79:33
	movl	-28(%rbp), %ecx
	addl	$1, %ecx
	movl	%ecx, -28(%rbp)
	.loc	3 80 24                 # print_ast.c:80:24
	movq	-24(%rbp), %rdi
	movq	%rdi, %rdx
	addq	$8, %rdx
	movq	%rdx, -24(%rbp)
	.loc	3 80 27 is_stmt 0       # print_ast.c:80:27
	movq	$0, (%rdi)
	.loc	3 81 29 is_stmt 1       # print_ast.c:81:29
	movq	-8(%rbp), %rdx
	.loc	3 81 35 is_stmt 0       # print_ast.c:81:35
	movq	8(%rdx), %rdx
	.loc	3 81 24                 # print_ast.c:81:24
	movq	-24(%rbp), %rdi
	movq	%rdi, %rsi
	addq	$8, %rsi
	movq	%rsi, -24(%rbp)
	.loc	3 81 27                 # print_ast.c:81:27
	movq	%rdx, (%rdi)
	movl	%eax, -84(%rbp)         # 4-byte Spill
	.loc	3 82 21 is_stmt 1       # print_ast.c:82:21
	jmp	.LBB0_18
.LBB0_17:                               #   in Loop: Header=BB0_4 Depth=1
	.loc	3 84 28                 # print_ast.c:84:28
	movl	-28(%rbp), %edi
	.loc	3 84 21 is_stmt 0       # print_ast.c:84:21
	callq	indent
	.loc	3 85 34 is_stmt 1       # print_ast.c:85:34
	movq	-8(%rbp), %rax
	movsbl	8(%rax), %esi
	.loc	3 85 21 is_stmt 0       # print_ast.c:85:21
	movabsq	$.L.str.10, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -88(%rbp)         # 4-byte Spill
.Ltmp11:
.LBB0_18:                               #   in Loop: Header=BB0_4 Depth=1
	.loc	3 89 13 is_stmt 1       # print_ast.c:89:13
	movabsq	$.L.str.11, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -92(%rbp)         # 4-byte Spill
.Ltmp12:
.LBB0_19:                               #   in Loop: Header=BB0_4 Depth=1
	.loc	3 21 5                  # print_ast.c:21:5
	jmp	.LBB0_4
.LBB0_20:
	.loc	3 93 10                 # print_ast.c:93:10
	movq	-16(%rbp), %rax
	.loc	3 93 5 is_stmt 0        # print_ast.c:93:5
	movq	%rax, %rdi
	callq	free
	.loc	3 94 16 is_stmt 1       # print_ast.c:94:16
	movq	$0, -24(%rbp)
	.loc	3 94 11 is_stmt 0       # print_ast.c:94:11
	movq	$0, -16(%rbp)
	.loc	3 95 1 is_stmt 1        # print_ast.c:95:1
	addq	$96, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp13:
.Lfunc_end0:
	.size	print_expr, .Lfunc_end0-print_expr
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.p2align	3
.LJTI0_0:
	.quad	.LBB0_8
	.quad	.LBB0_9
	.quad	.LBB0_10
	.quad	.LBB0_11
	.quad	.LBB0_12
	.quad	.LBB0_13
	.quad	.LBB0_14
	.quad	.LBB0_15
	.quad	.LBB0_16
	.quad	.LBB0_17
                                        # -- End function
	.text
	.p2align	4, 0x90         # -- Begin function indent
	.type	indent,@function
indent:                                 # @indent
.Lfunc_begin1:
	.loc	3 8 0                   # print_ast.c:8:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
.LBB1_1:                                # =>This Inner Loop Header: Depth=1
.Ltmp14:
	.loc	3 9 13 prologue_end     # print_ast.c:9:13
	movl	-4(%rbp), %eax
	movl	%eax, %ecx
	addl	$-1, %ecx
	movl	%ecx, -4(%rbp)
	.loc	3 9 16 is_stmt 0        # print_ast.c:9:16
	cmpl	$0, %eax
	.loc	3 9 5                   # print_ast.c:9:5
	jle	.LBB1_3
# %bb.2:                                #   in Loop: Header=BB1_1 Depth=1
	.loc	3 9 21                  # print_ast.c:9:21
	movabsq	$.L.str.20, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -8(%rbp)          # 4-byte Spill
	.loc	3 9 5                   # print_ast.c:9:5
	jmp	.LBB1_1
.LBB1_3:
	.loc	3 10 1 is_stmt 1        # print_ast.c:10:1
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp15:
.Lfunc_end1:
	.size	indent, .Lfunc_end1-indent
	.cfi_endproc
                                        # -- End function
	.globl	print_expr_table        # -- Begin function print_expr_table
	.p2align	4, 0x90
	.type	print_expr_table,@function
print_expr_table:                       # @print_expr_table
.Lfunc_begin2:
	.loc	3 97 0                  # print_ast.c:97:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$80, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
.LBB2_1:                                # =>This Inner Loop Header: Depth=1
.Ltmp16:
	.loc	3 98 12 prologue_end    # print_ast.c:98:12
	movq	-8(%rbp), %rax
	.loc	3 98 18 is_stmt 0       # print_ast.c:98:18
	cmpq	-16(%rbp), %rax
	.loc	3 98 5                  # print_ast.c:98:5
	je	.LBB2_14
# %bb.2:                                #   in Loop: Header=BB2_1 Depth=1
.Ltmp17:
	.loc	3 99 25 is_stmt 1       # print_ast.c:99:25
	movq	-8(%rbp), %rsi
	.loc	3 99 9 is_stmt 0        # print_ast.c:99:9
	movl	$.L.str.12, %edi
	xorl	%eax, %eax
	movb	%al, %cl
	movb	%cl, %al
	callq	printf
	.loc	3 100 17 is_stmt 1      # print_ast.c:100:17
	movq	-8(%rbp), %rsi
	.loc	3 100 24 is_stmt 0      # print_ast.c:100:24
	movl	(%rsi), %edx
	movl	%edx, %esi
	movq	%rsi, %rdi
	subq	$9, %rdi
	movl	%eax, -20(%rbp)         # 4-byte Spill
	movq	%rsi, -32(%rbp)         # 8-byte Spill
	movq	%rdi, -40(%rbp)         # 8-byte Spill
	.loc	3 100 9                 # print_ast.c:100:9
	ja	.LBB2_13
# %bb.15:                               #   in Loop: Header=BB2_1 Depth=1
	.loc	3 0 9                   # print_ast.c:0:9
	movq	-32(%rbp), %rax         # 8-byte Reload
	movq	.LJTI2_0(,%rax,8), %rcx
	jmpq	*%rcx
.LBB2_3:                                #   in Loop: Header=BB2_1 Depth=1
.Ltmp18:
	.loc	3 102 17 is_stmt 1      # print_ast.c:102:17
	jmp	.LBB2_13
.LBB2_4:                                #   in Loop: Header=BB2_1 Depth=1
	.loc	3 104 17                # print_ast.c:104:17
	movabsq	$.L.str.3, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -44(%rbp)         # 4-byte Spill
	.loc	3 105 17                # print_ast.c:105:17
	jmp	.LBB2_13
.LBB2_5:                                #   in Loop: Header=BB2_1 Depth=1
	.loc	3 107 17                # print_ast.c:107:17
	movabsq	$.L.str.4, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -48(%rbp)         # 4-byte Spill
	.loc	3 108 17                # print_ast.c:108:17
	jmp	.LBB2_13
.LBB2_6:                                #   in Loop: Header=BB2_1 Depth=1
	.loc	3 110 35                # print_ast.c:110:35
	movq	-8(%rbp), %rax
	.loc	3 110 42 is_stmt 0      # print_ast.c:110:42
	movq	8(%rax), %rsi
	.loc	3 110 49                # print_ast.c:110:49
	movq	-8(%rbp), %rax
	.loc	3 110 56                # print_ast.c:110:56
	movq	16(%rax), %rdx
	.loc	3 110 17                # print_ast.c:110:17
	movabsq	$.L.str.13, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -52(%rbp)         # 4-byte Spill
	.loc	3 111 17 is_stmt 1      # print_ast.c:111:17
	jmp	.LBB2_13
.LBB2_7:                                #   in Loop: Header=BB2_1 Depth=1
	.loc	3 113 35                # print_ast.c:113:35
	movq	-8(%rbp), %rax
	.loc	3 113 42 is_stmt 0      # print_ast.c:113:42
	movq	8(%rax), %rsi
	.loc	3 113 49                # print_ast.c:113:49
	movq	-8(%rbp), %rax
	.loc	3 113 56                # print_ast.c:113:56
	movq	16(%rax), %rdx
	.loc	3 113 17                # print_ast.c:113:17
	movabsq	$.L.str.14, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -56(%rbp)         # 4-byte Spill
	.loc	3 114 17 is_stmt 1      # print_ast.c:114:17
	jmp	.LBB2_13
.LBB2_8:                                #   in Loop: Header=BB2_1 Depth=1
	.loc	3 116 32                # print_ast.c:116:32
	movq	-8(%rbp), %rax
	.loc	3 116 39 is_stmt 0      # print_ast.c:116:39
	movq	8(%rax), %rsi
	.loc	3 116 17                # print_ast.c:116:17
	movabsq	$.L.str.15, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -60(%rbp)         # 4-byte Spill
	.loc	3 117 17 is_stmt 1      # print_ast.c:117:17
	jmp	.LBB2_13
.LBB2_9:                                #   in Loop: Header=BB2_1 Depth=1
	.loc	3 119 32                # print_ast.c:119:32
	movq	-8(%rbp), %rax
	.loc	3 119 39 is_stmt 0      # print_ast.c:119:39
	movq	8(%rax), %rsi
	.loc	3 119 17                # print_ast.c:119:17
	movabsq	$.L.str.16, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -64(%rbp)         # 4-byte Spill
	.loc	3 120 17 is_stmt 1      # print_ast.c:120:17
	jmp	.LBB2_13
.LBB2_10:                               #   in Loop: Header=BB2_1 Depth=1
	.loc	3 122 32                # print_ast.c:122:32
	movq	-8(%rbp), %rax
	.loc	3 122 39 is_stmt 0      # print_ast.c:122:39
	movq	8(%rax), %rsi
	.loc	3 122 17                # print_ast.c:122:17
	movabsq	$.L.str.17, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -68(%rbp)         # 4-byte Spill
	.loc	3 123 17 is_stmt 1      # print_ast.c:123:17
	jmp	.LBB2_13
.LBB2_11:                               #   in Loop: Header=BB2_1 Depth=1
	.loc	3 125 33                # print_ast.c:125:33
	movq	-8(%rbp), %rax
	.loc	3 125 40 is_stmt 0      # print_ast.c:125:40
	movq	8(%rax), %rsi
	.loc	3 125 17                # print_ast.c:125:17
	movabsq	$.L.str.18, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -72(%rbp)         # 4-byte Spill
	.loc	3 126 17 is_stmt 1      # print_ast.c:126:17
	jmp	.LBB2_13
.LBB2_12:                               #   in Loop: Header=BB2_1 Depth=1
	.loc	3 128 30                # print_ast.c:128:30
	movq	-8(%rbp), %rax
	movsbl	8(%rax), %esi
	.loc	3 128 17 is_stmt 0      # print_ast.c:128:17
	movabsq	$.L.str.10, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -76(%rbp)         # 4-byte Spill
.Ltmp19:
.LBB2_13:                               #   in Loop: Header=BB2_1 Depth=1
	.loc	3 132 9 is_stmt 1       # print_ast.c:132:9
	movabsq	$.L.str.19, %rdi
	movb	$0, %al
	callq	printf
	.loc	3 133 14                # print_ast.c:133:14
	movq	-8(%rbp), %rdi
	addq	$24, %rdi
	movq	%rdi, -8(%rbp)
	movl	%eax, -80(%rbp)         # 4-byte Spill
.Ltmp20:
	.loc	3 98 5                  # print_ast.c:98:5
	jmp	.LBB2_1
.LBB2_14:
	.loc	3 135 1                 # print_ast.c:135:1
	addq	$80, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp21:
.Lfunc_end2:
	.size	print_expr_table, .Lfunc_end2-print_expr_table
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.p2align	3
.LJTI2_0:
	.quad	.LBB2_3
	.quad	.LBB2_4
	.quad	.LBB2_5
	.quad	.LBB2_6
	.quad	.LBB2_7
	.quad	.LBB2_8
	.quad	.LBB2_9
	.quad	.LBB2_10
	.quad	.LBB2_11
	.quad	.LBB2_12
                                        # -- End function
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"exprs != NULL"
	.size	.L.str, 14

	.type	.L.str.1,@object        # @.str.1
.L.str.1:
	.asciz	"print_ast.c"
	.size	.L.str.1, 12

	.type	.L__PRETTY_FUNCTION__.print_expr,@object # @__PRETTY_FUNCTION__.print_expr
.L__PRETTY_FUNCTION__.print_expr:
	.asciz	"void print_expr(struct expr *)"
	.size	.L__PRETTY_FUNCTION__.print_expr, 31

	.type	.L.str.2,@object        # @.str.2
.L.str.2:
	.asciz	"null expression encountered\n"
	.size	.L.str.2, 29

	.type	.L.str.3,@object        # @.str.3
.L.str.3:
	.asciz	"\316\265"
	.size	.L.str.3, 3

	.type	.L.str.4,@object        # @.str.4
.L.str.4:
	.asciz	"."
	.size	.L.str.4, 2

	.type	.L.str.5,@object        # @.str.5
.L.str.5:
	.asciz	"|"
	.size	.L.str.5, 2

	.type	.L.str.6,@object        # @.str.6
.L.str.6:
	.asciz	"+"
	.size	.L.str.6, 2

	.type	.L.str.7,@object        # @.str.7
.L.str.7:
	.asciz	"*"
	.size	.L.str.7, 2

	.type	.L.str.8,@object        # @.str.8
.L.str.8:
	.asciz	"?"
	.size	.L.str.8, 2

	.type	.L.str.9,@object        # @.str.9
.L.str.9:
	.asciz	"()"
	.size	.L.str.9, 3

	.type	.L.str.10,@object       # @.str.10
.L.str.10:
	.asciz	"%c"
	.size	.L.str.10, 3

	.type	.L.str.11,@object       # @.str.11
.L.str.11:
	.asciz	"\n"
	.size	.L.str.11, 2

	.type	.L.str.12,@object       # @.str.12
.L.str.12:
	.asciz	"%p: ("
	.size	.L.str.12, 6

	.type	.L.str.13,@object       # @.str.13
.L.str.13:
	.asciz	"| %p %p"
	.size	.L.str.13, 8

	.type	.L.str.14,@object       # @.str.14
.L.str.14:
	.asciz	"+ %p %p"
	.size	.L.str.14, 8

	.type	.L.str.15,@object       # @.str.15
.L.str.15:
	.asciz	"* %p"
	.size	.L.str.15, 5

	.type	.L.str.16,@object       # @.str.16
.L.str.16:
	.asciz	"+ %p"
	.size	.L.str.16, 5

	.type	.L.str.17,@object       # @.str.17
.L.str.17:
	.asciz	"? %p"
	.size	.L.str.17, 5

	.type	.L.str.18,@object       # @.str.18
.L.str.18:
	.asciz	"() %p"
	.size	.L.str.18, 6

	.type	.L.str.19,@object       # @.str.19
.L.str.19:
	.asciz	")\n"
	.size	.L.str.19, 3

	.type	expr_actions,@object    # @expr_actions
	.comm	expr_actions,80,16
	.type	.L.str.20,@object       # @.str.20
.L.str.20:
	.asciz	"    "
	.size	.L.str.20, 5

	.section	.debug_str,"MS",@progbits,1
.Linfo_string0:
	.asciz	"clang version 8.0.0-3~ubuntu18.04.1 (tags/RELEASE_800/final)" # string offset=0
.Linfo_string1:
	.asciz	"print_ast.c"           # string offset=61
.Linfo_string2:
	.asciz	"/home/wroathe/compilers/src/auto" # string offset=73
.Linfo_string3:
	.asciz	"expr_actions"          # string offset=106
.Linfo_string4:
	.asciz	"_"                     # string offset=119
.Linfo_string5:
	.asciz	"expr"                  # string offset=121
.Linfo_string6:
	.asciz	"type"                  # string offset=126
.Linfo_string7:
	.asciz	"unsigned int"          # string offset=131
.Linfo_string8:
	.asciz	"NULL_EXPR"             # string offset=144
.Linfo_string9:
	.asciz	"EMPTY_EXPR"            # string offset=154
.Linfo_string10:
	.asciz	"DOTALL_EXPR"           # string offset=165
.Linfo_string11:
	.asciz	"ALT_EXPR"              # string offset=177
.Linfo_string12:
	.asciz	"CAT_EXPR"              # string offset=186
.Linfo_string13:
	.asciz	"STAR_EXPR"             # string offset=195
.Linfo_string14:
	.asciz	"PLUS_EXPR"             # string offset=205
.Linfo_string15:
	.asciz	"OPTIONAL_EXPR"         # string offset=215
.Linfo_string16:
	.asciz	"SUB_EXPR"              # string offset=229
.Linfo_string17:
	.asciz	"SYMBOL_EXPR"           # string offset=238
.Linfo_string18:
	.asciz	"expr_type"             # string offset=250
.Linfo_string19:
	.asciz	"lexpr"                 # string offset=260
.Linfo_string20:
	.asciz	"rexpr"                 # string offset=266
.Linfo_string21:
	.asciz	"symbol"                # string offset=272
.Linfo_string22:
	.asciz	"char"                  # string offset=279
.Linfo_string23:
	.asciz	"mach"                  # string offset=284
.Linfo_string24:
	.asciz	"start"                 # string offset=289
.Linfo_string25:
	.asciz	"ACCEPTING_STATE"       # string offset=295
.Linfo_string26:
	.asciz	"EPSILON_STATE"         # string offset=311
.Linfo_string27:
	.asciz	"BRANCH_STATE"          # string offset=325
.Linfo_string28:
	.asciz	"SYMBOL_STATE"          # string offset=338
.Linfo_string29:
	.asciz	"DOTALL_STATE"          # string offset=351
.Linfo_string30:
	.asciz	"nfa_state_type"        # string offset=364
.Linfo_string31:
	.asciz	"id"                    # string offset=379
.Linfo_string32:
	.asciz	"int"                   # string offset=382
.Linfo_string33:
	.asciz	"next"                  # string offset=386
.Linfo_string34:
	.asciz	"left"                  # string offset=391
.Linfo_string35:
	.asciz	"right"                 # string offset=396
.Linfo_string36:
	.asciz	"nfa_state"             # string offset=402
.Linfo_string37:
	.asciz	"end"                   # string offset=412
.Linfo_string38:
	.asciz	"end1"                  # string offset=416
.Linfo_string39:
	.asciz	"nfa"                   # string offset=421
.Linfo_string40:
	.asciz	"sym"                   # string offset=425
.Linfo_string41:
	.asciz	"rval"                  # string offset=429
.Linfo_string42:
	.asciz	"__ARRAY_SIZE_TYPE__"   # string offset=434
.Linfo_string43:
	.asciz	"print_expr"            # string offset=454
.Linfo_string44:
	.asciz	"indent"                # string offset=465
.Linfo_string45:
	.asciz	"print_expr_table"      # string offset=472
.Linfo_string46:
	.asciz	"exprs"                 # string offset=489
.Linfo_string47:
	.asciz	"sp"                    # string offset=495
.Linfo_string48:
	.asciz	"indent_level"          # string offset=498
.Linfo_string49:
	.asciz	"n"                     # string offset=511
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
	.byte	1                       # DW_TAG_array_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	4                       # Abbreviation Code
	.byte	33                      # DW_TAG_subrange_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	55                      # DW_AT_count
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	5                       # Abbreviation Code
	.byte	15                      # DW_TAG_pointer_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	6                       # Abbreviation Code
	.byte	21                      # DW_TAG_subroutine_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	7                       # Abbreviation Code
	.byte	5                       # DW_TAG_formal_parameter
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	8                       # Abbreviation Code
	.byte	15                      # DW_TAG_pointer_type
	.byte	0                       # DW_CHILDREN_no
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	9                       # Abbreviation Code
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
	.byte	10                      # Abbreviation Code
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
	.byte	11                      # Abbreviation Code
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
	.byte	12                      # Abbreviation Code
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
	.byte	13                      # Abbreviation Code
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
	.byte	14                      # Abbreviation Code
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
	.byte	15                      # Abbreviation Code
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
	.byte	16                      # Abbreviation Code
	.byte	40                      # DW_TAG_enumerator
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	28                      # DW_AT_const_value
	.byte	15                      # DW_FORM_udata
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	17                      # Abbreviation Code
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
	.byte	21                      # Abbreviation Code
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
	.byte	1                       # Abbrev [1] 0xb:0x2f2 DW_TAG_compile_unit
	.long	.Linfo_string0          # DW_AT_producer
	.short	12                      # DW_AT_language
	.long	.Linfo_string1          # DW_AT_name
	.long	.Lline_table_start0     # DW_AT_stmt_list
	.long	.Linfo_string2          # DW_AT_comp_dir
	.quad	.Lfunc_begin0           # DW_AT_low_pc
	.long	.Lfunc_end2-.Lfunc_begin0 # DW_AT_high_pc
	.byte	2                       # Abbrev [2] 0x2a:0x15 DW_TAG_variable
	.long	.Linfo_string3          # DW_AT_name
	.long	63                      # DW_AT_type
                                        # DW_AT_external
	.byte	2                       # DW_AT_decl_file
	.byte	46                      # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	expr_actions
	.byte	3                       # Abbrev [3] 0x3f:0xc DW_TAG_array_type
	.long	75                      # DW_AT_type
	.byte	4                       # Abbrev [4] 0x44:0x6 DW_TAG_subrange_type
	.long	588                     # DW_AT_type
	.byte	10                      # DW_AT_count
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x4b:0x5 DW_TAG_pointer_type
	.long	80                      # DW_AT_type
	.byte	6                       # Abbrev [6] 0x50:0xc DW_TAG_subroutine_type
                                        # DW_AT_prototyped
	.byte	7                       # Abbrev [7] 0x51:0x5 DW_TAG_formal_parameter
	.long	92                      # DW_AT_type
	.byte	7                       # Abbrev [7] 0x56:0x5 DW_TAG_formal_parameter
	.long	93                      # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	8                       # Abbrev [8] 0x5c:0x1 DW_TAG_pointer_type
	.byte	9                       # Abbrev [9] 0x5d:0x39 DW_TAG_union_type
	.long	.Linfo_string41         # DW_AT_name
	.byte	24                      # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	56                      # DW_AT_decl_line
	.byte	10                      # Abbrev [10] 0x65:0xc DW_TAG_member
	.long	.Linfo_string4          # DW_AT_name
	.long	92                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	57                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	10                      # Abbrev [10] 0x71:0xc DW_TAG_member
	.long	.Linfo_string5          # DW_AT_name
	.long	150                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	58                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	10                      # Abbrev [10] 0x7d:0xc DW_TAG_member
	.long	.Linfo_string23         # DW_AT_name
	.long	363                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	59                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	10                      # Abbrev [10] 0x89:0xc DW_TAG_member
	.long	.Linfo_string40         # DW_AT_name
	.long	356                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	60                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x96:0x5 DW_TAG_pointer_type
	.long	155                     # DW_AT_type
	.byte	11                      # Abbrev [11] 0x9b:0x79 DW_TAG_structure_type
	.long	.Linfo_string5          # DW_AT_name
	.byte	24                      # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	17                      # DW_AT_decl_line
	.byte	10                      # Abbrev [10] 0xa3:0xc DW_TAG_member
	.long	.Linfo_string6          # DW_AT_name
	.long	276                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	18                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	12                      # Abbrev [12] 0xaf:0x8 DW_TAG_member
	.long	183                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	19                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	13                      # Abbrev [13] 0xb7:0x5c DW_TAG_union_type
	.byte	16                      # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	19                      # DW_AT_decl_line
	.byte	12                      # Abbrev [12] 0xbb:0x8 DW_TAG_member
	.long	195                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	21                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	14                      # Abbrev [14] 0xc3:0x1d DW_TAG_structure_type
	.byte	16                      # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	21                      # DW_AT_decl_line
	.byte	10                      # Abbrev [10] 0xc7:0xc DW_TAG_member
	.long	.Linfo_string19         # DW_AT_name
	.long	150                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	21                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	10                      # Abbrev [10] 0xd3:0xc DW_TAG_member
	.long	.Linfo_string20         # DW_AT_name
	.long	150                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	21                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0xe0:0x8 DW_TAG_member
	.long	232                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	23                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	14                      # Abbrev [14] 0xe8:0x11 DW_TAG_structure_type
	.byte	8                       # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	23                      # DW_AT_decl_line
	.byte	10                      # Abbrev [10] 0xec:0xc DW_TAG_member
	.long	.Linfo_string5          # DW_AT_name
	.long	150                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	23                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0xf9:0x8 DW_TAG_member
	.long	257                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	25                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	14                      # Abbrev [14] 0x101:0x11 DW_TAG_structure_type
	.byte	1                       # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	25                      # DW_AT_decl_line
	.byte	10                      # Abbrev [10] 0x105:0xc DW_TAG_member
	.long	.Linfo_string21         # DW_AT_name
	.long	356                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	25                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	15                      # Abbrev [15] 0x114:0x49 DW_TAG_enumeration_type
	.long	349                     # DW_AT_type
	.long	.Linfo_string18         # DW_AT_name
	.byte	4                       # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	4                       # DW_AT_decl_line
	.byte	16                      # Abbrev [16] 0x120:0x6 DW_TAG_enumerator
	.long	.Linfo_string8          # DW_AT_name
	.byte	0                       # DW_AT_const_value
	.byte	16                      # Abbrev [16] 0x126:0x6 DW_TAG_enumerator
	.long	.Linfo_string9          # DW_AT_name
	.byte	1                       # DW_AT_const_value
	.byte	16                      # Abbrev [16] 0x12c:0x6 DW_TAG_enumerator
	.long	.Linfo_string10         # DW_AT_name
	.byte	2                       # DW_AT_const_value
	.byte	16                      # Abbrev [16] 0x132:0x6 DW_TAG_enumerator
	.long	.Linfo_string11         # DW_AT_name
	.byte	3                       # DW_AT_const_value
	.byte	16                      # Abbrev [16] 0x138:0x6 DW_TAG_enumerator
	.long	.Linfo_string12         # DW_AT_name
	.byte	4                       # DW_AT_const_value
	.byte	16                      # Abbrev [16] 0x13e:0x6 DW_TAG_enumerator
	.long	.Linfo_string13         # DW_AT_name
	.byte	5                       # DW_AT_const_value
	.byte	16                      # Abbrev [16] 0x144:0x6 DW_TAG_enumerator
	.long	.Linfo_string14         # DW_AT_name
	.byte	6                       # DW_AT_const_value
	.byte	16                      # Abbrev [16] 0x14a:0x6 DW_TAG_enumerator
	.long	.Linfo_string15         # DW_AT_name
	.byte	7                       # DW_AT_const_value
	.byte	16                      # Abbrev [16] 0x150:0x6 DW_TAG_enumerator
	.long	.Linfo_string16         # DW_AT_name
	.byte	8                       # DW_AT_const_value
	.byte	16                      # Abbrev [16] 0x156:0x6 DW_TAG_enumerator
	.long	.Linfo_string17         # DW_AT_name
	.byte	9                       # DW_AT_const_value
	.byte	0                       # End Of Children Mark
	.byte	17                      # Abbrev [17] 0x15d:0x7 DW_TAG_base_type
	.long	.Linfo_string7          # DW_AT_name
	.byte	7                       # DW_AT_encoding
	.byte	4                       # DW_AT_byte_size
	.byte	17                      # Abbrev [17] 0x164:0x7 DW_TAG_base_type
	.long	.Linfo_string22         # DW_AT_name
	.byte	6                       # DW_AT_encoding
	.byte	1                       # DW_AT_byte_size
	.byte	11                      # Abbrev [11] 0x16b:0x2d DW_TAG_structure_type
	.long	.Linfo_string39         # DW_AT_name
	.byte	24                      # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	50                      # DW_AT_decl_line
	.byte	10                      # Abbrev [10] 0x173:0xc DW_TAG_member
	.long	.Linfo_string24         # DW_AT_name
	.long	408                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	51                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	10                      # Abbrev [10] 0x17f:0xc DW_TAG_member
	.long	.Linfo_string37         # DW_AT_name
	.long	583                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	52                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	10                      # Abbrev [10] 0x18b:0xc DW_TAG_member
	.long	.Linfo_string38         # DW_AT_name
	.long	583                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	53                      # DW_AT_decl_line
	.byte	16                      # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x198:0x5 DW_TAG_pointer_type
	.long	413                     # DW_AT_type
	.byte	11                      # Abbrev [11] 0x19d:0x78 DW_TAG_structure_type
	.long	.Linfo_string36         # DW_AT_name
	.byte	24                      # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	38                      # DW_AT_decl_line
	.byte	10                      # Abbrev [10] 0x1a5:0xc DW_TAG_member
	.long	.Linfo_string6          # DW_AT_name
	.long	533                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	39                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	10                      # Abbrev [10] 0x1b1:0xc DW_TAG_member
	.long	.Linfo_string31         # DW_AT_name
	.long	576                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	40                      # DW_AT_decl_line
	.byte	4                       # DW_AT_data_member_location
	.byte	12                      # Abbrev [12] 0x1bd:0x8 DW_TAG_member
	.long	453                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	41                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	13                      # Abbrev [13] 0x1c5:0x4f DW_TAG_union_type
	.byte	16                      # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	41                      # DW_AT_decl_line
	.byte	12                      # Abbrev [12] 0x1c9:0x8 DW_TAG_member
	.long	465                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	43                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	14                      # Abbrev [14] 0x1d1:0x1d DW_TAG_structure_type
	.byte	16                      # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	43                      # DW_AT_decl_line
	.byte	10                      # Abbrev [10] 0x1d5:0xc DW_TAG_member
	.long	.Linfo_string33         # DW_AT_name
	.long	408                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	43                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	10                      # Abbrev [10] 0x1e1:0xc DW_TAG_member
	.long	.Linfo_string21         # DW_AT_name
	.long	356                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	43                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x1ee:0x8 DW_TAG_member
	.long	502                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	45                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	14                      # Abbrev [14] 0x1f6:0x1d DW_TAG_structure_type
	.byte	16                      # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	45                      # DW_AT_decl_line
	.byte	10                      # Abbrev [10] 0x1fa:0xc DW_TAG_member
	.long	.Linfo_string34         # DW_AT_name
	.long	408                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	45                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	10                      # Abbrev [10] 0x206:0xc DW_TAG_member
	.long	.Linfo_string35         # DW_AT_name
	.long	408                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	45                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	15                      # Abbrev [15] 0x215:0x2b DW_TAG_enumeration_type
	.long	349                     # DW_AT_type
	.long	.Linfo_string30         # DW_AT_name
	.byte	4                       # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	30                      # DW_AT_decl_line
	.byte	16                      # Abbrev [16] 0x221:0x6 DW_TAG_enumerator
	.long	.Linfo_string25         # DW_AT_name
	.byte	0                       # DW_AT_const_value
	.byte	16                      # Abbrev [16] 0x227:0x6 DW_TAG_enumerator
	.long	.Linfo_string26         # DW_AT_name
	.byte	1                       # DW_AT_const_value
	.byte	16                      # Abbrev [16] 0x22d:0x6 DW_TAG_enumerator
	.long	.Linfo_string27         # DW_AT_name
	.byte	2                       # DW_AT_const_value
	.byte	16                      # Abbrev [16] 0x233:0x6 DW_TAG_enumerator
	.long	.Linfo_string28         # DW_AT_name
	.byte	3                       # DW_AT_const_value
	.byte	16                      # Abbrev [16] 0x239:0x6 DW_TAG_enumerator
	.long	.Linfo_string29         # DW_AT_name
	.byte	4                       # DW_AT_const_value
	.byte	0                       # End Of Children Mark
	.byte	17                      # Abbrev [17] 0x240:0x7 DW_TAG_base_type
	.long	.Linfo_string32         # DW_AT_name
	.byte	5                       # DW_AT_encoding
	.byte	4                       # DW_AT_byte_size
	.byte	5                       # Abbrev [5] 0x247:0x5 DW_TAG_pointer_type
	.long	408                     # DW_AT_type
	.byte	18                      # Abbrev [18] 0x24c:0x7 DW_TAG_base_type
	.long	.Linfo_string42         # DW_AT_name
	.byte	8                       # DW_AT_byte_size
	.byte	7                       # DW_AT_encoding
	.byte	19                      # Abbrev [19] 0x253:0x4e DW_TAG_subprogram
	.quad	.Lfunc_begin0           # DW_AT_low_pc
	.long	.Lfunc_end0-.Lfunc_begin0 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string43         # DW_AT_name
	.byte	3                       # DW_AT_decl_file
	.byte	12                      # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x268:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string5          # DW_AT_name
	.byte	3                       # DW_AT_decl_file
	.byte	12                      # DW_AT_decl_line
	.long	150                     # DW_AT_type
	.byte	21                      # Abbrev [21] 0x276:0xe DW_TAG_variable
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string46         # DW_AT_name
	.byte	3                       # DW_AT_decl_file
	.byte	14                      # DW_AT_decl_line
	.long	759                     # DW_AT_type
	.byte	21                      # Abbrev [21] 0x284:0xe DW_TAG_variable
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.long	.Linfo_string47         # DW_AT_name
	.byte	3                       # DW_AT_decl_file
	.byte	14                      # DW_AT_decl_line
	.long	759                     # DW_AT_type
	.byte	21                      # Abbrev [21] 0x292:0xe DW_TAG_variable
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	100
	.long	.Linfo_string48         # DW_AT_name
	.byte	3                       # DW_AT_decl_file
	.byte	20                      # DW_AT_decl_line
	.long	576                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x2a1:0x24 DW_TAG_subprogram
	.quad	.Lfunc_begin1           # DW_AT_low_pc
	.long	.Lfunc_end1-.Lfunc_begin1 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string44         # DW_AT_name
	.byte	3                       # DW_AT_decl_file
	.byte	8                       # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	20                      # Abbrev [20] 0x2b6:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	124
	.long	.Linfo_string49         # DW_AT_name
	.byte	3                       # DW_AT_decl_file
	.byte	8                       # DW_AT_decl_line
	.long	576                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x2c5:0x32 DW_TAG_subprogram
	.quad	.Lfunc_begin2           # DW_AT_low_pc
	.long	.Lfunc_end2-.Lfunc_begin2 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string45         # DW_AT_name
	.byte	3                       # DW_AT_decl_file
	.byte	97                      # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x2da:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string24         # DW_AT_name
	.byte	3                       # DW_AT_decl_file
	.byte	97                      # DW_AT_decl_line
	.long	150                     # DW_AT_type
	.byte	20                      # Abbrev [20] 0x2e8:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string37         # DW_AT_name
	.byte	3                       # DW_AT_decl_file
	.byte	97                      # DW_AT_decl_line
	.long	150                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x2f7:0x5 DW_TAG_pointer_type
	.long	150                     # DW_AT_type
	.byte	0                       # End Of Children Mark
.Ldebug_info_end0:
	.section	.debug_macinfo,"",@progbits
	.byte	0                       # End Of Macro List Mark

	.ident	"clang version 8.0.0-3~ubuntu18.04.1 (tags/RELEASE_800/final)"
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym calloc
	.addrsig_sym __assert_fail
	.addrsig_sym fprintf
	.addrsig_sym indent
	.addrsig_sym printf
	.addrsig_sym free
	.addrsig_sym stderr
	.section	.debug_line,"",@progbits
.Lline_table_start0:
