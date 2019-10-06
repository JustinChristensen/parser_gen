	.text
	.file	"ast.c"
	.file	1 "/home/wroathe/compilers/src/auto" "./result_types.h"
	.file	2 "/home/wroathe/compilers/src/auto" "ast.c"
	.globl	noop_expr               # -- Begin function noop_expr
	.p2align	4, 0x90
	.type	noop_expr,@function
noop_expr:                              # @noop_expr
.Lfunc_begin0:
	.loc	2 104 0                 # ast.c:104:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
.Ltmp0:
	.loc	2 104 61 prologue_end   # ast.c:104:61
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp1:
.Lfunc_end0:
	.size	noop_expr, .Lfunc_end0-noop_expr
	.cfi_endproc
                                        # -- End function
	.globl	do_empty_expr           # -- Begin function do_empty_expr
	.p2align	4, 0x90
	.type	do_empty_expr,@function
do_empty_expr:                          # @do_empty_expr
.Lfunc_begin1:
	.loc	2 106 0                 # ast.c:106:0
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
	.loc	2 107 11 prologue_end   # ast.c:107:11
	movq	-8(%rbp), %rdi
	.loc	2 107 20 is_stmt 0      # ast.c:107:20
	leaq	-32(%rbp), %rax
	movq	%rdi, -40(%rbp)         # 8-byte Spill
	movq	%rax, %rdi
	callq	empty_expr
	movq	-40(%rbp), %rdi         # 8-byte Reload
	.loc	2 107 5                 # ast.c:107:5
	leaq	-32(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	sexpr
	.loc	2 108 1 is_stmt 1       # ast.c:108:1
	addq	$64, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp3:
.Lfunc_end1:
	.size	do_empty_expr, .Lfunc_end1-do_empty_expr
	.cfi_endproc
                                        # -- End function
	.globl	do_alt_expr             # -- Begin function do_alt_expr
	.p2align	4, 0x90
	.type	do_alt_expr,@function
do_alt_expr:                            # @do_alt_expr
.Lfunc_begin2:
	.loc	2 110 0                 # ast.c:110:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$80, %rsp
	leaq	16(%rbp), %rax
	movq	%rdi, -8(%rbp)
.Ltmp4:
	.loc	2 111 11 prologue_end   # ast.c:111:11
	movq	-8(%rbp), %rdi
	.loc	2 111 35 is_stmt 0      # ast.c:111:35
	movq	(%rax), %rsi
	.loc	2 111 47                # ast.c:111:47
	movq	-8(%rbp), %rax
	movq	%rdi, -40(%rbp)         # 8-byte Spill
	.loc	2 111 41                # ast.c:111:41
	movq	%rax, %rdi
	movq	%rsi, -48(%rbp)         # 8-byte Spill
	callq	gexpr
	.loc	2 111 20                # ast.c:111:20
	leaq	-32(%rbp), %rdi
	movq	-48(%rbp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	alt_expr
	movq	-40(%rbp), %rdi         # 8-byte Reload
	.loc	2 111 5                 # ast.c:111:5
	leaq	-32(%rbp), %rax
	movq	(%rax), %rdx
	movq	%rdx, (%rsp)
	movq	8(%rax), %rdx
	movq	%rdx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	sexpr
	.loc	2 112 1 is_stmt 1       # ast.c:112:1
	addq	$80, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp5:
.Lfunc_end2:
	.size	do_alt_expr, .Lfunc_end2-do_alt_expr
	.cfi_endproc
                                        # -- End function
	.globl	do_cat_expr             # -- Begin function do_cat_expr
	.p2align	4, 0x90
	.type	do_cat_expr,@function
do_cat_expr:                            # @do_cat_expr
.Lfunc_begin3:
	.loc	2 114 0                 # ast.c:114:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$80, %rsp
	leaq	16(%rbp), %rax
	movq	%rdi, -8(%rbp)
.Ltmp6:
	.loc	2 115 11 prologue_end   # ast.c:115:11
	movq	-8(%rbp), %rdi
	.loc	2 115 35 is_stmt 0      # ast.c:115:35
	movq	(%rax), %rsi
	.loc	2 115 47                # ast.c:115:47
	movq	-8(%rbp), %rax
	movq	%rdi, -40(%rbp)         # 8-byte Spill
	.loc	2 115 41                # ast.c:115:41
	movq	%rax, %rdi
	movq	%rsi, -48(%rbp)         # 8-byte Spill
	callq	gexpr
	.loc	2 115 20                # ast.c:115:20
	leaq	-32(%rbp), %rdi
	movq	-48(%rbp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	cat_expr
	movq	-40(%rbp), %rdi         # 8-byte Reload
	.loc	2 115 5                 # ast.c:115:5
	leaq	-32(%rbp), %rax
	movq	(%rax), %rdx
	movq	%rdx, (%rsp)
	movq	8(%rax), %rdx
	movq	%rdx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	sexpr
	.loc	2 116 1 is_stmt 1       # ast.c:116:1
	addq	$80, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp7:
.Lfunc_end3:
	.size	do_cat_expr, .Lfunc_end3-do_cat_expr
	.cfi_endproc
                                        # -- End function
	.globl	do_sub_expr             # -- Begin function do_sub_expr
	.p2align	4, 0x90
	.type	do_sub_expr,@function
do_sub_expr:                            # @do_sub_expr
.Lfunc_begin4:
	.loc	2 118 0                 # ast.c:118:0
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
	.loc	2 119 11 prologue_end   # ast.c:119:11
	movq	-8(%rbp), %rdi
	.loc	2 119 35 is_stmt 0      # ast.c:119:35
	movq	-8(%rbp), %rax
	movq	%rdi, -40(%rbp)         # 8-byte Spill
	.loc	2 119 29                # ast.c:119:29
	movq	%rax, %rdi
	callq	gexpr
	.loc	2 119 20                # ast.c:119:20
	leaq	-32(%rbp), %rdi
	movq	%rax, %rsi
	callq	sub_expr
	movq	-40(%rbp), %rdi         # 8-byte Reload
	.loc	2 119 5                 # ast.c:119:5
	leaq	-32(%rbp), %rax
	movq	(%rax), %rsi
	movq	%rsi, (%rsp)
	movq	8(%rax), %rsi
	movq	%rsi, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	sexpr
	.loc	2 120 1 is_stmt 1       # ast.c:120:1
	addq	$64, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp9:
.Lfunc_end4:
	.size	do_sub_expr, .Lfunc_end4-do_sub_expr
	.cfi_endproc
                                        # -- End function
	.globl	do_dotall_expr          # -- Begin function do_dotall_expr
	.p2align	4, 0x90
	.type	do_dotall_expr,@function
do_dotall_expr:                         # @do_dotall_expr
.Lfunc_begin5:
	.loc	2 122 0                 # ast.c:122:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$64, %rsp
	movq	%rdi, -8(%rbp)
.Ltmp10:
	.loc	2 123 11 prologue_end   # ast.c:123:11
	movq	-8(%rbp), %rdi
	.loc	2 123 20 is_stmt 0      # ast.c:123:20
	leaq	-32(%rbp), %rax
	movq	%rdi, -40(%rbp)         # 8-byte Spill
	movq	%rax, %rdi
	callq	dotall_expr
	movq	-40(%rbp), %rdi         # 8-byte Reload
	.loc	2 123 5                 # ast.c:123:5
	leaq	-32(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	sexpr
	.loc	2 124 1 is_stmt 1       # ast.c:124:1
	addq	$64, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp11:
.Lfunc_end5:
	.size	do_dotall_expr, .Lfunc_end5-do_dotall_expr
	.cfi_endproc
                                        # -- End function
	.globl	do_symbol_expr          # -- Begin function do_symbol_expr
	.p2align	4, 0x90
	.type	do_symbol_expr,@function
do_symbol_expr:                         # @do_symbol_expr
.Lfunc_begin6:
	.loc	2 126 0                 # ast.c:126:0
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
.Ltmp12:
	.loc	2 127 11 prologue_end   # ast.c:127:11
	movq	-8(%rbp), %rdi
	.loc	2 127 20 is_stmt 0      # ast.c:127:20
	leaq	-32(%rbp), %rcx
	movq	%rdi, -40(%rbp)         # 8-byte Spill
	movq	%rcx, %rdi
	movsbl	(%rax), %esi
	callq	symbol_expr
	movq	-40(%rbp), %rdi         # 8-byte Reload
	.loc	2 127 5                 # ast.c:127:5
	leaq	-32(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	sexpr
	.loc	2 128 1 is_stmt 1       # ast.c:128:1
	addq	$64, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp13:
.Lfunc_end6:
	.size	do_symbol_expr, .Lfunc_end6-do_symbol_expr
	.cfi_endproc
                                        # -- End function
	.globl	do_star_expr            # -- Begin function do_star_expr
	.p2align	4, 0x90
	.type	do_star_expr,@function
do_star_expr:                           # @do_star_expr
.Lfunc_begin7:
	.loc	2 130 0                 # ast.c:130:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$64, %rsp
	movq	%rdi, -8(%rbp)
.Ltmp14:
	.loc	2 131 11 prologue_end   # ast.c:131:11
	movq	-8(%rbp), %rdi
	.loc	2 131 36 is_stmt 0      # ast.c:131:36
	movq	-8(%rbp), %rax
	movq	%rdi, -40(%rbp)         # 8-byte Spill
	.loc	2 131 30                # ast.c:131:30
	movq	%rax, %rdi
	callq	gexpr
	.loc	2 131 20                # ast.c:131:20
	leaq	-32(%rbp), %rdi
	movq	%rax, %rsi
	callq	star_expr
	movq	-40(%rbp), %rdi         # 8-byte Reload
	.loc	2 131 5                 # ast.c:131:5
	leaq	-32(%rbp), %rax
	movq	(%rax), %rsi
	movq	%rsi, (%rsp)
	movq	8(%rax), %rsi
	movq	%rsi, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	sexpr
	.loc	2 132 1 is_stmt 1       # ast.c:132:1
	addq	$64, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp15:
.Lfunc_end7:
	.size	do_star_expr, .Lfunc_end7-do_star_expr
	.cfi_endproc
                                        # -- End function
	.globl	do_plus_expr            # -- Begin function do_plus_expr
	.p2align	4, 0x90
	.type	do_plus_expr,@function
do_plus_expr:                           # @do_plus_expr
.Lfunc_begin8:
	.loc	2 134 0                 # ast.c:134:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$64, %rsp
	movq	%rdi, -8(%rbp)
.Ltmp16:
	.loc	2 135 11 prologue_end   # ast.c:135:11
	movq	-8(%rbp), %rdi
	.loc	2 135 36 is_stmt 0      # ast.c:135:36
	movq	-8(%rbp), %rax
	movq	%rdi, -40(%rbp)         # 8-byte Spill
	.loc	2 135 30                # ast.c:135:30
	movq	%rax, %rdi
	callq	gexpr
	.loc	2 135 20                # ast.c:135:20
	leaq	-32(%rbp), %rdi
	movq	%rax, %rsi
	callq	plus_expr
	movq	-40(%rbp), %rdi         # 8-byte Reload
	.loc	2 135 5                 # ast.c:135:5
	leaq	-32(%rbp), %rax
	movq	(%rax), %rsi
	movq	%rsi, (%rsp)
	movq	8(%rax), %rsi
	movq	%rsi, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	sexpr
	.loc	2 136 1 is_stmt 1       # ast.c:136:1
	addq	$64, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp17:
.Lfunc_end8:
	.size	do_plus_expr, .Lfunc_end8-do_plus_expr
	.cfi_endproc
                                        # -- End function
	.globl	do_optional_expr        # -- Begin function do_optional_expr
	.p2align	4, 0x90
	.type	do_optional_expr,@function
do_optional_expr:                       # @do_optional_expr
.Lfunc_begin9:
	.loc	2 138 0                 # ast.c:138:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$64, %rsp
	movq	%rdi, -8(%rbp)
.Ltmp18:
	.loc	2 139 11 prologue_end   # ast.c:139:11
	movq	-8(%rbp), %rdi
	.loc	2 139 40 is_stmt 0      # ast.c:139:40
	movq	-8(%rbp), %rax
	movq	%rdi, -40(%rbp)         # 8-byte Spill
	.loc	2 139 34                # ast.c:139:34
	movq	%rax, %rdi
	callq	gexpr
	.loc	2 139 20                # ast.c:139:20
	leaq	-32(%rbp), %rdi
	movq	%rax, %rsi
	callq	optional_expr
	movq	-40(%rbp), %rdi         # 8-byte Reload
	.loc	2 139 5                 # ast.c:139:5
	leaq	-32(%rbp), %rax
	movq	(%rax), %rsi
	movq	%rsi, (%rsp)
	movq	8(%rax), %rsi
	movq	%rsi, 8(%rsp)
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)
	callq	sexpr
	.loc	2 140 1 is_stmt 1       # ast.c:140:1
	addq	$64, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp19:
.Lfunc_end9:
	.size	do_optional_expr, .Lfunc_end9-do_optional_expr
	.cfi_endproc
                                        # -- End function
	.globl	alt_expr                # -- Begin function alt_expr
	.p2align	4, 0x90
	.type	alt_expr,@function
alt_expr:                               # @alt_expr
.Lfunc_begin10:
	.loc	2 20 0                  # ast.c:20:0
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
.Ltmp20:
	.loc	2 21 26 prologue_end    # ast.c:21:26
	movl	$3, (%rdi)
	.loc	2 23 18                 # ast.c:23:18
	movq	-8(%rbp), %rdx
	.loc	2 23 9 is_stmt 0        # ast.c:23:9
	movq	%rdx, 8(%rdi)
	.loc	2 24 18 is_stmt 1       # ast.c:24:18
	movq	-16(%rbp), %rdx
	.loc	2 23 9                  # ast.c:23:9
	movq	%rdx, 16(%rdi)
	.loc	2 21 5                  # ast.c:21:5
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp21:
.Lfunc_end10:
	.size	alt_expr, .Lfunc_end10-alt_expr
	.cfi_endproc
                                        # -- End function
	.globl	cat_expr                # -- Begin function cat_expr
	.p2align	4, 0x90
	.type	cat_expr,@function
cat_expr:                               # @cat_expr
.Lfunc_begin11:
	.loc	2 28 0                  # ast.c:28:0
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
.Ltmp22:
	.loc	2 29 26 prologue_end    # ast.c:29:26
	movl	$4, (%rdi)
	.loc	2 31 18                 # ast.c:31:18
	movq	-8(%rbp), %rdx
	.loc	2 31 9 is_stmt 0        # ast.c:31:9
	movq	%rdx, 8(%rdi)
	.loc	2 32 18 is_stmt 1       # ast.c:32:18
	movq	-16(%rbp), %rdx
	.loc	2 31 9                  # ast.c:31:9
	movq	%rdx, 16(%rdi)
	.loc	2 29 5                  # ast.c:29:5
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp23:
.Lfunc_end11:
	.size	cat_expr, .Lfunc_end11-cat_expr
	.cfi_endproc
                                        # -- End function
	.globl	star_expr               # -- Begin function star_expr
	.p2align	4, 0x90
	.type	star_expr,@function
star_expr:                              # @star_expr
.Lfunc_begin12:
	.loc	2 36 0                  # ast.c:36:0
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
	.loc	2 37 26 prologue_end    # ast.c:37:26
	movl	$5, (%rdi)
	.loc	2 39 17                 # ast.c:39:17
	movq	-8(%rbp), %rsi
	.loc	2 39 9 is_stmt 0        # ast.c:39:9
	movq	%rsi, 8(%rdi)
	.loc	2 37 5 is_stmt 1        # ast.c:37:5
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp25:
.Lfunc_end12:
	.size	star_expr, .Lfunc_end12-star_expr
	.cfi_endproc
                                        # -- End function
	.globl	plus_expr               # -- Begin function plus_expr
	.p2align	4, 0x90
	.type	plus_expr,@function
plus_expr:                              # @plus_expr
.Lfunc_begin13:
	.loc	2 43 0                  # ast.c:43:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, %rax
	movq	%rsi, -8(%rbp)
.Ltmp26:
	.loc	2 44 26 prologue_end    # ast.c:44:26
	movl	$6, (%rdi)
	.loc	2 46 17                 # ast.c:46:17
	movq	-8(%rbp), %rsi
	.loc	2 46 9 is_stmt 0        # ast.c:46:9
	movq	%rsi, 8(%rdi)
	.loc	2 44 5 is_stmt 1        # ast.c:44:5
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp27:
.Lfunc_end13:
	.size	plus_expr, .Lfunc_end13-plus_expr
	.cfi_endproc
                                        # -- End function
	.globl	optional_expr           # -- Begin function optional_expr
	.p2align	4, 0x90
	.type	optional_expr,@function
optional_expr:                          # @optional_expr
.Lfunc_begin14:
	.loc	2 50 0                  # ast.c:50:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, %rax
	movq	%rsi, -8(%rbp)
.Ltmp28:
	.loc	2 51 26 prologue_end    # ast.c:51:26
	movl	$7, (%rdi)
	.loc	2 53 17                 # ast.c:53:17
	movq	-8(%rbp), %rsi
	.loc	2 53 9 is_stmt 0        # ast.c:53:9
	movq	%rsi, 8(%rdi)
	.loc	2 51 5 is_stmt 1        # ast.c:51:5
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp29:
.Lfunc_end14:
	.size	optional_expr, .Lfunc_end14-optional_expr
	.cfi_endproc
                                        # -- End function
	.globl	sub_expr                # -- Begin function sub_expr
	.p2align	4, 0x90
	.type	sub_expr,@function
sub_expr:                               # @sub_expr
.Lfunc_begin15:
	.loc	2 57 0                  # ast.c:57:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, %rax
	movq	%rsi, -8(%rbp)
.Ltmp30:
	.loc	2 58 26 prologue_end    # ast.c:58:26
	movl	$8, (%rdi)
	.loc	2 60 17                 # ast.c:60:17
	movq	-8(%rbp), %rsi
	.loc	2 60 9 is_stmt 0        # ast.c:60:9
	movq	%rsi, 8(%rdi)
	.loc	2 58 5 is_stmt 1        # ast.c:58:5
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp31:
.Lfunc_end15:
	.size	sub_expr, .Lfunc_end15-sub_expr
	.cfi_endproc
                                        # -- End function
	.globl	symbol_expr             # -- Begin function symbol_expr
	.p2align	4, 0x90
	.type	symbol_expr,@function
symbol_expr:                            # @symbol_expr
.Lfunc_begin16:
	.loc	2 64 0                  # ast.c:64:0
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
.Ltmp32:
	.loc	2 65 12 prologue_end    # ast.c:65:12
	movq	%rdi, %rdx
	movq	%rdi, -16(%rbp)         # 8-byte Spill
	movq	%rdx, %rdi
	movl	$24, %edx
	movq	%rcx, -24(%rbp)         # 8-byte Spill
	callq	memset
	movq	-16(%rbp), %rcx         # 8-byte Reload
	.loc	2 65 26 is_stmt 0       # ast.c:65:26
	movl	$9, (%rcx)
	.loc	2 67 19 is_stmt 1       # ast.c:67:19
	movb	-1(%rbp), %al
	.loc	2 67 9 is_stmt 0        # ast.c:67:9
	movb	%al, 8(%rcx)
	movq	-24(%rbp), %rax         # 8-byte Reload
	.loc	2 65 5 is_stmt 1        # ast.c:65:5
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp33:
.Lfunc_end16:
	.size	symbol_expr, .Lfunc_end16-symbol_expr
	.cfi_endproc
                                        # -- End function
	.globl	dotall_expr             # -- Begin function dotall_expr
	.p2align	4, 0x90
	.type	dotall_expr,@function
dotall_expr:                            # @dotall_expr
.Lfunc_begin17:
	.loc	2 71 0                  # ast.c:71:0
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
.Ltmp34:
	.loc	2 72 12 prologue_end    # ast.c:72:12
	movq	%rdi, %rcx
	movq	%rdi, -8(%rbp)          # 8-byte Spill
	movq	%rcx, %rdi
	movl	$24, %edx
	movq	%rax, -16(%rbp)         # 8-byte Spill
	callq	memset
	movq	-8(%rbp), %rax          # 8-byte Reload
	.loc	2 72 26 is_stmt 0       # ast.c:72:26
	movl	$2, (%rax)
	movq	-16(%rbp), %rax         # 8-byte Reload
	.loc	2 72 5                  # ast.c:72:5
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp35:
.Lfunc_end17:
	.size	dotall_expr, .Lfunc_end17-dotall_expr
	.cfi_endproc
                                        # -- End function
	.globl	empty_expr              # -- Begin function empty_expr
	.p2align	4, 0x90
	.type	empty_expr,@function
empty_expr:                             # @empty_expr
.Lfunc_begin18:
	.loc	2 77 0 is_stmt 1        # ast.c:77:0
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
.Ltmp36:
	.loc	2 78 12 prologue_end    # ast.c:78:12
	movq	%rdi, %rcx
	movq	%rdi, -8(%rbp)          # 8-byte Spill
	movq	%rcx, %rdi
	movl	$24, %edx
	movq	%rax, -16(%rbp)         # 8-byte Spill
	callq	memset
	movq	-8(%rbp), %rax          # 8-byte Reload
	.loc	2 78 26 is_stmt 0       # ast.c:78:26
	movl	$1, (%rax)
	movq	-16(%rbp), %rax         # 8-byte Reload
	.loc	2 78 5                  # ast.c:78:5
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp37:
.Lfunc_end18:
	.size	empty_expr, .Lfunc_end18-empty_expr
	.cfi_endproc
                                        # -- End function
	.globl	expr_context            # -- Begin function expr_context
	.p2align	4, 0x90
	.type	expr_context,@function
expr_context:                           # @expr_context
.Lfunc_begin19:
	.loc	2 83 0 is_stmt 1        # ast.c:83:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, %rax
	xorl	%ecx, %ecx
	movq	%rsi, -8(%rbp)
.Ltmp38:
	.loc	2 84 12 prologue_end    # ast.c:84:12
	movq	%rdi, %rsi
	movq	%rdi, -16(%rbp)         # 8-byte Spill
	movq	%rsi, %rdi
	movl	%ecx, %esi
	movl	$32, %edx
	movq	%rax, -24(%rbp)         # 8-byte Spill
	callq	memset
	.loc	2 85 20                 # ast.c:85:20
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rdx         # 8-byte Reload
	.loc	2 84 34                 # ast.c:84:34
	movq	%rax, (%rdx)
	movq	-24(%rbp), %rax         # 8-byte Reload
	.loc	2 84 5 is_stmt 0        # ast.c:84:5
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp39:
.Lfunc_end19:
	.size	expr_context, .Lfunc_end19-expr_context
	.cfi_endproc
                                        # -- End function
	.globl	sexpr                   # -- Begin function sexpr
	.p2align	4, 0x90
	.type	sexpr,@function
sexpr:                                  # @sexpr
.Lfunc_begin20:
	.loc	2 90 0 is_stmt 1        # ast.c:90:0
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
	.loc	2 91 6 prologue_end     # ast.c:91:6
	movq	-8(%rbp), %rdi
	.loc	2 91 15 is_stmt 0       # ast.c:91:15
	movq	(%rdi), %rdi
	.loc	2 91 25                 # ast.c:91:25
	movq	(%rax), %rcx
	movq	%rcx, (%rdi)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rdi)
	movq	16(%rax), %rax
	movq	%rax, 16(%rdi)
	.loc	2 92 21 is_stmt 1       # ast.c:92:21
	movq	-8(%rbp), %rax
	.loc	2 92 30 is_stmt 0       # ast.c:92:30
	movq	(%rax), %rax
	.loc	2 92 5                  # ast.c:92:5
	movq	-8(%rbp), %rcx
	.loc	2 92 19                 # ast.c:92:19
	movq	%rax, 8(%rcx)
	.loc	2 93 5 is_stmt 1        # ast.c:93:5
	movq	-8(%rbp), %rax
	.loc	2 93 21 is_stmt 0       # ast.c:93:21
	movq	(%rax), %rcx
	addq	$24, %rcx
	movq	%rcx, (%rax)
	.loc	2 94 1 is_stmt 1        # ast.c:94:1
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp41:
.Lfunc_end20:
	.size	sexpr, .Lfunc_end20-sexpr
	.cfi_endproc
                                        # -- End function
	.globl	gexpr                   # -- Begin function gexpr
	.p2align	4, 0x90
	.type	gexpr,@function
gexpr:                                  # @gexpr
.Lfunc_begin21:
	.loc	2 96 0                  # ast.c:96:0
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
.Ltmp42:
	.loc	2 97 12 prologue_end    # ast.c:97:12
	movq	-8(%rbp), %rdi
	.loc	2 97 21 is_stmt 0       # ast.c:97:21
	movq	8(%rdi), %rax
	.loc	2 97 5                  # ast.c:97:5
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp43:
.Lfunc_end21:
	.size	gexpr, .Lfunc_end21-gexpr
	.cfi_endproc
                                        # -- End function
	.globl	expr_to_rval            # -- Begin function expr_to_rval
	.p2align	4, 0x90
	.type	expr_to_rval,@function
expr_to_rval:                           # @expr_to_rval
.Lfunc_begin22:
	.loc	2 100 0 is_stmt 1       # ast.c:100:0
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
.Ltmp44:
	.loc	2 101 41 prologue_end   # ast.c:101:41
	movq	-8(%rbp), %rsi
	movq	%rdi, -16(%rbp)         # 8-byte Spill
	.loc	2 101 35 is_stmt 0      # ast.c:101:35
	movq	%rsi, %rdi
	movq	%rax, -24(%rbp)         # 8-byte Spill
	callq	gexpr
	movq	-16(%rbp), %rsi         # 8-byte Reload
	.loc	2 101 25                # ast.c:101:25
	movq	%rax, (%rsi)
	movq	-24(%rbp), %rax         # 8-byte Reload
	.loc	2 101 5                 # ast.c:101:5
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Ltmp45:
.Lfunc_end22:
	.size	expr_to_rval, .Lfunc_end22-expr_to_rval
	.cfi_endproc
                                        # -- End function
	.type	expr_actions,@object    # @expr_actions
	.data
	.globl	expr_actions
	.p2align	4
expr_actions:
	.quad	noop_expr
	.quad	do_empty_expr
	.quad	do_alt_expr
	.quad	do_cat_expr
	.quad	do_sub_expr
	.quad	do_dotall_expr
	.quad	do_symbol_expr
	.quad	do_star_expr
	.quad	do_plus_expr
	.quad	do_optional_expr
	.size	expr_actions, 80

	.file	3 "/home/wroathe/compilers/src/auto" "./ast.h"
	.file	4 "/home/wroathe/compilers/src/auto" "./parser.h"
	.section	.debug_str,"MS",@progbits,1
.Linfo_string0:
	.asciz	"clang version 8.0.0-3~ubuntu18.04.1 (tags/RELEASE_800/final)" # string offset=0
.Linfo_string1:
	.asciz	"ast.c"                 # string offset=61
.Linfo_string2:
	.asciz	"/home/wroathe/compilers/src/auto" # string offset=67
.Linfo_string3:
	.asciz	"expr_actions"          # string offset=100
.Linfo_string4:
	.asciz	"_"                     # string offset=113
.Linfo_string5:
	.asciz	"expr"                  # string offset=115
.Linfo_string6:
	.asciz	"type"                  # string offset=120
.Linfo_string7:
	.asciz	"unsigned int"          # string offset=125
.Linfo_string8:
	.asciz	"NULL_EXPR"             # string offset=138
.Linfo_string9:
	.asciz	"EMPTY_EXPR"            # string offset=148
.Linfo_string10:
	.asciz	"DOTALL_EXPR"           # string offset=159
.Linfo_string11:
	.asciz	"ALT_EXPR"              # string offset=171
.Linfo_string12:
	.asciz	"CAT_EXPR"              # string offset=180
.Linfo_string13:
	.asciz	"STAR_EXPR"             # string offset=189
.Linfo_string14:
	.asciz	"PLUS_EXPR"             # string offset=199
.Linfo_string15:
	.asciz	"OPTIONAL_EXPR"         # string offset=209
.Linfo_string16:
	.asciz	"SUB_EXPR"              # string offset=223
.Linfo_string17:
	.asciz	"SYMBOL_EXPR"           # string offset=232
.Linfo_string18:
	.asciz	"expr_type"             # string offset=244
.Linfo_string19:
	.asciz	"lexpr"                 # string offset=254
.Linfo_string20:
	.asciz	"rexpr"                 # string offset=260
.Linfo_string21:
	.asciz	"symbol"                # string offset=266
.Linfo_string22:
	.asciz	"char"                  # string offset=273
.Linfo_string23:
	.asciz	"mach"                  # string offset=278
.Linfo_string24:
	.asciz	"start"                 # string offset=283
.Linfo_string25:
	.asciz	"ACCEPTING_STATE"       # string offset=289
.Linfo_string26:
	.asciz	"EPSILON_STATE"         # string offset=305
.Linfo_string27:
	.asciz	"BRANCH_STATE"          # string offset=319
.Linfo_string28:
	.asciz	"SYMBOL_STATE"          # string offset=332
.Linfo_string29:
	.asciz	"DOTALL_STATE"          # string offset=345
.Linfo_string30:
	.asciz	"nfa_state_type"        # string offset=358
.Linfo_string31:
	.asciz	"id"                    # string offset=373
.Linfo_string32:
	.asciz	"int"                   # string offset=376
.Linfo_string33:
	.asciz	"next"                  # string offset=380
.Linfo_string34:
	.asciz	"left"                  # string offset=385
.Linfo_string35:
	.asciz	"right"                 # string offset=390
.Linfo_string36:
	.asciz	"nfa_state"             # string offset=396
.Linfo_string37:
	.asciz	"end"                   # string offset=406
.Linfo_string38:
	.asciz	"end1"                  # string offset=410
.Linfo_string39:
	.asciz	"nfa"                   # string offset=415
.Linfo_string40:
	.asciz	"sym"                   # string offset=419
.Linfo_string41:
	.asciz	"rval"                  # string offset=423
.Linfo_string42:
	.asciz	"__ARRAY_SIZE_TYPE__"   # string offset=428
.Linfo_string43:
	.asciz	"noop_expr"             # string offset=448
.Linfo_string44:
	.asciz	"do_empty_expr"         # string offset=458
.Linfo_string45:
	.asciz	"do_alt_expr"           # string offset=472
.Linfo_string46:
	.asciz	"do_cat_expr"           # string offset=484
.Linfo_string47:
	.asciz	"do_sub_expr"           # string offset=496
.Linfo_string48:
	.asciz	"do_dotall_expr"        # string offset=508
.Linfo_string49:
	.asciz	"do_symbol_expr"        # string offset=523
.Linfo_string50:
	.asciz	"do_star_expr"          # string offset=538
.Linfo_string51:
	.asciz	"do_plus_expr"          # string offset=551
.Linfo_string52:
	.asciz	"do_optional_expr"      # string offset=564
.Linfo_string53:
	.asciz	"alt_expr"              # string offset=581
.Linfo_string54:
	.asciz	"cat_expr"              # string offset=590
.Linfo_string55:
	.asciz	"star_expr"             # string offset=599
.Linfo_string56:
	.asciz	"plus_expr"             # string offset=609
.Linfo_string57:
	.asciz	"optional_expr"         # string offset=619
.Linfo_string58:
	.asciz	"sub_expr"              # string offset=633
.Linfo_string59:
	.asciz	"symbol_expr"           # string offset=642
.Linfo_string60:
	.asciz	"dotall_expr"           # string offset=654
.Linfo_string61:
	.asciz	"empty_expr"            # string offset=666
.Linfo_string62:
	.asciz	"expr_context"          # string offset=677
.Linfo_string63:
	.asciz	"exprbuf"               # string offset=690
.Linfo_string64:
	.asciz	"has_error"             # string offset=698
.Linfo_string65:
	.asciz	"_Bool"                 # string offset=708
.Linfo_string66:
	.asciz	"error"                 # string offset=714
.Linfo_string67:
	.asciz	"token_col"             # string offset=720
.Linfo_string68:
	.asciz	"expected"              # string offset=730
.Linfo_string69:
	.asciz	"actual"                # string offset=739
.Linfo_string70:
	.asciz	"parse_error"           # string offset=746
.Linfo_string71:
	.asciz	"sexpr"                 # string offset=758
.Linfo_string72:
	.asciz	"gexpr"                 # string offset=764
.Linfo_string73:
	.asciz	"expr_to_rval"          # string offset=770
.Linfo_string74:
	.asciz	"context"               # string offset=783
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
	.byte	22                      # Abbreviation Code
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
	.byte	0                       # EOM(3)
	.section	.debug_info,"",@progbits
.Lcu_begin0:
	.long	.Ldebug_info_end0-.Ldebug_info_start0 # Length of Unit
.Ldebug_info_start0:
	.short	4                       # DWARF version number
	.long	.debug_abbrev           # Offset Into Abbrev. Section
	.byte	8                       # Address Size (in bytes)
	.byte	1                       # Abbrev [1] 0xb:0x6bf DW_TAG_compile_unit
	.long	.Linfo_string0          # DW_AT_producer
	.short	12                      # DW_AT_language
	.long	.Linfo_string1          # DW_AT_name
	.long	.Lline_table_start0     # DW_AT_stmt_list
	.long	.Linfo_string2          # DW_AT_comp_dir
	.quad	.Lfunc_begin0           # DW_AT_low_pc
	.long	.Lfunc_end22-.Lfunc_begin0 # DW_AT_high_pc
	.byte	2                       # Abbrev [2] 0x2a:0x15 DW_TAG_variable
	.long	.Linfo_string3          # DW_AT_name
	.long	63                      # DW_AT_type
                                        # DW_AT_external
	.byte	2                       # DW_AT_decl_file
	.byte	7                       # DW_AT_decl_line
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
	.byte	19                      # Abbrev [19] 0x253:0x32 DW_TAG_subprogram
	.quad	.Lfunc_begin0           # DW_AT_low_pc
	.long	.Lfunc_end0-.Lfunc_begin0 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string43         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	104                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x268:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string74         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	104                     # DW_AT_decl_line
	.long	1732                    # DW_AT_type
	.byte	20                      # Abbrev [20] 0x276:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string4          # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	104                     # DW_AT_decl_line
	.long	93                      # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x285:0x32 DW_TAG_subprogram
	.quad	.Lfunc_begin1           # DW_AT_low_pc
	.long	.Lfunc_end1-.Lfunc_begin1 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string44         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	106                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x29a:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string74         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	106                     # DW_AT_decl_line
	.long	1732                    # DW_AT_type
	.byte	20                      # Abbrev [20] 0x2a8:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string4          # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	106                     # DW_AT_decl_line
	.long	93                      # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x2b7:0x32 DW_TAG_subprogram
	.quad	.Lfunc_begin2           # DW_AT_low_pc
	.long	.Lfunc_end2-.Lfunc_begin2 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string45         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	110                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x2cc:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string74         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	110                     # DW_AT_decl_line
	.long	1732                    # DW_AT_type
	.byte	20                      # Abbrev [20] 0x2da:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string19         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	110                     # DW_AT_decl_line
	.long	93                      # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x2e9:0x32 DW_TAG_subprogram
	.quad	.Lfunc_begin3           # DW_AT_low_pc
	.long	.Lfunc_end3-.Lfunc_begin3 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string46         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	114                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x2fe:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string74         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	114                     # DW_AT_decl_line
	.long	1732                    # DW_AT_type
	.byte	20                      # Abbrev [20] 0x30c:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string19         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	114                     # DW_AT_decl_line
	.long	93                      # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x31b:0x32 DW_TAG_subprogram
	.quad	.Lfunc_begin4           # DW_AT_low_pc
	.long	.Lfunc_end4-.Lfunc_begin4 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string47         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	118                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x330:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string74         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	118                     # DW_AT_decl_line
	.long	1732                    # DW_AT_type
	.byte	20                      # Abbrev [20] 0x33e:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string4          # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	118                     # DW_AT_decl_line
	.long	93                      # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x34d:0x32 DW_TAG_subprogram
	.quad	.Lfunc_begin5           # DW_AT_low_pc
	.long	.Lfunc_end5-.Lfunc_begin5 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string48         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	122                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x362:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string74         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	122                     # DW_AT_decl_line
	.long	1732                    # DW_AT_type
	.byte	20                      # Abbrev [20] 0x370:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string4          # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	122                     # DW_AT_decl_line
	.long	93                      # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x37f:0x32 DW_TAG_subprogram
	.quad	.Lfunc_begin6           # DW_AT_low_pc
	.long	.Lfunc_end6-.Lfunc_begin6 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string49         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	126                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x394:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string74         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	126                     # DW_AT_decl_line
	.long	1732                    # DW_AT_type
	.byte	20                      # Abbrev [20] 0x3a2:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string40         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	126                     # DW_AT_decl_line
	.long	93                      # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x3b1:0x32 DW_TAG_subprogram
	.quad	.Lfunc_begin7           # DW_AT_low_pc
	.long	.Lfunc_end7-.Lfunc_begin7 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string50         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	130                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x3c6:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string74         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	130                     # DW_AT_decl_line
	.long	1732                    # DW_AT_type
	.byte	20                      # Abbrev [20] 0x3d4:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string4          # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	130                     # DW_AT_decl_line
	.long	93                      # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x3e3:0x32 DW_TAG_subprogram
	.quad	.Lfunc_begin8           # DW_AT_low_pc
	.long	.Lfunc_end8-.Lfunc_begin8 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string51         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	134                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x3f8:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string74         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	134                     # DW_AT_decl_line
	.long	1732                    # DW_AT_type
	.byte	20                      # Abbrev [20] 0x406:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string4          # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	134                     # DW_AT_decl_line
	.long	93                      # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x415:0x32 DW_TAG_subprogram
	.quad	.Lfunc_begin9           # DW_AT_low_pc
	.long	.Lfunc_end9-.Lfunc_begin9 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string52         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	138                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x42a:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string74         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	138                     # DW_AT_decl_line
	.long	1732                    # DW_AT_type
	.byte	20                      # Abbrev [20] 0x438:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string4          # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	138                     # DW_AT_decl_line
	.long	93                      # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	21                      # Abbrev [21] 0x447:0x36 DW_TAG_subprogram
	.quad	.Lfunc_begin10          # DW_AT_low_pc
	.long	.Lfunc_end10-.Lfunc_begin10 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string53         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	20                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	155                     # DW_AT_type
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x460:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string19         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	20                      # DW_AT_decl_line
	.long	150                     # DW_AT_type
	.byte	20                      # Abbrev [20] 0x46e:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string20         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	20                      # DW_AT_decl_line
	.long	150                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	21                      # Abbrev [21] 0x47d:0x36 DW_TAG_subprogram
	.quad	.Lfunc_begin11          # DW_AT_low_pc
	.long	.Lfunc_end11-.Lfunc_begin11 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string54         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	28                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	155                     # DW_AT_type
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x496:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string19         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	28                      # DW_AT_decl_line
	.long	150                     # DW_AT_type
	.byte	20                      # Abbrev [20] 0x4a4:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.long	.Linfo_string20         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	28                      # DW_AT_decl_line
	.long	150                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	21                      # Abbrev [21] 0x4b3:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin12          # DW_AT_low_pc
	.long	.Lfunc_end12-.Lfunc_begin12 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string55         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	36                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	155                     # DW_AT_type
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x4cc:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string5          # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	36                      # DW_AT_decl_line
	.long	150                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	21                      # Abbrev [21] 0x4db:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin13          # DW_AT_low_pc
	.long	.Lfunc_end13-.Lfunc_begin13 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string56         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	43                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	155                     # DW_AT_type
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x4f4:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string5          # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	43                      # DW_AT_decl_line
	.long	150                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	21                      # Abbrev [21] 0x503:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin14          # DW_AT_low_pc
	.long	.Lfunc_end14-.Lfunc_begin14 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string57         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	50                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	155                     # DW_AT_type
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x51c:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string5          # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	50                      # DW_AT_decl_line
	.long	150                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	21                      # Abbrev [21] 0x52b:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin15          # DW_AT_low_pc
	.long	.Lfunc_end15-.Lfunc_begin15 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string58         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	57                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	155                     # DW_AT_type
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x544:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string5          # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	57                      # DW_AT_decl_line
	.long	150                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	21                      # Abbrev [21] 0x553:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin16          # DW_AT_low_pc
	.long	.Lfunc_end16-.Lfunc_begin16 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string59         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	64                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	155                     # DW_AT_type
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x56c:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	127
	.long	.Linfo_string21         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	64                      # DW_AT_decl_line
	.long	356                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x57b:0x19 DW_TAG_subprogram
	.quad	.Lfunc_begin17          # DW_AT_low_pc
	.long	.Lfunc_end17-.Lfunc_begin17 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string60         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	71                      # DW_AT_decl_line
	.long	155                     # DW_AT_type
                                        # DW_AT_external
	.byte	22                      # Abbrev [22] 0x594:0x19 DW_TAG_subprogram
	.quad	.Lfunc_begin18          # DW_AT_low_pc
	.long	.Lfunc_end18-.Lfunc_begin18 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string61         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	77                      # DW_AT_decl_line
	.long	155                     # DW_AT_type
                                        # DW_AT_external
	.byte	21                      # Abbrev [21] 0x5ad:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin19          # DW_AT_low_pc
	.long	.Lfunc_end19-.Lfunc_begin19 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string62         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	83                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	1623                    # DW_AT_type
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x5c6:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string63         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	83                      # DW_AT_decl_line
	.long	150                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x5d5:0x32 DW_TAG_subprogram
	.quad	.Lfunc_begin20          # DW_AT_low_pc
	.long	.Lfunc_end20-.Lfunc_begin20 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string71         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	90                      # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x5ea:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string74         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	90                      # DW_AT_decl_line
	.long	1732                    # DW_AT_type
	.byte	20                      # Abbrev [20] 0x5f8:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	16
	.long	.Linfo_string5          # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	90                      # DW_AT_decl_line
	.long	155                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	21                      # Abbrev [21] 0x607:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin21          # DW_AT_low_pc
	.long	.Lfunc_end21-.Lfunc_begin21 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string72         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	96                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	150                     # DW_AT_type
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x620:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string74         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	96                      # DW_AT_decl_line
	.long	1732                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	21                      # Abbrev [21] 0x62f:0x28 DW_TAG_subprogram
	.quad	.Lfunc_begin22          # DW_AT_low_pc
	.long	.Lfunc_end22-.Lfunc_begin22 # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.long	.Linfo_string73         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	100                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	93                      # DW_AT_type
                                        # DW_AT_external
	.byte	20                      # Abbrev [20] 0x648:0xe DW_TAG_formal_parameter
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.long	.Linfo_string74         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	100                     # DW_AT_decl_line
	.long	1732                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	11                      # Abbrev [11] 0x657:0x39 DW_TAG_structure_type
	.long	.Linfo_string62         # DW_AT_name
	.byte	32                      # DW_AT_byte_size
	.byte	3                       # DW_AT_decl_file
	.byte	9                       # DW_AT_decl_line
	.byte	10                      # Abbrev [10] 0x65f:0xc DW_TAG_member
	.long	.Linfo_string63         # DW_AT_name
	.long	150                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	10                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	10                      # Abbrev [10] 0x66b:0xc DW_TAG_member
	.long	.Linfo_string5          # DW_AT_name
	.long	150                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	11                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	10                      # Abbrev [10] 0x677:0xc DW_TAG_member
	.long	.Linfo_string64         # DW_AT_name
	.long	1680                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	12                      # DW_AT_decl_line
	.byte	16                      # DW_AT_data_member_location
	.byte	10                      # Abbrev [10] 0x683:0xc DW_TAG_member
	.long	.Linfo_string66         # DW_AT_name
	.long	1687                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	13                      # DW_AT_decl_line
	.byte	20                      # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	17                      # Abbrev [17] 0x690:0x7 DW_TAG_base_type
	.long	.Linfo_string65         # DW_AT_name
	.byte	2                       # DW_AT_encoding
	.byte	1                       # DW_AT_byte_size
	.byte	11                      # Abbrev [11] 0x697:0x2d DW_TAG_structure_type
	.long	.Linfo_string70         # DW_AT_name
	.byte	12                      # DW_AT_byte_size
	.byte	4                       # DW_AT_decl_file
	.byte	69                      # DW_AT_decl_line
	.byte	10                      # Abbrev [10] 0x69f:0xc DW_TAG_member
	.long	.Linfo_string67         # DW_AT_name
	.long	576                     # DW_AT_type
	.byte	4                       # DW_AT_decl_file
	.byte	70                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	10                      # Abbrev [10] 0x6ab:0xc DW_TAG_member
	.long	.Linfo_string68         # DW_AT_name
	.long	576                     # DW_AT_type
	.byte	4                       # DW_AT_decl_file
	.byte	71                      # DW_AT_decl_line
	.byte	4                       # DW_AT_data_member_location
	.byte	10                      # Abbrev [10] 0x6b7:0xc DW_TAG_member
	.long	.Linfo_string69         # DW_AT_name
	.long	576                     # DW_AT_type
	.byte	4                       # DW_AT_decl_file
	.byte	72                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x6c4:0x5 DW_TAG_pointer_type
	.long	1623                    # DW_AT_type
	.byte	0                       # End Of Children Mark
.Ldebug_info_end0:
	.section	.debug_macinfo,"",@progbits
	.byte	0                       # End Of Macro List Mark

	.ident	"clang version 8.0.0-3~ubuntu18.04.1 (tags/RELEASE_800/final)"
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym noop_expr
	.addrsig_sym do_empty_expr
	.addrsig_sym do_alt_expr
	.addrsig_sym do_cat_expr
	.addrsig_sym do_sub_expr
	.addrsig_sym do_dotall_expr
	.addrsig_sym do_symbol_expr
	.addrsig_sym do_star_expr
	.addrsig_sym do_plus_expr
	.addrsig_sym do_optional_expr
	.addrsig_sym alt_expr
	.addrsig_sym cat_expr
	.addrsig_sym star_expr
	.addrsig_sym plus_expr
	.addrsig_sym optional_expr
	.addrsig_sym sub_expr
	.addrsig_sym symbol_expr
	.addrsig_sym dotall_expr
	.addrsig_sym empty_expr
	.addrsig_sym sexpr
	.addrsig_sym gexpr
	.section	.debug_line,"",@progbits
.Lline_table_start0:
