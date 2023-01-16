	.text
	.file	"main"
	.globl	f1                              # -- Begin function f1
	.p2align	4, 0x90
	.type	f1,@function
f1:                                     # @f1
	.cfi_startproc
# %bb.0:                                # %entry
	movq	%rdi, %rax
	movq	V0@GOTPCREL(%rip), %rcx
	addq	(%rcx), %rax
	retq
.Lfunc_end0:
	.size	f1, .Lfunc_end0-f1
	.cfi_endproc
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	$0, (%rsp)
	cmpq	$26, (%rsp)
	je	.LBB1_3
	.p2align	4, 0x90
.LBB1_2:                                # %loop_body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsp), %rdi
	callq	f1@PLT
	movq	%rax, %rdi
	callq	putchar@PLT
	incq	(%rsp)
	cmpq	$26, (%rsp)
	jne	.LBB1_2
.LBB1_3:                                # %loop_exit
	movl	$10, %edi
	.p2align	4, 0x90
.LBB1_4:                                # %loop_entry1
                                        # =>This Inner Loop Header: Depth=1
	callq	putchar@PLT
	cmpq	$0, (%rsp)
	je	.LBB1_6
# %bb.5:                                # %loop_body2
                                        #   in Loop: Header=BB1_4 Depth=1
	movq	(%rsp), %rdi
	decq	%rdi
	movq	%rdi, (%rsp)
	callq	f1@PLT
	movq	%rax, %rdi
	jmp	.LBB1_4
.LBB1_6:                                # %loop_exit3
	movl	$10, %edi
	callq	putchar@PLT
	movq	(%rsp), %rax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc
                                        # -- End function
	.type	V0,@object                      # @V0
	.data
	.globl	V0
	.p2align	3
V0:
	.quad	65                              # 0x41
	.size	V0, 8

	.section	".note.GNU-stack","",@progbits
