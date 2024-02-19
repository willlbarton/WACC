.globl main
.section .rodata
.text
main:
.globl main
.section .rodata
.text
main:
        pushq %rbp
        movq  %rbp, %rsp
        movl  $-1, %eax
        movl  %eax, %edi
        call  _exit
        movl  $0, %eax
        movq  %rsp, %rbp
        popq  %rbp
        ret

_exit:
        pushq %rbp
        movq  %rbp, %rsp
        and   $-16, %rsp
        call  exit@plt
        movq  %rsp, %rbp
        popq  %rbp
        ret

.section .rodata
        .int 4
.prints_format:
        .asciz "%.*s"
_prints:
        pushq %rbp
        movq  %rbp, %rsp
        and   $-16, %rsp
        movq  %rdx, %rdi
        movq  %esi, -4(%rdi)
        leaq  .prints_format(%rip), %rdi
        movb  $0, %al
        call  printf@plt
        movq  $0, %rdi
        call  fflush@plt
        movq  %rsp, %rbp
        popq  %rbp
        ret

.section .rodata
        .int 2
.printi_format:
        .asciz "%d"
_printi:
        pushq %rbp
        movq  %rbp, %rsp
        and   $-16, %rsp
        movl  %esi, %edi
        leaq  .printi_format(%rip), %rdi
        movb  $0, %al
        call  printf@plt
        movq  $0, %rdi
        call  fflush@plt
        movq  %rsp, %rbp
        popq  %rbp
        ret

.section .rodata
        .int 0
.printn_format:
        .asciz ""
_printn:
        pushq %rbp
        movq  %rbp, %rsp
        and   $-16, %rsp
        leaq  .printn_format(%rip), %rdi
        movb  $0, %al
        call  puts@plt
        movq  $0, %rdi
        call  fflush@plt
        movq  %rsp, %rbp
        popq  %rbp
        ret

.section .rodata
        .int 2
.printc_format:
        .asciz "%c"
_printc:
        pushq %rbp
        movq  %rbp, %rsp
        and   $-16, %rsp
        movb  %sil, %dil
        leaq  .printc_format(%rip), %rdi
        movb  $0, %al
        call  printf@plt
        movq  $0, %rdi
        call  fflush@plt
        movq  %rsp, %rbp
        popq  %rbp
        ret
