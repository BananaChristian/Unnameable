section .text
    global _start
    extern main
    extern __bss_start
    extern _end
    
_start:
    ; Zero out the BSS
    mov rdi, __bss_start
    mov rcx, _end
    sub rcx, rdi        ; length of BSS
    xor rax, rax
    rep stosb           ; fill with zeros
    
    ;Align the stack
    xor rbp,rbp;Set the base register to zero
    
    pop rdi;Popping the argc parameter into the rdi register
    mov rsi, rsp;Move the values from the stack pointer into the rsi register
    
    and rsp, -16 ;Align the stack before calling main
    
    ;Enter the main function
    call main
    
    ;Exit
    mov rdi,rax ;Move the main's return value into the rdi register
    mov rax,60
    syscall ;Kernel take over