section .text
    global write
    global strlen
    global mmap
    global munmap
 
;write(int fd, const void *buf, size_t count)    
write:
    mov rax,1
    syscall 
    ret
    
strlen:
    xor rax,rax;length =0
.loop:
    cmp byte [rdi+rax],0;Check for a null termination
    je .done;If it is null terminated jump to done section
    inc rax;length ++
    jmp .loop;Repeat
.done:
    ret
    
;mmap(void *addr, size_t len, int prot, int flags, int fd, off_t offset)
mmap:
    mov rax,9 ; sys_mmap
    mov r10,rcx ; System V ABI (RCX) to Linux Syscall (R10) swap
    syscall
    ret
    
;munmap(void *addr, size_t len)
munmap:
    mov rax,11
    syscall
    ret