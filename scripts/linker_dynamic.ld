ENTRY(_start)

SECTIONS
{
    /* Start at 0x400000. 
       Must not define PHDRS manually. We let the linker do it. */
    . = 0x400000 + SIZEOF_HEADERS;

    /* INTERP must be first for the kernel to see it easily */
    .interp : { KEEP(*(.interp)) }

    /* Standard dynamic sections required for .so lookups */
    .hash     : { *(.hash) }
    .gnu.hash : { *(.gnu.hash) }
    .dynsym   : { *(.dynsym) }
    .dynstr   : { *(.dynstr) }
    .rela.plt : { *(.rela.plt) }

    /* Code */
    .text : {
        *(.text._start)
        *(.text)
        *(.text.*)
    }

    .plt : { *(.plt) *(.plt.got) }

    /* Transition to Data (New Page) */
    . = ALIGN(0x1000);

    .dynamic : { *(.dynamic) }
    .got     : { *(.got) *(.got.plt) }
    .rodata  : { *(.rodata*) }
    .data    : { *(.data*) }

    /* Memory footprint end */
    .bss : {
        __bss_start = .;
        *(.bss*)
        *(COMMON)
        _end = .;
    }

    /DISCARD/ : { *(.comment) *(.note*) }
}