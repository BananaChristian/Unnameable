
ENTRY(_start)

PHDRS
{
    text PT_LOAD FLAGS(5);
    data PT_LOAD FLAGS(6);
}

SECTIONS
{
    . = 0x400000;

    .text : {
        *(.text._start)
        *(.text)
        *(.text.*)
    } :text

    . = ALIGN(0x1000);

    .rodata : { *(.rodata*) } :data
    .data   : { *(.data*)   } :data

    /* Marking the start of the BSS area */
    . = ALIGN(8);
    __bss_start = .;
    .bss : {
        *(.bss*)
        *(COMMON)
    } :data
    /* The absolute end of the program's memory footprint */
    _end = .;

    /DISCARD/ : { *(.comment) *(.note*) }
}
