/* Adapted from eCos at91sam7s256 target. */

STARTUP(vectors.o)
ENTRY(reset_vector)
INPUT(extras.o)
GROUP( libtarget.a libgcc.a libsupc++.a )

MEMORY
{
    ram : ORIGIN = 0x00200000, LENGTH = 0x10000
    rom : ORIGIN = 0x00100000, LENGTH = 0x40000
}

SECTIONS
{
    .debug_aranges 0 : { *(.debug_aranges) }
    .debug_pubnames 0 : { *(.debug_pubnames) }
    .debug_info 0 : { *(.debug_info) }
    .debug_abbrev 0 : { *(.debug_abbrev) }
    .debug_line 0 : { *(.debug_line) }
    .debug_frame 0 : { *(.debug_frame) }
    .debug_str 0 : { *(.debug_str) }
    .debug_loc 0 : { *(.debug_loc) }
    .debug_macinfo 0 : { *(.debug_macinfo) }
    .note.arm.ident 0 : { KEEP (*(.note.arm.ident)) }
    /DISCARD/ 0 : { *(.fini_array*) }
    __reserved_bootmon = 0x00000000; 
    . = __reserved_bootmon + 0x01000;
    .rom_vectors 0x00100000 : { __rom_vectors_vma = ABSOLUTE(.); 
    . = .; KEEP (*(.vectors)) } > rom __rom_vectors_lma = LOADADDR(.rom_vectors);
    .ARM.extab ALIGN (0x1) : { PROVIDE (__stext = ABSOLUTE(.)); _stext = ABSOLUTE(.) ;
    . = .; } > rom /DISCARD/ 0 : { *(.ARM.extab* .gnu.linkonce.armextab.*) }         
    . = ALIGN(8);
    . = ALIGN(8); __exidx_start = ABSOLUTE(.);
    .ARM.exidx ALIGN(8) : AT ((LOADADDR (.ARM.extab) + SIZEOF (.ARM.extab) + (8) - 1) & ~ ((8) - 1)) { . = .; } > rom __exidx_end = ABSOLUTE(.); 
    /DISCARD/ 0 : { *(.ARM.exidx* .gnu.linkonce.armexidx.*) }
    .text ALIGN(8) : AT ((LOADADDR (.ARM.exidx) + SIZEOF (.ARM.exidx) + (8) - 1) & ~ ((8) - 1)) { *(.text*) *(.gnu.warning) *(.gnu.linkonce.t.*) *(.init) *(.glue_7) *(.glue_7t) __CTOR_LIST__ = ABSOLUTE (.);

    KEEP (*(SORT (.ctors*))) __CTOR_END__ = ABSOLUTE (.); __DTOR_LIST__ = ABSOLUTE (.); KEEP (*(SORT (.dtors*))) __DTOR_END__ = ABSOLUTE (.); } > rom _etext = .; 
    PROVIDE (__etext = .);

    .fini ALIGN (0x4) : { . = .; *(.fini) } > rom
    .rodata ALIGN (0x4) : { . = .; *(.rodata*) *(.gnu.linkonce.r.*) } > rom
    .rodata1 ALIGN (0x4) : { . = .; *(.rodata1) } > rom
    .fixup ALIGN (0x4) : { . = .; *(.fixup) } > rom
    .gcc_except_table ALIGN (0x4) : { . = .; } > rom /DISCARD/ 0 : { *(.gcc_except_table*) }
    .fixed_vectors 0x00200040 : { . = .; KEEP (*(.fixed_vectors)) } > ram
    .data ALIGN (0x4) : AT ((LOADADDR (.gcc_except_table) + SIZEOF (.gcc_except_table) + (4) - 1) & ~ ((4) - 1)) { __ram_data_start = ABSOLUTE (.);

    *(.data*) *(.data1) *(.gnu.linkonce.d.*) . = ALIGN (4);
    KEEP(*( SORT (.ecos.table.*))) ;
    . = ALIGN (4); __init_array_start__ = ABSOLUTE (.);
    KEEP (*(SORT (.init_array.*))) KEEP (*(SORT (.init_array))) __init_array_end__ = ABSOLUTE (.);
    *(.dynamic) *(.sdata*) *(.gnu.linkonce.s.*) . = ALIGN (4); *(.2ram.*) } > ram __rom_data_start = LOADADDR (.data); __ram_data_end = .; PROVIDE (__ram_data_end = .);
    _edata = .; PROVIDE (edata = .); PROVIDE (__rom_data_end = LOADADDR (.data) + SIZEOF(.data));
    .bss ALIGN (0x4) : { __bss_start = ABSOLUTE (.);
    *(.scommon) *(.dynsbss) *(.sbss*) *(.gnu.linkonce.sb.*) *(.dynbss) *(.bss*) *(.gnu.linkonce.b.*) *(COMMON) __bss_end = ABSOLUTE (.); } > ram
    __heap1 = ALIGN (0x8);
    . = ALIGN(4); _end = .; PROVIDE (end = .);
}
