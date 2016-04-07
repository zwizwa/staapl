/* To the extent possible under law, Tom Schouten has waived all
   copyright and related or neighboring rights to registers_stm32f103.c
   Code:    http://zwizwa.be/git/uc_tools
   License: http://creativecommons.org/publicdomain/zero/1.0 */


#define REGISTERS_IMPL
#include "registers_stm32f103.h"

// PPBI - PRIVATE PERIPHERAL BUS INTERNAL
volatile struct {
    uint8_t _res_0000[0xED00];
    uint32_t CPUID;
} cpu __attribute__ ((section (
".mem_E0"
)));


struct devices dev __attribute__ ((section (
".mem_40"
)));

