/* To the extent possible under law, Tom Schouten has waived all
   copyright and related or neighboring rights to registers_stm32f103.h
   Code:    http://zwizwa.be/git/uc_tools
   License: http://creativecommons.org/publicdomain/zero/1.0 */


#ifndef REGISTERS_H
#define REGISTERS_H

/* Some caveats:
   - some of the names used here conflict with ibopencm3 defines
   - struct member access has some indirection that's not optimized

   Keep in mind that the main purpose of this parallel definition is
   to have symbolic names for registers in gdb.  For fast access, use
   the libopencm3 defines.
*/

struct devices;
extern struct devices dev;

#define DEV_PTR(member) (&(((volatile struct devices *)0x4000000)->member))

#define REG volatile uint32_t

#include <stdint.h>

#define ASSERT_SIZE(structname,size) \
    typedef char assert_size_##structname[-1+(sizeof(struct structname)==size)]

struct map_reserved {
    uint8_t res[0x400];
};
ASSERT_SIZE(map_reserved,0x400);

struct map_rcc {
    REG cr;
    REG cfgr;
    REG cir;
    REG apb2rstr;
    REG apb1rstr;
    REG ahbenr;
    REG apb2enr;
    REG apb1enr;
    uint8_t _res[0x400 - 8*4];
};
ASSERT_SIZE(map_rcc,0x400);

struct map_gpio {
    REG crl;
    REG crh;
    REG idr;
    REG odr;
    REG bsrr;
    REG brr;
    REG lckr;
    uint8_t _res[0x400-0x1C];
};
ASSERT_SIZE(map_gpio,0x400);

struct map_timer {
    REG cr1;
    REG cr2;
    REG smcr;
    REG dier;
    REG sr;
    REG egr;
    REG ccmr1;
    REG ccmr2;
    REG ccer;
    REG cnt;
    REG psc;
    REG arr;
    REG _res_30;
    REG ccr1;
    REG ccr2;
    REG ccr3;
    REG ccr4;
    REG _res_44;
    REG dcr;
    REG dmar;
    uint8_t _res[0x400 - 0x50];
} __attribute__((__packed__));
ASSERT_SIZE(map_timer,0x400);

struct map_usart {
    REG sr;
    REG dr;
    REG brr;
    REG cr1;
    REG cr2;
    REG cr3;
    REG gtpr;
    uint8_t _res[0x400 - 7*4];
} __attribute__((__packed__));
ASSERT_SIZE(map_usart,0x400);

struct map_spi {
    REG cr1;
    REG cr2;
    REG sr;
    REG dr;
    REG crcpr;
    REG rxcrcr;
    REG txcrcr;
    REG i2scfgr;
    REG i2spr;
    uint8_t _res[0x400 - 9*4];
} __attribute__((__packed__));
ASSERT_SIZE(map_spi,0x400);

struct map_dma_channel {
    REG ccr;
    REG cndrr;
    REG cpar;
    REG cmar;
    REG _res;
} __attribute__((__packed__));
struct map_dma {
    REG isr;
    REG ifcr;
    struct map_dma_channel chan[6]; // zero based indexing; refman uses 1-7.
    uint8_t _res[0x400 - 0x80];
} __attribute__((__packed__));
ASSERT_SIZE(map_dma,0x400);

struct map_sdio {
    REG power;
    REG clkcr;
    REG arg;
    REG cmd;
    REG respcmd;
    REG resp1;
    REG resp2;
    REG resp3;
    REG resp4;
    REG dtimer;
    REG dlen;
    REG dctrl;
    REG dcount;
    REG sta;
    REG icr;
    REG mask;
    REG _res_40;
    REG _res_44;
    REG fifo_cnt;
    uint8_t _res_4C[0x80 - 0x4C];
    REG fifo;
    uint8_t _res_84[0x400 - 0x84];
};
ASSERT_SIZE(map_sdio,0x400);


struct devices {
    struct map_timer tim2;
    struct map_timer tim3;
    struct map_timer tim4;
    struct map_timer tim5;
    struct map_timer tim6;
    struct map_timer tim7;
    struct map_timer tim12;
    struct map_timer tim13;
    struct map_timer tim14;
    struct map_reserved _res_02000;
    struct map_reserved rtc;
    struct map_reserved wwdg;
    struct map_reserved iwdg;
    struct map_reserved _res_03400;
    struct map_spi spi2;
    struct map_spi spi3;
    struct map_reserved _res_04000;
    struct map_usart usart2;
    struct map_usart usart3;
    struct map_reserved uart4;
    struct map_reserved uart5;
    struct map_reserved i2c1;
    struct map_reserved i2c2;
    struct map_reserved usbdevfs;
    struct map_reserved usbram;
    struct map_reserved can1;
    struct map_reserved can2;
    struct map_reserved bkp;
    struct map_reserved pwr;
    struct map_reserved dac;
    struct map_reserved _res_07800[0x22];
    struct map_reserved afio;
    struct map_reserved exti;
    struct map_gpio gpioa;
    struct map_gpio gpiob;
    struct map_gpio gpioc;
    struct map_gpio gpiod;
    struct map_gpio gpioe;
    struct map_gpio gpiof;
    struct map_gpio gpiog;
    struct map_reserved adc1;
    struct map_reserved adc2;
    struct map_timer tim1;
    struct map_spi spi1;
    struct map_timer tim8;
    struct map_usart usart1;
    struct map_reserved adc3;
    struct map_reserved _res_14000[3];
    struct map_timer tim9;
    struct map_timer tim10;
    struct map_timer tim11;
    struct map_reserved _res_15800[0xa];
    struct map_sdio sdio;
    struct map_reserved _res_18400[0x1f];
    struct map_dma dma1;
    struct map_dma dma2;
    struct map_reserved _res_20800[2];
    struct map_rcc rcc;
} __attribute__((__packed__));


/* We like to have our cake and eat it:

   - GDB should know the symbol "dev", which means linker needs to
     know it.  It is defined in registers.c

   - C code can be better optimized if the address value of "dev" is
     known to the C compiler (as opposed to being defined as a symbol
     to be resolved by the linker).

*/


#ifndef REGISTERS_IMPL
#define dev (*((volatile struct devices*)0x40000000))
#endif

#endif
