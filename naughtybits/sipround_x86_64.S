/*****************************************************************************
*
*  Raw x86_64 assembly implementations of a SIPROUND:
*
*    https://github.com/veorq/SipHash/blob/93ca99dcfa6a32b1b617e9a5c3c044685254ce8e/siphash.c#L44
*
*  We go through all this only because GHC can't yet emit rotate instructions.
*  The translation from C to assembly would be trivial, but we use gcc's output
*  below which (arbitrarily?) compiles to constant right rotates instead of
*  left, and does some reordering of the instructions, presumably for better
*  pipelining. Interestingly clang does no reordering.
*
*  I didn't go through the excercise of measuring how much worse performance
*  would be if I had to allocate a struct to return the four values here and
*  peek them in haskell-land in the usual way, but we measure 6ns per iteration
*  of the haskell function wrapping `sipRound_s_x1`. Besides this is a much
*  more pleasant interface.
*
 ****************************************************************************/


/* 
*  See GHC's calling convention here:
*    https://github.com/ghc/ghc/blame/master/includes/stg/MachRegs.h#L159
*
*  These appear unlikely to change, and when they do we'll notice and do some
*  conditional stuff so just relax, okay?
*/
#define V0     %rbx
#define V1     %r14
#define V2     %rsi
#define V3     %rdi
#define REG_Sp %rbp

.macro sipRound
        addq     V1,  V0
        addq     V3,  V2
        ror     $51,  V1
        ror     $48,  V3
        xorq     V0,  V1
        xorq     V2,  V3
        ror     $32,  V0
        addq     V1,  V2
        addq     V3,  V0
        ror     $47,  V1
        ror     $43,  V3
        xorq     V2,  V1
        xorq     V0,  V3
        ror     $32,  V2
.endm

.macro returnHask
        jmp     *(REG_Sp)
.endm

/*
*  Exported stuff. The benefits to wrapping the hand-unrolled iterations here
*  are significant:
*
*     12x sipRound_s_x1 = 73 ns
*      4x sipRound_s_x3 = 45 ns
*/
.global sipRound_s_x1
sipRound_s_x1:
        sipRound
        returnHask

.global sipRound_s_x2
sipRound_s_x2:
        sipRound
        sipRound
        returnHask

.global sipRound_s_x3
sipRound_s_x3:
        sipRound
        sipRound
        sipRound
        returnHask

.global sipRound_s_x4
sipRound_s_x4:
        sipRound
        sipRound
        sipRound
        sipRound
        returnHask
