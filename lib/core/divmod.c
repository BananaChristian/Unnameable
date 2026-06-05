#include <stddef.h>
#include <stdint.h>

uint32_t divmod10_u128(__uint128_t* val) {
    uint64_t high = (uint64_t)(*val >> 64);
    uint64_t low = (uint64_t)(*val);
    uint64_t quotient_high, quotient_low, remainder;

    __asm__("divq %[divisor]"
            : "=a"(quotient_high), "=d"(remainder)
            : "a"(high), "d"(0ULL), [divisor] "r"(10ULL));

    __asm__("divq %[divisor]"
            : "=a"(quotient_low), "=d"(remainder)
            : "a"(low), "d"(remainder), [divisor] "r"(10ULL));

    *val = ((__uint128_t)quotient_high << 64) | quotient_low;
    return (uint32_t)remainder;
}
