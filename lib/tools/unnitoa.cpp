#include <stddef.h>
#include <stdint.h> 

extern "C" {

static uint32_t divmod10_u128(__uint128_t *val) {
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

char *unnitoa(__int128 val, char *buf) {
  char temp[64];
  int i = 0;
  int neg = 0;

  if (val == 0) {
    buf[0] = '0';
    buf[1] = '\0';
    return buf;
  }

  // Handle negative numbers by converting to unsigned for the math
  __uint128_t uval;
  if (val < 0) {
    neg = 1;
    uval = (__uint128_t)-val;
  } else {
    uval = (__uint128_t)val;
  }

  while (uval > 0) {
    temp[i++] = '0' + divmod10_u128(&uval);
  }

  if (neg)
    temp[i++] = '-';

  // Reverse into buf
  int j = 0;
  while (i > 0)
    buf[j++] = temp[--i];

  buf[j] = '\0';
  return buf;
}

} 
