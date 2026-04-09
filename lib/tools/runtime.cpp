#include <stddef.h>
#include <stdint.h>

extern "C" {

static uint32_t divmod10_u128(__uint128_t* val) {
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

char* unnitoa(__int128 val, char* buf) {
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

    if (neg) temp[i++] = '-';

    // Reverse into buf
    int j = 0;
    while (i > 0) buf[j++] = temp[--i];

    buf[j] = '\0';
    return buf;
}

char* unn_strcat(char* dest, const char* src) {
    char* rd = dest;
    while (*rd) rd++;  // Find the end of dest
    while (*src) {     // Copy src
        *rd++ = *src++;
    }
    *rd = '\0';
    return dest;
}

char* unnftoa(double val, char* buf) {
    if (val < 0) {
        *buf++ = '-';
        val = -val;
    }

    // Convert to 64-bit int instead of 128-bit
    // The CPU has a native instruction for double -> int64 (CVTTSD2SI)
    int64_t ipart = (int64_t)val;
    double fpart = val - (double)ipart;

    // Use  existing unnitoa for the head
    // (Cast ipart to __int128 here is safe/native sign-extension)
    unnitoa((__int128)ipart, buf);

    //  Move to the end of the integer part
    while (*buf) buf++;
    *buf++ = '.';

    //  Handle fractional part
    for (int i = 0; i < 6; i++) {
        fpart *= 10;
        int digit = (int)fpart;
        *buf++ = '0' + digit;
        fpart -= (double)digit;  // Native double -> int64 math
    }
    *buf = '\0';
    return buf;
}

// IPTOA (Pointer to ASCII - Hexadecimal)
char* unniptoa(uintptr_t val, char* buf) {
    const char* hex_digits = "0123456789ABCDEF";
    char temp[20];
    int i = 0;

    if (val == 0) {
        buf[0] = '0';
        buf[1] = 'x';
        buf[2] = '0';
        buf[3] = '\0';
        return buf;
    }

    while (val > 0) {
        temp[i++] = hex_digits[val % 16];
        val /= 16;
    }

    int j = 0;
    buf[j++] = '0';
    buf[j++] = 'x';
    while (i > 0) {
        buf[j++] = temp[--i];
    }
    buf[j] = '\0';
    return buf;
}
}
