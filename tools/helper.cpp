extern "C" char *unnitoa(__int128 val, char *buf)
{
    char temp[64];  // Enough for 128-bit decimal digits (max ~39 digits).
    int i = 0;
    int neg = 0;

    if (val == 0) {
        buf[0] = '0';
        buf[1] = '\0';
        return buf;
    }

    if (val < 0) {
        neg = 1;
        val = -val;
    }

    // Extract digits into temp (reversed)
    while (val > 0) {
        __int128 digit = val % 10;
        val /= 10;
        temp[i++] = '0' + (int)digit;
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