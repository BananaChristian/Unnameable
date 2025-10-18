extern "C" char *unnitoa(int val, char *buf)
{
    char *p = buf;
    if (val < 0)
    {
        *p++ = '-';
        val = -val;
    }

    // Remember start of digits
    char *start = p;

    // Convert digits
    do
    {
        *p++ = '0' + (val % 10);
        val /= 10;
    } while (val);

    // Null terminate
    *p = '\0';

    // Reverse digits in place
    char *end = p - 1;
    while (start < end)
    {
        char tmp = *start;
        *start++ = *end;
        *end-- = tmp;
    }

    return buf;
}