#include <stdint.h>

#define id(n) (n)

#define bswap32 __builtin_bswap32
#define bswap64 __builtin_bswap64

/*
 * Unfortunately GCC does not have a __builtin_bswap16, but
 * this should compile to pretty good assembly.
 */
inline static uint16_t bswap16(uint16_t n)
{
    return (n >> 8) | (n << 8);
}

#define CASE(_type,_bswap)                                 \
case sizeof (_type):                                       \
    /* repeat 'num_reps' times */                          \
    for (r = 0; r < num_reps; ++r) {                       \
        /* skip 'offsets[0]' bytes */                      \
        s += offsets[0];                                   \
        for (o = 1; o < num_offsets; ++o) {                \
            /* copy 'num_values * sizeof (_type)' bytes */ \
            for (v = 0; v < num_values; v++) {             \
                *(_type *)d = _bswap(*(_type *)s);         \
                d += sizeof (_type);                       \
                s += sizeof (_type);                       \
            }                                              \
            /* skip 'offsets[o]' bytes */                  \
            s += offsets[o];                               \
        }                                                  \
    }                                                      \
break;

#define OK    0
#define ERROR 1

/*
 * Copies data from the second memory area (source) into the first
 * memory area (destination) using the specified operations.
 *
 * Returns zero on success, non-zero otherwise.
 */
int data_layout_copy
    ( char **dst       /* destination memory area */
    , char **src       /* source memory area */
    , int num_reps     /* number of times to repeat the copy instructions */
    , int num_offsets  /* number of skip operations in 'offsets' */
    , int *offsets     /* list with number of bytes to skip in between each copy */
    , int num_values   /* number of values to copy in between each skip */
    , int value_size   /* size of a single value in bytes */
    , int swap_bytes   /* non-zero to swap the byte order of values */
    )
{
    int r, o, v;
    char *d, *s;

    d = *dst;
    s = *src;

    if (swap_bytes) {
        switch (value_size) {
            CASE (uint64_t, bswap64);
            CASE (uint32_t, bswap32);
            CASE (uint16_t, bswap16);
            CASE (uint8_t,  id);
            default: return ERROR;
        }
    } else {
        switch (value_size) {
            CASE (uint64_t, id);
            CASE (uint32_t, id);
            CASE (uint16_t, id);
            CASE (uint8_t,  id);
            default: return ERROR;
        }
    }

    *dst = d;
    *src = s;

    return OK;
}
