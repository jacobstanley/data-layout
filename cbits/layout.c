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

/*
 * Defines an encode/decode case for the specified type
 * and byte order.
 *
 * enc    The memory area (src or dst) which contains or will
 *        contain the encoded data. During encoding this will
 *        be 'dst' and during decoding this will be 'src'.
 *
 * type   The type of the values to copy.
 *
 * bswap  The function to use to swap the byte order. Pass
 *        the identity function if no byte swapping is
 *        required.
 */
#define CASE(_enc,_type,_bswap)                            \
case sizeof (_type):                                       \
    /* repeat 'num_reps' times */                          \
    for (r = 0; r < num_reps; ++r) {                       \
        /* skip 'offsets[0]' bytes */                      \
        _enc += offsets[0];                                \
        for (o = 1; o < num_offsets; ++o) {                \
            /* copy 'num_values * sizeof (_type)' bytes */ \
            for (v = 0; v < num_values; v++) {             \
                *(_type *)dst = _bswap(*(_type *)src);     \
                dst += sizeof (_type);                     \
                src += sizeof (_type);                     \
            }                                              \
            /* skip 'offsets[o]' bytes */                  \
            _enc += offsets[o];                            \
        }                                                  \
    }                                                      \
break;

#define OK    0
#define ERROR 1

/*
 * data_layout_encode / data_layout_decode
 *
 * Copies data from the second memory area (source) into the first
 * memory area (destination) using the specified operations.
 *
 * dst          destination memory area
 * src          source memory area
 * num_reps     number of times to repeat the decode
 * num_offsets  number of skip operations in 'offsets'
 * offsets      list with number of bytes to skip in between each copy
 * num_values   number of values to copy in between each skip
 * value_size   size of a single value in bytes
 * swap_bytes   non-zero to swap the byte order of values
 *
 * Returns zero on success, non-zero otherwise.
 */

#define CODEC_FN(_name,_enc)                               \
int data_layout_##_name                                    \
    ( char *dst                                            \
    , char *src                                            \
    , int num_reps                                         \
    , int num_offsets                                      \
    , int *offsets                                         \
    , int num_values                                       \
    , int value_size                                       \
    , int swap_bytes                                       \
    )                                                      \
{                                                          \
    int r, o, v;                                           \
                                                           \
    if (swap_bytes) {                                      \
        switch (value_size) {                              \
            CASE (_enc, uint64_t, bswap64);                \
            CASE (_enc, uint32_t, bswap32);                \
            CASE (_enc, uint16_t, bswap16);                \
            CASE (_enc, uint8_t,  id);                     \
            default: return ERROR;                         \
        }                                                  \
    } else {                                               \
        switch (value_size) {                              \
            CASE (_enc, uint64_t, id);                     \
            CASE (_enc, uint32_t, id);                     \
            CASE (_enc, uint16_t, id);                     \
            CASE (_enc, uint8_t,  id);                     \
            default: return ERROR;                         \
        }                                                  \
    }                                                      \
                                                           \
    return OK;                                             \
}

CODEC_FN (encode, dst)
CODEC_FN (decode, src)
