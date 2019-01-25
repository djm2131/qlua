#ifndef MARK_b7d77b2a_6c98_4b28_954e_957e416e0a19
#define MARK_b7d77b2a_6c98_4b28_954e_957e416e0a19

/* Encoding and decoding routines
 *
 * encode_*() returns NULL if there is not enough space in the buffer.
 * decode_*() returns a number of bytes consumed from the buffer. No bytes means
 *            an error.
 */
enum AffNodeType_e;

uint8_t *aff_encode_type(uint8_t *buf, uint32_t size, enum AffNodeType_e type);
uint8_t *aff_encode_u32(uint8_t *buf, uint32_t size, uint32_t data);
uint8_t *aff_encode_u64(uint8_t *buf, uint32_t size, uint64_t data);
uint8_t *aff_encode_double(uint8_t *buf, uint32_t size, double data);

uint8_t *aff_decode_type(enum AffNodeType_e *p, uint8_t *buf, uint32_t size);
uint8_t *aff_decode_u32(uint32_t *p, uint8_t *buf, uint32_t size);
uint8_t *aff_decode_u64(uint64_t *p, uint8_t *buf, uint32_t size);
uint8_t *aff_decode_double(double *p, uint8_t *buf, uint32_t size);

#endif /* !defined(MARK_b7d77b2a_6c98_4b28_954e_957e416e0a19) */
