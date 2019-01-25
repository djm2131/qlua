#ifndef MARK_880e534e_c7fe_47d1_b228_a84c42fd75d5
#define MARK_880e534e_c7fe_47d1_b228_a84c42fd75d5

/* Derived from RFC 1231. The original copyright may apply
 *
 * Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All
 * rights reserved.
 *
 * License to copy and use this software is granted provided that it
 * is identified as the "RSA Data Security, Inc. MD5 Message-Digest
 * Algorithm" in all material mentioning or referencing this software
 * or this function.
 *
 * License is also granted to make and use derivative works provided
 * that such works are identified as "derived from the RSA Data
 * Security, Inc. MD5 Message-Digest Algorithm" in all material
 * mentioning or referencing the derived work.
 *
 * RSA Data Security, Inc. makes no representations concerning either
 * the merchantability of this software or the suitability of this
 * software for any particular purpose. It is provided "as is"
 * without express or implied warranty of any kind.
 *
 * These notices must be retained in any copies of any part of this
 * documentation and/or software.
 */

struct AffMD5_s {
    uint32_t state[4];
    uint32_t count[2];
    uint8_t buffer[64];
};

void aff_md5_init(struct AffMD5_s *);
void aff_md5_update(struct AffMD5_s *, const uint8_t *, uint32_t);
void aff_md5_final(uint8_t [16], struct AffMD5_s *);

#endif /* !defined(MARK_880e534e_c7fe_47d1_b228_a84c42fd75d5) */
