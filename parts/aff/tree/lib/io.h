#ifndef MARK_b1deb73e_c3c5_49b5_bb41_b2d7422521d6
#define MARK_b1deb73e_c3c5_49b5_bb41_b2d7422521d6

/* There seems no portable way (as of 2007) to position into a file given
 * a 64-bit offset. Here is a function that encapsulates the positioning
 * operation. It always searches from the beginning of the file and returns 0
 * if the positioning operation was successful. We do not store the error code
 * here, since it's also non-portable.
 */
int aff_file_setpos(FILE *f, uint64_t position);

#endif /* !defined(MARK_b1deb73e_c3c5_49b5_bb41_b2d7422521d6) */
