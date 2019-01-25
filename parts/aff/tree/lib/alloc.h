#ifndef MARK_9001A65C_280F_41BE_B70F_2AC671835797
#define MARK_9001A65C_280F_41BE_B70F_2AC671835797

/* Default AFF allocator
 *
 * aff_realloc  allocates and frees heap memory. It behaves almost like
 *              realloc from stdlib, but if size == 0, the memory pointed by
 *              ptr is freed and NULL is returned.
 *              This function could be overwritten by the user.
 */
void *aff_realloc(void *ptr, size_t size);

#endif /* !defined(MARK_9001A65C_280F_41BE_B70F_2AC671835797) */
