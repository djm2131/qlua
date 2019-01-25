#include <clover.h>

void *
qx(allocate_eo)(struct Q(State) *state,
                size_t *size, void **aligned_ptr,
                size_t hdr_size, int even_count, int odd_count)
{
  int es = qx(sizeof_fermion)(state->even.full_size);
  int os = qx(sizeof_fermion)(state->odd.full_size);
  size_t total_size;
  void *res;

#define HFSIZE(n,xx) ((n) * ((xx) + CACHE_LINE_SIZE - 1))
  total_size = hdr_size + HFSIZE(even_count, es) + HFSIZE(odd_count, os);
  res = q(allocate_aligned)(state, size, aligned_ptr, hdr_size, total_size);

  return res;
}
