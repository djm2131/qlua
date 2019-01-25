#define DECLARE_FLOPS() \
    size_t lx_idx; size_t lx_limit = state->threads; size_t lx_flops = 0
#define LOOP_FLOPS(x) \
  for (lx_idx = 0; lx_idx < lx_limit; lx_idx++) { \
    size_t lx_low = (lx_idx * size) / lx_limit; \
    size_t lx_high = ((lx_idx + 1) * size) / lx_limit; \
    size_t lx_count = lx_high - lx_low; \
    lx_flops += x; \
  } \
  return lx_flops

#define DECLARE_VOID() \
    size_t lx_idx; size_t lx_limit = state->threads
#define LOOP_VOID(x) \
  for (lx_idx = 0; lx_idx < lx_limit; lx_idx++) { \
    size_t lx_low = (lx_idx * size) / lx_limit; \
    size_t lx_high = ((lx_idx + 1) * size) / lx_limit; \
    size_t lx_count = lx_high - lx_low; \
    x; \
  } \
  return

#define DECLARE_NORM2() \
  size_t lx_idx; size_t lx_limit = state->threads; size_t lx_flops = 0; double lx_local_norm = 0.0
#define LOOP_NORM2(n2, x)			  \
  for (lx_idx = 0; lx_idx < lx_limit; lx_idx++) { \
    size_t lx_low = (lx_idx * size) / lx_limit; \
    size_t lx_high = ((lx_idx + 1) * size) / lx_limit; \
    size_t lx_count = lx_high - lx_low; \
    double lx_norm = 0; \
    lx_flops += x; \
    lx_local_norm += lx_norm; \
  } \
  n2 = lx_local_norm; \
  return lx_flops
