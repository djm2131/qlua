#include <mdwf.h>

#define HFSIZE(n,xx) ((n) * ((xx) + CACHE_LINE_SIZE - 1))

void *
qx(allocate_eo_)(struct Q(State) *state,    /* [sns] rename for dev */
                size_t *size, void **aligned_ptr,
                size_t hdr_size, int even_count, int odd_count)
{
  int es = qx(sizeof_fermion)(state->even_.full_size, state->Ls);
  int os = qx(sizeof_fermion)(state->odd_.full_size, state->Ls);
  size_t total_size;

  total_size = hdr_size + HFSIZE(even_count, es) + HFSIZE(odd_count, os);

  return q(allocate_aligned)(state, size, aligned_ptr, hdr_size, total_size);
}

void *
qx(step_even_)(struct Q(State) *state, void *aligned_ptr)
{
    int size = 2 * Q(COLORS) * Q(FERMION_DIM) * sizeof (REAL);

    if (state == 0 || aligned_ptr == 0)
        return NULL;
    return ALIGN(aligned_ptr, state->Ls * state->even_.full_size * size);
}

void *
qx(step_odd_)(struct Q(State) *state, void *aligned_ptr)
{
    int size = 2 * Q(COLORS) * Q(FERMION_DIM) * sizeof (REAL);

    if (state == 0 || aligned_ptr == 0)
        return NULL;
    return ALIGN(aligned_ptr, state->Ls * state->odd_.full_size * size);
}


void *
qx(allocate_xy_)(struct Q(State) *state, 
                size_t *size, void **aligned_ptr,
                size_t hdr_size, int x_count, int y_count)
{
  int xs = qx(sizeof_fermion)(state->lat_x->full_size, state->Ls);
  int ys = qx(sizeof_fermion)(state->lat_y->full_size, state->Ls);
  size_t total_size;

  total_size = hdr_size + HFSIZE(x_count, xs) + HFSIZE(y_count, ys);

  return q(allocate_aligned)(state, size, aligned_ptr, hdr_size, total_size);
}

void *
qx(step_cb_x)(struct Q(State) *state, void *aligned_ptr)
{
    int size = 2 * Q(COLORS) * Q(FERMION_DIM) * sizeof (REAL);

    if (state == 0 || aligned_ptr == 0)
        return NULL;
    return ALIGN(aligned_ptr, state->Ls * state->lat_x->full_size * size);
}

void *
qx(step_cb_y)(struct Q(State) *state, void *aligned_ptr)
{
    int size = 2 * Q(COLORS) * Q(FERMION_DIM) * sizeof (REAL);

    if (state == 0 || aligned_ptr == 0)
        return NULL;
    return ALIGN(aligned_ptr, state->Ls * state->lat_y->full_size * size);
}
