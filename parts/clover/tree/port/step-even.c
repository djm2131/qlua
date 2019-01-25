#include <clover.h>

void *
qx(step_even)(struct Q(State) *state, void *aligned_ptr)
{
    int size = sizeof (REAL) * 2 * Q(COLORS) * Q(FERMION_DIM);
    void *res;

    if (state == 0 || aligned_ptr == 0)
        return NULL;
    res = ALIGN(aligned_ptr, state->even.full_size * size);

    return res;
}
