#include <mdwf.h>

int
QX(allocate_vector_fermion)(struct QX(VectorFermion) **vfermion_ptr,
                            struct Q(State) *state,
                            int count)
{
    struct QX(VectorFermion) *vfermion;
    void *cb_x;
    size_t size;
    
    if (state == NULL || state->error_latched)
        return 1;
    
    if (vfermion_ptr == NULL)
        return q(set_error)(state, 0,
                            "allocate_vector_fermion(): NULL pointer");
    
    *vfermion_ptr = NULL;
    vfermion = qx(allocate_xy_)(state, &size, &cb_x,
                               sizeof (struct QX(Fermion)), count, 0);
    if (vfermion == 0)
        return q(set_error)(state, 0,
                            "allocate_vector_fermion(): not enough memory");
    
    BEGIN_TIMING(state);
    vfermion->state = state;
    vfermion->size = size;
    vfermion->cb_x = cb_x;
    vfermion->count = count;
    *vfermion_ptr = vfermion;
    END_TIMING(state, 0, 0, 0);

    return 0;
}
