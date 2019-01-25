#include <mdwf.h>

/* NOTE: not the best overlap of comm/comm */

int
QX(DDW_operator)(struct QX(Fermion) *result,
                 const struct Q(Parameters) *params,
                 const struct QX(Gauge) *gauge,
                 const struct QX(Fermion) *source)
{
    DECLARE_STATE;
    long long flops = 0;
    long long sent = 0;
    long long received = 0;
    size_t alloc_size;
    void *alloc_ptr;
    void *aligned_ptr;
    struct Fermion *t_x;
    struct Fermion *t_y;
    
    CHECK_ARG0(result);
    CHECK_ARGn(params, "DDW_operator");
    CHECK_ARGn(gauge, "DDW_operator");
    CHECK_ARGn(source, "DDW_operator");
    
    if (q(setup_comm)(state, sizeof (REAL)))
      return q(set_error)(state, 0,
                          "DDW_operator(): communication setup failed");
    
    alloc_ptr = qx(allocate_xy_)(state, &alloc_size, &aligned_ptr, 0, 1, 1);
    if (alloc_ptr == 0)
        return q(set_error)(state, 0, "DDW_operator(): not enough memory");
    
    t_x = aligned_ptr;
    t_y = aligned_ptr = qx(step_cb_x)(state, aligned_ptr);
    
    BEGIN_TIMING(state);
    qx(op_D)(result->cb_x, state->lat_x, state->lat_y, params, gauge->data,
             source->cb_x, source->cb_y,
             &flops, &sent, &received, t_y);
    qx(op_D)(result->cb_y, state->lat_y, state->lat_x, params, gauge->data,
             source->cb_y, source->cb_x,
             &flops, &sent, &received, t_x);
    END_TIMING(state, flops, sent, received);
    
    q(free)(state, alloc_ptr, alloc_size);

    return 0;
}       
