#include <clover.h>

/* NOTE: not the best overlap of comm/comm */

int
QX(D_operator)(struct QX(Fermion) *result,
               const struct QX(Gauge) *gauge,
               const struct QX(Fermion) *source)
{
    DECLARE_STATE;
    long long flops = 0;
    long long sent = 0;
    long long received = 0;
    
    CHECK_ARG0(result);
    CHECK_ARGn(gauge, "D_operator");
    CHECK_ARGn(source, "D_operator");
    
    if (q(setup_comm)(state, sizeof (REAL)))
      return q(set_error)(state, 0,
                          "D_operator(): communication setup failed");
    
    BEGIN_TIMING(state);
    qx(op_ApB)(result->even, &state->even, gauge->g_data, gauge->ce_data,
               source->even, source->odd, &flops, &sent, &received);
    qx(op_ApB)(result->odd, &state->odd, gauge->g_data, gauge->co_data,
               source->odd, source->even, &flops, &sent, &received);
    END_TIMING(state, flops, sent, received);
    
    return 0;
}       
