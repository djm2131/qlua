#include <clover.h>

int
QX(D_operator_conjugated)(struct QX(Fermion) *result,
                          const struct QX(Gauge) *gauge,
                          const struct QX(Fermion) *fermion)
{
    DECLARE_STATE;
    long long flops = 0;
    long long sent = 0;
    long long received = 0;
    
    CHECK_ARG0(result);
    CHECK_ARGn(gauge, "D_operator");
    CHECK_ARGn(fermion, "D_operator");
    
    if (q(setup_comm)(state, sizeof (REAL)))
        return q(set_error)(state, 0, "D_operator_conjugated(): communication setup failed");
    
    BEGIN_TIMING(state);
    qx(op_AxpBx)(result->even, &state->even, gauge->g_data, gauge->ce_data,
                 fermion->even, fermion->odd, &flops, &sent, &received);
    qx(op_AxpBx)(result->odd, &state->odd, gauge->g_data, gauge->co_data,
                 fermion->odd, fermion->even, &flops, &sent, &received);
    END_TIMING(state, flops, sent, received);

    return 0;
}       
