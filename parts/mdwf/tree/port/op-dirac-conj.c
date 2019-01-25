#include <mdwf.h>

int
QX(DDW_operator_conjugated)(struct QX(Fermion) *result,
			    const struct Q(Parameters) *params,
			    const struct QX(Gauge) *gauge,
			    const struct QX(Fermion) *fermion)
{
    DECLARE_STATE;
    long long flops = 0;
    long long sent = 0;
    long long received = 0;
    
    CHECK_ARG0(result);
    CHECK_ARGn(params, "DDW_operator");
    CHECK_ARGn(gauge, "DDW_operator");
    CHECK_ARGn(fermion, "DDW_operator");
    
    if (q(setup_comm)(state, sizeof (REAL)))
	return q(set_error)(state, 0, "DDW_operator_conjugated(): communication setup failed");
    
    BEGIN_TIMING(state);
    qx(op_AxpBxFx)(result->cb_x, state->lat_x, params,
		   gauge->data, fermion->cb_x, fermion->cb_y,
		   &flops, &sent, &received);
    qx(op_AxpBxFx)(result->cb_y, state->lat_y, params,
		   gauge->data, fermion->cb_y, fermion->cb_x,
		   &flops, &sent, &received);
    END_TIMING(state, flops, sent, received);

    return 0;
}	
