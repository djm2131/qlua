#define QOP_MDWF_DEFAULT_PRECISION 'F'
#include <mdwf.h>

int
QX(deflator_add_vector)(const struct Q(Parameters)  *params,
                        const struct QX(Gauge)      *gauge,
                        struct QX(Deflator) *deflator,
                        const struct QX(HalfFermion) *hfermion)
{
    DECLARE_STATE;
    long long flops = 0;
    long long sent = 0;
    long long received = 0;
    int status;
    struct qx(MxM_workspace)  ws;
    void *ptr = 0;
    void *temps = 0;
    size_t ptr_size = 0;
    qx(defl_vec) lv, cur_v;

    defl_vec_set_null(&cur_v);

    /* check arguments */
    CHECK_ARG0(params);
    CHECK_ARGn(gauge, "deflator_add_vector");
    CHECK_ARGn(deflator, "deflator_add_vector");
    CHECK_ARGn(hfermion, "deflator_add_vector");

    if (deflator->loading == 0) {
        status = q(set_error)(state, 0, "deflator_add_vector(): not in loading state");
        goto clearerr;
    }

    /* setup communication */
    if (q(setup_comm)(state, sizeof (REAL))) {
        status = q(set_error)(state, 0, "deflator_add_vector(): communication setup failed");
        goto clearerr;
    }

    /* allocate temps */
    ptr = qx(allocate_xy_)(state, &ptr_size, &temps,
                        0,  /* header */
                        2,  /* cb_x */
                        1); /* cb_y */
    if (ptr == 0)
        return q(set_error)(state, 0, "deflator_add_vector(): not enough memory");

    BEGIN_TIMING(state);

    ws.state     = state;
    ws.params    = params;
    ws.gauge     = gauge->data;
    ws.tmp_x     = temps;
    ws.tmp2_x    = temps = qx(step_cb_x)(state, temps);
    ws.tmp_y     = temps = qx(step_cb_x)(state, temps);
    ws.flops     = &flops;
    ws.sent      = &sent;
    ws.received  = &received;

    lv = qx(defl_vec_view)(state, hfermion->cb_x);
    /* qx(defl_inject) modifies the injected vector, so copy it */
    cur_v = qx(defl_vec_alloc)(state);
    if (defl_vec_is_null(&cur_v)) {
        status = q(set_error)(state, 0, "deflator_add_vector(): not enough memory");
        goto clearerr;
    }
    qx(defl_vec_copy)(state, lv, cur_v);
    qx(defl_inject_back)(deflator, &ws, cur_v, N_REORTHO_ADDVEC);

    END_TIMING(state, flops, sent, received);
    if (NULL != ptr)
        q(free)(state, ptr, ptr_size);
    if ( ! defl_vec_is_null(&cur_v))
        qx(defl_vec_free)(state, &cur_v);
    return 0;

clearerr:
    if (NULL != ptr) 
        q(free)(state, ptr, ptr_size);
    if ( ! defl_vec_is_null(&cur_v))
        qx(defl_vec_free)(state, &cur_v);
    return status;
}
