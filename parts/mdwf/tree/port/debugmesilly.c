#include <string.h>
#include <mdwf.h>
int
qx(debugmesilly)(struct QX(Fermion)          *y,
                 struct Q(State)             *state,
                 const struct Q(Parameters)  *params,
                 const struct QX(Gauge)      *gauge,
                 const char                  *op_name,
                 const struct QX(Fermion)    *x)
{
#define ERROR(fatal, msg) do { status = q(set_error)(state, fatal, msg); \
                               goto end; } while(0)
    int x_size = state->lat_x->full_size;
    int y_size = state->lat_y->full_size;
    double global_norm;
    int Ls = state->Ls;

    long long flops = 0;
    long long sent = 0;
    long long received = 0;
    void *ptr_tmp = 0;
    size_t ptr_size_tmp = 0;
    void *temps = 0;
    int status = 0;
    struct Fermion *t0_x = 0;
    struct Fermion *t1_x = 0;
    struct Fermion *t2_x = 0;
    struct Fermion *t3_x = 0;
    struct Fermion *t0_y = 0;
    struct Fermion *t1_y = 0;
    struct Fermion *t2_y = 0;
    struct Fermion *t3_y = 0;
    const struct SUn *U = gauge->data;
    struct eo_lattice *lat_x = state->lat_x,
                      *lat_y = state->lat_y;

    /* allocate locals */
    ptr_tmp = qx(allocate_xy_)(state, &ptr_size_tmp, &temps,
                            0,  /* header */
                            4,  /* cb_x */
                            4); /* cb_y */
    if (ptr_tmp == 0)
        ERROR(1, "debugmesilly: not enough memory");

    t0_x  = temps;
    t1_x  = temps = qx(step_cb_x)(state, temps);
    t2_x  = temps = qx(step_cb_x)(state, temps);
    t3_x  = temps = qx(step_cb_x)(state, temps);
    t0_y  = temps = qx(step_cb_x)(state, temps);
    t1_y  = temps = qx(step_cb_y)(state, temps);
    t2_y  = temps = qx(step_cb_y)(state, temps);
    t3_y  = temps = qx(step_cb_y)(state, temps);


    BEGIN_TIMING(state);

    /* setup communication */
    if (q(setup_comm)(state, sizeof (double)))
        ERROR(1, "debugmesilly: communication setup failed");

    if (0 == strcmp("eo-prec", op_name)) {
        qx(omp_f_zero)(state, t0_x, x_size, Ls);
        qx(omp_f_zero)(state, y->cb_y, y_size, Ls);
        qx(cg_precondition)(y->cb_x, t1_x, state, params,
                            U, t0_x, x->cb_x, x->cb_y,
                            &flops, &sent, &received,
                            t2_x, t3_x, t0_y);
    } 
    else if (0 == strcmp("eo-inflate", op_name)) {
        qx(cg_inflate)(y->cb_x, y->cb_y,
                       state, params, U, x->cb_y, x->cb_x,
                       &flops, &sent, &received,
                       t0_x, t0_y);
    }
    /* M */
    else if (0 == strcmp("op-M", op_name)) {
        qx(omp_f_zero)(state, y->cb_y, y_size, Ls);
        qx(op_M)(y->cb_x, state, params, U, 
                x->cb_x, &flops, &sent, &received, t0_x, t0_y);
    }
    else if (0 == strcmp("op-Mn", op_name)) {       /* norm discarded */
        qx(omp_f_zero)(state, y->cb_y, y_size, Ls);
        qx(op_Mn)(y->cb_x, &global_norm, state, params, U, 
                x->cb_x, &flops, &sent, &received, t0_x, t0_y);
    }
    else if (0 == strcmp("op-Mx", op_name)) {
        qx(omp_f_zero)(state, y->cb_y, y_size, Ls);
        qx(op_Mx)(y->cb_x, state, params, U, 
                x->cb_x, &flops, &sent, &received, t0_x, t0_y);
    }
    else if (0 == strcmp("op-Mxn", op_name)) {      /* norm discarded */
        qx(omp_f_zero)(state, y->cb_y, y_size, Ls);
        qx(op_Mxn)(y->cb_x, &global_norm, state, params, U, 
                x->cb_x, &flops, &sent, &received, t0_x, t0_y);
    }
    /* A */
    else if (0 == strcmp("op-A", op_name)) {
        qx(op_A)(y->cb_x, lat_x, params, x->cb_x, &flops);
        qx(op_A)(y->cb_y, lat_y, params, x->cb_y, &flops);
    }
    else if (0 == strcmp("op-Ax", op_name)) {
        qx(op_Ax)(y->cb_x, lat_x, params, x->cb_x, &flops);
        qx(op_Ax)(y->cb_y, lat_y, params, x->cb_y, &flops);
    }
    else if (0 == strcmp("op-A1", op_name)) {
        qx(op_A1)(y->cb_x, lat_x, params, x->cb_x, &flops);
        qx(op_A1)(y->cb_y, lat_y, params, x->cb_y, &flops);
    }
    else if (0 == strcmp("op-A1x", op_name)) {
        qx(op_A1x)(y->cb_x, lat_x, params, x->cb_x, &flops);
        qx(op_A1x)(y->cb_y, lat_y, params, x->cb_y, &flops);
    }
    /* B */
    else if (0 == strcmp("op-B", op_name)) {
        qx(op_B)(y->cb_x, lat_x, params, x->cb_x, &flops);
        qx(op_B)(y->cb_y, lat_y, params, x->cb_y, &flops);
    }
    else if (0 == strcmp("op-Bx", op_name)) {
        qx(op_Bx)(y->cb_x, lat_x, params, x->cb_x, &flops);
        qx(op_Bx)(y->cb_y, lat_y, params, x->cb_y, &flops);
    }
    else if (0 == strcmp("op-B1", op_name)) {
        qx(op_B1)(y->cb_x, lat_x, params, x->cb_x, &flops);
        qx(op_B1)(y->cb_y, lat_y, params, x->cb_y, &flops);
    }
    else if (0 == strcmp("op-B1x", op_name)) {
        qx(op_B1x)(y->cb_x, lat_x, params, x->cb_x, &flops);
        qx(op_B1x)(y->cb_y, lat_y, params, x->cb_y, &flops);
    }
    /* F */
    else if (0 == strcmp("op-F", op_name)) {
        qx(op_F)(y->cb_y, lat_y, U, x->cb_x, &flops, &sent, &received);
        qx(op_F)(y->cb_x, lat_x, U, x->cb_y, &flops, &sent, &received);
    }
    else if (0 == strcmp("op-Fx", op_name)) {
        qx(op_Fx)(y->cb_y,  lat_y,  U, x->cb_x, &flops, &sent, &received);
        qx(op_Fx)(y->cb_x, lat_x, U, x->cb_y,  &flops, &sent, &received);
    }
    /* all K */
    else if (0 == strcmp("op-K", op_name)) {
        qx(op_K)(y->cb_x, lat_x, params, x->cb_x, &flops);
        qx(op_K)(y->cb_y, lat_y, params, x->cb_y, &flops);
    }
    else if (0 == strcmp("op-Kx", op_name)) {
        qx(op_Kx)(y->cb_x, lat_x, params, x->cb_x, &flops);
        qx(op_Kx)(y->cb_y, lat_y, params, x->cb_y, &flops);
    }
    else if (0 == strcmp("op-K1", op_name)) {
        qx(op_K1)(y->cb_x, lat_x, params, x->cb_x, &flops);
        qx(op_K1)(y->cb_y, lat_y, params, x->cb_y, &flops);
    }
    else if (0 == strcmp("op-K1x", op_name)) {
        qx(op_K1x)(y->cb_x, lat_x, params, x->cb_x, &flops);
        qx(op_K1x)(y->cb_y, lat_y, params, x->cb_y, &flops);
    }
    /* composites */
    else if (0 == strcmp("op-BA1", op_name)) {
        qx(op_BA1)(y->cb_x, lat_x, params, x->cb_x, &flops);
        qx(op_BA1)(y->cb_y, lat_y, params, x->cb_y, &flops);
    }
    else if (0 == strcmp("op-BA1F", op_name)) {
        qx(op_BA1F)(y->cb_y,  lat_y,  params, U, x->cb_x,
                &flops, &sent, &received);
        qx(op_BA1F)(y->cb_x, lat_x, params, U, x->cb_y,
                &flops, &sent, &received);
    }
    else if (0 == strcmp("op-A1xBx", op_name)) {
        qx(op_A1xBx)(y->cb_x, lat_x, params, x->cb_x, &flops);
        qx(op_A1xBx)(y->cb_y,  lat_y,  params, x->cb_y,  &flops);
    }
    else if (0 == strcmp("op-A1xBxFx", op_name)) {
        qx(op_A1xBxFx)(y->cb_y,  lat_y,  params, U, x->cb_x,
                &flops, &sent, &received);
        qx(op_A1xBxFx)(y->cb_x, lat_x, params, U, x->cb_y,
                &flops, &sent, &received);
    }
    /* composites with K */
    else if (0 == strcmp("op-BA1K1", op_name)) {
        qx(op_BA1K1)(y->cb_x, lat_x, params, x->cb_x, &flops);
        qx(op_BA1K1)(y->cb_y, lat_y, params, x->cb_y, &flops);
    }
    /* (x-op.y) operators require two arguments
       cb_x and cb_y parts of x are used */
    else if (0 == strcmp("op-1mBA1F", op_name)) {
        qx(op_1mBA1F)(y->cb_y, lat_y, params, U, x->cb_y, x->cb_x,
                &flops, &sent, &received);
        qx(op_1mBA1F)(y->cb_x, lat_x, params, U, x->cb_x, x->cb_y,
                &flops, &sent, &received);
    }
    else if (0 == strcmp("op-1mBA1Fn", op_name)) {  /* norm discarded */
        qx(op_1mBA1F_norm)(y->cb_y, &global_norm, lat_y, params, U, 
                x->cb_y, x->cb_x, &flops, &sent, &received);
        qx(op_1mBA1F_norm)(y->cb_x, &global_norm, lat_x, params, U, 
                x->cb_x, x->cb_y, &flops, &sent, &received);
    }
    else if (0 == strcmp("op-1mF", op_name)) {
        qx(op_1mF)(y->cb_y, lat_y, U, x->cb_y, x->cb_x,
                &flops, &sent, &received);
        qx(op_1mF)(y->cb_x, lat_x, U, x->cb_x, x->cb_y,
                &flops, &sent, &received);
    }
    else if (0 == strcmp("op-1mFx", op_name)) {
        qx(op_1mFx)(y->cb_y, lat_y, U, x->cb_y, x->cb_x,
                &flops, &sent, &received);
        qx(op_1mFx)(y->cb_x, lat_x, U, x->cb_x, x->cb_y,
                &flops, &sent, &received);
    }
    else if (0 == strcmp("op-1mFxn", op_name)) {    /* norm discarded */
        qx(op_1mFx_norm)(y->cb_y, &global_norm, lat_y, U, x->cb_y, x->cb_x,
                &flops, &sent, &received);
        qx(op_1mFx_norm)(y->cb_x, &global_norm, lat_x, U, x->cb_x, x->cb_y,
                &flops, &sent, &received);
    }
    /* (x-op.y) with K */
    else if (0 == strcmp("op-1mK1xA1xBxFx", op_name)) {
        qx(op_1mK1xA1xBxFx)(y->cb_y, lat_y, params, U, x->cb_y, x->cb_x,
                &flops, &sent, &received);
        qx(op_1mK1xA1xBxFx)(y->cb_x, lat_x, params, U, x->cb_x, x->cb_y,
                &flops, &sent, &received);
    }
    else if (0 == strcmp("op-1mK1xA1xBxFxn", op_name)) {    /* norm discarded */
        qx(op_1mK1xA1xBxFx_norm)(y->cb_y, &global_norm, lat_y, params, U, 
                x->cb_y, x->cb_x, &flops, &sent, &received);
        qx(op_1mK1xA1xBxFx_norm)(y->cb_x, &global_norm, lat_x, params, U, 
                x->cb_x, x->cb_y, &flops, &sent, &received);
    }
    else if (0 == strcmp("op-1mKF", op_name)) {
        qx(op_1mKF)(y->cb_y, lat_y, params, U, x->cb_y, x->cb_x,
                &flops, &sent, &received);
        qx(op_1mKF)(y->cb_x, lat_x, params, U, x->cb_x, x->cb_y,
                &flops, &sent, &received);
    }
    else if (0 == strcmp("op-1mKFn", op_name)) {    /* norm discarded */
        qx(op_1mKF_norm)(y->cb_y, &global_norm, lat_y, params, U, x->cb_y, x->cb_x,
                &flops, &sent, &received);
        qx(op_1mKF_norm)(y->cb_x, &global_norm, lat_x, params, U, x->cb_x, x->cb_y,
                &flops, &sent, &received);
    }
    /* TODO ops with norms (need 2 results) */
    /* unsupported operator */
    else {
        qx(omp_f_zero)(state, y->cb_x, x_size, Ls);
        qx(omp_f_zero)(state, y->cb_y, y_size, Ls);
        status = 1;
    }

    END_TIMING(state, flops, sent, received);

end:
    /* free memory */
    if (ptr_tmp)
        q(free)(state, ptr_tmp, ptr_size_tmp);
    else if (status != 0)
        q(set_error)(state, 0, "debugmesilly: unknown operator");

    return status;
}

int
QX(debugmesilly)(struct QX(Fermion)          *y,
                 const struct Q(Parameters)  *params,
                 const struct QX(Gauge)      *gauge,
                 const char                  *op_name,
                 const struct QX(Fermion)    *x)
{
    DECLARE_STATE;

    /* check arguments */
    CHECK_ARG0(y);
    CHECK_ARGn(x, "debugmesilly");
    CHECK_ARGn(params, "debugmesilly");
    CHECK_ARGn(gauge, "debugmesilly");

    return qx(debugmesilly)(y, state, params, gauge, op_name, x);
#undef ERROR
}
