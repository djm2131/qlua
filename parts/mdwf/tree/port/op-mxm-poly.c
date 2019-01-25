#include <mdwf.h>

int
QX(MxM_poly)(struct QX(HalfFermion) *result,
             struct QX(HalfFermion) *result_prev,
             const struct Q(Parameters) *params,
             const struct QX(Gauge) *gauge,
             const struct QX(HalfFermion) *fermion,
             int poly_n, 
             const double poly_a[], 
             const double poly_b[], 
             const double poly_c[])
{
    DECLARE_STATE;
    long long flops = 0;
    long long sent = 0;
    long long received = 0;
    size_t alloc_size;
    void *alloc_ptr;
    void *aligned_ptr;
    struct Fermion *tmp_y;
    struct Fermion *tmp_x1,
                   *tmp_x2;
    int Ls;
    int x_size;
    int i;
    struct Fermion *poly_ws_x[3];
    struct Fermion *poly_v0_x, 
                   *poly_v1_x, 
                   *poly_v2_x,
                   *aux;
    
    CHECK_ARG0(result);
    CHECK_ARGn(params, "MxM_poly");
    CHECK_ARGn(gauge, "MxM_poly");
    CHECK_ARGn(fermion, "MxM_poly");

    Ls      = state->Ls;
    x_size  = state->lat_x->full_size;

    if (poly_n <= 0) 
        return q(set_error)(state, 0,
                            "MxM_poly(): degree must be > 0");

    if (0 == poly_a 
            || 0 == poly_b
            || 0 == poly_c)
        return q(set_error)(state, 0,
                            "MxM_poly(): NULL coefficient tables");
    
    if (q(setup_comm)(state, sizeof (REAL)))
        return q(set_error)(state, 0,
                            "MxM_poly(): communication setup failed");

    /* ws for 3-term recurrence & 1 cb_x, 1 cb_y tmp vectors M^\dag.M */
    alloc_ptr = qx(allocate_xy_)(state, &alloc_size, &aligned_ptr, 0, 4, 1);
    if (alloc_ptr == 0)
        return q(set_error)(state, 0, "MxM_poly(): not enough memory");

    poly_ws_x[0]    = result->cb_x;
    poly_ws_x[1]    = aligned_ptr;
    poly_ws_x[2]    = aligned_ptr = qx(step_cb_x)(state, aligned_ptr);
    tmp_x1          = aligned_ptr = qx(step_cb_x)(state, aligned_ptr);
    tmp_x2          = aligned_ptr = qx(step_cb_x)(state, aligned_ptr);
    tmp_y           = aligned_ptr = qx(step_cb_x)(state, aligned_ptr);
    /* arrange pointers so that (poly_n-1) cyc.shifts v0<-v1<-v2
       set v1 == result->cb_x
       originally, orig{v}[k] <- ws[(-k+n)%3]
       after (n-1) shifts, v[k] <- orig{v}[(k+n-1)%3] = ws[(1-k)%3], 
       so that v[1] <- ws[0] = result->cb_x */
    poly_v0_x       = poly_ws_x[(0 + poly_n) % 3];
    poly_v1_x       = poly_ws_x[(2 + poly_n) % 3];
    poly_v2_x       = poly_ws_x[(1 + poly_n) % 3];
   
    BEGIN_TIMING(state); 
    /* v0 <- c0 * v */
    qx(omp_f_copy) (state, poly_v0_x, x_size, Ls, fermion->cb_x); 
    qx(omp_f_rmul1)(state, poly_v0_x, x_size, Ls, poly_c[0]);
    /* v1 <- (a0 + b0*A) . v */
    qx(op_M) (tmp_x1, state, params, gauge->data, fermion->cb_x,
                  &flops, &sent, &received, tmp_x2, tmp_y);
    qx(op_Mx)(poly_v1_x, state, params, gauge->data, tmp_x1,
                  &flops, &sent, &received, tmp_x2, tmp_y);
    qx(omp_f_rmul1)(state, poly_v1_x, x_size, Ls, poly_b[0]);
    qx(omp_f_add2) (state, poly_v1_x, x_size, Ls, poly_a[0], fermion->cb_x);
    
    for (i = 1; i < poly_n ; i++) {
        /* v2 = A.v1 */
        qx(op_M) (tmp_x1, state, params, gauge->data, poly_v1_x,
                      &flops, &sent, &received, tmp_x2, tmp_y);
        qx(op_Mx)(poly_v2_x, state, params, gauge->data, tmp_x1,
                      &flops, &sent, &received, tmp_x2, tmp_y);
        /* v2 *= b */
        qx(omp_f_rmul1)(state, poly_v2_x, x_size, Ls, poly_b[i]);
        /* v2 += a*v1 */
        qx(omp_f_add2) (state, poly_v2_x, x_size, Ls, poly_a[i], poly_v1_x);
        /* v2 += c*v0 */
        qx(omp_f_add2) (state, poly_v2_x, x_size, Ls, poly_c[i], poly_v0_x);

        /* shift v0 <- v1 <- v2*/
        aux         = poly_v0_x;
        poly_v0_x   = poly_v1_x;
        poly_v1_x   = poly_v2_x;
        poly_v2_x   = aux;
        /* the last computed poly is in v1 */
    }
    END_TIMING(state, flops, sent, received);

    if (NULL != result_prev) {
        CHECK_ARGn(result_prev, "MxM_poly");
        qx(omp_f_copy)(state, result_prev->cb_x, x_size, Ls, poly_v0_x);
    }

    q(free)(state, alloc_ptr, alloc_size);

    return 0;
} 
