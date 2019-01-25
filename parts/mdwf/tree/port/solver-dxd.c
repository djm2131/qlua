#include <mdwf.h>

/* Solve
 *   M^\dagget M psi = eta
 *
 * with psi as an initial guess
 *
 */ 

#define MAX_OPTIONS (Q(LOG_CG_RESIDUAL)     | \
                     Q(LOG_TRUE_RESIDUAL)   | \
                     Q(FINAL_CG_RESIDUAL))

static void
qx(op_d)(struct QX(Fermion)          *result,
         struct Q(State)             *state,
         const struct Q(Parameters)  *params,
         const struct QX(Gauge)      *gauge,
         const struct QX(Fermion)    *source,
         long long                   *flops,
         long long                   *sent,
         long long                   *received,
         struct Fermion              *tmp_x,
         struct Fermion              *tmp_y)
{
    qx(op_D)(result->cb_x, state->lat_x, state->lat_y, params, gauge->data,
             source->cb_x, source->cb_y,
             flops, sent, received, tmp_y);
    qx(op_D)(result->cb_y, state->lat_y, state->lat_x, params, gauge->data,
             source->cb_y, source->cb_x,
             flops, sent, received, tmp_x);
}

static void
qx(op_dx)(struct QX(Fermion)          *result,
          struct Q(State)             *state,
          const struct Q(Parameters)  *params,
          const struct QX(Gauge)      *gauge,
          const struct QX(Fermion)    *source,
          long long                   *flops,
          long long                   *sent,
          long long                   *received)
{
    qx(op_AxpBxFx)(result->cb_x, state->lat_x, params,
                   gauge->data, source->cb_x, source->cb_y,
                   flops, sent, received);
    qx(op_AxpBxFx)(result->cb_y, state->lat_y, params,
                   gauge->data, source->cb_y, source->cb_x,
                   flops, sent, received);
}

static void
qx(op_dn)(struct QX(Fermion)          *result,
          double                      *norm,
          struct Q(State)             *state,
          const struct Q(Parameters)  *params,
          const struct QX(Gauge)      *gauge,
          const struct QX(Fermion)    *source,
          long long                   *flops,
          long long                   *sent,
          long long                   *received,
          struct Fermion              *tmp_x,
          struct Fermion              *tmp_y)      
{
    double n_x, n_y;

    qx(op_D_norm)(result->cb_x, &n_x,
                  state->lat_x, state->lat_y, params, gauge->data,
                  source->cb_x, source->cb_y,
                  flops, sent, received, tmp_y);
    qx(op_D_norm)(result->cb_y, &n_y,
                  state->lat_y, state->lat_x, params, gauge->data,
                  source->cb_y, source->cb_x,
                  flops, sent, received, tmp_x);
    *norm = n_x + n_y;
    QMP_sum_double(norm);
}

static void
qx(F_copy)(struct QX(Fermion)      *result,
           struct Q(State)         *state,
           const struct Q(Fermion) *source)
{
    int Ls = state->Ls;

    qx(omp_f_copy)(state, result->cb_x, state->lat_x->full_size, Ls, source->cb_x);
    qx(omp_f_copy)(state, result->cb_y, state->lat_y->full_size, Ls, source->cb_y);
}

static int
qx(F_norm2)(const struct QX(Fermion) *fermion,
            double                   *n_f,
            struct Q(State)          *state)
{
    int Ls = state->Ls;
    int flops = 0;
    double n_x, n_y;

    flops += qx(omp_f_norm)(state, &n_x, state->lat_x->full_size, Ls, fermion->cb_x);
    flops += qx(omp_f_norm)(state, &n_y, state->lat_y->full_size, Ls, fermion->cb_y);
    *n_f = n_x + n_y;
    QMP_sum_double(n_f);

    return flops;
}

static int
qx(F_madd3)(struct QX(Fermion)        *r,
            struct Q(State)           *state,
            const struct QX(Fermion)  *a,
            double                     s,
            const struct QX(Fermion)  *b)
{
    int flops = 0;
    int Ls = state->Ls;

    flops += qx(omp_f_add3)(state, r->cb_x, state->lat_x->full_size, Ls, a->cb_x, s, b->cb_x);
    flops += qx(omp_f_add3)(state, r->cb_y, state->lat_y->full_size, Ls, a->cb_y, s, b->cb_y);

    return flops;
}

static int
qx(F_madd2)(struct QX(Fermion)        *r,
            struct Q(State)           *state,
            double                     s,
            const struct QX(Fermion)  *b)
{
    int flops = 0;
    int Ls = state->Ls;

    flops += qx(omp_f_add2)(state, r->cb_x, state->lat_x->full_size, Ls, s, b->cb_x);
    flops += qx(omp_f_add2)(state, r->cb_y, state->lat_y->full_size, Ls, s, b->cb_y);
    
    return flops;
}

static int
qx(F_madd2_norm)(struct QX(Fermion)         *r,
                 double                     *norm,
                 struct Q(State)            *state,
                 double                     s,
                 const struct QX(Fermion)   *b)
{
    int flops = 0;
    int Ls = state->Ls;
    double n_x, n_y;

    flops += qx(omp_f_add2_norm)(state, r->cb_x, &n_x, state->lat_x->full_size, Ls, s, b->cb_x);
    flops += qx(omp_f_add2_norm)(state, r->cb_y, &n_y, state->lat_y->full_size, Ls, s, b->cb_y);
    *norm = n_x + n_y;
    QMP_sum_double(norm);

    return flops;
}

static int
qx(F_xmadd)(struct QX(Fermion) *xi,
            struct QX(Fermion) *pi,
            struct Q(State)    *state,
            double a,
            double b,
            const struct QX(Fermion) *rho)
{
    int flops = 0;
    int Ls = state->Ls;

    flops += qx(omp_cg_xp)(state, xi->cb_x, pi->cb_x, state->lat_x->full_size, Ls, a, b, rho->cb_x);
    flops += qx(omp_cg_xp)(state, xi->cb_y, pi->cb_y, state->lat_y->full_size, Ls, a, b, rho->cb_y);

    return flops;
}

int
QX(DxD_CG)(struct QX(Fermion)            *psi,
           int                           *out_iterations,
           double                        *out_epsilon,
           const struct Q(Parameters)    *params,
           const struct QX(Fermion)      *psi_0,
           const struct QX(Gauge)        *gauge,
           const struct QX(Fermion)      *eta,
           int                            max_iterations,
           double                         min_epsilon,
           unsigned int                   options)
{
    DECLARE_STATE;
    long long flops = 0;
    long long sent = 0;
    long long received = 0;
    void *ptr = 0;
    size_t ptr_size = 0;
    void *temps = 0;
    int status = 1;
    struct QX(Fermion) *rho = 0;
    struct QX(Fermion) *pi = 0;
    struct QX(Fermion) *omega = 0;
    struct QX(Fermion) *zeta = 0;
    struct Fermion *tmp_x = 0;
    struct Fermion *tmp_y = 0;
    double rhs_norm = 0;
    static char *no_mem = "DxD_CG(): not enough memory";
    char *e_msg = 0;
    int k;
    double r, g, a, b, omega_norm;
#define CG_ERROR(msg) do { e_msg = msg; goto end; } while (0)

    /* check arguments */
    CHECK_ARG0(psi);
    CHECK_ARGn(psi_0, "DxD_CG");
    CHECK_ARGn(params, "DxD_CG");
    CHECK_ARGn(gauge, "DxD_CG");
    CHECK_ARGn(eta, "DxD_CG");

    /* setup communication */
    if (q(setup_comm)(state, sizeof (REAL))) {
        return q(set_error)(state, 0, "DxD_CG(): communication setup failed");
    }

    /* allocate locals */
    ptr = qx(allocate_xy_)(state, &ptr_size, &temps,
                          0,  /* header */
                          1,  /* cb_x */
                          1); /* cb_y */
    if (ptr == 0)
        CG_ERROR(no_mem);

    tmp_x  = temps;
    tmp_y  = temps = qx(step_cb_x)(state, temps);

    if (QX(allocate_fermion)(&rho, state) ||
        QX(allocate_fermion)(&pi, state) ||
        QX(allocate_fermion)(&rho, state) ||
        QX(allocate_fermion)(&zeta, state))
        CG_ERROR(no_mem);

    /* clear bits we do not understand */
    options = options & MAX_OPTIONS;

    BEGIN_TIMING(state);
    /* compute the norm of the RHS */
    flops += qx(omp_f_norm)(state, &rhs_norm, state->lat_x->full_size, state->Ls, eta->cb_x);
    if (options) {
        qx(zprint)(state, "DxD CG", "rhs norm %e normalized epsilon %e",
                   rhs_norm, min_epsilon * rhs_norm);
    }

    qx(F_copy)(psi, state, psi_0);
    qx(op_d)(pi, state, params, gauge, psi,
             &flops, &sent, &received, tmp_x, tmp_y);
    qx(op_dx)(omega, state, params, gauge, pi,
              &flops, &sent, &received);
    flops += qx(F_madd3)(rho, state, eta, -1.0, omega);
    qx(F_copy)(pi, state, rho);
    flops += qx(F_norm2)(rho, &r, state);
    for (k = 0; k < max_iterations; k++) {
        qx(op_dn)(omega, &omega_norm, state, params, gauge, pi,
                  &flops, &sent, &received, tmp_x, tmp_y);
        qx(op_dx)(zeta, state, params, gauge, omega,
                  &flops, &sent, &received);
        if (omega_norm == 0) {
            *out_iterations = k;
            *out_epsilon = r;
            status = 2;
            CG_ERROR("DxD_CG(): hit zero mode");
        }
        a = r / omega_norm;
        flops += qx(F_madd2_norm)(rho, &g, state, -a, zeta);
        if (g < rhs_norm * min_epsilon) {
            flops += qx(F_madd2)(psi, state, a, pi);
            r = g;
            status = 0;
            break;
        }
        b = g / r;
        r = g;
        flops += qx(F_xmadd)(psi, pi, state, a, b, rho);
    }
    *out_iterations = k;
    *out_epsilon = r;
    END_TIMING(state, flops, sent, received);

    /* output final residuals if desired */
    if (options) {
        qx(zprint)(state, "DxD CG", "status %d, total iterations %d",
                  status, *out_iterations);
    }
    if (options & (Q(FINAL_CG_RESIDUAL) | Q(LOG_CG_RESIDUAL))) {
        double norm = rhs_norm == 0? 1: rhs_norm;

        qx(zprint)(state, "DxD CG", "solver residual %e normalized %e",
                   *out_epsilon, *out_epsilon / norm);
    }
    if (rhs_norm != 0.0)
        *out_epsilon = *out_epsilon / rhs_norm;

end:
    /* free memory */
    q(free)(state, ptr, ptr_size);
    QX(free_fermion)(&rho);
    QX(free_fermion)(&pi);
    QX(free_fermion)(&omega);
    QX(free_fermion)(&zeta);
    if (status != 0) {
        q(set_error)(state, 0, e_msg);
    }
    return status;
}
