#include <mdwf.h>

static double
qx(cg_true_residual)(const struct Fermion *xi_x,
                     struct Q(State) *state,
                     const struct Q(Parameters) *params,
                     const struct SUn *U,
                     const struct Fermion *chi_x,
                     long long *flops,
                     long long *sent,
                     long long *received,
                     struct Fermion *t0_x,
                     struct Fermion *t1_x,
                     struct Fermion *t2_x,
                     struct Fermion *t0_y)
{
    double norm;

    qx(op_M)(t0_x, state, params, U, xi_x, flops, sent, received,
                  t2_x, t0_y);
    qx(op_Mx)(t1_x, state, params, U, t0_x, flops, sent, received,
                   t2_x, t0_y);
    *flops += qx(omp_f_diff_norm)(state, &norm, state->lat_x->full_size, state->Ls,
				  t1_x, chi_x);
    QMP_sum_double(&norm);

    return norm;
}

void
qx(cg_log)(double cg_res, const char *source, int iter,
           const struct Fermion *xi_x,
           struct Q(State) *state,
           const struct Q(Parameters) *params,
           const struct SUn *U,
           const struct Fermion *chi_x,
           long long *flops,
           long long *sent,
           long long *received,
           unsigned int options,
           struct Fermion *t0_x,
           struct Fermion *t1_x,
           struct Fermion *t2_x,
           struct Fermion *t0_y)
{
    double true_res = 0.0;

    if (options & Q(LOG_TRUE_RESIDUAL)) {
        true_res = qx(cg_true_residual)(xi_x, state, params, U, chi_x,
                                        flops, sent, received,
                                        t0_x, t1_x, t2_x, t0_y);
    }
#define ITER_LOG (Q(LOG_CG_RESIDUAL) |    \
                  Q(LOG_TRUE_RESIDUAL))
    switch (options & ITER_LOG) {
#undef ITER_LOG
    default:
        break;
    case Q(LOG_CG_RESIDUAL):
        qx(zprint)(state, source,
                   "CG step %5d"
                   "  CG residual %11.4e",
                   iter, cg_res);
        break;
    case Q(LOG_TRUE_RESIDUAL):
        qx(zprint)(state, source,
                   "CG step %5d"
                   "  true residual %11.4e",
                   iter, true_res);
        break;
    case Q(LOG_CG_RESIDUAL) | Q(LOG_TRUE_RESIDUAL):
        qx(zprint)(state, source,
                   "CG step %5d"
                   "  CG residual %11.4e"
                   "  true residual %11.4e",
                   iter, cg_res, true_res);
        break;
    }
}
