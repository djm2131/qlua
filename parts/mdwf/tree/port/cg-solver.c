#include <mdwf.h>

#if QOP_MDWF_DEFAULT_PRECISION == 'F'
#define DF_PREAMBLE(psi_x, rho_x, r, chi_x) do {                \
        if (NULL != deflator) {                                 \
           if (0 != (status = qx(defl_eigcg_preamble)(          \
                           deflator, psi_x, rho_x, r, chi_x,    \
                           &ws, options)))                      \
               return status;                                   \
        } else {                                                \
	  qx(preamble)(state, x_size, Ls,                       \
		       psi_x, rho_x, chi_x, r);              	\
    } } while (0)

#define DF_UPDATE0(a1,b1,a0,b0,r,rho) (NULL != (deflator)       \
        ? qx(defl_eigcg_update0)(deflator, a1, b1, a0, b0, r,   \
                                 rho, options)                  \
        : 0)

#define DF_UPDATE1(a1,b1,a0,b0,r,rho,A_rho) (NULL != (deflator) \
        ? qx(defl_eigcg_update1)(deflator, a1, b1, a0, b0, r,   \
                                 rho, A_rho, options)           \
        : 0)

#define DF_POSTAMBLE() do {                                     \
        if (NULL != deflator) {                                 \
            if (0 != (status = qx(defl_eigcg_postamble)(        \
                            deflator, &ws, options)))           \
                return status;                                  \
        } } while (0)
#else
#define DF_PREAMBLE(psi_x, rho_x, r, chi_x) do {            \
    qx(preamble)(state, x_size, Ls,		  	    \
		 psi_x, rho_x, chi_x, r);		    \
  } while (0)
#define DF_UPDATE0(a1,b1,a0,b0,r,rho)  0
#define DF_UPDATE1(a1,b1,a0,b0,r,rho,A_rho)  0
#define DF_POSTAMBLE()  do {} while (0)
#endif

static void
qx(preamble)(struct Q(State)             *state,
	     int                          x_size,
	     int                          Ls,
	     struct Fermion              *psi_x,
	     struct Fermion              *rho_x,
	     struct Fermion              *chi_x,
	     double                      *r)
{
  qx(omp_f_zero)(state, psi_x, x_size, Ls);
  qx(omp_f_copy)(state, rho_x, x_size, Ls, chi_x);
  state->flops += qx(omp_f_norm)(state, r, x_size, Ls, rho_x);
  QMP_sum_double(r);
}

void
qx(cg_operator)(struct Fermion           *res_x,
                const struct Fermion     *psi_x,
                struct qx(MxM_workspace)     *ws)
{
    qx(op_M) (ws->tmp_x, ws->state, ws->params, ws->gauge, psi_x,
                   ws->flops, ws->sent, ws->received,
                   ws->tmp2_x, ws->tmp_y);
    qx(op_Mx)(res_x, ws->state, ws->params, ws->gauge, ws->tmp_x,
                   ws->flops, ws->sent, ws->received,
                   ws->tmp2_x, ws->tmp_y);
}

CG_STATUS
qx(cg_solver)(struct Fermion              *xi_x,
              const char                  *source,
              int                         *out_iter,
              double                      *out_epsilon,
              struct Q(State)             *state,
              const struct Q(Parameters)  *params,
              const struct SUn            *U,
              const struct Fermion        *chi_x,
              struct QX(Deflator)         *deflator,
              int                          max_iter,
              double                       epsilon,
              unsigned                     options,
              long long                   *flops,
              long long                   *sent,
              long long                   *received,
              struct Fermion              *rho_x,
              struct Fermion              *pi_x,
              struct Fermion              *zeta_x,
              struct Fermion              *t0_x,
              struct Fermion              *t1_x,
              struct Fermion              *t2_x,
              struct Fermion              *t0_y)
{
#if QOP_MDWF_DEFAULT_PRECISION == 'F'
    double a0 = 1, b0 = 0;
    int status;
#endif /*  QOP_MDWF_DEFAULT_PRECISION == 'F' */
    int df_status;
    int x_size = state->lat_x->full_size;

    int Ls = state->Ls;
    double a, b, g, r, norm_omega;
    int i;
    struct qx(MxM_workspace)  ws;

    ws.state     = state;
    ws.params    = params;
    ws.gauge     = U;
    ws.tmp_x     = t0_x;
    ws.tmp2_x    = t2_x;
    ws.tmp_y     = t0_y;
    ws.flops     = flops;
    ws.sent      = sent;
    ws.received  = received;

    DF_PREAMBLE(xi_x, rho_x, &r, (struct Fermion *) chi_x);
    qx(omp_f_copy)(state, pi_x, x_size, Ls, rho_x);
    if (r < epsilon) {
        i = 0;
        goto end;
    }
    for (i = 0; i < max_iter; i++) {
        qx(op_Mn)(t0_x, &norm_omega, state, params, U, pi_x,
                      flops, sent, received,
                      t1_x, t0_y);
        qx(op_Mx)(zeta_x, state, params, U, t0_x,
                      flops, sent, received,
                      t1_x, t0_y);
        if (norm_omega == 0.0) {
            *out_iter = i;
            *out_epsilon = r;
            q(set_error)(state, 0, "cg_solver() hit zero mode");
            DF_POSTAMBLE();
            return CG_ZEROMODE;
        }
        a = r / norm_omega;
        *flops += qx(omp_f_add2_norm)(state, rho_x, &g, x_size, Ls, -a, zeta_x);
        QMP_sum_double(&g);
        if (g < epsilon) {
	  *flops += qx(omp_f_add2)(state, xi_x, x_size, Ls, a, pi_x);
            r = g;
            break;
        }
        b = g / r;
        r = g;
        qx(omp_cg_xp)(state, xi_x, pi_x, x_size, Ls, a, b, rho_x);
        df_status = DF_UPDATE0(a, b, a0, b0, g, rho_x);
        if (-1 == df_status) {
            qx(cg_operator)(zeta_x, rho_x, &ws);
            df_status = DF_UPDATE1(a, b, a0, b0, g, rho_x, zeta_x);
        }
        if (3 == df_status) {
            *out_iter = i;
            *out_epsilon = r;
            DF_POSTAMBLE();
            return CG_EIGCONV;
        }
#if QOP_MDWF_DEFAULT_PRECISION == 'F'
        a0 = a;
        b0 = b;
#endif /*  QOP_MDWF_DEFAULT_PRECISION == 'F' */
        if (options)
            qx(cg_log)(r, source,
                       i, xi_x, state, params, U, chi_x,
                       flops, sent, received,
                       options,
                       t0_x, t1_x, t2_x, t0_y);
    }
end:
    *out_iter = i;
    *out_epsilon = r;
    DF_POSTAMBLE();
    if (i == max_iter)
        return CG_MAXITER;
    return CG_SUCCESS;
}
