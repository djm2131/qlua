#include <clover.h>

#if QOP_CLOVER_DEFAULT_PRECISION == 'F'
#define DF_PREAMBLE(psi_e, rho_e, r, chi_e) do {                \
        if (NULL != deflator) {                                 \
           if (0 != (status = qx(defl_eigcg_preamble)(          \
                           deflator, psi_e, rho_e, r, chi_e,    \
                           &ws, options)))                      \
               return status;                                   \
        } else {                                                \
            qx(f_zero)(psi_e, e_size);                          \
            qx(f_copy)(rho_e, e_size, chi_e);                   \
            state->flops += qx(f_norm)(r, e_size, rho_e);       \
            QMP_sum_double(r);                                  \
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
#define DF_PREAMBLE(psi_e, rho_e, r, chi_e) do {        \
        qx(f_zero)(psi_e, e_size);                      \
        qx(f_copy)(rho_e, e_size, chi_e);               \
        state->flops += qx(f_norm)(r, e_size, rho_e);   \
        QMP_sum_double(r);                              \
    } while (0)
#define DF_UPDATE0(a1,b1,a0,b0,r,rho)  0
#define DF_UPDATE1(a1,b1,a0,b0,r,rho,A_rho)  0
#define DF_POSTAMBLE()  do {} while (0)
#endif


void
qx(cg_operator)(struct FermionX          *res_e,
                const struct FermionX    *psi_e,
                struct qx(MxM_workspace) *ws)
{
    qx(op_even_M)(ws->tmp_e, ws->state, ws->gauge, psi_e,
                  ws->flops, ws->sent, ws->received,
                  ws->tmp_o);
    qx(op_even_Mx)(res_e, ws->state, ws->gauge, ws->tmp_e,
                   ws->flops, ws->sent, ws->received,
                   ws->tmp_o);
}

CG_STATUS
qx(cg_solver)(struct FermionX           *psi_e,
              const char                *name,
              int                       *out_iter,
              double                    *out_epsilon,
              struct Q(State)           *state,
              const struct QX(Gauge)    *gauge,
              const struct FermionX     *chi_e,
              struct QF(Deflator)       *deflator,
              int                        max_iter,
              double                     epsilon,
              unsigned                   options,
              long long                 *flops,
              long long                 *sent,
              long long                 *received,
              struct FermionX           *rho_e,
              struct FermionX           *pi_e,
              struct FermionX           *zeta_e,
              struct FermionX           *t0_e,
              struct FermionX           *t1_e,
              struct FermionX           *t0_o,
              struct FermionX           *t1_o)
{
#if QOP_CLOVER_DEFAULT_PRECISION == 'F'
    double a0 = 1, b0 = 0;
    int status = 0;
#endif /* QOP_CLOVER_DEFAULT_PRECISION == 'F' */
    int df_status;
    int e_size = state->even.full_size;
    double a, b, g, r, norm_omega;
    int i;
    struct qx(MxM_workspace)  ws;

    ws.state = state;
    ws.gauge = gauge;
    ws.tmp_e = t0_e;
    ws.tmp_o = t0_o;
    ws.flops = flops;
    ws.sent = sent;
    ws.received = received;

    DF_PREAMBLE(psi_e, rho_e, &r, (struct FermionX *) chi_e);
    qx(f_copy)(pi_e, e_size, rho_e);
    if (r < epsilon) {
        i = 0;
        goto end;
    }
    for (i = 0; i < max_iter; i++) {
        qx(op_even_Mn)(t0_e, &norm_omega, state, gauge, pi_e,
                       flops, sent, received,
                       t0_o);
        qx(op_even_Mx)(zeta_e, state, gauge, t0_e,
                       flops, sent, received,
                       t0_o);
        if (norm_omega == 0.0) {
            *out_iter = i;
            *out_epsilon = r;
            q(set_error)(state, 0, "cg_solver() hit zero mode");
            DF_POSTAMBLE();
            return CG_ZEROMODE;
        }
        a = r / norm_omega;
        *flops += qx(f_add2_norm)(rho_e, &g, e_size, -a, zeta_e);
        QMP_sum_double(&g);
        if (g < epsilon) {
            *flops += qx(f_add2)(psi_e, e_size, a, pi_e);
            r = g;
            break;
        }
        b = g / r;
        r = g;
        qx(cg_xp)(psi_e, pi_e, e_size, a, b, rho_e);
        df_status = DF_UPDATE0(a, b, a0, b0, g, rho_e);
        if (-1 == df_status) {
            qx(cg_operator)(zeta_e, rho_e, &ws);
            df_status = DF_UPDATE1(a, b, a0, b0, g, rho_e, zeta_e);
        } 
        if (3 == df_status) {
            *out_iter = i;
            *out_epsilon = r;
            DF_POSTAMBLE();
            return CG_EIGCONV;
        }

#if QOP_CLOVER_DEFAULT_PRECISION == 'F'
        a0 = a;
        b0 = b;
#endif /* QOP_CLOVER_DEFAULT_PRECISION == 'F' */
        if (options)
            qx(cg_log)(r, name,
                       i, psi_e, state, gauge, chi_e,
                       flops, sent, received,
                       options,
                       t0_e, t1_e, t0_o, t1_o);
    }
end:
    *out_iter = i;
    *out_epsilon = r;
    DF_POSTAMBLE();
    if (i == max_iter) {
        return CG_MAXITER;
    }
    return CG_SUCCESS;
}
