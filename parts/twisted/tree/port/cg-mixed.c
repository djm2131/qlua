#include "twisted.h"

/* Solve
 *   D_cl psi = eta
 *
 * with psi_0 as an initial guess
 *
 */ 

#define MAX_OPTIONS (QOP_TWISTED_LOG_CG_RESIDUAL     | \
                     QOP_TWISTED_LOG_TRUE_RESIDUAL   | \
                     QOP_TWISTED_FINAL_CG_RESIDUAL   | \
                     QOP_TWISTED_FINAL_DIRAC_RESIDUAL | \
                     QOP_TWISTED_LOG_EIG_UPDATE1 | \
                     QOP_TWISTED_LOG_EIG_POSTAMBLE)

int
q(mixed_cg)(struct Q(State)             *state,
            const char                  *name,
            struct QD(Fermion)          *psi,
            int                         *out_iterations,
            double                      *out_epsilon,
            const struct QD(Fermion)    *psi_0,
            const struct QD(Gauge)      *gauge,
            const struct QD(Fermion)    *eta,
            struct QF(Deflator)         *deflator,
            int                          f_iter,
            double                       f_epsilon,
            int                          max_iterations,
            double                       min_epsilon,
            unsigned int                 options)
{
    long long flops = 0;
    long long sent = 0;
    long long received = 0;
    void *ptr_d = 0;
    void *ptr_f = 0;
    size_t ptr_size_d = 0;
    size_t ptr_size_f = 0;
    void *temps = 0;
    CG_STATUS cg_status;
    int status = 1;
    struct FermionD *chi_e = 0;
    struct FermionD *t0_e = 0;
    struct FermionD *t1_e = 0;
    struct FermionD *t0_o = 0;
    
    struct QF(Gauge) *gauge_F = 0;
    struct FermionF *delta_Fe = 0;
    struct FermionF *dx_Fe = 0;
    struct FermionF *pi_Fe = 0;
    struct FermionF *rho_Fe = 0;
    struct FermionF *zeta_Fe = 0;
    struct FermionF *t0_Fe = 0;
    struct FermionF *t1_Fe = 0;
    struct FermionF *t0_Fo = 0;
    struct FermionF *t1_Fo = 0;

    double dirac_residual = 0;
    double rhs_norm = 0;
    double scaled_eps;
    int iter_left;
    char *cg_error = 0;
    int df_stopped = QF(deflator_eigcg_is_stopped)(deflator);

#define CG_ERROR(msg) do { cg_error = msg; goto end; } while (0)
#define CG_ERROR_T(msg) do { \
        END_TIMING(state, flops, sent, received); \
        cg_error = msg; goto end; } while (0)

    if (Q(gauge_float_from_double)(&gauge_F, gauge) != 0)
      CG_ERROR("not enough space for gauge_F");

    /* clear bits we do not understand */
    options = options & MAX_OPTIONS;

    /* allocate double locals */
    ptr_d = qd(allocate_eo)(state, &ptr_size_d, &temps,
                            0,  /* header */
                            3,  /* evens */
                            1); /* odds */
    if (ptr_d == 0)
        CG_ERROR("mixed_D_CG(): not enough memory");

    chi_e = temps;
    t0_e  = temps = qd(step_even)(state, temps);
    t1_e  = temps = qd(step_even)(state, temps);
    t0_o  = temps = qd(step_even)(state, temps);

    /* allocate float temps */
    ptr_f = qf(allocate_eo)(state, &ptr_size_f, &temps,
                            0,  /* header */
                            7,  /* evens */
                            2); /* odds */
    if (ptr_f == 0)
        CG_ERROR("mixed_D_CG(): not enough memory");

    delta_Fe = temps;
    dx_Fe    = temps = qf(step_even)(state, temps);
    rho_Fe   = temps = qf(step_even)(state, temps);
    pi_Fe    = temps = qf(step_even)(state, temps);
    zeta_Fe  = temps = qf(step_even)(state, temps);
    t0_Fe    = temps = qf(step_even)(state, temps);
    t1_Fe    = temps = qf(step_even)(state, temps);
    t0_Fo    = temps = qf(step_even)(state, temps);
    t1_Fo    = temps = qf(step_odd)(state, temps);

    BEGIN_TIMING(state);
    /* compute the norm of the RHS */
    flops += qd(op_norm2)(&rhs_norm, eta, state);
    
    if (options) {
        qd(zprint)(state, name, "rhs norm %e normalized epsilon %e",
                   rhs_norm, min_epsilon * rhs_norm);
    }

    /* setup communication */
    if (q(setup_comm)(state, sizeof (double)))
        CG_ERROR_T("mixed_D_CG(): communication setup failed");
        
    /* precondition */
    qd(cg_precondition)(chi_e, state,
                        gauge,
                        eta->even, eta->odd,
                        &flops, &sent, &received,
                        t0_e,
                        t0_o);
    qd(f_copy)(psi->even,
               state->even.full_size,
               psi_0->even);

    scaled_eps = min_epsilon * rhs_norm;
    for (iter_left = max_iterations; iter_left > 0;) {
        int here_iter;
        double delta_norm2;
        double ff_eps;

        qd(op_even_M)(t0_e, state, gauge, psi->even,
                      &flops, &sent, &received, t0_o);
        qd(op_even_Mx)(t1_e, state, gauge, t0_e,
                       &flops, &sent, &received, t0_o);
        flops += q(f_f_eq_dmd_norm2)(delta_Fe, &delta_norm2, 
                                     state->even.full_size, chi_e, t1_e);
        QMP_sum_double(&delta_norm2);
        ff_eps = delta_norm2 * f_epsilon;
        if (ff_eps < scaled_eps)
            ff_eps = scaled_eps;
        /* run the solver for a while */
        if (q(setup_comm)(state, sizeof (float))) {
            CG_ERROR_T("mixed_D_CG(): communication setup failed");
        }
        cg_status = qf(cg_solver)(dx_Fe, name, &here_iter, out_epsilon,
                                  state, gauge_F, delta_Fe, deflator,
                                  iter_left > f_iter ? f_iter : iter_left,
                                  ff_eps, options,
                                  &flops, &sent, &received,
                                  rho_Fe, pi_Fe, zeta_Fe, t0_Fe,
                                  t1_Fe, t0_Fo, t1_Fo);
        QF(deflator_eigcg_stop)(deflator);

        if (q(setup_comm)(state, sizeof (double))) {
            CG_ERROR_T("mixed_D_CG(): communication setup failed");
        }
        flops += q(f_d_eq_dpf)(psi->even, state->even.full_size,
                               psi->even, dx_Fe);
        iter_left -= here_iter;
        *out_iterations = max_iterations - iter_left;
        /* continue or not */
        status = 1;
        switch (cg_status) {
        case CG_SUCCESS:
        case CG_MAXITER:
        case CG_EIGCONV:
            status = 0;
            break;
        default:
            CG_ERROR_T(NULL);
        }
        if (*out_epsilon < scaled_eps)
            break;
    }
    if ((df_stopped == 0) && deflator != 0)
        QF(deflator_eigcg_resume)(deflator);
    /* inflate */
    qd(cg_inflate)(psi->odd,
                   state, gauge, eta->odd, psi->even,
                   &flops, &sent, &received,
                   t0_o);    

    if (options & QOP_TWISTED_FINAL_DIRAC_RESIDUAL) {
        dirac_residual = qd(cg_dirac_error)(psi->even, psi->odd,
                                            state, gauge,
                                            eta->even, eta->odd,
                                            &flops, &sent, &received,
                                            t0_e, t0_o);
    }

    END_TIMING(state, flops, sent, received);

    /* output final residuals if desired */
    if (options) {
        qx(zprint)(state, name, "status %d, total iterations %d",
                  status, *out_iterations);
    }
    if (options & (QOP_TWISTED_FINAL_CG_RESIDUAL | QOP_TWISTED_LOG_CG_RESIDUAL)) {
        double norm = rhs_norm == 0? -1: rhs_norm;

        qx(zprint)(state, name, "solver residual %e normalized %e",
                   *out_epsilon, *out_epsilon / norm);
    }
    if (options & QOP_TWISTED_FINAL_DIRAC_RESIDUAL) {
        double norm = rhs_norm == 0? -1: rhs_norm;

        qx(zprint)(state, name, "Dirac residual %e normalized %e",
                   dirac_residual, dirac_residual / norm);
    }
    if (rhs_norm != 0.0)
        *out_epsilon = *out_epsilon / rhs_norm;
        
end:    

    if (ptr_d)
        q(free)(state, ptr_d, ptr_size_d);
    if (ptr_f)
        q(free)(state, ptr_f, ptr_size_f);
    QF(free_gauge)(&gauge_F);
    if (cg_error)
        return q(set_error)(state, 0, cg_error);
    if (status != 0)
        q(set_error)(state, 0, "mixed_D_CG() solver failed to converge");
    return status;
}
