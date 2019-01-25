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
                     QOP_TWISTED_FINAL_DIRAC_RESIDUAL)

int
QX(D_CG)(struct QX(Fermion)          *psi,
         int                         *out_iterations,
         double                      *out_epsilon,
         const struct QX(Fermion)    *psi_0,
         const struct QX(Gauge)      *gauge,
         const struct QX(Fermion)    *eta,
         int                          max_iterations,
         double                       min_epsilon,
         unsigned int                 options)
{
    DECLARE_STATE;
    long long flops = 0;
    long long sent = 0;
    long long received = 0;
    void *ptr = 0;
    size_t ptr_size = 0;
    void *temps = 0;
    CG_STATUS cg_status;
    int status;
    int e_size;
    struct FermionX *chi_e = 0;
    struct FermionX *rho_e = 0;
    struct FermionX *pi_e = 0;
    struct FermionX *zeta_e = 0;
    struct FermionX *t0_e = 0;
    struct FermionX *t1_e = 0;
    struct FermionX *t0_o = 0;
    struct FermionX *t1_o = 0;
    double dirac_residual = 0;
    double rhs_norm = 0;

    /* check arguments */
    CHECK_ARG0(psi);
    CHECK_ARGn(psi_0, "D_CG");
    CHECK_ARGn(gauge, "D_CG");
    CHECK_ARGn(eta, "D_CG");

    e_size = state->even.full_size;
    /* setup communication */
    if (q(setup_comm)(state, sizeof (REAL))) {
        return q(set_error)(state, 0, "DDW_CG(): communication setup failed");
    }

    /* allocate locals */
    ptr = qx(allocate_eo)(state, &ptr_size, &temps,
                          0,  /* header */
                          6,  /* evens */
                          2); /* odds */

    if (ptr == 0) {
        return q(set_error)(state, 0, "D_CG(): not enough memory");
    }
    t0_e  = temps;
    t1_e  = temps  = qx(step_even)(state, temps);
    chi_e = temps  = qx(step_even)(state, temps);
    rho_e = temps  = qx(step_even)(state, temps);
    pi_e = temps   = qx(step_even)(state, temps);
    zeta_e = temps = qx(step_even)(state, temps);
    t0_o  = temps  = qx(step_even)(state, temps);
    t1_o  = temps  = qx(step_odd)(state, temps);

    /* clear bits we do not understand */
    options = options & MAX_OPTIONS;

    BEGIN_TIMING(state);
    /* compute the norm of the RHS */
    flops += qx(op_norm2)(&rhs_norm, eta, state);
    if (options) {
        qx(zprint)(state, "DCL CG", "rhs norm %e normalized epsilon %e",
                   rhs_norm, min_epsilon * rhs_norm);
    }

    /* precondition */
    qx(cg_precondition)(chi_e, state,
                        gauge, eta->even, eta->odd,
                        &flops, &sent, &received,
                        t0_e, t0_o);
    /* reduce rhs */
    qx(op_even_M)(t1_e, state, gauge, psi_0->even,
                  &flops, &sent, &received,
                  t0_o);
    qx(op_even_Mx)(t0_e, state, gauge, t1_e,
                   &flops, &sent, &received,
                   t0_o);
    qx(f_add2)(chi_e, state->even.full_size, -1.0, t0_e);

    /* solve */
    cg_status = qx(cg_solver)(psi->even, "DCL CG", out_iterations, out_epsilon,
                              state, gauge, chi_e, NULL,
                              max_iterations, min_epsilon * rhs_norm, options,
                              &flops, &sent, &received,
                              rho_e, pi_e, zeta_e,
                              t0_e, t1_e, t0_o, t1_o);
    /* handle zero mode properly */
    switch (cg_status) {
    case CG_SUCCESS:
        status = 0;
        break;
    default:
        status = 1;
    }

    /* inflate */
    qx(f_add2)(psi->even, e_size, 1.0, psi_0->even);
    qx(cg_inflate)(psi->odd,
                   state, gauge, eta->odd, psi->even,
                   &flops, &sent, &received,
                   t0_o);

    if (options & QOP_TWISTED_FINAL_DIRAC_RESIDUAL) {
        dirac_residual = qx(cg_dirac_error)(psi->even, psi->odd,
                                            state, gauge,
                                            eta->even, eta->odd,
                                            &flops, &sent, &received,
                                            t0_e, t0_o);
    }

    END_TIMING(state, flops, sent, received);

    /* output final residuals if desired */
    if (options) {
        qx(zprint)(state, "DCL CG", "status %d, total iterations %d",
                  status, *out_iterations);
    }
    if (options & (QOP_TWISTED_FINAL_CG_RESIDUAL | QOP_TWISTED_LOG_CG_RESIDUAL)) {
        double norm = rhs_norm == 0? 1: rhs_norm;

        qx(zprint)(state, "DCL CG", "solver residual %e normalized %e",
                   *out_epsilon, *out_epsilon / norm);
    }
    if (options & QOP_TWISTED_FINAL_DIRAC_RESIDUAL) {
        double norm = rhs_norm == 0? 1: rhs_norm;

        qx(zprint)(state, "DCL CG", "Dirac residual %e normalized %e",
                   dirac_residual, dirac_residual / norm);
    }
    if (rhs_norm != 0.0)
        *out_epsilon = *out_epsilon / rhs_norm;

    /* free memory */
    q(free)(state, ptr, ptr_size);
    if (status != 0) {
        q(set_error)(state, 0, "D_CG() solver failed to converge");
    }
    return status;
}
