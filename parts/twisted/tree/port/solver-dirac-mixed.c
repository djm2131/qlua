#include "twisted.h"

/* Solve
 *   D_cl psi = eta
 *
 * with psi_0 as an initial guess
 *
 */ 

int
Q(mixed_D_CG)(struct QD(Fermion)          *psi,
              int                         *out_iterations,
              double                      *out_epsilon,
              const struct QD(Fermion)    *psi_0,
              const struct QD(Gauge)      *gauge,
              const struct QD(Fermion)    *eta,
              int                          f_iter,
              double                       f_epsilon,
              int                          max_iterations,
              double                       min_epsilon,
              unsigned int                 options)
{
    DECLARE_STATE;

    /* check arguments */
    CHECK_ARG0(psi);
    CHECK_ARGn(psi_0, "mD_CG");
    CHECK_ARGn(gauge, "mD_CG");
    CHECK_ARGn(eta, "mD_CG");

    return q(mixed_cg)(state, "mD_CG", 
                       psi, out_iterations, out_epsilon,
                       psi_0, gauge, eta, NULL,
                       f_iter, f_epsilon, max_iterations, min_epsilon,
                       options);
}
