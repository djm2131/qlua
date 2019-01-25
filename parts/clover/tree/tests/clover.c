#include "clover-test.h"

/* Full Clover operator:
 *  psi(x) = (1 - kappa D - kappa c_sw T)(x,x') phi(x')
 *  D(x,x')phi(x') = (1-gamma[mu])U_x[mu]phi(x+mu)
 *                 + (1+gamma[mu]U^\dagger_{x-mu}[mu])phi(x-mu)
 * T(x,x')phi(x') = sigma[mu,nu] F[mu,nu] phi(x)  // mu > nu
 */
void
std_clover_op(QDP_DiracFermion *psi,
              QDP_ColorMatrix *U[],
              QDP_ColorMatrix *Cl[],
              double kappa,
              double c_sw,
              QDP_DiracFermion *phi)
{
    static const int sigma[] = {
        3,  /* 0 1 */
        5,  /* 0 2 */
        9,  /* 0 3 */
        6,  /* 1 2 */
        10, /* 1 3 */
        12  /* 2 3 */
    };

    int mu;
    QLA_Real mk;
    QLA_Complex sw;
    QDP_DiracFermion *t[7];

    create_Dvector(t, NELEMS(t));

    /* Do not worry about performance here, get the expressions right */
    /* t[0] = 0; */
    QDP_D_eq_zero(t[0], QDP_all);
    for (mu = 0; mu < NDIM; mu++) {
        /* t[1](x) = phi(x+mu) */
        QDP_D_eq_sD(t[1], phi, QDP_neighbor[mu], QDP_forward, QDP_all);
        /* t[2] = U[mu] t[1] */
        QDP_D_eq_M_times_D(t[2], U[mu], t[1], QDP_all);
        /* t[3] = gamma[mu] t[2] */
        QDP_D_eq_gamma_times_D(t[3], t[2], 1 << mu, QDP_all);
        /* t[0] += t[2] */
        QDP_D_peq_D(t[0], t[2], QDP_all);
        /* t[0] -= t[3] */
        QDP_D_meq_D(t[0], t[3], QDP_all);
        /* t[4] = U[mu]^* phi(x) */
        QDP_D_eq_Ma_times_D(t[4], U[mu], phi, QDP_all);
        /* t[5] = t[4](x-mu) */
        QDP_D_eq_sD(t[5], t[4], QDP_neighbor[mu], QDP_backward, QDP_all);
        /* t[6] = gamma[mu] t[5] */
        QDP_D_eq_gamma_times_D(t[6], t[5], 1 << mu, QDP_all);
        /* t[0] += t[5] */
        QDP_D_peq_D(t[0], t[5], QDP_all);
        /* t[0] += t[6] */
        QDP_D_peq_D(t[0], t[6], QDP_all);
    }
    /* psi = phi - kappa * t[0] */
    QDP_D_eq_D(psi, phi, QDP_all);
    mk = kappa;
    QDP_D_meq_r_times_D(psi, &mk, t[0], QDP_all);

    /* t[1] = 0 */
    QDP_D_eq_zero(t[1], QDP_all);
    for (mu = 0; mu < ((NDIM - 1) * NDIM) / 2; mu++) {
        /* t[2] = Cl[mu] phi */
        QDP_D_eq_M_times_D(t[2], Cl[mu], phi, QDP_all);
        /* t[3] = sigma[mu] t[2] */
        QDP_D_eq_gamma_times_D(t[3], t[2], sigma[mu], QDP_all);
        /* t[1] += t[3] */
        QDP_D_peq_D(t[1], t[3], QDP_all);
    }
    /* sw = I * c_sw * kappa */
    QLA_real(sw) = 0;
    QLA_imag(sw) = c_sw * kappa;
    /* psi = phi - t[1] */
    QDP_D_meq_c_times_D(psi, &sw, t[1], QDP_all);

    destroy_Dvector(t, NELEMS(t));
}
