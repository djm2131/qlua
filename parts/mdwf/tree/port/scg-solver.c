#include <mdwf.h>

int
qx(scg_solver)(struct VectorFermion *v_xi_x,
               struct Fermion *xi_x,
               int count,
               const char *source,
               int *out_iterations,
               double *out_epsilon,
               struct Q(State) *state,
               const struct Q(Parameters) *params,
               const double shift[],
               const struct SUn *U,
               const struct Fermion *chi_x,
               int max_iterations,
               double min_epsilon,
               unsigned options,
               long long *flops,
               long long *sent,
               long long *received,
               double v[],
               double w[],
               double ad[],
               double bdd[],
               struct Fermion *rho_x,
               struct VectorFermion *v_pi_x,
               struct Fermion *pi_x,
               struct Fermion *zeta_x,
               struct Fermion *t0_x,
               struct Fermion *t1_x,
               struct Fermion *t2_x,
               struct Fermion *t0_y)
{
    int x_size = state->lat_x->full_size;
    int Ls = state->Ls;
    int k, j;
    double r, z, ap, bp, a, g, b;

    /* setup */
    qx(omp_f_zero)(state, xi_x, x_size, Ls);
    qx(omp_f_copy)(state, rho_x, x_size, Ls, chi_x);
    qx(omp_f_copy)(state, pi_x, x_size, Ls, chi_x);
    qx(omp_fv_zero)(state, v_xi_x, x_size, Ls, count);
    qx(omp_fv_copy)(state, v_pi_x, x_size, Ls, count, rho_x);
    for (j = 0; j < count; j++) {
       w[j] = v[j] = 1;
    }
    *flops += qx(omp_f_norm)(state, &r, x_size, Ls, rho_x);
    QMP_sum_double(&r);
    ap = bp = 1;
    if (r < min_epsilon) {
        k = 0;
        goto end;
    }
    /* loop for convergence */
    for (k = 0; k < max_iterations; k++) {
        qx(op_Mn)(t0_x, &z, state, params, U, pi_x,
                       flops, sent, received,
                       t1_x, t0_y);
        qx(op_Mx)(zeta_x, state, params, U, t0_x,
                       flops, sent, received,
                       t1_x, t0_y);
        if (z == 0.0) {
            *out_iterations = k;
            *out_epsilon = r;
            q(set_error)(state, 0, "scg_solver() hit zero mode");
            return 2;
        }
        a = r / z;
        *flops += qx(omp_f_add2_norm)(state, rho_x, &g, x_size, Ls, -a, zeta_x);
        QMP_sum_double(&g);
        b = g / r;
        r = g;
        for (j = 0; j < count; j++) {
            w[j] = 1/(1+a*(shift[j] + bp * (1 - w[j])/ap));
            ad[j] = a * w[j] * v[j];
            bdd[j] = b * w[j];
            v[j] = v[j] * w[j];
        }
        if (g < min_epsilon) {
	    qx(omp_scg_madd)(state, xi_x, v_xi_x, x_size, Ls, count, a, ad, pi_x);
            break;
        }
        qx(omp_scg_xp)(state, xi_x, pi_x,
		       v_xi_x, v_pi_x,
		       x_size, Ls, count,
		       a, b,
		       ad, bdd,
		       rho_x);
        bp = b;
        ap = a;
        if (options)
            qx(cg_log)(r, "MxM SCG",
                       k, xi_x, state, params, U, chi_x,
                       flops, sent, received,
                       options,
                       t0_x, t1_x, t2_x, t0_y);
    }
end:
    *out_iterations = k;
    *out_epsilon = r;
    if (k == max_iterations)
        return 1;
    return 0;
}
