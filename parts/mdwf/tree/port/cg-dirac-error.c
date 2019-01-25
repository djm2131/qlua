#include <mdwf.h>

double
qx(cg_dirac_error)(const struct Fermion *psi_x,
		   const struct Fermion *psi_y,
		   struct Q(State) *state,
		   const struct Q(Parameters) *params,
		   const struct SUn *U,
		   const struct Fermion *eta_x,
		   const struct Fermion *eta_y,
		   long long *flops,
		   long long *sent,
		   long long *received,
		   struct Fermion *t0_x,
		   struct Fermion *t1_x,
		   struct Fermion *t0_y)
{
    double x_norm, y_norm, norm;
    
    qx(op_D)(t0_x, state->lat_x, state->lat_y, params, U, psi_x, psi_y,
	     flops, sent, received,
	     t0_y);
    qx(op_D)(t0_y, state->lat_y, state->lat_x, params, U, psi_y, psi_x,
	     flops, sent, received,
	     t1_x);
    *flops += qx(omp_f_diff_norm)(state, &x_norm, state->lat_x->full_size, state->Ls,
				  t0_x, eta_x);
    *flops += qx(omp_f_diff_norm)(state, &y_norm, state->lat_y->full_size, state->Ls,
				  t0_y, eta_y);
    
    norm = x_norm + y_norm;
    *flops += 1; /* every flop counts ... */
    QMP_sum_double(&norm);

    return norm;
}

