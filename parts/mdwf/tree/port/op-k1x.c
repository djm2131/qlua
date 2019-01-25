#include <mdwf.h>

void
qx(op_K1x)(struct Fermion *r_x,
           struct eo_lattice *xy,
           const struct Q(Parameters) *params,
           const struct Fermion *s_x,
           long long *flops)
{
  *flops += (params->qx(op)->K_conj)(params->state, r_x,
				     xy->full_size, xy->Ls,
				     params->KiTable,
				     s_x);
}

