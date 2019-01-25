#include <mdwf.h>

void
qx(op_B1)(struct Fermion *r_x,
	  struct eo_lattice *xy,
	  const struct Q(Parameters) *params,
	  const struct Fermion *s_x,
	  long long *flops)
{
  *flops += (params->qx(op)->A_inverse)(params->state, r_x,
					xy->full_size, xy->Ls,
					params->BipTable,
					params->BimTable,
					s_x);
}
