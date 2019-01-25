#include <mdwf.h>

void
qx(op_Bx)(struct Fermion *r_x,
	  struct eo_lattice *xy,
	  const struct Q(Parameters) *params,
	  const struct Fermion *s_x,
	  long long *flops)
{
  *flops += (params->qx(op)->A_conj)(params->state, r_x,
				     xy->full_size, xy->Ls,
				     params->BxpTable,
				     params->BxmTable,
				     s_x);
}
