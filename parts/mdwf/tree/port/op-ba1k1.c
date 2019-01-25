#include <mdwf.h>

void
qx(op_BA1K1)(struct Fermion *r_x,
             struct eo_lattice *xy,
             const struct Q(Parameters) *params,
             const struct Fermion *s_x,
             long long *flops)
{
  *flops += (params->qx(op)->BA1K)(params->state, r_x, xy->full_size, xy->Ls,
				   params->BpTable, params->BmTable,
				   params->AipTable, params->AimTable,
				   params->KiTable,
				   s_x);
}
