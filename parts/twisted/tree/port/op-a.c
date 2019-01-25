#include "twisted.h"

void
qx(op_A)(struct FermionX *r_x,
         struct eo_lattice *xy,
         const struct Twisted *tw,
         const struct FermionX *s_x,
         long long *flops)
{
    *flops += qx(do_A)(r_x, xy->full_size, tw, s_x);
}
