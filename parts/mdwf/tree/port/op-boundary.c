#include <mdwf.h>
#include "helpers-omp.h"

static size_t
qx(omp_proj_g)(struct Q(State) *state,
	       size_t (*proj)(struct ProjectedFermion *pr,
				    size_t pstart, size_t psize, size_t pLs,
				    const struct down_pack *plink,
				    const struct Fermion *pf),
	       struct ProjectedFermion *r,
	       size_t size, size_t Ls,
	       const struct down_pack *link,
	       const struct Fermion *f)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(proj(r, lx_low, lx_count, Ls, link, f));
}

static size_t
qx(omp_proj_Ug)(struct Q(State) *state,
		size_t (*proj)(struct ProjectedFermion *pr,
				     size_t pstart, size_t psize, size_t pLs,
				     const struct up_pack *plink,
				     const struct SUn *pU,
				     const struct Fermion *pf),
		struct ProjectedFermion *r,
		size_t size, size_t Ls,
		const struct up_pack *link,
		const struct SUn *U,
		const struct Fermion *f)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(proj(r, lx_low, lx_count, Ls, link, U, f));
}

void
qx(boundary)(struct eo_lattice *xy,
             size_t Ls,
             const qx(Up_project) up_proj[],
             const qx(Down_project) down_proj[],
             const struct SUn *U,
             const struct Fermion *src_y,
             long long *flops)
{
    int i;

    for (i = 0; i < Q(DIM); i++) {
        if (xy->send_up_size[i])
	  *flops += qx(omp_proj_Ug)(xy->state, up_proj[i],
				    xy->send_up_buf[i],
				    xy->send_up_size[i], Ls,
				    xy->up_pack[i], U, src_y);
        if (xy->send_down_size[i])
	  *flops += qx(omp_proj_g)(xy->state, down_proj[i],
				   xy->send_down_buf[i],
				   xy->send_down_size[i], Ls,
				   xy->down_pack[i], src_y);
    }
}
