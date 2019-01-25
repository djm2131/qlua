#include <mdwf.h>
#include "helpers-omp.h"

/* Mixed precision pieces of openmp wraps
 */

size_t
q(omp_f_d_peq_f)(struct Q(State) *state,
		 struct FermionD *dst,
		 size_t size, size_t Ls,
		 const struct FermionF *src_f)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(q(f_d_peq_f)(dst, lx_low, lx_count, Ls, src_f));
}

void
q(omp_g_f_eq_d)(struct Q(State) *state,
		struct SUnF *dst,
		size_t size,
		const struct SUnD *src)
{
  DECLARE_VOID();
#ifdef _OPENMP
#pragma omp parallel for
#endif
  LOOP_VOID(q(g_f_eq_d)(dst, lx_low, lx_count, src));
}

size_t
q(omp_f_f_eq_dmd_norm2)(struct Q(State) *state,
			struct FermionF *dst,
			double *local_norm2,
			size_t size, size_t Ls,
			const struct FermionD *src_a,
			const struct FermionD *src_b)
{
  DECLARE_NORM2();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops) reduction(+:lx_local_norm)
#endif
  LOOP_NORM2(*local_norm2, q(f_f_eq_dmd_norm2)(dst, &lx_norm, lx_low, lx_count, Ls, src_a, src_b));
}
