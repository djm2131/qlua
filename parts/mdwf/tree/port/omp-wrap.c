#include <mdwf.h>
#include "helpers-omp.h"

size_t
qx(omp_do_1mBA1F)(struct Q(State) *state,
		  struct Fermion *r_y,
		  size_t start, size_t size, size_t Ls,
		  const struct ABTable *bptable,
		  const struct ABTable *bmtable,
		  const struct ABiTable *iatable_p,
		  const struct ABiTable *iatable_m,
		  const struct neighbor *neighbor,
		  const struct SUn *U,
		  const struct Fermion *a_y,
		  const struct Fermion *b_x,
		  void *rb[])
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(do_1mBA1F)(r_y,
			   lx_low + start, lx_count, Ls,
			   bptable, bmtable, iatable_p, iatable_m, neighbor,
			   U, a_y, b_x, rb));
}

size_t
qx(omp_do_1mK1xA1xBxFx)(struct Q(State) *state,
			struct Fermion *r_y,
			size_t start, size_t size, size_t Ls,
			const struct KTable *ktable,
			const struct ABiTable *iatable_p,
			const struct ABiTable *iatable_m,
			const struct ABTable *bptable,
			const struct ABTable *bmtable,
			const struct neighbor *neighbor,
			const struct SUn *U,
			const struct Fermion *a_y,
			const struct Fermion *b_x,
			void *rb[])
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(do_1mK1xA1xBxFx)(r_y,
				 lx_low + start, lx_count, Ls,
				 ktable,
				 iatable_p, iatable_m, 
				 bptable, bmtable,
				 neighbor,
				 U, a_y, b_x, rb));
}

size_t
qx(omp_doc_1mK1xA1xBxFx)(struct Q(State) *state,
			 struct Fermion *r_y,
			 size_t start, size_t size, size_t Ls,
			 const struct KTable *ktable,
			 const struct ABiTable *iatable_p,
			 const struct ABiTable *iatable_m,
			 const struct ABTable *bptable,
			 const struct ABTable *bmtable,
			 const struct neighbor *neighbor,
			 const struct SUn *U,
			 const struct Fermion *a_y,
			 const struct Fermion *b_x,
			 void *rb[])
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(doc_1mK1xA1xBxFx)(r_y,
				  lx_low + start, lx_count, Ls,
				  ktable,
				  iatable_p, iatable_m, 
				  bptable, bmtable,
				  neighbor,
				  U, a_y, b_x, rb));
}

size_t
qx(omp_do_AxpBxFx)(struct Q(State) *state,
		   struct Fermion *r_x,
		   size_t start, size_t size, size_t Ls,
		   const struct ABTable *aptable,
		   const struct ABTable *amtable,
		   const struct ABTable *bptable,
		   const struct ABTable *bmtable,
		   const struct neighbor *neighbor,
		   const struct SUn *U,
		   const struct Fermion *s_x,
		   const struct Fermion *s_y,
		   void *rb[])
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(do_AxpBxFx)(r_x,
			    lx_low + start, lx_count, Ls,
			    aptable, amtable, bptable, bmtable, neighbor,
			    U, s_x, s_y, rb));
}

size_t
qx(omp_do_A_conj)(struct Q(State) *state,
		  struct Fermion *r_x,
		  size_t size, size_t Ls,
		  const struct ABTable *axptable,
		  const struct ABTable *axmtable,
		  const struct Fermion *s_x)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(do_A_conj)(r_x,
			   lx_low, lx_count, Ls,
			   axptable, axmtable,
			   s_x));

}

size_t
qx(omp_do_A1xBxFx)(struct Q(State) *state,
		   struct Fermion *r_x,
		   size_t start, size_t size, size_t Ls,
		   const struct ABiTable *aiptable,
		   const struct ABiTable *aimtable,
		   const struct ABTable *bptable,
		   const struct ABTable *bmtable,
		   const struct neighbor *neighbor,
		   const struct SUn *U,
		   const struct Fermion *s_y,
		   void *rb[])
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(do_A1xBxFx)(r_x,
			    lx_low + start, lx_count, Ls,
			    aiptable, aimtable, bptable, bmtable, neighbor,
			    U, s_y, rb));

}

size_t
qx(omp_do_A1xBx)(struct Q(State) *state,
		 struct Fermion *r_y,
		 size_t size, size_t Ls,
		 const struct ABTable *bptable,
		 const struct ABTable *bmtable,
		 const struct ABiTable *iatable_p,
		 const struct ABiTable *iatable_m,
		 const struct Fermion *b_y)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(do_A1xBx)(r_y,
			  lx_low, lx_count, Ls,
			  bptable, bmtable, iatable_p, iatable_m,
			  b_y));
}

size_t
qx(omp_do_A_conj_inverse)(struct Q(State) *state,
			  struct Fermion *r,
			  size_t size, size_t Ls,
			  const struct ABiTable *iatable_p,
			  const struct ABiTable *iatable_m,
			  const struct Fermion *x)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(do_A_conj_inverse)(r,
				   lx_low, lx_count, Ls,
				   iatable_p, iatable_m,
				   x));
}

size_t
qx(omp_do_A_inverse)(struct Q(State) *state,
		     struct Fermion *r,
		     size_t size, size_t Ls,
		     const struct ABiTable *iatable_p,
		     const struct ABiTable *iatable_m,
		     const struct Fermion *x)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(do_A_inverse)(r,
			      lx_low, lx_count, Ls,
			      iatable_p, iatable_m,
			      x));
}

size_t
qx(omp_do_ApF)(struct Q(State) *state,
	       struct Fermion *r_x,
	       size_t start, size_t size, size_t Ls,
	       const struct ABTable *aptable,
	       const struct ABTable *amtable,
	       const struct neighbor *neighbor,
	       const struct SUn *U,
	       const struct Fermion *s_x,
	       const struct Fermion *s_y,
	       void *rb[])
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(do_ApF)(r_x,
			lx_low + start, lx_count, Ls,
			aptable, amtable, neighbor,
			U, s_x, s_y, rb));
}

size_t
qx(omp_do_A)(struct Q(State) *state,
	     struct Fermion *r_x,
	     size_t size, size_t Ls,
	     const struct ABTable *aptable,
	     const struct ABTable *amtable,
	     const struct Fermion *s_x)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(do_A)(r_x,
		      lx_low, lx_count, Ls,
		      aptable, amtable,
		      s_x));
}

size_t
qx(omp_do_BA1F)(struct Q(State) *state,
		struct Fermion *r_y,
		size_t start, size_t size, size_t Ls,
		const struct ABTable *bptable,
		const struct ABTable *bmtable,
		const struct ABiTable *iatable_p,
		const struct ABiTable *iatable_m,
		const struct neighbor *neighbor,
		const struct SUn *U,
		const struct Fermion *s_x,
		void *rb[])
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(do_BA1F)(r_y,
			 lx_low + start, lx_count, Ls,
			 bptable, bmtable, iatable_p, iatable_m, neighbor,
			 U, s_x, rb));
}

size_t
qx(omp_do_BA1)(struct Q(State) *state,
	       struct Fermion *r_x,
	       size_t size, size_t Ls,
	       const struct ABTable *bptable,
	       const struct ABTable *bmtable,
	       const struct ABiTable *iatable_p,
	       const struct ABiTable *iatable_m,
	       const struct Fermion *s_x)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(do_BA1)(r_x,
			lx_low, lx_count, Ls,
			bptable, bmtable, iatable_p, iatable_m,
			s_x));
}

size_t
qx(omp_do_BA1K)(struct Q(State) *state,
		struct Fermion *r_x,
		size_t size, size_t Ls,
		const struct ABTable *bptable,
		const struct ABTable *bmtable,
		const struct ABiTable *iatable_p,
		const struct ABiTable *iatable_m,
		const struct KTable *ktable,
		const struct Fermion *s_x)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(do_BA1K)(r_x,
			 lx_low, lx_count, Ls,
			 bptable, bmtable, iatable_p, iatable_m,
			 ktable,
			 s_x));
}

size_t
qx(omp_doc_BA1K)(struct Q(State) *state,
		 struct Fermion *r_x,
		 size_t size, size_t Ls,
		 const struct ABTable *bptable,
		 const struct ABTable *bmtable,
		 const struct ABiTable *iatable_p,
		 const struct ABiTable *iatable_m,
		 const struct KTable *ktable,
		 const struct Fermion *s_x)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(doc_BA1K)(r_x,
			  lx_low, lx_count, Ls,
			  bptable, bmtable, iatable_p, iatable_m,
			  ktable,
			  s_x));
}

size_t
qx(omp_do_K)(struct Q(State) *state,
	     struct Fermion *r_x,
	     size_t size, size_t Ls,
	     const struct KTable *ktable,
	     const struct Fermion *s_x)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(do_K)(r_x,
		      lx_low, lx_count, Ls,
		      ktable,
		      s_x));
}

size_t
qx(omp_doc_1mBA1F)(struct Q(State) *state,
		   struct Fermion *r_y,
		   size_t start, size_t size, size_t Ls,
		   const struct ABTable *bptable,
		   const struct ABTable *bmtable,
		   const struct ABiTable *iatable_p,
		   const struct ABiTable *iatable_m,
		   const struct neighbor *neighbor,
		   const struct SUn *U,
		   const struct Fermion *a_y,
		   const struct Fermion *b_x,
		   void *rb[])
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(doc_1mBA1F)(r_y,
			    lx_low + start, lx_count, Ls,
			    bptable, bmtable, iatable_p, iatable_m, neighbor,
			    U, a_y, b_x, rb));
}

size_t
qx(omp_doc_AxpBxFx)(struct Q(State) *state,
		    struct Fermion *r_x,
		    size_t start, size_t size, size_t Ls,
		    const struct ABTable *aptable,
		    const struct ABTable *amtable,
		    const struct ABTable *bptable,
		    const struct ABTable *bmtable,
		    const struct neighbor *neighbor,
		    const struct SUn *U,
		    const struct Fermion *s_x,
		    const struct Fermion *s_y,
		    void *rb[])
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(doc_AxpBxFx)(r_x,
			     lx_low + start, lx_count, Ls,
			     aptable, amtable, bptable, bmtable, neighbor,
			     U, s_x, s_y, rb));
}

size_t
qx(omp_doc_A_conj)(struct Q(State) *state,
		   struct Fermion *r_x,
		   size_t size, size_t Ls,
		   const struct ABTable *axptable,
		   const struct ABTable *axmtable,
		   const struct Fermion *s_x)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(doc_A_conj)(r_x,
			    lx_low, lx_count, Ls,
			    axptable, axmtable,
			    s_x));
}

size_t
qx(omp_doc_A1xBxFx)(struct Q(State) *state,
		    struct Fermion *r_x,
		    size_t start, size_t size, size_t Ls,
		    const struct ABiTable *aiptable,
		    const struct ABiTable *aimtable,
		    const struct ABTable *bptable,
		    const struct ABTable *bmtable,
		    const struct neighbor *neighbor,
		    const struct SUn *U,
		    const struct Fermion *s_y,
		    void *rb[])
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(doc_A1xBxFx)(r_x,
			     lx_low + start, lx_count, Ls,
			     aiptable, aimtable, bptable, bmtable, neighbor,
			     U, s_y, rb));
}

size_t
qx(omp_doc_A1xBx)(struct Q(State) *state,
		  struct Fermion *r_y,
		  size_t size, size_t Ls,
		  const struct ABTable *bptable,
		  const struct ABTable *bmtable,
		  const struct ABiTable *iatable_p,
		  const struct ABiTable *iatable_m,
		  const struct Fermion *b_y)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(doc_A1xBx)(r_y,
			   lx_low, lx_count, Ls,
			   bptable, bmtable, iatable_p, iatable_m,
			   b_y));
}

size_t
qx(omp_doc_A_inverse)(struct Q(State) *state,
		      struct Fermion *r,
		      size_t size, size_t Ls,
		      const struct ABiTable *iatable_p,
		      const struct ABiTable *iatable_m,
		      const struct Fermion *x)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(doc_A_inverse)(r,
			       lx_low, lx_count, Ls,
			       iatable_p, iatable_m,
			       x));
}

size_t
qx(omp_doc_A_conj_inverse)(struct Q(State) *state,
			   struct Fermion *r,
			   size_t size, size_t Ls,
			   const struct ABiTable *iatable_p,
			   const struct ABiTable *iatable_m,
			   const struct Fermion *x)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(doc_A_conj_inverse)(r,
				    lx_low, lx_count, Ls,
				    iatable_p, iatable_m,
				    x));
}

size_t
qx(omp_doc_ApF)(struct Q(State) *state,
		struct Fermion *r_x,
		size_t start, size_t size, size_t Ls,
		const struct ABTable *aptable,
		const struct ABTable *amtable,
		const struct neighbor *neighbor,
		const struct SUn *U,
		const struct Fermion *s_x,
		const struct Fermion *s_y,
		void *rb[])
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(doc_ApF)(r_x,
			 lx_low + start, lx_count, Ls,
			 aptable, amtable, neighbor,
			 U, s_x, s_y, rb));
}

size_t
qx(omp_doc_A)(struct Q(State) *state,
	      struct Fermion *r_x,
	      size_t size, size_t Ls,
	      const struct ABTable *aptable,
	      const struct ABTable *amtable,
	      const struct Fermion *s_x)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(doc_A)(r_x,
		       lx_low, lx_count, Ls,
		       aptable, amtable,
		       s_x));
}

size_t
qx(omp_doc_BA1F)(struct Q(State) *state,
		 struct Fermion *r_y,
		 size_t start, size_t size, size_t Ls,
		 const struct ABTable *bptable,
		 const struct ABTable *bmtable,
		 const struct ABiTable *iatable_p,
		 const struct ABiTable *iatable_m,
		 const struct neighbor *neighbor,
		 const struct SUn *U,
		 const struct Fermion *s_x,
		 void *rb[])
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(doc_BA1F)(r_y,
			  lx_low + start, lx_count, Ls,
			  bptable,bmtable, iatable_p, iatable_m, neighbor,
			  U, s_x, rb));
}

size_t
qx(omp_doc_BA1)(struct Q(State) *state,
		struct Fermion *r_x,
		size_t size, size_t Ls,
		const struct ABTable *bptable,
		const struct ABTable *bmtable,
		const struct ABiTable *iatable_p,
		const struct ABiTable *iatable_m,
		const struct Fermion *s_x)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(doc_BA1)(r_x,
			 lx_low, lx_count, Ls,
			 bptable, bmtable, iatable_p, iatable_m,
			 s_x));
}

size_t
qx(omp_doc_K)(struct Q(State) *state,
	      struct Fermion *r_x,
	      size_t size, size_t Ls,
	      const struct KTable *ktable,
	      const struct Fermion *s_x)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(doc_K)(r_x,
		       lx_low, lx_count, Ls,
		       ktable,
		       s_x));
}

size_t
qx(omp_doc_K_conj)(struct Q(State) *state,
		   struct Fermion *r_x,
		   size_t size, size_t Ls,
		   const struct KTable *ktable,
		   const struct Fermion *s_x)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(doc_Kx)(r_x,
			lx_low, lx_count, Ls,
			ktable,
			s_x));
}

void
qx(omp_fermion2blas)(struct Q(State) *state,
		     void *data,
		     const struct Fermion *f,
		     size_t size, size_t Ls)
{
  DECLARE_VOID();
#ifdef _OPENMP
#pragma omp parallel for
#endif
  LOOP_VOID(qx(fermion2blas)(data, f,
			     lx_low, lx_count, Ls));
}

void
qx(omp_blas2fermion)(struct Q(State) *state,
		     struct Fermion *f,
		     size_t size, size_t Ls,
		     const void *data)
{
  DECLARE_VOID();
#ifdef _OPENMP
#pragma omp parallel for
#endif
  LOOP_VOID(qx(blas2fermion)(f,
			     lx_low, lx_count, Ls,
			     data));
}

size_t
qx(omp_cg_xp)(struct Q(State) *state,
	      struct Fermion *x,
	      struct Fermion *p,
	      size_t size, size_t Ls,
	      double alpha,
	      double beta,
	      const struct Fermion *r)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(cg_xp)(x, p,
		       lx_low, lx_count, Ls,
		       alpha, beta,
		       r));
}

size_t
qx(omp_scg_madd)(struct Q(State) *state,
		 struct Fermion *xi_e,
		 struct VectorFermion *v_xi_e,
		 size_t size, size_t Ls, size_t count,
		 double a,
		 const double *ad,
		 const struct Fermion *pi_e)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(scg_madd)(xi_e, v_xi_e,
			  lx_low, lx_count, Ls, count,
			  a, ad, pi_e));
}

size_t
qx(omp_scg_xp)(struct Q(State) *state,
	       struct Fermion *xi_e,
	       struct Fermion *pi_e,
	       struct VectorFermion *v_xi_e,
	       struct VectorFermion *v_pi_e,
	       size_t size, size_t Ls, size_t count,
	       double a,
	       double b,
	       const double *ad,
	       const double *bdd,
	       const struct Fermion *rho_e)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(scg_xp)(xi_e, pi_e, v_xi_e, v_pi_e,
			lx_low, lx_count, Ls, count,
			a, b, ad, bdd, rho_e));
}

size_t
qx(omp_do_1mFx)(struct Q(State) *state,
		struct Fermion *r_y,
		size_t start, size_t size, size_t Ls,
		const struct neighbor *neighbor,
		const struct SUn *U,
		const struct Fermion *a_y,
		const struct Fermion *b_x,
		void *rb[])
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(do_1mFx)(r_y,
			 lx_low + start, lx_count, Ls,
			 neighbor,
			 U, a_y, b_x, rb));
}

size_t
qx(omp_do_1mF)(struct Q(State) *state,
	       struct Fermion *r_y,
	       size_t start, size_t size, size_t Ls,
	       const struct neighbor *neighbor,
	       const struct SUn *U,
	       const struct Fermion *a_y,
	       const struct Fermion *b_x,
	       void *rb[])
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(do_1mF)(r_y,
			lx_low + start, lx_count, Ls,
			neighbor,
			U, a_y, b_x, rb));
}

size_t
qx(omp_do_1mKF)(struct Q(State) *state,
		struct Fermion *r_y,
		size_t start, size_t size, size_t Ls,
		const struct KTable *ktable,
		const struct neighbor *neighbor,
		const struct SUn *U,
		const struct Fermion *a_y,
		const struct Fermion *b_x,
		void *rb[])
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(do_1mKF)(r_y,
			 lx_low + start, lx_count, Ls,
			 ktable,
			 neighbor,
			 U, a_y, b_x, rb));
}

size_t
qx(omp_doc_1mKF)(struct Q(State) *state,
		 struct Fermion *r_y,
		 size_t start, size_t size, size_t Ls,
		 const struct KTable *ktable,
		 const struct neighbor *neighbor,
		 const struct SUn *U,
		 const struct Fermion *a_y,
		 const struct Fermion *b_x,
		 void *rb[])
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(doc_1mKF)(r_y,
			  lx_low + start, lx_count, Ls,
			  ktable,
			  neighbor,
			  U, a_y, b_x, rb));
}

void
qx(omp_f_zero)(struct Q(State) *state,
	       struct Fermion *dst, 
	       size_t size, size_t Ls)
{
  DECLARE_VOID();
#ifdef _OPENMP
#pragma omp parallel for
#endif
  LOOP_VOID(qx(f_zero)(dst, 
		       lx_low, lx_count, Ls));
}

void
qx(omp_f_copy)(struct Q(State) *state,
	       struct Fermion *dst, 
	       size_t size, size_t Ls,
	       const struct Fermion *src)
{
  DECLARE_VOID();
#ifdef _OPENMP
#pragma omp parallel for
#endif
  LOOP_VOID(qx(f_copy)(dst, 
		       lx_low, lx_count, Ls,
		       src));
}

size_t
qx(omp_f_add3)(struct Q(State) *state,
	       struct Fermion *r,
	       size_t size, size_t Ls,
	       const struct Fermion *a,
	       double s,
	       const struct Fermion *b)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(f_add3)(r,
			lx_low, lx_count, Ls,
			a, s, b));
}

size_t
qx(omp_f_add2)(struct Q(State) *state,
	       struct Fermion *r,
	       size_t size, size_t Ls,
	       double s,
	       const struct Fermion *b)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(f_add2)(r,
			lx_low, lx_count, Ls,
			s, b));
}

size_t
qx(omp_f_cadd2)(struct Q(State) *state,
		struct Fermion *r,
		size_t size, size_t Ls,
		double sr, double si,
		const struct Fermion *b)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(f_cadd2)(r,
			 lx_low, lx_count, Ls,
			 sr, si, b));
}

size_t
qx(omp_f_rmul1)(struct Q(State) *state,
		struct Fermion *r,
		size_t size, size_t Ls,
		double s)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(f_rmul1)(r,
			 lx_low, lx_count, Ls,
			 s));
}

size_t
qx(omp_f_add2x)(struct Q(State) *state,
		struct Fermion *r,
		size_t size, size_t Ls,
		double s,
		const struct Fermion *b)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(f_add2x)(r,
			 lx_low, lx_count, Ls,
			 s, b));
}

void
qx(omp_fv_zero)(struct Q(State) *state,
		struct VectorFermion *vf,
		size_t size, size_t Ls, size_t count)
{
  DECLARE_VOID();
#ifdef _OPENMP
#pragma omp parallel for
#endif
  LOOP_VOID(qx(fv_zero)(vf,
			lx_low, lx_count, Ls, count));
}

void
qx(omp_fv_copy)(struct Q(State) *state,
		     struct VectorFermion *vf,
		     size_t size, size_t Ls, size_t count,
		     const struct Fermion *f)
{
  DECLARE_VOID();
#ifdef _OPENMP
#pragma omp parallel for
#endif
  LOOP_VOID(qx(fv_copy)(vf,
			lx_low, lx_count, Ls, count,
			f));
}

void
qx(omp_fv_get)(struct Q(State) *state,
	       struct Fermion *f,
	       size_t size, size_t Ls, size_t count,
	       const struct VectorFermion *vf, size_t k)
{
  DECLARE_VOID();
#ifdef _OPENMP
#pragma omp parallel for
#endif
  LOOP_VOID(qx(fv_get)(f,
		       lx_low, lx_count, Ls, count,
		       vf, k));
}

void
qx(omp_fv_put)(struct Q(State) *state,
	       struct VectorFermion *vf, size_t k,
	       size_t size, size_t Ls, size_t count,
	       const struct Fermion *f)
{
  DECLARE_VOID();
#ifdef _OPENMP
#pragma omp parallel for
#endif
  LOOP_VOID(qx(fv_put)(vf, k,
		       lx_low, lx_count, Ls, count,
		       f));
}

size_t
qx(omp_vf_copy)(struct Q(State) *state, size_t size, size_t Ls, size_t len,
		struct vFermion *fv, size_t fv_stride, size_t fv_begin,
		const struct vFermion *gv, size_t gv_stride, size_t gv_begin)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(vf_copy)(lx_low, lx_count, Ls, len,
			 fv, fv_stride, fv_begin,
			 gv, gv_stride, gv_begin));
}

size_t
qx(omp_vf_put)(struct Q(State) *state, size_t size, size_t Ls,
	       struct vFermion *fv, size_t fv_stride, size_t fv_idx,
	       const struct Fermion *x)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(vf_put)(lx_low, lx_count, Ls,
			fv, fv_stride, fv_idx,
			x));
}

size_t
qx(omp_vf_get)(struct Q(State) *state, size_t size, size_t Ls,
	       struct Fermion *x,
	       const struct vFermion *fv, size_t fv_stride, size_t fv_idx)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(vf_get)(lx_low, lx_count, Ls,
			x,
			fv, fv_stride, fv_idx));
}

size_t
qx(omp_vf_dot_vz)(struct Q(State) *state, size_t size, size_t Ls,
		  struct Fermion *g,
		  const struct vFermion *fv,
		  size_t fv_stride, size_t fv_begin, size_t fv_len,
		  const double *v)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(vf_dot_vz)(lx_low, lx_count, Ls,
			   g,
			   fv, fv_stride, fv_begin, fv_len,
			   v));
}

size_t
qx(omp_vf_dot_mz)(struct Q(State) *state, size_t size, size_t Ls,
		  struct vFermion *gv,
		  size_t gv_stride, size_t gv_begin, size_t gv_len,
		  const struct vFermion *fv,
		  size_t fv_stride, size_t fv_begin, size_t fv_len,
		  const double *m, size_t ldm)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(vf_dot_mz)(lx_low, lx_count, Ls,
			   gv, gv_stride, gv_begin, gv_len,
			   fv, fv_stride, fv_begin, fv_len,
			   m, ldm));
}

size_t
qx(omp_do_1mBA1F_norm)(struct Q(State) *state,
		       struct Fermion *r_y,
		       double *local_norm,
		       size_t start, size_t size, size_t Ls,
		       const struct ABTable *bptable,
		       const struct ABTable *bmtable,
		       const struct ABiTable *iatable_p,
		       const struct ABiTable *iatable_m,
		       const struct neighbor *neighbor,
		       const struct SUn *U,
		       const struct Fermion *a_y,
		       const struct Fermion *b_x,
		       void *rb[])
{
  DECLARE_NORM2();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops) reduction(+:lx_local_norm)
#endif
  LOOP_NORM2(*local_norm, qx(do_1mBA1F_norm)(r_y,
					     &lx_local_norm,
					     lx_low + start, lx_count, Ls,
					     bptable, bmtable, iatable_p, iatable_m, neighbor,
					     U, a_y, b_x, rb));
}

size_t
qx(omp_do_1mK1xA1xBxFx_norm)(struct Q(State) *state,
			     struct Fermion *r_y,
			     double *local_norm,
			     size_t start, size_t size, size_t Ls,
			     const struct KTable *ktable,
			     const struct ABiTable *iatable_p,
			     const struct ABiTable *iatable_m,
			     const struct ABTable *bptable,
			     const struct ABTable *bmtable,
			     const struct neighbor *neighbor,
			     const struct SUn *U,
			     const struct Fermion *a_y,
			     const struct Fermion *b_x,
			     void *rb[])
{
  DECLARE_NORM2();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops) reduction(+:lx_local_norm)
#endif
  LOOP_NORM2(*local_norm, qx(do_1mK1xA1xBxFx_norm)(r_y,
						   &lx_local_norm,
						   lx_low + start, lx_count, Ls,
						   ktable,
						   iatable_p, iatable_m,
						   bptable, bmtable,
						   neighbor,
						   U, a_y, b_x, rb));
}

size_t
qx(omp_doc_1mK1xA1xBxFx_norm)(struct Q(State) *state,
			      struct Fermion *r_y,
			      double *local_norm,
			      size_t start, size_t size, size_t Ls,
			      const struct KTable *ktable,
			      const struct ABiTable *iatable_p,
			      const struct ABiTable *iatable_m,
			      const struct ABTable *bptable,
			      const struct ABTable *bmtable,
			      const struct neighbor *neighbor,
			      const struct SUn *U,
			      const struct Fermion *a_y,
			      const struct Fermion *b_x,
			      void *rb[])
{
  DECLARE_NORM2();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops) reduction(+:lx_local_norm)
#endif
  LOOP_NORM2(*local_norm, qx(doc_1mK1xA1xBxFx_norm)(r_y,
						    &lx_local_norm,
						    lx_low + start, lx_count, Ls,
						    ktable,
						    iatable_p, iatable_m,
						    bptable, bmtable,
						    neighbor,
						    U, a_y, b_x, rb));
}

size_t
qx(omp_do_ApF_norm)(struct Q(State) *state,
		    struct Fermion *r_x,
		    double *local_norm,
		    size_t start, size_t size, size_t Ls,
		    const struct ABTable *aptable,
		    const struct ABTable *amtable,
		    const struct neighbor *neighbor,
		    const struct SUn *U,
		    const struct Fermion *s_x,
		    const struct Fermion *s_y,
		    void *rb[])
{
  DECLARE_NORM2();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops) reduction(+:lx_local_norm)
#endif
  LOOP_NORM2(*local_norm, qx(do_ApF_norm)(r_x,
					  &lx_local_norm,
					  lx_low + start, lx_count, Ls,
					  aptable, amtable, neighbor,
					  U, s_x, s_y, rb));
}

size_t
qx(omp_doc_1mBA1F_norm)(struct Q(State) *state,
			struct Fermion *r_y,
			double *local_norm,
			size_t start, size_t size, size_t Ls,
			const struct ABTable *bptable,
			const struct ABTable *bmtable,
			const struct ABiTable *iatable_p,
			const struct ABiTable *iatable_m,
			const struct neighbor *neighbor,
			const struct SUn *U,
			const struct Fermion *a_y,
			const struct Fermion *b_x,
			void *rb[])
{
  DECLARE_NORM2();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops) reduction(+:lx_local_norm)
#endif
  LOOP_NORM2(*local_norm, qx(doc_1mBA1F_norm)(r_y,
					      &lx_local_norm,
					      lx_low + start, lx_count, Ls,
					      bptable, bmtable, iatable_p, iatable_m, neighbor,
					      U, a_y, b_x, rb));
}

size_t
qx(omp_doc_ApF_norm)(struct Q(State) *state,
		     struct Fermion *r_x,
		     double *local_norm,
		     size_t start, size_t size, size_t Ls,
		     const struct ABTable *aptable,
		     const struct ABTable *amtable,
		     const struct neighbor *neighbor,
		     const struct SUn *U,
		     const struct Fermion *s_x,
		     const struct Fermion *s_y,
		     void *rb[])
{
  DECLARE_NORM2();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops) reduction(+:lx_local_norm)
#endif
  LOOP_NORM2(*local_norm, qx(doc_ApF_norm)(r_x,
					   &lx_local_norm,
					   lx_low + start, lx_count, Ls,
					   aptable, amtable, neighbor,
					   U, s_x, s_y, rb));
}

size_t
qx(omp_do_1mFx_norm)(struct Q(State) *state,
		     struct Fermion *r_y,
		     double *local_norm,
		     size_t start, size_t size, size_t Ls,
		     const struct neighbor *neighbor,
		     const struct SUn *U,
		     const struct Fermion *a_y,
		     const struct Fermion *b_x,
		     void *rb[])
{
  DECLARE_NORM2();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops) reduction(+:lx_local_norm)
#endif
  LOOP_NORM2(*local_norm, qx(do_1mFx_norm)(r_y,
					   &lx_local_norm,
					   lx_low + start, lx_count, Ls,
					   neighbor,
					   U, a_y, b_x, rb));
}

size_t
qx(omp_do_1mKF_norm)(struct Q(State) *state,
		     struct Fermion *r_y,
		     double *local_norm,
		     size_t start, size_t size, size_t Ls,
		     const struct KTable *ktable,
		     const struct neighbor *neighbor,
		     const struct SUn *U,
		     const struct Fermion *a_y,
		     const struct Fermion *b_x,
		     void *rb[])
{
  DECLARE_NORM2();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops) reduction(+:lx_local_norm)
#endif
  LOOP_NORM2(*local_norm, qx(do_1mKF_norm)(r_y,
					   &lx_local_norm,
					   lx_low + start, lx_count, Ls,
					   ktable,
					   neighbor,
					   U, a_y, b_x, rb));
}

size_t
qx(omp_doc_1mKF_norm)(struct Q(State) *state,
		      struct Fermion *r_y,
		      double *local_norm,
		      size_t start, size_t size, size_t Ls,
		      const struct KTable *ktable,
		      const struct neighbor *neighbor,
		      const struct SUn *U,
		      const struct Fermion *a_y,
		      const struct Fermion *b_x,
		      void *rb[])
{
  DECLARE_NORM2();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops) reduction(+:lx_local_norm)
#endif
  LOOP_NORM2(*local_norm, qx(doc_1mKF_norm)(r_y,
					    &lx_local_norm,
					    lx_low + start, lx_count, Ls,
					    ktable,
					    neighbor,
					    U, a_y, b_x, rb));
}

size_t
qx(omp_f_add2_norm)(struct Q(State) *state,
		    struct Fermion *r,
		    double *local_norm,
		    size_t size, size_t Ls,
		    double s,
		    const struct Fermion *b)
{
  DECLARE_NORM2();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops) reduction(+:lx_local_norm)
#endif
  LOOP_NORM2(*local_norm, qx(f_add2_norm)(r,
					  &lx_local_norm,
					  lx_low, lx_count, Ls,
					  s, b));
}

size_t
qx(omp_f_diff_norm)(struct Q(State) *state,
		    double *s,
		    size_t size, size_t Ls,
		    const struct Fermion *a,
		    const struct Fermion *b)
{
  DECLARE_NORM2();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops) reduction(+:lx_local_norm)
#endif
  LOOP_NORM2(*s, qx(f_diff_norm)(&lx_local_norm,
				 lx_low, lx_count, Ls,
				 a, b));
}

size_t
qx(omp_f_norm)(struct Q(State) *state,
	       double *s,
	       size_t size, size_t Ls,
	       const struct Fermion *a)
{
  DECLARE_NORM2();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops) reduction(+:lx_local_norm)
#endif
  LOOP_NORM2(*s, qx(f_norm)(&lx_local_norm,
			    lx_low, lx_count, Ls,
			    a));
}

size_t
qx(omp_f_dot)(struct Q(State) *state,
	      double *v_r, double *v_i,
	      size_t size, size_t Ls,
	      const struct Fermion *a,
	      const struct Fermion *b)
{
  size_t lx_idx;
  size_t lx_limit = state->threads;
  size_t lx_flops = 0;
  double lx_r = 0.0;
  double lx_i = 0.0;
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops) reduction(+:lx_r) reduction(+:lx_i)
#endif
  for (lx_idx = 0; lx_idx < lx_limit; lx_idx++) {
    size_t lx_low = (lx_idx * size) / lx_limit;
    size_t lx_high = ((lx_idx + 1) * size) / lx_limit;
    size_t lx_count = lx_high - lx_low;
    double dot_r = 0.0;
    double dot_i = 0.0;
    lx_flops += qx(f_dot)(&dot_r, &dot_i,
			  lx_low, lx_count, Ls,
			  a, b);
    lx_r += dot_r;
    lx_i += dot_i;
  }
  *v_r = lx_r;
  *v_i = lx_i;
  return lx_flops;
}

size_t
qx(omp_do_vfH_dot_f)(struct Q(State) *state, size_t size, size_t Ls,
		     double *c, /* [fv_len * 2] */
		     const struct vFermion *fv,
		     size_t fv_stride, size_t fv_begin, size_t fv_len,
		     const struct Fermion *g)
{
  size_t lx_idx;
  size_t lx_limit = state->threads;
  size_t lx_flops = 0;
  double lx_c[2 * fv_len * lx_limit];
  size_t i;

  for (i = 0; i < 2 * fv_len * lx_limit; i++) {
    lx_c[i] = 0;
  }
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  for (lx_idx = 0; lx_idx < lx_limit; lx_idx++) {
    size_t lx_low = (lx_idx * size) / lx_limit;
    size_t lx_high = ((lx_idx + 1) * size) / lx_limit;
    size_t lx_count = lx_high - lx_low;
    lx_flops += qx(do_vfH_dot_f)(lx_low, lx_count, Ls,
				 lx_c + lx_idx * 2 * fv_len,
				 fv, fv_stride, fv_begin, fv_len,
				 g);
  }
  for (i = 0; i < 2 * fv_len; i++) {
    double r = 0;
    size_t j;
    for (j = 0; j < lx_limit; j++) {
      r += lx_c[2 * fv_len * j + i];
    }
    c[i] = r;
  }
  return lx_flops;
}

#if 0 /* XXX */
size_t
qx(omp_do_vfH_dot_vf)(struct Q(State) *state, size_t size, size_t Ls,
		      double *c, size_t ldc,
		      const struct vFermion *fv,
		      size_t fv_stride, size_t fv_begin, size_t fv_len,
		      const struct vFermion *gv,
		      size_t gv_stride, size_t gv_begin, size_t gv_len)
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(do_vfH_dot_vf)(struct Q(State) *state, size_t size, size_t Ls,
		      double *c, size_t ldc,
		      const struct vFermion *fv,
		      size_t fv_stride, size_t fv_begin, size_t fv_len,
		      const struct vFermion *gv,
			       size_t gv_stride, size_t gv_begin, size_t gv_len));
}

#endif /* XXX */
