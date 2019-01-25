#include <mdwf.h>
#include "helpers-omp.h"

static unsigned int
qx(omp_do_F_conj)(struct Q(State) *state,
		  struct Fermion *res_x,
		  int start, int size, int Ls,
		  const struct neighbor *neighbor,
		  const struct SUn *U,
		  const struct Fermion *src_y,
		  void *rb[])
{
  DECLARE_FLOPS();
#ifdef _OPENMP
#pragma omp parallel for reduction(+:lx_flops)
#endif
  LOOP_FLOPS(qx(do_F_conj)(res_x,
			   lx_low + start, lx_count, Ls,
			   neighbor,
			   U, src_y, rb));
}

void
qx(op_Fx)(struct Fermion *r_x,
	  struct eo_lattice *xy,
	  const struct SUn *U,
	  const struct Fermion *s_y,
	  long long *flops,
	  long long *sent,
	  long long *received)
{
    int Ls = xy->Ls;

    qx(boundary)(xy, Ls, qx(up_project_x), qx(down_project_x), U, s_y, flops);

    if (xy->h_valid)
	QMP_start(xy->handle);

    if (xy->body_size)
      *flops += qx(omp_do_F_conj)(xy->state, r_x, 0, xy->body_size, Ls,
				  xy->neighbor, U, s_y, NULL);

    if (xy->h_valid)
	QMP_wait(xy->handle);

    if (xy->face_size)
      *flops += qx(omp_do_F_conj)(xy->state, r_x, xy->body_size, xy->face_size, Ls,
				  xy->neighbor, U, s_y, xy->receive_buf);
    *sent += xy->total_send;
    *received += xy->total_receive;
}
