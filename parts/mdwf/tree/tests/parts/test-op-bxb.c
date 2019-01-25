#include <qop-mdwf3.h>
#include "../../port/mdwf.h"
#include "opxtest.h"
#include "op-routines.h"

char *op_a_name = "conj(B)";
char *op_b_name = "B";

double
read_gauge(int dir,
	   const int pos[4],
	   int a, int b,
	   int re_im,
	   void *env)
{
    int i;
    unsigned int v = sum_init(seed_u);

    v = sum_add(v, re_im);
    v = sum_add(v, a);
    v = sum_add(v, b);
    for (i = 0; i < 4; i++) {
	v = sum_add(v, pos[i]);
	v = sum_add(v, dir);
    }
    v = sum_add(v, seed_u);
    return sum_fini(v);
}

static double
read_fermion(unsigned int seed,
	     const int pos[5],
	     int c, int d,
	     int re_im)
{
    int i;
    unsigned int v = sum_init(seed);
    v = sum_add(v, c);
    v = sum_add(v, d);
    v = sum_add(v, re_im);
    for (i = 0; i < 5; i++)
	v = sum_add(v, pos[i]);
    v = sum_add(v, seed);
    return sum_fini(v);
}

double
read_fermion_a(const int pos[5],
	       int c, int d,
	       int re_im,
	       void *env)
{
    return read_fermion(seed_a, pos, c, d, re_im);
}

double
read_fermion_b(const int pos[5],
	       int c, int d,
	       int re_im,
	       void *env)
{
    return read_fermion(seed_b, pos, c, d, re_im);
}

static int
operator_B(struct QX(Fermion) *result,
	   const struct Q(Parameters) *params,
	   const struct QX(Gauge) *gauge,
	   const struct QX(Fermion) *fermion)
{
    qx(do_A)(result->even,
	     result->state->even.full_size,
	     result->state->even.Ls,
	     params->BpTable,
	     params->BmTable,
	     fermion->even);
    qx(do_A)(result->odd,
	     result->state->odd.full_size,
	     result->state->odd.Ls,
	     params->BpTable,
	     params->BmTable,
	     fermion->odd);
    return 0;
}

static int
operator_Bx(struct QX(Fermion) *result,
	    const struct Q(Parameters) *params,
	    const struct QX(Gauge) *gauge,
	    const struct QX(Fermion) *fermion)
{
    qx(do_A_conj)(result->even,
		  result->state->even.full_size,
		  result->state->even.Ls,
		  params->BxpTable,
		  params->BxmTable,
		  fermion->even);
    qx(do_A_conj)(result->odd,
		  result->state->odd.full_size,
		  result->state->odd.Ls,
		  params->BxpTable,
		  params->BxmTable,
		  fermion->odd);
    return 0;
}

static void
dot(double *v_r, double *v_i,
    const struct QX(Fermion) *a,
    const struct QX(Fermion) *b)
{
    double r1, r2, i1, i2;

    qx(f_dot)(&r1, &i1, a->state->even.full_size, a->state->even.Ls,
	      a->even, b->even);
    qx(f_dot)(&r2, &i2, a->state->odd.full_size, a->state->odd.Ls,
	      a->odd, b->odd);
    *v_r = r1 + r2;
    *v_i = i1 + i2;
}

int
operator_a(void)
{
    double x, y;
    struct QX(Fermion) *fermion_x;

    if (QOP_MDWF_allocate_fermion(&fermion_x, gauge->state)) {
	zprint("operator_B(): alloc failed");
	return 1;
    }

    operator_B(fermion_x, params, gauge, fermion_a);
    dot(&x, &y, fermion_b, fermion_x);
    QOP_MDWF_free_fermion(&fermion_x);
    zprint("normal: %20.10e %20.10e", x, y);
    return 0;
}

int
operator_b(void)
{
    double x, y;
    struct QX(Fermion) *fermion_x;

    if (QOP_MDWF_allocate_fermion(&fermion_x, gauge->state)) {
	zprint("operator_Bx(): alloc failed");
	return 1;
    }

    operator_Bx(fermion_x, params, gauge, fermion_b);
    dot(&x, &y, fermion_x, fermion_a);
    QOP_MDWF_free_fermion(&fermion_x);
    zprint("conj  : %20.10e %20.10e", x, y);
    return 0;
}
