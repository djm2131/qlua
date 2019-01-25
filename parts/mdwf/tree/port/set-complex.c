#include <mdwf.h>

static struct qf(ABops) qf(complex_ops) = {
  qf(omp_doc_A),
  qf(omp_doc_A_conj),
  qf(omp_doc_A_inverse),
  qf(omp_doc_A_conj_inverse),
  qf(omp_doc_ApF),
  qf(omp_doc_ApF_norm),
  qf(omp_doc_AxpBxFx),
  qf(omp_doc_BA1),
  qf(omp_doc_BA1F),
  qf(omp_doc_1mBA1F),
  qf(omp_doc_1mBA1F_norm),
  qf(omp_doc_A1xBx),
  qf(omp_doc_A1xBxFx),
  qf(omp_doc_K),
  qf(omp_doc_K_conj),
  qf(omp_doc_BA1K),
  qf(omp_doc_1mK1xA1xBxFx),
  qf(omp_doc_1mK1xA1xBxFx_norm),
  qf(omp_doc_1mKF),
  qf(omp_doc_1mKF_norm)
};

static struct qd(ABops) qd(complex_ops) = {
  qd(omp_doc_A),
  qd(omp_doc_A_conj),
  qd(omp_doc_A_inverse),
  qd(omp_doc_A_conj_inverse),
  qd(omp_doc_ApF),
  qd(omp_doc_ApF_norm),
  qd(omp_doc_AxpBxFx),
  qd(omp_doc_BA1),
  qd(omp_doc_BA1F),
  qd(omp_doc_1mBA1F),
  qd(omp_doc_1mBA1F_norm),
  qd(omp_doc_A1xBx),
  qd(omp_doc_A1xBxFx),
  qd(omp_doc_K),
  qd(omp_doc_K_conj),
  qd(omp_doc_BA1K),
  qd(omp_doc_1mK1xA1xBxFx),
  qd(omp_doc_1mK1xA1xBxFx_norm),
  qd(omp_doc_1mKF),
  qd(omp_doc_1mKF_norm)
};

typedef struct { double re, im; } cx;
static cx mkC(double re, double im) { cx v; v.re = re; v.im = im; return v; }
static cx c_neg(cx v) { cx x; x.re = -v.re; x.im = -v.im; return x; }
static cx rc_sub(double r, cx v) { cx x; x.re = r - v.re; x.im = -v.im; return x;}
static cx c_inv(cx v) { double w = 1/(v.re*v.re+v.im*v.im); cx x; x.re = v.re * w; x.im = -v.im * w; return x;}
static cx rc_mul(double a, cx b) { cx r; r.re = a*b.re; r.im=a*b.im; return r; }
static cx c_mul(cx a, cx b) { cx r; r.re = a.re*b.re-a.im*b.im; r.im=a.re*b.im+a.im*b.re; return r; }
static cx c_div(cx a, cx b) { return c_mul(a,c_inv(b)); }

int
Q(set_complex)(struct Q(Parameters) **params_ptr,
	       struct Q(State) *state,
	       const double b_5_re[],
	       const double b_5_im[],
	       const double c_5_re[],
	       const double c_5_im[],
	       double M_5,
	       double m)
{
  char *msg = NULL;
  int i;
  int Ls;
  struct Q(Parameters) *params;
  int abs;
  int iabs;
  int ks;
  int iks;

#define CHECK(p, m) do { if ((p) == NULL) { msg = (m); goto error; }} while(0)
#define u_a(j)    (mkC(c_5_re[j] * (M_5 + 4) - 1, c_5_im[j] * (M_5 + 4)))
#define v_a(j)    (rc_mul(-m, u_a(j)))
#define w_a(j)    (mkC(b_5_re[j] * (M_5 + 4) + 1, b_5_im[j] * (M_5 + 4)))
#define u_b(j)    (mkC(c_5_re[j], c_5_im[j]))
#define v_b(j)    (rc_mul(-m, u_b(j)))
#define w_b(j)    (mkC(b_5_re[j], b_5_im[j]))
#define ikap(j)   (rc_mul(2, w_a(j)))
#define kap(j)    c_inv(ikap(j))

  if (state == NULL || state->error_latched)
    return 1;

  Ls = state->Ls;
  if (params_ptr == NULL)
    return q(set_error)(state, 0, "set_complex(): NULL pointer");

  *params_ptr = NULL;
  params = q(malloc)(state, sizeof (struct Q(Parameters)));
  CHECK(params, "set_complex(): Not enough memory for parameters");
  memset(params, 0, sizeof (struct Q(Parameters)));

  /* size of A & B tables */
  abs = q(sizeof_cABTable)(Ls);
  /* A tables */
  params->ApTable = q(malloc)(state, abs);
  CHECK(params->ApTable, "set_complex(): Not enough memory for A/+ table");
  q(put_cABTable)(params->ApTable, 0, w_a(0).re, w_a(0).im, v_a(0).re, v_a(0).im);
  for (i = 1; i < Ls; i++)
    q(put_cABTable)(params->ApTable, i, w_a(i).re, w_a(i).im, u_a(i).re, u_a(i).im);
  params->AmTable = q(malloc)(state, abs);
  CHECK(params->AmTable, "set_complex(): Not enough memory for A/- table");
  q(put_cABTable)(params->AmTable, 0, w_a(Ls-1).re, w_a(Ls-1).im, v_a(Ls-1).re, v_a(Ls-1).im);
  for (i = 1; i < Ls; i++)
      q(put_cABTable)(params->AmTable, i, w_a(Ls-1-i).re, w_a(Ls-1-i).im, u_a(Ls-1-i).re, u_a(Ls-1-i).im);

  /* B tables */
  params->BpTable = q(malloc)(state, abs);
  CHECK(params->BpTable, "set_complex(): Not enough memory for B/+ table");
  q(put_cABTable)(params->BpTable, 0, w_b(0).re, w_b(0).im, v_b(0).re, v_b(0).im);
  for (i = 1; i < Ls; i++)
      q(put_cABTable)(params->BpTable, i, w_b(i).re, w_b(i).im, u_b(i).re, u_b(i).im);
  params->BmTable = q(malloc)(state, abs);
  CHECK(params->BmTable, "set_complex(): Not enough memory for B/- table");
  q(put_cABTable)(params->BmTable, 0, w_b(Ls-1).re, w_b(Ls-1).im, v_b(Ls-1).re, v_b(Ls-1).im);
  for (i = 1; i < Ls; i++)
      q(put_cABTable)(params->BmTable, i, w_b(Ls-1-i).re, w_b(Ls-1-i).im, u_b(Ls-1-i).re, u_b(Ls-1-i).im);

  /* A* tables */
  params->AxpTable = q(malloc)(state, abs);
  CHECK(params->AxpTable, "set_complex(): Not enough memory for A*/+ table");
  q(put_cABTable)(params->AxpTable, 0, w_a(0).re, w_a(0).im, v_a(Ls-1).re, v_a(Ls-1).im);
  for (i = 1; i < Ls; i++)
      q(put_cABTable)(params->AxpTable, i, w_a(i).re, w_a(i).im, u_a(i-1).re, u_a(i-1).im);
  params->AxmTable = q(malloc)(state, abs);
  CHECK(params->AxmTable, "set_complex(): Not enough memory for A*/- table");
  q(put_cABTable)(params->AxmTable, 0, w_a(Ls-1).re, w_a(Ls-1).im, v_a(0).re, v_a(0).im);
  for (i = 1; i < Ls; i++)
      q(put_cABTable)(params->AxmTable, i, w_a(Ls-1-i).re, w_a(Ls-1-i).im, u_a(Ls-i).re, u_a(Ls-i).im);

  /* B* tables */
  params->BxpTable = q(malloc)(state, abs);
  CHECK(params->BxpTable, "set_complex(): Not enough memory for B*/+ table");
  q(put_cABTable)(params->BxpTable, 0, w_b(0).re, w_b(0).im, v_b(Ls-1).re, v_b(Ls-1).im);
  for (i = 1; i < Ls; i++)
      q(put_cABTable)(params->BxpTable, i, w_b(i).re, w_b(i).im, u_b(i-1).re, u_b(i-1).im);
  params->BxmTable = q(malloc)(state, abs);
  CHECK(params->BxmTable, "set_complex(): Not enough memory for B*/- table");
  q(put_cABTable)(params->BxmTable, 0, w_b(Ls-1).re, w_b(Ls-1).im, v_b(0).re, v_b(0).im);
  for (i = 1; i < Ls; i++)
      q(put_cABTable)(params->BxmTable, i, w_b(Ls-1-i).re, w_b(Ls-1-i).im, u_b(Ls-i).re, u_b(Ls-i).im);

  /* sizeof 1/A tables */
  iabs = q(sizeof_cABiTable)(Ls);
  /* 1/A tables */
  params->AipTable = q(malloc)(state, iabs);
  CHECK(params->AipTable, "set_complex(): Not enough memory for 1/A + table");
  {
    cx ak = c_neg(c_div(v_a(0), w_a(Ls-1)));
    for (i = Ls; --i;) {
      cx bk = c_neg(c_div(u_a(i), w_a(i)));
      cx ck = c_inv(w_a(i));
      q(put_cABiTable)(params->AipTable, i, ak.re,ak.im, bk.re,bk.im, ck.re,ck.im);
      ak = c_neg(c_div(c_mul(ak,u_a(i)), w_a(i-1)));
    }
    {
      cx wa = c_inv(c_mul(w_a(0),rc_sub(1.0,ak)));
      q(put_cABiTableZ)(params->AipTable, wa.re, wa.im);
    }
  }
  params->AimTable = q(malloc)(state, iabs);
  CHECK(params->AimTable, "set_complex(): Not enough memory for 1/A - table");
  {
    cx ak = c_neg(c_div(v_a(Ls-1), w_a(0)));
    for (i = 0; i < Ls - 1; i++) {
      cx bk = c_neg(c_div(u_a(i), w_a(i)));
      cx ck = c_inv(w_a(i));
      q(put_cABiTable)(params->AimTable, i + 1, ak.re,ak.im, bk.re,bk.im, ck.re,ck.im);
      ak = c_neg(c_div(c_mul(ak,u_a(i)),w_a(i+1)));
    }
    {
      cx wa = c_inv(c_mul(w_a(Ls-1),rc_sub(1.0,ak)));
      q(put_cABiTableZ)(params->AimTable, wa.re,wa.im);
    }
  }

  /* 1/B tables */
  params->BipTable = q(malloc)(state, iabs);
  CHECK(params->BipTable, "set_complex(): Not enough memory for 1/B + table");
  {
    cx ak = c_neg(c_div(v_b(0), w_b(Ls-1)));
    for (i = Ls; --i;) {
      cx bk = c_neg(c_div(u_b(i), w_b(i)));
      cx ck = c_inv(w_b(i));
      q(put_cABiTable)(params->BipTable, i, ak.re,ak.im, bk.re,bk.im, ck.re,ck.im);
      ak = c_neg(c_div(c_mul(ak, u_b(i)), w_b(i-1)));
    }
    {
      cx wa = c_inv(c_mul(w_b(0),rc_sub(1.0,ak)));
      q(put_cABiTableZ)(params->BipTable, wa.re, wa.im);
    }
  }
  params->BimTable = q(malloc)(state, iabs);
  CHECK(params->BimTable, "set_complex(): Not enough memory for 1/B - table");
  {
    cx ak = c_neg(c_div(v_b(Ls-1), w_b(0)));
    for (i = 0; i < Ls - 1; i++) {
      cx bk = c_neg(c_div(u_b(i), w_b(i)));
      cx ck = c_inv(w_b(i));
      q(put_cABiTable)(params->BimTable, i + 1, ak.re,ak.im, bk.re,bk.im, ck.re,ck.im);
      ak = c_neg(c_div(c_mul(ak, u_b(i)), w_b(i+1)));
    }
    {
      cx wa = c_inv(c_mul(w_b(Ls-1),rc_sub(1.0,ak)));
      q(put_cABiTableZ)(params->BimTable, wa.re,wa.im);
    }
  }

  /* 1/A+ tables */
  params->AxipTable = q(malloc)(state, iabs);
  CHECK(params->AxipTable, "set_complex(): Not enough memory for 1/A* + table");
  {
    cx ak = c_neg(c_div(v_a(0), w_a(0)));
    for (i = 1; i < Ls; i++) {
      cx bk = c_neg(c_div(u_a(i), w_a(i-1)));
      cx ck = c_inv(w_a(i-1));
      q(put_cABiTable)(params->AxipTable, i, ak.re,ak.im, bk.re,bk.im, ck.re,ck.im);
      ak = c_neg(c_div(c_mul(ak, u_a(i)), w_a(i)));
    }
    {
      cx wa = c_inv(c_mul(w_a(Ls-1),rc_sub(1.0,ak)));
      q(put_cABiTableZ)(params->AxipTable, wa.re,wa.im);
    }
  }
  params->AximTable = q(malloc)(state, iabs);
  CHECK(params->AximTable, "set_complex(): Not enough memory for 1/A* - table");
  {
    cx ak = c_neg(c_div(v_a(Ls-1), w_a(Ls-1)));
    for (i = Ls; --i;) {
      cx bk = c_neg(c_div(u_a(i-1), w_a(i)));
      cx ck = c_inv(w_a(i));
      q(put_cABiTable)(params->AximTable, i, ak.re,ak.im, bk.re,bk.im, ck.re,ck.im);
      ak = c_neg(c_div(c_mul(ak, u_a(i-1)), w_a(i-1)));
    }
    {
      cx wa = c_inv(c_mul(w_a(0), rc_sub(1.0,ak)));
      q(put_cABiTableZ)(params->AximTable, wa.re, wa.im);
    }
  }

  /* 1/B+ tables */
  params->BxipTable = q(malloc)(state, iabs);
  CHECK(params->BxipTable, "set_complex(): Not enough memory for 1/B* + table");
  {
    cx ak = c_neg(c_div(v_b(0), w_b(0)));
    for (i = 1; i < Ls; i++) {
      cx bk = c_neg(c_div(u_b(i), w_b(i-1)));
      cx ck = c_inv(w_b(i-1));
      q(put_cABiTable)(params->BxipTable, i, ak.re,ak.im, bk.re,bk.im, ck.re,ck.im);
      ak = c_neg(c_div(c_mul(ak,u_b(i)), w_b(i)));
    }
    {
      cx wa = c_inv(c_mul(w_b(Ls-1), rc_sub(1.0,ak)));
      q(put_cABiTableZ)(params->BxipTable, wa.re,wa.im);
    }
  }
  params->BximTable = q(malloc)(state, iabs);
  CHECK(params->BximTable, "set_complex(): Not enough memory for 1/B* - table");
  {
    cx ak = c_neg(c_div(v_b(Ls-1), w_b(Ls-1)));
    for (i = Ls; --i;) {
      cx bk = c_neg(c_div(u_b(i-1), w_b(i)));
      cx ck = c_inv(w_b(i));
      q(put_cABiTable)(params->BximTable, i, ak.re,ak.im, bk.re,bk.im, ck.re,ck.im);
      ak = c_neg(c_div(c_mul(ak, u_b(i-1)), w_b(i-1)));
    }
    {
      cx wa = c_inv(c_mul(w_b(0), rc_sub(1.0,ak)));
      q(put_cABiTableZ)(params->BximTable, wa.re,wa.im);
    }
  }
  
  /* K tables */
  ks = q(sizeof_cKTable)(Ls);
  params->KTable = q(malloc)(state, ks);
  CHECK(params->KTable, "set_complex(): Not enough memory for K table");
  for (i = 0; i < Ls; i++)
    q(put_cKTable)(params->KTable, i, kap(i).re, kap(i).im);
  /* 1/K tables */
  iks = q(sizeof_cKiTable)(Ls);
  params->KiTable = q(malloc)(state, iks);
  CHECK(params->KiTable, "set_complex(): Not enough memory for 1/K table");
  for (i = 0; i < Ls; i++)
    q(put_cKiTable)(params->KiTable, i, ikap(i).re, ikap(i).im);

  params->qf(op) = &qf(complex_ops);
  params->qd(op) = &qd(complex_ops);

  BEGIN_TIMING(state);
  params->state = state;
  *params_ptr = params;
  END_TIMING(state, 0, 0, 0);

  return 0;
#undef u_a_re
#undef u_a_im
#undef v_a_re
#undef v_a_im
#undef w_a_re
#undef w_a_im
#undef u_b_re
#undef u_b_im
#undef v_b_re
#undef v_b_im
#undef w_b_re
#undef w_b_im
#undef CHECK
 error:
  Q(free_parameters)(&params);
  return q(set_error)(state, 0, msg);
}
