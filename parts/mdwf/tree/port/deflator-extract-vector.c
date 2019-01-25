#define QOP_MDWF_DEFAULT_PRECISION 'F'
#include <mdwf.h>

int
QX(deflator_extract_vector)(struct QX(HalfFermion) *hf,
                            const struct QX(Deflator) *d,
                            int idx)
{
  DECLARE_STATE;
  qx(defl_vec) vf;

  CHECK_ARG0(hf);
  CHECK_ARGn(d, "deflator_extract_vector");

  if (idx < 0 || idx >= d->usize)
    return q(set_error)(state, 0, "deflator_extract_vector(): index out of range");

  vf = qx(defl_vec_view)(state, hf->cb_x);
  qx(defl_mat_get_col)(state, d->U, idx, vf);

  return 0;
}
