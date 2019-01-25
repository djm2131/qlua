#include <stdio.h>
#include "qdp_config.h"
#include <qdp.h>

#define printf0 if(QDP_this_node==0) printf

void
initial_gauge(QLA_ColorMatrix *g, int coords[])
{
  QLA_Complex z;
  int i;

  QLA_M_eq_zero(g);
  for(i=0; i<QLA_Nc; i++) {
    QLA_c_eq_r_plus_ir(z, i, 0);
    QLA_elem_M(*g,i,i) = z;
  }
}

void
initial_df(QLA_DiracFermion *df, int coords[])
{
  QLA_Complex z;
  int i,j;

  for(i=0; i<QLA_Nc; i++) {
    for(j=0; j<4; j++) {
      QLA_c_eq_r_plus_ir(z, i+j, coords[0]);
      QLA_elem_D(*df,i,j) = z;
    }
  }
}

void
initial_li(QLA_Int *li, int coords[])
{
  int i,t;

  t = coords[3];
  for(i=2; i>=0; --i) {
    t = t*4 + coords[i];
  }
  *li = t;
}

void
print_df(QLA_DiracFermion *df, int coords[])
{
  QLA_Complex z;
  int i,j;

  for(i=0; i<4; i++) if(coords[i]) return;
  for(i=0; i<QLA_Nc; i++) {
    for(j=0; j<4; j++) {
      z = QLA_elem_D(*df,i,j);
      printf("%g\t%g\n", QLA_real(z), QLA_imag(z));
    }
  }
}

int
timeslices(int x[])
{
  return x[3];
}

int
main(int argc, char *argv[])
{
  int lattice_size[4] = { 8,8,8,8 };
  char fn[]="qdpio-test.bin";
  QDP_DiracFermion *d1out, *d2out, *d1in, *d2in;
  QDP_ColorMatrix *mout[2], *min[2];
  QDP_RandomState *rs;
  QDP_Int *li;
  QDP_Reader *qr;
  QDP_Writer *qw;
  QDP_String *md;
  QLA_Real x, y;

  QDP_initialize(&argc, &argv);
  QDP_set_latsize(4, lattice_size);
  QDP_create_layout();

  if(argc>1) {
    QIO_verbose(QIO_VERB_DEBUG);
  }

  d1out = QDP_create_D();
  d2out = QDP_create_D();
  d1in = QDP_create_D();
  d2in = QDP_create_D();
  mout[0] = QDP_create_M();
  mout[1] = QDP_create_M();
  min[0] = QDP_create_M();
  min[1] = QDP_create_M();
  rs = QDP_create_S();
  li = QDP_create_I();

  QDP_I_eq_func(li, initial_li, QDP_all);
  QDP_S_eq_seed_i_I(rs, 987654321, li, QDP_all);

  printf0("creating string\n");
  md = QDP_string_create();
  printf0("setting string\n");
  QDP_string_set(md, "test");
  printf0("open write\n");
  qw = QDP_open_write(md, fn, QDP_SINGLEFILE);
  printf0("done open write\n");

  QDP_D_eq_gaussian_S(d1out, rs, QDP_all);
  QDP_r_eq_norm2_D(&x, d1out, QDP_all);
  printf0("writing field, norm = %g\n", x);
  QDP_write_D(qw, md, d1out);

  QDP_D_eq_gaussian_S(d2out, rs, QDP_all);
  QDP_r_eq_norm2_D(&x, d2out, QDP_all);
  printf0("writing field, norm = %g\n", x);
  QDP_write_D(qw, md, d2out);

  QDP_M_eq_gaussian_S(mout[0], rs, QDP_all);
  QDP_M_eq_gaussian_S(mout[1], rs, QDP_all);
  QDP_r_eq_norm2_M(&x, mout[0], QDP_all);
  printf0("writing mout[0], norm = %g\n", x);
  QDP_r_eq_norm2_M(&x, mout[1], QDP_all);
  printf0("writing mout[1], norm = %g\n", x);
  QDP_vwrite_M(qw, md, mout, 2);

  QDP_close_write(qw);

  QDP_set_read_group_size(2);
  qr = QDP_open_read(md, fn);
  printf0("file metadata: %s\n", QDP_string_ptr(md));

  QDP_read_D(qr, md, d1in);
  printf0("record metadata: %s\n", QDP_string_ptr(md));
  QDP_r_eq_norm2_D(&x, d1in, QDP_all);
  QDP_D_meq_D(d1out, d1in, QDP_all);
  QDP_r_eq_norm2_D(&y, d1out, QDP_all);
  printf0("read field,   norm = %g  error = %g\n", x, y);

  QDP_read_D(qr, md, d2in);
  printf0("record metadata: %s\n", QDP_string_ptr(md));
  QDP_r_eq_norm2_D(&x, d2in, QDP_all);
  QDP_D_meq_D(d2out, d2in, QDP_all);
  QDP_r_eq_norm2_D(&y, d2out, QDP_all);
  printf0("read field,   norm = %g  error = %g\n", x, y);

  QDP_vread_M(qr, md, min, 2);
  printf0("record metadata: %s\n", QDP_string_ptr(md));
  QDP_r_eq_norm2_M(&x, min[0], QDP_all);
  QDP_M_meq_M(mout[0], min[0], QDP_all);
  QDP_r_eq_norm2_M(&y, mout[0], QDP_all);
  printf0("read mout[0],   norm = %g  error = %g\n", x, y);
  QDP_r_eq_norm2_M(&x, min[1], QDP_all);
  QDP_M_meq_M(mout[1], min[1], QDP_all);
  QDP_r_eq_norm2_M(&y, mout[1], QDP_all);
  printf0("read mout[1],   norm = %g  error = %g\n", x, y);

  QDP_close_read(qr);

  QDP_string_destroy(md);
  QDP_destroy_D(d1out);
  QDP_destroy_D(d1in);
  QDP_destroy_D(d2out);
  QDP_destroy_D(d2in);
  QDP_destroy_S(rs);
  QDP_destroy_I(li);

  QDP_finalize();
  return 0;
}
