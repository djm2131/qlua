#include <test_common.h>
#include <math.h>

QDP_RandomState *rs;

static void
lex_int(QLA_Int *li, int coords[])
{
  int i,t;

  t = coords[0];
  for(i=1; i<QDP_ndim(); i++) {
    t = t*QDP_coord_size(i) + coords[i];
  }
  *li = t;
}

void
seed_rand(QDP_RandomState *rs, int seed)
{
  QDP_Int *li;

  li = QDP_create_I();

  QDP_I_eq_func(li, lex_int, QDP_all);
  QDP_S_eq_seed_i_I(rs, seed, li, QDP_all);

  QDP_destroy_I(li);
}

void
print_layout(void)
{
  int i, ndim, *latsize;

  ndim = QDP_ndim();
  latsize = (int *)malloc(ndim*sizeof(int));
  QDP_latsize(latsize);

  printf("nodes = %i\n", QMP_get_number_of_nodes());
  printf("size = %i", latsize[0]);
  for(i=1; i<ndim; i++) {
    printf(" %i", latsize[i]);
  }
  printf("\n");
  if(QMP_logical_topology_is_declared()) {
    int nd;
    const int *ld;
    nd = QMP_get_logical_number_of_dimensions();
    ld = QMP_get_logical_dimensions();
    printf("machine size = %i", ld[0]);
    for(i=1; i<nd; i++) {
      printf(" %i", ld[i]);
    }
    printf("\n");
    printf("sublattice = %i", latsize[0]/ld[0]);
    for(i=1; i<nd; i++) {
      printf(" %i", latsize[i]/ld[i]);
    }
    printf("\n");
  }
  free(latsize);
}

static QLA_Complex
det(QLA_ColorMatrix *m)
{
  QLA_ColorMatrix tm;
  QLA_Complex z1;

  for(int j = 0; j < QDP_Nc; j++)
    for(int i = 0; i < QDP_Nc; i++)
      QLA_elem_M(tm,j,i) = QLA_elem_M(*m,j,i);

  for(int j = 0; j < QDP_Nc; j++) {
    for(int i = 0; i <= j; i++) {
      QLA_Complex t2;
      QLA_c_eq_c(t2, QLA_elem_M(tm,j,i));
      for(int c = 0; c < i; c++)
        QLA_c_meq_c_times_c(t2, QLA_elem_M(tm,c,i), QLA_elem_M(tm,j,c));

      QLA_c_eq_c(QLA_elem_M(tm,j,i), t2);
    }

    for(int i = (j+1); i < QDP_Nc; i++) {
      QLA_Complex t2;
      t2 = QLA_elem_M(tm,j,i);
      for(int c = 0; c < j; c++)
        QLA_c_meq_c_times_c(t2, QLA_elem_M(tm,c,i), QLA_elem_M(tm,j,c));

      QLA_c_eq_c_div_c(QLA_elem_M(tm,j,i), t2, QLA_elem_M(tm,j,j));
    }
  }

  /* The determinant */
  z1 = QLA_elem_M(tm,0,0);
  for(int c = 1; c < QDP_Nc; c++) {
    QLA_Complex z;
    QLA_c_eq_c_times_c(z, z1, QLA_elem_M(tm,c,c));
    z1 = z;
  }

  return z1;
}

static void
normalize(QLA_ColorMatrix *m, int r)
{
  //QLA_Real n, t;
  QLA_Real n = 0;
  for(int c=0; c<QDP_Nc; c++) {
    //QLA_R_eq_norm2_C(&t, &QLA_elem_M(*m, r, c));
    //n += t;
    n += QLA_norm2_c(QLA_elem_M(*m, r, c));
  }
  n = 1/sqrt(n);
  for(int c=0; c<QDP_Nc; c++) {
    QLA_c_eq_r_times_c(QLA_elem_M(*m,r,c), n, QLA_elem_M(*m,r,c));
  }
}

static void
orthogonalize(QLA_ColorMatrix *m, int r1, int r2)
{
  QLA_Complex z;
  QLA_c_eq_r(z, 0);
  for(int c=0; c<QLA_Nc; c++) {
    QLA_Complex t;
    QLA_c_eq_ca_times_c(t, QLA_elem_M(*m,r1,c), QLA_elem_M(*m,r2,c));
    QLA_c_peq_c(z, t);
  }
  for(int c=0; c<QLA_Nc; c++) {
    QLA_c_meq_c_times_c(QLA_elem_M(*m,r2,c), z, QLA_elem_M(*m,r1,c));
  }
}

static void
make_unitary_func(NCPROT1 QLA_ColorMatrix *m, int coords[])
{
  for(int i=0; i<QDP_Nc; i++) {
    for(int j=0; j<i; j++) {
      orthogonalize(m, j, i);
    }
    normalize(m, i);
  }

  QLA_Complex z1, z2;
  z1 = det(m);

  QLA_Real r = QLA_norm_c(z1);
  QLA_c_eq_ca(z2, z1);
  QLA_c_eq_c_div_r(z1, z2, r);
  for(int c = 0; c < QDP_Nc; ++c) {
    QLA_Complex z;
    QLA_c_eq_c_times_c(z, QLA_elem_M(*m,c,QDP_Nc-1), z1);
    QLA_elem_M(*m,c,QDP_Nc-1) = z;
  }
  //z1 = det(m);
  //printf("%g\t%g\n", QLA_real(z1), QLA_imag(z1));
}

void
make_unitary(QDP_ColorMatrix **m, int n)
{
  for(int i=0; i<n; i++) {
    QDP_M_eq_func(m[i], make_unitary_func, QDP_all);
  }
}

QLA_Real
get_plaq(QDP_ColorMatrix *link[])
{
  int mu, nu;
  QLA_Real plaq;
  QDP_ColorMatrix *temp1, *temp2, *temp3, *temp4;

#ifdef LOCAL_SUM
  QDP_Real *treal1, *treal2;
  treal1 = QDP_create_R();
  treal2 = QDP_create_R();
  QDP_R_eq_zero(treal2, QDP_all);
#else
  QLA_Real tplaq;
  plaq = 0;
#endif

  temp1 = QDP_create_M();
  temp2 = QDP_create_M();
  temp3 = QDP_create_M();
  temp4 = QDP_create_M();

  for(mu=0; mu<QDP_ndim()-1; ++mu) {
    for(nu=mu+1; nu<QDP_ndim(); ++nu) {

      QDP_M_eq_sM(temp1, link[nu], QDP_neighbor[mu], QDP_forward, QDP_all);
      QDP_M_eq_sM(temp2, link[mu], QDP_neighbor[nu], QDP_forward, QDP_all);

      QDP_M_eq_Ma_times_M(temp3, link[nu], link[mu], QDP_all);

      QDP_M_eq_M_times_M(temp4, temp3, temp1, QDP_all);
      QDP_discard_M(temp1);

#ifdef LOCAL_SUM
      QDP_R_eq_re_M_dot_M(treal1, temp2, temp4, QDP_all);
      QDP_discard_M(temp2);
      QDP_R_peq_R(treal2, treal1, QDP_all);
#else
      QDP_r_eq_re_M_dot_M(&tplaq, temp2, temp4, QDP_all);
      QDP_discard_M(temp2);
      plaq += tplaq;
#endif

    }
  }

#ifdef LOCAL_SUM
  QDP_r_eq_sum_R(&plaq, treal2, QDP_all);
  QDP_destroy_R(treal1);
  QDP_destroy_R(treal2);
#endif

  QDP_destroy_M(temp1);
  QDP_destroy_M(temp2);
  QDP_destroy_M(temp3);
  QDP_destroy_M(temp4);

  return plaq/(0.5*QDP_ndim()*(QDP_ndim()-1)*QDP_volume()*QLA_Nc);
}

void
get_random_links(QDP_ColorMatrix **u, int n, QLA_Real r)
{
  int i;
  QDP_ColorMatrix *cm = QDP_create_M();

  for(i=0; i<n; i++) {
    QLA_Complex z;
    QLA_c_eq_r(z, 1);
    QDP_M_eq_c(u[i], &z, QDP_all);
    QDP_M_eq_gaussian_S(cm, rs, QDP_all);
    QDP_M_peq_r_times_M(u[i], &r, cm, QDP_all);
  }
  QDP_destroy_M(cm);
  make_unitary(u, n);
}

void
get_latsize(int *ndim, int **ls, char *fn)
{
  QIO_Layout ql;
  QIO_Reader *qr;
  QIO_String *qs;
  int i, *rls;

  ql.latdim = 0;
  ql.latsize = NULL;
  ql.this_node = QMP_get_node_number();
  ql.number_of_nodes = QMP_get_number_of_nodes();

  qs = QIO_string_create();

  qr = QIO_open_read(qs, fn, &ql, NULL, NULL);

  *ndim = QIO_get_reader_latdim(qr);
  printf0("lattice ndim = %i\n", *ndim);
  rls = QIO_get_reader_latsize(qr);
  *ls = (int *) malloc(*ndim*sizeof(int));
  printf0("lattice size =");
  for(i=0; i<*ndim; i++) {
    (*ls)[i] = rls[i];
    printf0(" %i", (*ls)[i]);
  }
  printf0("\n");
}

void
load_lattice(QDP_ColorMatrix *gauge[], char *fn)
{
  QDP_Reader *qr;
  QDP_String *md;
  int ndim = QDP_ndim();

  printf0("loading lattice file %s\n", fn);

  md = QDP_string_create();
  qr = QDP_open_read(md, fn);

#if QDP_Precision == 'F'
  QDP_F_vread_M(qr, md, gauge, ndim);
#else
  {
    int i;
    QDP_F_ColorMatrix *tm[ndim];
    for(i=0; i<ndim; i++) {
      tm[i] = QDP_F_create_M();
    }
    QDP_F_vread_M(qr, md, tm, ndim);
    for(i=0; i<ndim; i++) {
      QDP_DF_M_eq_M(gauge[i], tm[i], QDP_all);
      QDP_F_destroy_M(tm[i]);
    }
  }
#endif

  QDP_close_read(qr);
  QDP_string_destroy(md);
}

void
load_fermion(QDP_DiracFermion *df, char *fn)
{
  QDP_Reader *qr;
  QDP_String *md;

  printf0("loading fermion file %s\n", fn);

  md = QDP_string_create();
  qr = QDP_open_read(md, fn);

#if QDP_Precision == 'F'
  QDP_F_read_D(qr, md, df);
#else
  {
    QDP_F_DiracFermion *tdf;
    tdf = QDP_F_create_D();
    QDP_F_read_D(qr, md, tdf);
    QDP_DF_D_eq_D(df, tdf, QDP_all);
    QDP_F_destroy_D(tdf);
  }
#endif

  QDP_close_read(qr);
  QDP_string_destroy(md);
}

void
point_source_V(QDP_ColorVector *v, int *x, int c)
{
  QLA_ColorVector(*q);
  int n, i;
  QDP_V_eq_zero(v, QDP_all);
  q = QDP_expose_V(v);
  n = QDP_node_number(x);
  i = QDP_index(x);
  if(n==QDP_this_node) {
    QLA_c_eq_r(QLA_elem_V(q[i], c), 1);
  }
  QDP_reset_V(v);
}
