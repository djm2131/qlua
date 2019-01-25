#include <test_common.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <math.h>

static int ndim=4;
static int *lattice_size;
static int seed;
static int nit=10;
static QLA_Real mass=-1;
static int style=-1;
static int nmass=1;
static int verb=0;
static int cgtype=1;
static int eigcg_l=-1;
static int eigcg_m=-1;
static int eigcg_nev=-1;
static QDP_ColorMatrix *u[4];
static double u0=-1;
static int restart=1500;
static int nrestart=10;
static double rsqmin=1e-6;

QOP_FermionLinksAsqtad *fla;

void
setopt(char *tag, int val)
{
  QOP_opt_t opt;
  opt.tag = tag;
  opt.value = (double) val;
  QOP_asqtad_invert_set_opts(&opt, 1);
}

double
bench_inv(QOP_info_t *info, QOP_invert_arg_t *inv_arg,
	  QOP_resid_arg_t *res_arg, QDP_ColorVector *out, QDP_ColorVector *in)
{
  double sec=0, flop=0, mf=0;
  int i, j, iter=0;
  QOP_ColorVector *qopout[nmass], **pqo, *qopin;
  QLA_Real masses[nmass], *pm;
  QOP_resid_arg_t *ra[nmass], **pra;

  pqo = qopout;
  pm = masses;
  pra = ra;
  for(i=0; i<=0; i++) {
    QDP_V_eq_zero(out, QDP_all);
    qopin = QOP_create_V_from_qdp(in);
    for(j=0; j<nmass; j++) {
      qopout[j] = QOP_create_V_from_qdp(out);
      masses[j] = mass*(j+1);
      ra[j] = res_arg;
    }
    QMP_barrier();
    if(nmass == 1) {
      QOP_asqtad_invert(info, fla, inv_arg, res_arg, mass, qopout[0], qopin);
    } else {
      QOP_asqtad_invert_multi(info, fla, inv_arg, &pra, &pm, &nmass, &pqo, &qopin, 1);
    }
    QMP_barrier();
    if(res_arg->final_rsq>res_arg->rsqmin) {
      printf0("CG FAILED: %i %g %g\n", res_arg->final_iter, res_arg->final_rsq, res_arg->rsqmin);
    }
    if(i>=0) {
      iter += res_arg->final_iter;
      sec += info->final_sec;
      flop += info->final_flop;
      //mf += info->final_flop/(1e6*info->final_sec);
    }
    for(j=0; j<nmass; j++) {
      QOP_destroy_V(qopout[j]);
    }
    QOP_destroy_V(qopin);
  }
  mf = 1;
  QMP_sum_double(&mf);
  QMP_sum_double(&sec);
  QMP_sum_double(&flop);
  res_arg->final_iter = iter/1;
  info->final_sec = sec/(mf*1);
  info->final_flop = flop/(mf*1);
  mf = nit*info->final_flop/(1e6*info->final_sec);
  return mf;
}

void
start(void)
{
  double mf;
  int i;

  QDP_ColorMatrix *fatlinks[4], *longlinks[4];
  QDP_ColorVector *out, *in;
  out = QDP_create_V();
  in = QDP_create_V();
  for(i=0; i<4; i++) {
    fatlinks[i] = QDP_create_M();
    QDP_M_eq_M(fatlinks[i], u[i], QDP_all);
    longlinks[i] = QDP_create_M();
    QDP_M_eq_M(longlinks[i], u[i], QDP_all);
  }

  QOP_layout_t qoplayout = QOP_LAYOUT_ZERO;
  qoplayout.latdim = ndim;
  qoplayout.latsize = (int *) malloc(ndim*sizeof(int));
  for(i=0; i<ndim; i++) {
    qoplayout.latsize[i] = lattice_size[i];
  }
  qoplayout.machdim = -1;

  QOP_GaugeField *gf;
  QOP_asqtad_coeffs_t coeffs = QOP_ASQTAD_COEFFS_ZERO;
  {
    double u2, u4;
    u2 = 1.0/(u0*u0);
    u4 = u2*u2;
    coeffs.one_link = 5.0/8.0;
    coeffs.three_staple = -u2/16.0;
    coeffs.five_staple = u4/64.0;
    coeffs.seven_staple = -(u4*u2)/384.0;
    coeffs.lepage = -u4/16.0;
    coeffs.naik = -u2/24.0;
#define cp(x) printf0(#x " = %g\n", coeffs.x);
    cp(one_link);
    cp(three_staple);
    cp(five_staple);
    cp(seven_staple);
    cp(lepage);
    cp(naik);
#undef cp
  }

  QOP_info_t info = QOP_INFO_ZERO;
  QOP_invert_arg_t inv_arg = QOP_INVERT_ARG_DEFAULT;
  QOP_resid_arg_t res_arg = QOP_RESID_ARG_DEFAULT;
  res_arg.rsqmin = rsqmin;
  inv_arg.max_iter = nrestart*restart;
  inv_arg.restart = restart;
  inv_arg.max_restarts = nrestart;
  inv_arg.evenodd = QOP_EVEN;

  if(QDP_this_node==0) { printf("begin init\n"); fflush(stdout); }
  QOP_init(&qoplayout);
  QOP_verbose(verb);
  if(QDP_this_node==0) { printf("convert gauge field\n"); fflush(stdout); }
  gf = QOP_convert_G_from_qdp(u);
  if(QDP_this_node==0) { printf("begin load links\n"); fflush(stdout); }
  fla = QOP_asqtad_create_L_from_G(&info, &coeffs, gf);
  if(QDP_this_node==0) { printf("begin invert\n"); fflush(stdout); }

  setopt("cg", cgtype);
  if(style>=0) setopt("st", style);
  if(eigcg_l>=0) setopt("eigcg_l", eigcg_l);
  if(eigcg_m>=0) setopt("eigcg_m", eigcg_m);
  if(eigcg_nev>=0) setopt("eigcg_nev", eigcg_nev);
  //QDP_set_block_size(bs);

  for(i=0; i<nit; i++) {
#if 1
    QDP_V_eq_gaussian_S(in, rs, QDP_all);
#else
    int x[4], c, k;
    k = i;
    c = k%3; k /= 3;
    x[0] = k%QDP_coord_size(0); k /= QDP_coord_size(0);
    x[1] = k%QDP_coord_size(1); k /= QDP_coord_size(1);
    x[2] = k%QDP_coord_size(2); k /= QDP_coord_size(2);
    x[3] = k%QDP_coord_size(3); k /= QDP_coord_size(3);
    if( ((x[0]+x[1]+x[2]+x[3])&1) != 0 ) x[3]++;
    point_source_V(in, x, c);
#endif

    QOP_verbose(0);
    setopt("cg", 0);
    mf = bench_inv(&info, &inv_arg, &res_arg, out, in);
    printf0("CG: %i st%2i iter%5i sec%7.4f mflops = %g\n",
	    i, style, res_arg.final_iter, info.final_sec, mf);
    setopt("cg", cgtype);
    QOP_verbose(verb);

    mf = bench_inv(&info, &inv_arg, &res_arg, out, in);
    printf0("eigCG1: %i st%2i iter%5i sec%7.4f mflops = %g\n",
	    i, style, res_arg.final_iter, info.final_sec, mf);
    //mf = bench_inv(&info, &inv_arg, &res_arg, out, in);
    //printf0("eigCG2: %i st%2i iter%5i sec%7.4f mflops = %g\n",
    //i, style, res_arg.final_iter, info.final_sec, mf);
  }

  if(QDP_this_node==0) { printf("begin unload links\n"); fflush(stdout); }
  //QOP_asqtad_invert_unload_links();
  if(QDP_this_node==0) { printf("begin finalize\n"); fflush(stdout); }
  //QOP_asqtad_invert_finalize();
}

void
usage(char *s)
{
  printf("%s [l#] [m#] [n#] [s#] [S#] [x# [# ...]]\n",s);
  printf("\n");
  printf("c\tCG type\n");
  printf("l\tlattice file name\n");
  printf("m\tmass\n");
  printf("n\tnumber of iterations\n");
  printf("r\trestart\n");
  printf("R\tnrestart\n");
  printf("q\trsqmin\n");
  printf("s\tseed\n");
  printf("S\tstyle\n");
  printf("x\tlattice sizes (Lx, [Ly], ..)\n");
  printf("\n");
  exit(1);
}

int
main(int argc, char *argv[])
{
  int i, j=0, prof=0;
  char *lattice_fn=NULL;

  QDP_initialize(&argc, &argv);
  QIO_verbose(0);
  QDP_profcontrol(0);
  QOP_verbose(0);

  seed = time(NULL);
  for(i=1; i<argc; i++) {
    char *argp = &argv[i][1];
    if(argv[i][1]=='\0') if(i+1<argc) argp = argv[i+1];
    switch(argv[i][0]) {
    case 'c' : cgtype = atoi(argp); break;
    case 'l' : lattice_fn = argp; break;
    case 'm' : mass=atof(argp); break;
    case 'n' : nit=atoi(argp); break;
    case 'p' : prof = atoi(argp); break;
    case 'r' : restart=atoi(argp); break;
    case 'R' : nrestart=atoi(argp); break;
    case 'q' : rsqmin=atof(argp); break;
    case 's' : seed=atoi(argp); break;
    case 'S' : style=atoi(argp); break;
    case 'u' : u0=atof(argp); break;
    case 'x' : j=i; while((i+1<argc)&&(isdigit(argv[i+1][0]))) ++i; break;
    case 'L' : eigcg_l = atoi(argp); break;
    case 'M' : eigcg_m = atoi(argp); break;
    case 'N' : eigcg_nev = atoi(argp); break;
    case 'v' : verb=atoi(argp); break;
    default : usage(argv[0]);
    }
    if(argv[i][1]=='\0') i++;
  }
  QDP_profcontrol(prof);

  if(lattice_fn==NULL) {
    lattice_size = (int *) malloc(ndim*sizeof(int));
    if(j==0) {
      for(i=0; i<ndim; ++i) lattice_size[i] = 8;
    } else {
      if(!isdigit(argv[j][1])) usage(argv[0]);
      lattice_size[0] = atoi(&argv[j][1]);
      for(i=1; i<ndim; ++i) {
	if((++j<argc)&&(isdigit(argv[j][0]))) {
	  lattice_size[i] = atoi(&argv[j][0]);
	} else {
	  lattice_size[i] = lattice_size[i-1];
	}
      }
    }
  } else {
    get_latsize(&ndim, &lattice_size, lattice_fn);
  }
  QDP_set_latsize(ndim, lattice_size);
  QDP_create_layout();

  if(mass<0) {
    mass = 0.2 * pow(QDP_volume(),0.1);
  }

  if(QDP_this_node==0) {
    print_layout();
    printf("mass = %g\n", mass);
    printf("seed = %i\n", seed);
    if(lattice_fn) printf("lattice file = %s\n", lattice_fn);
    //printf("u0 = %g\n", u0);
    printf("restart = %i\n", restart);
    printf("nrestarts = %i\n", nrestart);
    printf("rsqmin = %g\n", rsqmin);
    printf("eigcg_l = %i\n", eigcg_l);
    printf("eigcg_m = %i\n", eigcg_m);
    printf("eigcg_nev = %i\n", eigcg_nev);
  }

  rs = QDP_create_S();
  QMP_broadcast(&seed, sizeof(seed));
  seed_rand(rs, seed);

  for(i=0; i<ndim; i++) u[i] = QDP_create_M();
  if(lattice_fn==NULL) {
    get_random_links(u, ndim, 0.3);
  } else {
    load_lattice(u, lattice_fn);
  }

  {
    double plaq = get_plaq(u);
    if(u0<0) u0 = pow(plaq/3,0.25);
    if(QDP_this_node==0) {
      printf("plaquette = %g\n", plaq);
      printf("u0 = %g\n", u0);
    }
  }

  start();

  QDP_finalize();
  return 0;
}
