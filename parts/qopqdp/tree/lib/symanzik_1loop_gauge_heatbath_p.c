//#define DO_TRACE
#include <qop_internal.h>
#include <math.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#define WENSLEY_CONST 1.05110196582237

#if QLA_Precision == 'F'
#define QLAP(x) QLA_F_ ## x
#else
#define QLAP(x) QLA_D_ ## x
#endif

static QLA_RandomState *rs;
static QLA_Real fac;

static void
su2_extract(NCPROT QLA_Real r[4], QLA_ColorMatrix(*m), int i, int j)
{
  QLA_Complex *a00, *a01, *a10, *a11;
  a00 = &QLA_elem_M(*m, i, i);
  a01 = &QLA_elem_M(*m, i, j);
  a10 = &QLA_elem_M(*m, j, i);
  a11 = &QLA_elem_M(*m, j, j);
  r[0] = QLA_real(*a00) + QLA_real(*a11);
  r[1] = QLA_imag(*a01) + QLA_imag(*a10);
  r[2] = QLA_real(*a01) - QLA_real(*a10);
  r[3] = QLA_imag(*a00) - QLA_imag(*a11);
}

static void
su2_fill(NCPROT QLA_ColorMatrix(*m), QLA_Real r[4], int i, int j)
{
  QLA_Complex z;
  QLA_c_eq_r(z, 1);
  QLA_M_eq_c(m, &z);

  QLA_c_eq_r_plus_ir(z, r[0], r[3]);
  QLA_M_eq_elem_C(m, &z, i, i);

  QLA_c_eq_r_plus_ir(z, r[2], r[1]);
  QLA_M_eq_elem_C(m, &z, i, j);

  r[2] = -r[2];
  QLA_c_eq_r_plus_ir(z, r[2], r[1]);
  QLA_M_eq_elem_C(m, &z, j, i);

  r[3] = -r[3];
  QLA_c_eq_r_plus_ir(z, r[0], r[3]);
  QLA_M_eq_elem_C(m, &z, j, j);
}

static void
get_hb2(QLA_Real *b, QLA_Real al, QLA_RandomState *srs)
{
  QLA_Real d, r, r2, rho, xr;

  if(al <= 2.0) {  /* creutz algorithm */
    QLA_Real xl, xd, a0;
    xl = exp(-2.0*al);
    xd = 1.0 - xl;
    for(int k=0; k<20; k++) {
      QLA_Real xr1, xr2, s;
      QLA_R_eq_random_S(&xr1, srs);
      QLA_R_eq_random_S(&xr2, srs);
      s = xl + xd*xr1;
      a0 = 1.0 + log(s)/al;
      if((1.0-a0*a0) > xr2*xr2) break;
    }
    d = 1.0 - a0;
  } else {  /* k-p algorithm */
    for(int k=0; k<20; k++) {
      QLA_Real xr1, xr2, xr3, xr4;
      QLA_R_eq_random_S(&xr1, srs);
      xr1 = log(xr1 + 1.e-10);
      QLA_R_eq_random_S(&xr2, srs);
      xr2 = log(xr2 + 1.e-10);
      QLA_R_eq_random_S(&xr3, srs);
      QLA_R_eq_random_S(&xr4, srs);
      xr3 = cos(2.0*M_PI*xr3);
      d = -(xr2 + xr1*xr3*xr3)/al;
      if((1.0 - 0.5*d) > xr4*xr4) break;
    }
  }

  b[0] = 1.0 - d;
  r2 = fabs(1.0 - b[0]*b[0]);
  r = sqrt(r2);

  QLA_R_eq_random_S(&xr, srs);
  b[3] = (2.0*xr - 1.0)*r;

  rho = sqrt(fabs(r2 - b[3]*b[3]));

  QLA_R_eq_random_S(&xr, srs);
  xr *= 2.0*M_PI;
  b[1] = rho*cos(xr);
  b[2] = rho*sin(xr);
}

// Wensley heatbath
static void
get_hb1(QLA_Real *theta, QLA_Real g, QLA_RandomState *srs)
{
  QLA_Real xr, f, norm;

  if( 0==g ) { // uniform distribution
    QLA_R_eq_random_S(&xr, srs);
    *theta = 2*xr - 1.0;
    *theta *= M_PI;
  }
  else if( g<0.01 ) { // simple accept/reject
    norm = exp( g );
    do {
      QLA_R_eq_random_S(&xr, srs);
      *theta = 2*xr - 1.0;
      *theta *= M_PI;
      f = exp( g * cos( *theta ) ); 
      QLA_R_eq_random_S(&xr, srs);
    } while( f < xr*norm );
  }
  else { // Wensley linear filter
    norm = exp( g*WENSLEY_CONST );
    do {
      QLA_R_eq_random_S(&xr, srs);
      if( xr<0.5 ) {
        *theta = log( 1 + 2*( exp( g ) - 1 )*xr ) / g - 1;
      }
      else {
        *theta = 1 - log( 1 + 2*( exp( g ) - 1 )*( 1 - xr ) ) / g;
      }
      *theta *= M_PI;
      f = exp( g*( cos( *theta ) + fabs( *theta )/M_PI ) ) / norm;
      QLA_R_eq_random_S(&xr, srs);
    } while( f < xr );
  }
}

static void
hb_func(NCPROT1 QLA_ColorMatrix(*m), int site)
{
  QLA_RandomState *srs = rs + site;
  if(QLA_Nc==1) { // exp(-fac*Re[u*z]) = exp(-fac*|z|*cos(t))
    // call Wensley heatbath
    QLA_Complex cc;
    QLA_Real r, phi, g, theta;

    // *m contains r*exp(i*phi), extract r and phi
    // extract QLA matrix element as complex number
    QLA_c_eq_c(cc, QLA_elem_M(*m,0,0));
    // get norm and arg
    QLA_R_eq_norm_C( &r, &cc );
    QLA_R_eq_arg_C( &phi, &cc );
    g = fac*r;

    // generate theta with probability P(theta)=exp( g*cos(theta) )
    get_hb1( &theta, g, srs );

    // convert to real and imag
    //QLA_Real vr = cos( theta - phi );
    //QLA_Real vi = sin( theta - phi );
    // assemble QLA complex number and set QLA U(1) matrix to this
    //QLA_c_eq_r_plus_i_r( QLA_elem_M(*m,0,0), vr, vi );
    QLA_elem_M(*m,0,0) = QLAP(cexpi)(theta - phi);
  } else {
    QLA_ColorMatrix(s);
    QLA_ColorMatrix(t);
    QLA_ColorMatrix(tt);
    QLA_Complex one;
    QLA_c_eq_r(one, 1);
    QLA_M_eq_c(&s, &one);

    /* Loop over SU(2) subgroup index */
    for(int i=0; i<QLA_Nc; i++) {
      for(int j=i+1; j<QLA_Nc; j++) {
	QLA_Real a[4], b[4], r[4], rn, rl;

	su2_extract(NCARG r, m, i, j);
	rn = sqrt( r[0]*r[0] + r[1]*r[1] + r[2]*r[2] + r[3]*r[3] );
	rl = fac*rn;
	if(rn<1e-10) {
	  a[0] = 1; a[1] = a[2] = a[3] = 0;
	} else {
	  rn = 1/rn;
	  a[0] =  rn*r[0];
	  a[1] = -rn*r[1];
	  a[2] = -rn*r[2];
	  a[3] = -rn*r[3];
	}

	get_hb2(b, rl, srs);
	//b[0] = 1; b[1] = b[2] = b[3] = 0;

	r[0] = b[0]*a[0] - b[1]*a[1] - b[2]*a[2] - b[3]*a[3];
	r[1] = b[0]*a[1] + b[1]*a[0] - b[2]*a[3] + b[3]*a[2];
	r[2] = b[0]*a[2] + b[2]*a[0] - b[3]*a[1] + b[1]*a[3];
	r[3] = b[0]*a[3] + b[3]*a[0] - b[1]*a[2] + b[2]*a[1];

	su2_fill(NCARG &t, r, i, j);
	QLA_M_eq_M_times_M(&tt, &t, &s);
	QLA_M_eq_M(&s, &tt);
	QLA_M_eq_M_times_M(&tt, &t, m);
	QLA_M_eq_M(m, &tt);
      }
    }
    QLA_M_eq_M(m, &s);
  }
}

static void
over_func(NCPROT1 QLA_ColorMatrix(*m), int site)
{
  if(QLA_Nc==1) {
    QLA_Complex z, zs, t;
    QLA_C_eq_elem_M(&z, m, 0, 0);
    QLA_c_eq_ca(zs, z);
    QLA_C_eq_C_divide_C(&t, &zs, &z);
    QLA_M_eq_elem_C(m, &t, 0, 0);
  } else {
    QLA_ColorMatrix(s);
    QLA_ColorMatrix(t);
    QLA_ColorMatrix(tt);
    QLA_Complex one;
    QLA_c_eq_r(one, 1);
    QLA_M_eq_c(&s, &one);

    /* Loop over SU(2) subgroup index */
    for(int i = 0; i < QLA_Nc; ++i) {
      for(int j = i+1; j < QLA_Nc; ++j) {
	QLA_Real a[4], r[4], rn;
	su2_extract(NCARG r, m, i, j);
	rn = sqrt( r[0]*r[0] + r[1]*r[1] + r[2]*r[2] + r[3]*r[3] );
	if(rn<1e-10) {
	  a[0] = 1; a[1] = a[2] = a[3] = 0;
	} else {
	  rn = 1/rn;
	  a[0] =  rn*r[0];
	  a[1] = -rn*r[1];
	  a[2] = -rn*r[2];
	  a[3] = -rn*r[3];
	}
	r[0] = a[0]*a[0] - a[1]*a[1] - a[2]*a[2] - a[3]*a[3];
	a[0] *= 2;
	r[1] = a[0]*a[1];
	r[2] = a[0]*a[2];
	r[3] = a[0]*a[3];
	su2_fill(NCARG &t, r, i, j);
	QLA_M_eq_M_times_M(&tt, &t, &s);
	QLA_M_eq_M(&s, &tt);
	QLA_M_eq_M_times_M(&tt, &t, m);
	QLA_M_eq_M(m, &tt);
      }
    }
    QLA_M_eq_M(m, &s);
  }
}

void 
QOP_symanzik_1loop_gauge_heatbath_qdp(QOP_info_t *info,
				      QDP_ColorMatrix *links[],
				      QLA_Real beta,
				      QOP_gauge_coeffs_t *coeffs,
				      QDP_RandomState *rs0,
				      int nup, int nhb, int nover)
{
#define NC QDP_get_nc(links[0])
  double dtime = QOP_time();
  double nflops = 0;
  if(coeffs->adjoint_plaquette) {
    QOP_error("%s: adj plaq not supported\n", __func__);
  }
  fac = beta/QLA_Nc;
  int imp = (coeffs->rectangle!=0)||(coeffs->parallelogram!=0);
  QDP_Lattice *lat = QDP_get_lattice_M(links[0]);
  int nd = QDP_ndim_L(lat);
  QDP_Subset *cbs=QDP_even_and_odd_L(lat);
  int ncb = 2;
  if(imp) {
    ncb = 32;
    cbs = QOP_get_sub32(lat);
  }

  QDP_ColorMatrix *staple = QDP_create_M_L(lat);
  QDP_ColorMatrix *v = QDP_create_M_L(lat);
  QDP_ColorMatrix *tmp = QDP_create_M_L(lat);
  rs = QDP_expose_S(rs0);

  for(int up=0; up<nup; up++) {
    for(int hb=0; hb<nhb; hb++) {
      for(int cb=0; cb<ncb; cb++) {
	QDP_Subset subset = cbs[cb];
	for(int mu=0; mu<nd; mu++) {
	  QDP_M_eq_zero(staple, subset);
	  QOP_symanzik_1loop_gauge_staple_qdp(info, links, staple, mu, coeffs, cbs, cb);
	  QDP_M_eq_M_times_Ma(v, links[mu], staple, subset);
	  QDP_M_eq_funcit(v, hb_func, subset);
	  QDP_M_eq_M_times_M(tmp, v, links[mu], subset);
	  QDP_M_eq_M(links[mu], tmp, subset);
	}
      }
    }
    for(int over=0; over<nover; over++) {
      for(int cb=0; cb<ncb; cb++) {
	QDP_Subset subset = cbs[cb];
	for(int mu=0; mu<nd; mu++) {
	  QDP_M_eq_zero(staple, subset);
	  QOP_symanzik_1loop_gauge_staple_qdp(info, links, staple, mu, coeffs, cbs, cb);
	  QDP_M_eq_M_times_Ma(v, links[mu], staple, subset);
	  QDP_M_eq_funcit(v, over_func, subset);
	  QDP_M_eq_M_times_M(tmp, v, links[mu], subset);
	  QDP_M_eq_M(links[mu], tmp, subset);
	}
      }
    }
  }

  QDP_reset_S(rs0);
  QDP_destroy_M(tmp);
  QDP_destroy_M(v);
  QDP_destroy_M(staple);

  info->final_sec = QOP_time() - dtime;
  info->final_flop = nflops*QDP_sites_on_node; 
  info->status = QOP_SUCCESS;
#undef NC
}
