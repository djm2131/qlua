/****** fermion_force_asqtad_qdp3.c  -- ******************/
/* MIMD version 6 */
/* Fermion force for Asqtad optimized with single direction QDP operations
 * smaller memory requirement.
 * This method still requires too many restarts in QDP shift operation
 * D.T. 1/28/98, starting from gauge_stuff.c
 * K.O. 3/99 Added optimized fattening for Asq actions
 * D.T. 4/99 Combine force calculations for both mass quarks
 * K.O. 4/99 Optimized force for Asq action
 * S.G. 7/01, modified to use t_longlink and t_fatlink
 * C.D. 10/02, consolidated quark_stuff.c and quark_stuff_tmp.c
 * T.B. 11/01, Added d(M)/d(u0) dependencies for equation of state calc's with
 *             Asqtad action - ATTN: site structure needs 'dfatlink_du0' in
 *             addition to 'fatlink': #define DM_DU0
 *
 * J.O. 3/04 Rearranged loops for optimization
 * J.O. C.D. 3/04 Copied forward links for optimization and 
 *                kept mtags open for restart_gather
 *                Worked with pointers where possible to avoid copying.
 * C.D. 6/04 Corrected memory leak
 * C.D. 3/05 vector QDP version
 * C.D. 3/05 Separated from quark_stuff?.c
 
 * In this directory, assume all paths connect even to odd sites, etc.
 * Tabulate "backwards" paths (e.g. "XDOWN" is backward path to "XUP")
 * as separate parity transforms of the fundamental paths.  They will
 * generally need a negative sign in Dslash.  See bottom for a long
 * comment on sign conventions.
 */

/*
 * 10/01/02, flopcount for ASQ_OPTIMIZED - C. DeTar
 * Fermion force: 253935 for eo_fermion_force()
 * Fermion force: 433968 for eo_fermion_force_3f()
 */

/**#define FFTIME**/
/**#define FFSTIME**/
/**#define QSINLINE**/

#include <qop_internal.h>
//#undef QOP_printf0
//#define QOP_printf0(...)
#define QOP_trace(...)

#define NULL_FP -1 /* NULL field_offset to be used in the optimized version *
                    * of the load_fatlinks subroutine */

#define OPP_DIR(i) (7-i)
#define GOES_FORWARDS(dir) (dir<4)
#define GOES_BACKWARDS(dir) (dir>=4)

static void u_shift_color_vecs(QDP_ColorVector *src[], QDP_ColorVector *dest[],
			       int dir, int nsrc, QDP_ColorVector *tmpvec[]);

static void add_forces_to_mom(QDP_ColorVector **back, QDP_ColorVector **forw,
			      int dir, REAL coeff[], int nsrc);

static void side_link_forces(int mu, int nu, REAL coeff[],
			     QDP_ColorVector **Path, QDP_ColorVector **Path_nu,
			     QDP_ColorVector **Path_mu,
			     QDP_ColorVector **Path_numu, int nsrc);

//static su3_matrix *backwardlink[4];
static QDP_ColorMatrix *tempmom_qdp[4];

//static QDP_ColorMatrix *bcklink[4];
//static QDP_ColorMatrix *fwdlink[4];
//static QDP_ColorVector *hw_qdp[8][2];

static QDP_ColorMatrix *fblink[8];
static QDP_Shift fbshift[8];
static QDP_ShiftDir fbshiftdir[8];
static QDP_ColorVector **tv;

void
QOP_asqtad_force_multi_asvec_qdp(QOP_info_t *info, QDP_ColorMatrix *links[],
				 QDP_ColorMatrix *force[], QOP_asqtad_coeffs_t *coef,
				 REAL eps[], QDP_ColorVector *xin[], int nsrc)
{
#define NC QDP_get_nc(xin[0])
  REAL coeff[nsrc];
  REAL OneLink[nsrc], Lepage[nsrc], Naik[nsrc], FiveSt[nsrc], ThreeSt[nsrc], SevenSt[nsrc];
  REAL mNaik[nsrc], mLepage[nsrc], mFiveSt[nsrc], mThreeSt[nsrc], mSevenSt[nsrc];

  QDP_ColorVector *P3[8][nsrc];

  QDP_ColorVector *P5[8][nsrc];
  QDP_ColorVector *P5tmp[8][8][nsrc];
  QDP_ColorVector *P5s[4][nsrc];
  QDP_ColorVector *P5tmps[4][8][nsrc];

  //QDP_ColorVector *xin[nsrc];
  QDP_ColorVector *xintmp[8][nsrc];
  QDP_ColorVector *Pmu[nsrc];
  QDP_ColorVector *Pmutmp[8][nsrc];
  QDP_ColorVector *Pnumu[nsrc];
  QDP_ColorVector *Pnumutmp[8][nsrc];
  QDP_ColorVector *Prhonumu[nsrc];
  QDP_ColorVector *Prhonumutmp[8][nsrc];
  QDP_ColorVector *P7[nsrc];
  QDP_ColorVector *P7tmp[8][nsrc];
  QDP_ColorVector *P7rho[nsrc];
  QDP_ColorVector *ttv[nsrc];

  int i, dir;
  int mu, nu, rho, sig;

  double nflop1 = 253935;
  double nflop2 = 433968;
  double nflop = nflop1 + (nflop2-nflop1)*(nsrc-1);
  double dtime;
  dtime = -QOP_time();

  ASQTAD_FORCE_BEGIN;

  QOP_trace("test 1\n");
  /* setup parallel transport */
  QDP_ColorMatrix *tmpmat = QDP_create_M();
  for(i=0; i<QOP_common.ndim; i++) {
    fbshift[i] = QDP_neighbor[i];
    fbshiftdir[i] = QDP_forward;
    fblink[i] = links[i];
    fbshift[OPP_DIR(i)] = QDP_neighbor[i];
    fbshiftdir[OPP_DIR(i)] = QDP_backward;
    fblink[OPP_DIR(i)] = QDP_create_M();
    QDP_M_eq_sM(tmpmat, fblink[i], QDP_neighbor[i], QDP_backward, QDP_all);
    QDP_M_eq_Ma(fblink[OPP_DIR(i)], tmpmat, QDP_all);
  }

  tv = ttv;
  for(i=0; i<nsrc; i++) {
    tv[i] = QDP_create_V();
  }

  QOP_trace("test 2\n");
  /* Allocate temporary vectors */
  for(i=0; i<nsrc; i++) {
    Pmu[i] = QDP_create_V();
    Pnumu[i] = QDP_create_V();
    Prhonumu[i] = QDP_create_V();
    P7[i] = QDP_create_V();
    P7rho[i] = QDP_create_V();
    for(dir=0; dir<8; dir++) {
      xintmp[dir][i] = QDP_create_V();
      Pmutmp[dir][i] = QDP_create_V();
      Pnumutmp[dir][i] = QDP_create_V();
      Prhonumutmp[dir][i] = QDP_create_V();
      P7tmp[dir][i] = QDP_create_V();
    }
#if 1
    for(mu=0; mu<4; mu++) {
      P5s[mu][i] = QDP_create_V();
      for(dir=0; dir<8; dir++) {
	P5tmps[mu][dir][i] = QDP_create_V();
      }
    }
#else
    for(mu=0; mu<8; mu++) {
      P5[mu][i] = QDP_create_V();
      for(dir=0; dir<8; dir++) {
	P5tmp[mu][dir][i] = QDP_create_V();
	//printf("%p %p\n", P5tmp[mu][dir][i], &(P5tmp[mu][dir][i])); fflush(stdout);
	if(P5tmp[mu][dir][i]==NULL) {
	  fprintf(stderr, "error: can't create V\n");
	  QDP_abort();
	}
      }
    }
#endif
  }
  //printf("%p\n", P5tmp[0][4][0]); fflush(stdout);

  for(mu=0; mu<8; mu++) {
    for(i=0; i<nsrc; i++) {
      P3[mu][i] = QDP_create_V();
      //P5[mu][i] = QDP_create_V();
    }
  }

  for(mu=0; mu<4; mu++) {
    tempmom_qdp[mu] = force[mu];
    QDP_M_eqm_M(tempmom_qdp[mu], tempmom_qdp[mu], QDP_odd);
  }

  /* Path coefficients times fermion epsilon */
  /* Load path coefficients from table */
  for(i=0; i<nsrc; i++) {
    OneLink[i] = coef->one_link     * eps[i];
    Naik[i]    = coef->naik         * eps[i]; mNaik[i]    = -Naik[i];
    ThreeSt[i] = coef->three_staple * eps[i]; mThreeSt[i] = -ThreeSt[i];
    FiveSt[i]  = coef->five_staple  * eps[i]; mFiveSt[i]  = -FiveSt[i];
    SevenSt[i] = coef->seven_staple * eps[i]; mSevenSt[i] = -SevenSt[i];
    Lepage[i]  = coef->lepage       * eps[i]; mLepage[i]  = -Lepage[i];
  }

#if 0
  printf("nsrc = %i\n", nsrc);
  printf("coeffs = %g %g %g %g %g %g\n", OneLink[0], ThreeSt[0], FiveSt[0],
	 SevenSt[0], Lepage[0], Naik[0]);
#endif

  /* *************************************** */

  QOP_trace("start force loop\n");
  for(mu=0; mu<8; mu++) {
    //u_shift_hw_fermion(temp_x_qdp, Pmu, OPP_DIR(mu), temp_hw[OPP_DIR(mu)]);
    u_shift_color_vecs(xin, Pmu, OPP_DIR(mu), nsrc, xintmp[OPP_DIR(mu)]);

    for(sig=0; sig<8; sig++) if( (sig!=mu)&&(sig!=OPP_DIR(mu)) ) {
      //u_shift_hw_fermion(Pmu, P3[sig], sig, temp_hw[sig]);
      u_shift_color_vecs(Pmu, P3[sig], sig, nsrc, Pmutmp[sig]);

      if(GOES_FORWARDS(sig)) {
	/* Add the force F_sig[x+mu]:         x--+             *
	 *                                   |   |             *
	 *                                   o   o             *
	 * the 1 link in the path: - (numbering starts form 0) */
	add_forces_to_mom(P3[sig], Pmu, sig, mThreeSt, nsrc);
      }
    }

    for(nu=0; nu<8; nu++) if( (nu!=mu)&&(nu!=OPP_DIR(mu)) ) {
      int nP5 = 0;
      //Pnumu = hw_qdp[OPP_DIR(nu)];
      //u_shift_hw_fermion(Pmu, Pnumu, OPP_DIR(nu), temp_hw[OPP_DIR(nu)]);
      u_shift_color_vecs(Pmu, Pnumu, OPP_DIR(nu), nsrc, Pmutmp[OPP_DIR(nu)]);
      //QDP_V_veq_V(Pnumu, P3[OPP_DIR(nu)], QDP_all, nsrc);
      for(sig=0; sig<8; sig++) if( (sig!=mu)&&(sig!=OPP_DIR(mu)) &&
				   (sig!=nu)&&(sig!=OPP_DIR(nu)) ) {
#if 1
	for(i=0; i<nsrc; i++) {
	  P5[sig][i] = P5s[nP5][i];
	  for(dir=0; dir<8; dir++) P5tmp[sig][dir][i] = P5tmps[nP5][dir][i];
	}
#endif
	nP5++;
	//u_shift_hw_fermion(Pnumu, P5[sig], sig, temp_hw[sig]);
	u_shift_color_vecs(Pnumu, P5[sig], sig, nsrc, Pnumutmp[sig]);

	if(GOES_FORWARDS(sig)) {
	  /* Add the force F_sig[x+mu+nu]:      x--+             *
	   *                                   |   |             *
	   *                                   o   o             *
	   * the 2 link in the path: + (numbering starts form 0) */
	  add_forces_to_mom(P5[sig], Pnumu, sig, FiveSt, nsrc);
	}
      }
      QOP_trace("test 4\n");
      for(rho=0; rho<8; rho++) if( (rho!=mu)&&(rho!=OPP_DIR(mu)) &&
				   (rho!=nu)&&(rho!=OPP_DIR(nu)) ) {
	//Prhonumu = hw_qdp[OPP_DIR(rho)];
	//u_shift_hw_fermion(Pnumu, Prhonumu, OPP_DIR(rho), 
	//		 temp_hw[OPP_DIR(rho)] );
	  u_shift_color_vecs(Pnumu, Prhonumu, OPP_DIR(rho), nsrc,
			     Pnumutmp[OPP_DIR(rho)]);
	  //QDP_V_veq_V(Prhonumu, P5[OPP_DIR(rho)], QDP_all, nsrc);
	for(sig=0; sig<8; sig++) if( (sig!=mu )&&(sig!=OPP_DIR(mu )) &&
				     (sig!=nu )&&(sig!=OPP_DIR(nu )) &&
				     (sig!=rho)&&(sig!=OPP_DIR(rho)) ) {
	  /* Length 7 paths */
	  //P7 = hw_qdp[sig];
	  //u_shift_hw_fermion(Prhonumu, P7, sig, temp_hw[sig] );
  QOP_trace("test 43\n");
	  u_shift_color_vecs(Prhonumu, P7, sig, nsrc, Prhonumutmp[sig]);
  QOP_trace("test 44\n");
	  //QDP_V_eq_r_times_V(P7[0], &SevenSt[0], P7[0], QDP_all);
	  //QDP_V_eq_r_times_V(P7[1], &SevenSt[1], P7[1], QDP_all);
	  if(GOES_FORWARDS(sig)) {
	    /* Add the force F_sig[x+mu+nu+rho]:  x--+             *
	     *                                   |   |             *
	     *                                   o   o             *
	     * the 3 link in the path: - (numbering starts form 0) */
  QOP_trace("test 45\n");
	    add_forces_to_mom(P7, Prhonumu, sig, mSevenSt, nsrc);
  QOP_trace("test 46\n");
	    //mom_meq_force(P7, Prhonumu, sig);
	  }
	  /* Add the force F_rho the 2(4) link in the path: +     */
	  //P7rho = hw_qdp[rho];
	  //u_shift_hw_fermion(P7, P7rho, rho, temp_hw[rho]);
  QOP_trace("test 47\n");
	  u_shift_color_vecs(P7, P7rho, rho, nsrc, P7tmp[rho]);
  QOP_trace("test 48\n");
	  side_link_forces(rho,sig,SevenSt,Pnumu,P7,Prhonumu,P7rho, nsrc);
  QOP_trace("test 49\n");
	  //side_link_3f_force2(rho,sig,Pnumu,P7,Prhonumu,P7rho);
	  /* Add the P7rho vector to P5 */
	  for(i=0; i<nsrc; i++) {
	    if(FiveSt[i]!=0) coeff[i] = SevenSt[i]/FiveSt[i];
	    else coeff[i] = 0;
  QOP_trace("test 410\n");
	    QDP_V_peq_r_times_V(P5[sig][i], &coeff[i], P7rho[i], QDP_all);
  QOP_trace("test 411\n");
	  }
	} /* sig */
      } /* rho */
  QOP_trace("test 5\n");
#define P5nu P7
      for(sig=0; sig<8; sig++) if( (sig!=mu)&&(sig!=OPP_DIR(mu)) &&
				   (sig!=nu)&&(sig!=OPP_DIR(nu)) ) {
	/* Length 5 paths */
	/* Add the force F_nu the 1(3) link in the path: -     */
	//P5nu = hw_qdp[nu];
	//u_shift_hw_fermion(P5[sig], P5nu, nu, temp_hw[nu]);
	u_shift_color_vecs(P5[sig], P5nu, nu, nsrc, P5tmp[sig][nu]);
	side_link_forces(nu, sig, mFiveSt, Pmu, P5[sig], Pnumu, P5nu, nsrc);
	/* Add the P5nu vector to P3 */
	for(i=0; i<nsrc; i++) {
	  if(ThreeSt[i]!=0) coeff[i] = FiveSt[i]/ThreeSt[i]; 
	  else coeff[i] = 0;
	  QDP_V_peq_r_times_V(P3[sig][i], &coeff[i], P5nu[i], QDP_all);
	}
      } /* sig */
    } /* nu */

#define Pmumu Pnumu
#define Pmumutmp Pnumutmp
#define P5sig Prhonumu
#define P5sigtmp Prhonumutmp
#define P3mu P7
#define Popmu P7
#define Pmumumu P7
    /* Now the Lepage term... It is the same as 5-link paths with
       nu=mu and FiveSt=Lepage. */
    //u_shift_hw_fermion(Pmu, Pmumu, OPP_DIR(mu), temp_hw[OPP_DIR(mu)] );
    u_shift_color_vecs(Pmu, Pmumu, OPP_DIR(mu), nsrc, Pmutmp[OPP_DIR(mu)]);

    for(sig=0; sig<8; sig++) if( (sig!=mu)&&(sig!=OPP_DIR(mu)) ) {
      //P5sig = hw_qdp[sig];
      //u_shift_hw_fermion(Pmumu, P5sig, sig, temp_hw[sig]);
      u_shift_color_vecs(Pmumu, P5sig, sig, nsrc, Pmumutmp[sig]);
      if(GOES_FORWARDS(sig)) {
	/* Add the force F_sig[x+mu+nu]:      x--+             *
	 *                                   |   |             *
	 *                                   o   o             *
	 * the 2 link in the path: + (numbering starts form 0) */
	add_forces_to_mom(P5sig, Pmumu, sig, Lepage, nsrc);
      }
      /* Add the force F_nu the 1(3) link in the path: -     */
      //P5nu = hw_qdp[mu];
      //u_shift_hw_fermion(P5sig, P5nu, mu, temp_hw[mu]);
      u_shift_color_vecs(P5sig, P5nu, mu, nsrc, P5sigtmp[mu]);
      side_link_forces(mu, sig, mLepage, Pmu, P5sig, Pmumu, P5nu, nsrc);
      /* Add the P5nu vector to P3 */
      for(i=0; i<nsrc; i++) {
	if(ThreeSt[i]!=0) coeff[i] = Lepage[i]/ThreeSt[i];
	else coeff[i] = 0;
	QDP_V_peq_r_times_V(P3[sig][i], &coeff[i], P5nu[i], QDP_all);
      }

      /* Length 3 paths (Not the Naik term) */
      /* Add the force F_mu the 0(2) link in the path: +     */
      if(GOES_FORWARDS(mu)) {
	//P3mu = hw_qdp[mu];  /* OK to clobber P5nu */
	//u_shift_hw_fermion(P3[sig], P3mu, mu, temp_hw[mu]);
	//u_shift_color_vecs(P3[sig], P3mu, mu, 2, temp_hw[mu]);
	for(i=0; i<nsrc; i++) {
	  QDP_V_eq_V(P5sig[i], P3[sig][i], QDP_all);
	}
	u_shift_color_vecs(P5sig, P3mu, mu, nsrc, P5sigtmp[mu]);
      }
      /* The above shift is not needed if mu is backwards */
      side_link_forces(mu, sig, ThreeSt, xin, P3[sig], Pmu, P3mu, nsrc);
    }

    /* Finally the OneLink and the Naik term */
    if(GOES_BACKWARDS(mu)) {
      /* Do only the forward terms in the Dslash */
      /* Because I have shifted with OPP_DIR(mu) Pmu is a forward *
       * shift.                                                   */
      /* The one link */
      add_forces_to_mom(Pmu, xin, OPP_DIR(mu), OneLink, nsrc);
      /* For the same reason Pmumu is the forward double link */

      /* Popmu is a backward shift */
      //Popmu = hw_qdp[mu]; /* OK to clobber P3mu */
      //u_shift_hw_fermion(xin, Popmu, mu, temp_hw[mu]);
      u_shift_color_vecs(xin, Popmu, mu, nsrc, xintmp[mu]);
      /* The Naik */
      /* link no 1: - */
      add_forces_to_mom(Pmumu, Popmu, OPP_DIR(mu), mNaik, nsrc);
      /* Pmumumu can overwrite Popmu which is no longer needed */
      //Pmumumu = hw_qdp[OPP_DIR(mu)];
      //u_shift_hw_fermion(Pmumu, Pmumumu, OPP_DIR(mu), temp_hw[OPP_DIR(mu)]);
      u_shift_color_vecs(Pmumu, Pmumumu, OPP_DIR(mu), nsrc, Pmumutmp[OPP_DIR(mu)]);
      /* link no 0: + */
      add_forces_to_mom(Pmumumu, xin, OPP_DIR(mu), Naik, nsrc);
    } else {
      /* The rest of the Naik terms */
      //Popmu = hw_qdp[mu]; /* OK to clobber P3mu */
      //u_shift_hw_fermion(xin, Popmu, mu, temp_hw[mu]);
      u_shift_color_vecs(xin, Popmu, mu, nsrc, xintmp[mu]);
      /* link no 2: + */
      /* Pmumu is double backward shift */
      add_forces_to_mom(Popmu, Pmumu, mu, Naik, nsrc);
    }
    /* Here we have to do together the Naik term and the one link term */

  }/* mu */
  QOP_trace("test 6\n");
  QOP_trace("test 7\n");

  for(mu=0; mu<4; mu++) {
    QDP_M_eq_M(tmpmat, tempmom_qdp[mu], QDP_even);
    QDP_M_eqm_M(tmpmat, tempmom_qdp[mu], QDP_odd);
    QDP_M_eq_antiherm_M(tempmom_qdp[mu], tmpmat, QDP_all);
  }
  QDP_destroy_M(tmpmat);

  //printf("%p\n", P5tmp[0][4][0]); fflush(stdout);
  //if(QDP_this_node==0) { printf("line %i\n",__LINE__); fflush(stdout); }
  /* Free temporary vectors */
  for(i=0; i<nsrc; i++) {
    QDP_destroy_V(Pmu[i]);
    QDP_destroy_V(Pnumu[i]);
    QDP_destroy_V(Prhonumu[i]);
    QDP_destroy_V(P7[i]);
    QDP_destroy_V(P7rho[i]);
    //if(QDP_this_node==0) { printf("line %i\n",__LINE__); fflush(stdout); }
    for(dir=0; dir<8; dir++) {
      QDP_destroy_V(xintmp[dir][i]);
      QDP_destroy_V(Pmutmp[dir][i]);
      QDP_destroy_V(Pnumutmp[dir][i]);
      QDP_destroy_V(Prhonumutmp[dir][i]);
      QDP_destroy_V(P7tmp[dir][i]);
    }
    //if(QDP_this_node==0) { printf("line %i\n",__LINE__); fflush(stdout); }
    for(mu=0; mu<4; mu++) {
      //if(QDP_this_node==0) { printf("line %i\n",__LINE__); fflush(stdout); }
      QDP_destroy_V(P5s[mu][i]);
      //QDP_destroy_V(P5[mu][i]);
      //if(QDP_this_node==0) { printf("line %i\n",__LINE__); fflush(stdout); }
      for(dir=0; dir<8; dir++) {
	//if(QDP_this_node==0) { printf("line %i\n",__LINE__); fflush(stdout); }
	QDP_destroy_V(P5tmps[mu][dir][i]);
	//printf("%p\n", P5tmp[mu][dir][i]); fflush(stdout);
	//QDP_destroy_V(P5tmp[mu][dir][i]);
	//if(QDP_this_node==0) { printf("line %i\n",__LINE__); fflush(stdout); }
      }
      //if(QDP_this_node==0) { printf("line %i\n",__LINE__); fflush(stdout); }
    }
    //if(QDP_this_node==0) { printf("line %i\n",__LINE__); fflush(stdout); }
  }

  //if(QDP_this_node==0) { printf("here3\n"); fflush(stdout); }
  for(mu=0; mu<8; mu++) {
    for(i=0; i<nsrc; i++) {
      QDP_destroy_V(P3[mu][i]);
    }
    //QDP_destroy_V(P5[mu][0]);
    //QDP_destroy_V(P5[mu][1]);
  }

  for(i=0; i<nsrc; i++) {
    QDP_destroy_V(tv[i]);
  }

  //if(QDP_this_node==0) { printf("here4\n"); fflush(stdout); }
  for(i=4; i<8; i++) {
    QDP_destroy_M(fblink[i]);
  }

  dtime += QOP_time();
  info->final_sec = dtime;
  info->final_flop = nflop*QDP_sites_on_node;
  info->status = QOP_SUCCESS;

  ASQTAD_FORCE_END;
#undef NC
}

#undef Pmu          
#undef Pnumu        
#undef Prhonumu     
#undef P7           
#undef P7rho        
#undef P7rhonu      
//#undef P5
#undef P3           
#undef P5nu         
#undef P3mu         
#undef Popmu        
#undef Pmumumu      

static void
u_shift_color_vecs(QDP_ColorVector *src[], QDP_ColorVector *dest[], int dir,
		   int n, QDP_ColorVector *tmpvec[])
{
  QDP_ColorMatrix *gv[n];
  int i;
  for(i=0; i<n; i++) {
    QDP_V_eq_sV(tmpvec[i], src[i], fbshift[dir], fbshiftdir[dir], QDP_all);
  }
  for(i=0; i<n; i++) {
    gv[i] = fblink[dir];
  }
  QDP_V_veq_M_times_V(dest, gv, tmpvec, QDP_all, n);
  for(i=0; i<n; i++) {
    //QDP_V_eq_M_times_V(dest[i], gv[i], tmpvec[i], QDP_all);
    QDP_discard_V(tmpvec[i]);
  }
}

/* Add in contribution to the force ( 3flavor case ) */
/* Put antihermitian traceless part into momentum */
static void
add_forces_to_mom(QDP_ColorVector **back_qdp, QDP_ColorVector **forw_qdp, 
		  int dir, REAL coeff[], int nsrc)
{
  REAL tmp_coeff[nsrc];
  QDP_ColorMatrix *tm[nsrc];
  int i;

  QOP_trace("test 481\n");
  if(GOES_BACKWARDS(dir)) {
    dir = OPP_DIR(dir); 
    for(i=0; i<nsrc; i++) {
      tmp_coeff[i] = -coeff[i];
    }
  } else {
    for(i=0; i<nsrc; i++) {
      tmp_coeff[i] = coeff[i];
    }
  }
  QOP_trace("test 482\n");

  for(i=0; i<nsrc; i++) {
    QDP_V_eq_r_times_V(tv[i], &tmp_coeff[i], forw_qdp[i], QDP_all);
    tm[i] = tempmom_qdp[dir];
  }
  QOP_trace("test 483\n");
  QDP_M_vpeq_V_times_Va(tm, back_qdp, tv, QDP_all, nsrc);
  QOP_trace("test 484\n");
}

/*  The 3 flavor version of side_link_force used *
 * to optimize fermion transports                */
static void
side_link_forces(int mu, int nu, REAL coeff[], QDP_ColorVector **Path,
		 QDP_ColorVector **Path_nu, QDP_ColorVector **Path_mu,
		 QDP_ColorVector **Path_numu, int nsrc)
{
  REAL m_coeff[nsrc];
  int i;

  for(i=0; i<nsrc; i++) {
    m_coeff[i] = -coeff[i];
  }

  if(GOES_FORWARDS(mu))
    {
      /*                    nu           * 
       * Add the force :  +----+         *
       *               mu |    |         *
       *                  x    (x)       *
       *                  o    o         */
      if(GOES_FORWARDS(nu))
	add_forces_to_mom(Path_numu, Path, mu, coeff, nsrc);
      else
	//add_forces_to_mom(Path,Path_numu,OPP_DIR(mu),m_coeff, nsrc);
	add_forces_to_mom(Path_numu,Path,mu,m_coeff, nsrc);
    }
  else /*GOES_BACKWARDS(mu)*/
    {
      /* Add the force :  o    o         *
       *               mu |    |         *
       *                  x    (x)       *
       *                  +----+         *
       *                    nu           */ 
      if(GOES_FORWARDS(nu))
	add_forces_to_mom(Path_nu, Path_mu, mu, m_coeff, nsrc);
      else
	add_forces_to_mom(Path_mu, Path_nu, OPP_DIR(mu), coeff, nsrc);
    }
}


/* LONG COMMENTS
   Here we have combined "xxx", (offset "x_off")  which is
(M_adjoint M)^{-1} phi, with Dslash times this vector, which goes in the
odd sites of xxx.  Recall that phi is defined only on even sites.  In
computing the fermion force, we are looking at

< X |  d/dt ( Dslash_eo Dslash_oe ) | X >
=
< X | d/dt Dslash_eo | T > + < T | d/dt Dslash_oe | X >
where T = Dslash X.

The subsequent manipulations to get the coefficent of H, the momentum
matrix, in the simulation time derivative above look the same for
the two terms, except for a minus sign at the end, if we simply stick
T, which lives on odd sites, into the odd sites of X

 Each path in the action contributes terms when any link of the path
is the link for which we are computing the force.  We get a minus sign
for odd numbered links in the path, since they connect sites of the
opposite parity from what it would be for an even numbered link.
Minus signs from "going around" plaquette - ie KS phases, are supposed
to be already encoded in the path coefficients.
Minus signs from paths that go backwards are supposed to be already
encoded in the path coefficients.

Here, for example, are comments reproduced from the force routine for
the one-link plus Naik plus single-staple-fat-link action:

 The three link force has three contributions, where the link that
was differentiated is the first, second, or third link in the 3-link
path, respectively.  Diagramatically, where "O" represents the momentum,
the solid line the link corresponding to the momentum, and the dashed
lines the other links:
 

	O______________ x ............ x ...............
+
	x..............O______________x.................
+
	x..............x..............O________________
Think of this as
	< xxx | O | UUUxxx >		(  xxx, UUUX_p3 )
+
	< xxx U | O | UUxxx >		( X_m1U , UUX_p2 )
+
	< xxx U U | O | Uxxx >		( X_m2UU , UX_p1 )
where "U" indicates parallel transport, "X_p3" is xxx displaced
by +3, etc.
Note the second contribution has a relative minus sign
because it effectively contributes to the <odd|even>, or M_adjoint,
part of the force when we work on an even site. i.e., for M on
an even site, this three link path begins on an odd site.

The staple force has six contributions from each plane containing the
link direction:
Call these diagrams A-F:


	x...........x		O____________x
		    .			     .
		    .			     .
		    .			     .
		    .			     .
		    .			     .
	O___________x		x............x
	   (A)			    (B)



	x	    x		O____________x
	.	    .		.	     .
	.	    .		.	     .
	.	    .		.	     .
	.	    .		.	     .
	.	    .		.	     .
	O___________x		x	     x
	   (C)			    (D)



	x...........x		O____________x
	.			.
	.			.
	.			.
	.			.
	.			.
	O___________x		x............x
	   (E)			    (F)

As with the Naik term, diagrams C and D have a relative minus
sign because they connect sites of the other parity.

Also note an overall minus sign in the staple terms relative to the
one link term because, with the KS phase factors included, the fat
link is  "U - w3 * UUU", or the straight link MINUS w3 times the staples.

Finally, diagrams B and E get one more minus sign because the link
we are differentiating is in the opposite direction from the staple
as a whole.  You can think of this as this "U" being a correction to
a "U_adjoint", but the derivative of U is iHU and the derivative
of U_adjoint is -iHU_adjoint.

*/
/* LONG COMMENT on sign conventions
In most of the program, the KS phases and antiperiodic boundary
conditions are absorbed into the link matrices.  This greatly simplfies
multiplying by the fermion matrix.  However, it requires care in
specifying the path coefficients.  Remember that each time you
encircle a plaquette, you pick up a net minus sign from the KS phases.
Thus, when you have more than one path to the same point, you generally
have a relative minus sign for each plaquette in a surface bounded by
this path and the basic path for that displacement.

Examples:
  Fat Link:
    Positive:	X-------X

    Negative     --------
	 	|	|
		|	|
		X	X

  Naik connection, smeared
    Positive:	X-------x-------x-------X

    Negative:	---------
		|	|
		|	|
		X	x-------x-------X

    Positive:	--------x--------
		|		|
		|		|
		X		x-------X

    Negative:	--------x-------x-------x
		|			|
		|			|
		X			X
*/



/* Comment on acceptable actions.
   We construct the backwards part of dslash by reversing all the
   paths in the forwards part.  So, for example, in the p4 action
   the forwards part includes +X+Y+Y

		X
		|
		|
		X
		|
		|
	X---->--X

  so we put -X-Y-Y in the backwards part.  But this isn't the adjoint
  of U_x(0)U_y(+x)U_y(+x+y).  Since much of the code assumes that the
  backwards hop is the adjoint of the forwards (for example, in
  preventing going to 8 flavors), the code only works for actions
  where this is true.  Roughly, this means that the fat link must
  be symmetric about reflection around its midpoint.  Equivalently,
  the paths in the backwards part of Dslash are translations of the
  paths in the forwards part.  In the case of the "P4" or knight's move
  action, this means that we have to have both paths
   +X+Y+Y and +Y+Y+X to the same point, with the same coefficients.
  Alternatively, we could just use the symmetric path +Y+X+Y.
*/
