rem(`
     Variable definitions
     (Include file for test_tensor_idx.[1-4].m4)
')

static  QLA_Real                  *sR1,*sR2,*sR3;
static  QLA_Complex               *sC1,*sC2,*sC3;
static  QLA_ColorMatrix           *sM1,*sM2,*sM3;
static  QLA_HalfFermion           *sH1,*sH2,*sH3;
static  QLA_DiracFermion          *sD1,*sD2,*sD3;
static  QLA_ColorVector           *sV1,*sV2,*sV3;
static  QLA_DiracPropagator       *sP1,*sP2,*sP3;

static  QLA_RandomState           sS1[MAX];

static  QLA_Real                  *destR,*chkR;
static  QLA_Complex               *destC,*chkC;
static  QLA_ColorMatrix           *destM,*chkM;
static  QLA_HalfFermion           *destH,*chkH;
static  QLA_DiracFermion          *destD,*chkD;
static  QLA_ColorVector           *destV,*chkV;
static  QLA_DiracPropagator       *destP,*chkP;

static  QLA_Real sC4re      = 1.2312;
static  QLA_Real sC4im      = -0.701;

static  QLA_Complex sC4;

static  QLA_ColorMatrix sM4;
static  QLA_HalfFermion sH4;
static  QLA_DiracFermion sD4;
static  QLA_ColorVector sV4;
static  QLA_DiracPropagator sP4;

static  int nI1[MAX] = { 3, 12, 7, 1, 5, 8, 3, 2, 1, 5};
static  int zI1[MAX] = { 3, 0, 7, 1, 0, 0, 3, 2, 1, 0};

static  QLA_Int sI1[MAX] = { 61, -10,  73, -96,  50, 92,  34, -21, -67, 104};

static  QLA_Int sI3[MAX] = { 92,  34, -21, -67, 104, 61, -10,  73, -96,  50};

static  QLA_Int sI4      = 5001;

static  int sR1x[MAX] = {3,0,1,8,2,4,5,9,7,6};
static  int sR2x[MAX] = {4,9,0,2,1,3,7,8,5,6};
static  int sR3x[MAX] = {8,3,2,5,6,9,7,4,0,1};

static  int sC1x[MAX] = {8,5,6,7,1,2,9,0,3,4};
static  int sC2x[MAX] = {4,9,0,2,1,3,7,8,5,6};

static  int sI1x[MAX] = {4,9,0,2,1,3,7,8,5,6};

static  int sH1x[MAX] = {4,6,1,2,9,7,0,3,5,8};
static  int sH2x[MAX] = {8,1,6,0,3,7,5,9,2,4};

static  int sD1x[MAX] = {8,6,0,3,7,2,9,1,5,4};
static  int sD2x[MAX] = {9,6,1,4,5,8,0,2,7,3};

static  int sV1x[MAX] = {6,4,0,2,1,9,7,3,5,8};
static  int sV2x[MAX] = {1,9,3,6,2,4,0,8,7,5};

static  int sP1x[MAX] = {3,7,9,0,2,4,1,8,5,6};
static  int sP2x[MAX] = {2,9,5,6,7,8,1,0,3,4};

static  int sM1x[MAX] = {6,0,3,7,9,5,8,1,2,4};
static  int sM2x[MAX] = {6,1,4,5,0,7,9,2,8,3};

static  int sS1x[MAX] = {1,3,8,5,9,4,7,6,0,2};

static  QLA_Int                 *nI1p[MAX], *zI1p[MAX];
static  QLA_Int                 *sI1p[MAX];
static  QLA_Real                *sR1p[MAX], *sR2p[MAX], *sR3p[MAX];
static  QLA_Complex             *sC1p[MAX], *sC2p[MAX], *sC3p[MAX], *chkCp[MAX];
static  QLA_ColorMatrix         *sM1p[MAX], *sM2p[MAX], *sM3p[MAX];
static  QLA_HalfFermion         *sH1p[MAX], *sH2p[MAX], *sH3p[MAX];
static  QLA_DiracFermion        *sD1p[MAX], *sD2p[MAX], *sD3p[MAX], *chkDp[MAX];
static  QLA_ColorVector         *sV1p[MAX], *sV2p[MAX], *sV3p[MAX], *chkVp[MAX];
static  QLA_DiracPropagator     *sP1p[MAX], *sP2p[MAX], *sP3p[MAX];
static  QLA_RandomState         *sS1p[MAX];

#if ( QLA_Precision != 'Q' )
#ifdef MULTISOURCE
//static int *sV1nx = sV1x;
static int *sM1nx = sM1x;
#define ND 3
static  QLA_ColorVector *sV1n[ND];
static  QLA_ColorMatrix *sM1n[ND];
static  QLA_ColorVector **sV1np[ND];
static  QLA_ColorMatrix **sM1np[ND];
static  QLA_ColorVector *sV1nt[ND];
static  QLA_ColorMatrix *sM1nt[ND];
#endif
#endif

static  char name[64];

static  int i;

#define arrayr(t,v,n) v=(t*)amalloc(n*sizeof(t),ealign*sizeof(QLA_Real))
#define arrayc(t,v,n) v=(t*)amalloc(n*sizeof(t),ealign*sizeof(QLA_Complex))
#define arrayrq(t,v,n) v=(t*)amalloc(n*sizeof(t),ealign*sizeof(QLA_Q_Real))
#define arraycq(t,v,n) v=(t*)amalloc(n*sizeof(t),ealign*sizeof(QLA_Q_Complex))
static void *
amalloc(size_t n, size_t align)
{
  size_t a2 = 2*align;
  void *t = malloc(n+a2);
  //fprintf(stderr, "align: %i  t: %p\n", (int)align, t);
  size_t endbits = ((size_t)t) & (a2-1);
  size_t offset = ((a2 + align) - endbits) % a2;
  //fprintf(stderr, "align: %i  offset: %i\n", (int)align, (int)offset);
  return (void*)(offset + (char*)t);
}

static void
initialize_variables(FILE *fp, int ealign)
{
  for(i = 0; i < MAX; i++){
    arrayr(QLA_Real, sR1, MAX);
    arrayr(QLA_Real, sR2, MAX);
    arrayr(QLA_Real, sR3, MAX);
    arrayc(QLA_Complex, sC1, MAX);
    arrayc(QLA_Complex, sC2, MAX);
    arrayc(QLA_Complex, sC3, MAX);
    arrayc(QLA_ColorMatrix, sM1, MAX);
    arrayc(QLA_ColorMatrix, sM2, MAX);
    arrayc(QLA_ColorMatrix, sM3, MAX);
    arrayc(QLA_HalfFermion, sH1, MAX);
    arrayc(QLA_HalfFermion, sH2, MAX);
    arrayc(QLA_HalfFermion, sH3, MAX);
    arrayc(QLA_DiracFermion, sD1, MAX);
    arrayc(QLA_DiracFermion, sD2, MAX);
    arrayc(QLA_DiracFermion, sD3, MAX);
    arrayc(QLA_ColorVector, sV1, MAX);
    arrayc(QLA_ColorVector, sV2, MAX);
    arrayc(QLA_ColorVector, sV3, MAX);
    arrayc(QLA_DiracPropagator, sP1, MAX);
    arrayc(QLA_DiracPropagator, sP2, MAX);
    arrayc(QLA_DiracPropagator, sP3, MAX);
    arrayr(QLA_Real, destR, MAX);
    arrayr(QLA_Real, chkR, MAX);
    arrayc(QLA_Complex, destC, MAX);
    arrayc(QLA_Complex, chkC, MAX);
    arrayc(QLA_ColorMatrix, destM, MAX);
    arrayc(QLA_ColorMatrix, chkM, MAX);
    arrayc(QLA_HalfFermion, destH, MAX);
    arrayc(QLA_HalfFermion, chkH, MAX);
    arrayc(QLA_DiracFermion, destD, MAX);
    arrayc(QLA_DiracFermion, chkD, MAX);
    arrayc(QLA_ColorVector, destV, MAX);
    arrayc(QLA_ColorVector, chkV, MAX);
    arrayc(QLA_DiracPropagator, destP, MAX);
    arrayc(QLA_DiracPropagator, chkP, MAX);
  }

#if ( QLA_Precision != 'Q' )
#ifdef MULTISOURCE
  sV1n[0] = sV1; sV1n[1] = sV2; sV1n[2] = sV3;
  sM1n[0] = sM1; sM1n[1] = sM2; sM1n[2] = sM3;
  sV1np[0] = sV1p; sV1np[1] = sV2p; sV1np[2] = sV3p;
  sM1np[0] = sM1p; sM1np[1] = sM2p; sM1np[2] = sM3p;
#endif
#endif

  for(i = 0; i < MAX; i++){
    sR1p[i] = &sR1[sR2x[i]];
    sR2p[i] = &sR2[sR3x[i]];
    sR3p[i] = &sR3[sR1x[i]];

    sC1p[i] = &sC1[sC1x[i]];
    sC2p[i] = &sC2[sC2x[i]];
    sC3p[i] = &sC3[sC2x[i]];
    chkCp[i] = &chkC[sC1x[i]];

    sH1p[i] = &sH1[sH1x[i]];
    sH2p[i] = &sH2[sH2x[i]];
    sH3p[i] = &sH3[sH2x[i]];

    sD1p[i] = &sD1[sD1x[i]];
    sD2p[i] = &sD2[sD2x[i]];
    sD3p[i] = &sD3[sD2x[i]];
    chkDp[i] = &chkD[sD1x[i]];

    sV1p[i] = &sV1[sV1x[i]];
    sV2p[i] = &sV2[sV2x[i]];
    sV3p[i] = &sV3[sV2x[i]];
    chkVp[i] = &chkV[sV1x[i]];

    sP1p[i] = &sP1[sP1x[i]];
    sP2p[i] = &sP2[sP2x[i]];
    sP3p[i] = &sP3[sP2x[i]];

    sM1p[i] = &sM1[sM1x[i]];
    sM2p[i] = &sM2[sM2x[i]];
    sM3p[i] = &sM3[sM2x[i]];

    sI1p[i] = &sI1[sI1x[i]];

    nI1p[i] = &nI1[sR3x[i]];
    zI1p[i] = &zI1[sR2x[i]];

    sS1p[i] = &sS1[sS1x[i]];
  }

  destR[0] = 0.;
  chkR[0] = 0.;
  QLA_c_eq_r(destC[0], 0.);

  QLA_c_eq_r_plus_ir(sC4,sC4re,sC4im);
  /* Preliminary check of vector copy */

alltensors(`chkVectorCopy')
alltensors2(`unary',eq);
alltensors2(`unary',peq);
alltensors2(`unary',eqm);
alltensors2(`unary',meq);

  /* Preliminary check of random number generator */

unaryrand(H,eq_gaussian)
unaryrand(D,eq_gaussian)
unaryrand(V,eq_gaussian)
unaryrand(P,eq_gaussian)
unaryrand(M,eq_gaussian)

`
  /* Create random test values */
  QLA_S_veq_seed_i_I(sS1,sI4,sI3,MAX);

  QLA_H_eq_gaussian_S(&sH4,sS1);
  QLA_D_eq_gaussian_S(&sD4,sS1);
  QLA_V_eq_gaussian_S(&sV4,sS1);
  QLA_P_eq_gaussian_S(&sP4,sS1);
  QLA_M_eq_gaussian_S(&sM4,sS1);

  for(i = 0; i < MAX; i++){

    QLA_H_veq_gaussian_S(sH1,sS1,MAX);
    QLA_H_veq_gaussian_S(sH2,sS1,MAX);
    QLA_H_veq_gaussian_S(sH3,sS1,MAX);
			  	 
    QLA_D_veq_gaussian_S(sD1,sS1,MAX);
    QLA_D_veq_gaussian_S(sD2,sS1,MAX);
    QLA_D_veq_gaussian_S(sD3,sS1,MAX);
			  	 
    QLA_V_veq_gaussian_S(sV1,sS1,MAX);
    QLA_V_veq_gaussian_S(sV2,sS1,MAX);
    QLA_V_veq_gaussian_S(sV3,sS1,MAX);
			  	 
    QLA_P_veq_gaussian_S(sP1,sS1,MAX);
    QLA_P_veq_gaussian_S(sP2,sS1,MAX);
    QLA_P_veq_gaussian_S(sP3,sS1,MAX);
			  	 
    QLA_M_veq_gaussian_S(sM1,sS1,MAX);
    QLA_M_veq_gaussian_S(sM2,sS1,MAX);
    QLA_M_veq_gaussian_S(sM3,sS1,MAX);
  }

}'
