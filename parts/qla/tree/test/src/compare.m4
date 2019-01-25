/* QLA test code */
/* for comparisons of all datatypes - single or indexed. */
/* C code generated by m4 from compare.m4 */

include(protocol_compare.m4)
`
#include <qla.h>
#include <math.h>
#include "compare.h"

double diff;
double checkeq1, checkeq2;
int nc = QLA_Nc;
int ns = 4;


int
report(int status, char *name, FILE *fp)
{
  if(status) {
    fprintf(fp, "FAIL %s %e %g %.20g %.20g\n",name,diff,((double)(TOLREL)),checkeq1,checkeq2);
  } else fprintf(fp, "OK %s\n",name);
  return status;
}
'
checkequalreal(I,)
checkequalreal(R,)
checkequalreal(R,Q)
checkequalreal(R,D)
checkequalreal(R,F)
checkequalcomplex(C,)
checkequalcomplex(C,Q)
checkequalcomplex(C,D)
checkequalcomplex(C,F)
`
int checkequalCRR(QLA_Complex *sa, QLA_Real *sbre, QLA_Real *sbim){
  int status;
  QLA_Real t;
  t = QLA_real(*sa);
  status = checkequalRR(&t, sbre);
  if(!status) {
    t = QLA_imag(*sa);
    status = checkequalRR(&t, sbim);
  }
  return status;
}
'
checkequaltensor(H,)
checkequaltensor(H,Q)
checkequaltensor(H,D)
checkequaltensor(H,F)

checkequaltensor(D,)
checkequaltensor(D,Q)
checkequaltensor(D,D)
checkequaltensor(D,F)

checkequaltensor(V,)
checkequaltensor(V,Q)
checkequaltensor(V,D)
checkequaltensor(V,F)

checkequaltensor(P,)
checkequaltensor(P,Q)
checkequaltensor(P,D)
checkequaltensor(P,F)

checkequaltensor(M,)
checkequaltensor(M,Q)
checkequaltensor(M,D)
checkequaltensor(M,F)

`
/* Comparing random number generator states is implementation dependent */
/* See qla_random.h for the definition of QLA_RandomState */
int checkequalSS(QLA_RandomState *sa, QLA_RandomState *sb){
  diff = 0;
  return (sa->r0 != sb->r0 ||
	  sa->r1 != sb->r1 ||
	  sa->r2 != sb->r2 ||
	  sa->r3 != sb->r3 ||
	  sa->r4 != sb->r4 ||
	  sa->r5 != sb->r5 ||
	  sa->r6 != sb->r6 ||
	  sa->multiplier != sb->multiplier ||
	  sa->addend != sb->addend ||
	  sa->ic_state != sb->ic_state ||
	  fabs(sa->scale - sb->scale) > TOLABS);
}
'

checkequalsng(I,)
checkequalsng(R,)
checkequalsng(R,F)
checkequalsng(R,D)
checkequalsng(R,Q)
checkequalsng(C,)
checkequalsng(C,F)
checkequalsng(C,D)
checkequalsng(C,Q)
`
int checkeqsngCRR(QLA_Complex *sa, QLA_Real *sbre, QLA_Real *sbim, char name[], FILE *fp){
  return report(checkequalCRR(sa,sbre,sbim),name, fp);
}
'
checkequalsng(H,)
checkequalsng(H,Q)
checkequalsng(H,D)
checkequalsng(H,F)

checkequalsng(D,)
checkequalsng(D,Q)
checkequalsng(D,D)
checkequalsng(D,F)

checkequalsng(V,)
checkequalsng(V,Q)
checkequalsng(V,D)
checkequalsng(V,F)

checkequalsng(P,)
checkequalsng(P,Q)
checkequalsng(P,D)
checkequalsng(P,F)

checkequalsng(M,)
checkequalsng(M,Q)
checkequalsng(M,D)
checkequalsng(M,F)

`
int checkeqsngSS(QLA_RandomState *sa, QLA_RandomState *sb, char name[], FILE *fp){
  return report(checkequalSS(sa,sb),name, fp);
}
'

checkequalidx(I,)

checkequalidx(R,)
checkequalidx(R,Q)
checkequalidx(R,D)
checkequalidx(R,F)

checkequalidx(C,)
checkequalidx(C,Q)
checkequalidx(C,D)
checkequalidx(C,F)

checkequalidx(H,)
checkequalidx(H,Q)
checkequalidx(H,D)
checkequalidx(H,F)
	              
checkequalidx(D,)
checkequalidx(D,Q)
checkequalidx(D,D)
checkequalidx(D,F)
	              
checkequalidx(V,)
checkequalidx(V,Q)
checkequalidx(V,D)
checkequalidx(V,F)
	              
checkequalidx(P,)
checkequalidx(P,Q)
checkequalidx(P,D)
checkequalidx(P,F)

checkequalidx(M,)
checkequalidx(M,Q)
checkequalidx(M,D)
checkequalidx(M,F)
`
int checkeqidxSS(QLA_RandomState a[], QLA_RandomState b[], char name[], FILE *fp){
  int i,status=0;
  for(i = 0; i < MAX; i++){
    if((status = checkequalSS(&a[i],&b[i])))break;
  }
  return report(status,name, fp);
}
'
