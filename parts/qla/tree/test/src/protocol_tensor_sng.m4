define(rem,)
rem(`
----------------------------------------------------------------------
     Test protocol include file for single tensor routines
----------------------------------------------------------------------
')

include(tensor_args.m4)

define(lower,`translit($1,ABCDEFGHIJKLMNOPQRSTUVWXYZ,abcdefghijklmnopqrstuvwxyz)')

rem(`
     Create long double field
')
rem(`makeGaussianQ(t1,name)')
define(makeGaussianQ,`
  for_$1_elem{
    arg1Q(R) = (QLA_Q_Real)QLA_gaussian(&sS1)/3.;
    arg2Q(R) = (QLA_Q_Real)QLA_gaussian(&sS1)/3.;
    QLA_c_eq_r_plus_ir($1_elem($2),arg1Q(R),arg2Q(R));
  }
')

rem(`
     Random field generation
')
rem(`chkGaussian(t1)')
define(chkGaussian,`
  strcpy(name,"QLA_$1_eq_gaussian_S");
  QLA_S_eq_seed_i_I(&sS1,sI2,&sI3);
  QLA_$1_eq_gaussian_S(&argt($1),&sS1);
  QLA_seed_random(&sS1,sI2,sI3);
  for_$1_elem{
    arg1(R) = QLA_gaussian(&sS1);
    arg2(R) = QLA_gaussian(&sS1);
    QLA_c_eq_r_plus_ir($1_elem(argd($1)),arg1(R),arg2(R));
  }
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')

rem(`
     Assignment
')
rem(`chkAssign(td,eq)')
define(chkAssign,`
  strcpy(name,"QLA_$1_$2_$1");
  QLA_$1_eq_$1(&argd($1),&arg2($1));
  QLA_$1_$2_$1(&argd($1),&arg1($1));
  QLA_$1_eq_$1(&argt($1),&arg2($1));
  for_$1_elem{
    QLA_c_eq_c($1_elem(argt($1)),$1_elem(arg2($1)));
    QLA_c_$2_c($1_elem(argt($1)),$1_elem(arg1($1)));
  }
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')

rem(`
     Iterator for Assignment
')
rem(`chkEqop(td)')
define(chkEqop,`
chkAssign($1,eq)
chkAssign($1,peq)
chkAssign($1,eqm)
chkAssign($1,meq)
')

rem(`
     Precision conversion
')
rem(`chkAssignDF(td)')
define(chkAssignDF,`
  strcpy(name,"QLA_DF_$1_eq_$1");
  QLA_D_$1_eq_$1(&argdD($1),&arg2D($1));
  QLA_DF_$1_eq_$1(&argdD($1),&arg1F($1));
  QLA_D_$1_eq_$1(&argtD($1),&arg2D($1));
  for_$1_elem{
    QLA_c_eq_c($1_elem(argtD($1)),$1_elem(arg2D($1)));
    QLA_c_eq_c($1_elem(argtD($1)),$1_elem(arg1F($1)));
  }
  checkeqsngD$1$1(&argdD($1),&argtD($1),name,fp);
')

rem(`
     Precision conversion
')
rem(`chkAssignFD(td)')
define(chkAssignFD,`
  strcpy(name,"QLA_FD_$1_eq_$1");
  QLA_F_$1_eq_$1(&argdF($1),&arg2F($1));
  QLA_FD_$1_eq_$1(&argdF($1),&arg1D($1));
  QLA_F_$1_eq_$1(&argtF($1),&arg2F($1));
  for_$1_elem{
    QLA_c_eq_c($1_elem(argtF($1)),$1_elem(arg2F($1)));
    QLA_c_eq_c($1_elem(argtF($1)),$1_elem(arg1D($1)));
  }
  checkeqsngF$1$1(&argdF($1),&argtF($1),name,fp);
')

rem(`
     Precision conversion
')
rem(`chkAssignDQ(td)')
define(chkAssignDQ,`
  strcpy(name,"QLA_DQ_$1_eq_$1");
  QLA_D_$1_eq_$1(&argdD($1),&arg2D($1));
  QLA_DQ_$1_eq_$1(&argdD($1),&arg1Q($1));
  QLA_D_$1_eq_$1(&argtD($1),&arg2D($1));
  for_$1_elem{
    QLA_c_eq_c($1_elem(argtD($1)),$1_elem(arg2D($1)));
    QLA_c_eq_c($1_elem(argtD($1)),$1_elem(arg1Q($1)));
  }
  checkeqsngD$1$1(&argdD($1),&argtD($1),name,fp);
')

rem(`
     Hermitian conjugate
')
rem(`chkAssignHconj(t1,eqop)')
define(chkAssignHconj,`
  strcpy(name,"QLA_$1_$2_$1a");
  QLA_$1_eq_$1(&argd($1),&arg2($1));
  QLA_$1_$2_$1a(&argd($1),&arg1($1));
  QLA_$1_eq_$1(&argt($1),&arg2($1));
  for_$1_elem{
    QLA_c_$2_ca($1_elem(argt($1)),$1t_elem(arg1($1)));
  }
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')

rem(`
     Hermitian conjugate iterator
')
rem(`chkHconj(t1)')
define(chkHconj,`
chkAssignHconj($1,eq)
chkAssignHconj($1,peq)
chkAssignHconj($1,eqm)
chkAssignHconj($1,meq)
')

rem(`
     Transpose
')
rem(`chkAssignTranspose(td,eqop)')
define(chkAssignTranspose,`
  strcpy(name,"QLA_$1_$2_transpose_$1");
  QLA_$1_eq_$1(&argd($1),&arg2($1));
  QLA_$1_$2_transpose_$1(&argd($1),&arg1($1));
  QLA_$1_eq_$1(&argt($1),&arg2($1));
  for_$1_elem{
    QLA_c_$2_c($1_elem(argt($1)),$1t_elem(arg1($1)));
  }
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')

rem(`
     Transpose iterator
')
rem(`chkTranspose(td)')
define(chkTranspose,`
chkAssignTranspose($1,eq)
chkAssignTranspose($1,peq)
chkAssignTranspose($1,eqm)
chkAssignTranspose($1,meq)
')

rem(`
     Complex conjugation
')
rem(`chkAssignConj(td,eq)')
define(chkAssignConj,`
  strcpy(name,"QLA_$1_$2_conj_$1");
  QLA_$1_$2_conj_$1(&argd($1),&arg1($1));
  for_$1_elem{
    QLA_c_$2_ca($1_elem(argt($1)),$1_elem(arg1($1)));
  }
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')

rem(`
     Iterator for complex conjugation
')
rem(`chkConj(td)')
define(chkConj,`
chkAssignConj($1,eq)
chkAssignConj($1,peq)
chkAssignConj($1,eqm)
chkAssignConj($1,meq)
')

rem(`
     Local squared norm
')

rem(`chkLocalNorm2eqop(t1,op)')
define(chkLocalNorm2eqop,`
  strcpy(name,"QLA_R_$2_norm2_$1");
  argdP(R) = 0;
  for_$1_elem{
    argdP(R) += QLA_norm2_c($1_elem(arg1($1)));
  }
  argt(R) = arg1(R);
  argt(R) eqop$2 argdP(R);
  QLA_R_eq_R(&argd(R),&arg1(R));
  QLA_R_$2_norm2_$1(&argd(R),&arg1($1));
  checkeqsngRR(&argd(R),&argt(R),name,fp);
')

rem(`chkLocalNorm2(t1)')
define(chkLocalNorm2,`
chkLocalNorm2eqop($1,eq)
chkLocalNorm2eqop($1,peq)
chkLocalNorm2eqop($1,eqm)
chkLocalNorm2eqop($1,meq)
')

rem(`
     Extraction of element
')
rem(`chkExtractElem(td)')
define(chkExtractElem,`
  strcpy(name,"QLA_C_eq_elem_$1");
  for_$1_elem{
    QLA_C_eq_elem_$1(&argd(C),&arg1($1),$1_list);
    QLA_c_eq_c($1_elem(argt($1)),argd(C));
  }
  checkeqsng$1$1(&argt($1),&arg1($1),name,fp);
')

rem(`
     Insertion of element
')
rem(`chkInsertElem(td)')
define(chkInsertElem,`
  strcpy(name,"QLA_$1_eq_elem_C");
  QLA_$1_eq_$1(&argd($1),&arg2($1));
  QLA_$1_eq_$1(&argt($1),&arg1($1));
  for_$1_elem{
    QLA_c_eq_c(argd(C),$1_elem(arg1($1)));
    QLA_$1_eq_elem_C(&argd($1),&argd(C),$1_list);
  }
  checkeqsng$1$1(&argd($1),&arg1($1),name,fp);
')

rem(`
     Extraction of color vector
')
rem(`chkExtractColorvec(td)')
define(chkExtractColorvec,`
  strcpy(name,"QLA_V_eq_colorvec_$1");
  for_$1_colorvec{
    QLA_V_eq_colorvec_$1(&argd(V),&arg1($1),$1_list_cvec);
    for(ic=0;ic<nc;ic++){
      QLA_C_eq_elem_V(&argd(C),&argd(V),ic);
      QLA_c_eq_c($1_elem(argt($1)),argd(C));
    }
  }
  checkeqsng$1$1(&argt($1),&arg1($1),name,fp);
')

rem(`
     Insertion of color vector
')
rem(`chkInsertColorvec(td)')
define(chkInsertColorvec,`
  strcpy(name,"QLA_$1_eq_colorvec_V");
  QLA_$1_eq_$1(&argd($1),&arg2($1));
  QLA_$1_eq_$1(&argt($1),&arg1($1));
  for_$1_colorvec{
    for(ic=0;ic<nc;ic++){
      QLA_c_eq_c(argd(C),$1_elem(arg1($1)));
      QLA_V_eq_elem_C(&argd(V),&argd(C),ic);
      QLA_$1_eq_colorvec_V(&argd($1),&argd(V),$1_list_cvec);
    }
  }
  checkeqsng$1$1(&argd($1),&arg1($1),name,fp);
')

rem(`
     Extraction of Dirac vector
')
rem(`chkExtractDiracvec(td)')
define(chkExtractDiracvec,`
  strcpy(name,"QLA_D_eq_diracvec_$1");
  for_$1_diracvec{
    QLA_D_eq_diracvec_$1(&argd(D),&arg1($1),$1_list_dvec);
    for(ic=0;ic<nc;ic++)for(is=0;is<ns;is++){
      QLA_C_eq_elem_D(&argd(C),&argd(D),ic,is);
      QLA_c_eq_c($1_elem(argt($1)),argd(C));
    }
  }
  checkeqsng$1$1(&argt($1),&arg1($1),name,fp);
')

rem(`
     Insertion of Dirac vector
')
rem(`chkInsertDiracvec(td)')
define(chkInsertDiracvec,`
  strcpy(name,"QLA_$1_eq_diracvec_D");
  QLA_$1_eq_$1(&argd($1),&arg2($1));
  QLA_$1_eq_$1(&argt($1),&arg1($1));
  for_$1_diracvec{
    for(ic=0;ic<nc;ic++)for(is=0;is<ns;is++){
      QLA_c_eq_c(argd(C),$1_elem(arg1($1)));
      QLA_D_eq_elem_C(&argd(D),&argd(C),ic,is);
      QLA_$1_eq_diracvec_D(&argd($1),&argd(D),$1_list_dvec);
    }
  }
  checkeqsng$1$1(&argd($1),&arg1($1),name,fp);
')

rem(`
     Trace
')
rem(`chkRealtrace')
define(chkRealtrace,`
  strcpy(name,"QLA_R_eq_re_trace_M");
  QLA_R_eq_re_trace_M(&argd(R),&arg1(M));
  argt(R) = 0;
  for(ic=0;ic<nc;ic++)argt(R)+=QLA_real(QLA_elem_M(arg1(M),ic,ic));
  checkeqsngRR(&argt(R),&argd(R),name,fp);
')

rem(`chkImagtrace')
define(chkImagtrace,`
  strcpy(name,"QLA_R_eq_im_trace_M");
  QLA_R_eq_im_trace_M(&argd(R),&arg1(M));
  argt(R) = 0;
  for(ic=0;ic<nc;ic++)argt(R)+=QLA_imag(QLA_elem_M(arg1(M),ic,ic));
  checkeqsngRR(&argt(R),&argd(R),name,fp);
')

rem(`chkTrace')
define(chkTrace,`
  strcpy(name,"QLA_C_eq_trace_M");
  QLA_C_eq_trace_M(&argd(C),&arg1(M));
  QLA_c_eq_r(argt(C),0.);
  for(ic=0;ic<nc;ic++)QLA_c_peq_c(argt(C),QLA_elem_M(arg1(M),ic,ic));
  checkeqsngCC(&argt(C),&argd(C),name,fp);
')

rem(`
     Spin trace
')
rem(`chkSpintrace')
define(chkSpintrace,`
  strcpy(name,"QLA_M_eq_spintrace_P");
  QLA_M_eq_spintrace_P(&argd(M),&arg1(P));
  for(ic=0;ic<nc;ic++)for(jc=0;jc<nc;jc++){
    QLA_c_eq_r(QLA_elem_M(argt(M),ic,jc),0.);
    for(is=0;is<ns;is++)
       QLA_c_peq_c(QLA_elem_M(argt(M),ic,jc),
         QLA_elem_P(arg1(P),ic,is,jc,is));
  }
  checkeqsngMM(&argt(M),&argd(M),name,fp);
')

rem(`
     Antihermitian projection
')
rem(`chkAntiherm')
define(chkAntiherm,`
  strcpy(name,"QLA_M_eq_antiherm_M");
  QLA_M_eq_antiherm_M(&argd(M),&arg1(M));
  for_M_elem{
    QLA_c_eq_c_minus_ca(QLA_elem_M(argt(M),ic,jc),QLA_elem_M(arg1(M),ic,jc),
           QLA_elem_M(arg1(M),jc,ic));
    QLA_c_eq_r_times_c(QLA_elem_M(argt(M),ic,jc),0.5,
           QLA_elem_M(argt(M),ic,jc));
  }
  QLA_R_eq_im_trace_M(&argt(R),&argt(M));
  for(ic=0;ic<nc;ic++)QLA_c_meq_r_plus_ir(QLA_elem_M(argt(M),ic,ic),0,argt(R)/nc);
  checkeqsngMM(&argd(M),&argt(M),name,fp);
')

rem(`
     Matrix determinant
')
rem(`chkMatDet')
define(chkMatDet,`
  strcpy(name,"QLA_C_eq_det_M");
  QLA_c_eq_r(argt(C), 1);
  for(ic=0;ic<nc;ic++) {
    QLA_c_eq_c(argd(C), argt(C));
    QLA_c_eq_c_times_c(argt(C), argd(C), QLA_elem_M(arg1(M),ic,ic));
  }
  QLA_M_eq_zero(&arg2(M));
  for(ic=0;ic<nc;ic++) QLA_c_eq_c(QLA_elem_M(arg2(M),ic,ic),QLA_elem_M(arg1(M),ic,ic));
  QLA_M_eq_inverse_M(&arg3(M),&arg1(M));
  QLA_M_eq_M_times_M(&argd(M),&arg2(M),&arg1(M));
  QLA_M_eq_M_times_M(&arg2(M),&arg3(M),&argd(M));
  QLA_C_eq_det_M(&argd(C),&arg2(M));
  checkeqsngCC(&argd(C),&argt(C),name,fp);
  QLA_M_eq_gaussian_S(&arg2(M),&sS1);
  QLA_M_eq_gaussian_S(&arg3(M),&sS1);
')

rem(`
     Matrix eigenvalues
')
rem(`chkMatEigenvals({""|"H"})')
define(chkMatEigenvals,`
  strcpy(name,"QLA_V_eq_eigenvals$1_M");
  for(ic=0; ic<nc; ic++) {  // set eigenvalues
    ifelse($1,`H',
    `QLA_c_eq_r(QLA_elem_V(argt(V),ic), QLA_real(QLA_elem_M(arg1(M),ic,ic)));',
    `QLA_c_eq_c(QLA_elem_V(argt(V),ic), QLA_elem_M(arg1(M),ic,ic));')
    //QLA_c_eq_c(QLA_elem_V(argt(V),ic), QLA_elem_M(arg1(M),0,0));
  }
  for(ic=0; ic<nc; ic++) {  // sort
    QLA_Real t = QLA_norm2_c(QLA_elem_V(argt(V),ic));
    int imin = ic;
    for(jc=ic+1; jc<nc; jc++) {
      QLA_Real t2 = QLA_norm2_c(QLA_elem_V(argt(V),jc));
      if(t2<t) { t = t2; imin = jc; }
    }
    if(imin!=ic) {
      QLA_Complex tc;
      QLA_c_eq_c(tc, QLA_elem_V(argt(V),ic));
      QLA_c_eq_c(QLA_elem_V(argt(V),ic), QLA_elem_V(argt(V),imin));
      QLA_c_eq_c(QLA_elem_V(argt(V),imin), tc);
    }
  }
  QLA_M_eq_zero(&arg2(M));
  for(ic=0; ic<nc; ic++) {
    QLA_c_eq_c(QLA_elem_M(arg2(M),ic,ic), QLA_elem_V(argt(V),ic));
  }
  QLA_M_eq_inverse_M(&arg3(M),&arg1(M));
  QLA_M_eq_M_times_M(&argd(M),&arg2(M),&arg1(M));
  QLA_M_eq_M_times_M(&arg2(M),&arg3(M),&argd(M));
  QLA_V_eq_eigenvals$1_M(&argd(V),&arg2(M));
  for(ic=0; ic<nc; ic++) {  // sort
    QLA_Real t = QLA_norm2_c(QLA_elem_V(argd(V),ic));
    int imin = ic;
    for(jc=ic+1; jc<nc; jc++) {
      QLA_Real t2 = QLA_norm2_c(QLA_elem_V(argd(V),jc));
      if(t2<t) { t = t2; imin = jc; }
    }
    if(imin!=ic) {
      QLA_Complex tc;
      QLA_c_eq_c(tc, QLA_elem_V(argd(V),ic));
      QLA_c_eq_c(QLA_elem_V(argd(V),ic), QLA_elem_V(argd(V),imin));
      QLA_c_eq_c(QLA_elem_V(argd(V),imin), tc);
    }
  }
//  for(ic=0; ic<nc; ic++) {
//#define C(x) QLA_real(x), QLA_imag(x)
//    printf("(%g,%g)  (%g,%g)\n",
//      C(QLA_elem_V(argd(V),ic)), C(QLA_elem_V(argt(V),ic)));
//#undef C
//  }
  checkeqsngVV(&argd(V),&argt(V),name,fp);
  QLA_M_eq_gaussian_S(&arg2(M),&sS1);
  QLA_M_eq_gaussian_S(&arg3(M),&sS1);
')

rem(`
     Matrix inverse
')
rem(`chkMatInverse')
define(chkMatInverse,`
  strcpy(name,"QLA_M_eq_inverse_M");
  QLA_M_eq_inverse_M(&argd(M),&arg1(M));
  QLA_M_eq_M_times_M(&argt(M),&argd(M),&arg1(M));
  QLA_c_eq_r(sC1, 1);
  QLA_M_eq_c(&argd(M),&sC1);
  checkeqsngMM(&argd(M),&argt(M),name,fp);
  QLA_M_eq_inverse_M(&argd(M),&arg1(M));
  QLA_M_eq_M_times_M(&argt(M),&arg1(M),&argd(M));
  QLA_c_eq_r(sC1, 1);
  QLA_M_eq_c(&argd(M),&sC1);
  checkeqsngMM(&argd(M),&argt(M),name,fp);
')

rem(`
     Matrix square root
')
rem(`chkMatSqrt({""|"PH"})')
define(chkMatSqrt,`
  strcpy(name,"QLA_M_eq_sqrt$1_M");
  //QLA_M_eq_sqrt_M(&argd(M),&arg1(M));
  //QLA_M_eq_M_times_M(&argt(M),&argd(M),&argd(M));
  //checkeqsngMM(&arg1(M),&argt(M),name,fp);
  QLA_M_eq_zero(&arg2(M));
  for(ic=0; ic<nc; ic++) {  // set matrix with desired eigenvalues
    QLA_Complex t;
    QLA_c_eq_c(t, QLA_elem_M(arg1(M),ic,ic));
    ifelse($1,`PH',
    `QLA_c_eq_r(t, fabs(QLA_real(t)));',
    `if(QLA_real(t)>0) QLA_c_eqm_c(t, t);')
    QLA_c_eq_c(QLA_elem_M(arg2(M),ic,ic), t);
  }
  QLA_M_eq_inverse_M(&arg3(M),&arg1(M));
  QLA_M_eq_M_times_M(&argd(M),&arg2(M),&arg1(M));
  QLA_M_eq_M_times_M(&argt(M),&arg3(M),&argd(M));
  QLA_M_eq_sqrt$1_M(&argd(M),&argt(M));
  for(ic=0; ic<nc; ic++) {
    QLA_Complex t;
    QLA_c_eq_c(t, QLA_elem_M(arg2(M),ic,ic));
    QLA_C_eq_csqrt_C(&QLA_elem_M(arg2(M),ic,ic), &t);
  }
  QLA_M_eq_M_times_M(&argt(M),&arg2(M),&arg1(M));
  QLA_M_eq_M_times_M(&arg2(M),&arg3(M),&argt(M));
  checkeqsngMM(&argd(M),&arg2(M),name,fp);
  QLA_M_eq_gaussian_S(&arg2(M),&sS1);
  QLA_M_eq_gaussian_S(&arg3(M),&sS1);
')

rem(`
     Matrix inverse square root
')
rem(`chkMatInvsqrt({""|"PH"})')
define(chkMatInvsqrt,`
  strcpy(name,"QLA_M_eq_invsqrt$1_M");
  //QLA_M_eq_invsqrt_M(&argd(M),&arg1(M));
  //QLA_M_eq_M_times_M(&argt(M),&argd(M),&argd(M));
  //QLA_M_eq_inverse_M(&arg2(M),&arg1(M));
  //checkeqsngMM(&arg2(M),&argt(M),name,fp);
  QLA_M_eq_zero(&arg2(M));
  for(ic=0; ic<nc; ic++) {
    QLA_Complex t;
    QLA_c_eq_c(t, QLA_elem_M(arg1(M),ic,ic));
    ifelse($1,`PH',
    `QLA_c_eq_r(t, fabs(QLA_real(t)));',
    `if(QLA_real(t)>0) QLA_c_eqm_c(t, t);')
    QLA_c_eq_c(QLA_elem_M(arg2(M),ic,ic), t);
  }
  QLA_M_eq_inverse_M(&arg3(M),&arg1(M));
  QLA_M_eq_M_times_M(&argd(M),&arg2(M),&arg1(M));
  QLA_M_eq_M_times_M(&argt(M),&arg3(M),&argd(M));
  QLA_M_eq_invsqrt$1_M(&argd(M),&argt(M));
  for(ic=0; ic<nc; ic++) {
    QLA_Complex t, t2;
    QLA_c_eq_c(t, QLA_elem_M(arg2(M),ic,ic));
    QLA_C_eq_csqrt_C(&t2, &t);
    QLA_c_eq_r_div_c(QLA_elem_M(arg2(M),ic,ic), 1, t2);
  }
  QLA_M_eq_M_times_M(&argt(M),&arg2(M),&arg1(M));
  QLA_M_eq_M_times_M(&arg2(M),&arg3(M),&argt(M));
  checkeqsngMM(&argd(M),&arg2(M),name,fp);
  QLA_M_eq_gaussian_S(&arg2(M),&sS1);
  QLA_M_eq_gaussian_S(&arg3(M),&sS1);
')

define(printMat,`
  for(ic=0;ic<nc;ic++) {
    for(jc=0;jc<nc;jc++) {
      printf("%i %i %g %g\n", QLA_real(QLA_elem_M($1,ic,jc)), QLA_imag(QLA_elem_M($1,ic,jc)));
    }
  }
')

rem(`
     Matrix exponential
')
rem(`chkMatExp({""|"A"|"TA"})')
define(chkMatExp,`
  strcpy(name,"QLA_M_eq_exp$1_M");
  //QLA_M_eq_zero(&arg1(M));
  //for(ic=0;ic<nc;ic++) QLA_c_eq_r_plus_ir(QLA_elem_M(arg1(M),ic,ic),0,1);
  //for(ic=0;ic<nc;ic++) for(jc=0;jc<nc;jc++) if(ic!=jc) QLA_c_eq_r(QLA_elem_M(arg1(M),ic,jc),0);
  QLA_M_eq_zero(&arg2(M));
  for(ic=0;ic<nc;ic++) {
    QLA_Complex t;
    ifelse($1,`',`QLA_c_eq_c(t, QLA_elem_M(arg1(M),ic,ic));')
    ifelse($1,`A',`QLA_c_eq_r_plus_ir(t, 0, QLA_real(QLA_elem_M(arg1(M),ic,ic)));')
    ifelse($1,`TA',`QLA_c_eq_r_plus_ir(t, 0, QLA_real(QLA_elem_M(arg1(M),ic,ic)));')
    ifelse($1,`iH',`QLA_c_eq_r_plus_ir(t, QLA_real(QLA_elem_M(arg1(M),ic,ic)), 0);')
    QLA_c_eq_c(QLA_elem_M(arg2(M),ic,ic), t);
  }
  ifelse($1,`TA',`QLA_M_eq_antiherm_M(&arg3(M), &arg2(M));
                  QLA_M_eq_M(&arg2(M), &arg3(M));')
  QLA_M_eq_inverse_M(&arg3(M),&arg1(M));
  QLA_M_eq_M_times_M(&argd(M),&arg2(M),&arg1(M));
  QLA_M_eq_M_times_M(&argt(M),&arg3(M),&argd(M));
  QLA_M_eq_exp$1_M(&argd(M),&argt(M));
  for(ic=0;ic<nc;ic++) {
    QLA_Complex t, t2;
    QLA_c_eq_c(t, QLA_elem_M(arg2(M),ic,ic));
    QLA_c_eq_c(t2, t);
    ifelse($1,`iH',`QLA_c_eq_r_plus_ir(t2, -QLA_imag(t), QLA_real(t));')
    QLA_C_eq_cexp_C(&QLA_elem_M(arg2(M),ic,ic), &t2);
  }
  QLA_M_eq_M_times_M(&argt(M),&arg2(M),&arg1(M));
  QLA_M_eq_M_times_M(&arg2(M),&arg3(M),&argt(M));
  checkeqsngMM(&argd(M),&arg2(M),name,fp);
  QLA_M_eq_gaussian_S(&arg2(M),&sS1);
  QLA_M_eq_gaussian_S(&arg3(M),&sS1);
')

rem(`
     Matrix log
')
rem(`chkMatLog')
define(chkMatLog,`
  strcpy(name,"QLA_M_eq_log_M");
  //QLA_M_eq_zero(&arg1(M));
  //for(ic=0;ic<nc;ic++) QLA_c_eq_r_plus_ir(QLA_elem_M(arg1(M),ic,ic),1,0);
  //for(ic=0;ic<nc;ic++) for(jc=0;jc<nc;jc++) if(ic!=jc) QLA_c_eq_r(QLA_elem_M(arg1(M),ic,jc),0);
  QLA_M_eq_zero(&arg2(M));
  for(ic=0; ic<nc; ic++) {
    QLA_Complex t;
    QLA_c_eq_c(t, QLA_elem_M(arg1(M),ic,ic));
    if(QLA_real(t)>0) QLA_c_eqm_c(t, t);
    QLA_c_eq_c(QLA_elem_M(arg2(M),ic,ic), t);
  }
  QLA_M_eq_inverse_M(&arg3(M),&arg1(M));
  QLA_M_eq_M_times_M(&argd(M),&arg2(M),&arg1(M));
  QLA_M_eq_M_times_M(&arg2(M),&arg3(M),&argd(M));
  QLA_M_eq_log_M(&argd(M),&arg2(M));
  QLA_M_eq_zero(&arg2(M));
  for(ic=0; ic<nc; ic++) {
    QLA_Complex t;
    QLA_c_eq_c(t, QLA_elem_M(arg1(M),ic,ic));
    if(QLA_real(t)>0) QLA_c_eqm_c(t, t);
    QLA_C_eq_clog_C(&QLA_elem_M(arg2(M),ic,ic), &t);
  }
  QLA_M_eq_M_times_M(&argt(M),&arg2(M),&arg1(M));
  QLA_M_eq_M_times_M(&arg2(M),&arg3(M),&argt(M));
  checkeqsngMM(&argd(M),&arg2(M),name,fp);
  QLA_M_eq_gaussian_S(&arg2(M),&sS1);
  QLA_M_eq_gaussian_S(&arg3(M),&sS1);
')

rem(`
     Spin projection
')
rem(`chkSpproj(dt,eq)')
define(chkSpproj,`
  /* Implementation dependent! */
  strcpy(name,"QLA_$1_$2_spproj_D");
  for(mu=0;mu<5;mu++)for(sign=-1;sign<2;sign+=2){
    wp_shrink(&argd(H),&arg1(D),mu,sign);
    ifelse($1,`D',`wp_grow(&argd(D),&argd(H),mu,sign);')
    QLA_$1_eq_$1(&argt($1),&arg2($1));
    QLA_$1_$2_$1(&argt($1),&argd($1));
    QLA_$1_eq_$1(&argd($1),&arg2($1));
    QLA_$1_$2_spproj_D(&argd($1),&arg1(D),mu,sign);
    checkeqsng$1$1(&argd($1),&argt($1),name,fp);
  }
')

rem(`
     Spin reconstruction
')
rem(`chkSprecon(eq)')
define(chkSprecon,`
  /* Implementation dependent! */
  strcpy(name,"QLA_D_$1_sprecon_H");
  for(mu=0;mu<5;mu++)for(sign=-1;sign<2;sign+=2){
    QLA_D_eq_D(&argt(D),&arg2(D));
    wp_grow(&argd(D),&arg1(H),mu,sign);
    QLA_D_$1_D(&argt(D),&argd(D));
    QLA_D_eq_D(&argd(D),&arg2(D));
    QLA_D_$1_sprecon_H(&argd(D),&arg1(H),mu,sign);
    checkeqsngDD(&argd(D),&argt(D),name,fp);
  }
')

rem(`
     Spin projection and matrix multiply
')
rem(`chkSpprojMult(dt,eq,adj)')
define(chkSpprojMult,`
  /* Implementation dependent! */
  strcpy(name,"QLA_$1_$2_spproj_M$3_times_D");
  for(mu=0;mu<5;mu++)for(sign=-1;sign<2;sign+=2){
    wp_shrink(&argd(H),&arg1(D),mu,sign);
    ifelse($1,`D',`wp_grow(&argd(D),&argd(H),mu,sign);')
    QLA_$1_eq_$1(&argt($1),&arg2($1));
    QLA_$1_$2_M$3_times_$1(&argt($1),&arg1(M),&argd($1));
    QLA_$1_eq_$1(&argd($1),&arg2($1));
    QLA_$1_$2_spproj_M$3_times_D(&argd($1),&arg1(M),&arg1(D),mu,sign);
    checkeqsng$1$1(&argd($1),&argt($1),name,fp);
  }
')

rem(`
     Spin reconstruction and matrix multiply
')
rem(`chkSpreconMult(eq,adj)')
define(chkSpreconMult,`
  /* Implementation dependent! */
  strcpy(name,"QLA_D_$1_sprecon_M$2_times_H");
  for(mu=0;mu<5;mu++)for(sign=-1;sign<2;sign+=2){
    wp_grow(&argd(D),&arg1(H),mu,sign);
    QLA_D_eq_D(&argt(D),&arg2(D));
    QLA_D_$1_M$2_times_D(&argt(D),&arg1(M),&argd(D));
    QLA_D_eq_D(&argd(D),&arg2(D));
    QLA_D_$1_sprecon_M$2_times_H(&argd(D),&arg1(M),&arg1(H),mu,sign);
    checkeqsngDD(&argd(D),&argt(D),name,fp);
  }
')

rem(`
     Gamma matrix multiplication
')
rem(`chkGammamult')
define(chkGammamult,`
  /* Implementation dependent! */
  strcpy(name,"QLA_P_eq_gamma_times_P");
  for(mu=0;mu<16;mu++){
    QLA_P_eq_gamma_times_P(&argd(P),&arg1(P),mu);
    mult_by_gamma_left(&argt(P),&arg1(P),mu);
    checkeqsngPP(&argd(P),&argt(P),name,fp);
  }

  strcpy(name,"QLA_D_eq_gamma_times_D");
  for(mu=0;mu<16;mu++){
    QLA_D_eq_gamma_times_D(&argd(D),&arg1(D),mu);
    mult_wv_by_gamma_left(&argt(D),&arg1(D),mu);
    checkeqsngDD(&argd(D),&argt(D),name,fp);
  }

  strcpy(name,"QLA_P_eq_P_times_gamma");
  for(mu=0;mu<16;mu++){
    QLA_P_eq_P_times_gamma(&argd(P),&arg1(P),mu);
    mult_by_gamma_right(&argt(P),&arg1(P),mu);
    checkeqsngPP(&argd(P),&argt(P),name,fp);
  }
')

rem(`
     Multiplication by real constant
')
rem(`chkAssignrMult(t1,eq,a)')
define(chkAssignrMult,`
  strcpy(name,"QLA_$1_$2_r_times_$1$3");
  QLA_$1_eq_$1(&argd($1),&arg2($1));
  QLA_$1_$2_r_times_$1$3(&argd($1),&sR1,&arg1($1));
  QLA_$1_eq_$1(&argt($1),&arg2($1));
  for_$1_elem{
    QLA_c_$2_r_times_c$3($1_elem(argt($1)),sR1,$1$3_elem(arg1($1)));
  }
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')
rem(`chkrMult(td)')
define(chkrMult,`
chkAssignrMult($1,eq)
chkAssignrMult($1,peq)
chkAssignrMult($1,eqm)
chkAssignrMult($1,meq)
')
rem(`chkrMultAdj(td)')
define(chkrMultAdj,`
chkAssignrMult($1,eq,a)
chkAssignrMult($1,peq,a)
chkAssignrMult($1,eqm,a)
chkAssignrMult($1,meq,a)
')

rem(`
     Multiplication by complex constant
')
rem(`chkAssigncMult(t1,eq,a)')
define(chkAssigncMult,`
  strcpy(name,"QLA_$1_$2_c_times_$1$3");
  QLA_$1_eq_$1(&argd($1),&arg2($1));
  QLA_$1_$2_c_times_$1$3(&argd($1),&sC1,&arg1($1));
  QLA_$1_eq_$1(&argt($1),&arg2($1));
  for_$1_elem{
    QLA_c_$2_c_times_c$3($1_elem(argt($1)),sC1,$1$3_elem(arg1($1)));
  }
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')
rem(`chkcMult(td)')
define(chkcMult,`
chkAssigncMult($1,eq)
chkAssigncMult($1,peq)
chkAssigncMult($1,eqm)
chkAssigncMult($1,meq)
')
rem(`chkcMultAdj(td)')
define(chkcMultAdj,`
chkAssigncMult($1,eq,a)
chkAssigncMult($1,peq,a)
chkAssigncMult($1,eqm,a)
chkAssigncMult($1,meq,a)
')

rem(`
     Multiplication by i
')
rem(`chkAssigniMult(t1,eq)')
define(chkAssigniMult,`
  strcpy(name,"QLA_$1_$2_i_$1");
  QLA_$1_eq_$1(&argd($1),&arg2($1));
  QLA_$1_$2_i_$1(&argd($1),&arg1($1));
  QLA_$1_eq_$1(&argt($1),&arg2($1));
  for_$1_elem{
    QLA_c_$2_ic($1_elem(argt($1)),$1_elem(arg1($1)));
  }
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')

rem(`chkiMult(td)')
define(chkiMult,`
chkAssigniMult($1,eq)
chkAssigniMult($1,peq)
chkAssigniMult($1,eqm)
chkAssigniMult($1,meq)
')

rem(`
     Addition
')
rem(`chkPlus(t1)')
define(chkPlus,`
  strcpy(name,"QLA_$1_eq_$1_plus_$1");
  QLA_$1_eq_$1_plus_$1(&argd($1),&arg1($1),&arg2($1));
  for_$1_elem{
    QLA_c_eq_c_plus_c($1_elem(argt($1)),$1_elem(arg1($1)),$1_elem(arg2($1)));
  }
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')

rem(`
     Subraction
')
rem(`chkMinus(t1)')
define(chkMinus,`
  strcpy(name,"QLA_$1_eq_$1_minus_$1");
  QLA_$1_eq_$1_minus_$1(&argd($1),&arg1($1),&arg2($1));
  for_$1_elem{
    QLA_c_eq_c_minus_c($1_elem(argt($1)),$1_elem(arg1($1)),$1_elem(arg2($1)));
  }
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')

rem(`
     Multiplication by Real and Complex fields
')
rem(`chkAssignRCMult(t1,rc,eq,a1,a2)')
define(chkAssignRCMult,`
  strcpy(name,"QLA_$1_$3_$2$4_times_$1$5");
  QLA_$1_eq_$1(&argd($1),&arg2($1));
  QLA_$1_$3_$2$4_times_$1$5(&argd($1),&arg1($2),&arg2($1));
  QLA_$1_eq_$1(&argt($1),&arg2($1));
  for_$1_elem{
    QLA_c_$3_`'lower($2)$4_times_c$5($1_elem(argt($1)),$2$4_elem(arg1($2)),$1$5_elem(arg2($1)));
  }
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')
rem(`chkRCMult(td)')
define(chkRCMult,`
chkAssignRCMult($1,R,eq)
chkAssignRCMult($1,R,eqm)
chkAssignRCMult($1,R,peq)
chkAssignRCMult($1,R,meq)
chkAssignRCMult($1,C,eq)
chkAssignRCMult($1,C,eqm)
chkAssignRCMult($1,C,peq)
chkAssignRCMult($1,C,meq)
chkAssignRCMult($1,C,eq,a)
chkAssignRCMult($1,C,eqm,a)
chkAssignRCMult($1,C,peq,a)
chkAssignRCMult($1,C,meq,a)
')
rem(`chkRCMultAdj(td)')
define(chkRCMultAdj,`
chkAssignRCMult($1,R,eq,,a)
chkAssignRCMult($1,R,eqm,,a)
chkAssignRCMult($1,R,peq,,a)
chkAssignRCMult($1,R,meq,,a)
chkAssignRCMult($1,C,eq,,a)
chkAssignRCMult($1,C,eqm,,a)
chkAssignRCMult($1,C,peq,,a)
chkAssignRCMult($1,C,meq,,a)
chkAssignRCMult($1,C,eq,a,a)
chkAssignRCMult($1,C,eqm,a,a)
chkAssignRCMult($1,C,peq,a,a)
chkAssignRCMult($1,C,meq,a,a)
')

rem(`
     Multiplication - uniform types
')
rem(`chkAssignUniformMult(t1,eq,a1,a2)')
define(chkAssignUniformMult,`
  strcpy(name,"QLA_$1_$2_$1$3_times_$1$4");
  QLA_$1_eq_$1(&argd($1),&arg2($1));
  QLA_$1_$2_$1$3_times_$1$4(&argd($1),&arg1($1),&arg2($1));
  QLA_$1_eq_$1(&argt($1),&arg2($1));
  for_$1_elem{
    QLA_c_eq_r(argt(C),0.);
    for_$1_dot{
      QLA_c_peq_c$3_times_c$4(argt(C),$1$3_elem_mleft(arg1($1)),
        $1$4_elem_mright(arg2($1)));
    }
   QLA_c_$2_c($1_elem(argt($1)),argt(C));
  }
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')
rem(`chkAssignUniformMultAdj(td,eq)')
define(chkAssignUniformMultAdj,`
chkAssignUniformMult($1,$2)
chkAssignUniformMult($1,$2,a)
chkAssignUniformMult($1,$2,,a)
chkAssignUniformMult($1,$2,a,a)
')
rem(`chkUniformMult(td)')
define(chkUniformMult,`
chkAssignUniformMultAdj($1,eq)
chkAssignUniformMultAdj($1,peq)
chkAssignUniformMultAdj($1,eqm)
chkAssignUniformMultAdj($1,meq)
')

rem(`
     Matrix inverse times field
')
rem(`chkMatInverseX')
define(chkMatInverseX,`
  strcpy(name,"QLA_$1_eq_M_inverse_$1");
  QLA_$1_eq_M_inverse_$1(&argd($1),&arg1(M),&arg2($1));
  QLA_$1_eq_M_times_$1(&argt($1),&arg1(M),&argd($1));
  checkeqsng$1$1(&argt($1),&arg2($1),name,fp);
')

rem(`
     Outer products
')
rem(`chkAssignOuterprod(td,eq)')
define(chkAssignOuterprod,`
  strcpy(name,"QLA_M_$2_V_times_Va");
  QLA_$1_eq_$1(&argd($1),&arg2($1));
  QLA_M_$2_V_times_Va(&argd(M),&arg1(V),&arg2(V));
  QLA_$1_eq_$1(&argt($1),&arg2($1));
  for_$1_elem{
    QLA_c_$2_c_times_ca($1_elem(argt($1)),QLA_elem_V(arg1(V),ic),QLA_elem_V(arg2(V),jc));
  }
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')
rem(`chkOuterprod')
define(chkOuterprod,`
chkAssignOuterprod(M,eq)
chkAssignOuterprod(M,peq)
chkAssignOuterprod(M,eqm)
chkAssignOuterprod(M,meq)
')

rem(`
     Local dot product
')
rem(`chkLocalDoteqop(t1,op,adj1,adj2)')
define(chkLocalDoteqop,`
  strcpy(name,"QLA_C_$2_$1$3_dot_$1$4");
  QLA_c_eq_r(argdP(C),0.);  
  for_$1_elem {
    QLA_c_peq_c`'opA($3)_times_c$4(argdP(C),$1$3_elem(arg1($1)),$1$4_elem(arg2($1)));
  }
  QLA_c_eq_c(argt(C), arg1(C));
  QLA_c_$2_c(argt(C), argdP(C));
  QLA_c_eq_c(argd(C), arg1(C));
  QLA_C_$2_$1$3_dot_$1$4(&argd(C),&arg1($1),&arg2($1));
  checkeqsngCC(&argd(C),&argt(C),name,fp);
')
rem(`chkLocalDot(t1,adj1,adj2)')
define(chkLocalDot,`
chkLocalDoteqop($1,eq,$2,$3)
chkLocalDoteqop($1,peq,$2,$3)
chkLocalDoteqop($1,eqm,$2,$3)
chkLocalDoteqop($1,meq,$2,$3)
')

rem(`
     Local dot product
')
rem(`chkLocalRealDoteqop(t1)')
define(chkLocalRealDoteqop,`
  strcpy(name,"QLA_R_$2_re_$1_dot_$1");
  argdP(R) = 0.;  
  for_$1_elem{
    QLA_r_peq_Re_ca_times_c(argdP(R),$1_elem(arg1($1)),$1_elem(arg2($1)));
  }
  argt(R) = arg1(R);
  argt(R) eqop$2 argdP(R);
  argd(R) = arg1(R);
  QLA_R_$2_re_$1_dot_$1(&argd(R),&arg1($1),&arg2($1));
  checkeqsngRR(&argd(R),&argt(R),name,fp);
')
rem(`chkLocalRealDot(t1)')
define(chkLocalRealDot,`
chkLocalRealDoteqop($1,eq)
chkLocalRealDoteqop($1,peq)
chkLocalRealDoteqop($1,eqm)
chkLocalRealDoteqop($1,meq)
')

rem(`
     Left multiplication by gauge matrix
')
rem(`chkAssignLeftMultM(t1,eq,adj)')
define(chkAssignLeftMultM,`
  strcpy(name,"QLA_$1_$2_M_times_$1$3");
  QLA_$1_eq_$1(&argd($1),&arg3($1));
  QLA_$1_$2_M_times_$1$3(&argd($1),&arg1(M),&arg2($1));
  QLA_$1_eq_$1(&argt($1),&arg3($1));
  for_$1_elem{
    QLA_c_eq_r(argt(C),0.);
    for_M_colordot{
      QLA_c_peq_c_times_c$3(argt(C),M_elem_mleft(arg1(M)),
        $1$3_elem_Mmright(arg2($1)));
    }
   QLA_c_$2_c($1_elem(argt($1)),argt(C));
  }
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')
rem(`chkLeftMultM(td,adj)')
define(chkLeftMultM,`
chkAssignLeftMultM($1,eq,$2)
chkAssignLeftMultM($1,peq,$2)
chkAssignLeftMultM($1,eqm,$2)
chkAssignLeftMultM($1,meq,$2)
')

rem(`
     Left multiplication by multiple gauge matrices
')
rem(`chkAssignLeftMultnM(t1,eq,adj)')
define(chkAssignLeftMultnM,`
  strcpy(name,"QLA_$1_$2_nM_times_n$1$3");
  QLA_$1_eq_$1(&argt($1),&arg3($1));
  QLA_$1_$2_M_times_$1$3(&argt($1),arg1n(M)[0],arg2n($1)[0]);
  for(int i=1; i<ND; i++) {
    QLA_$1_eq_zero(&argd($1));
    QLA_$1_$2_M_times_$1$3(&argd($1),arg1n(M)[i],arg2n($1)[i]);
    QLA_$1_peq_$1(&argt($1),&argd($1));
  }
  QLA_$1_eq_$1(&argd($1),&arg3($1));
  QLA_$1_$2_nM_times_n$1$3(&argd($1),arg1n(M),arg2n($1),ND);
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')
rem(`chkLeftMultnM(td,adj)')
define(chkLeftMultnM,`
chkAssignLeftMultnM($1,eq,$2)
chkAssignLeftMultnM($1,peq,$2)
chkAssignLeftMultnM($1,eqm,$2)
chkAssignLeftMultnM($1,meq,$2)
')

rem(`
     Left multiplication by adjoint gauge
')
rem(`chkAssignLeftMultMa(t1,eq,adj)')
define(chkAssignLeftMultMa,`
  strcpy(name,"QLA_$1_$2_Ma_times_$1$3");
  QLA_$1_eq_$1(&argd($1),&arg3($1));
  QLA_$1_$2_Ma_times_$1$3(&argd($1),&arg1(M),&arg2($1));
  QLA_$1_eq_$1(&argt($1),&arg3($1));
  for_$1_elem{
    QLA_c_eq_r(argt(C),0.);
    for_M_colordot{
      QLA_c_peq_ca_times_c$3(argt(C),Ma_elem_mleft(arg1(M)),
        $1$3_elem_Mmright(arg2($1)));
    }
   QLA_c_$2_c($1_elem(argt($1)),argt(C));
  }
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')
rem(`chkLeftMultMa(td,adj)')
define(chkLeftMultMa,`
chkAssignLeftMultMa($1,eq,$2)
chkAssignLeftMultMa($1,peq,$2)
chkAssignLeftMultMa($1,eqm,$2)
chkAssignLeftMultMa($1,meq,$2)
')

rem(`
     Left multiplication by multiple adjoint gauge matrices
')
rem(`chkAssignLeftMultnMa(t1,eq,adj)')
define(chkAssignLeftMultnMa,`
  strcpy(name,"QLA_$1_$2_nMa_times_n$1$3");
  QLA_$1_eq_$1(&argt($1),&arg3($1));
  QLA_$1_$2_Ma_times_$1$3(&argt($1),arg1n(M)[0],arg2n($1)[0]);
  for(int i=1; i<ND; i++) {
    QLA_$1_eq_zero(&argd($1));
    QLA_$1_$2_Ma_times_$1$3(&argd($1),arg1n(M)[i],arg2n($1)[i]);
    QLA_$1_peq_$1(&argt($1),&argd($1));
  }
  QLA_$1_eq_$1(&argd($1),&arg3($1));
  QLA_$1_$2_nMa_times_n$1$3(&argd($1),arg1n(M),arg2n($1),ND);
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')
rem(`chkLeftMultnMa(td,adj)')
define(chkLeftMultnMa,`
chkAssignLeftMultnMa($1,eq,$2)
chkAssignLeftMultnMa($1,peq,$2)
chkAssignLeftMultnMa($1,eqm,$2)
chkAssignLeftMultnMa($1,meq,$2)
')

rem(`
     Right multiplication by gauge
')
rem(`chkAssignRightMultM(t1,eq,adj)')
define(chkAssignRightMultM,`
  strcpy(name,"QLA_$1_$2_$1$3_times_M");
  QLA_$1_eq_$1(&argd($1),&arg3($1));
  QLA_$1_$2_$1$3_times_M(&argd($1),&arg1($1),&arg2(M));
  QLA_$1_eq_$1(&argt($1),&arg3($1));
  for_$1_elem{
    QLA_c_eq_r(argt(C),0.);
    for_M_colordot{
      QLA_c_peq_c$3_times_c(argt(C),$1$3_elem_mleftM(arg1($1)),
        M_elem_mright(arg2(M)));
    }
   QLA_c_$2_c($1_elem(argt($1)),argt(C));
  }
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')
rem(`chkRightMultM(td,adj)')
define(chkRightMultM,`
chkAssignRightMultM($1,eq,$2)
chkAssignRightMultM($1,peq,$2)
chkAssignRightMultM($1,eqm,$2)
chkAssignRightMultM($1,meq,$2)
')

rem(`
     Right multiplication by adjoint gauge
')
rem(`chkAssignRightMultMa(t1,eq,adj)')
define(chkAssignRightMultMa,`
  strcpy(name,"QLA_$1_$2_$1$3_times_Ma");
  QLA_$1_eq_$1(&argd($1),&arg3($1));
  QLA_$1_$2_$1$3_times_Ma(&argd($1),&arg1($1),&arg2(M));
  QLA_$1_eq_$1(&argt($1),&arg3($1));
  for_$1_elem{
    QLA_c_eq_r(argt(C),0.);
    for_M_colordot{
      QLA_c_peq_c$3_times_ca(argt(C),$1$3_elem_mleftM(arg1($1)),
        Ma_elem_mright(arg2(M)));
    }
   QLA_c_$2_c($1_elem(argt($1)),argt(C));
  }
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')
rem(`chkRightMultMa(td,adj)')
define(chkRightMultMa,`
chkAssignRightMultMa($1,eq,$2)
chkAssignRightMultMa($1,peq,$2)
chkAssignRightMultMa($1,eqm,$2)
chkAssignRightMultMa($1,meq,$2)
')

rem(`
     Ternary with real constant
')
rem(`chkrMultPM(t1,pm)')
define(chkrMultPM,`
  strcpy(name,"QLA_$1_eq_r_times_$1_$2_$1");
  QLA_$1_eq_r_times_$1_$2_$1(&argd($1),&sR1,&arg1($1),&arg2($1));
  for_$1_elem{
    QLA_c_eq_r_times_c_$2_c($1_elem(argt($1)),sR1,$1_elem(arg1($1)),$1_elem(arg2($1)));
  }
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')

rem(`chkrMultAdd(td)')
define(chkrMultAdd,`
chkrMultPM($1,plus)
chkrMultPM($1,minus)
')

rem(`
     Ternary with complex constant
')
rem(`chkcMultPM(t1,eq,pm)')
define(chkcMultPM,`
  strcpy(name,"QLA_$1_eq_c_times_$1_$2_$1");
  QLA_$1_eq_c_times_$1_$2_$1(&argd($1),&sC1,&arg1($1),&arg2($1));
  for_$1_elem{
    QLA_c_eq_c_times_c_$2_c($1_elem(argt($1)),sC1,$1_elem(arg1($1)),$1_elem(arg2($1)));
  }
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')

rem(`chkcMultAdd(td)')
define(chkcMultAdd,`
chkcMultPM($1,plus)
chkcMultPM($1,minus)
')

rem(`
     Copymask
')
rem(`chkMask(t1)')
define(chkMask,`
  strcpy(name,"QLA_$1_eq_$1_mask_I");
  QLA_$1_eq_$1(&argd($1),&arg2($1));
  QLA_$1_eq_$1_mask_I(&argd($1),&arg1($1),&arg1(I));
  if(arg1(I))checkeqsng$1$1(&argd($1),&arg1($1),name,fp);
  else checkeqsng$1$1(&argd($1),&arg2($1),name,fp);

  QLA_$1_eq_$1(&argd($1),&arg2($1));
  QLA_$1_eq_$1_mask_I(&argd($1),&arg1($1),&arg2(I));
  if(arg2(I))checkeqsng$1$1(&argd($1),&arg1($1),name,fp);
  else checkeqsng$1$1(&argd($1),&arg2($1),name,fp);
')

rem(`
     Squared norm
')
rem(`chkNorm2(t1)')
define(chkNorm2,`
  strcpy(name,"QLA_r_eq_norm2_$1");
  QLA_r_eq_norm2_$1(&argd(R),&arg1($1));
  argtQ(R) = 0;
  for_$1_elem{
    argtQ(R) += QLA_norm2_c($1_elem(arg1($1)));
  }
  argt(R) = argtQ(R);
  checkeqsngRR(&argd(R),&argt(R),name,fp);
')

rem(`
     Squared norm float to double
')
rem(`chkNorm2DF(t1)')
define(chkNorm2DF,`
  strcpy(name,"QLA_DF_r_eq_norm2_$1");
  QLA_DF_r_eq_norm2_$1(&argdD(R),&arg1F($1));
  argtD(R) = 0;
  for_$1_elem{
    argtD(R) += QLA_norm2_c($1_elem(arg1F($1)));
  }
  checkeqsngDRR(&argdD(R),&argtD(R),name,fp);
')

rem(`
     Squared norm double to long double
')
rem(`chkNorm2QD(t1)')
define(chkNorm2QD,`
  strcpy(name,"QLA_QD_r_eq_norm2_$1");
  QLA_QD_r_eq_norm2_$1(&argdQ(R),&arg1D($1));
  argtQ(R) = 0;
  for_$1_elem{
    argtQ(R) += QLA_norm2_c($1_elem(arg1D($1)));
  }
  checkeqsngQRR(&argdQ(R),&argtQ(R),name,fp);
')

rem(`
     Dot product
')
rem(`chkDot(t1,adj1)')
define(chkDot,`
  strcpy(name,"QLA_c_eq_$1$2_dot_$1");
  QLA_c_eq_$1$2_dot_$1(&argd(C),&arg1($1),&arg2($1));
  QLA_c_eq_r(argtQ(C),0.);  
  for_$1_elem{
    QLA_c_peq_c`'opA($2)_times_c(argtQ(C),$1$2_elem(arg1($1)),$1_elem(arg2($1)));
  }
  QLA_c_eq_c(argt(C), argtQ(C));
  checkeqsngCC(&argd(C),&argt(C),name,fp);
')

rem(`
     Real part of dot product
')
rem(`chkRealDot(t1)')
define(chkRealDot,`
  strcpy(name,"QLA_r_eq_re_$1_dot_$1");
  QLA_r_eq_re_$1_dot_$1(&argd(R),&arg1($1),&arg2($1));
  argtQ(R) =  0.;
  for_$1_elem{
    QLA_r_peq_Re_ca_times_c(argtQ(R),$1_elem(arg1($1)),$1_elem(arg2($1)));
  }
  argt(R) = argtQ(R);
  checkeqsngRR(&argd(R),&argt(R),name,fp);
')

rem(`
     Dot product float to double
')
rem(`chkDotDF(t1,adj1)')
define(chkDotDF,`
  strcpy(name,"QLA_DF_c_eq_$1$2_dot_$1");
  QLA_DF_c_eq_$1$2_dot_$1(&argdD(C),&arg1F($1),&arg2F($1));
  QLA_c_eq_r(argtD(C),0.);  
  for_$1_elem{
    QLA_c_peq_c`'opA($2)_times_c(argtD(C),$1$2_elem(arg1F($1)),$1_elem(arg2F($1)));
  }
  checkeqsngDCC(&argdD(C),&argtD(C),name,fp);
')

rem(`
     Real part of dot product float to double
')
rem(`chkRealDotDF(t1)')
define(chkRealDotDF,`
  strcpy(name,"QLA_DF_r_eq_re_$1_dot_$1");
  QLA_DF_r_eq_re_$1_dot_$1(&argdD(R),&arg1F($1),&arg2F($1));
  argtD(R) =  0.;  
  for_$1_elem{
    QLA_r_peq_Re_ca_times_c(argtD(R),$1_elem(arg1F($1)),$1_elem(arg2F($1)));
  }
  checkeqsngDRR(&argdD(R),&argtD(R),name,fp);
')

rem(`
     Dot product double to long double
')
rem(`chkDotQD(t1,adj1)')
define(chkDotQD,`
  strcpy(name,"QLA_QD_c_eq_$1$2_dot_$1");
  QLA_QD_c_eq_$1$2_dot_$1(&argdQ(C),&arg1D($1),&arg2D($1));
  QLA_c_eq_r(argtQ(C),0.);  
  for_$1_elem{
    QLA_c_peq_c`'opA($2)_times_c(argtQ(C),$1$2_elem(arg1D($1)),$1_elem(arg2D($1)));
  }
  checkeqsngQCC(&argdQ(C),&argtQ(C),name,fp);
')

rem(`
     Real part of dot product double to long double
')
rem(`chkRealDotQD(t1)')
define(chkRealDotQD,`
  strcpy(name,"QLA_QD_r_eq_re_$1_dot_$1");
  QLA_QD_r_eq_re_$1_dot_$1(&argdQ(R),&arg1D($1),&arg2D($1));
  argtQ(R) =  0.;  
  for_$1_elem{
    QLA_r_peq_Re_ca_times_c(argtQ(R),$1_elem(arg1D($1)),$1_elem(arg2D($1)));
  }
  checkeqsngQRR(&argdQ(R),&argtQ(R),name,fp);
')

rem(`
     Sum
')
rem(`chkSumDest(td,t1)')
define(chkSumDest,`
  strcpy(name,"QLA_$1_eq_sum_$2");
  QLA_$2_eq_$2(&argt($2),&arg1($2));
  QLA_$1_eq_sum_$2(&argd($2),&arg1($2));
  checkeqsng$2$2(&argd($2),&argt($2),name,fp);
')

rem(`chkSum(t1)')
define(chkSum,`
chkSumDest(lower($1),$1)
')

rem(`
     Sum float to double
')
rem(`chkSumDestDF(td,t1)')
define(chkSumDestDF,`
  strcpy(name,"QLA_DF_$1_eq_sum_$2");
  QLA_DF_$2_eq_$2(&argtD($2),&arg1F($2));
  QLA_DF_$1_eq_sum_$2(&argdD($2),&arg1F($2));
  checkeqsngD$2$2(&argdD($2),&argtD($2),name,fp);
')

rem(`chkSumDF(t1)')
define(chkSumDF,`
chkSumDestDF(lower($1),$1)
')

rem(`
     Sum double to long double
')

rem(`chkSumDestQD(td,t1)')
define(chkSumDestQD,`
  strcpy(name,"QLA_QD_$1_eq_sum_$2");
  for_$2_elem{
     QLA_c_eq_c($2_elem(argtQ($2)),$2_elem(arg1D($2)));
  }
  QLA_QD_$1_eq_sum_$2(&argdQ($2),&arg1D($2));
  checkeqsngQ$2$2(&argdQ($2),&argtQ($2),name,fp);
')

rem(`chkSumQD(t1)')
define(chkSumQD,`
chkSumDestQD(lower($1),$1)
')

rem(`
     Zero fill
')
rem(`chkZero(t1)')
define(chkZero,`
  strcpy(name,"QLA_$1_eq_zero");
  QLA_$1_eq_$1(&argd($1),&arg2($1));
  QLA_$1_eq_zero(&argd($1));
  for_$1_elem {
    QLA_c_eq_r($1_elem(argt($1)),0.);
  }
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')

rem(`
     Constant fill
')
rem(`chkConstDest(td,t1)')
define(chkConstDest,`
  strcpy(name,"QLA_$1_$2_$3");
  QLA_$1_eq_$1(&argd($1),&arg2($1));
  QLA_$1_$2_$3(&argd($1),&arg1($1));
  QLA_$1_eq_$1(&argt($1),&arg2($1));
  QLA_$1_$2_$1(&argt($1),&arg1($1));
  checkeqsng$1$1(&argd($1),&argt($1),name,fp);
')

rem(`chkConst(t1)')
define(chkConst,`
chkConstDest($1,eq,lower($1))
chkConstDest($1,peq,lower($1))
chkConstDest($1,eqm,lower($1))
chkConstDest($1,meq,lower($1))
')

rem(`
     Diagonal gauge constant fill
')
rem(`chkMConstEqop(eq)')
define(chkMConstEqop,`
  strcpy(name,"QLA_M_$1_c");
  QLA_M_eq_M(&argd(M),&arg2(M));
  QLA_M_$1_c(&argd(M),&arg1(C));
  QLA_M_eq_M(&argt(M),&arg2(M));
  for(ic=0;ic<nc;ic++)for(jc=0;jc<nc;jc++)
    QLA_c_$1_c(QLA_elem_M(argt(M),ic,jc),(ic==jc)?arg1(C):zeroC);
  checkeqsngMM(&argd(M),&argt(M),name,fp);
')

rem(`chkMConst')
define(chkMConst,`
chkMConstEqop(eq)
chkMConstEqop(peq)
chkMConstEqop(eqm)
chkMConstEqop(meq)
')
