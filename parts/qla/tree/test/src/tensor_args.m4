rem(`
     Function arguments for various types
     (Include file for protocol_tensor_sng.m4 and protocol_idx.m4)
')
define(opA,`ifelse($1,a,`',`a')')

define(argd, dest$1)
define(argt, chk$1)

define(argdQ, dest$1Q)
define(argtQ, chk$1Q)

define(argdD, dest$1D)
define(argtD, chk$1D)

define(argdF, dest$1F)
define(argtF, chk$1F)

define(argdP, dest$1P)
define(argdPF, `argdD($1)')
define(argdPD, `argdQ($1)')
define(argdPQ, `argdQ($1)')
define(argtP, chk$1P)
define(argtPF, `argtD($1)')
define(argtPD, `argtQ($1)')
define(argtPQ, `argtQ($1)')

define(arg1, ``s''`substr($1,0,1)'1)
define(arg2, s$1``2'')
define(arg3, s$1``3'')
define(arg4, s$1``4'')
define(argx, $1)

define(arg1Q, s$1Q1)
define(arg1D, ``s''`substr($1,0,1)'D1)
define(arg1F, ``s''`substr($1,0,1)'F1)
define(arg2Q, s$1Q``2'')
define(arg2D, s$1D``2'')
define(arg2F, s$1F``2'')
define(arg3Q, s$1Q``3'')
define(arg3D, s$1D``3'')
define(arg3F, s$1F``3'')

define(idxd, d$1x)
define(idx1, ``s''`substr($1,0,1)'1x)
define(idx2, s$1``2''x)
define(idxx, $1x)

define(arg1p, ``s''`substr($1,0,1)'1p)
define(arg2p, s$1``2''p)
define(argtp, chk$1p)
define(argxp, $1p)

define(arg1Qp, s$1Q``1''p)
define(arg1Dp, ``s''`substr($1,0,1)'D1p)
define(arg1Fp, ``s''`substr($1,0,1)'F1p)

define(arg2Qp, s$1Q``2''p)
define(arg2Dp, s$1D``2''p)
define(arg2Fp, s$1F``2''p)

define(arg1n, s$1``1''n)
define(arg2n, s$1``2''n)

rem(`
     Iterators for tensor components
')
define(for_R_elem, `')
define(for_C_elem, `')
define(for_I_elem, `')
define(for_H_elem, `for(ic=0;ic<nc;ic++)for(is=0;is<ns/2;is++)')
define(for_D_elem, `for(ic=0;ic<nc;ic++)for(is=0;is<ns;is++)')
define(for_V_elem, `for(ic=0;ic<nc;ic++)')
define(for_P_elem, `for(ic=0;ic<nc;ic++)for(is=0;is<ns;is++)\
   for(jc=0;jc<nc;jc++)for(js=0;js<ns;js++)')
define(for_M_elem, `for(ic=0;ic<nc;ic++)for(jc=0;jc<nc;jc++)')

define(for_H_colorvec, `for(is=0;is<ns/2;is++)')
define(for_D_colorvec, `for(is=0;is<ns;is++)')
define(for_P_colorvec, `for(is=0;is<ns;is++)\
   for(jc=0;jc<nc;jc++)for(js=0;js<ns;js++)')
define(for_M_colorvec, `for(jc=0;jc<nc;jc++)')

define(for_P_diracvec, `for(jc=0;jc<nc;jc++)for(js=0;js<ns;js++)')

define(for_P_dot, `for(kc=0;kc<nc;kc++)for(ks=0;ks<ns;ks++)')
define(for_M_dot, `for(kc=0;kc<nc;kc++)')

define(for_M_colordot, `for(kc=0;kc<nc;kc++)')

rem(`
     Subscript lists
')
define(R_list, `')
define(C_list, `')
define(I_list, `')
define(H_list, `ic,is')
define(D_list, `ic,is')
define(V_list, `ic')
define(P_list, `ic,is,jc,js')
define(M_list, `ic,jc')

define(H_list_cvec, `is')
define(D_list_cvec, `is')
define(P_list_cvec, `is,jc,js')
define(M_list_cvec, `jc')

define(P_list_dvec, `jc,js')

rem(`
     Accessors
')
define(R_elem, `$1')
define(C_elem, `$1')
define(I_elem, `$1')
define(H_elem, `QLA_elem_H($1,ic,is)')
define(D_elem, `QLA_elem_D($1,ic,is)')
define(V_elem, `QLA_elem_V($1,ic)')
define(P_elem, `QLA_elem_P($1,ic,is,jc,js)')
define(M_elem, `QLA_elem_M($1,ic,jc)')

rem(`
     Accessors for transpose (also use "a" for convenience)
')
define(Mt_elem, `QLA_elem_M($1,jc,ic)')
define(Pt_elem, `QLA_elem_P($1,jc,js,ic,is)')
define(Ca_elem, `$1')
define(Ma_elem, `QLA_elem_M($1,jc,ic)')
define(Pa_elem, `QLA_elem_P($1,jc,js,ic,is)')

rem(`
     Accessors for multiplicands and multipliers
')
define(M_elem_mleft, `QLA_elem_M($1,ic,kc)')
define(M_elem_mright, `QLA_elem_M($1,kc,jc)')
define(P_elem_mleft, `QLA_elem_P($1,ic,is,kc,ks)')
define(P_elem_mright, `QLA_elem_P($1,kc,ks,jc,js)')

rem(`
     Accessors for multiplication by adjoint 
')
define(Ma_elem_mleft, `QLA_elem_M($1,kc,ic)')
define(Ma_elem_mright, `QLA_elem_M($1,jc,kc)')
define(Pa_elem_mleft, `QLA_elem_P($1,kc,ks,ic,is)')
define(Pa_elem_mright, `QLA_elem_P($1,jc,js,kc,ks)')

rem(`
     Accessors for right multiplication by gauge matrix 
')
define(V_elem_Mmright, `QLA_elem_V($1,kc)')
define(H_elem_Mmright, `QLA_elem_H($1,kc,is)')
define(D_elem_Mmright, `QLA_elem_D($1,kc,is)')
define(M_elem_Mmright, `QLA_elem_M($1,kc,jc)')
define(P_elem_Mmright, `QLA_elem_P($1,kc,is,jc,js)')
define(Pa_elem_Mmright, `QLA_elem_P($1,jc,js,kc,is)')

rem(`
     Accessors for left multiplication by gauge matrix 
')
define(M_elem_mleftM, `QLA_elem_M($1,ic,kc)')
define(P_elem_mleftM, `QLA_elem_P($1,ic,is,kc,js)')
define(Pa_elem_mleftM, `QLA_elem_P($1,kc,js,ic,is)')

rem(`
     Iterator for all tensors
')
rem(`alltensors(macro)')
define(alltensors,`
$1(H)
$1(D)
$1(V)
$1(P)
$1(M)
')

rem(`alltensors2(macro,eqop)')
define(alltensors2,`
$1(H,$2,H)
$1(D,$2,D)
$1(V,$2,V)
$1(P,$2,P)
$1(M,$2,M)
')

rem(`alltensors3(macro,eqop,tc)')
define(alltensors3,`
$1(H,$2,$3,H)
$1(D,$2,$3,D)
$1(V,$2,$3,V)
$1(P,$2,$3,P)
$1(M,$2,$3,M)
')

define(alleqops,`
$1eq$2
$1eqm$2
$1peq$2
$1meq$2
')

define(eqopeq,`=')
define(eqopeqm,`=-')
define(eqoppeq,`+=')
define(eqopmeq,`-=')
