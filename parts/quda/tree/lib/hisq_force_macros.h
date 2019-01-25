#ifndef _HISQ_FORCE_MACROS_H_
#define _HISQ_FORCE_MACROS_H_


#ifndef HISQ_RECONSTRUCT_LINKS

#define LINK_W00_re LINK_W[0].x
#define LINK_W00_im LINK_W[0].y
#define LINK_W01_re LINK_W[1].x
#define LINK_W01_im LINK_W[1].y
#define LINK_W02_re LINK_W[2].x
#define LINK_W02_im LINK_W[2].y
#define LINK_W10_re LINK_W[3].x
#define LINK_W10_im LINK_W[3].y
#define LINK_W11_re LINK_W[4].x
#define LINK_W11_im LINK_W[4].y
#define LINK_W12_re LINK_W[5].x
#define LINK_W12_im LINK_W[5].y
#define LINK_W20_re LINK_W[6].x
#define LINK_W20_im LINK_W[6].y
#define LINK_W21_re LINK_W[7].x
#define LINK_W21_im LINK_W[7].y
#define LINK_W22_re LINK_W[8].x
#define LINK_W22_im LINK_W[8].y

#define LINK_X00_re LINK_X[0].x
#define LINK_X00_im LINK_X[0].y
#define LINK_X01_re LINK_X[1].x
#define LINK_X01_im LINK_X[1].y
#define LINK_X02_re LINK_X[2].x
#define LINK_X02_im LINK_X[2].y
#define LINK_X10_re LINK_X[3].x
#define LINK_X10_im LINK_X[3].y
#define LINK_X11_re LINK_X[4].x
#define LINK_X11_im LINK_X[4].y
#define LINK_X12_re LINK_X[5].x
#define LINK_X12_im LINK_X[5].y
#define LINK_X20_re LINK_X[6].x
#define LINK_X20_im LINK_X[6].y
#define LINK_X21_re LINK_X[7].x
#define LINK_X21_im LINK_X[7].y
#define LINK_X22_re LINK_X[8].x
#define LINK_X22_im LINK_X[8].y

#define LINK_Y00_re LINK_Y[0].x
#define LINK_Y00_im LINK_Y[0].y
#define LINK_Y01_re LINK_Y[1].x
#define LINK_Y01_im LINK_Y[1].y
#define LINK_Y02_re LINK_Y[2].x
#define LINK_Y02_im LINK_Y[2].y
#define LINK_Y10_re LINK_Y[3].x
#define LINK_Y10_im LINK_Y[3].y
#define LINK_Y11_re LINK_Y[4].x
#define LINK_Y11_im LINK_Y[4].y
#define LINK_Y12_re LINK_Y[5].x
#define LINK_Y12_im LINK_Y[5].y
#define LINK_Y20_re LINK_Y[6].x
#define LINK_Y20_im LINK_Y[6].y
#define LINK_Y21_re LINK_Y[7].x
#define LINK_Y21_im LINK_Y[7].y
#define LINK_Y22_re LINK_Y[8].x
#define LINK_Y22_im LINK_Y[8].y

#define LINK_Z00_re LINK_Z[0].x
#define LINK_Z00_im LINK_Z[0].y
#define LINK_Z01_re LINK_Z[1].x
#define LINK_Z01_im LINK_Z[1].y
#define LINK_Z02_re LINK_Z[2].x
#define LINK_Z02_im LINK_Z[2].y
#define LINK_Z10_re LINK_Z[3].x
#define LINK_Z10_im LINK_Z[3].y
#define LINK_Z11_re LINK_Z[4].x
#define LINK_Z11_im LINK_Z[4].y
#define LINK_Z12_re LINK_Z[5].x
#define LINK_Z12_im LINK_Z[5].y
#define LINK_Z20_re LINK_Z[6].x
#define LINK_Z20_im LINK_Z[6].y
#define LINK_Z21_re LINK_Z[7].x
#define LINK_Z21_im LINK_Z[7].y
#define LINK_Z22_re LINK_Z[8].x
#define LINK_Z22_im LINK_Z[8].y

#define ab_link00_re ab_link[0].x
#define ab_link00_im ab_link[0].y
#define ab_link01_re ab_link[1].x
#define ab_link01_im ab_link[1].y
#define ab_link02_re ab_link[2].x
#define ab_link02_im ab_link[2].y
#define ab_link10_re ab_link[3].x
#define ab_link10_im ab_link[3].y
#define ab_link11_re ab_link[4].x
#define ab_link11_im ab_link[4].y
#define ab_link12_re ab_link[5].x
#define ab_link12_im ab_link[5].y
#define ab_link20_re ab_link[6].x
#define ab_link20_im ab_link[6].y
#define ab_link21_re ab_link[7].x
#define ab_link21_im ab_link[7].y
#define ab_link22_re ab_link[8].x
#define ab_link22_im ab_link[8].y

#define bc_link00_re bc_link[0].x
#define bc_link00_im bc_link[0].y
#define bc_link01_re bc_link[1].x
#define bc_link01_im bc_link[1].y
#define bc_link02_re bc_link[2].x
#define bc_link02_im bc_link[2].y
#define bc_link10_re bc_link[3].x
#define bc_link10_im bc_link[3].y
#define bc_link11_re bc_link[4].x
#define bc_link11_im bc_link[4].y
#define bc_link12_re bc_link[5].x
#define bc_link12_im bc_link[5].y
#define bc_link20_re bc_link[6].x
#define bc_link20_im bc_link[6].y
#define bc_link21_re bc_link[7].x
#define bc_link21_im bc_link[7].y
#define bc_link22_re bc_link[8].x
#define bc_link22_im bc_link[8].y

#define ad_link00_re ad_link[0].x
#define ad_link00_im ad_link[0].y
#define ad_link01_re ad_link[1].x
#define ad_link01_im ad_link[1].y
#define ad_link02_re ad_link[2].x
#define ad_link02_im ad_link[2].y
#define ad_link10_re ad_link[3].x
#define ad_link10_im ad_link[3].y
#define ad_link11_re ad_link[4].x
#define ad_link11_im ad_link[4].y
#define ad_link12_re ad_link[5].x
#define ad_link12_im ad_link[5].y
#define ad_link20_re ad_link[6].x
#define ad_link20_im ad_link[6].y
#define ad_link21_re ad_link[7].x
#define ad_link21_im ad_link[7].y
#define ad_link22_re ad_link[8].x
#define ad_link22_im ad_link[8].y

#define de_link00_re de_link[0].x
#define de_link00_im de_link[0].y
#define de_link01_re de_link[1].x
#define de_link01_im de_link[1].y
#define de_link02_re de_link[2].x
#define de_link02_im de_link[2].y
#define de_link10_re de_link[3].x
#define de_link10_im de_link[3].y
#define de_link11_re de_link[4].x
#define de_link11_im de_link[4].y
#define de_link12_re de_link[5].x
#define de_link12_im de_link[5].y
#define de_link20_re de_link[6].x
#define de_link20_im de_link[6].y
#define de_link21_re de_link[7].x
#define de_link21_im de_link[7].y
#define de_link22_re de_link[8].x
#define de_link22_im de_link[8].y

#define ef_link00_re ef_link[0].x
#define ef_link00_im ef_link[0].y
#define ef_link01_re ef_link[1].x
#define ef_link01_im ef_link[1].y
#define ef_link02_re ef_link[2].x
#define ef_link02_im ef_link[2].y
#define ef_link10_re ef_link[3].x
#define ef_link10_im ef_link[3].y
#define ef_link11_re ef_link[4].x
#define ef_link11_im ef_link[4].y
#define ef_link12_re ef_link[5].x
#define ef_link12_im ef_link[5].y
#define ef_link20_re ef_link[6].x
#define ef_link20_im ef_link[6].y
#define ef_link21_re ef_link[7].x
#define ef_link21_im ef_link[7].y
#define ef_link22_re ef_link[8].x
#define ef_link22_im ef_link[8].y


#else // HISQ_RECONSTRUCT_LINKS

#define LINK_W00_re LINK_W[0].x
#define LINK_W00_im LINK_W[0].y
#define LINK_W01_re LINK_W[0].z
#define LINK_W01_im LINK_W[0].w
#define LINK_W02_re LINK_W[1].x
#define LINK_W02_im LINK_W[1].y
#define LINK_W10_re LINK_W[1].z
#define LINK_W10_im LINK_W[1].w
#define LINK_W11_re LINK_W[2].x
#define LINK_W11_im LINK_W[2].y
#define LINK_W12_re LINK_W[2].z
#define LINK_W12_im LINK_W[2].w
#define LINK_W20_re LINK_W[3].x
#define LINK_W20_im LINK_W[3].y
#define LINK_W21_re LINK_W[3].z
#define LINK_W21_im LINK_W[3].w
#define LINK_W22_re LINK_W[4].x
#define LINK_W22_im LINK_W[4].y


#define LINK_X00_re LINK_X[0].x
#define LINK_X00_im LINK_X[0].y
#define LINK_X01_re LINK_X[0].z
#define LINK_X01_im LINK_X[0].w
#define LINK_X02_re LINK_X[1].x
#define LINK_X02_im LINK_X[1].y
#define LINK_X10_re LINK_X[1].z
#define LINK_X10_im LINK_X[1].w
#define LINK_X11_re LINK_X[2].x
#define LINK_X11_im LINK_X[2].y
#define LINK_X12_re LINK_X[2].z
#define LINK_X12_im LINK_X[2].w
#define LINK_X20_re LINK_X[3].x
#define LINK_X20_im LINK_X[3].y
#define LINK_X21_re LINK_X[3].z
#define LINK_X21_im LINK_X[3].w
#define LINK_X22_re LINK_X[4].x
#define LINK_X22_im LINK_X[4].y


#define LINK_Y00_re LINK_Y[0].x
#define LINK_Y00_im LINK_Y[0].y
#define LINK_Y01_re LINK_Y[0].z
#define LINK_Y01_im LINK_Y[0].w
#define LINK_Y02_re LINK_Y[1].x
#define LINK_Y02_im LINK_Y[1].y
#define LINK_Y10_re LINK_Y[1].z
#define LINK_Y10_im LINK_Y[1].w
#define LINK_Y11_re LINK_Y[2].x
#define LINK_Y11_im LINK_Y[2].y
#define LINK_Y12_re LINK_Y[2].z
#define LINK_Y12_im LINK_Y[2].w
#define LINK_Y20_re LINK_Y[3].x
#define LINK_Y20_im LINK_Y[3].y
#define LINK_Y21_re LINK_Y[3].z
#define LINK_Y21_im LINK_Y[3].w
#define LINK_Y22_re LINK_Y[4].x
#define LINK_Y22_im LINK_Y[4].y


#define LINK_Z00_re LINK_Z[0].x
#define LINK_Z00_im LINK_Z[0].y
#define LINK_Z01_re LINK_Z[0].z
#define LINK_Z01_im LINK_Z[0].w
#define LINK_Z02_re LINK_Z[1].x
#define LINK_Z02_im LINK_Z[1].y
#define LINK_Z10_re LINK_Z[1].z
#define LINK_Z10_im LINK_Z[1].w
#define LINK_Z11_re LINK_Z[2].x
#define LINK_Z11_im LINK_Z[2].y
#define LINK_Z12_re LINK_Z[2].z
#define LINK_Z12_im LINK_Z[2].w
#define LINK_Z20_re LINK_Z[3].x
#define LINK_Z20_im LINK_Z[3].y
#define LINK_Z21_re LINK_Z[3].z
#define LINK_Z21_im LINK_Z[3].w
#define LINK_Z22_re LINK_Z[4].x
#define LINK_Z22_im LINK_Z[4].y

#endif // HISQ_RECONSTRUCT_LINKS

#ifndef GENERIC_MATRIX_MACROS 
#define GENERIC_MATRIX_MACROS

// Color matrices stored as an array of float2 or double2 

#define COLOR_MAT_T00_re COLOR_MAT_T[0].x
#define COLOR_MAT_T00_im COLOR_MAT_T[0].y
#define COLOR_MAT_T01_re COLOR_MAT_T[1].x
#define COLOR_MAT_T01_im COLOR_MAT_T[1].y
#define COLOR_MAT_T02_re COLOR_MAT_T[2].x
#define COLOR_MAT_T02_im COLOR_MAT_T[2].y
#define COLOR_MAT_T10_re COLOR_MAT_T[3].x
#define COLOR_MAT_T10_im COLOR_MAT_T[3].y 
#define COLOR_MAT_T11_re COLOR_MAT_T[4].x
#define COLOR_MAT_T11_im COLOR_MAT_T[4].y
#define COLOR_MAT_T12_re COLOR_MAT_T[5].x
#define COLOR_MAT_T12_im COLOR_MAT_T[5].y
#define COLOR_MAT_T20_re COLOR_MAT_T[6].x
#define COLOR_MAT_T20_im COLOR_MAT_T[6].y
#define COLOR_MAT_T21_re COLOR_MAT_T[7].x
#define COLOR_MAT_T21_im COLOR_MAT_T[7].y
#define COLOR_MAT_T22_re COLOR_MAT_T[8].x
#define COLOR_MAT_T22_im COLOR_MAT_T[8].y


#define COLOR_MAT_U00_re COLOR_MAT_U[0].x
#define COLOR_MAT_U00_im COLOR_MAT_U[0].y
#define COLOR_MAT_U01_re COLOR_MAT_U[1].x
#define COLOR_MAT_U01_im COLOR_MAT_U[1].y
#define COLOR_MAT_U02_re COLOR_MAT_U[2].x
#define COLOR_MAT_U02_im COLOR_MAT_U[2].y
#define COLOR_MAT_U10_re COLOR_MAT_U[3].x
#define COLOR_MAT_U10_im COLOR_MAT_U[3].y 
#define COLOR_MAT_U11_re COLOR_MAT_U[4].x
#define COLOR_MAT_U11_im COLOR_MAT_U[4].y
#define COLOR_MAT_U12_re COLOR_MAT_U[5].x
#define COLOR_MAT_U12_im COLOR_MAT_U[5].y
#define COLOR_MAT_U20_re COLOR_MAT_U[6].x
#define COLOR_MAT_U20_im COLOR_MAT_U[6].y
#define COLOR_MAT_U21_re COLOR_MAT_U[7].x
#define COLOR_MAT_U21_im COLOR_MAT_U[7].y
#define COLOR_MAT_U22_re COLOR_MAT_U[8].x
#define COLOR_MAT_U22_im COLOR_MAT_U[8].y


#define COLOR_MAT_V00_re COLOR_MAT_V[0].x
#define COLOR_MAT_V00_im COLOR_MAT_V[0].y
#define COLOR_MAT_V01_re COLOR_MAT_V[1].x
#define COLOR_MAT_V01_im COLOR_MAT_V[1].y
#define COLOR_MAT_V02_re COLOR_MAT_V[2].x
#define COLOR_MAT_V02_im COLOR_MAT_V[2].y
#define COLOR_MAT_V10_re COLOR_MAT_V[3].x
#define COLOR_MAT_V10_im COLOR_MAT_V[3].y 
#define COLOR_MAT_V11_re COLOR_MAT_V[4].x
#define COLOR_MAT_V11_im COLOR_MAT_V[4].y
#define COLOR_MAT_V12_re COLOR_MAT_V[5].x
#define COLOR_MAT_V12_im COLOR_MAT_V[5].y
#define COLOR_MAT_V20_re COLOR_MAT_V[6].x
#define COLOR_MAT_V20_im COLOR_MAT_V[6].y
#define COLOR_MAT_V21_re COLOR_MAT_V[7].x
#define COLOR_MAT_V21_im COLOR_MAT_V[7].y
#define COLOR_MAT_V22_re COLOR_MAT_V[8].x
#define COLOR_MAT_V22_im COLOR_MAT_V[8].y




#define COLOR_MAT_W00_re COLOR_MAT_W[0].x
#define COLOR_MAT_W00_im COLOR_MAT_W[0].y
#define COLOR_MAT_W01_re COLOR_MAT_W[1].x
#define COLOR_MAT_W01_im COLOR_MAT_W[1].y
#define COLOR_MAT_W02_re COLOR_MAT_W[2].x
#define COLOR_MAT_W02_im COLOR_MAT_W[2].y
#define COLOR_MAT_W10_re COLOR_MAT_W[3].x
#define COLOR_MAT_W10_im COLOR_MAT_W[3].y 
#define COLOR_MAT_W11_re COLOR_MAT_W[4].x
#define COLOR_MAT_W11_im COLOR_MAT_W[4].y
#define COLOR_MAT_W12_re COLOR_MAT_W[5].x
#define COLOR_MAT_W12_im COLOR_MAT_W[5].y
#define COLOR_MAT_W20_re COLOR_MAT_W[6].x
#define COLOR_MAT_W20_im COLOR_MAT_W[6].y
#define COLOR_MAT_W21_re COLOR_MAT_W[7].x
#define COLOR_MAT_W21_im COLOR_MAT_W[7].y
#define COLOR_MAT_W22_re COLOR_MAT_W[8].x
#define COLOR_MAT_W22_im COLOR_MAT_W[8].y


#define COLOR_MAT_X00_re COLOR_MAT_X[0].x
#define COLOR_MAT_X00_im COLOR_MAT_X[0].y
#define COLOR_MAT_X01_re COLOR_MAT_X[1].x
#define COLOR_MAT_X01_im COLOR_MAT_X[1].y
#define COLOR_MAT_X02_re COLOR_MAT_X[2].x
#define COLOR_MAT_X02_im COLOR_MAT_X[2].y
#define COLOR_MAT_X10_re COLOR_MAT_X[3].x
#define COLOR_MAT_X10_im COLOR_MAT_X[3].y 
#define COLOR_MAT_X11_re COLOR_MAT_X[4].x
#define COLOR_MAT_X11_im COLOR_MAT_X[4].y
#define COLOR_MAT_X12_re COLOR_MAT_X[5].x
#define COLOR_MAT_X12_im COLOR_MAT_X[5].y
#define COLOR_MAT_X20_re COLOR_MAT_X[6].x
#define COLOR_MAT_X20_im COLOR_MAT_X[6].y
#define COLOR_MAT_X21_re COLOR_MAT_X[7].x
#define COLOR_MAT_X21_im COLOR_MAT_X[7].y
#define COLOR_MAT_X22_re COLOR_MAT_X[8].x
#define COLOR_MAT_X22_im COLOR_MAT_X[8].y


#define COLOR_MAT_Y00_re COLOR_MAT_Y[0].x
#define COLOR_MAT_Y00_im COLOR_MAT_Y[0].y
#define COLOR_MAT_Y01_re COLOR_MAT_Y[1].x
#define COLOR_MAT_Y01_im COLOR_MAT_Y[1].y
#define COLOR_MAT_Y02_re COLOR_MAT_Y[2].x
#define COLOR_MAT_Y02_im COLOR_MAT_Y[2].y
#define COLOR_MAT_Y10_re COLOR_MAT_Y[3].x
#define COLOR_MAT_Y10_im COLOR_MAT_Y[3].y 
#define COLOR_MAT_Y11_re COLOR_MAT_Y[4].x
#define COLOR_MAT_Y11_im COLOR_MAT_Y[4].y
#define COLOR_MAT_Y12_re COLOR_MAT_Y[5].x
#define COLOR_MAT_Y12_im COLOR_MAT_Y[5].y
#define COLOR_MAT_Y20_re COLOR_MAT_Y[6].x
#define COLOR_MAT_Y20_im COLOR_MAT_Y[6].y
#define COLOR_MAT_Y21_re COLOR_MAT_Y[7].x
#define COLOR_MAT_Y21_im COLOR_MAT_Y[7].y
#define COLOR_MAT_Y22_re COLOR_MAT_Y[8].x
#define COLOR_MAT_Y22_im COLOR_MAT_Y[8].y


#define COLOR_MAT_Z00_re COLOR_MAT_Z[0].x
#define COLOR_MAT_Z00_im COLOR_MAT_Z[0].y
#define COLOR_MAT_Z01_re COLOR_MAT_Z[1].x
#define COLOR_MAT_Z01_im COLOR_MAT_Z[1].y
#define COLOR_MAT_Z02_re COLOR_MAT_Z[2].x
#define COLOR_MAT_Z02_im COLOR_MAT_Z[2].y
#define COLOR_MAT_Z10_re COLOR_MAT_Z[3].x
#define COLOR_MAT_Z10_im COLOR_MAT_Z[3].y 
#define COLOR_MAT_Z11_re COLOR_MAT_Z[4].x
#define COLOR_MAT_Z11_im COLOR_MAT_Z[4].y
#define COLOR_MAT_Z12_re COLOR_MAT_Z[5].x
#define COLOR_MAT_Z12_im COLOR_MAT_Z[5].y
#define COLOR_MAT_Z20_re COLOR_MAT_Z[6].x
#define COLOR_MAT_Z20_im COLOR_MAT_Z[6].y
#define COLOR_MAT_Z21_re COLOR_MAT_Z[7].x
#define COLOR_MAT_Z21_im COLOR_MAT_Z[7].y
#define COLOR_MAT_Z22_re COLOR_MAT_Z[8].x
#define COLOR_MAT_Z22_im COLOR_MAT_Z[8].y

#define FF_RECONSTRUCT_LINK_12(var, sign)				\
  ACC_CONJ_PROD_ASSIGN(var##20, +var##01, +var##12);			\
  ACC_CONJ_PROD(var##20, -var##02, +var##11);				\
  ACC_CONJ_PROD_ASSIGN(var##21, +var##02, +var##10);			\
  ACC_CONJ_PROD(var##21, -var##00, +var##12);				\
  ACC_CONJ_PROD_ASSIGN(var##22, +var##00, +var##11);			\
  ACC_CONJ_PROD(var##22, -var##01, +var##10);				\
  var##20_re *=sign;var##20_im *=sign; var##21_re *=sign; var##21_im *=sign; \
  var##22_re *=sign;var##22_im *=sign;



#define HISQ_LOAD_MATRIX_18_SINGLE_TEX(gauge, dir, idx, var, stride)do{	\
    var[0] = tex1Dfetch(gauge, idx + dir*stride*9);			\
    var[1] = tex1Dfetch(gauge, idx + dir*stride*9 + stride);            \
    var[2] = tex1Dfetch(gauge, idx + dir*stride*9 + stride*2);          \
    var[3] = tex1Dfetch(gauge, idx + dir*stride*9 + stride*3);          \
    var[4] = tex1Dfetch(gauge, idx + dir*stride*9 + stride*4);          \
    var[5] = tex1Dfetch(gauge, idx + dir*stride*9 + stride*5);          \
    var[6] = tex1Dfetch(gauge, idx + dir*stride*9 + stride*6);          \
    var[7] = tex1Dfetch(gauge, idx + dir*stride*9 + stride*7);          \
    var[8] = tex1Dfetch(gauge, idx + dir*stride*9 + stride*8);          \
  }while(0)

#define HISQ_LOAD_MATRIX_12_SINGLE_TEX(gauge, dir, idx, var, stride)do{	\
    float4 tmp;								\
    tmp = tex1Dfetch(gauge, idx + dir*stride*3);			\
    var[0] = make_float2(tmp.x, tmp.y);					\
    var[1] = make_float2(tmp.z, tmp.w);					\
    tmp = tex1Dfetch(gauge, idx + dir*stride*3 + stride);		\
    var[2] = make_float2(tmp.x, tmp.y);					\
    var[3] = make_float2(tmp.z, tmp.w);					\
    tmp = tex1Dfetch(gauge, idx + dir*stride*3 + 2*stride);		\
    var[4] = make_float2(tmp.x, tmp.y);					\
    var[5] = make_float2(tmp.z, tmp.w);					\
  }while(0)

#define HISQ_LOAD_MATRIX_18_DOUBLE_TEX(gauge_tex, gauge, dir, idx, var, stride)do{ \
    var[0] = READ_DOUBLE2_TEXTURE(gauge_tex, gauge, idx + dir*stride*9); \
    var[1] = READ_DOUBLE2_TEXTURE(gauge_tex, gauge, idx + dir*stride*9 + stride); \
    var[2] = READ_DOUBLE2_TEXTURE(gauge_tex, gauge, idx + dir*stride*9 + stride*2); \
    var[3] = READ_DOUBLE2_TEXTURE(gauge_tex, gauge, idx + dir*stride*9 + stride*3); \
    var[4] = READ_DOUBLE2_TEXTURE(gauge_tex, gauge, idx + dir*stride*9 + stride*4); \
    var[5] = READ_DOUBLE2_TEXTURE(gauge_tex, gauge, idx + dir*stride*9 + stride*5); \
    var[6] = READ_DOUBLE2_TEXTURE(gauge_tex, gauge, idx + dir*stride*9 + stride*6); \
    var[7] = READ_DOUBLE2_TEXTURE(gauge_tex, gauge, idx + dir*stride*9 + stride*7); \
    var[8] = READ_DOUBLE2_TEXTURE(gauge_tex, gauge, idx + dir*stride*9 + stride*8); \
  }while(0)

#define HISQ_LOAD_MATRIX_12_DOUBLE_TEX(gauge_tex, gauge, dir, idx, var, stride)do{ \
    var[0] = READ_DOUBLE2_TEXTURE(gauge_tex, gauge, idx + dir*stride*6); \
    var[1] = READ_DOUBLE2_TEXTURE(gauge_tex, gauge, idx + dir*stride*6 + stride); \
    var[2] = READ_DOUBLE2_TEXTURE(gauge_tex, gauge, idx + dir*stride*6 + stride*2); \
    var[3] = READ_DOUBLE2_TEXTURE(gauge_tex, gauge, idx + dir*stride*6 + stride*3); \
    var[4] = READ_DOUBLE2_TEXTURE(gauge_tex, gauge, idx + dir*stride*6 + stride*4); \
    var[5] = READ_DOUBLE2_TEXTURE(gauge_tex, gauge, idx + dir*stride*6 + stride*5); \
  }while(0)

#ifdef MULTI_GPU

#define FF_COMPUTE_NEW_FULL_IDX_PLUS_UPDATE(mydir, idx, new_idx) do { \
    switch(mydir){						      \
    case 0:							       \
      new_idx =  (xcomm || (new_x[0] != X1+1) )?(idx+1):(idx-X1m1);    \
      new_x[0] = (xcomm || (new_x[0] != X1+1) )?(new_x[0]+1):2;		\
      break;								\
    case 1:								\
      new_idx =  (ycomm || (new_x[1] != X2+1))?(idx+E1):(idx -X2m1*E1);	\
      new_x[1] = (ycomm || (new_x[1] != X2+1))?(new_x[1]+1):2;		\
      break;								\
    case 2:								\
      new_idx =  (zcomm || (new_x[2] != X3+1))?(idx+E2E1):(idx-X3m1*E2E1); \
      new_x[2] = (zcomm || (new_x[2] != X3+1))?(new_x[2]+1):2;		\
      break;								\
    case 3:								\
      new_idx =  (tcomm || (new_x[3] != X4+1))?(idx+E3E2E1):(idx-X4m1*E3E2E1); \
      new_x[3] = (tcomm || (new_x[3] != X4+1))?(new_x[3]+1):2;		\
      break;								\
    }									\
    if(new_x[mydir] >= E[mydir]) return;				\
  }while(0)



#else
#define FF_COMPUTE_NEW_FULL_IDX_PLUS_UPDATE(mydir, idx, new_idx) do {	\
    switch(mydir){							\
    case 0:                                                             \
      new_idx = ( (new_x[0]==X1m1)?idx-X1m1:idx+1);			\
      new_x[0] = (new_x[0]==X1m1)?0:new_x[0]+1;                         \
      break;								\
    case 1:                                                             \
      new_idx = ( (new_x[1]==X2m1)?idx-X2X1mX1:idx+X1);		        \
      new_x[1] = (new_x[1]==X2m1)?0:new_x[1]+1;                         \
      break;								\
    case 2:								\
      new_idx = ( (new_x[2]==X3m1)?idx-X3X2X1mX2X1:idx+X2X1);	        \
      new_x[2] = (new_x[2]==X3m1)?0:new_x[2]+1;                         \
      break;								\
    case 3:                                                             \
      new_idx = ( (new_x[3]==X4m1)?idx-X4X3X2X1mX3X2X1:idx+X3X2X1);     \
      new_x[3] = (new_x[3]==X4m1)?0:new_x[3]+1;                         \
      break;								\
    }									\
  }while(0)
#endif

#ifdef MULTI_GPU


#define FF_COMPUTE_NEW_FULL_IDX_MINUS_UPDATE(mydir, idx, new_idx) do {  \
  switch(mydir){							\
  case 0:								\
    new_idx =  (xcomm || (new_x[0] != 2))?(idx-1):(idx+X1m1);		\
    new_x[0] = (xcomm || (new_x[0] != 2))?(new_x[0]-1):(X1+1);		\
    break;								\
  case 1:								\
    new_idx = (ycomm || (new_x[1] != 2))?(idx-E1):(idx+X2m1*E1);	\
    new_x[1]= (ycomm || (new_x[1] != 2))?(new_x[1]-1):(X2+1);		\
    break;								\
  case 2:								\
    new_idx = (zcomm || (new_x[2] != 2))?(idx-E2E1):(idx+X3m1*E2E1);	\
    new_x[2]= (zcomm || (new_x[2] != 2))?(new_x[2]-1):(X3+1);		\
    break;								\
  case 3:								\
    new_idx = (tcomm || (new_x[3] !=2))?(idx-E3E2E1):(idx+X4m1*E3E2E1); \
    new_x[3]= (tcomm || (new_x[3] !=2))?(new_x[3]-1):(X4+1);		\
    break;								\
  }									\
  if(new_x[mydir] < 0) return;						\
  }while(0)

#else
#define FF_COMPUTE_NEW_FULL_IDX_MINUS_UPDATE(mydir, idx, new_idx) do {	\
    switch(mydir){							\
    case 0:                                                             \
      new_idx = ( (new_x[0]==0)?idx+X1m1:idx-1);			\
      new_x[0] = (new_x[0]==0)?X1m1:new_x[0] - 1;                       \
      break;								\
    case 1:                                                             \
      new_idx = ( (new_x[1]==0)?idx+X2X1mX1:idx-X1);		        \
      new_x[1] = (new_x[1]==0)?X2m1:new_x[1] - 1;                       \
      break;								\
    case 2:                                                             \
      new_idx = ( (new_x[2]==0)?idx+X3X2X1mX2X1:idx-X2X1);		\
      new_x[2] = (new_x[2]==0)?X3m1:new_x[2] - 1;                       \
      break;								\
    case 3:                                                             \
      new_idx = ( (new_x[3]==0)?idx+X4X3X2X1mX3X2X1:idx-X3X2X1);	\
      new_x[3] = (new_x[3]==0)?X4m1:new_x[3] - 1;                       \
      break;								\
    }									\
  }while(0)
#endif


// matrix macros:
#define ADJ_MAT(a, b) \
  b##00_re =  a##00_re; \
  b##00_im = -a##00_im; \
  b##01_re =  a##10_re; \
  b##01_im = -a##10_im; \
  b##02_re =  a##20_re; \
  b##02_im = -a##20_im; \
  b##10_re =  a##01_re; \
  b##10_im = -a##01_im; \
  b##11_re =  a##11_re; \
  b##11_im = -a##11_im; \
  b##12_re =  a##21_re; \
  b##12_im = -a##21_im; \
  b##20_re =  a##02_re; \
  b##20_im = -a##02_im; \
  b##21_re =  a##12_re; \
  b##21_im = -a##12_im; \
  b##22_re =  a##22_re; \
  b##22_im = -a##22_im; 


#define ASSIGN_MAT(a, b) \
  b##00_re =  a##00_re; \
  b##00_im =  a##00_im; \
  b##01_re =  a##01_re; \
  b##01_im =  a##01_im; \
  b##02_re =  a##02_re; \
  b##02_im =  a##02_im; \
  b##10_re =  a##10_re; \
  b##10_im =  a##10_im; \
  b##11_re =  a##11_re; \
  b##11_im =  a##11_im; \
  b##12_re =  a##12_re; \
  b##12_im =  a##12_im; \
  b##20_re =  a##20_re; \
  b##20_im =  a##20_im; \
  b##21_re =  a##21_re; \
  b##21_im =  a##21_im; \
  b##22_re =  a##22_re; \
  b##22_im =  a##22_im; \



#define MATRIX_PRODUCT(a, b, simple, c) do{ \
  if(simple){                               \
      c##00_re = a##00_re*b##00_re - a##00_im*b##00_im + a##01_re*b##10_re - a##01_im*b##10_im + a##02_re*b##20_re - a##02_im*b##20_im; \
      c##00_im = a##00_re*b##00_im + a##00_im*b##00_re + a##01_re*b##10_im + a##01_im*b##10_re + a##02_re*b##20_im + a##02_im*b##20_re; \
      c##01_re = a##00_re*b##01_re - a##00_im*b##01_im + a##01_re*b##11_re - a##01_im*b##11_im + a##02_re*b##21_re - a##02_im*b##21_im; \
      c##01_im = a##00_re*b##01_im + a##00_im*b##01_re + a##01_re*b##11_im + a##01_im*b##11_re + a##02_re*b##21_im + a##02_im*b##21_re; \
      c##02_re = a##00_re*b##02_re - a##00_im*b##02_im + a##01_re*b##12_re - a##01_im*b##12_im + a##02_re*b##22_re - a##02_im*b##22_im; \
      c##02_im = a##00_re*b##02_im + a##00_im*b##02_re + a##01_re*b##12_im + a##01_im*b##12_re + a##02_re*b##22_im + a##02_im*b##22_re; \
      c##10_re = a##10_re*b##00_re - a##10_im*b##00_im + a##11_re*b##10_re - a##11_im*b##10_im + a##12_re*b##20_re - a##12_im*b##20_im; \
      c##10_im = a##10_re*b##00_im + a##10_im*b##00_re + a##11_re*b##10_im + a##11_im*b##10_re + a##12_re*b##20_im + a##12_im*b##20_re; \
      c##11_re = a##10_re*b##01_re - a##10_im*b##01_im + a##11_re*b##11_re - a##11_im*b##11_im + a##12_re*b##21_re - a##12_im*b##21_im; \
      c##11_im = a##10_re*b##01_im + a##10_im*b##01_re + a##11_re*b##11_im + a##11_im*b##11_re + a##12_re*b##21_im + a##12_im*b##21_re; \
      c##12_re = a##10_re*b##02_re - a##10_im*b##02_im + a##11_re*b##12_re - a##11_im*b##12_im + a##12_re*b##22_re - a##12_im*b##22_im; \
      c##12_im = a##10_re*b##02_im + a##10_im*b##02_re + a##11_re*b##12_im + a##11_im*b##12_re + a##12_re*b##22_im + a##12_im*b##22_re; \
      c##20_re = a##20_re*b##00_re - a##20_im*b##00_im + a##21_re*b##10_re - a##21_im*b##10_im + a##22_re*b##20_re - a##22_im*b##20_im; \
      c##20_im = a##20_re*b##00_im + a##20_im*b##00_re + a##21_re*b##10_im + a##21_im*b##10_re + a##22_re*b##20_im + a##22_im*b##20_re; \
      c##21_re = a##20_re*b##01_re - a##20_im*b##01_im + a##21_re*b##11_re - a##21_im*b##11_im + a##22_re*b##21_re - a##22_im*b##21_im; \
      c##21_im = a##20_re*b##01_im + a##20_im*b##01_re + a##21_re*b##11_im + a##21_im*b##11_re + a##22_re*b##21_im + a##22_im*b##21_re; \
      c##22_re = a##20_re*b##02_re - a##20_im*b##02_im + a##21_re*b##12_re - a##21_im*b##12_im + a##22_re*b##22_re - a##22_im*b##22_im; \
      c##22_im = a##20_re*b##02_im + a##20_im*b##02_re + a##21_re*b##12_im + a##21_im*b##12_re + a##22_re*b##22_im + a##22_im*b##22_re; \
  }else{                                      \
      c##00_re = a##00_re*b##00_re + a##00_im*b##00_im + a##10_re*b##10_re + a##10_im*b##10_im + a##20_re*b##20_re + a##20_im*b##20_im; \
      c##00_im = a##00_re*b##00_im - a##00_im*b##00_re + a##10_re*b##10_im - a##10_im*b##10_re + a##20_re*b##20_im - a##20_im*b##20_re; \
      c##01_re = a##00_re*b##01_re + a##00_im*b##01_im + a##10_re*b##11_re + a##10_im*b##11_im + a##20_re*b##21_re + a##20_im*b##21_im; \
      c##01_im = a##00_re*b##01_im - a##00_im*b##01_re + a##10_re*b##11_im - a##10_im*b##11_re + a##20_re*b##21_im - a##20_im*b##21_re; \
      c##02_re = a##00_re*b##02_re + a##00_im*b##02_im + a##10_re*b##12_re + a##10_im*b##12_im + a##20_re*b##22_re + a##20_im*b##22_im; \
      c##02_im = a##00_re*b##02_im - a##00_im*b##02_re + a##10_re*b##12_im - a##10_im*b##12_re + a##20_re*b##22_im - a##20_im*b##22_re; \
      c##10_re = a##01_re*b##00_re + a##01_im*b##00_im + a##11_re*b##10_re + a##11_im*b##10_im + a##21_re*b##20_re + a##21_im*b##20_im; \
      c##10_im = a##01_re*b##00_im - a##01_im*b##00_re + a##11_re*b##10_im - a##11_im*b##10_re + a##21_re*b##20_im - a##21_im*b##20_re; \
      c##11_re = a##01_re*b##01_re + a##01_im*b##01_im + a##11_re*b##11_re + a##11_im*b##11_im + a##21_re*b##21_re + a##21_im*b##21_im; \
      c##11_im = a##01_re*b##01_im - a##01_im*b##01_re + a##11_re*b##11_im - a##11_im*b##11_re + a##21_re*b##21_im - a##21_im*b##21_re; \
      c##12_re = a##01_re*b##02_re + a##01_im*b##02_im + a##11_re*b##12_re + a##11_im*b##12_im + a##21_re*b##22_re + a##21_im*b##22_im; \
      c##12_im = a##01_re*b##02_im - a##01_im*b##02_re + a##11_re*b##12_im - a##11_im*b##12_re + a##21_re*b##22_im - a##21_im*b##22_re; \
      c##20_re = a##02_re*b##00_re + a##02_im*b##00_im + a##12_re*b##10_re + a##12_im*b##10_im + a##22_re*b##20_re + a##22_im*b##20_im; \
      c##20_im = a##02_re*b##00_im - a##02_im*b##00_re + a##12_re*b##10_im - a##12_im*b##10_re + a##22_re*b##20_im - a##22_im*b##20_re; \
      c##21_re = a##02_re*b##01_re + a##02_im*b##01_im + a##12_re*b##11_re + a##12_im*b##11_im + a##22_re*b##21_re + a##22_im*b##21_im; \
      c##21_im = a##02_re*b##01_im - a##02_im*b##01_re + a##12_re*b##11_im - a##12_im*b##11_re + a##22_re*b##21_im - a##22_im*b##21_re; \
      c##22_re = a##02_re*b##02_re + a##02_im*b##02_im + a##12_re*b##12_re + a##12_im*b##12_im + a##22_re*b##22_re + a##22_im*b##22_im; \
      c##22_im = a##02_re*b##02_im - a##02_im*b##02_re + a##12_re*b##12_im - a##12_im*b##12_re + a##22_re*b##22_im - a##22_im*b##22_re; \
  }    \
}while(0)


#define MAT_MUL_MAT(a, b, c) \
  c##00_re = a##00_re*b##00_re - a##00_im*b##00_im + a##01_re*b##10_re - a##01_im*b##10_im + a##02_re*b##20_re - a##02_im*b##20_im; \
  c##00_im = a##00_re*b##00_im + a##00_im*b##00_re + a##01_re*b##10_im + a##01_im*b##10_re + a##02_re*b##20_im + a##02_im*b##20_re; \
  c##01_re = a##00_re*b##01_re - a##00_im*b##01_im + a##01_re*b##11_re - a##01_im*b##11_im + a##02_re*b##21_re - a##02_im*b##21_im; \
  c##01_im = a##00_re*b##01_im + a##00_im*b##01_re + a##01_re*b##11_im + a##01_im*b##11_re + a##02_re*b##21_im + a##02_im*b##21_re; \
  c##02_re = a##00_re*b##02_re - a##00_im*b##02_im + a##01_re*b##12_re - a##01_im*b##12_im + a##02_re*b##22_re - a##02_im*b##22_im; \
  c##02_im = a##00_re*b##02_im + a##00_im*b##02_re + a##01_re*b##12_im + a##01_im*b##12_re + a##02_re*b##22_im + a##02_im*b##22_re; \
  c##10_re = a##10_re*b##00_re - a##10_im*b##00_im + a##11_re*b##10_re - a##11_im*b##10_im + a##12_re*b##20_re - a##12_im*b##20_im; \
  c##10_im = a##10_re*b##00_im + a##10_im*b##00_re + a##11_re*b##10_im + a##11_im*b##10_re + a##12_re*b##20_im + a##12_im*b##20_re; \
  c##11_re = a##10_re*b##01_re - a##10_im*b##01_im + a##11_re*b##11_re - a##11_im*b##11_im + a##12_re*b##21_re - a##12_im*b##21_im; \
  c##11_im = a##10_re*b##01_im + a##10_im*b##01_re + a##11_re*b##11_im + a##11_im*b##11_re + a##12_re*b##21_im + a##12_im*b##21_re; \
  c##12_re = a##10_re*b##02_re - a##10_im*b##02_im + a##11_re*b##12_re - a##11_im*b##12_im + a##12_re*b##22_re - a##12_im*b##22_im; \
  c##12_im = a##10_re*b##02_im + a##10_im*b##02_re + a##11_re*b##12_im + a##11_im*b##12_re + a##12_re*b##22_im + a##12_im*b##22_re; \
  c##20_re = a##20_re*b##00_re - a##20_im*b##00_im + a##21_re*b##10_re - a##21_im*b##10_im + a##22_re*b##20_re - a##22_im*b##20_im; \
  c##20_im = a##20_re*b##00_im + a##20_im*b##00_re + a##21_re*b##10_im + a##21_im*b##10_re + a##22_re*b##20_im + a##22_im*b##20_re; \
  c##21_re = a##20_re*b##01_re - a##20_im*b##01_im + a##21_re*b##11_re - a##21_im*b##11_im + a##22_re*b##21_re - a##22_im*b##21_im; \
  c##21_im = a##20_re*b##01_im + a##20_im*b##01_re + a##21_re*b##11_im + a##21_im*b##11_re + a##22_re*b##21_im + a##22_im*b##21_re; \
  c##22_re = a##20_re*b##02_re - a##20_im*b##02_im + a##21_re*b##12_re - a##21_im*b##12_im + a##22_re*b##22_re - a##22_im*b##22_im; \
  c##22_im = a##20_re*b##02_im + a##20_im*b##02_re + a##21_re*b##12_im + a##21_im*b##12_re + a##22_re*b##22_im + a##22_im*b##22_re; 

#define MAT_MUL_ADJ_MAT(a, b, c) \
  c##00_re =    a##00_re*b##00_re + a##00_im*b##00_im + a##01_re*b##01_re + a##01_im*b##01_im + a##02_re*b##02_re + a##02_im*b##02_im; \
  c##00_im =  - a##00_re*b##00_im + a##00_im*b##00_re - a##01_re*b##01_im + a##01_im*b##01_re - a##02_re*b##02_im + a##02_im*b##02_re; \
  c##01_re =    a##00_re*b##10_re + a##00_im*b##10_im + a##01_re*b##11_re + a##01_im*b##11_im + a##02_re*b##12_re + a##02_im*b##12_im; \
  c##01_im =  - a##00_re*b##10_im + a##00_im*b##10_re - a##01_re*b##11_im + a##01_im*b##11_re - a##02_re*b##12_im + a##02_im*b##12_re; \
  c##02_re =    a##00_re*b##20_re + a##00_im*b##20_im + a##01_re*b##21_re + a##01_im*b##21_im + a##02_re*b##22_re + a##02_im*b##22_im; \
  c##02_im =  - a##00_re*b##20_im + a##00_im*b##20_re - a##01_re*b##21_im + a##01_im*b##21_re - a##02_re*b##22_im + a##02_im*b##22_re; \
  c##10_re =    a##10_re*b##00_re + a##10_im*b##00_im + a##11_re*b##01_re + a##11_im*b##01_im + a##12_re*b##02_re + a##12_im*b##02_im; \
  c##10_im =  - a##10_re*b##00_im + a##10_im*b##00_re - a##11_re*b##01_im + a##11_im*b##01_re - a##12_re*b##02_im + a##12_im*b##02_re; \
  c##11_re =    a##10_re*b##10_re + a##10_im*b##10_im + a##11_re*b##11_re + a##11_im*b##11_im + a##12_re*b##12_re + a##12_im*b##12_im; \
  c##11_im =  - a##10_re*b##10_im + a##10_im*b##10_re - a##11_re*b##11_im + a##11_im*b##11_re - a##12_re*b##12_im + a##12_im*b##12_re; \
  c##12_re =    a##10_re*b##20_re + a##10_im*b##20_im + a##11_re*b##21_re + a##11_im*b##21_im + a##12_re*b##22_re + a##12_im*b##22_im; \
  c##12_im =  - a##10_re*b##20_im + a##10_im*b##20_re - a##11_re*b##21_im + a##11_im*b##21_re - a##12_re*b##22_im + a##12_im*b##22_re; \
  c##20_re =    a##20_re*b##00_re + a##20_im*b##00_im + a##21_re*b##01_re + a##21_im*b##01_im + a##22_re*b##02_re + a##22_im*b##02_im; \
  c##20_im =  - a##20_re*b##00_im + a##20_im*b##00_re - a##21_re*b##01_im + a##21_im*b##01_re - a##22_re*b##02_im + a##22_im*b##02_re; \
  c##21_re =    a##20_re*b##10_re + a##20_im*b##10_im + a##21_re*b##11_re + a##21_im*b##11_im + a##22_re*b##12_re + a##22_im*b##12_im; \
  c##21_im =  - a##20_re*b##10_im + a##20_im*b##10_re - a##21_re*b##11_im + a##21_im*b##11_re - a##22_re*b##12_im + a##22_im*b##12_re; \
  c##22_re =    a##20_re*b##20_re + a##20_im*b##20_im + a##21_re*b##21_re + a##21_im*b##21_im + a##22_re*b##22_re + a##22_im*b##22_im; \
  c##22_im =  - a##20_re*b##20_im + a##20_im*b##20_re - a##21_re*b##21_im + a##21_im*b##21_re - a##22_re*b##22_im + a##22_im*b##22_re; 

#define ADJ_MAT_MUL_MAT(a, b, c) \
    c##00_re = a##00_re*b##00_re + a##00_im*b##00_im + a##10_re*b##10_re + a##10_im*b##10_im + a##20_re*b##20_re + a##20_im*b##20_im; \
  c##00_im = a##00_re*b##00_im - a##00_im*b##00_re + a##10_re*b##10_im - a##10_im*b##10_re + a##20_re*b##20_im - a##20_im*b##20_re; \
  c##01_re = a##00_re*b##01_re + a##00_im*b##01_im + a##10_re*b##11_re + a##10_im*b##11_im + a##20_re*b##21_re + a##20_im*b##21_im; \
  c##01_im = a##00_re*b##01_im - a##00_im*b##01_re + a##10_re*b##11_im - a##10_im*b##11_re + a##20_re*b##21_im - a##20_im*b##21_re; \
  c##02_re = a##00_re*b##02_re + a##00_im*b##02_im + a##10_re*b##12_re + a##10_im*b##12_im + a##20_re*b##22_re + a##20_im*b##22_im; \
  c##02_im = a##00_re*b##02_im - a##00_im*b##02_re + a##10_re*b##12_im - a##10_im*b##12_re + a##20_re*b##22_im - a##20_im*b##22_re; \
  c##10_re = a##01_re*b##00_re + a##01_im*b##00_im + a##11_re*b##10_re + a##11_im*b##10_im + a##21_re*b##20_re + a##21_im*b##20_im; \
  c##10_im = a##01_re*b##00_im - a##01_im*b##00_re + a##11_re*b##10_im - a##11_im*b##10_re + a##21_re*b##20_im - a##21_im*b##20_re; \
  c##11_re = a##01_re*b##01_re + a##01_im*b##01_im + a##11_re*b##11_re + a##11_im*b##11_im + a##21_re*b##21_re + a##21_im*b##21_im; \
  c##11_im = a##01_re*b##01_im - a##01_im*b##01_re + a##11_re*b##11_im - a##11_im*b##11_re + a##21_re*b##21_im - a##21_im*b##21_re; \
  c##12_re = a##01_re*b##02_re + a##01_im*b##02_im + a##11_re*b##12_re + a##11_im*b##12_im + a##21_re*b##22_re + a##21_im*b##22_im; \
  c##12_im = a##01_re*b##02_im - a##01_im*b##02_re + a##11_re*b##12_im - a##11_im*b##12_re + a##21_re*b##22_im - a##21_im*b##22_re; \
  c##20_re = a##02_re*b##00_re + a##02_im*b##00_im + a##12_re*b##10_re + a##12_im*b##10_im + a##22_re*b##20_re + a##22_im*b##20_im; \
  c##20_im = a##02_re*b##00_im - a##02_im*b##00_re + a##12_re*b##10_im - a##12_im*b##10_re + a##22_re*b##20_im - a##22_im*b##20_re; \
  c##21_re = a##02_re*b##01_re + a##02_im*b##01_im + a##12_re*b##11_re + a##12_im*b##11_im + a##22_re*b##21_re + a##22_im*b##21_im; \
  c##21_im = a##02_re*b##01_im - a##02_im*b##01_re + a##12_re*b##11_im - a##12_im*b##11_re + a##22_re*b##21_im - a##22_im*b##21_re; \
  c##22_re = a##02_re*b##02_re + a##02_im*b##02_im + a##12_re*b##12_re + a##12_im*b##12_im + a##22_re*b##22_re + a##22_im*b##22_im; \
  c##22_im = a##02_re*b##02_im - a##02_im*b##02_re + a##12_re*b##12_im - a##12_im*b##12_re + a##22_re*b##22_im - a##22_im*b##22_re; 

#define ADJ_MAT_MUL_ADJ_MAT(a, b, c)					\
      c##00_re =    a##00_re*b##00_re - a##00_im*b##00_im + a##10_re*b##01_re - a##10_im*b##01_im + a##20_re*b##02_re - a##20_im*b##02_im; \
  c##00_im =  - a##00_re*b##00_im - a##00_im*b##00_re - a##10_re*b##01_im - a##10_im*b##01_re - a##20_re*b##02_im - a##20_im*b##02_re; \
  c##01_re =    a##00_re*b##10_re - a##00_im*b##10_im + a##10_re*b##11_re - a##10_im*b##11_im + a##20_re*b##12_re - a##20_im*b##12_im; \
  c##01_im =  - a##00_re*b##10_im - a##00_im*b##10_re - a##10_re*b##11_im - a##10_im*b##11_re - a##20_re*b##12_im - a##20_im*b##12_re; \
  c##02_re =    a##00_re*b##20_re - a##00_im*b##20_im + a##10_re*b##21_re - a##10_im*b##21_im + a##20_re*b##22_re - a##20_im*b##22_im; \
  c##02_im =  - a##00_re*b##20_im - a##00_im*b##20_re - a##10_re*b##21_im - a##10_im*b##21_re - a##20_re*b##22_im - a##20_im*b##22_re; \
  c##10_re =    a##01_re*b##00_re - a##01_im*b##00_im + a##11_re*b##01_re - a##11_im*b##01_im + a##21_re*b##02_re - a##21_im*b##02_im; \
  c##10_im =  - a##01_re*b##00_im - a##01_im*b##00_re - a##11_re*b##01_im - a##11_im*b##01_re - a##21_re*b##02_im - a##21_im*b##02_re; \
  c##11_re =    a##01_re*b##10_re - a##01_im*b##10_im + a##11_re*b##11_re - a##11_im*b##11_im + a##21_re*b##12_re - a##21_im*b##12_im; \
  c##11_im =  - a##01_re*b##10_im - a##01_im*b##10_re - a##11_re*b##11_im - a##11_im*b##11_re - a##21_re*b##12_im - a##21_im*b##12_re; \
  c##12_re =    a##01_re*b##20_re - a##01_im*b##20_im + a##11_re*b##21_re - a##11_im*b##21_im + a##21_re*b##22_re - a##21_im*b##22_im; \
  c##12_im =  - a##01_re*b##20_im - a##01_im*b##20_re - a##11_re*b##21_im - a##11_im*b##21_re - a##21_re*b##22_im - a##21_im*b##22_re; \
  c##20_re =    a##02_re*b##00_re - a##02_im*b##00_im + a##12_re*b##01_re - a##12_im*b##01_im + a##22_re*b##02_re - a##22_im*b##02_im; \
  c##20_im =  - a##02_re*b##00_im - a##02_im*b##00_re - a##12_re*b##01_im - a##12_im*b##01_re - a##22_re*b##02_im - a##22_im*b##02_re; \
  c##21_re =    a##02_re*b##10_re - a##02_im*b##10_im + a##12_re*b##11_re - a##12_im*b##11_im + a##22_re*b##12_re - a##22_im*b##12_im; \
  c##21_im =  - a##02_re*b##10_im - a##02_im*b##10_re - a##12_re*b##11_im - a##12_im*b##11_re - a##22_re*b##12_im - a##22_im*b##12_re; \
  c##22_re =    a##02_re*b##20_re - a##02_im*b##20_im + a##12_re*b##21_re - a##12_im*b##21_im + a##22_re*b##22_re - a##22_im*b##22_im; \
  c##22_im =  - a##02_re*b##20_im - a##02_im*b##20_re - a##12_re*b##21_im - a##12_im*b##21_re - a##22_re*b##22_im - a##22_im*b##22_re; 

  // end of macros specific to hisq routines


#define SCALAR_MULT_ADD_MATRIX(a, b, scalar, c) do{ \
    c##00_re = a##00_re + scalar*b##00_re;  \
    c##00_im = a##00_im + scalar*b##00_im;  \
    c##01_re = a##01_re + scalar*b##01_re;  \
    c##01_im = a##01_im + scalar*b##01_im;  \
    c##02_re = a##02_re + scalar*b##02_re;  \
    c##02_im = a##02_im + scalar*b##02_im;  \
    c##10_re = a##10_re + scalar*b##10_re;  \
    c##10_im = a##10_im + scalar*b##10_im;  \
    c##11_re = a##11_re + scalar*b##11_re;  \
    c##11_im = a##11_im + scalar*b##11_im;  \
    c##12_re = a##12_re + scalar*b##12_re;  \
    c##12_im = a##12_im + scalar*b##12_im;  \
    c##20_re = a##20_re + scalar*b##20_re;  \
    c##20_im = a##20_im + scalar*b##20_im;  \
    c##21_re = a##21_re + scalar*b##21_re;  \
    c##21_im = a##21_im + scalar*b##21_im;  \
    c##22_re = a##22_re + scalar*b##22_re;  \
    c##22_im = a##22_im + scalar*b##22_im;  \
}while(0)

#endif // GENERIC_MATRIX_MACROS

#endif // _HISQ_FORCE_MACROS_H_
