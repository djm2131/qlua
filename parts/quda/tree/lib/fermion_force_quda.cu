
#include <dslash_quda.h>
#include <read_gauge.h>
#include <gauge_field.h>
#include <clover_field.h>

#include <fermion_force_quda.h>
#include <force_common.h>
#include <hw_quda.h>

#if defined(GPU_FERMION_FORCE)
namespace quda {


  namespace fermionforce {
#include <dslash_constants.h>
#include <dslash_textures.h>
  }

  using namespace fermionforce;

#define BLOCK_DIM 64

#define LOAD_ANTI_HERMITIAN(src, dir, idx, var) LOAD_ANTI_HERMITIAN_DIRECT(src, dir, idx, var, Vh)

#define LOAD_HW_SINGLE(hw_even, hw_odd, idx, var, oddness)	do{	\
    Float2* hw = (oddness)?hw_odd:hw_even;				\
    var##0 = hw[idx + 0*Vh];						\
    var##1 = hw[idx + 1*Vh];						\
    var##2 = hw[idx + 2*Vh];						\
    var##3 = hw[idx + 3*Vh];						\
    var##4 = hw[idx + 4*Vh];						\
    var##5 = hw[idx + 5*Vh];						\
  }while(0)

#define WRITE_HW_SINGLE(hw_even, hw_odd, idx, var, oddness)	do{	\
    Float2* hw = (oddness)?hw_odd:hw_even;				\
    hw[idx + 0*Vh] = var##0;						\
    hw[idx + 1*Vh] = var##1;						\
    hw[idx + 2*Vh] = var##2;						\
    hw[idx + 3*Vh] = var##3;						\
    hw[idx + 4*Vh] = var##4;						\
    hw[idx + 5*Vh] = var##5;						\
  }while(0)

#define LOAD_HW(hw_eve, hw_odd, idx, var, oddness) LOAD_HW_SINGLE(hw_eve, hw_odd, idx, var, oddness)
#define WRITE_HW(hw_even, hw_odd, idx, var, oddness) WRITE_HW_SINGLE(hw_even, hw_odd, idx, var, oddness)
#define LOAD_MATRIX(src, dir, idx, var) LOAD_MATRIX_12_SINGLE(src, dir, idx, var, Vh)

#define FF_SITE_MATRIX_LOAD_TEX 1

#define linkEvenTex siteLink0TexSingle_recon
#define linkOddTex siteLink1TexSingle_recon

#if (FF_SITE_MATRIX_LOAD_TEX == 1)
#define FF_LOAD_MATRIX(dir, idx, var, oddness) LOAD_MATRIX_12_SINGLE_TEX(((oddness)?linkOddTex:linkEvenTex), dir, idx, var, Vh)
#else
#define FF_LOAD_MATRIX(dir, idx, var, oddness) LOAD_MATRIX_12_SINGLE(((oddness)?linkOdd:linkEven), dir, idx, var, Vh)
#endif


#define linka00_re LINKA0.x
#define linka00_im LINKA0.y
#define linka01_re LINKA0.z
#define linka01_im LINKA0.w
#define linka02_re LINKA1.x
#define linka02_im LINKA1.y
#define linka10_re LINKA1.z
#define linka10_im LINKA1.w
#define linka11_re LINKA2.x
#define linka11_im LINKA2.y
#define linka12_re LINKA2.z
#define linka12_im LINKA2.w
#define linka20_re LINKA3.x
#define linka20_im LINKA3.y
#define linka21_re LINKA3.z
#define linka21_im LINKA3.w
#define linka22_re LINKA4.x
#define linka22_im LINKA4.y

#define linkb00_re LINKB0.x
#define linkb00_im LINKB0.y
#define linkb01_re LINKB0.z
#define linkb01_im LINKB0.w
#define linkb02_re LINKB1.x
#define linkb02_im LINKB1.y
#define linkb10_re LINKB1.z
#define linkb10_im LINKB1.w
#define linkb11_re LINKB2.x
#define linkb11_im LINKB2.y
#define linkb12_re LINKB2.z
#define linkb12_im LINKB2.w
#define linkb20_re LINKB3.x
#define linkb20_im LINKB3.y
#define linkb21_re LINKB3.z
#define linkb21_im LINKB3.w
#define linkb22_re LINKB4.x
#define linkb22_im LINKB4.y


#define MAT_MUL_HW(M, HW, HWOUT)				\
  HWOUT##00_re = (M##00_re * HW##00_re - M##00_im * HW##00_im)	\
    +          (M##01_re * HW##01_re - M##01_im * HW##01_im)	\
    +          (M##02_re * HW##02_re - M##02_im * HW##02_im);	\
  HWOUT##00_im = (M##00_re * HW##00_im + M##00_im * HW##00_re)	\
    +          (M##01_re * HW##01_im + M##01_im * HW##01_re)	\
    +          (M##02_re * HW##02_im + M##02_im * HW##02_re);	\
  HWOUT##01_re = (M##10_re * HW##00_re - M##10_im * HW##00_im)	\
    +          (M##11_re * HW##01_re - M##11_im * HW##01_im)	\
    +          (M##12_re * HW##02_re - M##12_im * HW##02_im);	\
  HWOUT##01_im = (M##10_re * HW##00_im + M##10_im * HW##00_re) 	\
    +          (M##11_re * HW##01_im + M##11_im * HW##01_re)	\
    +          (M##12_re * HW##02_im + M##12_im * HW##02_re);	\
  HWOUT##02_re = (M##20_re * HW##00_re - M##20_im * HW##00_im)	\
    +          (M##21_re * HW##01_re - M##21_im * HW##01_im)	\
    +          (M##22_re * HW##02_re - M##22_im * HW##02_im);	\
  HWOUT##02_im = (M##20_re * HW##00_im + M##20_im * HW##00_re)	\
    +          (M##21_re * HW##01_im + M##21_im * HW##01_re)	\
    +          (M##22_re * HW##02_im + M##22_im * HW##02_re);	\
  HWOUT##10_re = (M##00_re * HW##10_re - M##00_im * HW##10_im)	\
    +          (M##01_re * HW##11_re - M##01_im * HW##11_im)	\
    +          (M##02_re * HW##12_re - M##02_im * HW##12_im);	\
  HWOUT##10_im = (M##00_re * HW##10_im + M##00_im * HW##10_re)	\
    +          (M##01_re * HW##11_im + M##01_im * HW##11_re)	\
    +          (M##02_re * HW##12_im + M##02_im * HW##12_re);	\
  HWOUT##11_re = (M##10_re * HW##10_re - M##10_im * HW##10_im)	\
    +          (M##11_re * HW##11_re - M##11_im * HW##11_im)	\
    +          (M##12_re * HW##12_re - M##12_im * HW##12_im);	\
  HWOUT##11_im = (M##10_re * HW##10_im + M##10_im * HW##10_re) 	\
    +          (M##11_re * HW##11_im + M##11_im * HW##11_re)	\
    +          (M##12_re * HW##12_im + M##12_im * HW##12_re);	\
  HWOUT##12_re = (M##20_re * HW##10_re - M##20_im * HW##10_im)	\
    +          (M##21_re * HW##11_re - M##21_im * HW##11_im)	\
    +          (M##22_re * HW##12_re - M##22_im * HW##12_im);	\
  HWOUT##12_im = (M##20_re * HW##10_im + M##20_im * HW##10_re)	\
    +          (M##21_re * HW##11_im + M##21_im * HW##11_re)	\
    +          (M##22_re * HW##12_im + M##22_im * HW##12_re);


#define ADJ_MAT_MUL_HW(M, HW, HWOUT)				\
  HWOUT##00_re = (M##00_re * HW##00_re + M##00_im * HW##00_im)	\
    +          (M##10_re * HW##01_re + M##10_im * HW##01_im)	\
    +          (M##20_re * HW##02_re + M##20_im * HW##02_im);	\
  HWOUT##00_im = (M##00_re * HW##00_im - M##00_im * HW##00_re)	\
    +          (M##10_re * HW##01_im - M##10_im * HW##01_re)	\
    +          (M##20_re * HW##02_im - M##20_im * HW##02_re);	\
  HWOUT##01_re = (M##01_re * HW##00_re + M##01_im * HW##00_im)	\
    +          (M##11_re * HW##01_re + M##11_im * HW##01_im)	\
    +          (M##21_re * HW##02_re + M##21_im * HW##02_im);	\
  HWOUT##01_im = (M##01_re * HW##00_im - M##01_im * HW##00_re)	\
    +          (M##11_re * HW##01_im - M##11_im * HW##01_re)	\
    +          (M##21_re * HW##02_im - M##21_im * HW##02_re);	\
  HWOUT##02_re = (M##02_re * HW##00_re + M##02_im * HW##00_im)	\
    +          (M##12_re * HW##01_re + M##12_im * HW##01_im)	\
    +          (M##22_re * HW##02_re + M##22_im * HW##02_im);	\
  HWOUT##02_im = (M##02_re * HW##00_im - M##02_im * HW##00_re)	\
    +          (M##12_re * HW##01_im - M##12_im * HW##01_re)	\
    +          (M##22_re * HW##02_im - M##22_im * HW##02_re);	\
  HWOUT##10_re = (M##00_re * HW##10_re + M##00_im * HW##10_im)	\
    +          (M##10_re * HW##11_re + M##10_im * HW##11_im)	\
    +          (M##20_re * HW##12_re + M##20_im * HW##12_im);	\
  HWOUT##10_im = (M##00_re * HW##10_im - M##00_im * HW##10_re)	\
    +          (M##10_re * HW##11_im - M##10_im * HW##11_re)	\
    +          (M##20_re * HW##12_im - M##20_im * HW##12_re);	\
  HWOUT##11_re = (M##01_re * HW##10_re + M##01_im * HW##10_im)	\
    +          (M##11_re * HW##11_re + M##11_im * HW##11_im)	\
    +          (M##21_re * HW##12_re + M##21_im * HW##12_im);	\
  HWOUT##11_im = (M##01_re * HW##10_im - M##01_im * HW##10_re)	\
    +          (M##11_re * HW##11_im - M##11_im * HW##11_re)	\
    +          (M##21_re * HW##12_im - M##21_im * HW##12_re);	\
  HWOUT##12_re = (M##02_re * HW##10_re + M##02_im * HW##10_im)	\
    +          (M##12_re * HW##11_re + M##12_im * HW##11_im)	\
    +          (M##22_re * HW##12_re + M##22_im * HW##12_im);	\
  HWOUT##12_im = (M##02_re * HW##10_im - M##02_im * HW##10_re)	\
    +          (M##12_re * HW##11_im - M##12_im * HW##11_re)	\
    +          (M##22_re * HW##12_im - M##22_im * HW##12_re);


#define SU3_PROJECTOR(va, vb, m)			\
  m##00_re = va##0_re * vb##0_re + va##0_im * vb##0_im;	\
  m##00_im = va##0_im * vb##0_re - va##0_re * vb##0_im;	\
  m##01_re = va##0_re * vb##1_re + va##0_im * vb##1_im;	\
  m##01_im = va##0_im * vb##1_re - va##0_re * vb##1_im;	\
  m##02_re = va##0_re * vb##2_re + va##0_im * vb##2_im;	\
  m##02_im = va##0_im * vb##2_re - va##0_re * vb##2_im;	\
  m##10_re = va##1_re * vb##0_re + va##1_im * vb##0_im;	\
  m##10_im = va##1_im * vb##0_re - va##1_re * vb##0_im;	\
  m##11_re = va##1_re * vb##1_re + va##1_im * vb##1_im;	\
  m##11_im = va##1_im * vb##1_re - va##1_re * vb##1_im;	\
  m##12_re = va##1_re * vb##2_re + va##1_im * vb##2_im;	\
  m##12_im = va##1_im * vb##2_re - va##1_re * vb##2_im;	\
  m##20_re = va##2_re * vb##0_re + va##2_im * vb##0_im;	\
  m##20_im = va##2_im * vb##0_re - va##2_re * vb##0_im;	\
  m##21_re = va##2_re * vb##1_re + va##2_im * vb##1_im;	\
  m##21_im = va##2_im * vb##1_re - va##2_re * vb##1_im;	\
  m##22_re = va##2_re * vb##2_re + va##2_im * vb##2_im;	\
  m##22_im = va##2_im * vb##2_re - va##2_re * vb##2_im;

  //vc = va + vb*s 
#define SCALAR_MULT_ADD_SU3_VECTOR(va, vb, s, vc) do {	\
    vc##0_re = va##0_re + vb##0_re * s;			\
    vc##0_im = va##0_im + vb##0_im * s;			\
    vc##1_re = va##1_re + vb##1_re * s;			\
    vc##1_im = va##1_im + vb##1_im * s;			\
    vc##2_re = va##2_re + vb##2_re * s;			\
    vc##2_im = va##2_im + vb##2_im * s;			\
  }while (0)


#define FF_COMPUTE_NEW_FULL_IDX_PLUS_UPDATE(mydir, idx, new_idx) do {	\
    switch(mydir){							\
    case 0:								\
      new_idx = ( (new_x1==X1m1)?idx-X1m1:idx+1);			\
      new_x1 = (new_x1==X1m1)?0:new_x1+1;				\
      break;								\
    case 1:								\
      new_idx = ( (new_x2==X2m1)?idx-X2X1mX1:idx+X1);			\
      new_x2 = (new_x2==X2m1)?0:new_x2+1;				\
      break;								\
    case 2:								\
      new_idx = ( (new_x3==X3m1)?idx-X3X2X1mX2X1:idx+X2X1);		\
      new_x3 = (new_x3==X3m1)?0:new_x3+1;				\
      break;								\
    case 3:								\
      new_idx = ( (new_x4==X4m1)?idx-X4X3X2X1mX3X2X1:idx+X3X2X1);	\
      new_x4 = (new_x4==X4m1)?0:new_x4+1;				\
      break;								\
    }									\
  }while(0)

#define FF_COMPUTE_NEW_FULL_IDX_MINUS_UPDATE(mydir, idx, new_idx) do {	\
    switch(mydir){							\
    case 0:								\
      new_idx = ( (new_x1==0)?idx+X1m1:idx-1);				\
      new_x1 = (new_x1==0)?X1m1:new_x1 - 1;				\
      break;								\
    case 1:								\
      new_idx = ( (new_x2==0)?idx+X2X1mX1:idx-X1);			\
      new_x2 = (new_x2==0)?X2m1:new_x2 - 1;				\
      break;								\
    case 2:								\
      new_idx = ( (new_x3==0)?idx+X3X2X1mX2X1:idx-X2X1);		\
      new_x3 = (new_x3==0)?X3m1:new_x3 - 1;				\
      break;								\
    case 3:								\
      new_idx = ( (new_x4==0)?idx+X4X3X2X1mX3X2X1:idx-X3X2X1);		\
      new_x4 = (new_x4==0)?X4m1:new_x4 - 1;				\
      break;								\
    }									\
  }while(0)


#define FF_COMPUTE_NEW_FULL_IDX_PLUS(old_x1, old_x2, old_x3, old_x4, idx, mydir, new_idx) do { \
    switch(mydir){							\
    case 0:								\
      new_idx = ( (old_x1==X1m1)?idx-X1m1:idx+1);			\
      break;								\
    case 1:								\
      new_idx = ( (old_x2==X2m1)?idx-X2X1mX1:idx+X1);			\
      break;								\
    case 2:								\
      new_idx = ( (old_x3==X3m1)?idx-X3X2X1mX2X1:idx+X2X1);		\
      break;								\
    case 3:								\
      new_idx = ( (old_x4==X4m1)?idx-X4X3X2X1mX3X2X1:idx+X3X2X1);	\
      break;								\
    }									\
  }while(0)

#define FF_COMPUTE_NEW_FULL_IDX_MINUS(old_x1, old_x2, old_x3, old_x4, idx, mydir, new_idx) do { \
    switch(mydir){							\
    case 0:								\
      new_idx = ( (old_x1==0)?idx+X1m1:idx-1);				\
      break;								\
    case 1:								\
      new_idx = ( (old_x2==0)?idx+X2X1mX1:idx-X1);			\
      break;								\
    case 2:								\
      new_idx = ( (old_x3==0)?idx+X3X2X1mX2X1:idx-X2X1);		\
      break;								\
    case 3:								\
      new_idx = ( (old_x4==0)?idx+X4X3X2X1mX3X2X1:idx-X3X2X1);		\
      break;								\
    }									\
  }while(0)

  //this macro require linka, linkb, and ah variables defined
#define ADD_FORCE_TO_MOM(hw1, hw2, idx, dir, cf,oddness) do{		\
    Float2 my_coeff;							\
    int mydir;								\
    if (GOES_BACKWARDS(dir)){						\
      mydir=OPP_DIR(dir);						\
      my_coeff.x = -cf.x;						\
      my_coeff.y = -cf.y;						\
    }else{								\
      mydir=dir;							\
      my_coeff.x = cf.x;						\
      my_coeff.y = cf.y;						\
    }									\
    Float2 tmp_coeff;							\
    tmp_coeff.x = my_coeff.x;						\
    tmp_coeff.y = my_coeff.y;						\
    if(oddness){							\
      tmp_coeff.x = - my_coeff.x;					\
      tmp_coeff.y = - my_coeff.y;					\
    }									\
    Float2* mom = oddness?momOdd:momEven;				\
    LOAD_ANTI_HERMITIAN(mom, mydir, idx, AH);				\
    UNCOMPRESS_ANTI_HERMITIAN(ah, linka);				\
    SU3_PROJECTOR(hw1##0, hw2##0, linkb);				\
    SCALAR_MULT_ADD_SU3_MATRIX(linka, linkb, tmp_coeff.x, linka);	\
    SU3_PROJECTOR(hw1##1, hw2##1, linkb);				\
    SCALAR_MULT_ADD_SU3_MATRIX(linka, linkb, tmp_coeff.y, linka);	\
    MAKE_ANTI_HERMITIAN(linka, ah);					\
    WRITE_ANTI_HERMITIAN(mom, mydir, idx, AH, Vh);			\
  }while(0)


#define FF_COMPUTE_RECONSTRUCT_SIGN(sign, dir, i1,i2,i3,i4) do {        \
    sign =1;								\
    switch(dir){							\
    case XUP:								\
      if ( (i4 & 1) == 1){						\
	sign = -1;							\
      }									\
      break;								\
    case YUP:								\
      if ( ((i4+i1) & 1) == 1){						\
	sign = -1;							\
      }									\
      break;								\
    case ZUP:								\
      if ( ((i4+i1+i2) & 1) == 1){					\
	sign = -1;							\
      }									\
      break;								\
    case TUP:								\
      if (i4 == X4m1 ){							\
	sign = -1;							\
      }									\
      break;								\
    }									\
  }while (0)


#define hwa00_re HWA0.x
#define hwa00_im HWA0.y
#define hwa01_re HWA1.x
#define hwa01_im HWA1.y
#define hwa02_re HWA2.x
#define hwa02_im HWA2.y
#define hwa10_re HWA3.x
#define hwa10_im HWA3.y
#define hwa11_re HWA4.x
#define hwa11_im HWA4.y
#define hwa12_re HWA5.x
#define hwa12_im HWA5.y

#define hwb00_re HWB0.x
#define hwb00_im HWB0.y
#define hwb01_re HWB1.x
#define hwb01_im HWB1.y
#define hwb02_re HWB2.x
#define hwb02_im HWB2.y
#define hwb10_re HWB3.x
#define hwb10_im HWB3.y
#define hwb11_re HWB4.x
#define hwb11_im HWB4.y
#define hwb12_re HWB5.x
#define hwb12_im HWB5.y

#define hwc00_re HWC0.x
#define hwc00_im HWC0.y
#define hwc01_re HWC1.x
#define hwc01_im HWC1.y
#define hwc02_re HWC2.x
#define hwc02_im HWC2.y
#define hwc10_re HWC3.x
#define hwc10_im HWC3.y
#define hwc11_re HWC4.x
#define hwc11_im HWC4.y
#define hwc12_re HWC5.x
#define hwc12_im HWC5.y

#define hwd00_re HWD0.x
#define hwd00_im HWD0.y
#define hwd01_re HWD1.x
#define hwd01_im HWD1.y
#define hwd02_re HWD2.x
#define hwd02_im HWD2.y
#define hwd10_re HWD3.x
#define hwd10_im HWD3.y
#define hwd11_re HWD4.x
#define hwd11_im HWD4.y
#define hwd12_re HWD5.x
#define hwd12_im HWD5.y

#define hwe00_re HWE0.x
#define hwe00_im HWE0.y
#define hwe01_re HWE1.x
#define hwe01_im HWE1.y
#define hwe02_re HWE2.x
#define hwe02_im HWE2.y
#define hwe10_re HWE3.x
#define hwe10_im HWE3.y
#define hwe11_re HWE4.x
#define hwe11_im HWE4.y
#define hwe12_re HWE5.x
#define hwe12_im HWE5.y


  void fermion_force_init_cuda(QudaGaugeParam* param)
  {
  
#ifdef MULTI_GPU
#error "multi gpu is not supported for fermion force computation"  
#endif

    static int fermion_force_init_cuda_flag = 0; 
  
    if (fermion_force_init_cuda_flag) return;
  
    fermion_force_init_cuda_flag=1;

  }

  /*
   * This function computes contribution to mometum from the middle link in a staple
   *
   * tempx:    IN
   * Pmu:      OUT
   * P3:       OUT
   *
   */

  template<int sig_positive, int mu_positive, int oddBit, typename Float2> 
  __global__ void
  do_middle_link_kernel(Float2* tempxEven, Float2* tempxOdd, 
			Float2* PmuEven, Float2* PmuOdd,
			Float2* P3Even, Float2* P3Odd,
			int sig, int mu, Float2 coeff,
			float4* linkEven, float4* linkOdd,
			Float2* momEven, Float2* momOdd)
  {
    int sid = blockIdx.x * blockDim.x + threadIdx.x;
    
    int z1 = sid / X1h;
    int x1h = sid - z1*X1h;
    int z2 = z1 / X2;
    int x2 = z1 - z2*X2;
    int x4 = z2 / X3;
    int x3 = z2 - x4*X3;
    int x1odd = (x2 + x3 + x4 + oddBit) & 1;
    int x1 = 2*x1h + x1odd;
    int X = 2*sid + x1odd;

    int new_x1, new_x2, new_x3, new_x4;
    int new_mem_idx;
    int ad_link_sign=1;
    int ab_link_sign=1;
    int bc_link_sign=1;
    
    Float2 HWA0, HWA1, HWA2, HWA3, HWA4, HWA5;
    Float2 HWB0, HWB1, HWB2, HWB3, HWB4, HWB5;
    Float2 HWC0, HWC1, HWC2, HWC3, HWC4, HWC5;
    Float2 HWD0, HWD1, HWD2, HWD3, HWD4, HWD5;
    float4 LINKA0, LINKA1, LINKA2, LINKA3, LINKA4;
    float4 LINKB0, LINKB1, LINKB2, LINKB3, LINKB4;
    Float2 AH0, AH1, AH2, AH3, AH4;
  
    /*         sig
     *	          A________B
     *	     mu   |        |
     *	        D |        |C
     *	     
     *	     A is the current point (sid)
     */
  
    int point_b, point_c, point_d;
    int ad_link_nbr_idx, ab_link_nbr_idx, bc_link_nbr_idx;
    int mymu;
 
    new_x1 = x1;
    new_x2 = x2;
    new_x3 = x3;
    new_x4 = x4;
    
    if(mu_positive){
      mymu =mu;
      FF_COMPUTE_NEW_FULL_IDX_MINUS_UPDATE(mu, X, new_mem_idx);
    }else{
      mymu = OPP_DIR(mu);
      FF_COMPUTE_NEW_FULL_IDX_PLUS_UPDATE(OPP_DIR(mu), X, new_mem_idx);	
    }
    point_d = (new_mem_idx >> 1);
    if (mu_positive){
      ad_link_nbr_idx = point_d;
      FF_COMPUTE_RECONSTRUCT_SIGN(ad_link_sign, mymu, new_x1,new_x2,new_x3,new_x4);
    }else{
      ad_link_nbr_idx = sid;
      FF_COMPUTE_RECONSTRUCT_SIGN(ad_link_sign, mymu, x1, x2, x3, x4);	
    }
    
    int mysig; 
    if(sig_positive){
      mysig = sig;
      FF_COMPUTE_NEW_FULL_IDX_PLUS_UPDATE(sig, new_mem_idx, new_mem_idx);
    }else{
      mysig = OPP_DIR(sig);
      FF_COMPUTE_NEW_FULL_IDX_MINUS_UPDATE(OPP_DIR(sig), new_mem_idx, new_mem_idx);	
    }
    point_c = (new_mem_idx >> 1);
    if (mu_positive){
      bc_link_nbr_idx = point_c;	
      FF_COMPUTE_RECONSTRUCT_SIGN(bc_link_sign, mymu, new_x1,new_x2,new_x3,new_x4);
    }
    new_x1 = x1;
    new_x2 = x2;
    new_x3 = x3;
    new_x4 = x4;
    if(sig_positive){
      FF_COMPUTE_NEW_FULL_IDX_PLUS_UPDATE(sig, X, new_mem_idx);
    }else{
      FF_COMPUTE_NEW_FULL_IDX_MINUS_UPDATE(OPP_DIR(sig), X, new_mem_idx);	
    }
    point_b = (new_mem_idx >> 1); 
    
    if (!mu_positive){
      bc_link_nbr_idx = point_b;
      FF_COMPUTE_RECONSTRUCT_SIGN(bc_link_sign, mymu, new_x1,new_x2,new_x3,new_x4);
    }   
    
    if(sig_positive){
      ab_link_nbr_idx = sid;
      FF_COMPUTE_RECONSTRUCT_SIGN(ab_link_sign, mysig, x1, x2, x3, x4);	
    }else{	
      ab_link_nbr_idx = point_b;
      FF_COMPUTE_RECONSTRUCT_SIGN(ab_link_sign, mysig, new_x1,new_x2,new_x3,new_x4);
    }
    
    LOAD_HW(tempxEven, tempxOdd, point_d, HWA, 1-oddBit );
    if(mu_positive){
      FF_LOAD_MATRIX(mymu, ad_link_nbr_idx, LINKA, 1-oddBit);
    }else{
      FF_LOAD_MATRIX(mymu, ad_link_nbr_idx, LINKA, oddBit);
    }

    RECONSTRUCT_LINK_12(ad_link_sign, linka);	
    if (mu_positive){
      ADJ_MAT_MUL_HW(linka, hwa, hwd);
    }else{
      MAT_MUL_HW(linka, hwa, hwd);
    }
    WRITE_HW(PmuEven,PmuOdd, sid, HWD, oddBit);	
        
    LOAD_HW(tempxEven,tempxOdd, point_c, HWA, oddBit);
    if(mu_positive){
      FF_LOAD_MATRIX(mymu, bc_link_nbr_idx, LINKA, oddBit);
    }else{
      FF_LOAD_MATRIX(mymu, bc_link_nbr_idx, LINKA, 1-oddBit);
    }
    
    RECONSTRUCT_LINK_12(bc_link_sign, linka);
    if (mu_positive){    
      ADJ_MAT_MUL_HW(linka, hwa, hwb);
    }else{
      MAT_MUL_HW(linka, hwa, hwb);
    }
    if(sig_positive){
      FF_LOAD_MATRIX(mysig, ab_link_nbr_idx, LINKB, oddBit);
    }else{
      FF_LOAD_MATRIX(mysig, ab_link_nbr_idx, LINKB, 1-oddBit);
    }
    
    RECONSTRUCT_LINK_12(ab_link_sign, linkb);
    if (sig_positive){        
      MAT_MUL_HW(linkb, hwb, hwc);
    }else{
      ADJ_MAT_MUL_HW(linkb, hwb, hwc);
    }
    WRITE_HW(P3Even, P3Odd, sid, HWC, oddBit);
    
    if (sig_positive){	
      //add the force to mom	
      ADD_FORCE_TO_MOM(hwc, hwd, sid, sig, coeff, oddBit);
    }
  }


  template<typename Float2>
  static void
  middle_link_kernel(Float2* tempxEven, Float2* tempxOdd, 
		     Float2* PmuEven, Float2* PmuOdd,
		     Float2* P3Even, Float2* P3Odd,
		     int sig, int mu, Float2 coeff,
		     float4* linkEven, float4* linkOdd, cudaGaugeField &siteLink,
		     Float2* momEven, Float2* momOdd,
		     dim3 gridDim, dim3 BlockDim)
  {
    dim3 halfGridDim(gridDim.x/2, 1,1);

   
#define CALL_MIDDLE_LINK_KERNEL(sig_sign, mu_sign)			\
    do_middle_link_kernel<sig_sign, mu_sign,0><<<halfGridDim, BlockDim>>>( tempxEven,  tempxOdd, \
									   PmuEven,  PmuOdd, \
									   P3Even,  P3Odd, \
									   sig, mu, coeff, \
									   linkEven, linkOdd, \
									   momEven,  momOdd); \
    do_middle_link_kernel<sig_sign, mu_sign, 1><<<halfGridDim, BlockDim>>>(tempxEven,  tempxOdd, \
									   PmuEven,  PmuOdd, \
									   P3Even,  P3Odd, \
									   sig, mu, coeff, \
									   linkEven, linkOdd, \
									   momEven, momOdd); 
  
  
    if (GOES_FORWARDS(sig) && GOES_FORWARDS(mu)){	
      CALL_MIDDLE_LINK_KERNEL(1, 1);
    }else if (GOES_FORWARDS(sig) && GOES_BACKWARDS(mu)){
      CALL_MIDDLE_LINK_KERNEL(1, 0);
    }else if (GOES_BACKWARDS(sig) && GOES_FORWARDS(mu)){
      CALL_MIDDLE_LINK_KERNEL(0, 1);
    }else{
      CALL_MIDDLE_LINK_KERNEL(0, 0);
    }
#undef CALL_MIDDLE_LINK_KERNEL

  }

  /*
   * Computes contribution to momentum from the side links in a staple
   *
   *  P3:       IN
   *  P3mu:     not used
   *  Tempx:    IN
   *  Pmu:      IN
   *  shortPE:  OUT
   *
   */

  template<int sig_positive, int mu_positive, int oddBit, typename Float2>
  __global__ void
  do_side_link_kernel(Float2* P3Even, Float2* P3Odd, 
		      Float2* P3muEven, Float2* P3muOdd,
		      Float2* TempxEven, Float2* TempxOdd,
		      Float2* PmuEven,  Float2* PmuOdd,
		      Float2* shortPEven,  Float2* shortPOdd,
		      int sig, int mu, Float2 coeff, Float2 accumu_coeff,
		      float4* linkEven, float4* linkOdd,
		      Float2* momEven, Float2* momOdd)
  {
    Float2 mcoeff;
    mcoeff.x = -coeff.x;
    mcoeff.y = -coeff.y;
    
    int sid = blockIdx.x * blockDim.x + threadIdx.x;
    
    int z1 = sid / X1h;
    int x1h = sid - z1*X1h;
    int z2 = z1 / X2;
    int x2 = z1 - z2*X2;
    int x4 = z2 / X3;
    int x3 = z2 - x4*X3;
    int x1odd = (x2 + x3 + x4 + oddBit) & 1;
    int x1 = 2*x1h + x1odd;
    int X = 2*sid + x1odd;

    int ad_link_sign = 1;
    Float2 HWA0, HWA1, HWA2, HWA3, HWA4, HWA5;
    Float2 HWB0, HWB1, HWB2, HWB3, HWB4, HWB5;
    Float2 HWC0, HWC1, HWC2, HWC3, HWC4, HWC5;
    float4 LINKA0, LINKA1, LINKA2, LINKA3, LINKA4;
    float4 LINKB0, LINKB1, LINKB2, LINKB3, LINKB4;
    Float2 AH0, AH1, AH2, AH3, AH4;
  

    
    /*    
     * 	  compute the side link contribution to the momentum
     *
     *  
     *         sig
     *      A________B
     *      |        |   mu
     *    D |        |C
     *
     *   A is the current point (sid)
     */
  
    int point_d;
    int ad_link_nbr_idx;
    int mymu;
    int new_mem_idx;

    int new_x1 = x1;
    int new_x2 = x2;
    int new_x3 = x3;
    int new_x4 = x4;

    if(mu_positive){
      mymu =mu;
      FF_COMPUTE_NEW_FULL_IDX_MINUS_UPDATE(mymu,X, new_mem_idx);
    }else{
      mymu = OPP_DIR(mu);
      FF_COMPUTE_NEW_FULL_IDX_PLUS_UPDATE(mymu, X, new_mem_idx);
    }
    point_d = (new_mem_idx >> 1);
    
    if (mu_positive){
      ad_link_nbr_idx = point_d;
      FF_COMPUTE_RECONSTRUCT_SIGN(ad_link_sign, mymu, new_x1,new_x2,new_x3,new_x4);
    }else{
      ad_link_nbr_idx = sid;
      FF_COMPUTE_RECONSTRUCT_SIGN(ad_link_sign, mymu, x1, x2, x3, x4);	
    }

    
    LOAD_HW(P3Even, P3Odd, sid, HWA, oddBit);
    if(mu_positive){
      FF_LOAD_MATRIX(mymu, ad_link_nbr_idx, LINKA, 1 - oddBit);
    }else{
      FF_LOAD_MATRIX(mymu, ad_link_nbr_idx, LINKA, oddBit);
    }

    RECONSTRUCT_LINK_12(ad_link_sign, linka);	
    if (mu_positive){
      MAT_MUL_HW(linka, hwa, hwb);
    }else{
      ADJ_MAT_MUL_HW(linka, hwa, hwb);
    }

    
    //start to add side link force
    if (mu_positive){
      LOAD_HW(TempxEven, TempxOdd, point_d, HWC, 1-oddBit);
	
      if (sig_positive){
	ADD_FORCE_TO_MOM(hwb, hwc, point_d, mu, coeff, 1-oddBit);
      }else{
	ADD_FORCE_TO_MOM(hwc, hwb, point_d, OPP_DIR(mu), mcoeff, 1- oddBit);	    
      }
    }else{
      LOAD_HW(PmuEven, PmuOdd, sid, HWC, oddBit);
      if (sig_positive){
	ADD_FORCE_TO_MOM(hwa, hwc, sid, mu, mcoeff, oddBit);
      }else{
	ADD_FORCE_TO_MOM(hwc, hwa, sid, OPP_DIR(mu), coeff, oddBit);	    
      }
	
    }
    
    if (shortPOdd){
      LOAD_HW(shortPEven, shortPOdd, point_d, HWA, 1-oddBit);
      SCALAR_MULT_ADD_SU3_VECTOR(hwa0, hwb0, accumu_coeff.x, hwa0);
      SCALAR_MULT_ADD_SU3_VECTOR(hwa1, hwb1, accumu_coeff.y, hwa1);
      WRITE_HW(shortPEven, shortPOdd, point_d, HWA, 1-oddBit);
    }

  }


  template<typename Float2>
  static void
  side_link_kernel(Float2* P3Even, Float2* P3Odd, 
		   Float2* P3muEven, Float2* P3muOdd,
		   Float2* TempxEven, Float2* TempxOdd,
		   Float2* PmuEven,  Float2* PmuOdd,
		   Float2* shortPEven,  Float2* shortPOdd,
		   int sig, int mu, Float2 coeff, Float2 accumu_coeff,
		   float4* linkEven, float4* linkOdd, cudaGaugeField &siteLink,
		   Float2* momEven, Float2* momOdd,
		   dim3 gridDim, dim3 blockDim)
  {
    dim3 halfGridDim(gridDim.x/2,1,1);
  
#define CALL_SIDE_LINK_KERNEL(sig_sign, mu_sign)			\
    do_side_link_kernel<sig_sign,mu_sign,0><<<halfGridDim, blockDim>>>( P3Even,  P3Odd, \
									P3muEven,  P3muOdd, \
									TempxEven,  TempxOdd, \
									PmuEven,   PmuOdd, \
									shortPEven,   shortPOdd, \
									sig, mu, coeff, accumu_coeff, \
									linkEven, linkOdd, \
									momEven, momOdd); \
    do_side_link_kernel<sig_sign,mu_sign,1><<<halfGridDim, blockDim>>>( P3Even,  P3Odd, \
									P3muEven,  P3muOdd, \
									TempxEven,  TempxOdd, \
									PmuEven,   PmuOdd, \
									shortPEven,   shortPOdd, \
									sig, mu, coeff, accumu_coeff, \
									linkEven, linkOdd, \
									momEven, momOdd);    
  
    if (GOES_FORWARDS(sig) && GOES_FORWARDS(mu)){
      CALL_SIDE_LINK_KERNEL(1,1);  	
    }else if (GOES_FORWARDS(sig) && GOES_BACKWARDS(mu)){
      CALL_SIDE_LINK_KERNEL(1,0);  			
    }else if (GOES_BACKWARDS(sig) && GOES_FORWARDS(mu)){
      CALL_SIDE_LINK_KERNEL(0,1);  				
    }else{
      CALL_SIDE_LINK_KERNEL(0,0);
    }

#undef CALL_SIDE_LINK_KERNEL

  }

  /*
   *  This function computes the contribution to momentum from middle and side links
   *
   *  tempx:  IN
   *  Pmu:    not used
   *  P3:     not used
   *  P3mu:   not used
   *  shortP: OUT 
   *
   */

  template<int sig_positive, int mu_positive, int oddBit, typename Float2>
  __global__ void
  do_all_link_kernel(Float2* tempxEven, Float2* tempxOdd, 
		     Float2* PmuEven, Float2* PmuOdd,
		     Float2* P3Even, Float2* P3Odd,
		     Float2* P3muEven, Float2* P3muOdd,
		     Float2* shortPEven, Float2* shortPOdd,
		     int sig, int mu, Float2 coeff, Float2 mcoeff, Float2 accumu_coeff,
		     float4* linkEven, float4* linkOdd,
		     Float2* momEven, Float2* momOdd)
  {
    int sid = blockIdx.x * blockDim.x + threadIdx.x;

    int z1 = sid / X1h;
    int x1h = sid - z1*X1h;
    int z2 = z1 / X2;
    int x2 = z1 - z2*X2;
    int x4 = z2 / X3;
    int x3 = z2 - x4*X3;
    int x1odd = (x2 + x3 + x4 + oddBit) & 1;
    int x1 = 2*x1h + x1odd;
    int X = 2*sid + x1odd;
    
    int new_x1, new_x2, new_x3, new_x4;
    int ad_link_sign=1;
    int ab_link_sign=1;
    int bc_link_sign=1;   
    
    Float2 HWA0, HWA1, HWA2, HWA3, HWA4, HWA5;
    Float2 HWB0, HWB1, HWB2, HWB3, HWB4, HWB5;
    Float2 HWC0, HWC1, HWC2, HWC3, HWC4, HWC5;
    Float2 HWD0, HWD1, HWD2, HWD3, HWD4, HWD5;
    Float2 HWE0, HWE1, HWE2, HWE3, HWE4, HWE5;
    float4 LINKA0, LINKA1, LINKA2, LINKA3, LINKA4;
    float4 LINKB0, LINKB1, LINKB2, LINKB3, LINKB4;
    float4 LINKC0, LINKC1, LINKC2, LINKC3, LINKC4;
    Float2 AH0, AH1, AH2, AH3, AH4;
  
  
    /*              sig
     *           A________B
     *       mu  |        |
     *         D |        |C
     *
     *        A is the current point (sid)
     */

   
    int point_b, point_c, point_d;
    int ad_link_nbr_idx, ab_link_nbr_idx, bc_link_nbr_idx;
    int mymu;
    int new_mem_idx;
    new_x1 = x1;
    new_x2 = x2;
    new_x3 = x3;
    new_x4 = x4;

    if(mu_positive){
      mymu =mu;
      FF_COMPUTE_NEW_FULL_IDX_MINUS_UPDATE(mu, X, new_mem_idx);
    }else{
      mymu = OPP_DIR(mu);
      FF_COMPUTE_NEW_FULL_IDX_PLUS_UPDATE(OPP_DIR(mu), X, new_mem_idx);	
    }
    point_d = (new_mem_idx >> 1);

    if (mu_positive){
      ad_link_nbr_idx = point_d;
      FF_COMPUTE_RECONSTRUCT_SIGN(ad_link_sign, mymu, new_x1,new_x2,new_x3,new_x4);
    }else{
      ad_link_nbr_idx = sid;
      FF_COMPUTE_RECONSTRUCT_SIGN(ad_link_sign, mymu, x1, x2, x3, x4);	
    }
    
    int mysig; 
    if(sig_positive){
      mysig = sig;
      FF_COMPUTE_NEW_FULL_IDX_PLUS_UPDATE(sig, new_mem_idx, new_mem_idx);
    }else{
      mysig = OPP_DIR(sig);
      FF_COMPUTE_NEW_FULL_IDX_MINUS_UPDATE(OPP_DIR(sig), new_mem_idx, new_mem_idx);	
    }
    point_c = (new_mem_idx >> 1);
    if (mu_positive){
      bc_link_nbr_idx = point_c;	
      FF_COMPUTE_RECONSTRUCT_SIGN(bc_link_sign, mymu, new_x1,new_x2,new_x3,new_x4);
    }
    
    new_x1 = x1;
    new_x2 = x2;
    new_x3 = x3;
    new_x4 = x4;
    if(sig_positive){
      FF_COMPUTE_NEW_FULL_IDX_PLUS_UPDATE(sig, X, new_mem_idx);
    }else{
      FF_COMPUTE_NEW_FULL_IDX_MINUS_UPDATE(OPP_DIR(sig), X, new_mem_idx);	
    }
    point_b = (new_mem_idx >> 1);
    if (!mu_positive){
      bc_link_nbr_idx = point_b;
      FF_COMPUTE_RECONSTRUCT_SIGN(bc_link_sign, mymu, new_x1,new_x2,new_x3,new_x4);
    }      
    
    if(sig_positive){
      ab_link_nbr_idx = sid;
      FF_COMPUTE_RECONSTRUCT_SIGN(ab_link_sign, mysig, x1, x2, x3, x4);	
    }else{	
      ab_link_nbr_idx = point_b;
      FF_COMPUTE_RECONSTRUCT_SIGN(ab_link_sign, mysig, new_x1,new_x2,new_x3,new_x4);
    }

    LOAD_HW(tempxEven, tempxOdd, point_d, HWE, 1-oddBit);
    if (mu_positive){
      FF_LOAD_MATRIX(mymu, ad_link_nbr_idx, LINKC, 1-oddBit);
    }else{
      FF_LOAD_MATRIX(mymu, ad_link_nbr_idx, LINKC, oddBit);
    }
    
    RECONSTRUCT_LINK_12(ad_link_sign, linkc);	
    if (mu_positive){
      ADJ_MAT_MUL_HW(linkc, hwe, hwd);
    }else{
      MAT_MUL_HW(linkc, hwe, hwd);
    }
    //we do not need to write Pmu here
    //WRITE_HW(myPmu, sid, HWD);	
        
    LOAD_HW(tempxEven, tempxOdd, point_c, HWA, oddBit);
    if (mu_positive){
      FF_LOAD_MATRIX(mymu, bc_link_nbr_idx, LINKA, oddBit);
    }else{
      FF_LOAD_MATRIX(mymu, bc_link_nbr_idx, LINKA, 1-oddBit);	
    }

    RECONSTRUCT_LINK_12(bc_link_sign, linka);
    if (mu_positive){    
      ADJ_MAT_MUL_HW(linka, hwa, hwb);
    }else{
      MAT_MUL_HW(linka, hwa, hwb);
    }
    if (sig_positive){
      FF_LOAD_MATRIX(mysig, ab_link_nbr_idx, LINKA, oddBit);
    }else{
      FF_LOAD_MATRIX(mysig, ab_link_nbr_idx, LINKA, 1-oddBit);
    }

    RECONSTRUCT_LINK_12(ab_link_sign, linka);
    if (sig_positive){        
      MAT_MUL_HW(linka, hwb, hwc);
    }else{
      ADJ_MAT_MUL_HW(linka, hwb, hwc);
    }

    //we do not need to write P3 here
    //WRITE_HW(myP3, sid, HWC);
   
    //The middle link contribution
    if (sig_positive){	
      //add the force to mom	
      ADD_FORCE_TO_MOM(hwc, hwd, sid, sig, mcoeff, oddBit);
    }

    //P3 is hwc
    //ad_link is linkc
    if (mu_positive){
      MAT_MUL_HW(linkc, hwc, hwa);
    }else{
      ADJ_MAT_MUL_HW(linkc, hwc, hwa);
    }    
    
    //accumulate P7rho to P5
    //WRITE_HW(otherP3mu, point_d, HWA);	 
    LOAD_HW(shortPEven, shortPOdd, point_d, HWB, 1-oddBit);
    SCALAR_MULT_ADD_SU3_VECTOR(hwb0, hwa0, accumu_coeff.x, hwb0);
    SCALAR_MULT_ADD_SU3_VECTOR(hwb1, hwa1, accumu_coeff.y, hwb1);
    WRITE_HW(shortPEven, shortPOdd, point_d, HWB, 1-oddBit);

    //hwe holds tempx at point_d
    //hwd holds Pmu at point A(sid)
    if (mu_positive){
      if (sig_positive){
	ADD_FORCE_TO_MOM(hwa, hwe, point_d, mu, coeff, 1-oddBit);
      }else{
	ADD_FORCE_TO_MOM(hwe, hwa, point_d, OPP_DIR(mu), mcoeff, 1- oddBit);	    
      }
    }else{
      if (sig_positive){
	ADD_FORCE_TO_MOM(hwc, hwd, sid, mu, mcoeff, oddBit);
      }else{
	ADD_FORCE_TO_MOM(hwd, hwc, sid, OPP_DIR(mu), coeff, oddBit);	    
      }
	
    }  
    

  }



  template<typename Float2>
  static void
  all_link_kernel(Float2* tempxEven, Float2* tempxOdd, 
		  Float2* PmuEven, Float2* PmuOdd,
		  Float2* P3Even, Float2* P3Odd,
		  Float2* P3muEven, Float2* P3muOdd,
		  Float2* shortPEven, Float2* shortPOdd,
		  int sig, int mu, Float2 coeff, Float2 mcoeff, Float2 accumu_coeff,
		  float4* linkEven, float4* linkOdd, cudaGaugeField &siteLink,
		  Float2* momEven, Float2* momOdd,
		  dim3 gridDim, dim3 blockDim)
		   
  {
    dim3 halfGridDim(gridDim.x/2, 1,1);

#define CALL_ALL_LINK_KERNEL(sig_sign, mu_sign)				\
    do_all_link_kernel<sig_sign,mu_sign,0><<<halfGridDim, blockDim>>>(tempxEven,  tempxOdd, \
								      PmuEven,  PmuOdd, \
								      P3Even,  P3Odd, \
								      P3muEven,  P3muOdd, \
								      shortPEven,  shortPOdd, \
								      sig,  mu, coeff, mcoeff, accumu_coeff, \
								      linkEven, linkOdd, \
								      momEven, momOdd); \
    do_all_link_kernel<sig_sign,mu_sign,1><<<halfGridDim, blockDim>>>(tempxEven,  tempxOdd, \
								      PmuEven,  PmuOdd, \
								      P3Even,  P3Odd, \
								      P3muEven,  P3muOdd, \
								      shortPEven,  shortPOdd, \
								      sig,  mu, coeff, mcoeff, accumu_coeff, \
								      linkEven, linkOdd, \
								      momEven, momOdd);
  
  
    if (GOES_FORWARDS(sig) && GOES_FORWARDS(mu)){		
      CALL_ALL_LINK_KERNEL(1,1);
    }else if (GOES_FORWARDS(sig) && GOES_BACKWARDS(mu)){
      CALL_ALL_LINK_KERNEL(1,0);
    }else if (GOES_BACKWARDS(sig) && GOES_FORWARDS(mu)){
      CALL_ALL_LINK_KERNEL(0,1);		
    }else{
      CALL_ALL_LINK_KERNEL(0,0);		
    }

#undef CALL_ALL_LINK_KERNEL
    
  }

  /* This function computes the one and naik terms' contribution to momentum
   * 
   * Tempx:    IN
   * Pmu:      IN
   * Pnumu:    IN
   *
   */
  template <int oddBit, typename Float2>
  __global__ void
  do_one_and_naik_terms_kernel(Float2* TempxEven, Float2* TempxOdd,
			       Float2* PmuEven,   Float2* PmuOdd, 
			       Float2* PnumuEven, Float2* PnumuOdd,
			       int mu, Float2 OneLink, Float2 Naik, Float2 mNaik,
			       float4* linkEven, float4* linkOdd,
			       Float2* momEven, Float2* momOdd)
  {
    Float2 HWA0, HWA1, HWA2, HWA3, HWA4, HWA5;
    Float2 HWB0, HWB1, HWB2, HWB3, HWB4, HWB5;
    Float2 HWC0, HWC1, HWC2, HWC3, HWC4, HWC5;
    Float2 HWD0, HWD1, HWD2, HWD3, HWD4, HWD5;
    float4 LINKA0, LINKA1, LINKA2, LINKA3, LINKA4;
    float4 LINKB0, LINKB1, LINKB2, LINKB3, LINKB4;
    Float2 AH0, AH1, AH2, AH3, AH4;    
    
    int sid = blockIdx.x * blockDim.x + threadIdx.x;
    int z1 = sid / X1h;
    int x1h = sid - z1*X1h;
    int z2 = z1 / X2;
    int x2 = z1 - z2*X2;
    int x4 = z2 / X3;
    int x3 = z2 - x4*X3;
    int x1odd = (x2 + x3 + x4 + oddBit) & 1;
    int x1 = 2*x1h + x1odd;
    //int X = 2*sid + x1odd;
    
    int dx[4];
    int new_x1, new_x2, new_x3, new_x4, new_idx;
    int sign=1;
    
    if (GOES_BACKWARDS(mu)){
      //The one link
      LOAD_HW(PmuEven, PmuOdd, sid, HWA, oddBit);
      LOAD_HW(TempxEven, TempxOdd, sid, HWB, oddBit);
      ADD_FORCE_TO_MOM(hwa, hwb, sid, OPP_DIR(mu), OneLink, oddBit);
	
      //Naik term
      dx[3]=dx[2]=dx[1]=dx[0]=0;
      dx[OPP_DIR(mu)] = -1;
      new_x1 = (x1 + dx[0] + X1)%X1;
      new_x2 = (x2 + dx[1] + X2)%X2;
      new_x3 = (x3 + dx[2] + X3)%X3;
      new_x4 = (x4 + dx[3] + X4)%X4;	
      new_idx = (new_x4*X3X2X1+new_x3*X2X1+new_x2*X1+new_x1) >> 1;
      LOAD_HW(TempxEven, TempxOdd, new_idx, HWA, 1-oddBit);
      FF_LOAD_MATRIX(OPP_DIR(mu), new_idx, LINKA, 1-oddBit);
      FF_COMPUTE_RECONSTRUCT_SIGN(sign, OPP_DIR(mu), new_x1,new_x2,new_x3,new_x4);
      RECONSTRUCT_LINK_12(sign, linka);		
      ADJ_MAT_MUL_HW(linka, hwa, hwc); //Popmu
	
      LOAD_HW(PnumuEven, PnumuOdd, sid, HWD, oddBit);
      ADD_FORCE_TO_MOM(hwd, hwc, sid, OPP_DIR(mu), mNaik, oddBit);
	
      dx[3]=dx[2]=dx[1]=dx[0]=0;
      dx[OPP_DIR(mu)] = 1;
      new_x1 = (x1 + dx[0] + X1)%X1;
      new_x2 = (x2 + dx[1] + X2)%X2;
      new_x3 = (x3 + dx[2] + X3)%X3;
      new_x4 = (x4 + dx[3] + X4)%X4;	
      new_idx = (new_x4*X3X2X1+new_x3*X2X1+new_x2*X1+new_x1) >> 1;
      LOAD_HW(PnumuEven, PnumuOdd, new_idx, HWA, 1-oddBit);
      FF_LOAD_MATRIX(OPP_DIR(mu), sid, LINKA, oddBit);
      FF_COMPUTE_RECONSTRUCT_SIGN(sign, OPP_DIR(mu), x1, x2, x3, x4);
      RECONSTRUCT_LINK_12(sign, linka);	
      MAT_MUL_HW(linka, hwa, hwc);
      ADD_FORCE_TO_MOM(hwc, hwb, sid, OPP_DIR(mu), Naik, oddBit);	
    }else{
      dx[3]=dx[2]=dx[1]=dx[0]=0;
      dx[mu] = 1;
      new_x1 = (x1 + dx[0] + X1)%X1;
      new_x2 = (x2 + dx[1] + X2)%X2;
      new_x3 = (x3 + dx[2] + X3)%X3;
      new_x4 = (x4 + dx[3] + X4)%X4;	
      new_idx = (new_x4*X3X2X1+new_x3*X2X1+new_x2*X1+new_x1) >> 1;
      LOAD_HW(TempxEven, TempxOdd, new_idx, HWA, 1-oddBit);
      FF_LOAD_MATRIX(mu, sid, LINKA, oddBit);
      FF_COMPUTE_RECONSTRUCT_SIGN(sign, mu, x1, x2, x3, x4);
      RECONSTRUCT_LINK_12(sign, linka);
      MAT_MUL_HW(linka, hwa, hwb);
	
      LOAD_HW(PnumuEven, PnumuOdd, sid, HWC, oddBit);
      ADD_FORCE_TO_MOM(hwb, hwc, sid, mu, Naik, oddBit);
	

    }
  }

  template<typename Float2>
  static void
  one_and_naik_terms_kernel(Float2* TempxEven, Float2* TempxOdd,
			    Float2* PmuEven,   Float2* PmuOdd,
			    Float2* PnumuEven, Float2* PnumuOdd,
			    int mu, Float2 OneLink, Float2 Naik, Float2 mNaik,
			    float4* linkEven, float4* linkOdd,
			    Float2* momEven, Float2* momOdd,
			    dim3 gridDim, dim3 blockDim)
  {  
    dim3 halfGridDim(gridDim.x/2, 1,1);

    do_one_and_naik_terms_kernel<0><<<halfGridDim, blockDim>>>(TempxEven, TempxOdd,
							       PmuEven, PmuOdd,
							       PnumuEven, PnumuOdd,
							       mu, OneLink, Naik, mNaik, 
							       linkEven, linkOdd,
							       momEven, momOdd);
    do_one_and_naik_terms_kernel<1><<<halfGridDim, blockDim>>>(TempxEven, TempxOdd,
							       PmuEven, PmuOdd,
							       PnumuEven, PnumuOdd,
							       mu, OneLink, Naik, mNaik, 
							       linkEven, linkOdd,
							       momEven, momOdd); 
    return;
  }



#define Pmu          tempvec[0] 
#define Pnumu        tempvec[1]
#define Prhonumu     tempvec[2]
#define P7           tempvec[3]
#define P7rho        tempvec[4]              
#define P7rhonu      tempvec[5]
#define P5           tempvec[6]
#define P3           tempvec[7]
#define P5nu         tempvec[3]
#define P3mu         tempvec[3]
#define Popmu        tempvec[4]
#define Pmumumu      tempvec[4]

  template<typename Real>
  static void
  do_fermion_force_cuda(Real eps, Real weight1, Real weight2,  Real* act_path_coeff, FullHw cudaHw, 
			cudaGaugeField &siteLink, cudaGaugeField &cudaMom, FullHw tempvec[8], QudaGaugeParam* param)
  {
    
    int mu, nu, rho, sig;
    float2 coeff;
    
    float2 OneLink, Lepage, Naik, FiveSt, ThreeSt, SevenSt;
    float2 mNaik, mLepage, mFiveSt, mThreeSt, mSevenSt;
    
    Real ferm_epsilon;
    ferm_epsilon = 2.0*weight1*eps;
    OneLink.x = act_path_coeff[0]*ferm_epsilon ;
    Naik.x    = act_path_coeff[1]*ferm_epsilon ; mNaik.x    = -Naik.x;
    ThreeSt.x = act_path_coeff[2]*ferm_epsilon ; mThreeSt.x = -ThreeSt.x;
    FiveSt.x  = act_path_coeff[3]*ferm_epsilon ; mFiveSt.x  = -FiveSt.x;
    SevenSt.x = act_path_coeff[4]*ferm_epsilon ; mSevenSt.x = -SevenSt.x;
    Lepage.x  = act_path_coeff[5]*ferm_epsilon ; mLepage.x  = -Lepage.x;
    
    ferm_epsilon = 2.0*weight2*eps;
    OneLink.y = act_path_coeff[0]*ferm_epsilon ;
    Naik.y    = act_path_coeff[1]*ferm_epsilon ; mNaik.y    = -Naik.y;
    ThreeSt.y = act_path_coeff[2]*ferm_epsilon ; mThreeSt.y = -ThreeSt.y;
    FiveSt.y  = act_path_coeff[3]*ferm_epsilon ; mFiveSt.y  = -FiveSt.y;
    SevenSt.y = act_path_coeff[4]*ferm_epsilon ; mSevenSt.y = -SevenSt.y;
    Lepage.y  = act_path_coeff[5]*ferm_epsilon ; mLepage.y  = -Lepage.y;
    
    int DirectLinks[8] ;    
    
    for(mu=0;mu<8;mu++){
      DirectLinks[mu] = 0 ;
    }
        
    int volume = param->X[0]*param->X[1]*param->X[2]*param->X[3];
    dim3 blockDim(BLOCK_DIM,1,1);
    dim3 gridDim(volume/blockDim.x, 1, 1);


    cudaBindTexture(0, siteLink0TexSingle_recon, siteLink.Even_p(), siteLink.Bytes()/2);
    cudaBindTexture(0, siteLink1TexSingle_recon, siteLink.Odd_p(), siteLink.Bytes()/2);

    
    for(sig=0; sig < 8; sig++){
      for(mu = 0; mu < 8; mu++){
	if ( (mu == sig) || (mu == OPP_DIR(sig))){
	  continue;
	}
	//3-link
	//Kernel A: middle link
	    
	middle_link_kernel( (float2*)cudaHw.even.data, (float2*)cudaHw.odd.data,
			    (float2*)Pmu.even.data, (float2*)Pmu.odd.data,
			    (float2*)P3.even.data, (float2*)P3.odd.data,
			    sig, mu, mThreeSt,
			    (float4*)siteLink.Even_p(), (float4*)siteLink.Odd_p(), siteLink, 
			    (float2*)cudaMom.Even_p(), (float2*)cudaMom.Odd_p(), 
			    gridDim, blockDim);
	checkCudaError();
	for(nu=0; nu < 8; nu++){
	  if (nu == sig || nu == OPP_DIR(sig)
	      || nu == mu || nu == OPP_DIR(mu)){
	    continue;
	  }
	  //5-link: middle link
	  //Kernel B
	  middle_link_kernel( (float2*)Pmu.even.data, (float2*)Pmu.odd.data,
			      (float2*)Pnumu.even.data, (float2*)Pnumu.odd.data,
			      (float2*)P5.even.data, (float2*)P5.odd.data,
			      sig, nu, FiveSt,
			      (float4*)siteLink.Even_p(), (float4*)siteLink.Odd_p(), siteLink, 
			      (float2*)cudaMom.Even_p(), (float2*)cudaMom.Odd_p(),
			      gridDim, blockDim);
	  checkCudaError();
		
	  for(rho =0; rho < 8; rho++){
	    if (rho == sig || rho == OPP_DIR(sig)
		|| rho == mu || rho == OPP_DIR(mu)
		|| rho == nu || rho == OPP_DIR(nu)){
	      continue;
	    }
	    //7-link: middle link and side link
	    //kernel C
		    
	    if(FiveSt.x != 0)coeff.x = SevenSt.x/FiveSt.x ; else coeff.x = 0;
	    if(FiveSt.y != 0)coeff.y = SevenSt.y/FiveSt.y ; else coeff.y = 0;		    
	    all_link_kernel((float2*)Pnumu.even.data, (float2*)Pnumu.odd.data,
			    (float2*)Prhonumu.even.data, (float2*)Prhonumu.odd.data,
			    (float2*)P7.even.data, (float2*)P7.odd.data,
			    (float2*)P7rho.even.data, (float2*)P7rho.odd.data,
			    (float2*)P5.even.data, (float2*)P5.odd.data,
			    sig, rho, SevenSt,mSevenSt,coeff,
			    (float4*)siteLink.Even_p(), (float4*)siteLink.Odd_p(), siteLink,
			    (float2*)cudaMom.Even_p(), (float2*)cudaMom.Odd_p(),
			    gridDim, blockDim);	
	    checkCudaError();
		    
	  }//rho  		
		
	  //5-link: side link
	  //kernel B2
	  if(ThreeSt.x != 0)coeff.x = FiveSt.x/ThreeSt.x ; else coeff.x = 0;
	  if(ThreeSt.y != 0)coeff.y = FiveSt.y/ThreeSt.y ; else coeff.y = 0;
	  side_link_kernel((float2*)P5.even.data, (float2*)P5.odd.data,
			   (float2*)P5nu.even.data, (float2*)P5nu.odd.data,
			   (float2*)Pmu.even.data, (float2*)Pmu.odd.data,
			   (float2*)Pnumu.even.data, (float2*)Pnumu.odd.data,
			   (float2*)P3.even.data, (float2*)P3.odd.data,
			   sig, nu, mFiveSt, coeff,
			   (float4*)siteLink.Even_p(), (float4*)siteLink.Odd_p(), siteLink,
			   (float2*)cudaMom.Even_p(), (float2*)cudaMom.Odd_p(),
			   gridDim, blockDim);
	  checkCudaError();
		
	}//nu
	    
	//lepage
	//Kernel A2
	middle_link_kernel( (float2*)Pmu.even.data, (float2*)Pmu.odd.data,
			    (float2*)Pnumu.even.data, (float2*)Pnumu.odd.data,
			    (float2*)P5.even.data, (float2*)P5.odd.data,
			    sig, mu, Lepage,
			    (float4*)siteLink.Even_p(), (float4*)siteLink.Odd_p(), siteLink, 
			    (float2*)cudaMom.Even_p(), (float2*)cudaMom.Odd_p(),
			    gridDim, blockDim);	    
	checkCudaError();		
	    
	if(ThreeSt.x != 0)coeff.x = Lepage.x/ThreeSt.x ; else coeff.x = 0;
	if(ThreeSt.y != 0)coeff.y = Lepage.y/ThreeSt.y ; else coeff.y = 0;
	    
	side_link_kernel((float2*)P5.even.data, (float2*)P5.odd.data,
			 (float2*)P5nu.even.data, (float2*)P5nu.odd.data,
			 (float2*)Pmu.even.data, (float2*)Pmu.odd.data,
			 (float2*)Pnumu.even.data, (float2*)Pnumu.odd.data,
			 (float2*)P3.even.data, (float2*)P3.odd.data,
			 sig, mu, mLepage,coeff,
			 (float4*)siteLink.Even_p(), (float4*)siteLink.Odd_p(), siteLink,
			 (float2*)cudaMom.Even_p(), (float2*)cudaMom.Odd_p(),
			 gridDim, blockDim);
	checkCudaError();		
	    
	//3-link side link
	coeff.x=coeff.y=0;
	side_link_kernel((float2*)P3.even.data, (float2*)P3.odd.data,
			 (float2*)P3mu.even.data, (float2*)P3mu.odd.data,
			 (float2*)cudaHw.even.data, (float2*)cudaHw.odd.data,
			 (float2*)Pmu.even.data, (float2*)Pmu.odd.data,
			 (float2*)NULL, (float2*)NULL,
			 sig, mu, ThreeSt,coeff,
			 (float4*)siteLink.Even_p(), (float4*)siteLink.Odd_p(), siteLink,
			 (float2*)cudaMom.Even_p(), (float2*)cudaMom.Odd_p(),
			 gridDim, blockDim);
	checkCudaError();			    

	//1-link and naik term	    
	if (!DirectLinks[mu]){
	  DirectLinks[mu]=1;
	  //kernel Z	    
	  one_and_naik_terms_kernel((float2*)cudaHw.even.data, (float2*)cudaHw.odd.data,
				    (float2*)Pmu.even.data, (float2*)Pmu.odd.data,
				    (float2*)Pnumu.even.data, (float2*)Pnumu.odd.data,
				    mu, OneLink, Naik, mNaik, 
				    (float4*)siteLink.Even_p(), (float4*)siteLink.Odd_p(),
				    (float2*)cudaMom.Even_p(), (float2*)cudaMom.Odd_p(),
				    gridDim, blockDim);
	
	  checkCudaError();		
	}
	    
      }//mu

    }//sig
        
    cudaUnbindTexture(siteLink0TexSingle_recon);
    cudaUnbindTexture(siteLink1TexSingle_recon);
 
  }

#undef Pmu
#undef Pnumu
#undef Prhonumu
#undef P7
#undef P7rho
#undef P7rhonu
#undef P5
#undef P3
#undef P5nu
#undef P3mu
#undef Popmu
#undef Pmumumu

  void
  fermion_force_cuda(double eps, double weight1, double weight2, void* act_path_coeff,
		     FullHw cudaHw, cudaGaugeField &siteLink, cudaGaugeField &cudaMom, QudaGaugeParam* param)
  {
    int i;
    FullHw tempvec[8];
  
    if (siteLink.Reconstruct() != QUDA_RECONSTRUCT_12) 
      errorQuda("Reconstruct type %d not supported for gauge field", siteLink.Reconstruct());

    if (cudaMom.Reconstruct() != QUDA_RECONSTRUCT_10)
      errorQuda("Reconstruct type %d not supported for momentum field", cudaMom.Reconstruct());

    for(i=0;i < 8;i++){
      tempvec[i]  = createHwQuda(param->X, param->cuda_prec);
    }
    
    if (param->cuda_prec == QUDA_DOUBLE_PRECISION){
      /*
	do_fermion_force_cuda( (double)eps, (double)weight1, (double)weight2, (double*)act_path_coeff,
	cudaHw, siteLink, cudaMom, tempvec, param);
      */
      errorQuda("Double precision not supported?");
    }else{	
      do_fermion_force_cuda( (float)eps, (float)weight1, (float)weight2, (float*)act_path_coeff,
			     cudaHw, siteLink, cudaMom, tempvec, param);	
    }
    
    for(i=0;i < 8;i++){
      freeHwQuda(tempvec[i]);
    }
   


  }

#undef BLOCK_DIM

#undef FF_COMPUTE_NEW_FULL_IDX_PLUS_UPDATE
#undef FF_COMPUTE_NEW_FULL_IDX_MINUS_UPDATE

} // namespace quda

#endif // defined(GPU_FERMION_FORCE)
