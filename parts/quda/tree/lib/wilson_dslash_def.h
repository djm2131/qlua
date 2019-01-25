// wilson_dslash_def.h - Dslash kernel definitions

// There are currently 72 different variants of the Wilson Dslash
// kernel, each one characterized by a set of 5 options, where each
// option can take one of several values (2*3*2*2*3 = 72).  This file
// is structured so that the C preprocessor loops through all 72
// variants (in a manner resembling a counter), sets the appropriate
// macros, and defines the corresponding functions.
//
// As an example of the function naming conventions, consider
//
// cloverDslash12DaggerXpayKernel(float4* out, ...).
//
// This is a clover Dslash^dagger kernel where the result is
// multiplied by "a" and summed with an input vector (Xpay), and the
// gauge matrix is reconstructed from 12 real numbers.  More
// generally, each function name is given by the concatenation of the
// following 4 fields, with "Kernel" at the end:
//
// DD_NAME_F = dslash, cloverDslash
// DD_RECON_F = 8, 12, 18
// DD_DAG_F = Dagger, [blank]
// DD_XPAY_F = Xpay, [blank]
//
// In addition, the kernels are templated on the precision of the
// fields (double, single, or half).

// initialize on first iteration

#ifndef DD_LOOP
#define DD_LOOP
#define DD_DAG 0
#define DD_XPAY 0
#define DD_RECON 0
#define DD_PREC 0
#endif

// set options for current iteration

#if (DD_CLOVER==0) // no clover
#define DD_NAME_F dslash
#elif (DD_CLOVER==1)   // clover
#define DSLASH_CLOVER
#define DD_NAME_F cloverDslash
#else
#define DSLASH_CLOVER
#define DSLASH_CLOVER_XPAY
#define DD_NAME_F asymCloverDslash
#endif

#if (DD_DAG==0) // no dagger
#define DD_DAG_F
#else           // dagger
#define DD_DAG_F Dagger
#endif

// DSLASH_CLOVER_XPAY implies DD_XPAY=1
#if (DD_XPAY==0) && defined(DSLASH_CLOVER_XPAY)
#undef DD_XPAY
#define DD_XPAY 1
#endif

#if (DD_XPAY==0) // no xpay 
#define DD_XPAY_F 
#else            // xpay
#define DD_XPAY_F Xpay
#define DSLASH_XPAY
#endif

#if (DD_PREC == 0)
#define DD_PARAM_XPAY const double2 *x, const float *xNorm, const double a,
#elif (DD_PREC == 1) 
#define DD_PARAM_XPAY const float4 *x, const float *xNorm, const float a,
#else
#define DD_PARAM_XPAY const short4 *x, const float *xNorm, const float a,
#endif

#if (DD_RECON==0) // reconstruct from 8 reals
#define DD_RECON_F 8

#if (DD_PREC==0)
#define DD_PARAM_GAUGE const double2 *gauge0, const double2 *gauge1,
#define RECONSTRUCT_GAUGE_MATRIX RECONSTRUCT_MATRIX_8_DOUBLE
#ifdef DIRECT_ACCESS_LINK
#define READ_GAUGE_MATRIX READ_GAUGE_MATRIX_8_DOUBLE2
#else 
#define READ_GAUGE_MATRIX READ_GAUGE_MATRIX_8_DOUBLE2_TEX
#endif // DIRECT_ACCESS_LINK

#elif (DD_PREC==1)
#define DD_PARAM_GAUGE const float4 *gauge0, const float4 *gauge1,
#define RECONSTRUCT_GAUGE_MATRIX RECONSTRUCT_MATRIX_8_SINGLE
#ifdef DIRECT_ACCESS_LINK
#define READ_GAUGE_MATRIX READ_GAUGE_MATRIX_8_FLOAT4
#else
#define READ_GAUGE_MATRIX READ_GAUGE_MATRIX_8_FLOAT4_TEX
#endif // DIRECT_ACCESS_LINK

#else
#define DD_PARAM_GAUGE const short4 *gauge0, const short4* gauge1,
#define RECONSTRUCT_GAUGE_MATRIX RECONSTRUCT_MATRIX_8_SINGLE
#ifdef DIRECT_ACCESS_LINK
#define READ_GAUGE_MATRIX READ_GAUGE_MATRIX_8_SHORT4
#else
#define READ_GAUGE_MATRIX READ_GAUGE_MATRIX_8_SHORT4_TEX
#endif // DIRECT_ACCESS_LINK
#endif // DD_PREC
#elif (DD_RECON==1) // reconstruct from 12 reals
#define DD_RECON_F 12

#if (DD_PREC==0)
#define RECONSTRUCT_GAUGE_MATRIX RECONSTRUCT_MATRIX_12_DOUBLE
#ifdef DIRECT_ACCESS_LINK
#define READ_GAUGE_MATRIX READ_GAUGE_MATRIX_12_DOUBLE2
#else
#define READ_GAUGE_MATRIX READ_GAUGE_MATRIX_12_DOUBLE2_TEX
#endif // DIRECT_ACCESS_LINK
#define DD_PARAM_GAUGE const double2 *gauge0, const double2 *gauge1,

#elif (DD_PREC==1)
#define DD_PARAM_GAUGE const float4 *gauge0, const float4 *gauge1,
#define RECONSTRUCT_GAUGE_MATRIX RECONSTRUCT_MATRIX_12_SINGLE
#ifdef DIRECT_ACCESS_LINK
#define READ_GAUGE_MATRIX READ_GAUGE_MATRIX_12_FLOAT4
#else
#define READ_GAUGE_MATRIX READ_GAUGE_MATRIX_12_FLOAT4_TEX
#endif // DIRECT_ACCESS_LINK

#else
#define DD_PARAM_GAUGE const short4 *gauge0, const short4 *gauge1,
#define RECONSTRUCT_GAUGE_MATRIX RECONSTRUCT_MATRIX_12_SINGLE
#ifdef DIRECT_ACCESS_LINK
#define READ_GAUGE_MATRIX READ_GAUGE_MATRIX_12_SHORT4
#else
#define READ_GAUGE_MATRIX READ_GAUGE_MATRIX_12_SHORT4_TEX
#endif // DIRECT_ACCESS_LINK
#endif // DD_PREC
#else               // no reconstruct, load all components
#define DD_RECON_F 18
#define GAUGE_FLOAT2
#if (DD_PREC==0)
#define RECONSTRUCT_GAUGE_MATRIX RECONSTRUCT_MATRIX_18_DOUBLE
#ifdef DIRECT_ACCESS_LINK
#define READ_GAUGE_MATRIX READ_GAUGE_MATRIX_18_DOUBLE2
#else
#define READ_GAUGE_MATRIX READ_GAUGE_MATRIX_18_DOUBLE2_TEX
#endif // DIRECT_ACCESS_LINK
#define DD_PARAM_GAUGE const double2 *gauge0, const double2 *gauge1,

#elif (DD_PREC==1)
#define DD_PARAM_GAUGE const float4 *gauge0, const float4 *gauge1, // FIXME for direct reading, really float2
#define RECONSTRUCT_GAUGE_MATRIX RECONSTRUCT_MATRIX_18_SINGLE
#ifdef DIRECT_ACCESS_LINK
#define READ_GAUGE_MATRIX READ_GAUGE_MATRIX_18_FLOAT2
#else
#define READ_GAUGE_MATRIX READ_GAUGE_MATRIX_18_FLOAT2_TEX
#endif // DIRECT_ACCESS_LINK

#else
#define DD_PARAM_GAUGE const short4 *gauge0, const short4 *gauge1, // FIXME for direct reading, really short2
#define RECONSTRUCT_GAUGE_MATRIX RECONSTRUCT_MATRIX_18_SINGLE
#ifdef DIRECT_ACCESS_LINK
#define READ_GAUGE_MATRIX READ_GAUGE_MATRIX_18_SHORT2
#else
#define READ_GAUGE_MATRIX READ_GAUGE_MATRIX_18_SHORT2_TEX
#endif //DIRECT_ACCESS_LINK
#endif
#endif

#if (DD_PREC==0) // double-precision fields

#define TPROJSCALE tProjScale

// double-precision gauge field
#if (defined DIRECT_ACCESS_LINK) || (defined FERMI_NO_DBLE_TEX)
#define GAUGE0TEX gauge0
#define GAUGE1TEX gauge1
#else
#ifdef USE_TEXTURE_OBJECTS
#define GAUGE0TEX param.gauge0Tex
#define GAUGE1TEX param.gauge1Tex
#else
#define GAUGE0TEX gauge0TexDouble2
#define GAUGE1TEX gauge1TexDouble2
#endif
#endif

#define GAUGE_FLOAT2

// double-precision spinor fields
#define DD_PARAM_OUT double2* out, float *null1,
#define DD_PARAM_IN const double2* in, const float *null4,

#if (defined DIRECT_ACCESS_WILSON_SPINOR) || (defined FERMI_NO_DBLE_TEX)
#define READ_SPINOR READ_SPINOR_DOUBLE
#define READ_SPINOR_UP READ_SPINOR_DOUBLE_UP
#define READ_SPINOR_DOWN READ_SPINOR_DOUBLE_DOWN
#define SPINORTEX in
#else
#define READ_SPINOR READ_SPINOR_DOUBLE_TEX
#define READ_SPINOR_UP READ_SPINOR_DOUBLE_UP_TEX
#define READ_SPINOR_DOWN READ_SPINOR_DOUBLE_DOWN_TEX
#ifdef USE_TEXTURE_OBJECTS
#define SPINORTEX param.inTex
#else
#define SPINORTEX spinorTexDouble
#endif // USE_TEXTURE_OBJECTS
#endif
#if (defined DIRECT_ACCESS_WILSON_INTER) || (defined FERMI_NO_DBLE_TEX)
#define READ_INTERMEDIATE_SPINOR READ_SPINOR_DOUBLE
#define INTERTEX out
#else
#define READ_INTERMEDIATE_SPINOR READ_SPINOR_DOUBLE_TEX
#ifdef USE_TEXTURE_OBJECTS
#define INTERTEX param.outTex
#else
#define INTERTEX interTexDouble
#endif
#endif
#define WRITE_SPINOR WRITE_SPINOR_DOUBLE2_STR
#define SPINOR_DOUBLE
#if (DD_XPAY==1)
#if (defined DIRECT_ACCESS_WILSON_ACCUM) || (defined FERMI_NO_DBLE_TEX)
#define ACCUMTEX x
#define READ_ACCUM READ_ACCUM_DOUBLE
#else
#ifdef USE_TEXTURE_OBJECTS
#define ACCUMTEX param.xTex
#else
#define ACCUMTEX accumTexDouble
#endif // USE_TEXTURE_OBJECTS
#define READ_ACCUM READ_ACCUM_DOUBLE_TEX
#endif

#endif

#define SPINOR_HOP 12

// double-precision clover field
#if (DD_CLOVER==0)
#define DD_PARAM_CLOVER
#else
#define DD_PARAM_CLOVER const double2 *clover, const float *null3,
#endif
#if (defined DIRECT_ACCESS_CLOVER) || (defined FERMI_NO_DBLE_TEX)
#define CLOVERTEX clover
#define READ_CLOVER READ_CLOVER_DOUBLE_STR
#else
#ifdef USE_TEXTURE_OBJECTS
#define CLOVERTEX (param.cloverTex)
#else
#define CLOVERTEX cloverTexDouble
#endif
#define READ_CLOVER READ_CLOVER_DOUBLE_TEX
#endif
#define CLOVER_DOUBLE

#elif (DD_PREC==1) // single-precision fields

#define TPROJSCALE tProjScale_f

// single-precision gauge field
#ifdef DIRECT_ACCESS_LINK
#define GAUGE0TEX gauge0
#define GAUGE1TEX gauge1
#else
#ifdef USE_TEXTURE_OBJECTS
#define GAUGE0TEX param.gauge0Tex
#define GAUGE1TEX param.gauge1Tex
#else
#if (DD_RECON_F == 18)
#define GAUGE0TEX gauge0TexSingle2
#define GAUGE1TEX gauge1TexSingle2
#else
#define GAUGE0TEX gauge0TexSingle4
#define GAUGE1TEX gauge1TexSingle4
#endif
#endif // USE_TEXTURE_OBJECTS
#endif


// single-precision spinor fields
#define DD_PARAM_OUT float4* out, float *null1,
#define DD_PARAM_IN const float4* in, const float *null4,
#ifdef DIRECT_ACCESS_WILSON_SPINOR
#define READ_SPINOR READ_SPINOR_SINGLE
#define READ_SPINOR_UP READ_SPINOR_SINGLE_UP
#define READ_SPINOR_DOWN READ_SPINOR_SINGLE_DOWN
#define SPINORTEX in
#else
#define READ_SPINOR READ_SPINOR_SINGLE_TEX
#define READ_SPINOR_UP READ_SPINOR_SINGLE_UP_TEX
#define READ_SPINOR_DOWN READ_SPINOR_SINGLE_DOWN_TEX
#ifdef USE_TEXTURE_OBJECTS
#define SPINORTEX param.inTex
#else
#define SPINORTEX spinorTexSingle
#endif // USE_TEXTURE_OBJECTS
#endif
#ifdef DIRECT_ACCESS_WILSON_INTER
#define READ_INTERMEDIATE_SPINOR READ_SPINOR_SINGLE
#define INTERTEX out
#else
#define READ_INTERMEDIATE_SPINOR READ_SPINOR_SINGLE_TEX
#ifdef USE_TEXTURE_OBJECTS
#define INTERTEX param.outTex
#else
#define INTERTEX interTexSingle
#endif // USE_TEXTURE_OBJECTS
#endif
#define WRITE_SPINOR WRITE_SPINOR_FLOAT4_STR
#if (DD_XPAY==1)
#ifdef DIRECT_ACCESS_WILSON_ACCUM
#define ACCUMTEX x
#define READ_ACCUM READ_ACCUM_SINGLE
#else
#ifdef USE_TEXTURE_OBJECTS
#define ACCUMTEX param.xTex
#else
#define ACCUMTEX accumTexSingle
#endif // USE_TEXTURE_OBJECTS
#define READ_ACCUM READ_ACCUM_SINGLE_TEX
#endif
#endif

#define SPINOR_HOP 6

// single-precision clover field
#if (DD_CLOVER==0)
#define DD_PARAM_CLOVER
#else
#define DD_PARAM_CLOVER const float4 *clover, const float *null3,
#endif
#ifdef DIRECT_ACCESS_CLOVER
#define CLOVERTEX clover
#define READ_CLOVER READ_CLOVER_SINGLE
#else
#ifdef USE_TEXTURE_OBJECTS
#define CLOVERTEX (param.cloverTex)
#else
#define CLOVERTEX cloverTexSingle
#endif
#define READ_CLOVER READ_CLOVER_SINGLE_TEX
#endif

#else             // half-precision fields

#define TPROJSCALE tProjScale_f

// half-precision gauge field
#ifdef DIRECT_ACCESS_LINK
#define GAUGE0TEX gauge0
#define GAUGE1TEX gauge1
#else
#ifdef USE_TEXTURE_OBJECTS
#define GAUGE0TEX param.gauge0Tex
#define GAUGE1TEX param.gauge1Tex
#else
#if (DD_RECON_F == 18)
#define GAUGE0TEX gauge0TexHalf2
#define GAUGE1TEX gauge1TexHalf2
#else
#define GAUGE0TEX gauge0TexHalf4
#define GAUGE1TEX gauge1TexHalf4
#endif
#endif // USE_TEXTURE_OBJECTS
#endif


// half-precision spinor fields
#ifdef DIRECT_ACCESS_WILSON_SPINOR
#define READ_SPINOR READ_SPINOR_HALF
#define READ_SPINOR_UP READ_SPINOR_HALF_UP
#define READ_SPINOR_DOWN READ_SPINOR_HALF_DOWN
#define SPINORTEX in
#else
#define READ_SPINOR READ_SPINOR_HALF_TEX
#define READ_SPINOR_UP READ_SPINOR_HALF_UP_TEX
#define READ_SPINOR_DOWN READ_SPINOR_HALF_DOWN_TEX
#ifdef USE_TEXTURE_OBJECTS
#define SPINORTEX param.inTex
#else
#define SPINORTEX spinorTexHalf
#endif // USE_TEXTURE_OBJECTS
#endif
#ifdef DIRECT_ACCESS_WILSON_INTER
#define READ_INTERMEDIATE_SPINOR READ_SPINOR_HALF
#define INTERTEX out
#else
#define READ_INTERMEDIATE_SPINOR READ_SPINOR_HALF_TEX
#ifdef USE_TEXTURE_OBJECTS
#define INTERTEX param.outTex
#else
#define INTERTEX interTexHalf
#endif // USE_TEXTURE_OBJECTS
#endif
#define DD_PARAM_OUT short4* out, float *outNorm,
#define DD_PARAM_IN const short4* in, const float *inNorm,
#define WRITE_SPINOR WRITE_SPINOR_SHORT4_STR
#if (DD_XPAY==1)
#ifdef DIRECT_ACCESS_WILSON_ACCUM
#define ACCUMTEX x
#define READ_ACCUM READ_ACCUM_HALF
#else
#ifdef USE_TEXTURE_OBJECTS
#define ACCUMTEX param.xTex
#else
#define ACCUMTEX accumTexHalf
#endif // USE_TEXTURE_OBJECTS
#define READ_ACCUM READ_ACCUM_HALF_TEX
#endif
#endif

#define SPINOR_HOP 6

// half-precision clover field
#if (DD_CLOVER==0)
#define DD_PARAM_CLOVER 
#else
#define DD_PARAM_CLOVER const short4 *clover, const float *cloverNorm,
#endif
#ifdef DIRECT_ACCESS_CLOVER
#define CLOVERTEX clover
#define READ_CLOVER READ_CLOVER_HALF
#else
#ifdef USE_TEXTURE_OBJECTS
#define CLOVERTEX (param.cloverTex)
#define CLOVERTEXNORM (param.cloverNormTex)
#else
#define CLOVERTEX cloverTexHalf
#define CLOVERTEXNORM cloverTexNorm
#endif
#define READ_CLOVER READ_CLOVER_HALF_TEX
#endif

#endif

#define DD_CONCAT(n,r,d,x) n ## r ## d ## x ## Kernel
#define DD_FUNC(n,r,d,x) DD_CONCAT(n,r,d,x)

// define the kernel

template <KernelType kernel_type>
__global__ void	DD_FUNC(DD_NAME_F, DD_RECON_F, DD_DAG_F, DD_XPAY_F)
  (DD_PARAM_OUT DD_PARAM_GAUGE DD_PARAM_CLOVER DD_PARAM_IN DD_PARAM_XPAY const DslashParam param) {

  // build Wilson or clover as appropriate
#if ((DD_CLOVER==0 && defined(GPU_WILSON_DIRAC)) || ((DD_CLOVER==1 || DD_CLOVER==2) && defined(GPU_CLOVER_DIRAC)))

#ifdef SHARED_WILSON_DSLASH // Fermi optimal code

#ifdef DSLASH_CLOVER_XPAY

#if DD_DAG
#include "asym_wilson_clover_dslash_dagger_fermi_core.h"
#else
#include "asym_wilson_clover_dslash_fermi_core.h"
#endif

#else

#if DD_DAG
#include "wilson_dslash_dagger_fermi_core.h"
#else
#include "wilson_dslash_fermi_core.h"
#endif

#endif

#else // no shared-memory blocking

#ifdef DSLASH_CLOVER_XPAY

#if DD_DAG
#include "asym_wilson_clover_dslash_dagger_gt200_core.h"
#else
#include "asym_wilson_clover_dslash_gt200_core.h"
#endif

#else

#if DD_DAG
#include "wilson_dslash_dagger_gt200_core.h"
#else
#include "wilson_dslash_gt200_core.h"
#endif

#endif

#endif // SHARED_WILSON_DSLASH


#endif // DD_CLOVER

}

#ifdef MULTI_GPU
template <>
__global__ void	DD_FUNC(DD_NAME_F, DD_RECON_F, DD_DAG_F, DD_XPAY_F)<EXTERIOR_KERNEL_ALL>
  (DD_PARAM_OUT DD_PARAM_GAUGE DD_PARAM_CLOVER DD_PARAM_IN DD_PARAM_XPAY const DslashParam param) {

  // build Wilson or clover as appropriate
#if ((DD_CLOVER==0 && defined(GPU_WILSON_DIRAC)) || ((DD_CLOVER==1 || DD_CLOVER==2) && defined(GPU_CLOVER_DIRAC)))

#ifdef SHARED_WILSON_DSLASH // Fermi optimal code

#ifdef DSLASH_CLOVER_XPAY

#if DD_DAG
#include "asym_wilson_clover_fused_exterior_dslash_dagger_fermi_core.h"
#else
#include "asym_wilson_clover_fused_exterior_dslash_fermi_core.h"
#endif

#else

#if DD_DAG
#include "wilson_fused_exterior_dslash_dagger_fermi_core.h"
#else
#include "wilson_fused_exterior_dslash_fermi_core.h"
#endif

#endif

#else // no shared-memory blocking

#ifdef DSLASH_CLOVER_XPAY

#if DD_DAG
#include "asym_wilson_clover_fused_exterior_dslash_dagger_gt200_core.h"
#else
#include "asym_wilson_clover_fused_exterior_dslash_gt200_core.h"
#endif

#else

#if DD_DAG
#include "wilson_fused_exterior_dslash_dagger_gt200_core.h"
#else
#include "wilson_fused_exterior_dslash_gt200_core.h"
#endif

#endif

#endif // SHARED_WILSON_DSLASH


#endif // DD_CLOVER

}
#endif // MULTI_GPU

// clean up

#undef DD_NAME_F
#undef DD_RECON_F
#undef DD_DAG_F
#undef DD_XPAY_F
#undef DD_PARAM_OUT
#undef DD_PARAM_GAUGE
#undef DD_PARAM_CLOVER
#undef DD_PARAM_IN
#undef DD_PARAM_XPAY
#undef DD_CONCAT
#undef DD_FUNC

#undef DSLASH_XPAY
#undef READ_GAUGE_MATRIX
#undef RECONSTRUCT_GAUGE_MATRIX
#undef GAUGE0TEX
#undef GAUGE1TEX
#undef READ_SPINOR
#undef READ_SPINOR_UP
#undef READ_SPINOR_DOWN
#undef SPINORTEX
#undef READ_INTERMEDIATE_SPINOR
#undef INTERTEX
#undef WRITE_SPINOR
#undef READ_ACCUM
#undef ACCUMTEX
#undef READ_CLOVER
#undef CLOVERTEX
#undef DSLASH_CLOVER
#undef DSLASH_CLOVER_XPAY
#undef GAUGE_FLOAT2
#undef SPINOR_DOUBLE
#undef CLOVER_DOUBLE
#undef SPINOR_HOP

#undef TPROJSCALE

// prepare next set of options, or clean up after final iteration

#if (DD_DAG==0)
#undef DD_DAG
#define DD_DAG 1
#else
#undef DD_DAG
#define DD_DAG 0

#if (DD_XPAY==0)
#undef DD_XPAY
#define DD_XPAY 1
#else
#undef DD_XPAY
#define DD_XPAY 0

#if (DD_RECON==0)
#undef DD_RECON
#define DD_RECON 1
#elif (DD_RECON==1)
#undef DD_RECON
#define DD_RECON 2
#else
#undef DD_RECON
#define DD_RECON 0

#if (DD_PREC==0)
#undef DD_PREC
#define DD_PREC 1
#elif (DD_PREC==1)
#undef DD_PREC
#define DD_PREC 2
#else

#undef DD_LOOP
#undef DD_DAG
#undef DD_XPAY
#undef DD_RECON
#undef DD_PREC

#endif // DD_PREC
#endif // DD_RECON
#endif // DD_XPAY
#endif // DD_DAG

#ifdef DD_LOOP
#include "wilson_dslash_def.h"
#endif
