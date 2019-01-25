#include <cstdlib>
#include <cstdio>
#include <string>
#include <iostream>

#include <color_spinor_field.h>
#include <clover_field.h>

//these are access control for staggered action
#ifdef GPU_STAGGERED_DIRAC
#if (__COMPUTE_CAPABILITY__ >= 300) // Kepler works best with texture loads only
//#define DIRECT_ACCESS_FAT_LINK
//#define DIRECT_ACCESS_LONG_LINK
//#define DIRECT_ACCESS_SPINOR
//#define DIRECT_ACCESS_ACCUM
//#define DIRECT_ACCESS_INTER
//#define DIRECT_ACCESS_PACK
#else // Fermi
//#define DIRECT_ACCESS_FAT_LINK
//#define DIRECT_ACCESS_LONG_LINK
#define DIRECT_ACCESS_SPINOR
//#define DIRECT_ACCESS_ACCUM
//#define DIRECT_ACCESS_INTER
//#define DIRECT_ACCESS_PACK
#endif
#endif // GPU_STAGGERED_DIRAC

#include <quda_internal.h>
#include <dslash_quda.h>
#include <sys/time.h>
#include <blas_quda.h>
#include <face_quda.h>

#include <inline_ptx.h>

namespace quda {

  namespace improvedstaggered {
#include <dslash_constants.h>
#include <dslash_textures.h>
#include <dslash_index.cuh>

#undef GPU_NDEG_TWISTED_MASS_DIRAC
#undef GPU_CLOVER_DIRAC
#undef GPU_DOMAIN_WALL_DIRAC
#define DD_IMPROVED 1
#include <staggered_dslash_def.h> // staggered Dslash kernels
#undef DD_IMPROVED

#include <dslash_quda.cuh>
  } // end namespace improvedstaggered

  // declare the dslash events
#include <dslash_events.cuh>

  using namespace improvedstaggered;

  template<typename T> struct RealType {};
  template<> struct RealType<double2> { typedef double type; };
  template<> struct RealType<float2> { typedef float type; };
  template<> struct RealType<float4> { typedef float type; };
  template<> struct RealType<short2> { typedef short type; };
  template<> struct RealType<short4> { typedef short type; };

#ifdef GPU_STAGGERED_DIRAC
  template <typename sFloat, typename fatGFloat, typename longGFloat, typename phaseFloat>
  class StaggeredDslashCuda : public DslashCuda {

  private:
    const fatGFloat *fat0, *fat1;
    const longGFloat *long0, *long1;
    const phaseFloat *phase0, *phase1;
    const double a;

  protected:
    unsigned int sharedBytesPerThread() const
    {
      int reg_size = (typeid(sFloat)==typeid(double2) ? sizeof(double) : sizeof(float));
      return 6 * reg_size;
    }

  public:
    StaggeredDslashCuda(cudaColorSpinorField *out, const fatGFloat *fat0, const fatGFloat *fat1,
			const longGFloat *long0, const longGFloat *long1,
			const phaseFloat *phase0, const phaseFloat *phase1, 
			const QudaReconstructType reconstruct, const cudaColorSpinorField *in,
			const cudaColorSpinorField *x, const double a, const int dagger)
      : DslashCuda(out, in, x, reconstruct, dagger), fat0(fat0), fat1(fat1), long0(long0), 
	long1(long1), phase0(phase0), phase1(phase1), a(a)
    { 
      bindSpinorTex<sFloat>(in, out, x);
    }

    virtual ~StaggeredDslashCuda() { unbindSpinorTex<sFloat>(in, out, x); }

    void apply(const cudaStream_t &stream)
    {
      TuneParam tp = tuneLaunch(*this, getTuning(), getVerbosity());
      dim3 gridDim( (dslashParam.threads+tp.block.x-1) / tp.block.x, 1, 1);
      IMPROVED_STAGGERED_DSLASH(gridDim, tp.block, tp.shared_bytes, stream, dslashParam,
				(sFloat*)out->V(), (float*)out->Norm(), 
				fat0, fat1, long0, long1, phase0, phase1, 
				(sFloat*)in->V(), (float*)in->Norm(), 
				(sFloat*)(x ? x->V() : 0), (float*)(x ? x->Norm() : 0), a); 
    }

    int Nface() { return 6; } 

    /*
      per direction / dimension flops
      SU(3) matrix-vector flops = (8 Nc - 2) * Nc
      xpay = 2 * 2 * Nc * Ns
      
      So for the full dslash we have      
      flops = (2 * 2 * Nd * (8*Nc-2) * Nc)  +  ((2 * 2 * Nd - 1) * 2 * Nc * Ns)
      flops_xpay = flops + 2 * 2 * Nc * Ns
      
      For Asqtad this should give 1146 for Nc=3,Ns=2 and 1158 for the axpy equivalent
    */
    virtual long long flops() const {
      int mv_flops = (8 * in->Ncolor() - 2) * in->Ncolor(); // SU(3) matrix-vector flops
      int ghost_flops = (3 + 1) * (mv_flops + 2*in->Ncolor()*in->Nspin());
      int xpay_flops = 2 * 2 * in->Ncolor() * in->Nspin(); // multiply and add per real component
      int num_dir = 2 * 4; // dir * dim

      long long flops;
      switch(dslashParam.kernel_type) {
      case EXTERIOR_KERNEL_X:
      case EXTERIOR_KERNEL_Y:
      case EXTERIOR_KERNEL_Z:
      case EXTERIOR_KERNEL_T:
	flops = ghost_flops * 2 * in->GhostFace()[dslashParam.kernel_type];
	break;
      case EXTERIOR_KERNEL_ALL:
	{
	  long long ghost_sites = 2 * (in->GhostFace()[0]+in->GhostFace()[1]+in->GhostFace()[2]+in->GhostFace()[3]);
	  flops = ghost_flops * ghost_sites;
	  break;
	}
      case INTERIOR_KERNEL:
	{
	  long long sites = in->VolumeCB();
	  flops = (2*num_dir*mv_flops +                   // SU(3) matrix-vector multiplies
		   (2*num_dir-1)*2*in->Ncolor()*in->Nspin()) * sites;   // accumulation
	  if (x) flops += xpay_flops * sites; // axpy is always on interior

	  // now correct for flops done by exterior kernel
	  long long ghost_sites = 0;
	  for (int d=0; d<4; d++) if (dslashParam.commDim[d]) ghost_sites += 2 * in->GhostFace()[d];
	  flops -= ghost_flops * ghost_sites;
	  
	  break;
	}
      }
      return flops;
    }

    virtual long long bytes() const {
      int gauge_bytes_fat = QUDA_RECONSTRUCT_NO * in->Precision();
      int gauge_bytes_long = reconstruct * in->Precision();
      bool isHalf = in->Precision() == sizeof(short) ? true : false;
      int spinor_bytes = 2 * in->Ncolor() * in->Nspin() * in->Precision() + (isHalf ? sizeof(float) : 0);
      int ghost_bytes = 3 * (spinor_bytes + gauge_bytes_long) + (spinor_bytes + gauge_bytes_fat) + spinor_bytes;
      int num_dir = 2 * 4; // set to 4 dimensions since we take care of 5-d fermions in derived classes where necessary

      long long bytes;
      switch(dslashParam.kernel_type) {
      case EXTERIOR_KERNEL_X:
      case EXTERIOR_KERNEL_Y:
      case EXTERIOR_KERNEL_Z:
      case EXTERIOR_KERNEL_T:
	bytes = ghost_bytes * 2 * in->GhostFace()[dslashParam.kernel_type];
	break;
      case EXTERIOR_KERNEL_ALL:
	{
	  long long ghost_sites = 2 * (in->GhostFace()[0]+in->GhostFace()[1]+in->GhostFace()[2]+in->GhostFace()[3]);
	  bytes = ghost_bytes * ghost_sites;
	  break;
	}
      case INTERIOR_KERNEL:
	{
	  long long sites = in->VolumeCB();
	  bytes = (num_dir*(gauge_bytes_fat + gauge_bytes_long) + // gauge reads
		   num_dir*2*spinor_bytes +                       // spinor reads
		   spinor_bytes)*sites;                           // spinor write
	  if (x) bytes += spinor_bytes;

	  // now correct for bytes done by exterior kernel
	  long long ghost_sites = 0;
	  for (int d=0; d<4; d++) if (dslashParam.commDim[d]) ghost_sites += 2*in->GhostFace()[d];
	  bytes -= ghost_bytes * ghost_sites;
	  
	  break;
	}
      }
      return bytes;
    }

  };
#endif // GPU_STAGGERED_DIRAC

#include <dslash_policy.cuh>

  void improvedStaggeredDslashCuda(cudaColorSpinorField *out, const cudaGaugeField &fatGauge, 
				   const cudaGaugeField &longGauge, const cudaColorSpinorField *in,
				   const int parity, const int dagger, const cudaColorSpinorField *x,
				   const double &k, const int *commOverride, TimeProfile &profile, const QudaDslashPolicy &dslashPolicy)
  {
    inSpinor = (cudaColorSpinorField*)in; // EVIL

#ifdef GPU_STAGGERED_DIRAC

#ifdef MULTI_GPU
    for(int i=0;i < 4; i++){
      if(commDimPartitioned(i) && (fatGauge.X()[i] < 6)){
	errorQuda("ERROR: partitioned dimension with local size less than 6 is not supported in staggered dslash\n");
      }    
    }
#endif

    int Npad = (in->Ncolor()*in->Nspin()*2)/in->FieldOrder(); // SPINOR_HOP in old code

    dslashParam.parity = parity;
    dslashParam.gauge_stride = fatGauge.Stride();
    dslashParam.long_gauge_stride = longGauge.Stride();
    dslashParam.fat_link_max = fatGauge.LinkMax();

    for(int i=0;i<4;i++){
      dslashParam.ghostDim[i] = commDimPartitioned(i); // determines whether to use regular or ghost indexing at boundary
      dslashParam.ghostOffset[i] = Npad*(in->GhostOffset(i) + in->Stride());
      dslashParam.ghostNormOffset[i] = in->GhostNormOffset(i) + in->Stride();
      dslashParam.commDim[i] = (!commOverride[i]) ? 0 : commDimPartitioned(i); // switch off comms if override = 0
    }

    void *fatGauge0, *fatGauge1;
    void* longGauge0, *longGauge1;
    bindFatGaugeTex(fatGauge, parity, &fatGauge0, &fatGauge1);
    bindLongGaugeTex(longGauge, parity, &longGauge0, &longGauge1);
    void *longPhase0 = (char*)longGauge0 + longGauge.PhaseOffset();
    void *longPhase1 = (char*)longGauge1 + longGauge.PhaseOffset();   

    if (in->Precision() != fatGauge.Precision() || in->Precision() != longGauge.Precision()){
      errorQuda("Mixing gauge and spinor precision not supported"
		"(precision=%d, fatlinkGauge.precision=%d, longGauge.precision=%d",
		in->Precision(), fatGauge.Precision(), longGauge.Precision());
    }

    DslashCuda *dslash = 0;
    size_t regSize = sizeof(float);

    if (in->Precision() == QUDA_DOUBLE_PRECISION) {
      dslash = new StaggeredDslashCuda<double2, double2, double2, double>
	(out, (double2*)fatGauge0, (double2*)fatGauge1,
	 (double2*)longGauge0, (double2*)longGauge1,
	 (double*)longPhase0, (double*)longPhase1, 
	 longGauge.Reconstruct(), in, x, k, dagger);
      regSize = sizeof(double);
    } else if (in->Precision() == QUDA_SINGLE_PRECISION) {
      dslash = new StaggeredDslashCuda<float2, float2, float4, float>
	(out, (float2*)fatGauge0, (float2*)fatGauge1,
	 (float4*)longGauge0, (float4*)longGauge1, 
	 (float*)longPhase0, (float*)longPhase1,
	 longGauge.Reconstruct(), in, x, k, dagger);
    } else if (in->Precision() == QUDA_HALF_PRECISION) {	
      dslash = new StaggeredDslashCuda<short2, short2, short4, short>
	(out, (short2*)fatGauge0, (short2*)fatGauge1,
	 (short4*)longGauge0, (short4*)longGauge1, 
	 (short*)longPhase0, (short*)longPhase1,
	 longGauge.Reconstruct(), in, x, k, dagger);
    }

#ifndef GPU_COMMS
    DslashPolicyImp* dslashImp = DslashFactory::create(dslashPolicy);
#else
    DslashPolicyImp* dslashImp = DslashFactory::create(QUDA_GPU_COMMS_DSLASH);
#endif
    (*dslashImp)(*dslash, const_cast<cudaColorSpinorField*>(in), regSize, parity, dagger, in->Volume(), in->GhostFace(), profile);
    delete dslashImp;

    delete dslash;
    unbindFatGaugeTex(fatGauge);
    unbindLongGaugeTex(longGauge);

    checkCudaError();

#else
    errorQuda("Staggered dslash has not been built");
#endif  // GPU_STAGGERED_DIRAC
  }

}
