#include <cstdio>
#include <cstdlib>

#include <tune_quda.h>
#include <quda_internal.h>
#include <gauge_field_order.h>
#include <quda_matrix.h>
#include <color_spinor.h>
#include <dslash_quda.h>

namespace quda {

#ifdef GPU_CLOVER_DIRAC

  namespace { // anonymous
#include <texture.h>
  }
  
  template<int N>
  void createEventArray(cudaEvent_t (&event)[N], unsigned int flags=cudaEventDefault)
  {
    for(int i=0; i<N; ++i)
      cudaEventCreate(&event[i],flags);
    return;
  }
  
  template<int N>
  void destroyEventArray(cudaEvent_t (&event)[N])
  {
    for(int i=0; i<N; ++i)
      cudaEventDestroy(event[i]);
  }
  
  
  static cudaEvent_t packEnd;
  static cudaEvent_t gatherEnd[4];
  static cudaEvent_t scatterEnd[4];
  static cudaEvent_t oprodStart;
  static cudaEvent_t oprodEnd;


  void createCloverForceEvents(){
#ifdef MULTI_GPU
    cudaEventCreate(&packEnd, cudaEventDisableTiming);
    createEventArray(gatherEnd, cudaEventDisableTiming);
    createEventArray(scatterEnd, cudaEventDisableTiming);
#endif
    cudaEventCreate(&oprodStart, cudaEventDisableTiming);
    cudaEventCreate(&oprodEnd, cudaEventDisableTiming);
    return;
  }

  void destroyCloverForceEvents(){
#ifdef MULTI_GPU
    destroyEventArray(gatherEnd);
    destroyEventArray(scatterEnd);
    cudaEventDestroy(packEnd);
#endif
    cudaEventDestroy(oprodStart);
    cudaEventDestroy(oprodEnd);
    return;
  }


  enum KernelType {OPROD_INTERIOR_KERNEL, OPROD_EXTERIOR_KERNEL};

  template<typename Complex, typename Output, typename Gauge, typename InputA, typename InputB, typename InputC, typename InputD>
    struct CloverForceArg {
      unsigned int length;
      int X[4];
      unsigned int parity;
      unsigned int dir;
      unsigned int ghostOffset[4];
      unsigned int displacement;
      KernelType kernelType;
      bool partitioned[4];
      InputA inA;
      InputB inB_shift;
      InputC inC;
      InputD inD_shift;
      Gauge  gauge;
      Output force;
      typename RealTypeId<Complex>::Type coeff;
      
    CloverForceArg(const unsigned int parity,
		   const unsigned int dir,
		   const unsigned int *ghostOffset,
		   const unsigned int displacement,   
		   const KernelType kernelType, 
		   const double coeff,
		   InputA& inA,
		   InputB& inB_shift,
		   InputC& inC,
		   InputD& inD_shift,
		   Gauge& gauge,
		   Output& force,
		   GaugeField &meta) : length(meta.VolumeCB()), parity(parity), dir(5),
				       displacement(displacement), kernelType(kernelType), 
				       coeff(coeff), inA(inA), inB_shift(inB_shift), 
				       inC(inC), inD_shift(inD_shift), 
				       gauge(gauge), force(force)
      {
        for(int i=0; i<4; ++i) this->X[i] = meta.X()[i];
        for(int i=0; i<4; ++i) this->ghostOffset[i] = ghostOffset[i];
        for(int i=0; i<4; ++i) this->partitioned[i] = commDimPartitioned(i) ? true : false;
      }
  };

  enum IndexType {
    EVEN_X = 0,
    EVEN_Y = 1,
    EVEN_Z = 2,
    EVEN_T = 3
  };

  template <IndexType idxType>
    static __device__ __forceinline__ void coordsFromIndex(int& idx, int c[4],  
        const unsigned int cb_idx, const unsigned int parity, const int X[4])
    {
      const int &LX = X[0];
      const int &LY = X[1];
      const int &LZ = X[2];
      const int XYZ = X[2]*X[1]*X[0];
      const int XY = X[1]*X[0];

      idx = 2*cb_idx;

      int x, y, z, t;

      if (idxType == EVEN_X /*!(LX & 1)*/) { // X even
        //   t = idx / XYZ;
        //   z = (idx / XY) % Z;
        //   y = (idx / X) % Y;
        //   idx += (parity + t + z + y) & 1;
        //   x = idx % X;
        // equivalent to the above, but with fewer divisions/mods:
        int aux1 = idx / LX;
        x = idx - aux1 * LX;
        int aux2 = aux1 / LY;
        y = aux1 - aux2 * LY;
        t = aux2 / LZ;
        z = aux2 - t * LZ;
        aux1 = (parity + t + z + y) & 1;
        x += aux1;
        idx += aux1;
      } else if (idxType == EVEN_Y /*!(LY & 1)*/) { // Y even
        t = idx / XYZ;
        z = (idx / XY) % LZ;
        idx += (parity + t + z) & 1;
        y = (idx / LX) % LY;
        x = idx % LX;
      } else if (idxType == EVEN_Z /*!(LZ & 1)*/) { // Z even
        t = idx / XYZ;
        idx += (parity + t) & 1;
        z = (idx / XY) % LZ;
        y = (idx / LX) % LY;
        x = idx % LX;
      } else {
        idx += parity;
        t = idx / XYZ;
        z = (idx / XY) % LZ;
        y = (idx / LX) % LY;
        x = idx % LX;
      }

      c[0] = x;
      c[1] = y;
      c[2] = z;
      c[3] = t;
    }



  // Get the  coordinates for the exterior kernels
  __device__ void coordsFromIndex(int x[4], const unsigned int cb_idx, const int X[4], const unsigned int dir, const int displacement, const unsigned int parity)
  {
    unsigned int Xh[2] = {X[0]/2, X[1]/2};
    switch(dir){
    case 0:
      x[2] = cb_idx/Xh[1] % X[2];
      x[3] = cb_idx/(Xh[1]*X[2]) % X[3];
      x[0] = cb_idx/(Xh[1]*X[2]*X[3]);
      x[0] += (X[0] - displacement);
      x[1] = 2*(cb_idx % Xh[1]) + ((x[0]+x[2]+x[3]+parity)&1);
      break;
      
    case 1:
      x[2] = cb_idx/Xh[0] % X[2];
      x[3] = cb_idx/(Xh[0]*X[2]) % X[3];
      x[1] = cb_idx/(Xh[0]*X[2]*X[3]);
      x[1] += (X[1] - displacement);
      x[0] = 2*(cb_idx % Xh[0]) + ((x[1]+x[2]+x[3]+parity)&1);
      break;
      
    case 2:
      x[1] = cb_idx/Xh[0] % X[1];
      x[3] = cb_idx/(Xh[0]*X[1]) % X[3];
      x[2] = cb_idx/(Xh[0]*X[1]*X[3]);
      x[2] += (X[2] - displacement);
      x[0] = 2*(cb_idx % Xh[0]) + ((x[1]+x[2]+x[3]+parity)&1);
      break;
      
    case 3:
      x[1] = cb_idx/Xh[0] % X[1];
      x[2] = cb_idx/(Xh[0]*X[1]) % X[2];
      x[3] = cb_idx/(Xh[0]*X[1]*X[2]);
      x[3] += (X[3] - displacement);
      x[0] = 2*(cb_idx % Xh[0]) + ((x[1]+x[2]+x[3]+parity)&1);   
      break;
    }
    return;
  }


  __device__  int ghostIndexFromCoords(const int x[4], const int X[4], unsigned int dir, const int shift)
    {
      int ghost_idx;
      /*
	FIXME The below could be absorbed into the ghost offset values as
	all they are just offsets to get to the forward face from
	starting at the backward face.

	The factor of 6 comes from being spin projected, with Float2
	indexing assumed.  This needs to be fixed for single precision
	(Float4) indexing.
       */
      if(shift > 0){
        if((x[dir] + shift) >= X[dir]){
          switch(dir){
            case 0:
	      ghost_idx = 6*(X[3]*X[2]*X[1])/2;
              break;          
            case 1:
	      ghost_idx = 6*(X[3]*X[2]*X[0])/2;
              break;
            case 2:
	      ghost_idx = 6*(X[3]*X[1]*X[0])/2;
              break;
            case 3:
	      ghost_idx = 6*(X[2]*X[1]*X[0])/2;
              break;
            default:
              break;
          } // switch
        } // x[dir] + shift[dir] >= X[dir]
      }else{ // shift < 0
	ghost_idx = 0;
      } // shift < 0

      return ghost_idx;
    }




  __device__ __forceinline__
  int neighborIndex(const unsigned int& cb_idx, const int shift[4],  const bool partitioned[4], const unsigned int& parity, 
		    const int X[4]){    
    int  full_idx;
    int x[4]; 
    coordsFromIndex<EVEN_X>(full_idx, x, cb_idx, parity, X);
    
#ifdef MULTI_GPU
    for(int dim = 0; dim<4; ++dim){
      if(partitioned[dim])
	if( (x[dim]+shift[dim])<0 || (x[dim]+shift[dim])>=X[dim]) return -1;
    }
#endif
    
    for(int dim=0; dim<4; ++dim){
      x[dim] = shift[dim] ? (x[dim]+shift[dim] + X[dim]) % X[dim] : x[dim];
    }
    return  (((x[3]*X[2] + x[2])*X[1] + x[1])*X[0] + x[0]) >> 1;
  }



  template<typename Complex, typename Output, typename Gauge, typename InputA, typename InputB, typename InputC, typename InputD>
  __global__ void interiorOprodKernel(CloverForceArg<Complex, Output, Gauge, InputA, InputB, InputC, InputD> arg) {
    typedef typename RealTypeId<Complex>::Type real;
    int idx = blockIdx.x*blockDim.x + threadIdx.x;

    ColorSpinor<real,3,4> A, B_shift, C, D_shift;
    Matrix<Complex,3> U, result, temp;

    while(idx<arg.length){
      arg.inA.load(static_cast<Complex*>(A.data), idx);
      arg.inC.load(static_cast<Complex*>(C.data), idx);
      
      for(int dim=0; dim<4; ++dim){
	int shift[4] = {0,0,0,0};
	shift[dim] = 1;
	const int nbr_idx = neighborIndex(idx, shift, arg.partitioned, arg.parity, arg.X);

	if(nbr_idx >= 0){
	  arg.inB_shift.load(static_cast<Complex*>(B_shift.data), nbr_idx);
	  arg.inD_shift.load(static_cast<Complex*>(D_shift.data), nbr_idx);

	  B_shift = (B_shift.project(dim,1)).reconstruct(dim,1);	  
	  result = outerProdSpinTrace(B_shift,A);

	  D_shift = (D_shift.project(dim,-1)).reconstruct(dim,-1);
	  result += outerProdSpinTrace(D_shift,C);
	  
	  arg.force.load(reinterpret_cast<real*>(temp.data), idx, dim, arg.parity); 
	  arg.gauge.load(reinterpret_cast<real*>(U.data), idx, dim, arg.parity); 
	  result = temp + U*result*arg.coeff;
	  arg.force.save(reinterpret_cast<real*>(result.data), idx, dim, arg.parity); 
	}
      } // dir
      idx += gridDim.x*blockDim.x;
    }
    return;
  } // interiorOprodKernel

  template<int dim, typename Complex, typename Output, typename Gauge, typename InputA, typename InputB, typename InputC, typename InputD> 
  __global__ void exteriorOprodKernel(CloverForceArg<Complex, Output, Gauge, InputA, InputB, InputC, InputD> arg) {
    typedef typename RealTypeId<Complex>::Type real;
    int cb_idx = blockIdx.x*blockDim.x + threadIdx.x;
    
    ColorSpinor<real,3,4> A, B_shift, C, D_shift;
    ColorSpinor<real,3,2> projected_tmp;
    Matrix<Complex,3> U, result, temp;
    
    int x[4];
    while(cb_idx<arg.length){
      coordsFromIndex(x, cb_idx, arg.X, dim, arg.displacement, arg.parity); 
      const unsigned int bulk_cb_idx = ((((x[3]*arg.X[2] + x[2])*arg.X[1] + x[1])*arg.X[0] + x[0]) >> 1);
      arg.inA.load(static_cast<Complex*>(A.data), bulk_cb_idx);
      arg.inC.load(static_cast<Complex*>(C.data), bulk_cb_idx);
      
      const unsigned int ghost_idx = arg.ghostOffset[dim] + ghostIndexFromCoords(x, arg.X, dim, arg.displacement) + cb_idx;
      arg.inB_shift.loadGhost(static_cast<Complex*>(projected_tmp.data), ghost_idx, dim);
      B_shift = projected_tmp.reconstruct(dim, 1);
      result = outerProdSpinTrace(B_shift,A);
      
      arg.inD_shift.loadGhost(static_cast<Complex*>(projected_tmp.data), ghost_idx, dim);
      D_shift = projected_tmp.reconstruct(dim,-1);
      result += outerProdSpinTrace(D_shift,C);
      
      arg.force.load(reinterpret_cast<real*>(temp.data), bulk_cb_idx, dim, arg.parity); 
      arg.gauge.load(reinterpret_cast<real*>(U.data), bulk_cb_idx, dim, arg.parity); 
      result = temp + U*result*arg.coeff;
      arg.force.save(reinterpret_cast<real*>(result.data), bulk_cb_idx, dim, arg.parity); 
      
      cb_idx += gridDim.x*blockDim.x;
    }
    return;
  } // exteriorOprodKernel
  
  template<typename Complex, typename Output, typename Gauge, typename InputA, typename InputB, typename InputC, typename InputD> 
  class CloverForce : public Tunable {
    
  private:
    CloverForceArg<Complex,Output,Gauge,InputA,InputB,InputC,InputD> &arg;
    const GaugeField &meta;
    QudaFieldLocation location; // location of the lattice fields
    
    unsigned int sharedBytesPerThread() const { return 0; }
    unsigned int sharedBytesPerBlock(const TuneParam &) const { return 0; }
    
    unsigned int minThreads() const { return arg.length; }
    bool tuneGridDim() const { return false; }
    
  public:
    CloverForce(CloverForceArg<Complex,Output,Gauge,InputA,InputB,InputC,InputD> &arg,
		const GaugeField &meta, QudaFieldLocation location)
      : arg(arg), meta(meta), location(location) {
      writeAuxString("prec=%lu,stride=%d", sizeof(Complex)/2, arg.inA.Stride());
      // this sets the communications pattern for the packing kernel
      int comms[QUDA_MAX_DIM] = { commDimPartitioned(0), commDimPartitioned(1), commDimPartitioned(2), commDimPartitioned(3) };
      setPackComms(comms);
    } 
    
    virtual ~CloverForce() {}
    
    void apply(const cudaStream_t &stream){
      if(location == QUDA_CUDA_FIELD_LOCATION){
	// Disable tuning for the time being
	TuneParam tp = tuneLaunch(*this,getTuning(),getVerbosity());
	
	if(arg.kernelType == OPROD_INTERIOR_KERNEL){
	  interiorOprodKernel<<<tp.grid,tp.block,tp.shared_bytes,stream>>>(arg);
	} else if(arg.kernelType == OPROD_EXTERIOR_KERNEL) {
	  if (arg.dir == 0) exteriorOprodKernel<0><<<tp.grid,tp.block,tp.shared_bytes, stream>>>(arg);
	  if (arg.dir == 1) exteriorOprodKernel<1><<<tp.grid,tp.block,tp.shared_bytes, stream>>>(arg);
	  if (arg.dir == 2) exteriorOprodKernel<2><<<tp.grid,tp.block,tp.shared_bytes, stream>>>(arg);
	  if (arg.dir == 3) exteriorOprodKernel<3><<<tp.grid,tp.block,tp.shared_bytes, stream>>>(arg);
	} else {
	  errorQuda("Kernel type not supported\n");
	}
      }else{ // run the CPU code
	errorQuda("No CPU support for staggered outer-product calculation\n");
      }
    } // apply
    
    void preTune(){
      this->arg.force.save();
    }
    void postTune(){
      this->arg.force.load();
    }
  
    long long flops() const { 
      if (arg.kernelType == OPROD_INTERIOR_KERNEL) {
	((long long)arg.length)*4*(24 + 144 + 234); // spin project + spin trace + multiply-add
      } else {
	((long long)arg.length)*(144 + 234); // spin trace + multiply-add
      }
    }
    long long bytes() const { 
      if (arg.kernelType == OPROD_INTERIOR_KERNEL) {
	((long long)arg.length)*(arg.inA.Bytes() + arg.inC.Bytes() + 4*(arg.inB_shift.Bytes() + arg.inD_shift.Bytes() + 2*arg.force.Bytes() + arg.gauge.Bytes())); 
      } else {
	((long long)arg.length)*(arg.inA.Bytes() + arg.inB_shift.Bytes()/2 + arg.inC.Bytes() + arg.inD_shift.Bytes()/2 + 2*arg.force.Bytes() + arg.gauge.Bytes()); 
      }
    }
  
    TuneKey tuneKey() const { 
      char new_aux[TuneKey::aux_n];
      strcpy(new_aux, aux);
      if (arg.kernelType == OPROD_INTERIOR_KERNEL) {
	strcat(new_aux, ",interior");
      } else {
	strcat(new_aux, ",exterior");
	if (arg.dir==0) strcat(new_aux, ",dir=0");
	else if (arg.dir==1) strcat(new_aux, ",dir=1");
	else if (arg.dir==2) strcat(new_aux, ",dir=2");
	else if (arg.dir==3) strcat(new_aux, ",dir=3");
      }
      return TuneKey(meta.VolString(), typeid(*this).name(), new_aux);
    }
  }; // CloverForce
  
  void exchangeGhost(cudaColorSpinorField &a, int parity, int dag) {
    // need to enable packing in temporal direction to get spin-projector correct
    bool pack_old = getKernelPackT();
    setKernelPackT(true);

    // first transfer src1
    cudaDeviceSynchronize();

    a.pack(1, 1-parity, dag, Nstream-1, 0);

    cudaDeviceSynchronize();

    for(int i=3; i>=0; i--){
      if(commDimPartitioned(i)){
	// Initialize the host transfer from the source spinor
	a.gather(1, dag, 2*i); 
      } // commDim(i)
    } // i=3,..,0
    
    cudaDeviceSynchronize();
    
    for (int i=3; i>=0; i--) {
      if(commDimPartitioned(i)) {
	a.commsStart(1, 2*i, dag);
      }
    }
    
    for (int i=3; i>=0; i--) {
      if(commDimPartitioned(i)) {
	a.commsWait(1, 2*i, dag);
	a.scatter(1, dag, 2*i);
      }
    }
    
    cudaDeviceSynchronize();
    setKernelPackT(pack_old); // restore packing state
  }

  template<typename Complex, typename Output, typename Gauge, typename InputA, typename InputB, typename InputC, typename InputD>
  void computeCloverForceCuda(Output force, Gauge gauge, cudaGaugeField& out, 
			      InputA& inA, InputB& inB, InputC &inC, InputD &inD,
			      cudaColorSpinorField& src1, cudaColorSpinorField& src2,
			      const unsigned int parity, const int faceVolumeCB[4], 
			      const unsigned int ghostOffset[4], const double coeff)
    {
      cudaEventRecord(oprodStart, streams[Nstream-1]);
      // Create the arguments for the interior kernel 
      CloverForceArg<Complex,Output,Gauge,InputA,InputB,InputC,InputD> arg(parity, 0, ghostOffset, 1, OPROD_INTERIOR_KERNEL, coeff, inA, inB, inC, inD, gauge, force, out);
      CloverForce<Complex,Output,Gauge,InputA,InputB,InputC,InputD> oprod(arg, out, QUDA_CUDA_FIELD_LOCATION);

      int dag = 1;
      exchangeGhost(src1, parity, dag);
      exchangeGhost(src2, parity, 1-dag);

      arg.kernelType = OPROD_INTERIOR_KERNEL;
      arg.length = src1.VolumeCB();
      oprod.apply(0); 

#ifdef MULTI_GPU
      for (int i=3; i>=0; i--) {
	if (commDimPartitioned(i)) {
	  // update parameters for this exterior kernel
	  arg.kernelType = OPROD_EXTERIOR_KERNEL;
	  arg.dir = i;
	  arg.length = faceVolumeCB[i];
	  arg.displacement = 1;
	  oprod.apply(0); 
	}
      } // i=3,..,0 
#endif
    } // computeCloverForceCuda

#endif // GPU_CLOVER_FORCE

  void computeCloverForce(cudaGaugeField& force,
			  const cudaGaugeField& U,
			  cudaColorSpinorField& x,  
			  cudaColorSpinorField& p,
			  const double coeff)
  {

#ifdef GPU_CLOVER_DIRAC

    if(force.Order() != QUDA_FLOAT2_GAUGE_ORDER)
      errorQuda("Unsupported output ordering: %d\n", force.Order());    

    unsigned int ghostOffset[4] = {0,0,0,0};
#ifdef MULTI_GPU
    const unsigned int Npad = x.Ncolor()*x.Nspin()*2/x.FieldOrder();
    for(int dir=0; dir<4; ++dir){
      ghostOffset[dir] = Npad*(x.GhostOffset(dir) + x.Stride()); 
    }
#endif

    if(x.Precision() != force.Precision()) errorQuda("Mixed precision not supported: %d %d\n", x.Precision(), force.Precision());

    createCloverForceEvents(); // FIXME not actually used

    for (int parity=0; parity<2; parity++) {
      
      cudaColorSpinorField& inA = (parity&1) ? p.Odd() : p.Even();
      cudaColorSpinorField& inB = (parity&1) ? x.Even(): x.Odd();
      cudaColorSpinorField& inC = (parity&1) ? x.Odd() : x.Even();
      cudaColorSpinorField& inD = (parity&1) ? p.Even(): p.Odd();
      
      if(x.Precision() == QUDA_DOUBLE_PRECISION){
	Spinor<double2, double2, double2, 12, 0, 0> spinorA(inA);
	Spinor<double2, double2, double2, 12, 0, 1> spinorB(inB);
	Spinor<double2, double2, double2, 12, 0, 0> spinorC(inC);
	Spinor<double2, double2, double2, 12, 0, 1> spinorD(inD);
	if (U.Reconstruct() == QUDA_RECONSTRUCT_NO) {
	  computeCloverForceCuda<double2>(FloatNOrder<double, 18, 2, 18>(force), 
					  FloatNOrder<double,18, 2, 18>(U),
					  force, spinorA, spinorB, spinorC, spinorD, inB, inD, parity, 
					  inB.GhostFace(), ghostOffset, coeff);
	} else if (U.Reconstruct() == QUDA_RECONSTRUCT_12) {
	  computeCloverForceCuda<double2>(FloatNOrder<double, 18, 2, 18>(force), 
					  FloatNOrder<double,18, 2, 12>(U),
					  force, spinorA, spinorB, spinorC, spinorD, inB, inD, parity, 
					  inB.GhostFace(), ghostOffset, coeff);
	} else {
	  errorQuda("Unsupported recontruction type");
	}
      }else if(x.Precision() == QUDA_SINGLE_PRECISION){
#if 0
	Spinor<float4, float4, float4, 6, 0, 0> spinorA(inA);
	Spinor<float4, float4, float4, 6, 0, 1> spinorB(inB);
	if (U.Reconstruct() == QUDA_RECONSTRUCT_NO) {
	  computeCloverForceCuda<float2>(FloatNOrder<float, 18, 2, 18>(force), 
					 FloatNOrder<float, 18, 2, 18>(U), 
					 force, spinorA, spinorB, spinorC, spinorD, inB, inD, parity, 
					 inB.GhostFace(), ghostOffset, coeff);
	} else if (U.Reconstruct() == QUDA_RECONSTRUCT_12) {
	  computeCloverForceCuda<float2>(FloatNOrder<float, 18, 2, 18>(force), 
					 FloatNOrder<float, 18, 4, 12>(U), 
					 force, spinorA, spinorB, spinorC, spinorD, inB, inD, parity, 
					 inB.GhostFace(), ghostOffset, coeff);
	}
#endif
      } else {
	errorQuda("Unsupported precision: %d\n", x.Precision());
      }
    }

    destroyCloverForceEvents(); // not actually used

#else // GPU_CLOVER_DIRAC not defined
   errorQuda("Clover Dirac operator has not been built!"); 
#endif

   checkCudaError();
   return;
  } // computeCloverForce



} // namespace quda
