#include <quda_internal.h>
#include <gauge_field_order.h>

namespace quda {

  template <typename Order, int nDim, int dim>
  struct ExtractGhostExArg {
    Order order;
    int X[nDim];
    int R[nDim];
    int surfaceCB[nDim];
    int A0[nDim];
    int A1[nDim];
    int B0[nDim];
    int B1[nDim];
    int C0[nDim];
    int C1[nDim];
    int fBody[nDim][nDim];
    int fBuf[nDim][nDim];
    int localParity[nDim];
    ExtractGhostExArg(const Order &order, const int *X_, const int *R_, 
		      const int *surfaceCB_, 
		      const int *A0_, const int *A1_, const int *B0_, const int *B1_, 
		      const int *C0_, const int *C1_, const int fBody_[nDim][nDim], 
		      const int fBuf_[nDim][nDim], const int *localParity_) 
  : order(order) { 
      for (int d=0; d<nDim; d++) {
	X[d] = X_[d];
	R[d] = R_[d];
	surfaceCB[d] = surfaceCB_[d];
	A0[d] = A0_[d];
	A1[d] = A1_[d];
	B0[d] = B0_[d];
	B1[d] = B1_[d];
	C0[d] = C0_[d];
	C1[d] = C1_[d];
	for (int e=0; e<nDim; e++) {
	  fBody[d][e] = fBody_[d][e];
	  fBuf[d][e] = fBuf_[d][e];
	}
	localParity[d] = localParity_[d]; 
      }
    }

  };

  template <typename Float, int length, int dim, typename Arg>
  __device__ __host__ void extractor(Arg &arg, int dir, int a, int b, 
				     int c, int d, int g, int parity) {
    typename mapper<Float>::type u[length];
    int srcIdx = (a*arg.fBody[dim][0] + b*arg.fBody[dim][1] + 
		  c*arg.fBody[dim][2] + d*arg.fBody[dim][3]) >> 1;
    
    int dstIdx = (a*arg.fBuf[dim][0] + b*arg.fBuf[dim][1] + 
		  c*arg.fBuf[dim][2] + (d-(dir?arg.X[dim]:arg.R[dim]))*arg.fBuf[dim][3]) >> 1;
    
    // load the ghost element from the bulk
    arg.order.load(u, srcIdx, g, parity); 

    // need dir dependence in write
    // srcIdx is used here to determine boundary condition
    arg.order.saveGhostEx(u, dstIdx, srcIdx, dir, dim, g, 
			  (parity+arg.localParity[dim])&1, arg.R);
  }


  template <typename Float, int length, int dim, typename Arg>
  __device__ __host__ void injector(Arg &arg, int dir, int a, int b, 
				    int c, int d, int g, int parity) {
    typename mapper<Float>::type u[length];
    int srcIdx = (a*arg.fBuf[dim][0] + b*arg.fBuf[dim][1] + 
		  c*arg.fBuf[dim][2] + (d-dir*(arg.X[dim]+arg.R[dim]))*arg.fBuf[dim][3]) >> 1;
    
    int dstIdx = (a*arg.fBody[dim][0] + b*arg.fBody[dim][1] + 
		  c*arg.fBody[dim][2] + d*arg.fBody[dim][3]) >> 1;
    
    // need dir dependence in read
    // dstIdx is used here to determine boundary condition
    arg.order.loadGhostEx(u, srcIdx, dstIdx, dir, dim, g, 
			  (parity+arg.localParity[dim])&1, arg.R);
    
    arg.order.save(u, dstIdx, g, parity); // save the ghost element into the bulk
  }
  
  /**
     Generic CPU gauge ghost extraction and packing
     NB This routines is specialized to four dimensions
  */
  template <typename Float, int length, int nDim, int dim, typename Order, bool extract>
  void extractGhostEx(ExtractGhostExArg<Order,nDim,dim> arg) {  
    typedef typename mapper<Float>::type RegType;

    for (int parity=0; parity<2; parity++) {

      // the following 4-way loop means this is specialized for 4 dimensions 
      // dir = 0 backwards, dir = 1 forwards
      for (int dir = 0; dir<2; dir++) {

	int D0 = extract ? dir*arg.X[dim] + (1-dir)*arg.R[dim] : dir*(arg.X[dim] + arg.R[dim]); 
	  
	for (int d=D0; d<D0+arg.R[dim]; d++) {
	  for (int a=arg.A0[dim]; a<arg.A1[dim]; a++) { // loop over the interior surface
	    for (int b=arg.B0[dim]; b<arg.B1[dim]; b++) { // loop over the interior surface
	      for (int c=arg.C0[dim]; c<arg.C1[dim]; c++) { // loop over the interior surface
		for (int g=0; g<arg.order.geometry; g++) {

		  // we only do the extraction for parity we are currently working on
		  int oddness = (a+b+c+d) & 1;
		  if (oddness == parity) {
		    if (extract) extractor<Float,length,dim>(arg, dir, a, b, c, d, g, parity);
		    else injector<Float,length,dim>(arg, dir, a, b, c, d, g, parity);
		  } // oddness == parity
		} // g
	      } // c
	    } // b
	  } // a
	} // d
      } // dir
      
    } // parity

  }

  /**
     Generic GPU gauge ghost extraction and packing
     NB This routines is specialized to four dimensions
     FIXME this implementation will have two-way warp divergence
  */
  
  /**
     Generic CPU gauge ghost extraction and packing
     NB This routines is specialized to four dimensions
  */
  template <typename Float, int length, int nDim, int dim, typename Order, bool extract>
  __global__ void extractGhostExKernel(ExtractGhostExArg<Order,nDim,dim> arg) {  
    typedef typename mapper<Float>::type RegType;

    // parallelize over parity and dir using block or grid 
    /*for (int parity=0; parity<2; parity++) {*/
    {
      int parity = blockIdx.z;

      // the following 4-way loop means this is specialized for 4 dimensions 
      // dir = 0 backwards, dir = 1 forwards
      //for (int dir = 0; dir<2; dir++) {
      {
	int dir = blockIdx.y;

	// this will have two-warp divergence since we only do work on
	// one parity but parity alternates between threads
	// linear index used for writing into ghost buffer
	int X = blockIdx.x * blockDim.x + threadIdx.x; 	

	int dA = arg.A1[dim]-arg.A0[dim];
	int dB = arg.B1[dim]-arg.B0[dim];
	int dC = arg.C1[dim]-arg.C0[dim];
	int D0 = extract ? dir*arg.X[dim] + (1-dir)*arg.R[dim] : dir*(arg.X[dim] + arg.R[dim]); 

	if (X >= arg.R[dim]*dA*dB*dC*arg.order.geometry) return;

	// thread order is optimized to maximize coalescing
	// X = (((g*R + d) * dA + a)*dB + b)*dC + c
	int gdab = X / dC;
	int c    = arg.C0[dim] + X    - gdab*dC;
	int gda  = gdab / dB;
	int b    = arg.B0[dim] + gdab - gda *dB;
	int gd   = gda / dA;
	int a    = arg.A0[dim] + gda  - gd  *dA;
	int g    = gd / arg.R[dim];
	int d    = D0          + gd   - g   *arg.R[dim];

	// we only do the extraction for parity we are currently working on
	int oddness = (a+b+c+d) & 1;
	if (oddness == parity) {
	  if (extract) extractor<Float,length,dim>(arg, dir, a, b, c, d, g, parity);
	  else injector<Float,length,dim>(arg, dir, a, b, c, d, g, parity);
	} // oddness == parity
      } // dir
      
    } // parity

  }

  template <typename Float, int length, int nDim, int dim, typename Order>
  class ExtractGhostEx : Tunable {
    ExtractGhostExArg<Order,nDim,dim> arg;
    int size;
    bool extract;
    const GaugeField &meta;
    QudaFieldLocation location;

  private:
    unsigned int sharedBytesPerThread() const { return 0; }
    unsigned int sharedBytesPerBlock(const TuneParam &param) const { return 0 ;}

    bool tuneGridDim() const { return false; } // Don't tune the grid dimensions.
    unsigned int minThreads() const { return size; }

  public:
    ExtractGhostEx(ExtractGhostExArg<Order,nDim,dim> &arg, bool extract, 
		   const GaugeField &meta, QudaFieldLocation location)
      : arg(arg), extract(extract), meta(meta), location(location) {
      int dA = arg.A1[dim]-arg.A0[dim];
      int dB = arg.B1[dim]-arg.B0[dim];
      int dC = arg.C1[dim]-arg.C0[dim];
      size = arg.R[dim]*dA*dB*dC*arg.order.geometry;
      writeAuxString("prec=%lu,stride=%d,extract=%d,dimension=%d",
		     sizeof(Float),arg.order.stride, extract, dim);
    }
    virtual ~ExtractGhostEx() { ; }
  
    void apply(const cudaStream_t &stream) {
      if (extract) {
	if (location==QUDA_CPU_FIELD_LOCATION) {
	  extractGhostEx<Float,length,nDim,dim,Order,true>(arg);
	} else {
	  TuneParam tp = tuneLaunch(*this, getTuning(), getVerbosity());
	  tp.grid.y = 2;
	  tp.grid.z = 2;
	  extractGhostExKernel<Float,length,nDim,dim,Order,true> 
	    <<<tp.grid, tp.block, tp.shared_bytes, stream>>>(arg);
	}
      } else { // we are injecting
	if (location==QUDA_CPU_FIELD_LOCATION) {
	  extractGhostEx<Float,length,nDim,dim,Order,false>(arg);
	} else {
	  TuneParam tp = tuneLaunch(*this, getTuning(), getVerbosity());
	  tp.grid.y = 2;
	  tp.grid.z = 2;
	  extractGhostExKernel<Float,length,nDim,dim,Order,false> 
	    <<<tp.grid, tp.block, tp.shared_bytes, stream>>>(arg);
	}
      }
    }

    TuneKey tuneKey() const { return TuneKey(meta.VolString(), typeid(*this).name(), aux); }

    std::string paramString(const TuneParam &param) const { // Don't bother printing the grid dim.
      std::stringstream ps;
      ps << "block=(" << param.block.x << "," << param.block.y << "," << param.block.z << "), ";
      ps << "shared=" << param.shared_bytes;
      return ps.str();
    }

    long long flops() const { return 0; } 
    long long bytes() const { return 2 * 2 * 2 * size * arg.order.Bytes(); } // 2 for i/o    
  };


  /**
     Generic CPU gauge ghost extraction and packing
     NB This routines is specialized to four dimensions
     @param E the extended gauge dimensions
     @param R array holding the radius of the extended region 
     @param extract Whether we are extracting or injecting the ghost zone
  */
  template <typename Float, int length, typename Order>
  void extractGhostEx(Order order, const int dim, const int *surfaceCB, const int *E, 
		      const int *R, bool extract, const GaugeField &u, QudaFieldLocation location) {
    const int nDim = 4;
    //loop variables: a, b, c with a the most signifcant and c the least significant
    //A0, B0, C0 the minimum value
    //A0, B0, C0 the maximum value

    int X[nDim]; // compute interior dimensions
    for (int d=0; d<nDim; d++) X[d] = E[d] - 2*R[d];

    //..........x..........y............z.............t
    int A0[nDim] = {R[3],      R[3],        R[3],         0};
    int A1[nDim] = {X[3]+R[3], X[3]+R[3],   X[3]+R[3],    X[2]+2*R[2]};
    
    int B0[nDim] = {R[2],      R[2],        0,            0};
    int B1[nDim] = {X[2]+R[2], X[2]+R[2],   X[1]+2*R[1],  X[1]+2*R[1]};
    
    int C0[nDim] = {R[1],      0,           0,            0};
    int C1[nDim] = {X[1]+R[1], X[0]+2*R[0], X[0]+2*R[0],  X[0]+2*R[0]};

    int fSrc[nDim][nDim] = {
      {E[2]*E[1]*E[0], E[1]*E[0], E[0],              1},
      {E[2]*E[1]*E[0], E[1]*E[0],    1,           E[0]},
      {E[2]*E[1]*E[0],      E[0],    1,      E[1]*E[0]},
      {E[1]*E[0],           E[0],    1, E[2]*E[1]*E[0]}
    };  
  
    int fBuf[nDim][nDim]={
      {E[2]*E[1], E[1], 1, E[3]*E[2]*E[1]},
      {E[2]*E[0], E[0], 1, E[3]*E[2]*E[0]}, 
      {E[1]*E[0], E[0], 1, E[3]*E[1]*E[0]},
      {E[1]*E[0], E[0], 1, E[2]*E[1]*E[0]}
    };

    //set the local processor parity 
    //switching odd and even ghost gauge when that dimension size is odd
    //only switch if X[dir] is odd and the gridsize in that dimension is greater than 1
    // FIXME - I don't understand this, shouldn't it be commDim(dim) == 0 ?
    int localParity[nDim];
    for (int d=0; d<nDim; d++) 
      localParity[dim] = ((X[dim] % 2 ==1) && (commDim(dim) > 1)) ? 1 : 0;
    //      localParity[dim] = (X[dim]%2==0 || commDim(dim)) ? 0 : 1;

    if (dim==0) {
      ExtractGhostExArg<Order,nDim,0> arg(order, X, R, surfaceCB, A0, A1, B0, B1, 
					  C0, C1, fSrc, fBuf, localParity);
      ExtractGhostEx<Float,length,nDim,0,Order> extractor(arg, extract, u, location);
      extractor.apply(0);
    } else if (dim==1) {
      ExtractGhostExArg<Order,nDim,1> arg(order, X, R, surfaceCB, A0, A1, B0, B1, 
					  C0, C1, fSrc, fBuf, localParity);
      ExtractGhostEx<Float,length,nDim,1,Order> extractor(arg, extract, u, location);
      extractor.apply(0);
    } else if (dim==2) {
      ExtractGhostExArg<Order,nDim,2> arg(order, X, R, surfaceCB, A0, A1, B0, B1, 
					  C0, C1, fSrc, fBuf, localParity);
      ExtractGhostEx<Float,length,nDim,2,Order> extractor(arg, extract, u, location);
      extractor.apply(0);
    } else if (dim==3) {
      ExtractGhostExArg<Order,nDim,3> arg(order, X, R, surfaceCB, A0, A1, B0, B1, 
					  C0, C1, fSrc, fBuf, localParity);
      ExtractGhostEx<Float,length,nDim,3,Order> extractor(arg, extract, u, location);
      extractor.apply(0);
    } else {
      errorQuda("Invalid dim=%d", dim);
    }

    if (location == QUDA_CUDA_FIELD_LOCATION) {
      cudaDeviceSynchronize(); // need to sync before we commence any communication
      checkCudaError();
    }
  }

  /** This is the template driver for extractGhost */
  template <typename Float>
  void extractGhostEx(const GaugeField &u, int dim, const int *R, Float **Ghost, bool extract) {

    const int length = 18;

    QudaFieldLocation location = 
      (typeid(u)==typeid(cudaGaugeField)) ? QUDA_CUDA_FIELD_LOCATION : QUDA_CPU_FIELD_LOCATION;

    if (u.isNative()) {
      if (u.Reconstruct() == QUDA_RECONSTRUCT_NO) {
	if (typeid(Float)==typeid(short) && u.LinkType() == QUDA_ASQTAD_FAT_LINKS) {
	  extractGhostEx<short,length>(FloatNOrder<short,length,2,19>(u, 0, (short**)Ghost), 
				       dim, u.SurfaceCB(), u.X(), R, extract, u, location);
	} else {
	  typedef typename gauge_mapper<Float,QUDA_RECONSTRUCT_NO>::type G;
	  extractGhostEx<Float,length>(G(u, 0, Ghost),
				       dim, u.SurfaceCB(), u.X(), R, extract, u, location);
	}
      } else if (u.Reconstruct() == QUDA_RECONSTRUCT_12) {
	typedef typename gauge_mapper<Float,QUDA_RECONSTRUCT_12>::type G;
	extractGhostEx<Float,length>(G(u, 0, Ghost),
				     dim, u.SurfaceCB(), u.X(), R, extract, u, location);
      } else if (u.Reconstruct() == QUDA_RECONSTRUCT_8) {
	typedef typename gauge_mapper<Float,QUDA_RECONSTRUCT_8>::type G;
	extractGhostEx<Float,length>(G(u, 0, Ghost), 
				     dim, u.SurfaceCB(), u.X(), R, extract, u, location);
      } else if (u.Reconstruct() == QUDA_RECONSTRUCT_13) {
	typedef typename gauge_mapper<Float,QUDA_RECONSTRUCT_13>::type G;
	extractGhostEx<Float,length>(G(u, 0, Ghost),
				     dim, u.SurfaceCB(), u.X(), R, extract, u, location);
      } else if (u.Reconstruct() == QUDA_RECONSTRUCT_9) {
	typedef typename gauge_mapper<Float,QUDA_RECONSTRUCT_13>::type G;
	extractGhostEx<Float,length>(G(u, 0, Ghost),
				     dim, u.SurfaceCB(), u.X(), R, extract, u, location);
      }
    } else if (u.Order() == QUDA_QDP_GAUGE_ORDER) {
      
#ifdef BUILD_QDP_INTERFACE
      extractGhostEx<Float,length>(QDPOrder<Float,length>(u, 0, Ghost),
				   dim, u.SurfaceCB(), u.X(), R, extract, u, location);
#else
      errorQuda("QDP interface has not been built\n");
#endif
      
    } else if (u.Order() == QUDA_QDPJIT_GAUGE_ORDER) {

#ifdef BUILD_QDPJIT_INTERFACE
      extractGhostEx<Float,length>(QDPJITOrder<Float,length>(u, 0, Ghost),
				   dim, u.SurfaceCB(), u.X(), R, extract, u, location);
#else
      errorQuda("QDPJIT interface has not been built\n");
#endif

    } else if (u.Order() == QUDA_CPS_WILSON_GAUGE_ORDER) {

#ifdef BUILD_CPS_INTERFACE
      extractGhostEx<Float,length>(CPSOrder<Float,length>(u, 0, Ghost),
				   dim, u.SurfaceCB(), u.X(), R, extract, u, location);
#else
      errorQuda("CPS interface has not been built\n");
#endif

    } else if (u.Order() == QUDA_MILC_GAUGE_ORDER) {

#ifdef BUILD_MILC_INTERFACE
      extractGhostEx<Float,length>(MILCOrder<Float,length>(u, 0, Ghost),
				   dim, u.SurfaceCB(), u.X(), R, extract, u, location);
#else
      errorQuda("MILC interface has not been built\n");
#endif

    } else if (u.Order() == QUDA_BQCD_GAUGE_ORDER) {

#ifdef BUILD_BQCD_INTERFACE
      extractGhostEx<Float,length>(BQCDOrder<Float,length>(u, 0, Ghost),
				   dim, u.SurfaceCB(), u.X(), R, extract, u, location);
#else
      errorQuda("BQCD interface has not been built\n");
#endif

    } else if (u.Order() == QUDA_TIFR_GAUGE_ORDER) {

#ifdef BUILD_TIFR_INTERFACE
      extractGhostEx<Float,length>(TIFROrder<Float,length>(u, 0, Ghost),
				   dim, u.SurfaceCB(), u.X(), R, extract, u, location);
#else
      errorQuda("TIFR interface has not been built\n");
#endif

    } else {
      errorQuda("Gauge field %d order not supported", u.Order());
    }

  }

  void extractExtendedGaugeGhost(const GaugeField &u, int dim, const int *R, 
				 void **ghost, bool extract) {

    if (u.Precision() == QUDA_DOUBLE_PRECISION) {
      extractGhostEx(u, dim, R, (double**)ghost, extract);
    } else if (u.Precision() == QUDA_SINGLE_PRECISION) {
      extractGhostEx(u, dim, R, (float**)ghost, extract);
    } else if (u.Precision() == QUDA_HALF_PRECISION) {
      extractGhostEx(u, dim, R, (short**)ghost, extract);      
    } else {
      errorQuda("Unknown precision type %d", u.Precision());
    }

  }

} // namespace quda
