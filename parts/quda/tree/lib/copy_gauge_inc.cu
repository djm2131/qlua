#include <gauge_field_order.h>

namespace quda {

  /**
     Kernel argument struct
   */
  template <typename OutOrder, typename InOrder>
  struct CopyGaugeArg {
    OutOrder out;
    const InOrder in;
    int volume;
    int faceVolumeCB[QUDA_MAX_DIM];
    int nDim;
    int geometry;
    CopyGaugeArg(const OutOrder &out, const InOrder &in, int volume, 
		 const int *faceVolumeCB, int nDim, int geometry) 
      : out(out), in(in), volume(volume), nDim(nDim), geometry(geometry) {
      for (int d=0; d<nDim; d++) this->faceVolumeCB[d] = faceVolumeCB[d];
    }
  };

  /**
     Generic CPU gauge reordering and packing 
  */
  template <typename FloatOut, typename FloatIn, int length, typename OutOrder, typename InOrder>
  void copyGauge(CopyGaugeArg<OutOrder,InOrder> arg) {  
    typedef typename mapper<FloatIn>::type RegTypeIn;
    typedef typename mapper<FloatOut>::type RegTypeOut;

    for (int parity=0; parity<2; parity++) {

      for (int d=0; d<arg.geometry; d++) {
	for (int x=0; x<arg.volume/2; x++) {
	  RegTypeIn in[length];
	  RegTypeOut out[length];
	  arg.in.load(in, x, d, parity);
	  for (int i=0; i<length; i++) out[i] = in[i];
	  arg.out.save(out, x, d, parity);
	}
      }

    }
  }

  /**
     Check whether the field contains Nans
  */
  template <typename Float, int length, typename Arg>
  void checkNan(Arg arg) {  
    typedef typename mapper<Float>::type RegType;

    for (int parity=0; parity<2; parity++) {

      for (int d=0; d<arg.geometry; d++) {
	for (int x=0; x<arg.volume/2; x++) {
	  RegType u[length];
	  arg.in.load(u, x, d, parity);
	  for (int i=0; i<length; i++) 
	    if (isnan(u[i])) 
	      errorQuda("Nan detected at parity=%d, dir=%d, x=%d, i=%d", parity, d, x, i);
	}
      }

    }
  }


  /** 
      Generic CUDA gauge reordering and packing.  Adopts a similar form as
      the CPU version, using the same inlined functions.
  */
  template <typename FloatOut, typename FloatIn, int length, typename OutOrder, typename InOrder>
  __global__ void copyGaugeKernel(CopyGaugeArg<OutOrder,InOrder> arg) {  
    typedef typename mapper<FloatIn>::type RegTypeIn;
    typedef typename mapper<FloatOut>::type RegTypeOut;

    for (int parity=0; parity<2; parity++) {

      for (int d=0; d<arg.geometry; d++) {
	int x = blockIdx.x * blockDim.x + threadIdx.x;
	if (x >= arg.volume/2) return;

	RegTypeIn in[length];
	RegTypeOut out[length];
	arg.in.load(in, x, d, parity);
	for (int i=0; i<length; i++) out[i] = in[i];
	arg.out.save(out, x, d, parity);
      }
    }
  }

  /**
     Generic CPU gauge ghost reordering and packing 
  */
  template <typename FloatOut, typename FloatIn, int length, typename OutOrder, typename InOrder>
    void copyGhost(CopyGaugeArg<OutOrder,InOrder> arg) {  
    typedef typename mapper<FloatIn>::type RegTypeIn;
    typedef typename mapper<FloatOut>::type RegTypeOut;

    for (int parity=0; parity<2; parity++) {

      for (int d=0; d<arg.nDim; d++) {
	for (int x=0; x<arg.faceVolumeCB[d]; x++) {
	  RegTypeIn in[length];
	  RegTypeOut out[length];
	  arg.in.loadGhost(in, x, d, parity); // assumes we are loading 
	  for (int i=0; i<length; i++) out[i] = in[i];
	  arg.out.saveGhost(out, x, d, parity);
	}
      }

    }
  }

  /**
     Generic CUDA kernel for copying the ghost zone.  Adopts a similar form as
     the CPU version, using the same inlined functions.
  */
  template <typename FloatOut, typename FloatIn, int length, typename OutOrder, typename InOrder>
  __global__ void copyGhostKernel(CopyGaugeArg<OutOrder,InOrder> arg) {  
    typedef typename mapper<FloatIn>::type RegTypeIn;
    typedef typename mapper<FloatOut>::type RegTypeOut;

    int x = blockIdx.x * blockDim.x + threadIdx.x;

    for (int parity=0; parity<2; parity++) {
      for (int d=0; d<arg.nDim; d++) {
	if (x < arg.faceVolumeCB[d]) {
	  RegTypeIn in[length];
	  RegTypeOut out[length];
	  arg.in.loadGhost(in, x, d, parity); // assumes we are loading 
	  for (int i=0; i<length; i++) out[i] = in[i];
	  arg.out.saveGhost(out, x, d, parity);
	}
      }

    }
  }

  template <typename FloatOut, typename FloatIn, int length, typename OutOrder, typename InOrder, bool isGhost>
  class CopyGauge : Tunable {
    CopyGaugeArg<OutOrder,InOrder> arg;
    int size;
    const GaugeField &meta;

  private:
    unsigned int sharedBytesPerThread() const { return 0; }
    unsigned int sharedBytesPerBlock(const TuneParam &param) const { return 0 ;}

    bool tuneGridDim() const { return false; } // Don't tune the grid dimensions.
    unsigned int minThreads() const { return size; }

  public:
    CopyGauge(CopyGaugeArg<OutOrder,InOrder> &arg, const GaugeField &meta) : arg(arg), meta(meta) { 
      int faceMax = 0;
      for (int d=0; d<arg.nDim; d++) {
	faceMax = (arg.faceVolumeCB[d] > faceMax ) ? arg.faceVolumeCB[d] : faceMax;
      }
      size = isGhost ? faceMax : arg.volume/2;
      writeAuxString("out_stride=%d,in_stride=%d", arg.out.stride, arg.in.stride);
    }

    virtual ~CopyGauge() { ; }
  
    void apply(const cudaStream_t &stream) {
      TuneParam tp = tuneLaunch(*this, getTuning(), getVerbosity());
      if (!isGhost) {
	copyGaugeKernel<FloatOut, FloatIn, length, OutOrder, InOrder> 
	  <<<tp.grid, tp.block, tp.shared_bytes, stream>>>(arg);
      } else {
	copyGhostKernel<FloatOut, FloatIn, length, OutOrder, InOrder> 
	  <<<tp.grid, tp.block, tp.shared_bytes, stream>>>(arg);
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
    long long bytes() const { 
      int sites = 4*arg.volume/2;
      if (isGhost) {
	sites = 0;
	for (int d=0; d<4; d++) sites += arg.faceVolumeCB[d];
      }
      return 2 * sites * (  arg.in.Bytes() + arg.in.hasPhase*sizeof(FloatIn) 
			    + arg.out.Bytes() + arg.out.hasPhase*sizeof(FloatOut) ); 
    } 
  };


  template <typename FloatOut, typename FloatIn, int length, typename OutOrder, typename InOrder>
    void copyGauge(OutOrder outOrder, const InOrder inOrder, int volume, const int *faceVolumeCB, 
		   int nDim, int geometry, const GaugeField &out, QudaFieldLocation location, int type) {

    CopyGaugeArg<OutOrder,InOrder> arg(outOrder, inOrder, volume, faceVolumeCB, nDim, geometry);

    if (location == QUDA_CPU_FIELD_LOCATION) {
#ifdef HOST_DEBUG
      checkNan<FloatIn, length>(arg);
#endif

      if (type == 0 || type == 2) {
	copyGauge<FloatOut, FloatIn, length>(arg);
      }
#ifdef MULTI_GPU // only copy the ghost zone if doing multi-gpu
      if (type == 0 || type == 1) {
	if (geometry == QUDA_VECTOR_GEOMETRY) copyGhost<FloatOut, FloatIn, length>(arg);
	//else warningQuda("Cannot copy for %d geometry gauge field", geometry);
      }
#endif
    } else if (location == QUDA_CUDA_FIELD_LOCATION) {
      // first copy body
      if (type == 0 || type == 2) {
	CopyGauge<FloatOut, FloatIn, length, OutOrder, InOrder, 0> gaugeCopier(arg, out);
	gaugeCopier.apply(0);
      }
#ifdef MULTI_GPU
      if (type == 0 || type == 1) {
	if (geometry == QUDA_VECTOR_GEOMETRY) {
	  // now copy ghost
	  CopyGauge<FloatOut, FloatIn, length, OutOrder, InOrder, 1> ghostCopier(arg, out);
	  ghostCopier.apply(0);
	} else {
	  //warningQuda("Cannot copy for %d geometry gauge field", geometry);
	}
      }
#endif
    } else {
      errorQuda("Undefined field location %d for copyGauge", location);
    }

  }
  
  template <typename FloatOut, typename FloatIn, int length, typename InOrder>
  void copyGauge(const InOrder &inOrder, GaugeField &out, QudaFieldLocation location, 
		 FloatOut *Out, FloatOut **outGhost, int type) {
    int faceVolumeCB[QUDA_MAX_DIM];
    for (int i=0; i<4; i++) faceVolumeCB[i] = out.SurfaceCB(i) * out.Nface(); 
    if (out.isNative()) {
      if (out.Reconstruct() == QUDA_RECONSTRUCT_NO) {
	if (typeid(FloatOut)==typeid(short) && out.LinkType() == QUDA_ASQTAD_FAT_LINKS) {
	  copyGauge<short,FloatIn,length>
	    (FloatNOrder<short,length,2,19>(out, (short*)Out, (short**)outGhost), inOrder,
	     out.Volume(), faceVolumeCB, out.Ndim(), out.Geometry(), out, location, type);
	} else {
	  typedef typename gauge_mapper<FloatOut,QUDA_RECONSTRUCT_NO>::type G;
	  copyGauge<FloatOut,FloatIn,length>
	    (G(out,Out,outGhost), inOrder, out.Volume(), faceVolumeCB,
	     out.Ndim(), out.Geometry(), out, location, type);
	}
      } else if (out.Reconstruct() == QUDA_RECONSTRUCT_12) {
	typedef typename gauge_mapper<FloatOut,QUDA_RECONSTRUCT_12>::type G;
	copyGauge<FloatOut,FloatIn,length>
	  (G(out,Out,outGhost), inOrder, out.Volume(), faceVolumeCB,
	   out.Ndim(), out.Geometry(), out, location, type);
      } else if (out.Reconstruct() == QUDA_RECONSTRUCT_8) {
	typedef typename gauge_mapper<FloatOut,QUDA_RECONSTRUCT_8>::type G;
	copyGauge<FloatOut,FloatIn,length> 
	  (G(out,Out,outGhost), inOrder, out.Volume(), faceVolumeCB,
	   out.Ndim(), out.Geometry(), out, location, type);
#ifdef GPU_STAGGERED_DIRAC
      } else if (out.Reconstruct() == QUDA_RECONSTRUCT_13) {
	typedef typename gauge_mapper<FloatOut,QUDA_RECONSTRUCT_13>::type G;
        copyGauge<FloatOut,FloatIn,length>
	  (G(out, Out, outGhost), inOrder, out.Volume(), faceVolumeCB,
	   out.Ndim(),  out.Geometry(), out, location, type);
      } else if (out.Reconstruct() == QUDA_RECONSTRUCT_9) {
	typedef typename gauge_mapper<FloatOut,QUDA_RECONSTRUCT_9>::type G;
        copyGauge<FloatOut,FloatIn,length>
	  (G(out, Out, outGhost), inOrder, out.Volume(), faceVolumeCB,
	   out.Ndim(), out.Geometry(), out, location, type);
#endif
      } else {
	errorQuda("Reconstruction %d and order %d not supported", out.Reconstruct(), out.Order());
      }
    } else if (out.Order() == QUDA_QDP_GAUGE_ORDER) {

#ifdef BUILD_QDP_INTERFACE
      copyGauge<FloatOut,FloatIn,length>
	(QDPOrder<FloatOut,length>(out, Out, outGhost), inOrder, out.Volume(), 
	 faceVolumeCB, out.Ndim(), out.Geometry(), out, location, type);
#else
      errorQuda("QDP interface has not been built\n");
#endif

    } else if (out.Order() == QUDA_QDPJIT_GAUGE_ORDER) {

#ifdef BUILD_QDPJIT_INTERFACE
      copyGauge<FloatOut,FloatIn,length>
	(QDPJITOrder<FloatOut,length>(out, Out, outGhost), inOrder, out.Volume(),
	 faceVolumeCB, out.Ndim(), out.Geometry(), out, location, type);
#else
      errorQuda("QDPJIT interface has not been built\n");
#endif

    } else if (out.Order() == QUDA_CPS_WILSON_GAUGE_ORDER) {

#ifdef BUILD_CPS_INTERFACE
      copyGauge<FloatOut,FloatIn,length>
	(CPSOrder<FloatOut,length>(out, Out, outGhost), inOrder, out.Volume(),
	 faceVolumeCB, out.Ndim(), out.Geometry(), out, location, type);
#else
      errorQuda("CPS interface has not been built\n");
#endif

    } else if (out.Order() == QUDA_MILC_GAUGE_ORDER) {

#ifdef BUILD_MILC_INTERFACE
      copyGauge<FloatOut,FloatIn,length>
	(MILCOrder<FloatOut,length>(out, Out, outGhost), inOrder, out.Volume(),
	 faceVolumeCB, out.Ndim(), out.Geometry(), out, location, type);
#else
      errorQuda("MILC interface has not been built\n");
#endif

    } else if (out.Order() == QUDA_BQCD_GAUGE_ORDER) {

#ifdef BUILD_BQCD_INTERFACE
      copyGauge<FloatOut,FloatIn,length>
	(BQCDOrder<FloatOut,length>(out, Out, outGhost), inOrder, out.Volume(),
	 faceVolumeCB, out.Ndim(), out.Geometry(), out, location, type);
#else
      errorQuda("BQCD interface has not been built\n");
#endif

    } else if (out.Order() == QUDA_TIFR_GAUGE_ORDER) {

#ifdef BUILD_TIFR_INTERFACE
      copyGauge<FloatOut,FloatIn,length>
	(TIFROrder<FloatOut,length>(out, Out, outGhost), inOrder, out.Volume(),
	 faceVolumeCB, out.Ndim(), out.Geometry(), out, location, type);
#else
      errorQuda("TIFR interface has not been built\n");
#endif

    } else {
      errorQuda("Gauge field %d order not supported", out.Order());
    }

  }

  template <typename FloatOut, typename FloatIn, int length>
    void copyGauge(GaugeField &out, const GaugeField &in, QudaFieldLocation location, 
		   FloatOut *Out, FloatIn *In, FloatOut **outGhost, FloatIn **inGhost, int type) {

    // reconstruction only supported on FloatN fields currently
    if (in.isNative()) {      
      if (in.Reconstruct() == QUDA_RECONSTRUCT_NO) {
	if (typeid(FloatIn)==typeid(short) && in.LinkType() == QUDA_ASQTAD_FAT_LINKS) {
	  copyGauge<FloatOut,short,length> (FloatNOrder<short,length,2,19>
					    (in,(short*)In,(short**)inGhost),
					    out, location, Out, outGhost, type);
	} else {
	  typedef typename gauge_mapper<FloatIn,QUDA_RECONSTRUCT_NO>::type G;
	  copyGauge<FloatOut,FloatIn,length> (G(in,In,inGhost), out, location, Out, outGhost, type);
	}
      } else if (in.Reconstruct() == QUDA_RECONSTRUCT_12) {
	typedef typename gauge_mapper<FloatIn,QUDA_RECONSTRUCT_12>::type G;
	copyGauge<FloatOut,FloatIn,length> (G(in,In,inGhost), out, location, Out, outGhost, type);
      } else if (in.Reconstruct() == QUDA_RECONSTRUCT_8) {
	typedef typename gauge_mapper<FloatIn,QUDA_RECONSTRUCT_8>::type G;
	copyGauge<FloatOut,FloatIn,length> (G(in,In,inGhost), out, location, Out, outGhost, type);
#ifdef GPU_STAGGERED_DIRAC
      } else if (in.Reconstruct() == QUDA_RECONSTRUCT_13) {
	typedef typename gauge_mapper<FloatIn,QUDA_RECONSTRUCT_13>::type G;
	copyGauge<FloatOut,FloatIn,length> (G(in,In,inGhost), out, location, Out, outGhost, type);
      } else if (in.Reconstruct() == QUDA_RECONSTRUCT_9) {
	typedef typename gauge_mapper<FloatIn,QUDA_RECONSTRUCT_9>::type G;
	copyGauge<FloatOut,FloatIn,length> (G(in,In,inGhost), out, location, Out, outGhost, type);
#endif
      } else {
	errorQuda("Reconstruction %d and order %d not supported", in.Reconstruct(), in.Order());
      }
    } else if (in.Order() == QUDA_QDP_GAUGE_ORDER) {

#ifdef BUILD_QDP_INTERFACE
      copyGauge<FloatOut,FloatIn,length>(QDPOrder<FloatIn,length>(in, In, inGhost), 
					 out, location, Out, outGhost, type);
#else
      errorQuda("QDP interface has not been built\n");
#endif

    } else if (in.Order() == QUDA_QDPJIT_GAUGE_ORDER) {

#ifdef BUILD_QDPJIT_INTERFACE
      copyGauge<FloatOut,FloatIn,length>(QDPJITOrder<FloatIn,length>(in, In, inGhost), 
					 out, location, Out, outGhost, type);
#else
      errorQuda("QDPJIT interface has not been built\n");
#endif

    } else if (in.Order() == QUDA_CPS_WILSON_GAUGE_ORDER) {

#ifdef BUILD_CPS_INTERFACE
      copyGauge<FloatOut,FloatIn,length>(CPSOrder<FloatIn,length>(in, In, inGhost), 
					 out, location, Out, outGhost, type);
#else
      errorQuda("CPS interface has not been built\n");
#endif

    } else if (in.Order() == QUDA_MILC_GAUGE_ORDER) {

#ifdef BUILD_MILC_INTERFACE
      copyGauge<FloatOut,FloatIn,length>(MILCOrder<FloatIn,length>(in, In, inGhost), 
					 out, location, Out, outGhost, type);
#else
      errorQuda("MILC interface has not been built\n");
#endif

    } else if (in.Order() == QUDA_BQCD_GAUGE_ORDER) {

#ifdef BUILD_BQCD_INTERFACE
      copyGauge<FloatOut,FloatIn,length>(BQCDOrder<FloatIn,length>(in, In, inGhost), 
					 out, location, Out, outGhost, type);
#else
      errorQuda("BQCD interface has not been built\n");
#endif

    } else if (in.Order() == QUDA_TIFR_GAUGE_ORDER) {

#ifdef BUILD_TIFR_INTERFACE
      copyGauge<FloatOut,FloatIn,length>(TIFROrder<FloatIn,length>(in, In, inGhost), 
					 out, location, Out, outGhost, type);
#else
      errorQuda("TIFR interface has not been built\n");
#endif

    } else {
      errorQuda("Gauge field %d order not supported", in.Order());
    }

  }

  void checkMomOrder(const GaugeField &u);

  template <typename FloatOut, typename FloatIn>
  void copyGauge(GaugeField &out, const GaugeField &in, QudaFieldLocation location, FloatOut *Out, 
		 FloatIn *In, FloatOut **outGhost, FloatIn **inGhost, int type) {

    if (in.Ncolor() != 3 && out.Ncolor() != 3) {
      errorQuda("Unsupported number of colors; out.Nc=%d, in.Nc=%d", out.Ncolor(), in.Ncolor());
    }
    
    if (out.Geometry() != in.Geometry()) {
      errorQuda("Field geometries %d %d do not match", out.Geometry(), in.Geometry());
    }

    if (in.LinkType() != QUDA_ASQTAD_MOM_LINKS && out.LinkType() != QUDA_ASQTAD_MOM_LINKS) {
      // we are doing gauge field packing
      copyGauge<FloatOut,FloatIn,18>(out, in, location, Out, In, outGhost, inGhost, type);
    } else {
      if (location != QUDA_CPU_FIELD_LOCATION) errorQuda("Location %d not supported", location);
      if (out.Geometry() != QUDA_VECTOR_GEOMETRY) errorQuda("Unsupported geometry %d", out.Geometry());

      checkMomOrder(in);
      checkMomOrder(out);
    
      int faceVolumeCB[QUDA_MAX_DIM];
      for (int d=0; d<in.Ndim(); d++) faceVolumeCB[d] = in.SurfaceCB(d) * in.Nface();

      // momentum only currently supported on MILC (10), TIFR (18) and Float2 (10) fields currently
	if (out.Order() == QUDA_FLOAT2_GAUGE_ORDER) {
	  if (in.Order() == QUDA_FLOAT2_GAUGE_ORDER) {
	    CopyGaugeArg<FloatNOrder<FloatOut,10,2,10>, FloatNOrder<FloatIn,10,2,10> >
	      arg(FloatNOrder<FloatOut,10,2,10>(out, Out), 
		  FloatNOrder<FloatIn,10,2,10>(in, In), in.Volume(), faceVolumeCB, in.Ndim(), in.Geometry());
	    copyGauge<FloatOut,FloatIn,10>(arg);
	  } else if (in.Order() == QUDA_MILC_GAUGE_ORDER) {
#ifdef BUILD_MILC_INTERFACE
	    CopyGaugeArg<FloatNOrder<FloatOut,10,2,10>, MILCOrder<FloatIn,10> >
	      arg(FloatNOrder<FloatOut,10,2,10>(out, Out), MILCOrder<FloatIn,10>(in, In), 
		  in.Volume(), faceVolumeCB, in.Ndim(), in.Geometry());
	    copyGauge<FloatOut,FloatIn,10>(arg);
#else
	    errorQuda("MILC interface has not been built\n");
#endif
	    
	  } else if (in.Order() == QUDA_TIFR_GAUGE_ORDER) {
#ifdef BUILD_TIFR_INTERFACE
	    CopyGaugeArg<FloatNOrder<FloatOut,18,2,11>, TIFROrder<FloatIn,18> >
	      arg(FloatNOrder<FloatOut,18,2,11>(out, Out), TIFROrder<FloatIn,18>(in, In), 
		  in.Volume(), faceVolumeCB, in.Ndim(), in.Geometry());
	    copyGauge<FloatOut,FloatIn,18>(arg);
#else
	    errorQuda("TIFR interface has not been built\n");
#endif
	    
	  } else {
	    errorQuda("Gauge field orders %d not supported", in.Order());
	  }
	} else if (out.Order() == QUDA_MILC_GAUGE_ORDER) {
#ifdef BUILD_MILC_INTERFACE
	  if (in.Order() == QUDA_FLOAT2_GAUGE_ORDER) {
	    CopyGaugeArg<MILCOrder<FloatOut,10>, FloatNOrder<FloatIn,10,2,10> >
	      arg(MILCOrder<FloatOut,10>(out, Out), FloatNOrder<FloatIn,10,2,10>(in, In),
		  in.Volume(), faceVolumeCB, in.Ndim(), in.Geometry());
	    copyGauge<FloatOut,FloatIn,10>(arg);
	  } else if (in.Order() == QUDA_MILC_GAUGE_ORDER) {
	    CopyGaugeArg<MILCOrder<FloatOut,10>, MILCOrder<FloatIn,10> >
	      arg(MILCOrder<FloatOut,10>(out, Out), MILCOrder<FloatIn,10>(in, In),
		  in.Volume(), faceVolumeCB, in.Ndim(), in.Geometry());
	    copyGauge<FloatOut,FloatIn,10>(arg);
	  } else {
	    errorQuda("Gauge field orders %d not supported", in.Order());
	  }
#else
	  errorQuda("MILC interface has not been built\n");
#endif
	  
	} else if (out.Order() == QUDA_TIFR_GAUGE_ORDER) {
#ifdef BUILD_TIFR_INTERFACE
	  if (in.Order() == QUDA_FLOAT2_GAUGE_ORDER) {
	    // FIX ME - 11 is a misnomer to avoid confusion in template instantiation
	    CopyGaugeArg<TIFROrder<FloatOut,18>, FloatNOrder<FloatIn,18,2,11> >
	      arg(TIFROrder<FloatOut,18>(out, Out), FloatNOrder<FloatIn,18,2,11>(in, In),
		  in.Volume(), faceVolumeCB, in.Ndim(), in.Geometry());
	    copyGauge<FloatOut,FloatIn,18>(arg);
	  } else if (in.Order() == QUDA_TIFR_GAUGE_ORDER) {
	    CopyGaugeArg<TIFROrder<FloatOut,18>, TIFROrder<FloatIn,18> >
	      arg(TIFROrder<FloatOut,18>(out, Out), TIFROrder<FloatIn,18>(in, In),
		  in.Volume(), faceVolumeCB, in.Ndim(), in.Geometry());
	    copyGauge<FloatOut,FloatIn,10>(arg);
	  } else {
	    errorQuda("Gauge field orders %d not supported", in.Order());
	  }
#else
	  errorQuda("TIFR interface has not been built\n");
#endif
	} else {
	  errorQuda("Gauge field orders %d not supported", out.Order());
	}
    }
  }


} // namespace quda
