#include <blas_quda.h>
#include <tune_quda.h>
#include <float_vector.h>

#define QudaSumFloat double
#define QudaSumFloat2 double2
#define QudaSumFloat3 double3

#define REDUCE_MAX_BLOCKS 65536

#define checkSpinor(a, b)						\
  {									\
    if (a.Precision() != b.Precision())					\
      errorQuda("precisions do not match: %d %d", a.Precision(), b.Precision()); \
    if (a.Length() != b.Length())					\
      errorQuda("lengths do not match: %d %d", a.Length(), b.Length());	\
    if (a.Stride() != b.Stride())					\
      errorQuda("strides do not match: %d %d", a.Stride(), b.Stride());	\
  }

#define checkLength(a, b)						\
  {									\
    if (a.Length() != b.Length())					\
      errorQuda("lengths do not match: %d %d", a.Length(), b.Length());	\
    if (a.Stride() != b.Stride())					\
      errorQuda("strides do not match: %d %d", a.Stride(), b.Stride());	\
  }

static struct {
  const char *vol_str;
  const char *aux_str;
  char aux_tmp[quda::TuneKey::aux_n];
} blasStrings;

// These are used for reduction kernels
static QudaSumFloat *d_reduce=0;
static QudaSumFloat *h_reduce=0;
static QudaSumFloat *hd_reduce=0;
static cudaEvent_t reduceEnd;
    
namespace quda {

  cudaStream_t* getBlasStream();
    
  void* getDeviceReduceBuffer() { return d_reduce; }
  void* getMappedHostReduceBuffer() { return hd_reduce; }
  void* getHostReduceBuffer() { return h_reduce; }

  void initReduce()
  { 

    const int MaxReduce = 12;
    // reduction buffer size
    size_t bytes = MaxReduce*3*REDUCE_MAX_BLOCKS*sizeof(QudaSumFloat); // Factor of N for composite reductions


    if (!d_reduce) d_reduce = (QudaSumFloat *) device_malloc(bytes);
    
    // these arrays are actually oversized currently (only needs to be QudaSumFloat3)
    
    // if the device supports host-mapped memory then use a host-mapped array for the reduction
    if (!h_reduce) {
      // only use zero copy reductions when using 64-bit
#if (defined(_MSC_VER) && defined(_WIN64)) || defined(__LP64__)
      if(deviceProp.canMapHostMemory) {
	h_reduce = (QudaSumFloat *) mapped_malloc(bytes);	
	cudaHostGetDevicePointer(&hd_reduce, h_reduce, 0); // set the matching device pointer
      } else 
#endif 
      {
	h_reduce = (QudaSumFloat *) pinned_malloc(bytes);
	hd_reduce = d_reduce;
      }
      memset(h_reduce, 0, bytes); // added to ensure that valgrind doesn't report h_reduce is unitialised
    }
    
    cudaEventCreateWithFlags(&reduceEnd, cudaEventDisableTiming);
    
    checkCudaError();
  }

  void endReduce(void)
  {
    if (d_reduce) {
      device_free(d_reduce);
      d_reduce = 0;
    }
    if (h_reduce) {
      host_free(h_reduce);
      h_reduce = 0;
    }
    hd_reduce = 0;
    
    cudaEventDestroy(reduceEnd);
  }

  namespace reduce {

#include <texture.h>
#include <reduce_core.h>
#include <reduce_mixed_core.h>
    
  } // namespace reduce

  /**
     Base class from which all reduction functors should derive.
  */
  template <typename ReduceType, typename Float2, typename FloatN>
  struct ReduceFunctor {
    
    //! pre-computation routine called before the "M-loop"
    virtual __device__ void pre() { ; }
    
    //! where the reduction is usually computed and any auxiliary operations
    virtual __device__ void operator()(ReduceType &sum, FloatN &x, FloatN &y, 
				       FloatN &z, FloatN &w, FloatN &v) = 0;
    
    //! post-computation routine called after the "M-loop"
    virtual __device__ void post(ReduceType &sum) { ; }
    
  };

  /**
     Return the L1 norm of x
  */
  __device__ double norm1_(const double2 &a) { return fabs(a.x) + fabs(a.y); }
  __device__ double norm1_(const float2 &a) { return (double)fabs(a.x) + (double)fabs(a.y); }
  __device__ double norm1_(const float4 &a) { return (double)fabs(a.x) + (double)fabs(a.y) + (double)fabs(a.z) + (double)fabs(a.w); }

  template <typename ReduceType, typename Float2, typename FloatN>
  struct Norm1 : public ReduceFunctor<ReduceType, Float2, FloatN> {
    Norm1(const Float2 &a, const Float2 &b) { ; }
    __device__ void operator()(ReduceType &sum, FloatN &x, FloatN &y, FloatN &z,FloatN  &w, FloatN &v) { sum += norm1_(x); }
    static int streams() { return 1; } //! total number of input and output streams
    static int flops() { return 2; } //! flops per element
  };

  double norm1Cuda(const cudaColorSpinorField &x) {
    cudaColorSpinorField &y = (cudaColorSpinorField&)x; // FIXME
#ifdef HOST_DEBUG
    return reduce::reduceCuda<double,QudaSumFloat,QudaSumFloat,Norm1,0,0,0,0,0,false>
      (make_double2(0.0, 0.0), make_double2(0.0, 0.0), y, y, y, y, y);
#else
    errorQuda("L1 norm kernel only built when HOST_DEBUG is enabled");
    return 0.0;
#endif
  }

  /**
     Return the L2 norm of x
  */
  __device__ double norm2_(const double2 &a) { return a.x*a.x + a.y*a.y; }
  __device__ double norm2_(const float2 &a) { return (double)a.x*(double)a.x + (double)a.y*(double)a.y; }
  __device__ double norm2_(const float4 &a) { return (double)a.x*(double)a.x + (double)a.y*(double)a.y +
      (double)a.z*(double)a.z + (double)a.w*(double)a.w; }

  template <typename ReduceType, typename Float2, typename FloatN>
  struct Norm2 : public ReduceFunctor<ReduceType, Float2, FloatN> {
    Norm2(const Float2 &a, const Float2 &b) { ; }
    __device__ void operator()(ReduceType &sum, FloatN &x, FloatN &y, FloatN &z,FloatN  &w, FloatN &v) { sum += norm2_(x); }
    static int streams() { return 1; } //! total number of input and output streams
    static int flops() { return 2; } //! flops per element
  };

  double normCuda(const cudaColorSpinorField &x) {
    cudaColorSpinorField &y = (cudaColorSpinorField&)x; // FIXME
    return reduce::reduceCuda<double,QudaSumFloat,QudaSumFloat,Norm2,0,0,0,0,0,false>
      (make_double2(0.0, 0.0), make_double2(0.0, 0.0), y, y, y, y, y);
  }

  /**
     Return the real dot product of x and y
  */
    __device__ double dot_(const double2 &a, const double2 &b) { return a.x*b.x + a.y*b.y; }
    __device__ double dot_(const float2 &a, const float2 &b) { return (double)a.x*(double)b.x + (double)a.y*(double)b.y; }
    __device__ double dot_(const float4 &a, const float4 &b) { return (double)a.x*(double)b.x + (double)a.y*(double)b.y + (double)a.z*(double)b.z + (double)a.w*(double)b.w; }

  template <typename ReduceType, typename Float2, typename FloatN>
  struct Dot : public ReduceFunctor<ReduceType, Float2, FloatN> {
    Dot(const Float2 &a, const Float2 &b) { ; }
    __device__ void operator()(ReduceType &sum, FloatN &x, FloatN &y, FloatN &z, FloatN &w, FloatN &v) { sum += dot_(x,y); }
    static int streams() { return 2; } //! total number of input and output streams
    static int flops() { return 2; } //! flops per element
  };

  double reDotProductCuda(cudaColorSpinorField &x, cudaColorSpinorField &y) {
    return reduce::reduceCuda<double,QudaSumFloat,QudaSumFloat,Dot,0,0,0,0,0,false>
      (make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
  }

 
  void reDotProductCuda(double* result, std::vector<cudaColorSpinorField*>& x, std::vector<cudaColorSpinorField*>& y){
#ifndef SSTEP
    errorQuda("S-step code not built\n");
#else 
    switch(x.size()){
      case 1:
        reduce::multiReduceCuda<1,double,QudaSumFloat,QudaSumFloat,Dot,0,0,0,0,0,false>
        (result, make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
        break;
      case 2:
        reduce::multiReduceCuda<2,double,QudaSumFloat,QudaSumFloat,Dot,0,0,0,0,0,false>
        (result, make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
        break; 
      case 3:
        reduce::multiReduceCuda<3,double,QudaSumFloat,QudaSumFloat,Dot,0,0,0,0,0,false>
        (result, make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
        break;
      case 4:
        reduce::multiReduceCuda<4,double,QudaSumFloat,QudaSumFloat,Dot,0,0,0,0,0,false>
        (result, make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
        break;
      case 5:
        reduce::multiReduceCuda<5,double,QudaSumFloat,QudaSumFloat,Dot,0,0,0,0,0,false>
        (result, make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
        break;
      case 6:
        reduce::multiReduceCuda<6,double,QudaSumFloat,QudaSumFloat,Dot,0,0,0,0,0,false>
        (result, make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
        break;
      case 7:
        reduce::multiReduceCuda<7,double,QudaSumFloat,QudaSumFloat,Dot,0,0,0,0,0,false>
        (result, make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
        break;
      case 8:
        reduce::multiReduceCuda<8,double,QudaSumFloat,QudaSumFloat,Dot,0,0,0,0,0,false>
        (result, make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
        break;
      case 9:
        reduce::multiReduceCuda<9,double,QudaSumFloat,QudaSumFloat,Dot,0,0,0,0,0,false>
        (result, make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
        break;
      case 10:
        reduce::multiReduceCuda<10,double,QudaSumFloat,QudaSumFloat,Dot,0,0,0,0,0,false>
        (result, make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
        break;
      case 11:
        reduce::multiReduceCuda<11,double,QudaSumFloat,QudaSumFloat,Dot,0,0,0,0,0,false>
        (result, make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
        break;
      case 12:
        reduce::multiReduceCuda<12,double,QudaSumFloat,QudaSumFloat,Dot,0,0,0,0,0,false>
        (result, make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
        break;
      case 13:
        reduce::multiReduceCuda<13,double,QudaSumFloat,QudaSumFloat,Dot,0,0,0,0,0,false>
        (result, make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
        break;
      case 14:
        reduce::multiReduceCuda<14,double,QudaSumFloat,QudaSumFloat,Dot,0,0,0,0,0,false>
        (result, make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
        break;
      case 15:
        reduce::multiReduceCuda<15,double,QudaSumFloat,QudaSumFloat,Dot,0,0,0,0,0,false>
        (result, make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
        break;
      case 16:
        reduce::multiReduceCuda<16,double,QudaSumFloat,QudaSumFloat,Dot,0,0,0,0,0,false>
        (result, make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
        break;
      default:
        errorQuda("Unsupported vector size");
        break;
    }
#endif // SSTEP
  } 


    /* 
      returns the real component of the dot product of a and b 
      and the norm of a
    */
  __device__ double2 dotNormA_(const double2 &a, const double2 &b)
  { return make_double2(a.x*b.x + a.y*b.y, a.x*a.x + a.y*a.y); }
 
  __device__ double2 dotNormA_(const float2 &a, const float2 &b)
    { return make_double2((double)a.x*(double)b.x + (double)a.y*(double)b.y, (double)a.x*(double)a.x + (double)a.y*(double)a.y); }

 
  __device__ double2 dotNormA_(const float4 &a, const float4 & b)
    { return make_double2((double)a.x*(double)b.x + (double)a.y*(double)b.y + (double)a.z*(double)b.z + (double)a.w*(double)b.w,
			  (double)a.x*(double)a.x + (double)a.y*(double)a.y + (double)a.z*(double)a.z + (double)a.w*(double)a.w); }



  template <typename ReduceType, typename Float2, typename FloatN>
  struct DotNormA : public ReduceFunctor<ReduceType, Float2, FloatN> {
    DotNormA(const Float2 &a, const Float2 &b){}
    __device__ void operator()(ReduceType &sum, FloatN &x, FloatN &y, FloatN &z,  FloatN &w, FloatN &v){sum += dotNormA_(x,y);}
    static int streams() { return 2; }
    static int flops() { return 4; }
  };

  double2 reDotProductNormACuda(cudaColorSpinorField &x,cudaColorSpinorField &y){
    return reduce::reduceCuda<double2,QudaSumFloat2,QudaSumFloat,DotNormA,0,0,0,0,0,false>
      (make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
  }


  /**
     First performs the operation y[i] = a*x[i]
     Return the norm of y
  */
  template <typename ReduceType, typename Float2, typename FloatN>
  struct axpyNorm2 : public ReduceFunctor<ReduceType, Float2, FloatN> {
    Float2 a;
    axpyNorm2(const Float2 &a, const Float2 &b) : a(a) { ; }
    __device__ void operator()(ReduceType &sum, FloatN &x, FloatN &y, FloatN &z, FloatN &w, FloatN &v) { 
      y += a.x*x; sum += norm2_(y); }
    static int streams() { return 3; } //! total number of input and output streams
    static int flops() { return 4; } //! flops per element
  };

  double axpyNormCuda(const double &a, cudaColorSpinorField &x, cudaColorSpinorField &y) {
    return reduce::reduceCuda<double,QudaSumFloat,QudaSumFloat,axpyNorm2,0,1,0,0,0,false>
      (make_double2(a, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
  }

  /**
     First performs the operation y[i] = x[i] - y[i]
     Second returns the norm of y
  */
  template <typename ReduceType, typename Float2, typename FloatN>
  struct xmyNorm2 : public ReduceFunctor<ReduceType, Float2, FloatN> {
    xmyNorm2(const Float2 &a, const Float2 &b) { ; }
    __device__ void operator()(ReduceType &sum, FloatN &x, FloatN &y, FloatN &z, FloatN &w, FloatN &v) { 
      y = x - y; sum += norm2_(y); }
    static int streams() { return 3; } //! total number of input and output streams
    static int flops() { return 3; } //! flops per element
  };

  double xmyNormCuda(cudaColorSpinorField &x, cudaColorSpinorField &y) {
    return reduce::reduceCuda<double,QudaSumFloat,QudaSumFloat,xmyNorm2,0,1,0,0,0,false>
      (make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
  }


  /**
     Functor to perform the operation y += a * x  (complex-valued)
  */

  __device__ void Caxpy_(const float2 &a, const float4 &x, float4 &y) {
    y.x += a.x*x.x; y.x -= a.y*x.y;
    y.y += a.y*x.x; y.y += a.x*x.y;
    y.z += a.x*x.z; y.z -= a.y*x.w;
    y.w += a.y*x.z; y.w += a.x*x.w;
  }

  __device__ void Caxpy_(const float2 &a, const float2 &x, float2 &y) {
    y.x += a.x*x.x; y.x -= a.y*x.y;
    y.y += a.y*x.x; y.y += a.x*x.y;
  }

  __device__ void Caxpy_(const double2 &a, const double2 &x, double2 &y) {
    y.x += a.x*x.x; y.x -= a.y*x.y;
    y.y += a.y*x.x; y.y += a.x*x.y;
  }

  /**
     First performs the operation y[i] = a*x[i] + y[i] (complex-valued)
     Second returns the norm of y
  */
  template <typename ReduceType, typename Float2, typename FloatN>
  struct caxpyNorm2 : public ReduceFunctor<ReduceType, Float2, FloatN> {
    Float2 a;
    caxpyNorm2(const Float2 &a, const Float2 &b) : a(a) { ; }
    __device__ void operator()(ReduceType &sum, FloatN &x, FloatN &y, FloatN &z, FloatN &w, FloatN &v) { 
      Caxpy_(a, x, y); sum += norm2_(y); }
    static int streams() { return 3; } //! total number of input and output streams
    static int flops() { return 6; } //! flops per element
  };

  double caxpyNormCuda(const Complex &a, cudaColorSpinorField &x, cudaColorSpinorField &y) {
    return reduce::reduceCuda<double,QudaSumFloat,QudaSumFloat,caxpyNorm2,0,1,0,0,0,false>
      (make_double2(REAL(a), IMAG(a)), make_double2(0.0, 0.0), x, y, x, x, x);
  }

  /**
     double caxpyXmayNormCuda(float a, float *x, float *y, n){}
   
     First performs the operation y[i] += a*x[i]
     Second performs the operator x[i] -= a*z[i]
     Third returns the norm of x
  */
  template <typename ReduceType, typename Float2, typename FloatN>
  struct caxpyxmaznormx : public ReduceFunctor<ReduceType, Float2, FloatN> {
    Float2 a;
    caxpyxmaznormx(const Float2 &a, const Float2 &b) : a(a) { ; }
    __device__ void operator()(ReduceType &sum, FloatN &x, FloatN &y, FloatN &z, FloatN &w, FloatN &v) { Caxpy_(a, x, y); Caxpy_(-a, z, x); sum += norm2_(x); }
    static int streams() { return 5; } //! total number of input and output streams
    static int flops() { return 10; } //! flops per element
  };

  double caxpyXmazNormXCuda(const Complex &a, cudaColorSpinorField &x, 
			    cudaColorSpinorField &y, cudaColorSpinorField &z) {
    return reduce::reduceCuda<double,QudaSumFloat,QudaSumFloat,caxpyxmaznormx,1,1,0,0,0,false>
      (make_double2(REAL(a), IMAG(a)), make_double2(0.0, 0.0), x, y, z, x, x);
  }

  /**
     double cabxpyAxNormCuda(float a, complex b, float *x, float *y, n){}
   
     First performs the operation y[i] += a*b*x[i]
     Second performs x[i] *= a
     Third returns the norm of x
  */
    template <typename ReduceType, typename Float2, typename FloatN>
    struct cabxpyaxnorm : public ReduceFunctor<ReduceType, Float2, FloatN> {
    Float2 a;
    Float2 b;
    cabxpyaxnorm(const Float2 &a, const Float2 &b) : a(a), b(b) { ; }
      __device__ void operator()(ReduceType &sum, FloatN &x, FloatN &y, FloatN &z, FloatN &w, FloatN &v) { x *= a.x; Caxpy_(b, x, y); sum += norm2_(y); }
    static int streams() { return 4; } //! total number of input and output streams
    static int flops() { return 10; } //! flops per element
  };

  double cabxpyAxNormCuda(const double &a, const Complex &b, 
			  cudaColorSpinorField &x, cudaColorSpinorField &y) {
    return reduce::reduceCuda<double,QudaSumFloat,QudaSumFloat,cabxpyaxnorm,1,1,0,0,0,false>
      (make_double2(a, 0.0), make_double2(REAL(b), IMAG(b)), x, y, x, x, x);
  }

  /**
     Returns complex-valued dot product of x and y
  */
  __device__ double2 cdot_(const double2 &a, const double2 &b) 
  { return make_double2(a.x*b.x + a.y*b.y, a.x*b.y - a.y*b.x); }
  __device__ double2 cdot_(const float2 &a, const float2 &b) 
      { return make_double2((double)a.x*(double)b.x + (double)a.y*(double)b.y,
			    (double)a.x*(double)b.y - (double)a.y*(double)b.x); }
  __device__ double2 cdot_(const float4 &a, const float4 &b) 
      { return make_double2((double)a.x*(double)b.x + (double)a.y*(double)b.y +
			    (double)a.z*(double)b.z + (double)a.w*(double)b.w,
			    (double)a.x*(double)b.y - (double)a.y*(double)b.x +
			    (double)a.z*(double)b.w - (double)a.w*(double)b.z); }

  template <typename ReduceType, typename Float2, typename FloatN>
  struct Cdot : public ReduceFunctor<ReduceType, Float2, FloatN> {
    Cdot(const Float2 &a, const Float2 &b) { ; }
    __device__ void operator()(ReduceType &sum, FloatN &x, FloatN &y, FloatN &z, FloatN &w, FloatN &v) { sum += cdot_(x,y); }
    static int streams() { return 2; } //! total number of input and output streams
    static int flops() { return 4; } //! flops per element
  };

  Complex cDotProductCuda(cudaColorSpinorField &x, cudaColorSpinorField &y) {
    double2 cdot = reduce::reduceCuda<double2,QudaSumFloat2,QudaSumFloat,Cdot,0,0,0,0,0,false>
      (make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
    return Complex(cdot.x, cdot.y);
  }

  void cDotProductCuda(Complex* result, std::vector<cudaColorSpinorField*>& x, std::vector<cudaColorSpinorField*>& y){
#ifndef SSTEP
    errorQuda("S-step code not built\n");
#else
    double2* cdot = new double2[x.size()];

    switch(x.size()){
      case 1:
        reduce::multiReduceCuda<1,double2,QudaSumFloat2,QudaSumFloat,Cdot,0,0,0,0,0,false>
        (cdot, make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
        break;
      case 6:
        reduce::multiReduceCuda<6,double2,QudaSumFloat2,QudaSumFloat,Cdot,0,0,0,0,0,false>
        (cdot, make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
        break;
      case 10:
        reduce::multiReduceCuda<10,double2,QudaSumFloat2,QudaSumFloat,Cdot,0,0,0,0,0,false>
        (cdot, make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
        break;
      case 14:
        reduce::multiReduceCuda<14,double2,QudaSumFloat2,QudaSumFloat,Cdot,0,0,0,0,0,false>
        (cdot, make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
        break;
      default:
        errorQuda("Unsupported vector size\n");
        break;
    }

    for(int i=0; i<x.size(); ++i) result[i] = Complex(cdot[i].x,cdot[i].y);
    delete[] cdot;
#endif
  }

  /**
     double2 xpaycDotzyCuda(float2 *x, float a, float2 *y, float2 *z, int n) {}
   
     First performs the operation y = x + a*y
     Second returns cdot product (z,y)
  */
  template <typename ReduceType, typename Float2, typename FloatN>
  struct xpaycdotzy : public ReduceFunctor<ReduceType, Float2, FloatN> {
    Float2 a;
    xpaycdotzy(const Float2 &a, const Float2 &b) : a(a) { ; }
    __device__ void operator()(ReduceType &sum, FloatN &x, FloatN &y, FloatN &z, FloatN &w, FloatN &v) { y = x + a.x*y; sum += cdot_(z,y); }
    static int streams() { return 4; } //! total number of input and output streams
    static int flops() { return 6; } //! flops per element
  };

  Complex xpaycDotzyCuda(cudaColorSpinorField &x, const double &a, cudaColorSpinorField &y, cudaColorSpinorField &z) {
    double2 cdot = reduce::reduceCuda<double2,QudaSumFloat2,QudaSumFloat,xpaycdotzy,0,1,0,0,0,false>
      (make_double2(a, 0.0), make_double2(0.0, 0.0), x, y, z, x, x);
    return Complex(cdot.x, cdot.y);
  }

  /**
     double caxpyDotzyCuda(float a, float *x, float *y, float *z, n){}
   
     First performs the operation y[i] = a*x[i] + y[i]
     Second returns the dot product (z,y)
  */
  template <typename ReduceType, typename Float2, typename FloatN>
  struct caxpydotzy : public ReduceFunctor<ReduceType, Float2, FloatN> {
    Float2 a;
    caxpydotzy(const Float2 &a, const Float2 &b) : a(a) { ; }
    __device__ void operator()(ReduceType &sum, FloatN &x, FloatN &y, FloatN &z, FloatN &w, FloatN &v) { Caxpy_(a, x, y); sum += cdot_(z,y); }
    static int streams() { return 4; } //! total number of input and output streams
    static int flops() { return 8; } //! flops per element
  };

  Complex caxpyDotzyCuda(const Complex &a, cudaColorSpinorField &x, cudaColorSpinorField &y,
			 cudaColorSpinorField &z) {
    double2 cdot = reduce::reduceCuda<double2,QudaSumFloat2,QudaSumFloat,caxpydotzy,0,1,0,0,0,false>
      (make_double2(REAL(a), IMAG(a)), make_double2(0.0, 0.0), x, y, z, x, x);
    return Complex(cdot.x, cdot.y);
  }

  /**
     First returns the dot product (x,y)
     Returns the norm of x
  */
  __device__ double3 cdotNormA_(const double2 &a, const double2 &b) 
  { return make_double3(a.x*b.x + a.y*b.y, a.x*b.y - a.y*b.x, a.x*a.x + a.y*a.y); }
  __device__ double3 cdotNormA_(const float2 &a, const float2 &b) 
  { return make_double3(a.x*b.x + a.y*b.y, a.x*b.y - a.y*b.x, a.x*a.x + a.y*a.y); }
  __device__ double3 cdotNormA_(const float4 &a, const float4 &b) 
  { return make_double3(a.x*b.x + a.y*b.y + a.z*b.z + a.w*b.w, 
			a.x*b.y - a.y*b.x + a.z*b.w - a.w*b.z,
			a.x*a.x + a.y*a.y + a.z*a.z + a.w*a.w); }

  template <typename ReduceType, typename Float2, typename FloatN>
  struct CdotNormA : public ReduceFunctor<ReduceType, Float2, FloatN> {
    CdotNormA(const Float2 &a, const Float2 &b) { ; }
    __device__ void operator()(ReduceType &sum, FloatN &x, FloatN &y, FloatN &z, FloatN &w, FloatN &v) { sum += cdotNormA_(x,y); }
    static int streams() { return 2; } //! total number of input and output streams
    static int flops() { return 6; } //! flops per element
  };

  double3 cDotProductNormACuda(cudaColorSpinorField &x, cudaColorSpinorField &y) {
    return reduce::reduceCuda<double3,QudaSumFloat3,QudaSumFloat,CdotNormA,0,0,0,0,0,false>
      (make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
  }

  /**
     First returns the dot product (x,y)
     Returns the norm of y
  */
  __device__ double3 cdotNormB_(const double2 &a, const double2 &b) 
  { return make_double3(a.x*b.x + a.y*b.y, a.x*b.y - a.y*b.x, b.x*b.x + b.y*b.y); }
  __device__ double3 cdotNormB_(const float2 &a, const float2 &b) 
  { return make_double3(a.x*b.x + a.y*b.y, a.x*b.y - a.y*b.x, b.x*b.x + b.y*b.y); }
  __device__ double3 cdotNormB_(const float4 &a, const float4 &b) 
  { return make_double3(a.x*b.x + a.y*b.y + a.z*b.z + a.w*b.w, a.x*b.y - a.y*b.x + a.z*b.w - a.w*b.z,
			b.x*b.x + b.y*b.y + b.z*b.z + b.w*b.w); }

  template <typename ReduceType, typename Float2, typename FloatN>
  struct CdotNormB : public ReduceFunctor<ReduceType, Float2, FloatN> {
    CdotNormB(const Float2 &a, const Float2 &b) { ; }
    __device__ void operator()(ReduceType &sum, FloatN &x, FloatN &y, FloatN &z, FloatN &w, FloatN &v) { sum += cdotNormB_(x,y); }
    static int streams() { return 2; } //! total number of input and output streams
    static int flops() { return 6; } //! flops per element
  };

  double3 cDotProductNormBCuda(cudaColorSpinorField &x, cudaColorSpinorField &y) {
    return reduce::reduceCuda<double3,QudaSumFloat3,QudaSumFloat,CdotNormB,0,0,0,0,0,false>
      (make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
  }

  /**
     This convoluted kernel does the following: 
     z += a*x + b*y, y -= b*w, norm = (y,y), dot = (u, y)
  */
  template <typename ReduceType, typename Float2, typename FloatN>
  struct caxpbypzYmbwcDotProductUYNormY : public ReduceFunctor<ReduceType, Float2, FloatN> {
    Float2 a;
    Float2 b;
    caxpbypzYmbwcDotProductUYNormY(const Float2 &a, const Float2 &b) : a(a), b(b) { ; }
    __device__ void operator()(ReduceType &sum, FloatN &x, FloatN &y, FloatN &z, FloatN &w, FloatN &v) { Caxpy_(a, x, z); Caxpy_(b, y, z); Caxpy_(-b, w, y); sum += cdotNormB_(v,y); }
    static int streams() { return 7; } //! total number of input and output streams
    static int flops() { return 18; } //! flops per element
  };

  double3 caxpbypzYmbwcDotProductUYNormYCuda(const Complex &a, cudaColorSpinorField &x, 
					     const Complex &b, cudaColorSpinorField &y,
					     cudaColorSpinorField &z, cudaColorSpinorField &w,
					     cudaColorSpinorField &u) {
    if (x.Precision() != z.Precision()) {
      return reduce::mixed::reduceCuda<double3,QudaSumFloat3,QudaSumFloat,caxpbypzYmbwcDotProductUYNormY,0,1,1,0,0,false>
      (make_double2(REAL(a), IMAG(a)), make_double2(REAL(b), IMAG(b)), x, y, z, w, u);

    } else {
      return reduce::reduceCuda<double3,QudaSumFloat3,QudaSumFloat,caxpbypzYmbwcDotProductUYNormY,0,1,1,0,0,false>
      (make_double2(REAL(a), IMAG(a)), make_double2(REAL(b), IMAG(b)), x, y, z, w, u);
    }
  }


  /**
     Specialized kernel for the modified CG norm computation for
     computing beta.  Computes y = y + a*x and returns norm(y) and
     dot(y, delta(y)) where delta(y) is the difference between the
     input and out y vector.
  */
  template <typename ReduceType, typename Float2, typename FloatN>
  struct axpyCGNorm2 : public ReduceFunctor<ReduceType, Float2, FloatN> {
    Float2 a;
    axpyCGNorm2(const Float2 &a, const Float2 &b) : a(a) { ; }
    __device__ void operator()(ReduceType &sum, FloatN &x, FloatN &y, FloatN &z, FloatN &w, FloatN &v) { 
      FloatN y_new = y + a.x*x;
      sum.x += norm2_(y_new); 
      sum.y += dot_(y_new, y_new-y);
      y = y_new;
    }
    static int streams() { return 3; } //! total number of input and output streams
    static int flops() { return 6; } //! flops per real element
  };

  Complex axpyCGNormCuda(const double &a, cudaColorSpinorField &x, cudaColorSpinorField &y) {
    double2 cg_norm = reduce::reduceCuda<double2,QudaSumFloat2,QudaSumFloat,axpyCGNorm2,0,1,0,0,0,false>
      (make_double2(a, 0.0), make_double2(0.0, 0.0), x, y, x, x, x);
    return Complex(cg_norm.x, cg_norm.y);
  }

  /**
     This kernel returns (x, x) and (r,r) and also returns the so-called
     heavy quark norm as used by MILC: 1 / N * \sum_i (r, r)_i / (x, x)_i, where
     i is site index and N is the number of sites.
     
     When this kernel is launched, we must enforce that the parameter M
     in the launcher corresponds to the number of FloatN fields used to
     represent the spinor, e.g., M=6 for Wilson and M=3 for staggered.
     This is only the case for half-precision kernels by default.  To
     enable this, the siteUnroll template parameter must be set true
     when reduceCuda is instantiated.
  */
  template <typename ReduceType, typename Float2, typename FloatN>
  struct HeavyQuarkResidualNorm : public ReduceFunctor<ReduceType, Float2, FloatN> {
    Float2 a;
    Float2 b;
    ReduceType aux;
    HeavyQuarkResidualNorm(const Float2 &a, const Float2 &b) : a(a), b(b) { ; }
    
    __device__ void pre() { aux.x = 0; aux.y = 0; }
    
    __device__ void operator()(ReduceType &sum, FloatN &x, FloatN &y, FloatN &z, FloatN &w, FloatN &v) { aux.x += norm2_(x); aux.y += norm2_(y); }
    
    //! sum the solution and residual norms, and compute the heavy-quark norm
    __device__ void post(ReduceType &sum) 
    { 
      sum.x += aux.x; sum.y += aux.y; sum.z += (aux.x > 0.0) ? (aux.y / aux.x) : 1.0; 
    }
    
    static int streams() { return 2; } //! total number of input and output streams
    static int flops() { return 4; } //! undercounts since it excludes the per-site division
  };
  
  double3 HeavyQuarkResidualNormCuda(cudaColorSpinorField &x, cudaColorSpinorField &r) {
    double3 rtn = reduce::reduceCuda<double3,QudaSumFloat3,QudaSumFloat,HeavyQuarkResidualNorm,0,0,0,0,0,true>
      (make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, r, r, r, r);
#ifdef MULTI_GPU
    rtn.z /= (x.Volume()*comm_size());
#else
    rtn.z /= x.Volume();
#endif
    return rtn;
  }
  
  /**
     Variant of the HeavyQuarkResidualNorm kernel: this takes three
     arguments, the first two are summed together to form the
     solution, with the third being the residual vector.  This removes
     the need an additional xpy call in the solvers, impriving
     performance.
  */
  template <typename ReduceType, typename Float2, typename FloatN>
  struct xpyHeavyQuarkResidualNorm : public ReduceFunctor<ReduceType, Float2, FloatN> {
    Float2 a;
    Float2 b;
    ReduceType aux;
    xpyHeavyQuarkResidualNorm(const Float2 &a, const Float2 &b) : a(a), b(b) { ; }
    
    __device__ void pre() { aux.x = 0; aux.y = 0; }
    
    __device__ void operator()(ReduceType &sum, FloatN &x, FloatN &y, FloatN &z, FloatN &w, FloatN &v) 
    { aux.x += norm2_(x + y); aux.y += norm2_(z); }
    
    //! sum the solution and residual norms, and compute the heavy-quark norm
    __device__ void post(ReduceType &sum) 
    { 
      sum.x += aux.x; sum.y += aux.y; sum.z += (aux.x > 0.0) ? (aux.y / aux.x) : 1.0; 
    }
    
    static int streams() { return 3; } //! total number of input and output streams
    static int flops() { return 5; }
  };
  
  double3 xpyHeavyQuarkResidualNormCuda(cudaColorSpinorField &x, cudaColorSpinorField &y,
					cudaColorSpinorField &r) {
    double3 rtn = reduce::reduceCuda<double3,QudaSumFloat3,QudaSumFloat,xpyHeavyQuarkResidualNorm,0,0,0,0,0,true>
      (make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, r, r, r);
#ifdef MULTI_GPU
    rtn.z /= (x.Volume()*comm_size());
#else
    rtn.z /= x.Volume();
#endif
    return rtn;
  }

  /**
     double3 tripleCGUpdate(V x, V y, V z){}

     First performs the operation norm2(x)
     Second performs the operatio norm2(y)
     Third performs the operation dotPropduct(y,z)
  */

  template <typename ReduceType, typename Float2, typename FloatN>
  struct tripleCGReduction : public ReduceFunctor<ReduceType, Float2, FloatN> {
    tripleCGReduction(const Float2 &a, const Float2 &b) { ; }
    __device__ void operator()(ReduceType &sum, FloatN &x, FloatN &y, FloatN &z, FloatN &w, FloatN &v) 
    { sum.x += norm2_(x); sum.y += norm2_(y); sum.z += dot_(y,z); }
    static int streams() { return 3; } //! total number of input and output streams
    static int flops() { return 6; } //! flops per element
  };

  double3 tripleCGReductionCuda(cudaColorSpinorField &x, cudaColorSpinorField &y, cudaColorSpinorField &z) {
    return reduce::reduceCuda<double3,QudaSumFloat3,QudaSumFloat,tripleCGReduction,0,0,0,0,0,false>
      (make_double2(0.0, 0.0), make_double2(0.0, 0.0), x, y, z, x, x);
  }

} // namespace quda
