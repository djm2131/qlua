#include <quda_internal.h>
#include <quda_matrix.h>
#include <tune_quda.h>
#include <clover_field.h>
#include <gauge_field.h>
#include <gauge_field_order.h>

namespace CloverOrder {
  using namespace quda;
#include <clover_field_order.h>
} // CloverOrder


namespace quda {

#ifdef GPU_CLOVER_DIRAC

  template<typename Clover1, typename Clover2, typename Gauge>
    struct CloverTraceArg {
      Clover1 clover1;
      Clover2 clover2;
      Gauge gauge;
      int dir1;
      int dir2;

      CloverTraceArg(Clover1 &clover1, Clover2 &clover2, Gauge &gauge, int dir1, int dir2)
        : clover1(clover1), clover2(clover2), gauge(gauge), dir1(dir1), dir2(dir2) {}
    };


  template <typename Float, typename Clover1, typename Clover2, typename Gauge>
    __device__ __host__ void cloverSigmaTraceCompute(CloverTraceArg<Clover1, Clover2, Gauge>& arg, int x, int parity) 
    {

      Float A[72];
      typedef typename ComplexTypeId<Float>::Type Complex;

      Matrix<Complex,3> mat;  
      setZero(&mat);

      // load the clover term into memory

      int dir1 = arg.dir1;
      int dir2 = arg.dir2;


      Float sign = 1;
      if(dir2 < dir1){
        int tmp = dir2;
        dir2 = dir1;
        dir1 = tmp;
        sign = -1;
      }


      Float diag[2][6];
      complex<Float> tri[2][15];
      const int idtab[15]={0,1,3,6,10,2,4,7,11,5,8,12,9,13,14};
      complex<Float> ctmp;

      if(parity==0){
        arg.clover1.load(A,x,parity);
      }else{
        arg.clover2.load(A,x,parity);
      }

      for(int ch=0; ch<2; ++ch){
        // factor of two is inherent to QUDA clover storage
        for (int i=0; i<6; i++) diag[ch][i] = 2.0*A[ch*36+i];
        for (int i=0; i<15; i++) tri[ch][idtab[i]] = complex<Float>(2.0*A[ch*36+6+2*i], 2.0*A[ch*36+6+2*i+1]);
      }


      // X, Y
      if(dir1 == 0){
        if(dir2 == 1){
          for(int j=0; j<3; ++j){
            mat(j,j).y = diag[0][j+3] + diag[1][j+3] - diag[0][j] - diag[1][j];  
          }

          // triangular part
          int jk=0;
          for(int j=1; j<3; ++j){
            int jk2 = (j+3)*(j+2)/2 + 3;
            for(int k=0; k<j; ++k){
              ctmp = tri[0][jk2] + tri[1][jk2] - tri[0][jk] - tri[1][jk];

              mat(j,k).x = -ctmp.imag();
              mat(j,k).y =  ctmp.real();

              mat(k,j).x =  ctmp.imag();
              mat(k,j).y =  ctmp.real();

              jk++; jk2++; 
            }
          } // X Y


        }else if(dir2 == 2){

          for(int j=0; j<3; ++j){
            int jk = (j+3)*(j+2)/2;
            for(int k=0; k<3; ++k){
              int kj = (k+3)*(k+2)/2 + j;
              ctmp = conj(tri[0][kj]) - tri[0][jk] + conj(tri[1][kj]) - tri[1][jk];
              mat(j,k).x = ctmp.real();
              mat(j,k).y = ctmp.imag();
              jk++;
            }
          } // X Z

        }else if(dir2 == 3){
          for(int j=0; j<3; ++j){
            int jk = (j+3)*(j+2)/2;
            for(int k=0; k<3; ++k){
              int kj = (k+3)*(k+2)/2 + j;
              ctmp = conj(tri[0][kj]) + tri[0][jk] - conj(tri[1][kj]) - tri[1][jk]; 
              mat(j,k).x = -ctmp.imag();
              mat(j,k).y =  ctmp.real();
              jk++;
            }
          }

        } // dir2 == 3 // X T

      }else if(dir1 == 1){
        if(dir2 == 2){ // Y Z
          for(int j=0; j<3; ++j){
            int jk = (j+3)*(j+2)/2;
            for(int k=0; k<3; ++k){
              int kj = (k+3)*(k+2)/2 + j;
              ctmp = conj(tri[0][kj]) + tri[0][jk] + conj(tri[1][kj]) + tri[1][jk];
              mat(j,k).x =  ctmp.imag();
              mat(j,k).y = -ctmp.real();
              jk++;
            }
          }
        }else if(dir2 == 3){ // Y T
          for(int j=0; j<3; ++j){
            int jk = (j+3)*(j+2)/2;
            for(int k=0; k<3; ++k){
              int kj = (k+3)*(k+2)/2 + j;
              ctmp = conj(tri[0][kj]) - tri[0][jk] - conj(tri[1][kj]) + tri[1][jk];
              mat(j,k).x = ctmp.real();
              mat(j,k).y = ctmp.imag();
              jk++;
            }
          }
        } // dir2 == 3
      } // dir1 == 1
      else if(dir1 == 2){
        if(dir2 == 3){
          for(int j=0; j<3; ++j){
            mat(j,j).y = diag[0][j] - diag[0][j+3] - diag[1][j] + diag[1][j+3];
          }
          int jk=0;
          for(int j=1; j<3; ++j){
            int jk2 = (j+3)*(j+2)/2 + 3;
            for(int k=0; k<j; ++k){
              ctmp = tri[0][jk] - tri[0][jk2] - tri[1][jk] + tri[1][jk2];
              mat(j,k).x = -ctmp.imag();
              mat(j,k).y =  ctmp.real();

              mat(k,j).x = ctmp.imag();
              mat(k,j).y = ctmp.real();
              jk++; jk2++;
            }
          }
        }
      }
      // if we dir1 and dir2 were swapped, multiply by -1
      mat *= sign;

      arg.gauge.save((Float*)(mat.data), x, 0, parity);

      return;
    }

  template<typename Float, typename Clover1, typename Clover2, typename Gauge>
    void cloverSigmaTrace(CloverTraceArg<Clover1,Clover2,Gauge> arg)
    {
      for(int x=0; x<arg.clover1.volumeCB; x++){
        cloverSigmaTraceCompute<Float,Clover1,Clover2,Gauge>(arg, x, 1);
      }
      return;
    }


  template<typename Float, typename Clover1, typename Clover2, typename Gauge>
    __global__ void cloverSigmaTraceKernel(CloverTraceArg<Clover1,Clover2,Gauge> arg)
    {
      int idx = blockIdx.x*blockDim.x + threadIdx.x;
      if(idx >= arg.clover1.volumeCB) return;
      // odd parity
      cloverSigmaTraceCompute<Float,Clover1,Clover2,Gauge>(arg, idx, 1);
    }

  template<typename Float, typename Clover1, typename Clover2, typename Gauge>
    class CloverSigmaTrace : Tunable {
      CloverTraceArg<Clover1,Clover2,Gauge> arg;
      const GaugeField &meta;
      const QudaFieldLocation location;

      private:
      unsigned int sharedBytesPerThread() const { return 0; }
      unsigned int sharedBytesPerBlock(const TuneParam &param) const { return 0; }

      bool tuneSharedBytes() const { return false; } // Don't tune the shared memory
      bool tuneGridDim() const { return false; } // Don't tune the grid dimensions.
      unsigned int minThreads() const { return arg.clover1.volumeCB; }

      public: 
      CloverSigmaTrace(CloverTraceArg<Clover1,Clover2,Gauge> &arg, const GaugeField &meta, QudaFieldLocation location)
        : arg(arg), meta(meta), location(location) {
	writeAuxString("stride=%d", arg.clover1.stride);
      }
      virtual ~CloverSigmaTrace() {;}

      void apply(const cudaStream_t &stream){
        if (location == QUDA_CUDA_FIELD_LOCATION) {
	  TuneParam tp = tuneLaunch(*this, getTuning(), getVerbosity());
          cloverSigmaTraceKernel<Float,Clover1,Clover2,Gauge><<<tp.grid,tp.block,0>>>(arg);
        } else {
          cloverSigmaTrace<Float,Clover1,Clover2,Gauge>(arg);
        }
      }

      TuneKey tuneKey() const { return TuneKey(meta.VolString(), typeid(*this).name(), aux); }

      std::string paramString(const TuneParam &param) const { // Don't print the grid dim.
        std::stringstream ps;
        ps << "block=(" << param.block.x << "," << param.block.y << "," << param.block.z << "), ";
        ps << "shared=" << param.shared_bytes;
        return ps.str();
      }

      long long flops() const { return 0; } // Fix this
      long long bytes() const { return (arg.clover1.Bytes() + arg.gauge.Bytes()) * arg.clover1.volumeCB; } 

    }; // CloverSigmaTrace


  template<typename Float, typename Clover1, typename Clover2, typename Gauge>
  void computeCloverSigmaTrace(Clover1 clover1, Clover2 clover2, Gauge gauge, int dir1, int dir2,
			       const GaugeField &meta, QudaFieldLocation location)
  {
    CloverTraceArg<Clover1, Clover2, Gauge> arg(clover1, clover2, gauge, dir1, dir2);
    CloverSigmaTrace<Float,Clover1,Clover2,Gauge> traceCompute(arg, meta, location);
    traceCompute.apply(0);
    return;
  }

  template<typename Float>
    void computeCloverSigmaTrace(GaugeField& gauge, const CloverField& clover, int dir1, int dir2,
        QudaFieldLocation location){

    if(clover.isNative()) {
      typedef typename CloverOrder::quda::clover_mapper<Float>::type C;
      if (gauge.isNative()) {
	if (gauge.Reconstruct() == QUDA_RECONSTRUCT_NO) {
	  typedef typename gauge_mapper<Float,QUDA_RECONSTRUCT_NO>::type G;
	  computeCloverSigmaTrace<Float>( C(clover,0), C(clover,1), G(gauge), dir1, dir2, gauge, location);
	} else if(gauge.Reconstruct() == QUDA_RECONSTRUCT_12) {
	  typedef typename gauge_mapper<Float,QUDA_RECONSTRUCT_NO>::type G;
	  computeCloverSigmaTrace<Float>( C(clover,0), C(clover,1), G(gauge), dir1, dir2, gauge, location);
	} else {
	  errorQuda("Reconstruction type %d not supported", gauge.Reconstruct());
	}
      } else {
	errorQuda("Gauge order %d not supported", gauge.Order());
      }
    } else {
      errorQuda("clover order %d not supported", clover.Order());
    } // clover order

  }

#endif

  void computeCloverSigmaTrace(GaugeField& output, const CloverField& clover, int dir1, int dir2, 
      QudaFieldLocation location){

#ifdef GPU_CLOVER_DIRAC
    if(clover.Precision() == QUDA_HALF_PRECISION){
      errorQuda("Half precision not supported\n");
    }  

    if(clover.Precision() == QUDA_SINGLE_PRECISION){
      computeCloverSigmaTrace<float>(output, clover, dir1, dir2, location);
    }else if(clover.Precision() == QUDA_DOUBLE_PRECISION){
      computeCloverSigmaTrace<double>(output, clover, dir1, dir2, location);
    }else{
      errorQuda("Precision %d not supported", clover.Precision());
    }

#else
    errorQuda("Clover has not been built");
#endif

  }     


} // namespace quda
