#include <quda_internal.h>
#include <lattice_field.h>
#include <read_gauge.h>
#include <gauge_field.h>
#include <ks_improved_force.h>
#include <hw_quda.h>
#include <hisq_force_macros.h>
#include <utility>
#include <quda_matrix.h>
#include <force_common.h>
#include <tune_quda.h>
#include <color_spinor_field.h>
#include <face_quda.h>
#include <index_helper.cuh>

#ifdef GPU_HISQ_FORCE

//DEBUG : control compile 
#define COMPILE_HISQ_DP_18 
#define COMPILE_HISQ_DP_12 
#define COMPILE_HISQ_SP_18 
#define COMPILE_HISQ_SP_12

// Disable texture read for now. Need to revisit this.
#define HISQ_SITE_MATRIX_LOAD_TEX 1
#define HISQ_NEW_OPROD_LOAD_TEX 1

#ifdef USE_TEXTURE_OBJECTS
#define TEX1DFETCH(type, tex, idx) tex1Dfetch<type>((tex), idx)
#else
#define TEX1DFETCH(type, tex, idx) tex1Dfetch((tex), idx)
#endif


template<typename Tex>
static __inline__ __device__ double fetch_double(Tex t, int i)
{
  int2 v = TEX1DFETCH(int2, t, i);
  return __hiloint2double(v.y, v.x);
}

template <typename Tex>
static __inline__ __device__ double2 fetch_double2(Tex t, int i)
{
  int4 v = TEX1DFETCH(int4, t, i);
  return make_double2(__hiloint2double(v.y, v.x), __hiloint2double(v.w, v.z));
}

static __inline__ __device__ double2 fetch_double2_old(texture<int4, 1> t, int i)
{
  int4 v = tex1Dfetch(t,i);
  return make_double2(__hiloint2double(v.y, v.x), __hiloint2double(v.w, v.z));
}





namespace quda {
  namespace fermion_force {

    struct hisq_kernel_param_t{
      unsigned long threads;
      int X[4];
      int D[4];
      int D1h;
      int base_idx[4];
      int ghostDim[4];
      int color_matrix_stride;
      int thin_link_stride;
      int momentum_stride;

      mutable int oddness_change;

      void setStride(const QudaGaugeParam& param){
        int half_volume = param.X[0]*param.X[1]*param.X[2]*param.X[3]/2;
#ifdef MULTI_GPU
        int extended_half_volume = (param.X[0]+4)*(param.X[1]+4)*(param.X[2]+4)*(param.X[3]+4)/2;
        thin_link_stride = extended_half_volume + param.site_ga_pad;
        color_matrix_stride = extended_half_volume;
#else
        thin_link_stride  = half_volume + param.site_ga_pad;
        color_matrix_stride = half_volume;
#endif
        momentum_stride = half_volume + param.mom_ga_pad;
      }
    };


    //Double precision for site link
    texture<int4, 1> thinLink0TexDouble;
    texture<int4, 1> thinLink1TexDouble;

    //Single precision for site link
    texture<float2, 1, cudaReadModeElementType> thinLink0TexSingle;
    texture<float2, 1, cudaReadModeElementType> thinLink1TexSingle;

    texture<float4, 1, cudaReadModeElementType> thinLink0TexSingle_recon;
    texture<float4, 1, cudaReadModeElementType> thinLink1TexSingle_recon;


    texture<int4, 1> newOprod0TexDouble;
    texture<int4, 1> newOprod1TexDouble;
    texture<float2, 1, cudaReadModeElementType>  newOprod0TexSingle;
    texture<float2, 1, cudaReadModeElementType> newOprod1TexSingle;

    template <int dir>
    inline __device__ __host__ void updateCoords(int x[], int shift, const int X[4], const int partitioned[]){
#ifdef MULTI_GPU
      if (shift == 1) {
        x[dir] = (partitioned[dir] || (x[dir] != X[dir]+1)) ? x[dir]+1 : 2;
      } else if (shift == -1) {
        x[dir] = (partitioned[dir] || (x[dir] != 2)) ? x[dir]-1 : X[dir]+1;
      }
#else 
      x[dir] = (x[dir]+shift + X[dir])%X[dir];
#endif
    }

    inline __device__ __host__ void updateCoords(int x[], int dir, int shift, const int X[4], const int partitioned[]) {
      switch (dir) {
        case 0:
	  updateCoords<0>(x, shift, X, partitioned);
	  break;
        case 1:
	  updateCoords<1>(x, shift, X, partitioned);
	  break;
        case 2:
	  updateCoords<2>(x, shift, X, partitioned);
	  break;
        case 3:
	  updateCoords<3>(x, shift, X, partitioned);
	  break;
      }

      return;
    }


    __device__ __host__ inline int posDir(int dir){
      return (dir >= 4) ? 7-dir : dir;
    }


    //struct for holding the fattening path coefficients
    template<class Real>
      struct PathCoefficients
      {
        Real one; 
        Real three;
        Real five;
        Real seven;
        Real naik;
        Real lepage;
      };


    inline __device__ float2 operator*(float a, const float2 & b)
    {
      return make_float2(a*b.x,a*b.y);
    }

    inline __device__ double2 operator*(double a, const double2 & b)
    {
      return make_double2(a*b.x,a*b.y);
    }

    inline __device__ const float2 & operator+=(float2 & a, const float2 & b)
    {
      a.x += b.x;
      a.y += b.y;
      return a;
    }

    inline __device__ const double2 & operator+=(double2 & a, const double2 & b)
    {
      a.x += b.x;
      a.y += b.y;
      return a;
    }

    inline __device__ const float4 & operator+=(float4 & a, const float4 & b)
    {
      a.x += b.x;
      a.y += b.y;
      a.z += b.z;
      a.w += b.w;
      return a;
    }

    // Replication of code 
    // This structure is already defined in 
    // unitarize_utilities.h

    template<class T>
      struct RealTypeId; 

    template<>
      struct RealTypeId<float2>
      {
        typedef float Type;
      };

    template<>
      struct RealTypeId<double2>
      {
        typedef double Type;
      };


    template<class T>
      inline __device__
      void adjointMatrix(T* mat)
      {
#define CONJ_INDEX(i,j) j*3 + i

        T tmp;
        mat[CONJ_INDEX(0,0)] = Conj(mat[0]);
        mat[CONJ_INDEX(1,1)] = Conj(mat[4]);
        mat[CONJ_INDEX(2,2)] = Conj(mat[8]);
        tmp  = Conj(mat[1]);
        mat[CONJ_INDEX(1,0)] = Conj(mat[3]);
        mat[CONJ_INDEX(0,1)] = tmp;	
        tmp = Conj(mat[2]);
        mat[CONJ_INDEX(2,0)] = Conj(mat[6]);
        mat[CONJ_INDEX(0,2)] = tmp;
        tmp = Conj(mat[5]);
        mat[CONJ_INDEX(2,1)] = Conj(mat[7]);
        mat[CONJ_INDEX(1,2)] = tmp;

#undef CONJ_INDEX
        return;
      }


    template<int N, class T>
      inline __device__
      void loadMatrixFromField(const T* const field_even, const T* const field_odd,
          int dir, int idx, T* const mat, int oddness, int stride)
      {
        const T* const field = (oddness)?field_odd:field_even;
        for(int i = 0;i < N ;i++){
          mat[i] = field[idx + dir*N*stride + i*stride];          
        }
        return;
      }

    template<class T>
      inline __device__
      void loadMatrixFromField(const T* const field_even, const T* const field_odd,
          int dir, int idx, T* const mat, int oddness, int stride)
      {
        loadMatrixFromField<9> (field_even, field_odd, dir, idx, mat, oddness, stride);
        return;
      }



    inline __device__
      void loadMatrixFromField(const float4* const field_even, const float4* const field_odd, 
          int dir, int idx, float2* const mat, int oddness, int stride)
      {
        const float4* const field = oddness?field_odd: field_even;
        float4 tmp;
        tmp = field[idx + dir*stride*3];
        mat[0] = make_float2(tmp.x, tmp.y);
        mat[1] = make_float2(tmp.z, tmp.w);
        tmp = field[idx + dir*stride*3 + stride];
        mat[2] = make_float2(tmp.x, tmp.y);
        mat[3] = make_float2(tmp.z, tmp.w);
        tmp = field[idx + dir*stride*3 + 2*stride];
        mat[4] = make_float2(tmp.x, tmp.y);
        mat[5] = make_float2(tmp.z, tmp.w);
        return;
      }

    template<class T>
      inline __device__
      void loadMatrixFromField(const T* const field_even, const T* const field_odd, int idx, T* const mat, int oddness, int stride)
      {
        const T* const field = (oddness)?field_odd:field_even;
        mat[0] = field[idx];
        mat[1] = field[idx + stride];
        mat[2] = field[idx + stride*2];
        mat[3] = field[idx + stride*3];
        mat[4] = field[idx + stride*4];
        mat[5] = field[idx + stride*5];
        mat[6] = field[idx + stride*6];
        mat[7] = field[idx + stride*7];
        mat[8] = field[idx + stride*8];

        return;
      }

    template<class U>
      inline __device__
      void  addMatrixToNewOprod(const double2* const mat,  int dir, int idx, U coeff, 
          double2* const field_even, double2* const field_odd, int oddness, int stride){
        double2* const field = (oddness)?field_odd: field_even;		
        double2 value[9];			

#if (HISQ_NEW_OPROD_LOAD_TEX == 1)
        value[0] = READ_DOUBLE2_TEXTURE( ((oddness)?newOprod1TexDouble:newOprod0TexDouble), field, idx+dir*stride*9); 
        value[1] = READ_DOUBLE2_TEXTURE( ((oddness)?newOprod1TexDouble:newOprod0TexDouble), field, idx+dir*stride*9 + stride); 
        value[2] = READ_DOUBLE2_TEXTURE( ((oddness)?newOprod1TexDouble:newOprod0TexDouble), field, idx+dir*stride*9 + 2*stride); 
        value[3] = READ_DOUBLE2_TEXTURE( ((oddness)?newOprod1TexDouble:newOprod0TexDouble), field, idx+dir*stride*9 + 3*stride); 
        value[4] = READ_DOUBLE2_TEXTURE( ((oddness)?newOprod1TexDouble:newOprod0TexDouble), field, idx+dir*stride*9 + 4*stride); 
        value[5] = READ_DOUBLE2_TEXTURE( ((oddness)?newOprod1TexDouble:newOprod0TexDouble), field, idx+dir*stride*9 + 5*stride); 
        value[6] = READ_DOUBLE2_TEXTURE( ((oddness)?newOprod1TexDouble:newOprod0TexDouble), field, idx+dir*stride*9 + 6*stride); 
        value[7] = READ_DOUBLE2_TEXTURE( ((oddness)?newOprod1TexDouble:newOprod0TexDouble), field, idx+dir*stride*9 + 7*stride); 
        value[8] = READ_DOUBLE2_TEXTURE( ((oddness)?newOprod1TexDouble:newOprod0TexDouble), field, idx+dir*stride*9 + 8*stride); 
#else
        for(int i=0; i<9; ++i) value[i] = field[i];
#endif

        field[idx + dir*stride*9]              = value[0] + coeff*mat[0]; 
        field[idx + dir*stride*9 + stride]     = value[1] + coeff*mat[1];	
        field[idx + dir*stride*9 + stride*2]   = value[2] + coeff*mat[2];	
        field[idx + dir*stride*9 + stride*3]   = value[3] + coeff*mat[3];	
        field[idx + dir*stride*9 + stride*4]   = value[4] + coeff*mat[4];	
        field[idx + dir*stride*9 + stride*5]   = value[5] + coeff*mat[5];	
        field[idx + dir*stride*9 + stride*6]   = value[6] + coeff*mat[6];	
        field[idx + dir*stride*9 + stride*7]   = value[7] + coeff*mat[7];	
        field[idx + dir*stride*9 + stride*8]   = value[8] + coeff*mat[8];	

        return;
      }					


    template<class U>
      inline __device__
      void  addMatrixToNewOprod(const float2* const mat,  int dir, int idx, U coeff, 
          float2* const field_even, float2* const field_odd, int oddness, int stride){
        float2* const field = (oddness)?field_odd: field_even;		
        float2 value[9];			

#if (HISQ_NEW_OPROD_LOAD_TEX == 1)
        value[0] = tex1Dfetch( ((oddness)?newOprod1TexSingle:newOprod0TexSingle),  idx+dir*stride*9); 
        value[1] = tex1Dfetch( ((oddness)?newOprod1TexSingle:newOprod0TexSingle),  idx+dir*stride*9 + stride); 
        value[2] = tex1Dfetch( ((oddness)?newOprod1TexSingle:newOprod0TexSingle),  idx+dir*stride*9 + 2*stride); 
        value[3] = tex1Dfetch( ((oddness)?newOprod1TexSingle:newOprod0TexSingle),  idx+dir*stride*9 + 3*stride); 
        value[4] = tex1Dfetch( ((oddness)?newOprod1TexSingle:newOprod0TexSingle),  idx+dir*stride*9 + 4*stride); 
        value[5] = tex1Dfetch( ((oddness)?newOprod1TexSingle:newOprod0TexSingle),  idx+dir*stride*9 + 5*stride); 
        value[6] = tex1Dfetch( ((oddness)?newOprod1TexSingle:newOprod0TexSingle),  idx+dir*stride*9 + 6*stride); 
        value[7] = tex1Dfetch( ((oddness)?newOprod1TexSingle:newOprod0TexSingle),  idx+dir*stride*9 + 7*stride); 
        value[8] = tex1Dfetch( ((oddness)?newOprod1TexSingle:newOprod0TexSingle),  idx+dir*stride*9 + 8*stride); 
#else 
        for(int i=0; i<9; ++i) value[i] = field[i];
#endif
        field[idx + dir*stride*9]              = value[0] + coeff*mat[0]; 
        field[idx + dir*stride*9 + stride]     = value[1] + coeff*mat[1];	
        field[idx + dir*stride*9 + stride*2]   = value[2] + coeff*mat[2];	
        field[idx + dir*stride*9 + stride*3]   = value[3] + coeff*mat[3];	
        field[idx + dir*stride*9 + stride*4]   = value[4] + coeff*mat[4];	
        field[idx + dir*stride*9 + stride*5]   = value[5] + coeff*mat[5];	
        field[idx + dir*stride*9 + stride*6]   = value[6] + coeff*mat[6];	
        field[idx + dir*stride*9 + stride*7]   = value[7] + coeff*mat[7];	
        field[idx + dir*stride*9 + stride*8]   = value[8] + coeff*mat[8];	

        return;
      }					


    // only works if Promote<T,U>::Type = T

    template<class T, class U>   
      inline __device__
      void addMatrixToField(const T* const mat, int dir, int idx, U coeff, 
          T* const field_even, T* const field_odd, int oddness, int stride)
      {
        T* const field = (oddness)?field_odd: field_even;
        field[idx + dir*stride*9]          += coeff*mat[0];
        field[idx + dir*stride*9 + stride]     += coeff*mat[1];
        field[idx + dir*stride*9 + stride*2]   += coeff*mat[2];
        field[idx + dir*stride*9 + stride*3]   += coeff*mat[3];
        field[idx + dir*stride*9 + stride*4]   += coeff*mat[4];
        field[idx + dir*stride*9 + stride*5]   += coeff*mat[5];
        field[idx + dir*stride*9 + stride*6]   += coeff*mat[6];
        field[idx + dir*stride*9 + stride*7]   += coeff*mat[7];
        field[idx + dir*stride*9 + stride*8]   += coeff*mat[8];

        return;
      }


    template<class T, class U>
      inline __device__
      void addMatrixToField(const T* const mat, int idx, U coeff, T* const field_even,
          T* const field_odd, int oddness, int stride)
      {
        T* const field = (oddness)?field_odd: field_even;
        field[idx ]         += coeff*mat[0];
        field[idx + stride]     += coeff*mat[1];
        field[idx + stride*2]   += coeff*mat[2];
        field[idx + stride*3]   += coeff*mat[3];
        field[idx + stride*4]   += coeff*mat[4];
        field[idx + stride*5]   += coeff*mat[5];
        field[idx + stride*6]   += coeff*mat[6];
        field[idx + stride*7]   += coeff*mat[7];
        field[idx + stride*8]   += coeff*mat[8];

        return;
      }

    template<class T, class U>
      inline __device__
      void addMatrixToField_test(const T* const mat, int idx, U coeff, T* const field_even,
          T* const field_odd, int oddness, int stride)
      {
        T* const field = (oddness)?field_odd: field_even;
        //T oldvalue=field[idx];
        field[idx ]         += coeff*mat[0];
        field[idx + stride]     += coeff*mat[1];
        field[idx + stride*2]   += coeff*mat[2];
        field[idx + stride*3]   += coeff*mat[3];
        field[idx + stride*4]   += coeff*mat[4];
        field[idx + stride*5]   += coeff*mat[5];
        field[idx + stride*6]   += coeff*mat[6];
        field[idx + stride*7]   += coeff*mat[7];
        field[idx + stride*8]   += coeff*mat[8];

        printf("value is  coeff(%f) * mat[0].x(%f)=%f\n", coeff, mat[0].x, field[idx].x);
        return;
      }

    template<class T>
      inline __device__
      void storeMatrixToField(const T* const mat, int dir, int idx, T* const field_even, T* const field_odd, int oddness, int stride)
      {
        T* const field = (oddness)?field_odd: field_even;
        field[idx + dir*stride*9]          = mat[0];
        field[idx + dir*stride*9 + stride]     = mat[1];
        field[idx + dir*stride*9 + stride*2]   = mat[2];
        field[idx + dir*stride*9 + stride*3]   = mat[3];
        field[idx + dir*stride*9 + stride*4]   = mat[4];
        field[idx + dir*stride*9 + stride*5]   = mat[5];
        field[idx + dir*stride*9 + stride*6]   = mat[6];
        field[idx + dir*stride*9 + stride*7]   = mat[7];
        field[idx + dir*stride*9 + stride*8]   = mat[8];

        return;
      }


    template<class T>
      inline __device__
      void storeMatrixToField(const T* const mat, int idx, T* const field_even, T* const field_odd, int oddness, int stride)
      {
        T* const field = (oddness)?field_odd: field_even;
        field[idx]          = mat[0];
        field[idx + stride]     = mat[1];
        field[idx + stride*2]   = mat[2];
        field[idx + stride*3]   = mat[3];
        field[idx + stride*4]   = mat[4];
        field[idx + stride*5]   = mat[5];
        field[idx + stride*6]   = mat[6];
        field[idx + stride*7]   = mat[7];
        field[idx + stride*8]   = mat[8];

        return;
      }


    template<class T, class U> 
      inline __device__
      void storeMatrixToMomentumField(const T* const mat, int dir, int idx, U coeff, 
          T* const mom_even, T* const mom_odd, int oddness, int stride)
      {
        T* const mom_field = (oddness)?mom_odd:mom_even;
        T temp2;
        temp2.x = (mat[1].x - mat[3].x)*0.5*coeff;
        temp2.y = (mat[1].y + mat[3].y)*0.5*coeff;
        mom_field[idx + dir*stride*5] = temp2;	

        temp2.x = (mat[2].x - mat[6].x)*0.5*coeff;
        temp2.y = (mat[2].y + mat[6].y)*0.5*coeff;
        mom_field[idx + dir*stride*5 + stride] = temp2;

        temp2.x = (mat[5].x - mat[7].x)*0.5*coeff;
        temp2.y = (mat[5].y + mat[7].y)*0.5*coeff;
        mom_field[idx + dir*stride*5 + stride*2] = temp2;

        const typename RealTypeId<T>::Type temp = (mat[0].y + mat[4].y + mat[8].y)*0.3333333333333333333333333;
        temp2.x =  (mat[0].y-temp)*coeff; 
        temp2.y =  (mat[4].y-temp)*coeff;
        mom_field[idx + dir*stride*5 + stride*3] = temp2;

        temp2.x = (mat[8].y - temp)*coeff;
        temp2.y = 0.0;
        mom_field[idx + dir*stride*5 + stride*4] = temp2;

        return;
      }

    __device__ __host__ inline int CoeffSign(int pos_dir, int odd_lattice) {
      return 2*((pos_dir + odd_lattice + 1) & 1) - 1;
    }

    __device__ __host__ inline int Sign(int parity) {
      return parity ? -1 : 1;
    }

    template<class RealX>
      struct ArrayLength
      {
        static const int result=9;
      };

    template<>
      struct ArrayLength<float4>
      {
        static const int result=5;
      };




    // Flops: four matrix additions per lattice site = 72 Flops per lattice site
    template<class RealA>
      __global__ void 
      do_one_link_term_kernel(const RealA* const oprodEven, const RealA* const oprodOdd,
          typename RealTypeId<RealA>::Type coeff,
          RealA* const outputEven, RealA* const outputOdd, hisq_kernel_param_t kparam)
      {
        int sid = blockIdx.x * blockDim.x + threadIdx.x;
        if (sid >= kparam.threads) return;
	int oddBit = threadIdx.y;
#ifdef MULTI_GPU
        int dx[4] = {0,0,0,0};
        int x[4];
        getCoords(x, sid, kparam.X, oddBit);
        int E[4] = {kparam.X[0]+4, kparam.X[1]+4, kparam.X[2]+4, kparam.X[3]+4};
        for(int dir=0; dir<4; ++dir) x[dir] += 2; 
        int new_sid = linkIndexShift(x,dx,E);
#else
        int new_sid = sid;
#endif
	for(int sig=0; sig<4; ++sig){
          RealA COLOR_MAT_W[ArrayLength<RealA>::result];
          loadMatrixFromField(oprodEven, oprodOdd, sig, new_sid, COLOR_MAT_W, oddBit, kparam.color_matrix_stride);
          addMatrixToField(COLOR_MAT_W, sig, new_sid, coeff, outputEven, outputOdd, oddBit, kparam.color_matrix_stride);
	}
        return;
      }


    template<int N>
      __device__ void loadLink(const double2* const linkEven, const double2* const linkOdd, int dir, int idx, double2* const var, int oddness, int stride){
#if (HISQ_SITE_MATRIX_LOAD_TEX == 1)
        HISQ_LOAD_MATRIX_18_DOUBLE_TEX((oddness)?thinLink1TexDouble:thinLink0TexDouble,  (oddness)?linkOdd:linkEven, dir, idx, var, stride);        
#else
        loadMatrixFromField(linkEven, linkOdd, dir, idx, var, oddness, stride);
#endif
      }

    template<>
      void loadLink<12>(const double2* const linkEven, const double2* const linkOdd, int dir, int idx, double2* const var, int oddness, int stride){
#if (HISQ_SITE_MATRIX_LOAD_TEX == 1)
        HISQ_LOAD_MATRIX_12_DOUBLE_TEX((oddness)?thinLink1TexDouble:thinLink0TexDouble,  (oddness)?linkOdd:linkEven,dir, idx, var, stride);        
#else
        loadMatrixFromField<6>(linkEven, linkOdd, dir, idx, var, oddness, stride);
#endif
      }

    template<int N>
      __device__ void loadLink(const float4* const linkEven, const float4* const linkOdd, int dir, int idx, float2* const var, int oddness, int stride){
#if (HISQ_SITE_MATRIX_LOAD_TEX == 1)
        HISQ_LOAD_MATRIX_12_SINGLE_TEX((oddness)?thinLink1TexSingle_recon:thinLink0TexSingle_recon, dir, idx, var, stride);  
#else
        loadMatrixFromField(linkEven, linkOdd, dir, idx, var, oddness, stride);      
#endif  
      }

    template<int N>
      __device__ void loadLink(const float2* const linkEven, const float2* const linkOdd, int dir, int idx, float2* const var , int oddness, int stride){
#if (HISQ_SITE_MATRIX_LOAD_TEX == 1)
        HISQ_LOAD_MATRIX_18_SINGLE_TEX((oddness)?thinLink1TexSingle:thinLink0TexSingle, dir, idx, var, stride);        
#else
        loadMatrixFromField(linkEven, linkOdd, dir, idx, var, oddness, stride);        
#endif
      }



#define DD_CONCAT(n,r) n ## r ## kernel

#define HISQ_KERNEL_NAME(a,b) DD_CONCAT(a,b)
    //precision: 0 is for double, 1 is for single

    //double precision, recon=18
#define PRECISION 0
#define RECON 18
#include "hisq_paths_force_core.h"
#undef PRECISION
#undef RECON

    //double precision, recon=12
#define PRECISION 0
#define RECON 12
#include "hisq_paths_force_core.h"
#undef PRECISION
#undef RECON

    //single precision, recon=18  
#define PRECISION 1
#define RECON 18
#include "hisq_paths_force_core.h"
#undef PRECISION
#undef RECON

    //single precision, recon=12
#define PRECISION 1
#define RECON 12
#include "hisq_paths_force_core.h"
#undef PRECISION
#undef RECON





    template<class RealA, class RealB>
      class MiddleLink : public TunableLocalParity {

        private:
          const cudaGaugeField &link;
          const cudaGaugeField &oprod;
          const cudaGaugeField &Qprev;
          const int sig;
          const int mu;
          const typename RealTypeId<RealA>::Type &coeff; 
          cudaGaugeField &Pmu;
          cudaGaugeField &P3;
          cudaGaugeField &Qmu;
          cudaGaugeField &newOprod;
          const hisq_kernel_param_t &kparam;
          unsigned int minThreads() const { return kparam.threads; }

        public:
          MiddleLink(const cudaGaugeField &link, 
              const cudaGaugeField &oprod,
              const cudaGaugeField &Qprev,
              int sig, int mu,
              const typename RealTypeId<RealA>::Type &coeff, 
              cudaGaugeField &Pmu, // write only
              cudaGaugeField &P3,  // write only
              cudaGaugeField &Qmu,
              cudaGaugeField &newOprod,
              const hisq_kernel_param_t &kparam) :
            link(link), oprod(oprod), Qprev(Qprev), sig(sig), mu(mu), 
            coeff(coeff), Pmu(Pmu), P3(P3), Qmu(Qmu), newOprod(newOprod), kparam(kparam)
        {	; }
          // need alternative constructor to hack around null pointer passing
          MiddleLink(const cudaGaugeField &link, 
              const cudaGaugeField &oprod,
              int sig, int mu,
              const typename RealTypeId<RealA>::Type &coeff, 
              cudaGaugeField &Pmu, // write only
              cudaGaugeField &P3,  // write only
              cudaGaugeField &Qmu,
              cudaGaugeField &newOprod,
              const hisq_kernel_param_t &kparam) :
            link(link), oprod(oprod), Qprev(link), sig(sig), mu(mu), 
            coeff(coeff), Pmu(Pmu), P3(P3), Qmu(Qmu), newOprod(newOprod), kparam(kparam)
        {	; }
          virtual ~MiddleLink() { ; }

          TuneKey tuneKey() const {
            std::stringstream vol, aux;
            vol << kparam.D[0] << "x";
            vol << kparam.D[1] << "x";
            vol << kparam.D[2] << "x";
            vol << kparam.D[3];    
            aux << "threads=" << kparam.threads << ",prec=" << link.Precision();
            aux << ",recon=" << link.Reconstruct() << ",sig=" << sig << ",mu=" << mu;
            return TuneKey(vol.str().c_str(), typeid(*this).name(), aux.str().c_str());
          }  


#define CALL_ARGUMENTS(typeA, typeB) <<<tp.grid, tp.block>>>		\
          ((typeA*)oprod.Even_p(), (typeA*)oprod.Odd_p(),			\
           (typeA*)Qprev_even, (typeA*)Qprev_odd,				\
           (typeB*)link.Even_p(), (typeB*)link.Odd_p(),			\
           sig, mu, coeff,							\
           (typeA*)Pmu.Even_p(), (typeA*)Pmu.Odd_p(),			\
           (typeA*)P3.Even_p(), (typeA*)P3.Odd_p(),				\
           (typeA*)Qmu.Even_p(), (typeA*)Qmu.Odd_p(),			\
           (typeA*)newOprod.Even_p(), (typeA*)newOprod.Odd_p(), kparam)


#define CALL_MIDDLE_LINK_KERNEL(sig_sign, mu_sign)			\
      if (sizeof(RealA) == sizeof(float2)) {				\
	if (recon  == QUDA_RECONSTRUCT_NO) {				\
	  do_middle_link_sp_18_kernel<float2, float2, sig_sign, mu_sign> CALL_ARGUMENTS(float2, float2); \
	} else {							\
	  do_middle_link_sp_12_kernel<float2, float4, sig_sign, mu_sign> CALL_ARGUMENTS(float2, float4); \
	}								\
      } else {								\
	if (recon  == QUDA_RECONSTRUCT_NO) {				\
	  do_middle_link_dp_18_kernel<double2, double2, sig_sign, mu_sign> CALL_ARGUMENTS(double2, double2); \
	} else {							\
	  do_middle_link_dp_12_kernel<double2, double2, sig_sign, mu_sign> CALL_ARGUMENTS(double2, double2); \
	}								\
      }

          void apply(const cudaStream_t &stream) {
            TuneParam tp = tuneLaunch(*this, getTuning(), getVerbosity());
            QudaReconstructType recon = link.Reconstruct();
            kparam.oddness_change = (kparam.base_idx[0] + kparam.base_idx[1]
				  + kparam.base_idx[2] + kparam.base_idx[3])&1;

            const void *Qprev_even = (&Qprev == &link) ? NULL : Qprev.Even_p();
            const void *Qprev_odd = (&Qprev == &link) ? NULL : Qprev.Odd_p();

            if (GOES_FORWARDS(sig) && GOES_FORWARDS(mu)){	
              CALL_MIDDLE_LINK_KERNEL(1,1);
            }else if (GOES_FORWARDS(sig) && GOES_BACKWARDS(mu)){
              CALL_MIDDLE_LINK_KERNEL(1,0);
            }else if (GOES_BACKWARDS(sig) && GOES_FORWARDS(mu)){
              CALL_MIDDLE_LINK_KERNEL(0,1);
            }else{
              CALL_MIDDLE_LINK_KERNEL(0,0);
            }
          }

#undef CALL_ARGUMENTS	
#undef CALL_MIDDLE_LINK_KERNEL

          void preTune() {
            Pmu.backup();
            P3.backup();
            Qmu.backup();
            newOprod.backup();
          }

          void postTune() {
            Pmu.restore();
            P3.restore();
            Qmu.restore();
            newOprod.restore();
          }

          long long flops() const { return 0; }
      };


    template<class RealA, class RealB>
      class LepageMiddleLink : public TunableLocalParity {

        private:
          const cudaGaugeField &link;
          const cudaGaugeField &oprod;
          const cudaGaugeField &Qprev;
          const int sig;
          const int mu;
          const typename RealTypeId<RealA>::Type &coeff; 
          cudaGaugeField &P3; // write only
          cudaGaugeField &newOprod;
          const hisq_kernel_param_t &kparam;
          unsigned int minThreads() const { return kparam.threads; }

        public:
          LepageMiddleLink(const cudaGaugeField &link, 
              const cudaGaugeField &oprod, 
              const cudaGaugeField &Qprev,
              int sig, int mu,
              const typename RealTypeId<RealA>::Type &coeff, 
              cudaGaugeField &P3, cudaGaugeField &newOprod,
              const hisq_kernel_param_t &kparam) :
            link(link), oprod(oprod), Qprev(Qprev), sig(sig), mu(mu), 
            coeff(coeff), P3(P3), newOprod(newOprod), kparam(kparam)
        {	; }
          virtual ~LepageMiddleLink() { ; }

          TuneKey tuneKey() const {
            std::stringstream vol, aux;
            vol << kparam.D[0] << "x";
            vol << kparam.D[1] << "x";
            vol << kparam.D[2] << "x";
            vol << kparam.D[3];    
            aux << "threads=" << kparam.threads << ",prec=" << link.Precision();
            aux << ",recon=" << link.Reconstruct() << ",sig=" << sig << ",mu=" << mu;
            return TuneKey(vol.str().c_str(), typeid(*this).name(), aux.str().c_str());
          }  

#define CALL_ARGUMENTS(typeA, typeB) <<<tp.grid, tp.block>>>		\
      ((typeA*)oprod.Even_p(), (typeA*)oprod.Odd_p(),			\
       (typeA*)Qprev.Even_p(), (typeA*)Qprev.Odd_p(),			\
       (typeB*)link.Even_p(), (typeB*)link.Odd_p(),			\
       sig, mu, coeff,							\
       (typeA*)P3.Even_p(), (typeA*)P3.Odd_p(),				\
       (typeA*)newOprod.Even_p(), (typeA*)newOprod.Odd_p(),		\
       kparam)
      
#define CALL_MIDDLE_LINK_KERNEL(sig_sign, mu_sign)			\
      if (sizeof(RealA) == sizeof(float2)) {				\
	if (recon  == QUDA_RECONSTRUCT_NO) {				\
	  do_lepage_middle_link_sp_18_kernel<float2, float2, sig_sign, mu_sign> CALL_ARGUMENTS(float2, float2); \
	} else {							\
	  do_lepage_middle_link_sp_12_kernel<float2, float4, sig_sign, mu_sign> CALL_ARGUMENTS(float2, float4); \
	}								\
      } else {								\
	if (recon  == QUDA_RECONSTRUCT_NO) {				\
	  do_lepage_middle_link_dp_18_kernel<double2, double2, sig_sign, mu_sign> CALL_ARGUMENTS(double2, double2); \
	} else {							\
	  do_lepage_middle_link_dp_12_kernel<double2, double2, sig_sign, mu_sign> CALL_ARGUMENTS(double2, double2); \
	}								\
      }									\
  
      void apply(const cudaStream_t &stream) {
	TuneParam tp = tuneLaunch(*this, getTuning(), getVerbosity());
	QudaReconstructType recon = link.Reconstruct();
	kparam.oddness_change = (kparam.base_idx[0] + kparam.base_idx[1]
				 + kparam.base_idx[2] + kparam.base_idx[3])&1;
	
	if (GOES_FORWARDS(sig) && GOES_FORWARDS(mu)){	
	  CALL_MIDDLE_LINK_KERNEL(1,1);
	}else if (GOES_FORWARDS(sig) && GOES_BACKWARDS(mu)){
	  CALL_MIDDLE_LINK_KERNEL(1,0);
	}else if (GOES_BACKWARDS(sig) && GOES_FORWARDS(mu)){
	  CALL_MIDDLE_LINK_KERNEL(0,1);
	}else{
	  CALL_MIDDLE_LINK_KERNEL(0,0);
	}
	
      }
      
#undef CALL_ARGUMENTS	
#undef CALL_MIDDLE_LINK_KERNEL
      
      void preTune() {
	P3.backup();
	newOprod.backup();
      }
      
      void postTune() {
	P3.restore();
	newOprod.restore();
      }
      
      long long flops() const { 
	if(GOES_FORWARDS(sig)) return 810*kparam.X[0]*kparam.X[1]*kparam.X[2]*kparam.X[3];
	return kparam.X[0]*kparam.X[1]*kparam.X[2]*kparam.X[3]*396; 
      }
    };

    template<class RealA, class RealB>
      class SideLink : public TunableLocalParity {

        private:
          const cudaGaugeField &link;
          const cudaGaugeField &P3;
          const cudaGaugeField &oprod;
          const int sig;
          const int mu;
          const typename RealTypeId<RealA>::Type &coeff; 
          const typename RealTypeId<RealA>::Type &accumu_coeff;
          cudaGaugeField &shortP;
          cudaGaugeField &newOprod;
          const hisq_kernel_param_t &kparam;

          unsigned int minThreads() const { return kparam.threads; }

        public:
          SideLink(const cudaGaugeField &link, 
              const cudaGaugeField &P3,
              const cudaGaugeField &oprod,
              int sig, int mu, 
              const typename RealTypeId<RealA>::Type &coeff, 
              const typename RealTypeId<RealA>::Type &accumu_coeff,
              cudaGaugeField &shortP,
              cudaGaugeField &newOprod,
              const hisq_kernel_param_t &kparam) :
            link(link), P3(P3), oprod(oprod), 
            sig(sig), mu(mu), coeff(coeff), accumu_coeff(accumu_coeff), 
            shortP(shortP), newOprod(newOprod), kparam(kparam)
        {	; }
          virtual ~SideLink() { ; }

          TuneKey tuneKey() const {
            std::stringstream vol, aux;
            vol << kparam.D[0] << "x";
            vol << kparam.D[1] << "x";
            vol << kparam.D[2] << "x";
            vol << kparam.D[3];    
            aux << "threads=" << kparam.threads << ",prec=" << link.Precision();
            aux << ",recon=" << link.Reconstruct() << ",sig=" << sig << ",mu=" << mu;
            return TuneKey(vol.str().c_str(), typeid(*this).name(), aux.str().c_str());
          }  

#define CALL_ARGUMENTS(typeA, typeB) <<<tp.grid, tp.block>>>		\
          ((typeA*)P3.Even_p(), (typeA*)P3.Odd_p(),				\
           (typeA*)oprod.Even_p(),  (typeA*)oprod.Odd_p(),			\
           (typeB*)link.Even_p(), (typeB*)link.Odd_p(),			\
           sig, mu,								\
           coeff,			\
           (typename RealTypeId<typeA>::Type) accumu_coeff,			\
           (typeA*)shortP.Even_p(), (typeA*)shortP.Odd_p(),			\
           (typeA*)newOprod.Even_p(), (typeA*)newOprod.Odd_p(),		\
           kparam)

#define CALL_SIDE_LINK_KERNEL(sig_sign, mu_sign)			\
      if (sizeof(RealA) == sizeof(float2)) {				\
	if (recon  == QUDA_RECONSTRUCT_NO) {				\
	  do_side_link_sp_18_kernel<float2, float2, sig_sign, mu_sign> CALL_ARGUMENTS(float2, float2); \
	} else {							\
	  do_side_link_sp_12_kernel<float2, float4, sig_sign, mu_sign> CALL_ARGUMENTS(float2, float4); \
	}								\
      } else {								\
	if(recon  == QUDA_RECONSTRUCT_NO){				\
	  do_side_link_dp_18_kernel<double2, double2, sig_sign, mu_sign> CALL_ARGUMENTS(double2, double2); \
	} else {							\
	  do_side_link_dp_12_kernel<double2, double2, sig_sign, mu_sign> CALL_ARGUMENTS(double2, double2); \
	}								\
      }

          void apply(const cudaStream_t &stream) {
            TuneParam tp = tuneLaunch(*this, getTuning(), getVerbosity());
            QudaReconstructType recon = link.Reconstruct();
            kparam.oddness_change = (kparam.base_idx[0] + kparam.base_idx[1]
                + kparam.base_idx[2] + kparam.base_idx[3])&1;

            if (GOES_FORWARDS(sig) && GOES_FORWARDS(mu)){
              CALL_SIDE_LINK_KERNEL(1,1);
            }else if (GOES_FORWARDS(sig) && GOES_BACKWARDS(mu)){
              CALL_SIDE_LINK_KERNEL(1,0); 
            }else if (GOES_BACKWARDS(sig) && GOES_FORWARDS(mu)){
              CALL_SIDE_LINK_KERNEL(0,1);
            }else{
              CALL_SIDE_LINK_KERNEL(0,0);
            }
          }

#undef CALL_SIDE_LINK_KERNEL
#undef CALL_ARGUMENTS      

          void preTune() {
            shortP.backup();
            newOprod.backup();
          }

          void postTune() {
            shortP.restore();
            newOprod.restore();
          }

          long long flops() const { return 0; }
      };


    template<class RealA, class RealB>
      class SideLinkShort : public TunableLocalParity {

        private:
          const cudaGaugeField &link;
          const cudaGaugeField &P3; 
          const int sig;
          const int mu;
          const typename RealTypeId<RealA>::Type &coeff; 
          cudaGaugeField &newOprod;
          const hisq_kernel_param_t &kparam;

          unsigned int minThreads() const { return kparam.threads; }

        public:
          SideLinkShort(const cudaGaugeField &link, const cudaGaugeField &P3, int sig, int mu, 
              const typename RealTypeId<RealA>::Type &coeff, cudaGaugeField &newOprod,
              const hisq_kernel_param_t &kparam) :
            link(link), P3(P3), sig(sig), mu(mu), coeff(coeff), newOprod(newOprod), kparam(kparam)
        {	; }
          virtual ~SideLinkShort() { ; }

          TuneKey tuneKey() const {
            std::stringstream vol, aux;
            vol << kparam.D[0] << "x";
            vol << kparam.D[1] << "x";
            vol << kparam.D[2] << "x";
            vol << kparam.D[3];    
            aux << "threads=" << kparam.threads << ",prec=" << link.Precision();
            aux << ",recon=" << link.Reconstruct() << ",sig=" << sig << ",mu=" << mu;
            return TuneKey(vol.str().c_str(), typeid(*this).name(), aux.str().c_str());
          }  

#define CALL_ARGUMENTS(typeA, typeB) <<<tp.grid, tp.block>>>		\
          ((typeA*)P3.Even_p(), (typeA*)P3.Odd_p(),				\
           (typeB*)link.Even_p(), (typeB*)link.Odd_p(),			\
           sig, mu,	(typename RealTypeId<typeA>::Type) coeff,		\
           (typeA*)newOprod.Even_p(), (typeA*)newOprod.Odd_p(), kparam)


#define CALL_SIDE_LINK_KERNEL(sig_sign, mu_sign)			\
    if (sizeof(RealA) == sizeof(float2)) {				\
      if (recon  == QUDA_RECONSTRUCT_NO) {				\
	do_side_link_short_sp_18_kernel<float2, float2, sig_sign, mu_sign> CALL_ARGUMENTS(float2, float2); \
      }else{								\
	do_side_link_short_sp_12_kernel<float2, float4, sig_sign, mu_sign> CALL_ARGUMENTS(float2, float4); \
      }									\
    } else {								\
      if(recon  == QUDA_RECONSTRUCT_NO){				\
	do_side_link_short_dp_18_kernel<double2, double2, sig_sign, mu_sign> CALL_ARGUMENTS(double2, double2); \
      }else{								\
	do_side_link_short_dp_12_kernel<double2, double2, sig_sign, mu_sign> CALL_ARGUMENTS(double2, double2); \
      }									\
    }

          void apply(const cudaStream_t &stream) {
            TuneParam tp = tuneLaunch(*this, getTuning(), getVerbosity());
            QudaReconstructType recon = link.Reconstruct();
            kparam.oddness_change = (kparam.base_idx[0] + kparam.base_idx[1]
                + kparam.base_idx[2] + kparam.base_idx[3])&1;

            if (GOES_FORWARDS(sig) && GOES_FORWARDS(mu)){
              CALL_SIDE_LINK_KERNEL(1,1);
            }else if (GOES_FORWARDS(sig) && GOES_BACKWARDS(mu)){
              CALL_SIDE_LINK_KERNEL(1,0);

            }else if (GOES_BACKWARDS(sig) && GOES_FORWARDS(mu)){
              CALL_SIDE_LINK_KERNEL(0,1);
            }else{
              CALL_SIDE_LINK_KERNEL(0,0);
            }	
          }

#undef CALL_SIDE_LINK_KERNEL
#undef CALL_ARGUMENTS      


          void preTune() {
            newOprod.backup();
          }

          void postTune() {
            newOprod.restore();
          }

          long long flops() const { return 0; }
      };

    template<class RealA, class RealB>
      class AllLink : public TunableLocalParity {

        private:
          const cudaGaugeField &link;
          const cudaGaugeField &oprod;
          const cudaGaugeField &Qprev;
          const int sig;
          const int mu;
          const typename RealTypeId<RealA>::Type &coeff; 
          const typename RealTypeId<RealA>::Type &accumu_coeff;
          cudaGaugeField &shortP;
          cudaGaugeField &newOprod;
          const hisq_kernel_param_t &kparam;

          unsigned int minThreads() const { return kparam.threads; }

        public:
          AllLink(const cudaGaugeField &link, 
              const cudaGaugeField &oprod, 
              const cudaGaugeField &Qprev, 
              int sig, int mu,
              const typename RealTypeId<RealA>::Type &coeff, 
              const typename RealTypeId<RealA>::Type &accumu_coeff,
              cudaGaugeField &shortP, cudaGaugeField &newOprod, 
              const hisq_kernel_param_t &kparam) : 
            link(link), oprod(oprod), Qprev(Qprev), sig(sig), mu(mu), 
            coeff(coeff), accumu_coeff(accumu_coeff), shortP(shortP), 
            newOprod(newOprod), kparam(kparam)
        { ; }
          virtual ~AllLink() { ; }

          TuneKey tuneKey() const {
            std::stringstream vol, aux;
            vol << kparam.D[0] << "x";
            vol << kparam.D[1] << "x";
            vol << kparam.D[2] << "x";
            vol << kparam.D[3];    
            aux << "threads=" << kparam.threads << ",prec=" << link.Precision();
            aux << ",recon=" << link.Reconstruct() << ",sig=" << sig << ",mu=" << mu;
            return TuneKey(vol.str().c_str(), typeid(*this).name(), aux.str().c_str());
          }  

#define CALL_ARGUMENTS(typeA, typeB) <<<tp.grid, tp.block>>>		\
          ((typeA*)oprod.Even_p(), (typeA*)oprod.Odd_p(),			\
           (typeA*)Qprev.Even_p(), (typeA*)Qprev.Odd_p(),			\
           (typeB*)link.Even_p(), (typeB*)link.Odd_p(), sig,  mu,		\
           (typename RealTypeId<typeA>::Type)coeff,				\
           (typename RealTypeId<typeA>::Type)accumu_coeff,			\
           (typeA*)shortP.Even_p(),(typeA*)shortP.Odd_p(),			\
           (typeA*)newOprod.Even_p(), (typeA*)newOprod.Odd_p(), kparam)

#define CALL_ALL_LINK_KERNEL(sig_sign, mu_sign)				\
      if (sizeof(RealA) == sizeof(float2)) {				\
	if (recon  == QUDA_RECONSTRUCT_NO) {				\
	  do_all_link_sp_18_kernel<float2, float2, sig_sign, mu_sign> CALL_ARGUMENTS(float2, float2); \
	} else {							\
	  do_all_link_sp_12_kernel<float2, float4, sig_sign, mu_sign> CALL_ARGUMENTS(float2, float4); \
	}								\
      } else {								\
	if (recon  == QUDA_RECONSTRUCT_NO) {				\
	  do_all_link_dp_18_kernel<double2, double2, sig_sign, mu_sign> CALL_ARGUMENTS(double2, double2); \
	} else {							\
	  do_all_link_dp_12_kernel<double2, double2, sig_sign, mu_sign> CALL_ARGUMENTS(double2, double2); \
	}								\
      }

          void apply(const cudaStream_t &stream) {
            TuneParam tp = tuneLaunch(*this, getTuning(), getVerbosity());
            QudaReconstructType recon = link.Reconstruct();
            kparam.oddness_change = (kparam.base_idx[0] + kparam.base_idx[1]
                + kparam.base_idx[2] + kparam.base_idx[3])&1;

            if (GOES_FORWARDS(sig) && GOES_FORWARDS(mu)){
              CALL_ALL_LINK_KERNEL(1, 1);
            }else if (GOES_FORWARDS(sig) && GOES_BACKWARDS(mu)){
              CALL_ALL_LINK_KERNEL(1, 0);
            }else if (GOES_BACKWARDS(sig) && GOES_FORWARDS(mu)){
              CALL_ALL_LINK_KERNEL(0, 1);
            }else{
              CALL_ALL_LINK_KERNEL(0, 0);
            }

            return;
          }

#undef CALL_ARGUMENTS
#undef CALL_ALL_LINK_KERNEL	    

          void preTune() {
            shortP.backup();
            newOprod.backup();
          }

          void postTune() {
            shortP.restore();
            newOprod.restore();
          }

          long long flops() const { 
	    if(GOES_FORWARDS(sig)) return kparam.X[0]*kparam.X[1]*kparam.X[2]*kparam.X[3]*1242;
	
	    return kparam.X[0]*kparam.X[1]*kparam.X[2]*kparam.X[3]*828;
	  }
      };


    template<class RealA, class RealB>
      class OneLinkTerm : public TunableLocalParity {

        private:
          const cudaGaugeField &oprod;
          const typename RealTypeId<RealA>::Type &coeff; 
          cudaGaugeField &ForceMatrix;
          int X[4];
          hisq_kernel_param_t kparam;

          unsigned int minThreads() const { return X[0]*X[1]*X[2]*X[3]/2; }

        public:
          OneLinkTerm(const cudaGaugeField &oprod,  
              const typename RealTypeId<RealA>::Type &coeff, 
              cudaGaugeField &ForceMatrix, const QudaGaugeParam& param) :
            oprod(oprod), coeff(coeff), ForceMatrix(ForceMatrix)
        { 
          for(int dir=0; dir<4; ++dir) X[dir] = param.X[dir];

          kparam.threads = X[0]*X[1]*X[2]*X[3]/2;
          for(int dir=0; dir<4; ++dir){
            kparam.X[dir] = X[dir];
          }
          kparam.setStride(param);
        }

          virtual ~OneLinkTerm() { ; }

          TuneKey tuneKey() const {
            std::stringstream vol, aux;
            vol << X[0] << "x";
            vol << X[1] << "x";
            vol << X[2] << "x";
            vol << X[3];    
            int threads = X[0]*X[1]*X[2]*X[3]/2;
            aux << "threads=" << threads << ",prec=" << oprod.Precision();
            aux << ",coeff=" << coeff;
            return TuneKey(vol.str().c_str(), typeid(*this).name(), aux.str().c_str());
          }  

          void apply(const cudaStream_t &stream) {
            TuneParam tp = tuneLaunch(*this, getTuning(), getVerbosity());

            do_one_link_term_kernel<RealA><<<tp.grid,tp.block>>>(static_cast<const RealA*>(oprod.Even_p()), 
								 static_cast<const RealA*>(oprod.Odd_p()), 
								 coeff,
								 static_cast<RealA*>(ForceMatrix.Even_p()), 
								 static_cast<RealA*>(ForceMatrix.Odd_p()),
								 kparam);
          }

          void preTune() {
            ForceMatrix.backup();
          }

          void postTune() {
            ForceMatrix.restore();
          }

          long long flops() const { 
	    return 72*kparam.X[0]*kparam.X[1]*kparam.X[2]*kparam.X[3];
	  }
      };


    template<class RealA, class RealB>
      class LongLinkTerm : public TunableLocalParity {

        private:
          const cudaGaugeField &link;
          const cudaGaugeField &naikOprod;
          const typename RealTypeId<RealA>::Type naik_coeff;
          cudaGaugeField &output;
          int X[4];
          const hisq_kernel_param_t &kparam;

          unsigned int minThreads() const { return X[0]*X[1]*X[2]*X[3]/2; }

        public:
          LongLinkTerm(const cudaGaugeField &link, const cudaGaugeField &naikOprod,
              const typename RealTypeId<RealA>::Type &naik_coeff,
              cudaGaugeField &output, const hisq_kernel_param_t &kparam) :
            link(link), naikOprod(naikOprod),  naik_coeff(naik_coeff), output(output),
            kparam(kparam)
        { for(int dir=0; dir<4; ++dir) X[dir] = kparam.X[dir]; }

          virtual ~LongLinkTerm() { ; }

          TuneKey tuneKey() const {
            std::stringstream vol, aux;
            vol << X[0] << "x";
            vol << X[1] << "x";
            vol << X[2] << "x";
            vol << X[3];    
            int threads = X[0]*X[1]*X[2]*X[3]/2;
            aux << "threads=" << threads << ",prec=" << link.Precision();
            return TuneKey(vol.str().c_str(), typeid(*this).name(), aux.str().c_str());
          }  

#define CALL_ARGUMENTS(typeA, typeB) <<<tp.grid,tp.block>>>		\
          ((typeB*)link.Even_p(), (typeB*)link.Odd_p(),			\
           (typeA*)naikOprod.Even_p(),  (typeA*)naikOprod.Odd_p(),		\
           naik_coeff,							\
           (typeA*)output.Even_p(), (typeA*)output.Odd_p(),			\
           kparam);		

          void apply(const cudaStream_t &stream) {
            TuneParam tp = tuneLaunch(*this, getTuning(), getVerbosity());
            QudaReconstructType recon = link.Reconstruct();

            if(sizeof(RealA) == sizeof(float2)){
              if(recon == QUDA_RECONSTRUCT_NO){
                do_longlink_sp_18_kernel<float2,float2> CALL_ARGUMENTS(float2, float2);
              }else{
                do_longlink_sp_12_kernel<float2,float4> CALL_ARGUMENTS(float2, float4);
              }
            }else{
              if(recon == QUDA_RECONSTRUCT_NO){
                do_longlink_dp_18_kernel<double2,double2> CALL_ARGUMENTS(double2, double2);
              }else{
                do_longlink_dp_12_kernel<double2,double2> CALL_ARGUMENTS(double2, double2);
              }
            }
          }

#undef CALL_ARGUMENTS	

          void preTune() {
            output.backup();
          }

          void postTune() {
            output.restore();
          }

          long long flops() const { return 4968*kparam.X[0]*kparam.X[1]*kparam.X[2]*kparam.X[3]; }
      };




    template<class RealA, class RealB>
      class CompleteForce : public TunableLocalParity {

        private:
          const cudaGaugeField &link;
          const cudaGaugeField &oprod;
          cudaGaugeField &mom;
          int X[4];
          hisq_kernel_param_t kparam;

          unsigned int minThreads() const { return X[0]*X[1]*X[2]*X[3]/2; }

        public:
          CompleteForce(const cudaGaugeField &link, const cudaGaugeField &oprod, 
             cudaGaugeField &mom, const QudaGaugeParam &param) :
            link(link), oprod(oprod), mom(mom)
        {  

          for(int dir=0; dir<4; ++dir){
            X[dir] = param.X[dir];
            kparam.X[dir] = X[dir];
          }
          kparam.threads = X[0]*X[1]*X[2]*X[3]/2;
          kparam.setStride(param);
        }

          virtual ~CompleteForce() { ; }

          TuneKey tuneKey() const {
            std::stringstream vol, aux;
            vol << X[0] << "x";
            vol << X[1] << "x";
            vol << X[2] << "x";
            vol << X[3];    
            int threads = X[0]*X[1]*X[2]*X[3]/2;
            aux << "threads=" << threads << ",prec=" << link.Precision();
            return TuneKey(vol.str().c_str(), typeid(*this).name(), aux.str().c_str());
          }  

#define CALL_ARGUMENTS(typeA, typeB)  <<<tp.grid, tp.block>>>		\
          ((typeB*)link.Even_p(), (typeB*)link.Odd_p(),			\
           (typeA*)oprod.Even_p(), (typeA*)oprod.Odd_p(),			\
           (typeA*)mom.Even_p(), (typeA*)mom.Odd_p(),			\
           kparam);		

          void apply(const cudaStream_t &stream) {
            TuneParam tp = tuneLaunch(*this, getTuning(), getVerbosity());
            QudaReconstructType recon = link.Reconstruct();

            if(sizeof(RealA) == sizeof(float2)){
              if(recon == QUDA_RECONSTRUCT_NO){
                do_complete_force_sp_18_kernel<float2,float2> CALL_ARGUMENTS(float2, float2);
              }else{
                do_complete_force_sp_12_kernel<float2,float4> CALL_ARGUMENTS(float2, float4);
              }
            }else{
              if(recon == QUDA_RECONSTRUCT_NO){
                do_complete_force_dp_18_kernel<double2,double2> CALL_ARGUMENTS(double2, double2);
              }else{
                do_complete_force_dp_12_kernel<double2,double2> CALL_ARGUMENTS(double2, double2);
              }
            }
          }

#undef CALL_ARGUMENTS	

          void preTune() {
            mom.backup();
          }

          void postTune() {
            mom.restore();
          }

          long long flops() const { 
	    return kparam.X[0]*kparam.X[1]*kparam.X[2]*kparam.X[3]*792;
	  }
      };


    static void 
      bind_tex_link(const cudaGaugeField& link, const cudaGaugeField& newOprod)
      {
        if(link.Precision() == QUDA_DOUBLE_PRECISION){
          cudaBindTexture(0, thinLink0TexDouble, link.Even_p(), link.Bytes()/2);
          cudaBindTexture(0, thinLink1TexDouble, link.Odd_p(), link.Bytes()/2);

          cudaBindTexture(0, newOprod0TexDouble, newOprod.Even_p(), newOprod.Bytes()/2);
          cudaBindTexture(0, newOprod1TexDouble, newOprod.Odd_p(), newOprod.Bytes()/2);
        }else{
          if(link.Reconstruct() == QUDA_RECONSTRUCT_NO){
            cudaBindTexture(0, thinLink0TexSingle, link.Even_p(), link.Bytes()/2);      
            cudaBindTexture(0, thinLink1TexSingle, link.Odd_p(), link.Bytes()/2);      
          }else{
            cudaBindTexture(0, thinLink0TexSingle_recon, link.Even_p(), link.Bytes()/2);      
            cudaBindTexture(0, thinLink1TexSingle_recon, link.Odd_p(), link.Bytes()/2);            
          }
          cudaBindTexture(0, newOprod0TexSingle, newOprod.Even_p(), newOprod.Bytes()/2);
          cudaBindTexture(0, newOprod1TexSingle, newOprod.Odd_p(), newOprod.Bytes()/2);

        }
      }

    static void 
      unbind_tex_link(const cudaGaugeField& link, const cudaGaugeField& newOprod)
      {
        if(link.Precision() == QUDA_DOUBLE_PRECISION){
          cudaUnbindTexture(thinLink0TexDouble);
          cudaUnbindTexture(thinLink1TexDouble);
          cudaUnbindTexture(newOprod0TexDouble);
          cudaUnbindTexture(newOprod1TexDouble);
        }else{
          if(link.Reconstruct() == QUDA_RECONSTRUCT_NO){
            cudaUnbindTexture(thinLink0TexSingle);
            cudaUnbindTexture(thinLink1TexSingle);      
          }else{
            cudaUnbindTexture(thinLink0TexSingle_recon);
            cudaUnbindTexture(thinLink1TexSingle_recon);      
          }
          cudaUnbindTexture(newOprod0TexSingle);
          cudaUnbindTexture(newOprod1TexSingle);
        }
      }

    template<class Real, class RealA, class RealB>
      static void
      do_hisq_staples_force_cuda( PathCoefficients<Real> act_path_coeff,
          const QudaGaugeParam& param,
          const cudaGaugeField &oprod, 
          const cudaGaugeField &link,
          cudaGaugeField &Pmu,
          cudaGaugeField &P3,
          cudaGaugeField &P5,
          cudaGaugeField &Pnumu,
          cudaGaugeField &Qmu,
          cudaGaugeField &Qnumu,
          cudaGaugeField &newOprod)
      {

        Real coeff;
        Real OneLink, Lepage, FiveSt, ThreeSt, SevenSt;
        Real mLepage, mFiveSt, mThreeSt;

        OneLink = act_path_coeff.one;
        ThreeSt = act_path_coeff.three; mThreeSt = -ThreeSt;
        FiveSt  = act_path_coeff.five; mFiveSt  = -FiveSt;
        SevenSt = act_path_coeff.seven; 
        Lepage  = act_path_coeff.lepage; mLepage  = -Lepage;



       	OneLinkTerm<RealA, RealB> oneLink(oprod, OneLink, newOprod, param);
        oneLink.apply(0);
        checkCudaError();


        int ghostDim[4]={
          commDimPartitioned(0),
          commDimPartitioned(1),
          commDimPartitioned(2),
          commDimPartitioned(3)
        };

        hisq_kernel_param_t kparam_1g, kparam_2g;

        for(int dir=0; dir<4; ++dir){
          kparam_1g.X[dir] = param.X[dir];
          kparam_2g.X[dir] = param.X[dir];
        }

        kparam_1g.setStride(param);
        kparam_2g.setStride(param);


#ifdef MULTI_GPU
        kparam_1g.D[0] = commDimPartitioned(0)?(param.X[0]+2):(param.X[0]);
        kparam_1g.D[1] = commDimPartitioned(1)?(param.X[1]+2):(param.X[1]);
        kparam_1g.D[2] = commDimPartitioned(2)?(param.X[2]+2):(param.X[2]);
        kparam_1g.D[3] = commDimPartitioned(3)?(param.X[3]+2):(param.X[3]);
        kparam_1g.D1h =  kparam_1g.D[0]/2;
        kparam_1g.base_idx[0]=commDimPartitioned(0)?1:2;
        kparam_1g.base_idx[1]=commDimPartitioned(1)?1:2;
        kparam_1g.base_idx[2]=commDimPartitioned(2)?1:2;
        kparam_1g.base_idx[3]=commDimPartitioned(3)?1:2;
        kparam_1g.threads = kparam_1g.D[0]*kparam_1g.D[1]*kparam_1g.D[2]*kparam_1g.D[3]/2;

        kparam_2g.D[0] = commDimPartitioned(0)?(param.X[0]+4):(param.X[0]);
        kparam_2g.D[1] = commDimPartitioned(1)?(param.X[1]+4):(param.X[1]);
        kparam_2g.D[2] = commDimPartitioned(2)?(param.X[2]+4):(param.X[2]);
        kparam_2g.D[3] = commDimPartitioned(3)?(param.X[3]+4):(param.X[3]);
        kparam_2g.D1h = kparam_2g.D[0]/2;
        kparam_2g.base_idx[0]=commDimPartitioned(0)?0:2;
        kparam_2g.base_idx[1]=commDimPartitioned(1)?0:2;
        kparam_2g.base_idx[2]=commDimPartitioned(2)?0:2;
        kparam_2g.base_idx[3]=commDimPartitioned(3)?0:2;
        kparam_2g.threads = kparam_2g.D[0]*kparam_2g.D[1]*kparam_2g.D[2]*kparam_2g.D[3]/2;


        for(int i=0;i < 4; i++){
          kparam_1g.ghostDim[i] = kparam_2g.ghostDim[i]=kparam_1g.ghostDim[i]=kparam_2g.ghostDim[i] = ghostDim[i];
        }
#else
        hisq_kernel_param_t kparam;
        kparam.D[0] = param.X[0];
        kparam.D[1] = param.X[1];
        kparam.D[2] = param.X[2];
        kparam.D[3] = param.X[3];
        kparam.D1h = param.X[0]/2;
        kparam.threads=param.X[0]*param.X[1]*param.X[2]*param.X[3]/2;
        kparam.base_idx[0]=0;
        kparam.base_idx[1]=0;
        kparam.base_idx[2]=0;
        kparam.base_idx[3]=0;
        kparam_2g.threads = kparam_1g.threads = kparam.threads;
  
        for(int i=0; i<4; ++i){
          kparam_2g.D[i] = kparam_1g.D[i] = kparam.D[i];
          kparam_2g.D1h  = kparam_1g.D1h  = kparam.D1h;
          kparam_2g.base_idx[i] = kparam_1g.base_idx[i] = 0;
          kparam_2g.ghostDim[i] = kparam_1g.ghostDim[i] = 0;
        }
#endif
        for(int sig=0; sig<8; sig++){
          for(int mu=0; mu<8; mu++){
            if ( (mu == sig) || (mu == OPP_DIR(sig))){
              continue;
            }
            //3-link
            //Kernel A: middle link

            MiddleLink<RealA,RealB> middleLink( link, oprod,  // read only
                sig, mu, mThreeSt,
                Pmu, P3, Qmu, // write only
                newOprod, kparam_2g);
            middleLink.apply(0);
            checkCudaError();

            for(int nu=0; nu < 8; nu++){
              if (nu == sig || nu == OPP_DIR(sig)
                  || nu == mu || nu == OPP_DIR(mu)){
                continue;
              }
              //5-link: middle link
              //Kernel B
              MiddleLink<RealA,RealB> middleLink( link, Pmu, Qmu, // read only
                  sig, nu, FiveSt,
                  Pnumu, P5, Qnumu, // write only
                  newOprod, kparam_1g);
              middleLink.apply(0);
              checkCudaError();

              for(int rho = 0; rho < 8; rho++){
                if (rho == sig || rho == OPP_DIR(sig)
                    || rho == mu || rho == OPP_DIR(mu)
                    || rho == nu || rho == OPP_DIR(nu)){
                  continue;
                }

                //7-link: middle link and side link
                if(FiveSt != 0)coeff = SevenSt/FiveSt; else coeff = 0;
                AllLink<RealA,RealB> allLink(link, Pnumu, Qnumu, sig, rho, SevenSt, coeff,
                    P5, newOprod, kparam_1g);

                allLink.apply(0);
                checkCudaError();

                //return;
              }//rho  		

              //5-link: side link
              if(ThreeSt != 0)coeff = FiveSt/ThreeSt; else coeff = 0;
              SideLink<RealA,RealB> sideLink(link, P5, Qmu, //read only
                  sig, nu, mFiveSt, coeff,
                  P3, // write only
                  newOprod, kparam_1g);
              sideLink.apply(0);
              checkCudaError();

            } //nu 

            //lepage
            if(Lepage != 0.){
              LepageMiddleLink<RealA,RealB> 
                lepageMiddleLink ( link, Pmu, Qmu, // read only
                    sig, mu, Lepage,
                    P5, // write only
                    newOprod, kparam_2g);
              lepageMiddleLink.apply(0);
              checkCudaError();

              if(ThreeSt != 0)coeff = Lepage/ThreeSt ; else coeff = 0;

              SideLink<RealA, RealB> sideLink(link, P5, Qmu, // read only
                  sig, mu, mLepage, coeff,
                  P3, //write only
                  newOprod, kparam_2g);

              sideLink.apply(0);
              checkCudaError();		

            } // Lepage != 0.0

            //3-link side link
            SideLinkShort<RealA,RealB> sideLinkShort(link, P3, // read only
                sig, mu, ThreeSt,
                newOprod, kparam_1g);
            sideLinkShort.apply(0);
            checkCudaError();			    

          }//mu
        }//sig

        return; 
      } // do_hisq_staples_force_cuda


#undef Pmu
#undef Pnumu
#undef P3
#undef P5
#undef Qmu
#undef Qnumu


    void hisqCompleteForceCuda(const QudaGaugeParam &param,
        const cudaGaugeField &oprod,
        const cudaGaugeField &link,
        cudaGaugeField* force, 
	long long* flops)
    {
      bind_tex_link(link, oprod);

      if(param.cuda_prec == QUDA_DOUBLE_PRECISION){
        CompleteForce<double2,double2> completeForce(link, oprod, *force, param);
        completeForce.apply(0);
	if(flops) *flops = completeForce.flops();
        checkCudaError();
      }else if(param.cuda_prec == QUDA_SINGLE_PRECISION){
        CompleteForce<float2,float2> completeForce(link, oprod, *force, param);
        completeForce.apply(0);
	if(flops) *flops = completeForce.flops();
        checkCudaError();
      }else{
          errorQuda("Unsupported precision");
      }


      unbind_tex_link(link, oprod);
      return;
    }


    void hisqLongLinkForceCuda(double coeff,
        const QudaGaugeParam &param,
        const cudaGaugeField &oldOprod,
        const cudaGaugeField &link,
        cudaGaugeField  *newOprod,
	long long* flops)
    {
      bind_tex_link(link, *newOprod);
      const int volume = param.X[0]*param.X[1]*param.X[2]*param.X[3];
      hisq_kernel_param_t kparam;
      for(int i=0; i<4; i++){
        kparam.X[i] = param.X[i];
        kparam.ghostDim[i] = commDimPartitioned(i);
      }
      kparam.threads = volume/2;
      kparam.setStride(param);

      if(param.cuda_prec == QUDA_DOUBLE_PRECISION){
        LongLinkTerm<double2,double2> longLink(link, oldOprod, coeff, *newOprod, kparam);
        longLink.apply(0);
	if(flops) (*flops) = longLink.flops();
        checkCudaError();
      }else if(param.cuda_prec == QUDA_SINGLE_PRECISION){
        LongLinkTerm<float2,float2> longLink(link, oldOprod, static_cast<float>(coeff), *newOprod, kparam);
        longLink.apply(0);
	if(flops) (*flops) = longLink.flops();
        checkCudaError();
      }else{
        errorQuda("Unsupported precision");
      }
      unbind_tex_link(link, *newOprod);
      return;
    }





    void
      hisqStaplesForceCuda(const double path_coeff_array[6],
          const QudaGaugeParam &param,
          const cudaGaugeField &oprod, 
          const cudaGaugeField &link, 
          cudaGaugeField* newOprod,
	  long long* flops)
      {

#ifdef MULTI_GPU
        int X[4] = {
          param.X[0]+4,  param.X[1]+4,  param.X[2]+4,  param.X[3]+4
        };
#else
        int X[4] = {
          param.X[0],  param.X[1],  param.X[2],  param.X[3]
        };
#endif	

        // create color matrix fields with zero padding
        int pad = 0;
        GaugeFieldParam gauge_param(X, param.cuda_prec, QUDA_RECONSTRUCT_NO, pad, QUDA_SCALAR_GEOMETRY);

        gauge_param.ghostExchange = QUDA_GHOST_EXCHANGE_NO;
        gauge_param.siteSubset = QUDA_FULL_SITE_SUBSET;
        gauge_param.order = QUDA_FLOAT2_GAUGE_ORDER;
        cudaGaugeField Pmu(gauge_param);
        cudaGaugeField P3(gauge_param);
        cudaGaugeField P5(gauge_param);
        cudaGaugeField Pnumu(gauge_param);
        cudaGaugeField Qmu(gauge_param);
        cudaGaugeField Qnumu(gauge_param);

        bind_tex_link(link, *newOprod);

        cudaEvent_t start, end;

        cudaEventCreate(&start);
        cudaEventCreate(&end);

        cudaEventRecord(start);
        if (param.cuda_prec == QUDA_DOUBLE_PRECISION){

          PathCoefficients<double> act_path_coeff;
          act_path_coeff.one    = path_coeff_array[0];
          act_path_coeff.naik   = path_coeff_array[1];
          act_path_coeff.three  = path_coeff_array[2];
          act_path_coeff.five   = path_coeff_array[3];
          act_path_coeff.seven  = path_coeff_array[4];
          act_path_coeff.lepage = path_coeff_array[5];
          do_hisq_staples_force_cuda<double,double2,double2>( act_path_coeff,
              param,
              oprod,
              link, 
              Pmu,
              P3,
              P5,
              Pnumu,
              Qmu,
              Qnumu,
              *newOprod);


        }else if(param.cuda_prec == QUDA_SINGLE_PRECISION){	
          PathCoefficients<float> act_path_coeff;
          act_path_coeff.one    = path_coeff_array[0];
          act_path_coeff.naik   = path_coeff_array[1];
          act_path_coeff.three  = path_coeff_array[2];
          act_path_coeff.five   = path_coeff_array[3];
          act_path_coeff.seven  = path_coeff_array[4];
          act_path_coeff.lepage = path_coeff_array[5];

          do_hisq_staples_force_cuda<float,float2,float2>( act_path_coeff,
              param,
              oprod,
              link, 
              Pmu,
              P3,
              P5,
              Pnumu,
              Qmu,
              Qnumu,
              *newOprod);
        }else{
          errorQuda("Unsupported precision");
        }


        cudaEventRecord(end);
        cudaEventSynchronize(end);
        float runtime;
        cudaEventElapsedTime(&runtime, start, end);
	
	if(flops){
	  int volume = param.X[0]*param.X[1]*param.X[2]*param.X[3];
	  // Middle Link, side link, short side link, AllLink, OneLink
	  *flops = (134784 + 24192 + 103680 + 864 + 397440 + 72);
	  			
	  if(path_coeff_array[5] != 0.) *flops += 28944; // Lepage contribution
	  *flops *= volume;
	}

        unbind_tex_link(link, *newOprod);

        cudaEventDestroy(start);
        cudaEventDestroy(end);

        return; 
      }

  } // namespace fermion_force
} // namespace quda

#endif // GPU_HISQ_FORCE
