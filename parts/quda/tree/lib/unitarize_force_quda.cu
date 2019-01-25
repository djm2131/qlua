#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <iomanip>
#include <cuda.h>
#include <gauge_field.h>
#include <tune_quda.h>

#include <tune_quda.h>
#include <quda_matrix.h>

#ifdef GPU_HISQ_FORCE

// work around for CUDA 7.0 bug on OSX
#if defined(__APPLE__) && CUDA_VERSION >= 7000 && CUDA_VERSION < 7050
#define EXPONENT_TYPE Real
#else
#define EXPONENT_TYPE int
#endif

namespace quda{
namespace {
  #include <svd_quda.h>
}

namespace { // anonymous
#include <svd_quda.h>
}

#define HISQ_UNITARIZE_PI 3.14159265358979323846
#define HISQ_UNITARIZE_PI23 HISQ_UNITARIZE_PI*2.0/3.0

// constants - File scope only
__constant__ double DEV_HISQ_UNITARIZE_EPS;
__constant__ double DEV_HISQ_FORCE_FILTER;
__constant__ double DEV_MAX_DET_ERROR;
__constant__ bool DEV_REUNIT_ALLOW_SVD;
__constant__ bool DEV_REUNIT_SVD_ONLY;
__constant__ double DEV_REUNIT_SVD_REL_ERROR;
__constant__ double DEV_REUNIT_SVD_ABS_ERROR;

static double HOST_HISQ_UNITARIZE_EPS;
static double HOST_HISQ_FORCE_FILTER;
static double HOST_MAX_DET_ERROR;
static bool   HOST_REUNIT_ALLOW_SVD;
static bool   HOST_REUNIT_SVD_ONLY;
static double HOST_REUNIT_SVD_REL_ERROR;
static double HOST_REUNIT_SVD_ABS_ERROR;


 
  namespace fermion_force{


    void setUnitarizeForceConstants(double unitarize_eps_h, double hisq_force_filter_h, 
				    double max_det_error_h, bool allow_svd_h, bool svd_only_h,
				    double svd_rel_error_h, double svd_abs_error_h)
    {

      // not_set is only initialised once
      static bool not_set=true;
		
      if(not_set){

	cudaMemcpyToSymbol(DEV_HISQ_UNITARIZE_EPS, &unitarize_eps_h, sizeof(double));
	cudaMemcpyToSymbol(DEV_HISQ_FORCE_FILTER, &hisq_force_filter_h, sizeof(double));
	cudaMemcpyToSymbol(DEV_MAX_DET_ERROR, &max_det_error_h, sizeof(double));
	cudaMemcpyToSymbol(DEV_REUNIT_ALLOW_SVD, &allow_svd_h, sizeof(bool));
	cudaMemcpyToSymbol(DEV_REUNIT_SVD_ONLY, &svd_only_h, sizeof(bool));
	cudaMemcpyToSymbol(DEV_REUNIT_SVD_REL_ERROR, &svd_rel_error_h, sizeof(double));
	cudaMemcpyToSymbol(DEV_REUNIT_SVD_ABS_ERROR, &svd_abs_error_h, sizeof(double));

	HOST_HISQ_UNITARIZE_EPS = unitarize_eps_h;
	HOST_HISQ_FORCE_FILTER = hisq_force_filter_h;
	HOST_MAX_DET_ERROR = max_det_error_h;     
	HOST_REUNIT_ALLOW_SVD = allow_svd_h;
	HOST_REUNIT_SVD_ONLY = svd_only_h;
	HOST_REUNIT_SVD_REL_ERROR = svd_rel_error_h;
	HOST_REUNIT_SVD_ABS_ERROR = svd_abs_error_h;
	not_set = false;
      }
      checkCudaError();
      return;
    }


    template<class Real>
    class DerivativeCoefficients{
    private:
      Real b[6]; 
      __device__ __host__       
      Real computeC00(const Real &, const Real &, const Real &);
      __device__ __host__
      Real computeC01(const Real &, const Real &, const Real &);
      __device__ __host__
      Real computeC02(const Real &, const Real &, const Real &);
      __device__ __host__
      Real computeC11(const Real &, const Real &, const Real &);
      __device__ __host__
      Real computeC12(const Real &, const Real &, const Real &);
      __device__ __host__
      Real computeC22(const Real &, const Real &, const Real &);

    public:
      __device__ __host__ void set(const Real & u, const Real & v, const Real & w);
      __device__ __host__
      Real getB00() const { return b[0]; }
      __device__ __host__
      Real getB01() const { return b[1]; }
      __device__ __host__
      Real getB02() const { return b[2]; }
      __device__ __host__
      Real getB11() const { return b[3]; }
      __device__ __host__
      Real getB12() const { return b[4]; }
      __device__ __host__
      Real getB22() const { return b[5]; }
    };

    template<class Real>
    __device__ __host__
    Real DerivativeCoefficients<Real>::computeC00(const Real & u, const Real & v, const Real & w){
      Real result = -pow(w,static_cast<EXPONENT_TYPE>(3)) * pow(u,static_cast<EXPONENT_TYPE>(6))
	+ 3*v*pow(w,static_cast<EXPONENT_TYPE>(3))*pow(u,static_cast<EXPONENT_TYPE>(4))
	+ 3*pow(v,static_cast<EXPONENT_TYPE>(4))*w*pow(u,static_cast<EXPONENT_TYPE>(4))
	-   pow(v,static_cast<EXPONENT_TYPE>(6))*pow(u,static_cast<EXPONENT_TYPE>(3))
	- 4*pow(w,static_cast<EXPONENT_TYPE>(4))*pow(u,static_cast<EXPONENT_TYPE>(3))
	- 12*pow(v,static_cast<EXPONENT_TYPE>(3))*pow(w,static_cast<EXPONENT_TYPE>(2))*pow(u,static_cast<EXPONENT_TYPE>(3))
	+ 16*pow(v,static_cast<EXPONENT_TYPE>(2))*pow(w,static_cast<EXPONENT_TYPE>(3))*pow(u,static_cast<EXPONENT_TYPE>(2))
	+ 3*pow(v,static_cast<EXPONENT_TYPE>(5))*w*pow(u,static_cast<EXPONENT_TYPE>(2))
	- 8*v*pow(w,static_cast<EXPONENT_TYPE>(4))*u
	- 3*pow(v,static_cast<EXPONENT_TYPE>(4))*pow(w,static_cast<EXPONENT_TYPE>(2))*u
	+ pow(w,static_cast<EXPONENT_TYPE>(5))
	+ pow(v,static_cast<EXPONENT_TYPE>(3))*pow(w,static_cast<EXPONENT_TYPE>(3));

      return result;
    }

    template<class Real>
    __device__ __host__
    Real DerivativeCoefficients<Real>::computeC01(const Real & u, const Real & v, const Real & w){
      Real result =  - pow(w,static_cast<EXPONENT_TYPE>(2))*pow(u,static_cast<EXPONENT_TYPE>(7))
	- pow(v,static_cast<EXPONENT_TYPE>(2))*w*pow(u,static_cast<EXPONENT_TYPE>(6))
	+ pow(v,static_cast<EXPONENT_TYPE>(4))*pow(u,static_cast<EXPONENT_TYPE>(5))   // This was corrected!
	+ 6*v*pow(w,static_cast<EXPONENT_TYPE>(2))*pow(u,static_cast<EXPONENT_TYPE>(5))
	- 5*pow(w,static_cast<EXPONENT_TYPE>(3))*pow(u,static_cast<EXPONENT_TYPE>(4))    // This was corrected!
	- pow(v,static_cast<EXPONENT_TYPE>(3))*w*pow(u,static_cast<EXPONENT_TYPE>(4))
	- 2*pow(v,static_cast<EXPONENT_TYPE>(5))*pow(u,static_cast<EXPONENT_TYPE>(3))
	- 6*pow(v,static_cast<EXPONENT_TYPE>(2))*pow(w,static_cast<EXPONENT_TYPE>(2))*pow(u,static_cast<EXPONENT_TYPE>(3))
	+ 10*v*pow(w,static_cast<EXPONENT_TYPE>(3))*pow(u,static_cast<EXPONENT_TYPE>(2))
	+ 6*pow(v,static_cast<EXPONENT_TYPE>(4))*w*pow(u,static_cast<EXPONENT_TYPE>(2))
	- 3*pow(w,static_cast<EXPONENT_TYPE>(4))*u
	- 6*pow(v,static_cast<EXPONENT_TYPE>(3))*pow(w,static_cast<EXPONENT_TYPE>(2))*u
	+ 2*pow(v,static_cast<EXPONENT_TYPE>(2))*pow(w,static_cast<EXPONENT_TYPE>(3));
      return result;
    }

    template<class Real>
    __device__ __host__
    Real DerivativeCoefficients<Real>::computeC02(const Real & u, const Real & v, const Real & w){
      Real result =   pow(w,static_cast<EXPONENT_TYPE>(2))*pow(u,static_cast<EXPONENT_TYPE>(5))
	+ pow(v,static_cast<EXPONENT_TYPE>(2))*w*pow(u,static_cast<EXPONENT_TYPE>(4))
	- pow(v,static_cast<EXPONENT_TYPE>(4))*pow(u,static_cast<EXPONENT_TYPE>(3))
	- 4*v*pow(w,static_cast<EXPONENT_TYPE>(2))*pow(u,static_cast<EXPONENT_TYPE>(3))
	+ 4*pow(w,static_cast<EXPONENT_TYPE>(3))*pow(u,static_cast<EXPONENT_TYPE>(2))
	+ 3*pow(v,static_cast<EXPONENT_TYPE>(3))*w*pow(u,static_cast<EXPONENT_TYPE>(2))
	- 3*pow(v,static_cast<EXPONENT_TYPE>(2))*pow(w,static_cast<EXPONENT_TYPE>(2))*u
	+ v*pow(w,static_cast<EXPONENT_TYPE>(3));
      return result;
    }

    template<class Real>
    __device__ __host__
    Real DerivativeCoefficients<Real>::computeC11(const Real & u, const Real & v, const Real & w){
      Real result = - w*pow(u,static_cast<EXPONENT_TYPE>(8))
	- pow(v,static_cast<EXPONENT_TYPE>(2))*pow(u,static_cast<EXPONENT_TYPE>(7))
	+ 7*v*w*pow(u,static_cast<EXPONENT_TYPE>(6))
	+ 4*pow(v,static_cast<EXPONENT_TYPE>(3))*pow(u,static_cast<EXPONENT_TYPE>(5))
	- 5*pow(w,static_cast<EXPONENT_TYPE>(2))*pow(u,static_cast<EXPONENT_TYPE>(5))
	- 16*pow(v,static_cast<EXPONENT_TYPE>(2))*w*pow(u,static_cast<EXPONENT_TYPE>(4))
	- 4*pow(v,static_cast<EXPONENT_TYPE>(4))*pow(u,static_cast<EXPONENT_TYPE>(3))
	+ 16*v*pow(w,static_cast<EXPONENT_TYPE>(2))*pow(u,static_cast<EXPONENT_TYPE>(3))
	- 3*pow(w,static_cast<EXPONENT_TYPE>(3))*pow(u,static_cast<EXPONENT_TYPE>(2))
	+ 12*pow(v,static_cast<EXPONENT_TYPE>(3))*w*pow(u,static_cast<EXPONENT_TYPE>(2))
	- 12*pow(v,static_cast<EXPONENT_TYPE>(2))*pow(w,static_cast<EXPONENT_TYPE>(2))*u
	+ 3*v*pow(w,static_cast<EXPONENT_TYPE>(3));
      return result;
    }

    template<class Real>
    __device__ __host__
    Real DerivativeCoefficients<Real>::computeC12(const Real & u, const Real & v, const Real & w){
      Real result =  w*pow(u,static_cast<EXPONENT_TYPE>(6))
	+ pow(v,static_cast<EXPONENT_TYPE>(2))*pow(u,static_cast<EXPONENT_TYPE>(5)) // Fixed this!
	- 5*v*w*pow(u,static_cast<EXPONENT_TYPE>(4))  // Fixed this!
	- 2*pow(v,static_cast<EXPONENT_TYPE>(3))*pow(u,static_cast<EXPONENT_TYPE>(3))
	+ 4*pow(w,static_cast<EXPONENT_TYPE>(2))*pow(u,static_cast<EXPONENT_TYPE>(3))
	+ 6*pow(v,static_cast<EXPONENT_TYPE>(2))*w*pow(u,static_cast<EXPONENT_TYPE>(2))
	- 6*v*pow(w,static_cast<EXPONENT_TYPE>(2))*u
	+ pow(w,static_cast<EXPONENT_TYPE>(3));
      return result;
    }

    template<class Real>
    __device__ __host__
    Real DerivativeCoefficients<Real>::computeC22(const Real & u, const Real & v, const Real & w){
      Real result = - w*pow(u,static_cast<EXPONENT_TYPE>(4))
	- pow(v,static_cast<EXPONENT_TYPE>(2))*pow(u,static_cast<EXPONENT_TYPE>(3))
	+ 3*v*w*pow(u,static_cast<EXPONENT_TYPE>(2))
	- 3*pow(w,static_cast<EXPONENT_TYPE>(2))*u;
      return result;
    }

    template <class Real>
    __device__ __host__
    void  DerivativeCoefficients<Real>::set(const Real & u, const Real & v, const Real & w){
      const Real & denominator = 2.0*pow(w*(u*v-w),static_cast<EXPONENT_TYPE>(3));
      b[0] = computeC00(u,v,w)/denominator;
      b[1] = computeC01(u,v,w)/denominator;
      b[2] = computeC02(u,v,w)/denominator;
      b[3] = computeC11(u,v,w)/denominator;
      b[4] = computeC12(u,v,w)/denominator;
      b[5] = computeC22(u,v,w)/denominator;
      return;
    }


    template<class Cmplx>
    __device__ __host__
    void accumBothDerivatives(Matrix<Cmplx,3>* result, const Matrix<Cmplx,3> & left, const Matrix<Cmplx,3> & right, const Matrix<Cmplx,3> & outer_prod)
    {
      const typename RealTypeId<Cmplx>::Type temp = 2.0*getTrace(left*outer_prod).x;
      for(int k=0; k<3; ++k){
	for(int l=0; l<3; ++l){
	  // Need to write it this way to get it to work 
	  // on the CPU. Not sure why.
	  result->operator()(k,l).x += temp*right(k,l).x;
	  result->operator()(k,l).y += temp*right(k,l).y;
	}
      }
      return;
    }


    template<class Cmplx>
    __device__ __host__
    void accumDerivatives(Matrix<Cmplx,3>* result, const Matrix<Cmplx,3> & left, const Matrix<Cmplx,3> & right, const Matrix<Cmplx,3> & outer_prod)
    {
      Cmplx temp = getTrace(left*outer_prod);
      for(int k=0; k<3; ++k){
	for(int l=0; l<3; ++l){
	  result->operator()(k,l) = temp*right(k,l);
	}
      }
      return;
    }


    template<class T>
    __device__ __host__
    T getAbsMin(const T* const array, int size){
      T min = fabs(array[0]);
      for(int i=1; i<size; ++i){
        T abs_val = fabs(array[i]);
        if((abs_val) < min){ min = abs_val; }   
      }
      return min;
    }


    template<class Real>
    __device__ __host__
    inline bool checkAbsoluteError(Real a, Real b, Real epsilon)
    {
      if( fabs(a-b) <  epsilon) return true;
      return false;
    }


    template<class Real>
    __device__ __host__ 
    inline bool checkRelativeError(Real a, Real b, Real epsilon)
    {
      if( fabs((a-b)/b)  < epsilon ) return true;
      return false;
    }
    



    // Compute the reciprocal square root of the matrix q
    // Also modify q if the eigenvalues are dangerously small.
    template<class Cmplx> 
    __device__  __host__ 
    void reciprocalRoot(Matrix<Cmplx,3>* res, DerivativeCoefficients<typename RealTypeId<Cmplx>::Type>* deriv_coeffs, 
			typename RealTypeId<Cmplx>::Type f[3], Matrix<Cmplx,3> & q, int *unitarization_failed){

      Matrix<Cmplx,3> qsq, tempq;

      typename RealTypeId<Cmplx>::Type c[3];
      typename RealTypeId<Cmplx>::Type g[3];

#ifdef __CUDA_ARCH__
#define REUNIT_SVD_ONLY DEV_REUNIT_SVD_ONLY
#else
#define REUNIT_SVD_ONLY HOST_REUNIT_SVD_ONLY
#endif
      if(!REUNIT_SVD_ONLY){
	qsq = q*q;
	tempq = qsq*q;

	c[0] = getTrace(q).x;
	c[1] = getTrace(qsq).x/2.0;
	c[2] = getTrace(tempq).x/3.0;

	g[0] = g[1] = g[2] = c[0]/3.;
	typename RealTypeId<Cmplx>::Type r,s,theta;
	s = c[1]/3. - c[0]*c[0]/18;
	r = c[2]/2. - (c[0]/3.)*(c[1] - c[0]*c[0]/9.);

#ifdef __CUDA_ARCH__
#define HISQ_UNITARIZE_EPS DEV_HISQ_UNITARIZE_EPS
#else
#define HISQ_UNITARIZE_EPS HOST_HISQ_UNITARIZE_EPS
#endif

	typename RealTypeId<Cmplx>::Type cosTheta = r/sqrt(s*s*s);
	if(fabs(s) < HISQ_UNITARIZE_EPS){
	  cosTheta = 1.;
	  s = 0.0; 
	}
	if(fabs(cosTheta)>1.0){ r>0 ? theta=0.0 : theta=HISQ_UNITARIZE_PI/3.0; }
	else{ theta = acos(cosTheta)/3.0; }

	s = 2.0*sqrt(s);
	for(int i=0; i<3; ++i){
	  g[i] += s*cos(theta + (i-1)*HISQ_UNITARIZE_PI23);
	}

      } // !REUNIT_SVD_ONLY?

	//
	// Compare the product of the eigenvalues computed thus far to the 
	// absolute value of the determinant. 
	// If the determinant is very small or the relative error is greater than some predefined value 
	// then recompute the eigenvalues using a singular-value decomposition.
	// Note that this particular calculation contains multiple branches, 
	// so it doesn't appear to be particularly well-suited to the GPU 
	// programming model. However, the analytic calculation of the 
	// unitarization is extremely fast, and if the SVD routine is not called 
	// too often, we expect pretty good performance.
	//

#ifdef __CUDA_ARCH__
#define REUNIT_ALLOW_SVD DEV_REUNIT_ALLOW_SVD
#define REUNIT_SVD_REL_ERROR DEV_REUNIT_SVD_REL_ERROR
#define REUNIT_SVD_ABS_ERROR DEV_REUNIT_SVD_ABS_ERROR
#else // cpu
#define REUNIT_ALLOW_SVD HOST_REUNIT_ALLOW_SVD
#define REUNIT_SVD_REL_ERROR HOST_REUNIT_SVD_REL_ERROR
#define REUNIT_SVD_ABS_ERROR HOST_REUNIT_SVD_ABS_ERROR
#endif

      if(REUNIT_ALLOW_SVD){
	bool perform_svd = true;
	if(!REUNIT_SVD_ONLY){
	  const typename RealTypeId<Cmplx>::Type det = getDeterminant(q).x;
	  if( fabs(det) >= REUNIT_SVD_ABS_ERROR){  
	    if( checkRelativeError(g[0]*g[1]*g[2],det,REUNIT_SVD_REL_ERROR) ) perform_svd = false;
	  }
	}	

	if(perform_svd){	
	  Matrix<Cmplx,3> tmp2;
	  // compute the eigenvalues using the singular value decomposition
	  computeSVD<Cmplx>(q,tempq,tmp2,g);
	  // The array g contains the eigenvalues of the matrix q
	  // The determinant is the product of the eigenvalues, and I can use this
	  // to check the SVD
	  const typename RealTypeId<Cmplx>::Type determinant = getDeterminant(q).x;
	  const typename RealTypeId<Cmplx>::Type gprod = g[0]*g[1]*g[2];
	  // Check the svd result for errors
#ifdef __CUDA_ARCH__
#define MAX_DET_ERROR DEV_MAX_DET_ERROR
#else
#define MAX_DET_ERROR HOST_MAX_DET_ERROR
#endif
	  if(fabs(gprod - determinant) > MAX_DET_ERROR){
	    printf("Warning: Error in determinant computed by SVD : %g > %g\n", fabs(gprod-determinant), MAX_DET_ERROR);
	    printLink(q);

#ifdef __CUDA_ARCH__
	    atomicAdd(unitarization_failed,1);
#else
	    (*unitarization_failed)++;
#endif
	  } 
	} // perform_svd?

      } // REUNIT_ALLOW_SVD?

#ifdef __CUDA_ARCH__
#define HISQ_FORCE_FILTER DEV_HISQ_FORCE_FILTER
#else
#define HISQ_FORCE_FILTER HOST_HISQ_FORCE_FILTER
#endif	
      typename RealTypeId<Cmplx>::Type delta = getAbsMin(g,3);
      if(delta < HISQ_FORCE_FILTER){
	for(int i=0; i<3; ++i){ 
	  g[i]     += HISQ_FORCE_FILTER; 
	  q(i,i).x += HISQ_FORCE_FILTER;
	}
	qsq = q*q; // recalculate Q^2
      }


      // At this point we have finished with the c's 
      // use these to store sqrt(g)
      for(int i=0; i<3; ++i) c[i] = sqrt(g[i]);

      // done with the g's, use these to store u, v, w
      g[0] = c[0]+c[1]+c[2];
      g[1] = c[0]*c[1] + c[0]*c[2] + c[1]*c[2];
      g[2] = c[0]*c[1]*c[2];
        
      // set the derivative coefficients!
      deriv_coeffs->set(g[0], g[1], g[2]);

      const typename RealTypeId<Cmplx>::Type & denominator  = g[2]*(g[0]*g[1]-g[2]); 
      c[0] = (g[0]*g[1]*g[1] - g[2]*(g[0]*g[0]+g[1]))/denominator;
      c[1] = (-g[0]*g[0]*g[0] - g[2] + 2.*g[0]*g[1])/denominator;
      c[2] =  g[0]/denominator;

      tempq = c[1]*q + c[2]*qsq;
      // Add a real scalar
      tempq(0,0).x += c[0];
      tempq(1,1).x += c[0];
      tempq(2,2).x += c[0];

      f[0] = c[0];
      f[1] = c[1];
      f[2] = c[2];

      *res = tempq;
      return;
    }



    // "v" denotes a "fattened" link variable
    template<class Cmplx>
    __device__ __host__
    void getUnitarizeForceSite(const Matrix<Cmplx,3> & v, const Matrix<Cmplx,3> & outer_prod, Matrix<Cmplx,3>* result, int *unitarization_failed)
    {
      typename RealTypeId<Cmplx>::Type f[3]; 
      typename RealTypeId<Cmplx>::Type b[6];

      Matrix<Cmplx,3> v_dagger = conj(v);  // okay!
      Matrix<Cmplx,3> q   = v_dagger*v;    // okay!

      Matrix<Cmplx,3> rsqrt_q;

      DerivativeCoefficients<typename RealTypeId<Cmplx>::Type> deriv_coeffs;

      reciprocalRoot<Cmplx>(&rsqrt_q, &deriv_coeffs, f, q, unitarization_failed); // approx 529 flops (assumes no SVD)

      // Pure hack here
      b[0] = deriv_coeffs.getB00();
      b[1] = deriv_coeffs.getB01();
      b[2] = deriv_coeffs.getB02();
      b[3] = deriv_coeffs.getB11();
      b[4] = deriv_coeffs.getB12();
      b[5] = deriv_coeffs.getB22();


      Matrix<Cmplx,3> & local_result = *result;

      local_result = rsqrt_q*outer_prod;

      // We are now finished with rsqrt_q
      Matrix<Cmplx,3> qv_dagger  = q*v_dagger;
      Matrix<Cmplx,3> vv_dagger  = v*v_dagger; 
      Matrix<Cmplx,3> vqv_dagger = v*qv_dagger;
      Matrix<Cmplx,3> temp = f[1]*vv_dagger + f[2]*vqv_dagger;


      temp = f[1]*v_dagger + f[2]*qv_dagger;
      Matrix<Cmplx,3> conj_outer_prod = conj(outer_prod);


      temp = f[1]*v + f[2]*v*q;
      local_result = local_result + outer_prod*temp*v_dagger + f[2]*q*outer_prod*vv_dagger;

      local_result = local_result + v_dagger*conj_outer_prod*conj(temp) + f[2]*qv_dagger*conj_outer_prod*v_dagger;


      // now done with vv_dagger, I think
      Matrix<Cmplx,3> qsqv_dagger = q*qv_dagger;
      Matrix<Cmplx,3> pv_dagger   = b[0]*v_dagger + b[1]*qv_dagger + b[2]*qsqv_dagger;
      accumBothDerivatives(&local_result, v, pv_dagger, outer_prod); // 41 flops

      Matrix<Cmplx,3> rv_dagger = b[1]*v_dagger + b[3]*qv_dagger + b[4]*qsqv_dagger;
      Matrix<Cmplx,3> vq = v*q;
      accumBothDerivatives(&local_result, vq, rv_dagger, outer_prod); // 41 flops

      Matrix<Cmplx,3> sv_dagger = b[2]*v_dagger + b[4]*qv_dagger + b[5]*qsqv_dagger;
      Matrix<Cmplx,3> vqsq = vq*q;
      accumBothDerivatives(&local_result, vqsq, sv_dagger, outer_prod); // 41 flops
      return;
      // 4528 flops - 17 matrix multiplies (198 flops each) + reciprocal root (approx 529 flops) + accumBothDerivatives (41 each) + miscellaneous
    } // get unit force term



    template<class Cmplx>
    __global__ void getUnitarizeForceField(const int threads, const Cmplx* link_even, const Cmplx* link_odd,
					   const Cmplx* old_force_even, const Cmplx* old_force_odd,
					   Cmplx* force_even, Cmplx* force_odd,
					   int* unitarization_failed)
    {
       
      int mem_idx = blockIdx.x*blockDim.x + threadIdx.x;
      // The number of GPU threads is equal to the local volume
      const int HALF_VOLUME = threads/2;
      if(mem_idx >= threads) return;
	
      Cmplx* force;
      const Cmplx* link;
      const Cmplx* old_force;

      force = force_even;
      link = link_even;
      old_force = old_force_even;
      if(mem_idx >= HALF_VOLUME){
	      mem_idx = mem_idx - HALF_VOLUME;
	      force = force_odd;
	      link = link_odd;
	      old_force = old_force_odd;
      }


      // This part of the calculation is always done in double precision
      Matrix<double2,3> v, result, oprod;
           
      for(int dir=0; dir<4; ++dir){
	loadLinkVariableFromArray(old_force, dir, mem_idx, HALF_VOLUME, &oprod);
	loadLinkVariableFromArray(link, dir, mem_idx, HALF_VOLUME, &v);

	getUnitarizeForceSite<double2>(v, oprod, &result, unitarization_failed); 

	writeLinkVariableToArray(result, dir, mem_idx, HALF_VOLUME, force); 
      } // 4*4528 flops per site
      return;
    } // getUnitarizeForceField


    void unitarizeForceCPU(cpuGaugeField& cpuOldForce, cpuGaugeField& cpuGauge, cpuGaugeField* cpuNewForce)
    {
      
      int num_failures = 0;	
      Matrix<double2,3> old_force, new_force, v;

      // I can change this code to make it much more compact

      const QudaGaugeFieldOrder order = cpuGauge.Order();

      if(order == QUDA_MILC_GAUGE_ORDER){
        for(int i=0; i<cpuGauge.Volume(); ++i){
	  for(int dir=0; dir<4; ++dir){
	    if(cpuGauge.Precision() == QUDA_SINGLE_PRECISION){
	      copyArrayToLink(&old_force, ((float*)(cpuOldForce.Gauge_p()) + (i*4 + dir)*18)); 
	      copyArrayToLink(&v, ((float*)(cpuGauge.Gauge_p()) + (i*4 + dir)*18)); 
	      getUnitarizeForceSite<double2>(v, old_force, &new_force, &num_failures);
	      copyLinkToArray(((float*)(cpuNewForce->Gauge_p()) + (i*4 + dir)*18), new_force); 
	    }else if(cpuGauge.Precision() == QUDA_DOUBLE_PRECISION){
	      copyArrayToLink(&old_force, ((double*)(cpuOldForce.Gauge_p()) + (i*4 + dir)*18)); 
	      copyArrayToLink(&v, ((double*)(cpuGauge.Gauge_p()) + (i*4 + dir)*18)); 
	      getUnitarizeForceSite<double2>(v, old_force, &new_force, &num_failures);
	      copyLinkToArray(((double*)(cpuNewForce->Gauge_p()) + (i*4 + dir)*18), new_force); 
	    } // precision?
	  } // dir
        } // i
      }else if(order == QUDA_QDP_GAUGE_ORDER){
        for(int dir=0; dir<4; ++dir){
          for(int i=0; i<cpuGauge.Volume(); ++i){
	    if(cpuGauge.Precision() == QUDA_SINGLE_PRECISION){
	      copyArrayToLink(&old_force, ((float**)(cpuOldForce.Gauge_p()))[dir] + i*18);
	      copyArrayToLink(&v, ((float**)(cpuGauge.Gauge_p()))[dir] + i*18);
	      getUnitarizeForceSite<double2>(v, old_force, &new_force, &num_failures);
	      copyLinkToArray(((float**)(cpuNewForce->Gauge_p()))[dir] + i*18, new_force);
	    }else if(cpuGauge.Precision() == QUDA_DOUBLE_PRECISION){
	      copyArrayToLink(&old_force, ((double**)(cpuOldForce.Gauge_p()))[dir] + i*18);
	      copyArrayToLink(&v, ((double**)(cpuGauge.Gauge_p()))[dir] + i*18);
	      getUnitarizeForceSite<double2>(v, old_force, &new_force, &num_failures);
	      copyLinkToArray(((double**)(cpuNewForce->Gauge_p()))[dir] + i*18, new_force);
	    }
          }
        }
      }else{
        errorQuda("Only MILC and QDP gauge orders supported\n");
      }
      return;
    } // unitarize_force_cpu

    class UnitarizeForceCuda : public Tunable {
    private:
      const cudaGaugeField &oldForce;
      const cudaGaugeField &gauge;
      cudaGaugeField &newForce;
      int *fails;

      unsigned int sharedBytesPerThread() const { return 0; }
      unsigned int sharedBytesPerBlock(const TuneParam &) const { return 0; }

      // don't tune the grid dimension
      bool tuneGridDim() const { return false; }
      unsigned int minThreads() const { return gauge.Volume(); }

    public:
      UnitarizeForceCuda(const cudaGaugeField& oldForce, const cudaGaugeField& gauge,  
			 cudaGaugeField& newForce, int* fails) : 
	oldForce(oldForce), gauge(gauge), newForce(newForce), fails(fails) { 
	writeAuxString("threads=%d,prec=%lu,stride=%d", 
		       gauge.Volume(), gauge.Precision(), gauge.Stride());
      }
      virtual ~UnitarizeForceCuda() { ; }

      void apply(const cudaStream_t &stream) {
	TuneParam tp = tuneLaunch(*this, getTuning(), getVerbosity());

	if(gauge.Precision() == QUDA_SINGLE_PRECISION){
	  getUnitarizeForceField<<<tp.grid,tp.block>>>(gauge.Volume(), (const float2*)gauge.Even_p(), (const float2*)gauge.Odd_p(),
						       (const float2*)oldForce.Even_p(), (const float2*)oldForce.Odd_p(),
						       (float2*)newForce.Even_p(), (float2*)newForce.Odd_p(), 
						       fails);
	}else if(gauge.Precision() == QUDA_DOUBLE_PRECISION){
	  getUnitarizeForceField<<<tp.grid,tp.block>>>(gauge.Volume(), (const double2*)gauge.Even_p(), (const double2*)gauge.Odd_p(),
						       (const double2*)oldForce.Even_p(), (const double2*)oldForce.Odd_p(),
						       (double2*)newForce.Even_p(), (double2*)newForce.Odd_p(), 
						       fails);      
	}
      }
      
      void preTune() { ; }
      void postTune() { cudaMemset(fails, 0, sizeof(int)); } // reset fails counter
      
      long long flops() const { return 4ll*4528*gauge.Volume(); }
      
      TuneKey tuneKey() const { return TuneKey(gauge.VolString(), typeid(*this).name(), aux); }
    }; // UnitarizeForceCuda

    void unitarizeForceCuda(cudaGaugeField &cudaOldForce,
                            cudaGaugeField &cudaGauge, cudaGaugeField *cudaNewForce, int* unitarization_failed, long long *flops) {

      UnitarizeForceCuda unitarizeForce(cudaOldForce, cudaGauge, *cudaNewForce, unitarization_failed);
      unitarizeForce.apply(0);
      cudaDeviceSynchronize(); // need to synchronize to ensure failure write has completed
      if(flops) *flops = unitarizeForce.flops(); 
      checkCudaError();
    }
    
    
  } // namespace fermion_force

//#endif
} // namespace quda


#endif
