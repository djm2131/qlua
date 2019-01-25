#ifndef _CONVERT_H
#define _CONVERT_H

/**
 * @file convert.h
 *
 * @section DESCRIPTION 
 * Conversion functions that are used as building blocks for
 * arbitrary field and register ordering.
 */

#include <quda_internal.h> // for MAX_SHORT

template <typename type> int vecLength() { return 0; }

template<> int vecLength<short>() { return 1; }
template<> int vecLength<float>() { return 1; }
template<> int vecLength<double>() { return 1; }

template<> int vecLength<short2>() { return 2; }
template<> int vecLength<float2>() { return 2; }
template<> int vecLength<double2>() { return 2; }

template<> int vecLength<short4>() { return 4; }
template<> int vecLength<float4>() { return 4; }
template<> int vecLength<double4>() { return 4; }

// MAX_SHORT 32767
#define MAX_SHORT_INV 3.051850948e-5
static inline __device__ float s2f(const short &a) { return static_cast<float>(a) * MAX_SHORT_INV; }
static inline __device__ float s2d(const short &a) { return static_cast<double>(a) * MAX_SHORT_INV; }

template <typename FloatN>
__device__ inline void copyFloatN(FloatN &a, const FloatN &b) { a = b; }

// This is emulating the texture normalized return
__device__ inline void copyFloatN(float2 &a, const short2 &b) { a = make_float2(s2f(b.x), s2f(b.y)); }
__device__ inline void copyFloatN(float4 &a, const short4 &b) { a = make_float4(s2f(b.x), s2f(b.y), s2f(b.z), s2f(b.w)); }
__device__ inline void copyFloatN(double2 &a, const short2 &b) { a = make_double2(s2d(b.x), s2d(b.y)); }
__device__ inline void copyFloatN(double4 &a, const short4 &b) { a = make_double4(s2d(b.x), s2d(b.y), s2d(b.z), s2d(b.w)); }

__device__ inline void copyFloatN(float2 &a, const double2 &b) { a = make_float2(b.x, b.y); }
__device__ inline void copyFloatN(double2 &a, const float2 &b) { a = make_double2(b.x, b.y); }
__device__ inline void copyFloatN(float4 &a, const double4 &b) { a = make_float4(b.x, b.y, b.z, b.w); }
__device__ inline void copyFloatN(double4 &a, const float4 &b) { a = make_double4(b.x, b.y, b.z, b.w); }

/* Here we assume that the input data has already been normalized and shifted. */
__device__ inline void copyFloatN(short2 &a, const float2 &b) { a = make_short2(b.x, b.y); }
__device__ inline void copyFloatN(short4 &a, const float4 &b) { a = make_short4(b.x, b.y, b.z, b.w); }
__device__ inline void copyFloatN(short2 &a, const double2 &b) { a = make_short2(b.x, b.y); }
__device__ inline void copyFloatN(short4 &a, const double4 &b) { a = make_short4(b.x, b.y, b.z, b.w); }


/**
 Convert a vector of type InputType to type OutputType.

 The main current limitation is that there is an implicit assumption
 that N * sizeof(OutputType) / sizeof(InputType) is an integer.  E.g.,
 you cannot convert a vector 9 float2s into a vector of 5 float4s.

 @param x Output vector.
 @param y Input vector.
 @param N Length of output vector.
 */
template<typename OutputType, typename InputType>
__device__ inline void convert(OutputType x[], InputType y[], const int N) {
  // default is one-2-one conversion, e.g., matching vector lengths and precisions
#pragma unroll
  for (int j=0; j<N; j++) copyFloatN(x[j], y[j]);
}

template<> __device__ inline void convert<float2,short2>(float2 x[], short2 y[], const int N) {
#pragma unroll
  for (int j=0; j<N; j++) x[j] = make_float2(y[j].x, y[j].y);
}

template<> __device__ inline void convert<float4,short4>(float4 x[], short4 y[], const int N) {
#pragma unroll
  for (int j=0; j<N; j++) x[j] = make_float4(y[j].x, y[j].y, y[j].z, y[j].w);
}

// 4 <-> 2 vector conversion

template<> __device__ inline void convert<double4,double2>(double4 x[], double2 y[], const int N) {
#pragma unroll
  for (int j=0; j<N; j++) x[j] = make_double4(y[2*j].x, y[2*j].y, y[2*j+1].x, y[2*j+1].y);
}

template<> __device__ inline void convert<double2,double4>(double2 x[], double4 y[], const int N) {
#pragma unroll
  for (int j=0; j<N/2; j++) {
    x[2*j] = make_double2(y[j].x, y[j].y);
    x[2*j+1] = make_double2(y[j].z, y[j].w);
  }
}

template<> __device__ inline void convert<float4,float2>(float4 x[], float2 y[], const int N) {
#pragma unroll
  for (int j=0; j<N; j++) x[j] = make_float4(y[2*j].x, y[2*j].y, y[2*j+1].x, y[2*j+1].y);
}

template<> __device__ inline void convert<float2,float4>(float2 x[], float4 y[], const int N) {
#pragma unroll
  for (int j=0; j<N/2; j++) {
    x[2*j] = make_float2(y[j].x, y[j].y);
    x[2*j+1] = make_float2(y[j].z, y[j].w);
  }
}

template<> __device__ inline void convert<short4,float2>(short4 x[], float2 y[], const int N) {
#pragma unroll
  for (int j=0; j<N; j++) x[j] = make_short4(y[2*j].x, y[2*j].y, y[2*j+1].x, y[2*j+1].y);
}

template<> __device__ inline void convert<float2,short4>(float2 x[], short4 y[], const int N) {
#pragma unroll
  for (int j=0; j<N/2; j++) {
    x[2*j] = make_float2(y[j].x, y[j].y);
    x[2*j+1] = make_float2(y[j].z, y[j].w);
  }
}

template<> __device__ inline void convert<float4,short2>(float4 x[], short2 y[], const int N) {
#pragma unroll
  for (int j=0; j<N; j++) x[j] = make_float4(y[2*j].x, y[2*j].y, y[2*j+1].x, y[2*j+1].y);
}

template<> __device__ inline void convert<short2,float4>(short2 x[], float4 y[], const int N) {
#pragma unroll
  for (int j=0; j<N/2; j++) {
    x[2*j] = make_short2(y[j].x, y[j].y);
    x[2*j+1] = make_short2(y[j].z, y[j].w);
  }
}

template<> __device__ inline void convert<short4,double2>(short4 x[], double2 y[], const int N) {
#pragma unroll
  for (int j=0; j<N; j++) x[j] = make_short4(y[2*j].x, y[2*j].y, y[2*j+1].x, y[2*j+1].y);
}

template<> __device__ inline void convert<double2,short4>(double2 x[], short4 y[], const int N) {
#pragma unroll
  for (int j=0; j<N/2; j++) {
    x[2*j] = make_double2(y[j].x, y[j].y);
    x[2*j+1] = make_double2(y[j].z, y[j].w);
  }
}

template<> __device__ inline void convert<double4,short2>(double4 x[], short2 y[], const int N) {
#pragma unroll
  for (int j=0; j<N; j++) x[j] = make_double4(y[2*j].x, y[2*j].y, y[2*j+1].x, y[2*j+1].y);
}

template<> __device__ inline void convert<short2,double4>(short2 x[], double4 y[], const int N) {
#pragma unroll
  for (int j=0; j<N/2; j++) {
    x[2*j] = make_short2(y[j].x, y[j].y);
    x[2*j+1] = make_short2(y[j].z, y[j].w);
  }
}

template<> __device__ inline void convert<float4,double2>(float4 x[], double2 y[], const int N) {
#pragma unroll
  for (int j=0; j<N; j++) x[j] = make_float4(y[2*j].x, y[2*j].y, y[2*j+1].x, y[2*j+1].y);
}

template<> __device__ inline void convert<double2,float4>(double2 x[], float4 y[], const int N) {
#pragma unroll
  for (int j=0; j<N/2; j++) {
    x[2*j] = make_double2(y[j].x, y[j].y);
    x[2*j+1] = make_double2(y[j].z, y[j].w);
  }
}

template<> __device__ inline void convert<double4,float2>(double4 x[], float2 y[], const int N) {
#pragma unroll
  for (int j=0; j<N; j++) x[j] = make_double4(y[2*j].x, y[2*j].y, y[2*j+1].x, y[2*j+1].y);
}

template<> __device__ inline void convert<float2,double4>(float2 x[], double4 y[], const int N) {
#pragma unroll
  for (int j=0; j<N/2; j++) {
    x[2*j] = make_float2(y[j].x, y[j].y);
    x[2*j+1] = make_float2(y[j].z, y[j].w);
  }
}

#endif // _CONVERT_H
