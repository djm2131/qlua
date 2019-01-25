struct QX(Fermion);
struct QX(HalfFermion);
struct QX(HalfFermionMat);
struct QX(VectorFermion);
struct QX(Gauge);
struct qx(MxM_workspace);

#include "deflator-x.h"

/* MDWF types */
struct QX(Fermion) {
  struct Q(State) *state;
  size_t size;
  struct Fermion *cb_x;  
  struct Fermion *cb_y;
};

struct QX(HalfFermion) {
  struct Q(State) *state;
  size_t size;
  struct Fermion *cb_x;  
};
struct QX(HalfFermionMat) {
  struct Q(State) *state;
  size_t mem_size;
  qx(defl_mat) m;
};

struct QX(VectorFermion) {
  struct Q(State) *state;
  size_t size;
  int    count;
  struct VectorFermion *cb_x;
};

struct QX(Gauge) {
  struct Q(State) *state;
  size_t size;
  struct SUn *data;
};

struct qx(MxM_workspace) {
    struct Q(State)             *state;
    const struct Q(Parameters)  *params;
    const struct SUn            *gauge;
    struct Fermion              *tmp_x;
    struct Fermion              *tmp2_x;
    struct Fermion              *tmp_y;
    long long                   *flops;
    long long                   *sent;
    long long                   *received;
};

void *qx(allocate_eo_)(struct Q(State) *state,
                      size_t *size, void **aligned_ptr,
                      size_t hdr_size, int even_count, int odd_count);
void *qx(step_even_)(struct Q(State) *state, void *aligned_ptr);
void *qx(step_odd_)(struct Q(State) *state, void *aligned_ptr);

void *qx(allocate_xy_)(struct Q(State) *state, 
                size_t *size, void **aligned_ptr,
                size_t hdr_size, int x_count, int y_count);
void *qx(step_cb_x)(struct Q(State) *state, void *aligned_ptr);
void *qx(step_cb_y)(struct Q(State) *state, void *aligned_ptr);

void qx(x_import)(struct eo_lattice *eo,
                  double r[],
                  struct Fermion *data, 
                  void (*reader)(double *val_re,
                                 double *val_im,
                                 const int pos[Q(DIM)+1],
                                 int color,
                                 int dirac, 
                                 void *env),
                  void *env);
void qx(x4_import)(struct eo_lattice *eo,
                   double r[],
                   struct Fermion *data, 
                   void (*reader)(double *val_re,
                                  double *val_im,
                                  const int pos[Q(DIM)+1],
                                  int color,
                                  int dirac, 
                                  void *env),
                  void *env);
void qx(x_export)(struct eo_lattice *eo,
                  double r[],
                  const struct Fermion *data, 
                  void (*writer)(const int pos[Q(DIM)+1],
                                 int color,
                                 int dirac, 
                                 double val_re,
                                 double val_im,
                                 void *env),
                  void *env);
void qx(x4_export)(struct eo_lattice *eo,
                   double r[],
                   const struct Fermion *data, 
                   void (*writer)(const int pos[Q(DIM)],
                                  int color,
                                  int dirac, 
                                  double val_re,
                                  double val_im,
                                  void *env),
                   void *env);
void qx(x_midpoint)(struct eo_lattice *eo,
                    double r[],
                    const struct Fermion *data,
                    void (*writer)(const int pos[Q(DIM)],
                                   double value,
                                   void *env),
                    void *env);

/* Projections */
typedef size_t (*qx(Up_project))(struct ProjectedFermion *r,
                                       size_t start, size_t size, size_t Ls,
                                       const struct up_pack *link,
                                       const struct SUn *U,
                                       const struct Fermion *f);
typedef size_t (*qx(Down_project))(struct ProjectedFermion *r,
                                         size_t start, size_t size, size_t Ls,
                                         const struct down_pack *link,
                                         const struct Fermion *f);
size_t qx(proj_g0plus)(struct ProjectedFermion *r,
                             size_t start, size_t size, size_t Ls,
                             const struct down_pack *link,
                             const struct Fermion *f);
size_t qx(proj_g1plus)(struct ProjectedFermion *r,
                             size_t start, size_t size, size_t Ls,
                             const struct down_pack *link,
                             const struct Fermion *f);
size_t qx(proj_g2plus)(struct ProjectedFermion *r,
                             size_t start, size_t size, size_t Ls,
                             const struct down_pack *link,
                             const struct Fermion *f);
size_t qx(proj_g3plus)(struct ProjectedFermion *r,
                             size_t start, size_t size, size_t Ls,
                             const struct down_pack *link,
                             const struct Fermion *f);
size_t qx(proj_g0minus)(struct ProjectedFermion *r,
                              size_t start, size_t size, size_t Ls,
                              const struct down_pack *link,
                              const struct Fermion *f);
size_t qx(proj_g1minus)(struct ProjectedFermion *r,
                              size_t start, size_t size, size_t Ls,
                              const struct down_pack *link,
                              const struct Fermion *f);
size_t qx(proj_g2minus)(struct ProjectedFermion *r,
                              size_t start, size_t size, size_t Ls,
                              const struct down_pack *link,
                              const struct Fermion *f);
size_t qx(proj_g3minus)(struct ProjectedFermion *r,
                              size_t start, size_t size, size_t Ls,
                              const struct down_pack *link,
                              const struct Fermion *f);
size_t qx(proj_Ucg0plus)(struct ProjectedFermion *r,
                               size_t start, size_t size, size_t Ls,
                               const struct up_pack *link,
                               const struct SUn *U,
                               const struct Fermion *f);
size_t qx(proj_Ucg1plus)(struct ProjectedFermion *r,
                               size_t start, size_t size, size_t Ls,
                               const struct up_pack *link,
                               const struct SUn *U,
                               const struct Fermion *f);
size_t qx(proj_Ucg2plus)(struct ProjectedFermion *r,
                               size_t start, size_t size, size_t Ls,
                               const struct up_pack *link,
                               const struct SUn *U,
                               const struct Fermion *f);
size_t qx(proj_Ucg3plus)(struct ProjectedFermion *r,
                               size_t start, size_t size, size_t Ls,
                               const struct up_pack *link,
                               const struct SUn *U,
                               const struct Fermion *f);
size_t qx(proj_Ucg0minus)(struct ProjectedFermion *r,
                                size_t start, size_t size, size_t Ls,
                                const struct up_pack *link,
                                const struct SUn *U,
                                const struct Fermion *f);
size_t qx(proj_Ucg1minus)(struct ProjectedFermion *r,
                                size_t start, size_t size, size_t Ls,
                                const struct up_pack *link,
                                const struct SUn *U,
                                const struct Fermion *f);
size_t qx(proj_Ucg2minus)(struct ProjectedFermion *r,
                                size_t start, size_t size, size_t Ls,
                                const struct up_pack *link,
                                const struct SUn *U,
                                const struct Fermion *f);
size_t qx(proj_Ucg3minus)(struct ProjectedFermion *r,
                                size_t start, size_t size, size_t Ls,
                                const struct up_pack *link,
                                const struct SUn *U,
                                const struct Fermion *f);


/* projection tables */
/*  normal projection */
extern qx(Up_project) qx(up_project_n)[Q(DIM)];
extern qx(Down_project) qx(down_project_n)[Q(DIM)];
/*  conjugated projection */
extern qx(Up_project) qx(up_project_x)[Q(DIM)];
extern qx(Down_project) qx(down_project_x)[Q(DIM)];
/* compute projections on the boundary and fill the send buffers */
void qx(boundary)(struct eo_lattice *xy,
                  size_t Ls,
                  const qx(Up_project) up_proj[],
                  const qx(Down_project) down_proj[],
                  const struct SUn *U,
                  const struct Fermion *src_y,
                  long long *flops);
/* same as above by do the down boundary only */
void qx(down_boundary)(struct eo_lattice *xy,
                       size_t Ls,
                       const qx(Down_project) down_proj[],
                       const struct Fermion *src_y,
                       long long *flops);
size_t qx(sizeof_fermion)(size_t volume, size_t Ls);
size_t qx(sizeof_projected_fermion)(size_t volume, size_t Ls);
size_t qx(sizeof_gauge)(size_t volume);
size_t qx(sizeof_vfermion)(size_t volume, size_t Ls, size_t count);
size_t qx(strideof_vfermion)(size_t volume, size_t Ls);

/* qa0 level data access routines */
void qx(put_gauge)(struct SUn *ptr, size_t pos, const double r[]);
void qx(put_fermion)(struct Fermion *data, size_t pos, size_t Ls, const double r[]);
void qx(get_fermion)(double r[], const struct Fermion *data, size_t pos, size_t Ls);
void qx(fermion2blas)(void *data, const struct Fermion *f, size_t start, size_t size, size_t Ls);
void qx(blas2fermion)(struct Fermion *f, size_t start, size_t size, size_t Ls, const void *data);
/* Linear algebra on fermions */
void qx(f_zero)(struct Fermion *dst, 
                size_t start, size_t size, size_t Ls);
void qx(f_copy)(struct Fermion *dst, 
                size_t start, size_t size, size_t Ls,
                const struct Fermion *src);
size_t qx(f_dot)(double *v_r, double *v_i,
                       size_t start, size_t size, size_t Ls,
                       const struct Fermion *a,
                       const struct Fermion *b);
size_t qx(f_add3)(struct Fermion *r,
                        size_t start, size_t size, size_t Ls,
                        const struct Fermion *a,
                        double s,
                        const struct Fermion *b);
size_t qx(f_add2)(struct Fermion *r,
                        size_t start, size_t size, size_t Ls,
                        double s,
                        const struct Fermion *b);
size_t qx(f_cadd2)(struct Fermion *r,
                         size_t start, size_t size, size_t Ls,
                         double sr, double si,
                         const struct Fermion *b);
size_t qx(f_add2_norm)(struct Fermion *r,
                             double *local_norm,
                             size_t start, size_t size, size_t Ls,
                             double s,
                             const struct Fermion *b);
size_t qx(f_rmul1)(struct Fermion *r,
                         size_t start, size_t size, size_t Ls,
                         double s);
size_t qx(f_add2x)(struct Fermion *r,
                         size_t start, size_t size, size_t Ls,
                         double s,
                         const struct Fermion *b);
size_t qx(f_norm)(double *s,
                        size_t start, size_t size, size_t Ls,
                        const struct Fermion *a);
size_t qx(f_diff_norm)(double *s,
                             size_t start, size_t size, size_t Ls,
                             const struct Fermion *a,
                             const struct Fermion *b);
void qx(fv_zero)(struct VectorFermion *vf,
                 size_t start, size_t size, size_t Ls, size_t count);
void qx(fv_copy)(struct VectorFermion *vf,
                 size_t start, size_t size, size_t Ls, size_t count,
                 const struct Fermion *f);
void qx(fv_get)(struct Fermion *f,
                size_t start, size_t size, size_t Ls, size_t count,
                const struct VectorFermion *vf, size_t k);
void qx(fv_put)(struct VectorFermion *vf, size_t k,
                size_t start, size_t size, size_t Ls, size_t count,
                const struct Fermion *f);

/* algebra for arrays of fermions */

/* fv[fv_begin + (0 .. len-1)] = gv[gv_begin + (0 .. len-1)]
*/
size_t qx(vf_copy)(size_t start, size_t size, size_t Ls, size_t len,
                         struct vFermion *fv, size_t fv_stride, size_t fv_begin,
                         const struct vFermion *gv, size_t gv_stride, size_t gv_begin);
/*
 * set fv[idx] = x
*/
size_t qx(vf_put)(size_t start, size_t size, size_t Ls,
                        struct vFermion *fv, size_t fv_stride, size_t fv_idx,
                        const struct Fermion *x);

/*
 * read x = fv[idx]
*/
size_t qx(vf_get)(size_t start, size_t size, size_t Ls,
                        struct Fermion *x,
                        const struct vFermion *fv, size_t fv_stride, size_t fv_idx);

/*
*   g = fv[fv_begin + (0 .. f_vlen-1)] . v
*   v is a complex vector [fv_len] indexed as [re:0/im:1 + 2 * i]
*/
size_t qx(vf_dot_vz)(size_t start, size_t size, size_t Ls,
                           struct Fermion *g,
                           const struct vFermion *fv,
                           size_t fv_stride, size_t fv_begin, size_t fv_len,
                           const double *v);

/*
*   gv[gv_begin + (0 .. gv_len-1)] = fv[fv_begin + (0 .. f_len - 1)] . m
*   m is a complex matrix [fv_len*gv_len] indexed as [re:0/im:1 + 2 * (row + ldm * col) ]
*/
size_t qx(vf_dot_mz)(size_t start, size_t size, size_t Ls,
                           struct vFermion *gv,
                           size_t gv_stride, size_t gv_begin, size_t gv_len,
                           const struct vFermion *fv,
                           size_t fv_stride, size_t fv_begin, size_t fv_len,
                           const double *m, size_t ldm);

/*  This does not include global reduction
 *  c[i] = herm(fv[fv_begin+i]) * g 
 *      for all i = (0 .. fv_len-1)
 *  c is complex vector as [re:0/im:1 + 2 * i]
 */
size_t qx(do_vfH_dot_f)(size_t start, size_t size, size_t Ls,
                              double *c,
                              const struct vFermion *fv,
                              size_t fv_stride, size_t fv_begin, size_t fv_len,
                              const struct Fermion *g);

/* This does not include global reduction
 * c[i,j] = herm(fv[fv_begin + i]) . g[gv_begin+j] 
 *      for all i = (0 .. fv_len-1), 
 *              j = (0 .. gv_len-1),
 * c is a complex matrix as [re:0/im:1 + 2 * (i + ldc * j)]
 */
size_t qx(do_vfH_dot_vf)(size_t start, size_t size, size_t Ls,
                               double *c, size_t ldc,
                               const struct vFermion *fv,
                               size_t fv_stride, size_t fv_begin, size_t fv_len,
                               const struct vFermion *gv,
                               size_t gv_stride, size_t gv_begin, size_t gv_len);

/* basic matrices */
size_t qx(op_norm2)(double *global_norm,
                          const struct QX(Fermion) *psi,
                          struct Q(State) *state);
size_t qx(do_A)(struct Fermion *r_x,
                      size_t start, size_t size, size_t Ls,
                      const struct ABTable *aptable,
                      const struct ABTable *amtable,
                      const struct Fermion *s_x);
size_t qx(do_A_conj)(struct Fermion *r_x,
                           size_t start, size_t size, size_t Ls,
                           const struct ABTable *axptable,
                           const struct ABTable *axmtable,
                           const struct Fermion *s_x);
size_t qx(do_A_inverse)(struct Fermion *r,
                              size_t start, size_t size, size_t Ls,
                              const struct ABiTable *iatable_p,
                              const struct ABiTable *iatable_m,
                              const struct Fermion *x);
size_t qx(do_A_conj_inverse)(struct Fermion *r,
                                   size_t start, size_t size, size_t Ls,
                                   const struct ABiTable *iatable_p,
                                   const struct ABiTable *iatable_m,
                                   const struct Fermion *x);
size_t qx(doc_A)(struct Fermion *r_x,
                       size_t start, size_t size, size_t Ls,
                       const struct ABTable *aptable,
                       const struct ABTable *amtable,
                       const struct Fermion *s_x);
size_t qx(doc_A_conj)(struct Fermion *r_x,
                            size_t start, size_t size, size_t Ls,
                            const struct ABTable *axptable,
                            const struct ABTable *axmtable,
                            const struct Fermion *s_x);
size_t qx(doc_A_inverse)(struct Fermion *r,
                               size_t start, size_t size, size_t Ls,
                               const struct ABiTable *iatable_p,
                               const struct ABiTable *iatable_m,
                               const struct Fermion *x);
size_t qx(doc_A_conj_inverse)(struct Fermion *r,
                                    size_t start, size_t size, size_t Ls,
                                    const struct ABiTable *iatable_p,
                                    const struct ABiTable *iatable_m,
                                    const struct Fermion *x);
size_t qx(do_F)(struct Fermion *res_x,
                      size_t start, size_t size, size_t Ls,
                      const struct neighbor *neighbor,
                      const struct SUn *U,
                      const struct Fermion *src_y,
                      void *rb[]);
size_t qx(do_F_conj)(struct Fermion *res_x,
                           size_t start, size_t size, size_t Ls,
                           const struct neighbor *neighbor,
                           const struct SUn *U,
                           const struct Fermion *src_y,
                           void *rb[]);

/* basic axial current */
size_t qx(do_axial_current)(double *val, /* [ 2 * Q(DIM) ] */
                                  size_t p,
                                  size_t Ls,
                                  const struct neighbor *neighbor,
                                  const struct SUn *U,
                                  const struct Fermion *s_x,
                                  const struct Fermion *s_y,
                                  void *rv[]);
/* basic A+F, A and B */
size_t qx(do_ApF)(struct Fermion *r_x,
                        size_t start, size_t size, size_t Ls,
                        const struct ABTable *aptable,
                        const struct ABTable *amtable,
                        const struct neighbor *neighbor,
                        const struct SUn *U,
                        const struct Fermion *s_x,
                        const struct Fermion *s_y,
                        void *rb[]);
size_t qx(do_ApF_norm)(struct Fermion *r_x,
                             double *local_norm,
                             size_t start, size_t size, size_t Ls,
                             const struct ABTable *aptable,
                             const struct ABTable *amtable,
                             const struct neighbor *neighbor,
                             const struct SUn *U,
                             const struct Fermion *s_x,
                             const struct Fermion *s_y,
                             void *rb[]);
size_t qx(do_AxpBxFx)(struct Fermion *r_x,
                            size_t start, size_t size, size_t Ls,
                            const struct ABTable *aptable,
                            const struct ABTable *amtable,
                            const struct ABTable *bptable,
                            const struct ABTable *bmtable,
                            const struct neighbor *neighbor,
                            const struct SUn *U,
                            const struct Fermion *s_x,
                            const struct Fermion *s_y,
                            void *rb[]);
size_t qx(do_BA1)(struct Fermion *r_x,
                        size_t start, size_t size, size_t Ls,
                        const struct ABTable *bptable,
                        const struct ABTable *bmtable,
                        const struct ABiTable *iatable_p,
                        const struct ABiTable *iatable_m,
                        const struct Fermion *s_x);
size_t qx(do_BA1K)(struct Fermion *r_x,
			 size_t start, size_t size, size_t Ls,
			 const struct ABTable *bptable,
			 const struct ABTable *bmtable,
			 const struct ABiTable *iatable_p,
			 const struct ABiTable *iatable_m,
			 const struct KTable *ktable,
			 const struct Fermion *s_x);
size_t qx(doc_BA1K)(struct Fermion *r_x,
			  size_t start, size_t size, size_t Ls,
			  const struct ABTable *bptable,
			  const struct ABTable *bmtable,
			  const struct ABiTable *iatable_p,
			  const struct ABiTable *iatable_m,
			  const struct KTable *ktable,
			  const struct Fermion *s_x);
size_t qx(do_K)(struct Fermion *r_x,
		      size_t start, size_t size, size_t Ls,
		      const struct KTable *ktable,
		      const struct Fermion *s_x);
size_t qx(do_BA1F)(struct Fermion *r_y,
                         size_t start, size_t size, size_t Ls,
                         const struct ABTable *bptable,
                         const struct ABTable *bmtable,
                         const struct ABiTable *iatable_p,
                         const struct ABiTable *iatable_m,
                         const struct neighbor *neighbor,
                         const struct SUn *U,
                         const struct Fermion *s_x,
                         void *rb[]);
size_t qx(do_1mBA1F)(struct Fermion *r_y,
                           size_t start, size_t size, size_t Ls,
                           const struct ABTable *bptable,
                           const struct ABTable *bmtable,
                           const struct ABiTable *iatable_p,
                           const struct ABiTable *iatable_m,
                           const struct neighbor *neighbor,
                           const struct SUn *U,
                           const struct Fermion *a_y,
                           const struct Fermion *b_x,
                           void *rb[]);
size_t qx(do_1mK1xA1xBxFx)(struct Fermion *r_y,
				 size_t start, size_t size, size_t Ls,
				 const struct KTable *ktable,
				 const struct ABiTable *iatable_p,
				 const struct ABiTable *iatable_m,
				 const struct ABTable *bptable,
				 const struct ABTable *bmtable,
				 const struct neighbor *neighbor,
				 const struct SUn *U,
				 const struct Fermion *a_y,
				 const struct Fermion *b_x,
				 void *rb[]);
size_t qx(doc_1mK1xA1xBxFx)(struct Fermion *r_y,
				  size_t start, size_t size, size_t Ls,
				  const struct KTable *ktable,
				  const struct ABiTable *iatable_p,
				  const struct ABiTable *iatable_m,
				  const struct ABTable *bptable,
				  const struct ABTable *bmtable,
				  const struct neighbor *neighbor,
				  const struct SUn *U,
				  const struct Fermion *a_y,
				  const struct Fermion *b_x,
				  void *rb[]);
size_t qx(do_1mBA1F_norm)(struct Fermion *r_y,
                                double *local_norm,
                                size_t start, size_t size, size_t Ls,
                                const struct ABTable *bptable,
                                const struct ABTable *bmtable,
                                const struct ABiTable *iatable_p,
                                const struct ABiTable *iatable_m,
                                const struct neighbor *neighbor,
                                const struct SUn *U,
                                const struct Fermion *a_y,
                                const struct Fermion *b_x,
                                void *rb[]);
size_t qx(do_1mK1xA1xBxFx_norm)(struct Fermion *r_y,
				     double *local_norm,
				     size_t start, size_t size, size_t Ls,
				     const struct KTable *ktable,
				     const struct ABiTable *iatable_p,
				     const struct ABiTable *iatable_m,
				     const struct ABTable *bptable,
				     const struct ABTable *bmtable,
				     const struct neighbor *neighbor,
				     const struct SUn *U,
				     const struct Fermion *a_y,
				     const struct Fermion *b_x,
				     void *rb[]);
size_t qx(doc_1mK1xA1xBxFx_norm)(struct Fermion *r_y,
				      double *local_norm,
				      size_t start, size_t size, size_t Ls,
				      const struct KTable *ktable,
				      const struct ABiTable *iatable_p,
				      const struct ABiTable *iatable_m,
				      const struct ABTable *bptable,
				      const struct ABTable *bmtable,
				      const struct neighbor *neighbor,
				      const struct SUn *U,
				      const struct Fermion *a_y,
				      const struct Fermion *b_x,
				      void *rb[]);
size_t qx(do_A1xBx)(struct Fermion *r_y,
                          size_t start, size_t size, size_t Ls,
                          const struct ABTable *bptable,
                          const struct ABTable *bmtable,
                          const struct ABiTable *iatable_p,
                          const struct ABiTable *iatable_m,
                          const struct Fermion *b_y);
size_t qx(do_A1xBxFx)(struct Fermion *r_x,
                            size_t start, size_t size, size_t Ls,
                            const struct ABiTable *aiptable,
                            const struct ABiTable *aimtable,
                            const struct ABTable *bptable,
                            const struct ABTable *bmtable,
                            const struct neighbor *neighbor,
                            const struct SUn *U,
                            const struct Fermion *s_y,
                            void *rb[]);
size_t qx(do_1mF)(struct Fermion *r_y,
                        size_t start, size_t size, size_t Ls,
                        const struct neighbor *neighbor,
                        const struct SUn *U,
                        const struct Fermion *a_y,
                        const struct Fermion *b_x,
                        void *rb[]);
size_t qx(do_1mKF)(struct Fermion *r_y,
			 size_t start, size_t size, size_t Ls,
			 const struct KTable *ktable,
			 const struct neighbor *neighbor,
			 const struct SUn *U,
			 const struct Fermion *a_y,
			 const struct Fermion *b_x,
			 void *rb[]);
size_t qx(doc_1mKF)(struct Fermion *r_y,
			  size_t start, size_t size, size_t Ls,
			  const struct KTable *ktable,
			  const struct neighbor *neighbor,
			  const struct SUn *U,
			  const struct Fermion *a_y,
			  const struct Fermion *b_x,
			  void *rb[]);
size_t qx(do_1mFx)(struct Fermion *r_y,
                         size_t start, size_t size, size_t Ls,
                         const struct neighbor *neighbor,
                         const struct SUn *U,
                         const struct Fermion *a_y,
                         const struct Fermion *b_x,
                         void *rb[]);
size_t qx(do_1mFx_norm)(struct Fermion *r_y,
                              double *local_norm,
                              size_t start, size_t size, size_t Ls,
                              const struct neighbor *neighbor,
                              const struct SUn *U,
                              const struct Fermion *a_y,
                              const struct Fermion *b_x,
                              void *rb[]);
size_t qx(do_1mKF_norm)(struct Fermion *r_y,
                              double *local_norm,
                              size_t start, size_t size, size_t Ls,
			      const struct KTable *ktable,
                              const struct neighbor *neighbor,
                              const struct SUn *U,
                              const struct Fermion *a_y,
                              const struct Fermion *b_x,
                              void *rb[]);
size_t qx(doc_1mKF_norm)(struct Fermion *r_y,
			       double *local_norm,
			       size_t start, size_t size, size_t Ls,
			       const struct KTable *ktable,
			       const struct neighbor *neighbor,
			       const struct SUn *U,
			       const struct Fermion *a_y,
			       const struct Fermion *b_x,
			       void *rb[]);

void qx(op_K)(struct Fermion *r_x,
              struct eo_lattice *xy,
              const struct Q(Parameters) *params,
              const struct Fermion *s_y,
              long long *flops);
void qx(op_K1)(struct Fermion *r_x,
               struct eo_lattice *xy,
               const struct Q(Parameters) *params,
               const struct Fermion *s_y,
               long long *flops);
void qx(op_Kx)(struct Fermion *r_x,
               struct eo_lattice *xy,
               const struct Q(Parameters) *params,
               const struct Fermion *s_y,
               long long *flops);
void qx(op_K1x)(struct Fermion *r_x,
                struct eo_lattice *xy,
                const struct Q(Parameters) *params,
                const struct Fermion *s_y,
                long long *flops);
void qx(op_BA1K1)(struct Fermion *r_x,
                  struct eo_lattice *xy,
                  const struct Q(Parameters) *params,
                  const struct Fermion *s_y,
                  long long *flops);
void qx(op_1mKF)(struct Fermion *r_x,
                 struct eo_lattice *xy,
                 const struct Q(Parameters) *params,
                 const struct SUn *U,
                 const struct Fermion *s_x,
                 const struct Fermion *s_y,
                 long long *flops,
                 long long *sent,
                 long long *received);
void qx(op_1mKF_norm)(struct Fermion *r_x,
                      double *local_norm,
                      struct eo_lattice *xy,
                      const struct Q(Parameters) *params,
                      const struct SUn *U,
                      const struct Fermion *s_x,
                      const struct Fermion *s_y,
                      long long *flops,
                      long long *sent,
                      long long *received);
void qx(op_1mK1xA1xBxFx)(struct Fermion *r_x,
                         struct eo_lattice *xy,
                         const struct Q(Parameters) *params,
                         const struct SUn *U,
                         const struct Fermion *a_x,
                         const struct Fermion *a_y,
                         long long *flops,
                         long long *sent,
                         long long *received);
void qx(op_1mK1xA1xBxFx_norm)(struct Fermion *r_x,
                              double *local_norm,
                              struct eo_lattice *xy,
                              const struct Q(Parameters) *params,
                              const struct SUn *U,
                              const struct Fermion *a_x,
                              const struct Fermion *a_y,
                              long long *flops,
                              long long *sent,
                              long long *received);

/* complex basic A+F, A and B */
size_t qx(doc_ApF)(struct Fermion *r_x,
                         size_t start, size_t size, size_t Ls,
                         const struct ABTable *aptable,
                         const struct ABTable *amtable,
                         const struct neighbor *neighbor,
                         const struct SUn *U,
                         const struct Fermion *s_x,
                         const struct Fermion *s_y,
                         void *rb[]);
size_t qx(doc_ApF_norm)(struct Fermion *r_x,
                              double *local_norm,
                              size_t start, size_t size, size_t Ls,
                              const struct ABTable *aptable,
                              const struct ABTable *amtable,
                              const struct neighbor *neighbor,
                              const struct SUn *U,
                              const struct Fermion *s_x,
                              const struct Fermion *s_y,
                              void *rb[]);
size_t qx(doc_AxpBxFx)(struct Fermion *r_x,
                             size_t start, size_t size, size_t Ls,
                             const struct ABTable *aptable,
                             const struct ABTable *amtable,
                             const struct ABTable *bptable,
                             const struct ABTable *bmtable,
                             const struct neighbor *neighbor,
                             const struct SUn *U,
                             const struct Fermion *s_x,
                             const struct Fermion *s_y,
                             void *rb[]);
size_t qx(doc_BA1)(struct Fermion *r_x,
                         size_t start, size_t size, size_t Ls,
                         const struct ABTable *bptable,
                         const struct ABTable *bmtable,
                         const struct ABiTable *iatable_p,
                         const struct ABiTable *iatable_m,
                         const struct Fermion *s_x);
size_t qx(doc_K)(struct Fermion *r_x,
		       size_t start, size_t size, size_t Ls,
		       const struct KTable *ktable,
		       const struct Fermion *s_x);
size_t qx(doc_Kx)(struct Fermion *r_x,
			size_t start, size_t size, size_t Ls,
			const struct KTable *ktable,
			const struct Fermion *s_x);
size_t qx(doc_BA1F)(struct Fermion *r_y,
                          size_t start, size_t size, size_t Ls,
                          const struct ABTable *bptable,
                          const struct ABTable *bmtable,
                          const struct ABiTable *iatable_p,
                          const struct ABiTable *iatable_m,
                          const struct neighbor *neighbor,
                          const struct SUn *U,
                          const struct Fermion *s_x,
                          void *rb[]);
size_t qx(doc_1mBA1F)(struct Fermion *r_y,
                            size_t start, size_t size, size_t Ls,
                            const struct ABTable *bptable,
                            const struct ABTable *bmtable,
                            const struct ABiTable *iatable_p,
                            const struct ABiTable *iatable_m,
                            const struct neighbor *neighbor,
                            const struct SUn *U,
                            const struct Fermion *a_y,
                            const struct Fermion *b_x,
                            void *rb[]);
size_t qx(doc_1mBA1F_norm)(struct Fermion *r_y,
                                 double *local_norm,
                                 size_t start, size_t size, size_t Ls,
                                 const struct ABTable *bptable,
                                 const struct ABTable *bmtable,
                                 const struct ABiTable *iatable_p,
                                 const struct ABiTable *iatable_m,
                                 const struct neighbor *neighbor,
                                 const struct SUn *U,
                                 const struct Fermion *a_y,
                                 const struct Fermion *b_x,
                                 void *rb[]);
size_t qx(doc_A1xBx)(struct Fermion *r_y,
                           size_t start, size_t size, size_t Ls,
                           const struct ABTable *bptable,
                           const struct ABTable *bmtable,
                           const struct ABiTable *iatable_p,
                           const struct ABiTable *iatable_m,
                           const struct Fermion *b_y);
size_t qx(doc_A1xBxFx)(struct Fermion *r_x,
                             size_t start, size_t size, size_t Ls,
                             const struct ABiTable *aiptable,
                             const struct ABiTable *aimtable,
                             const struct ABTable *bptable,
                             const struct ABTable *bmtable,
                             const struct neighbor *neighbor,
                             const struct SUn *U,
                             const struct Fermion *s_y,
                             void *rb[]);
/* even/odd level routines */
void qx(op_A)(struct Fermion *r_x,
              struct eo_lattice *xy,
              const struct Q(Parameters) *params,
              const struct Fermion *s_x,
              long long *flops);
void qx(op_Ax)(struct Fermion *r_x,
               struct eo_lattice *xy,
               const struct Q(Parameters) *params,
               const struct Fermion *s_x,
               long long *flops);
void qx(op_A1)(struct Fermion *r_x,
               struct eo_lattice *xy,
               const struct Q(Parameters) *params,
               const struct Fermion *s_x,
               long long *flops);
void qx(op_A1x)(struct Fermion *r_x,
                struct eo_lattice *xy,
                const struct Q(Parameters) *params,
                const struct Fermion *s_x,
                long long *flops);
void qx(op_B)(struct Fermion *r_x,
              struct eo_lattice *xy,
              const struct Q(Parameters) *params,
              const struct Fermion *s_x,
              long long *flops);
void qx(op_Bx)(struct Fermion *r_x,
               struct eo_lattice *xy,
               const struct Q(Parameters) *params,
               const struct Fermion *s_x,
               long long *flops);
void qx(op_B1)(struct Fermion *r_x,
               struct eo_lattice *xy,
               const struct Q(Parameters) *params,
               const struct Fermion *s_x,
               long long *flops);
void qx(op_B1x)(struct Fermion *r_x,
                struct eo_lattice *xy,
                const struct Q(Parameters) *params,
                const struct Fermion *s_x,
                long long *flops);
void qx(op_A1xBx)(struct Fermion *r_x,
                  struct eo_lattice *xy,
                  const struct Q(Parameters) *params,
                  const struct Fermion *s_x,
                  long long *flops);
void qx(op_BA1)(struct Fermion *r_x,
                struct eo_lattice *xy,
                const struct Q(Parameters) *params,
                const struct Fermion *a_x,
                long long *flops);
void qx(op_F)(struct Fermion *r_x,
              struct eo_lattice *xy,
              const struct SUn *U,
              const struct Fermion *s_y,
              long long *flops,
              long long *sent,
              long long *received);
void qx(op_Fx)(struct Fermion *r_x,
               struct eo_lattice *xy,
               const struct SUn *U,
               const struct Fermion *s_y,
               long long *flops,
               long long *sent,
               long long *received);
void qx(op_ApF)(struct Fermion *r_x,
                struct eo_lattice *xy,
                const struct Q(Parameters) *params,
                const struct SUn *U,
                const struct Fermion *a_x,
                const struct Fermion *a_y,
                long long *flops,
                long long *sent,
                long long *received);
void qx(op_ApF_norm)(struct Fermion *r_x,
                     double *local_norm,
                     struct eo_lattice *xy,
                     const struct Q(Parameters) *params,
                     const struct SUn *U,
                     const struct Fermion *a_x,
                     const struct Fermion *a_y,
                     long long *flops,
                     long long *sent,
                     long long *received);
void qx(op_AxpBxFx)(struct Fermion *r_x,
                    struct eo_lattice *xy,
                    const struct Q(Parameters) *params,
                    const struct SUn *U,
                    const struct Fermion *a_x,
                    const struct Fermion *a_y,
                    long long *flops,
                    long long *sent,
                    long long *received);
void qx(op_1mF)(struct Fermion *r_x,
                struct eo_lattice *xy,
                const struct SUn *U,
                const struct Fermion *a_x,
                const struct Fermion *a_y,
                long long *flops,
                long long *sent,
                long long *received);
void qx(op_1mFx)(struct Fermion *r_x,
                 struct eo_lattice *xy,
                 const struct SUn *U,
                 const struct Fermion *a_x,
                 const struct Fermion *a_y,
                 long long *flops,
                 long long *sent,
                 long long *received);
void qx(op_1mBA1F)(struct Fermion *r_x,
                   struct eo_lattice *xy,
                   const struct Q(Parameters) *params,
                   const struct SUn *U,
                   const struct Fermion *a_x,
                   const struct Fermion *a_y,
                   long long *flops,
                   long long *sent,
                   long long *received);
void qx(op_1mBA1F_norm)(struct Fermion *r_x,
                        double *norm,
                        struct eo_lattice *xy,
                        const struct Q(Parameters) *params,
                        const struct SUn *U,
                        const struct Fermion *a_x,
                        const struct Fermion *a_y,
                        long long *flops,
                        long long *sent,
                        long long *received);
void qx(op_1mFx_norm)(struct Fermion *r_x,
                      double *local_norm,
                      struct eo_lattice *xy,
                      const struct SUn *U,
                      const struct Fermion *a_x,
                      const struct Fermion *a_y,
                      long long *flops,
                      long long *sent,
                      long long *received);
void qx(op_A1xBxFx)(struct Fermion *r_x,
                    struct eo_lattice *xy,
                    const struct Q(Parameters) *params,
                    const struct SUn *U,
                    const struct Fermion *a_y,
                    long long *flops,
                    long long *sent,
                    long long *received);
void qx(op_BA1F)(struct Fermion *r_x,
                 struct eo_lattice *xy,
                 const struct Q(Parameters) *params,
                 const struct SUn *U,
                 const struct Fermion *a_y,
                 long long *flops,
                 long long *sent,
                 long long *received);
void qx(op_axial_current)(void (*writer)(const int pos[Q(DIM)],
                                         int dir,
                                         double value,
                                         void *env),
                          void *env,
                          struct eo_lattice *xy,
                          struct eo_lattice *yx,
                          const struct SUn *U,
                          const struct Fermion *a_x,
                          const struct Fermion *a_y,
                          long long *flops,
                          long long *sent,
                          long long *received,
			  int node);
void qx(op_D)(struct Fermion *r_x,
              struct eo_lattice *xy,
              struct eo_lattice *yx,
              const struct Q(Parameters) *params,
              const struct SUn *U,
              const struct Fermion *a_x,
              const struct Fermion *a_y,
              long long *flops,
              long long *sent,
              long long *received,
              struct Fermion *tmp_y);
void qx(op_D_norm)(struct Fermion *r_x,
                   double *local_norm,
                   struct eo_lattice *xy,
                   struct eo_lattice *yx,
                   const struct Q(Parameters) *params,
                   const struct SUn *U,
                   const struct Fermion *a_x,
                   const struct Fermion *a_y,
                   long long *flops,
                   long long *sent,
                   long long *received,
                   struct Fermion *tmp_y);
#if 0
void qx(op_pc2_M)(struct Fermion *r_x,
              struct Q(State) *state,
              const struct Q(Parameters) *params,
              const struct SUn *gauge,
              const struct Fermion *s_x,
              long long *flops,
              long long *sent,
              long long *received,
              struct Fermion *t_x,
              struct Fermion *t_y)
void qx(op_pc2_Mn)(struct Fermion *r_x,
              double *global_norm,
              struct Q(State) *state,
              const struct Q(Parameters) *params,
              const struct SUn *gauge,
              const struct Fermion *s_x,
              long long *flops,
              long long *sent,
              long long *received,
              struct Fermion *t_x,
              struct Fermion *t_y)
void qx(op_pc2_Mx)(struct Fermion *r_x,
              struct Q(State) *state,
              const struct Q(Parameters) *params,
              const struct SUn *gauge,
              const struct Fermion *s_x,
              long long *flops,
              long long *sent,
              long long *received,
              struct Fermion *t_x,
              struct Fermion *t_y)
void qx(op_pc2_Mxn)(struct Fermion *r_x,
              double *global_norm,
              struct Q(State) *state,
              const struct Q(Parameters) *params,
              const struct SUn *gauge,
              const struct Fermion *s_x,
              long long *flops,
              long long *sent,
              long long *received,
              struct Fermion *t_x,
              struct Fermion *t_y)
void qx(op_pc2p_M)(struct Fermion *r_x,
              struct Q(State) *state,
              const struct Q(Parameters) *params,
              const struct SUn *gauge,
              const struct Fermion *s_x,
              long long *flops,
              long long *sent,
              long long *received,
              struct Fermion *t_x,
              struct Fermion *t_y)
void qx(op_pc2p_Mn)(struct Fermion *r_x,
              double *global_norm,
              struct Q(State) *state,
              const struct Q(Parameters) *params,
              const struct SUn *gauge,
              const struct Fermion *s_x,
              long long *flops,
              long long *sent,
              long long *received,
              struct Fermion *t_x,
              struct Fermion *t_y)
void qx(op_pc2p_Mx)(struct Fermion *r_x,
              struct Q(State) *state,
              const struct Q(Parameters) *params,
              const struct SUn *gauge,
              const struct Fermion *s_x,
              long long *flops,
              long long *sent,
              long long *received,
              struct Fermion *t_x,
              struct Fermion *t_y)
void qx(op_pc2p_Mxn)(struct Fermion *r_x,
              double *global_norm,
              struct Q(State) *state,
              const struct Q(Parameters) *params,
              const struct SUn *gauge,
              const struct Fermion *s_x,
              long long *flops,
              long long *sent,
              long long *received,
              struct Fermion *t_x,
              struct Fermion *t_y)
/* parts of the CG solver */
void qx(cg_pc2_precondition)(struct Fermion *xi0_x,
                             struct Fermion *chi_x,
                             struct Q(State) *state,
                             const struct Q(Parameters) *params,
                             const struct SUn *U,
                             const struct Fermion *psi0_x,
                             const struct Fermion *eta_x,
                             const struct Fermion *eta_y,
                             long long *flops,
                             long long *sent,
                             long long *received,
                             struct Fermion *t0_x,
                             struct Fermion *t1_x,
                             struct Fermion *t0_y);
void qx(cg_pc2p_precondition)(struct Fermion *xi0_x,
                              struct Fermion *chi_x,
                              struct Q(State) *state,
                              const struct Q(Parameters) *params,
                              const struct SUn *U,
                              const struct Fermion *psi0_x,
                              const struct Fermion *eta_x,
                              const struct Fermion *eta_y,
                              long long *flops,
                              long long *sent,
                              long long *received,
                              struct Fermion *t0_x,
                              struct Fermion *t1_x,
                              struct Fermion *t0_y);
void qx(cg_pc2_inflate)(struct Fermion *psi_x,
                        struct Fermion *psi_y,
                        struct Q(State) *state,
                        const struct Q(Parameters) *params,
                        const struct SUn *U,
                        const struct Fermion *eta_y,
                        const struct Fermion *xi_x,
                        long long *flops,
                        long long *sent,
                        long long *received,
                        struct Fermion *t_x,
                        struct Fermion *t_y);
void qx(cg_pc2p_inflate)(struct Fermion *psi_x,
                         struct Fermion *psi_y,
                         struct Q(State) *state,
                         const struct Q(Parameters) *params,
                         const struct SUn *U,
                         const struct Fermion *eta_y,
                         const struct Fermion *xi_x,
                         long long *flops,
                         long long *sent,
                         long long *received,
                         struct Fermion *t_x,
                         struct Fermion *t_y);
#else /* shorter function prototypes */
#define qx_op_eo(op, pc) void qx(op_##pc##op)(\
        struct Fermion *r_x,\
        struct Q(State) *state,\
        const struct Q(Parameters) *params,\
        const struct SUn *gauge,\
        const struct Fermion *s_x,\
        long long *flops,\
        long long *sent,\
        long long *received,\
        struct Fermion *t_x,\
        struct Fermion *t_y)
#define qx_op_norm_eo(op, pc) void qx(op_##pc##op##n)(\
        struct Fermion *r_x,\
        double *global_norm,\
        struct Q(State) *state,\
        const struct Q(Parameters) *params,\
        const struct SUn *gauge,\
        const struct Fermion *s_x,\
        long long *flops,\
        long long *sent,\
        long long *received,\
        struct Fermion *t_x,\
        struct Fermion *t_y)
qx_op_eo(M,   pc2_);
qx_op_eo(M,   pc2p_);
qx_op_eo(M,   );
qx_op_norm_eo(M,  pc2_);
qx_op_norm_eo(M,  pc2p_);
qx_op_norm_eo(M,  );
qx_op_eo(Mx,  pc2_);
qx_op_eo(Mx,  pc2p_);
qx_op_eo(Mx,  );
qx_op_norm_eo(Mx, pc2_);
qx_op_norm_eo(Mx, pc2p_);
qx_op_norm_eo(Mx, );
/* parts of the CG solver */
#define qx_cg_precondition_eo(pc) void qx(cg_##pc##precondition)(\
        struct Fermion *xi0_x,\
        struct Fermion *chi_x,\
        struct Q(State) *state,\
        const struct Q(Parameters) *params,\
        const struct SUn *U,\
        const struct Fermion *psi0_x,\
        const struct Fermion *eta_x,\
        const struct Fermion *eta_y,\
        long long *flops,\
        long long *sent,\
        long long *received,\
        struct Fermion *t0_x,\
        struct Fermion *t1_x,\
        struct Fermion *t0_y)
qx_cg_precondition_eo(pc2_);
qx_cg_precondition_eo(pc2p_);
qx_cg_precondition_eo();
#define qx_cg_inflate_eo(pc) void qx(cg_##pc##inflate)(\
        struct Fermion *psi_x,\
        struct Fermion *psi_y,\
        struct Q(State) *state,\
        const struct Q(Parameters) *params,\
        const struct SUn *U,\
        const struct Fermion *eta_y,\
        const struct Fermion *xi_x,\
        long long *flops,\
        long long *sent,\
        long long *received,\
        struct Fermion *t_x,\
        struct Fermion *t_y)
qx_cg_inflate_eo(pc2_);
qx_cg_inflate_eo(pc2p_);
qx_cg_inflate_eo();

#endif
/* logging */
void qx(zprint)(struct Q(State) *state,
                const char *source,
                const char *fmt,
                ...);

double qx(cg_dirac_error)(const struct Fermion *psi_x,
                          const struct Fermion *psi_y,
                          struct Q(State) *state,
                          const struct Q(Parameters) *params,
                          const struct SUn *U,
                          const struct Fermion *eta_x,
                          const struct Fermion *eta_y,
                          long long *flops,
                          long long *sent,
                          long long *received,
                          struct Fermion *t0_x,
                          struct Fermion *t1_x,
                          struct Fermion *t0_y);
void qx(cg_log)(double cg_res, const char *source, int iter,
                const struct Fermion *xi_x,
                struct Q(State) *state,
                const struct Q(Parameters) *params,
                const struct SUn *U,
                const struct Fermion *chi_x,
                long long *flops,
                long long *sent,
                long long *received,
                unsigned int options,
                struct Fermion *t0_x,
                struct Fermion *t1_x,
                struct Fermion *t2_x,
                struct Fermion *t0_y);

void qx(cg_operator)(struct Fermion            *res_x,
                     const struct Fermion      *psi_x,
                     struct qx(MxM_workspace)  *ws);

CG_STATUS qx(cg_solver)(struct Fermion              *psi_x,
                        const char                  *source,
                        int                         *out_iter,
                        double                      *out_epsilon,
                        struct Q(State)             *state,
                        const struct Q(Parameters)  *params,
                        const struct SUn            *U,
                        const struct Fermion        *chi_x,
                        struct QX(Deflator)         *deflator,
                        int                          max_iter,
                        double                       epsilon,
                        unsigned                     options,
                        long long                   *flops,
                        long long                   *sent,
                        long long                   *received,
                        struct Fermion              *rho_x,
                        struct Fermion              *pi_x,
                        struct Fermion              *zeta_x,
                        struct Fermion              *t0_x,
                        struct Fermion              *t1_x,
                        struct Fermion              *t2_x,
                        struct Fermion              *t0_y);
int qx(scg_solver)(struct VectorFermion         *v_xi_e,
                   struct Fermion               *xi_e,
                   int                           count,
                   const char                   *source,
                   int                          *out_iterations,
                   double                       *out_epsilon,
                   struct Q(State)              *state,
                   const struct Q(Parameters)   *params,
                   const double                  shift[],
                   const struct SUn             *U,
                   const struct Fermion         *chi_e,
                   int                           max_iterations,
                   double                        min_epsilon,
                   unsigned                      options,
                   long long                    *flops,
                   long long                    *sent,
                   long long                    *received,
                   double                        v[],
                   double                        w[],
                   double                        ad[],
                   double                        bdd[],
                   struct Fermion               *rho_e,
                   struct VectorFermion         *vpi_e,
                   struct Fermion               *pi_e,
                   struct Fermion               *zeta_e,
                   struct Fermion               *t0_e,
                   struct Fermion               *t1_e,
                   struct Fermion               *t2_e,
                   struct Fermion               *t0_o);
/*
 *  compute x <- x + alpha p
 *          p <- r + beta p
 */
size_t qx(cg_xp)(struct Fermion *x,
                       struct Fermion *p,
                       size_t start, size_t size, size_t Ls,
                       double alpha,
                       double beta,
                       const struct Fermion *r);
/*
 * compute xi <- xi + a * pi
 *         v_xi[i] <- v_xi[i] + ad[i] * pi
 */
size_t qx(scg_madd)(struct Fermion *xi_e,
                          struct VectorFermion *v_xi_e,
                          size_t start, size_t size, size_t Ls, size_t count,
                          double a,
                          const double *ad,
                          const struct Fermion *pi_e);
/*
 * compute xi <- xi + a * pi
 *         pi <- rho + b * pi
 *         v_xi[i] <- v_xi[i] + ad[i] * v_pi[i]
 *         v_pi[i] <- rho + bdd[i] * v_pi[i]
 */
size_t qx(scg_xp)(struct Fermion *xi_e,
                        struct Fermion *pi_e,
                        struct VectorFermion *v_xi_e,
                        struct VectorFermion *v_pi_e,
                        size_t start, size_t size, size_t Ls, size_t count,
                        double a,
                        double b,
                        const double *ad,
                        const double *bdd,
                        const struct Fermion *rho_e);

/*
 *  AB operations
 */
struct qx(ABops) {
  size_t (*A)(struct Q(State) *state,
                    struct Fermion *r_x,
                    size_t size, size_t Ls,
                    const struct ABTable *aptable,
                    const struct ABTable *amtable,
                    const struct Fermion *s_x);
  size_t (*A_conj)(struct Q(State) *state,
                         struct Fermion *r_x,
                         size_t size, size_t Ls,
                         const struct ABTable *axptable,
                         const struct ABTable *axmtable,
                         const struct Fermion *s_x);
  size_t (*A_inverse)(struct Q(State) *state,
                            struct Fermion *r,
                            size_t size, size_t Ls,
                            const struct ABiTable *iatable_p,
                            const struct ABiTable *iatable_m,
                            const struct Fermion *x);
  size_t (*A_conj_inverse)(struct Q(State) *state,
                                 struct Fermion *r,
                                 size_t size, size_t Ls,
                                 const struct ABiTable *iatable_p,
                                 const struct ABiTable *iatable_m,
                                 const struct Fermion *x);
  size_t (*ApF)(struct Q(State) *state,
                      struct Fermion *r_x,
                      size_t start, size_t size, size_t Ls,
                      const struct ABTable *aptable,
                      const struct ABTable *amtable,
                      const struct neighbor *neighbor,
                      const struct SUn *U,
                      const struct Fermion *s_x,
                      const struct Fermion *s_y,
                      void *rb[]);
  size_t (*ApF_norm)(struct Q(State) *state,
                           struct Fermion *r_x,
                           double *local_norm,
                           size_t start, size_t size, size_t Ls,
                           const struct ABTable *aptable,
                           const struct ABTable *amtable,
                           const struct neighbor *neighbor,
                           const struct SUn *U,
                           const struct Fermion *s_x,
                           const struct Fermion *s_y,
                           void *rb[]);
  size_t (*AxpBxFx)(struct Q(State) *state,
                          struct Fermion *r_x,
                          size_t start, size_t size, size_t Ls,
                          const struct ABTable *aptable,
                          const struct ABTable *amtable,
                          const struct ABTable *bptable,
                          const struct ABTable *bmtable,
                          const struct neighbor *neighbor,
                          const struct SUn *U,
                          const struct Fermion *s_x,
                          const struct Fermion *s_y,
                          void *rb[]);
  size_t (*BA1)(struct Q(State) *state,
                      struct Fermion *r_x,
                      size_t size, size_t Ls,
                      const struct ABTable *bptable,
                      const struct ABTable *bmtable,
                      const struct ABiTable *iatable_p,
                      const struct ABiTable *iatable_m,
                      const struct Fermion *s_x);
  size_t (*BA1F)(struct Q(State) *state,
                       struct Fermion *r_y,
                       size_t start, size_t size, size_t Ls,
                       const struct ABTable *bptable,
                       const struct ABTable *bmtable,
                       const struct ABiTable *iatable_p,
                       const struct ABiTable *iatable_m,
                       const struct neighbor *neighbor,
                       const struct SUn *U,
                       const struct Fermion *s_x,
                       void *rb[]);
  size_t (*z1mBA1F)(struct Q(State) * state,
                          struct Fermion *r_y,
                          size_t start, size_t size, size_t Ls,
                          const struct ABTable *bptable,
                          const struct ABTable *bmtable,
                          const struct ABiTable *iatable_p,
                          const struct ABiTable *iatable_m,
                          const struct neighbor *neighbor,
                          const struct SUn *U,
                          const struct Fermion *a_y,
                          const struct Fermion *b_x,
                          void *rb[]);
  size_t (*z1mBA1F_norm)(struct Q(State) * state,
                               struct Fermion *r_y,
                               double *local_norm,
                               size_t start, size_t size, size_t Ls,
                               const struct ABTable *bptable,
                               const struct ABTable *bmtable,
                               const struct ABiTable *iatable_p,
                               const struct ABiTable *iatable_m,
                               const struct neighbor *neighbor,
                               const struct SUn *U,
                               const struct Fermion *a_y,
                               const struct Fermion *b_x,
                               void *rb[]);
  size_t (*A1xBx)(struct Q(State) *state,
                        struct Fermion *r_y,
                        size_t size, size_t Ls,
                        const struct ABTable *bptable,
                        const struct ABTable *bmtable,
                        const struct ABiTable *iatable_p,
                        const struct ABiTable *iatable_m,
                        const struct Fermion *b_y);
  size_t (*A1xBxFx)(struct Q(State) *state,
                          struct Fermion *r_x,
                          size_t start, size_t size, size_t Ls,
                          const struct ABiTable *aiptable,
                          const struct ABiTable *aimtable,
                          const struct ABTable *bptable,
                          const struct ABTable *bmtable,
                          const struct neighbor *neighbor,
                          const struct SUn *U,
                          const struct Fermion *s_y,
                          void *rb[]);
  size_t (*K)(struct Q(State) *state,
		    struct Fermion *r_x,
		    size_t size, size_t Ls,
		    const struct KTable *ktable,
		    const struct Fermion *s_x);
  size_t (*K_conj)(struct Q(State) *state,
			 struct Fermion *r_x,
			 size_t size, size_t Ls,
			 const struct KTable *ktable,
			 const struct Fermion *s_x);
  size_t (*BA1K)(struct Q(State) *state,
		       struct Fermion *r_x,
		       size_t size, size_t Ls,
		       const struct ABTable *bptable,
		       const struct ABTable *bmtable,
		       const struct ABiTable *iatable_p,
		       const struct ABiTable *iatable_m,
		       const struct KTable *ktable,
		       const struct Fermion *s_x);
  size_t (*z1mK1xA1xBxFx)(struct Q(State) * state,
				struct Fermion *r_y,
				size_t start, size_t size, size_t Ls,
				const struct KTable *iktable,
				const struct ABiTable *iatable_p,
				const struct ABiTable *iatable_m,
				const struct ABTable *bptable,
				const struct ABTable *bmtable,
				const struct neighbor *neighbor,
				const struct SUn *U,
				const struct Fermion *a_y,
				const struct Fermion *b_x,
				void *rb[]);
  size_t (*z1mK1xA1xBxFx_norm)(struct Q(State) * state,
				     struct Fermion *r_y,
				     double *local_norm,
				     size_t start, size_t size, size_t Ls,
				     const struct KTable *iktable,
				     const struct ABiTable *iatable_p,
				     const struct ABiTable *iatable_m,
				     const struct ABTable *bptable,
				     const struct ABTable *bmtable,
				     const struct neighbor *neighbor,
				     const struct SUn *U,
				     const struct Fermion *a_y,
				     const struct Fermion *b_x,
				     void *rb[]);
  size_t (*z1mKF)(struct Q(State) * state,
			struct Fermion *r_y,
			size_t start, size_t size, size_t Ls,
			const struct KTable *iktable,
			const struct neighbor *neighbor,
			const struct SUn *U,
			const struct Fermion *a_y,
			const struct Fermion *b_x,
			void *rb[]);
  size_t (*z1mKF_norm)(struct Q(State) * state,
			     struct Fermion *r_y,
			     double *local_norm,
			     size_t start, size_t size, size_t Ls,
			     const struct KTable *iktable,
			     const struct neighbor *neighbor,
			     const struct SUn *U,
			     const struct Fermion *a_y,
			     const struct Fermion *b_x,
			     void *rb[]);
};

/* OpenMP wraps */
/* Other openMP wrappers */
size_t qx(omp_do_1mBA1F)(struct Q(State) *state,
                               struct Fermion *r_y,
                               size_t start, size_t size, size_t Ls,
                               const struct ABTable *bptable,
                               const struct ABTable *bmtable,
                               const struct ABiTable *iatable_p,
                               const struct ABiTable *iatable_m,
                               const struct neighbor *neighbor,
                               const struct SUn *U,
                               const struct Fermion *a_y,
                               const struct Fermion *b_x,
                               void *rb[]);
size_t qx(omp_do_AxpBxFx)(struct Q(State) *state,
                                struct Fermion *r_x,
                                size_t start, size_t size, size_t Ls,
                                const struct ABTable *aptable,
                                const struct ABTable *amtable,
                                const struct ABTable *bptable,
                                const struct ABTable *bmtable,
                                const struct neighbor *neighbor,
                                const struct SUn *U,
                                const struct Fermion *s_x,
                                const struct Fermion *s_y,
                                void *rb[]);                           
size_t qx(omp_do_A_conj)(struct Q(State) *state,
                               struct Fermion *r_x,
                               size_t size, size_t Ls,
                               const struct ABTable *axptable,
                               const struct ABTable *axmtable,
                               const struct Fermion *s_x);
size_t qx(omp_do_A1xBxFx)(struct Q(State) *state,
                                struct Fermion *r_x,
                                size_t start, size_t size, size_t Ls,
                                const struct ABiTable *aiptable,
                                const struct ABiTable *aimtable,
                                const struct ABTable *bptable,
                                const struct ABTable *bmtable,
                                const struct neighbor *neighbor,
                                const struct SUn *U,
                                const struct Fermion *s_y,
                                void *rb[]);
size_t qx(omp_do_A1xBx)(struct Q(State) *state,
                              struct Fermion *r_y,
                              size_t size, size_t Ls,
                              const struct ABTable *bptable,
                              const struct ABTable *bmtable,
                              const struct ABiTable *iatable_p,
                              const struct ABiTable *iatable_m,
                              const struct Fermion *b_y);
size_t qx(omp_do_A_conj_inverse)(struct Q(State) *state,
                                       struct Fermion *r,
                                       size_t size, size_t Ls,
                                       const struct ABiTable *iatable_p,
                                       const struct ABiTable *iatable_m,
                                       const struct Fermion *x);
size_t qx(omp_do_A_inverse)(struct Q(State) *state,
                                  struct Fermion *r,
                                  size_t size, size_t Ls,
                                  const struct ABiTable *iatable_p,
                                  const struct ABiTable *iatable_m,
                                  const struct Fermion *x);
size_t qx(omp_do_ApF)(struct Q(State) *state,
                            struct Fermion *r_x,
                            size_t start, size_t size, size_t Ls,
                            const struct ABTable *aptable,
                            const struct ABTable *amtable,
                            const struct neighbor *neighbor,
                            const struct SUn *U,
                            const struct Fermion *s_x,
                            const struct Fermion *s_y,
                            void *rb[]);
size_t qx(omp_do_A)(struct Q(State) *state,
                          struct Fermion *r_x,
                          size_t size, size_t Ls,
                          const struct ABTable *aptable,
                          const struct ABTable *amtable,
                          const struct Fermion *s_x);
size_t qx(omp_do_K)(struct Q(State) *state,
                          struct Fermion *r_x,
                          size_t size, size_t Ls,
                          const struct KTable *ktable,
                          const struct Fermion *s_x);
size_t qx(omp_do_BA1F)(struct Q(State) *state,
                             struct Fermion *r_y,
                             size_t start, size_t size, size_t Ls,
                             const struct ABTable *bptable,
                             const struct ABTable *bmtable,
                             const struct ABiTable *iatable_p,
                             const struct ABiTable *iatable_m,
                             const struct neighbor *neighbor,
                             const struct SUn *U,
                             const struct Fermion *s_x,
                             void *rb[]);
size_t qx(omp_do_BA1)(struct Q(State) *state,
                            struct Fermion *r_x,
                            size_t size, size_t Ls,
                            const struct ABTable *bptable,
                            const struct ABTable *bmtable,
                            const struct ABiTable *iatable_p,
                            const struct ABiTable *iatable_m,
                            const struct Fermion *s_x);
size_t qx(omp_do_BA1K)(struct Q(State) *state,
			     struct Fermion *r_x,
			     size_t size, size_t Ls,
			     const struct ABTable *bptable,
			     const struct ABTable *bmtable,
			     const struct ABiTable *iatable_p,
			     const struct ABiTable *iatable_m,
			     const struct KTable *ktable,
			     const struct Fermion *s_x);
size_t qx(omp_do_1mK1xA1xBxFx)(struct Q(State) *state,
				     struct Fermion *r_y,
				     size_t start, size_t size, size_t Ls,
				     const struct KTable *ktable,
				     const struct ABiTable *iatable_p,
				     const struct ABiTable *iatable_m,
				     const struct ABTable *bptable,
				     const struct ABTable *bmtable,
				     const struct neighbor *neighbor,
				     const struct SUn *U,
				     const struct Fermion *a_y,
				     const struct Fermion *b_x,
				     void *rb[]);
size_t qx(omp_do_1mK1xA1xBxFx_norm)(struct Q(State) *state,
					  struct Fermion *r_y,
					  double *local_norm,
					  size_t start, size_t size, size_t Ls,
					  const struct KTable *ktable,
					  const struct ABiTable *iatable_p,
					  const struct ABiTable *iatable_m,
					  const struct ABTable *bptable,
					  const struct ABTable *bmtable,
					  const struct neighbor *neighbor,
					  const struct SUn *U,
					  const struct Fermion *a_y,
					  const struct Fermion *b_x,
					  void *rb[]);
size_t qx(omp_doc_1mBA1F)(struct Q(State) *state,
                                struct Fermion *r_y,
                                size_t start, size_t size, size_t Ls,
                                const struct ABTable *bptable,
                                const struct ABTable *bmtable,
                                const struct ABiTable *iatable_p,
                                const struct ABiTable *iatable_m,
                                const struct neighbor *neighbor,
                                const struct SUn *U,
                                const struct Fermion *a_y,
                                const struct Fermion *b_x,
                                void *rb[]);
size_t qx(omp_doc_AxpBxFx)(struct Q(State) *state,
                                 struct Fermion *r_x,
                                 size_t start, size_t size, size_t Ls,
                                 const struct ABTable *aptable,
                                 const struct ABTable *amtable,
                                 const struct ABTable *bptable,
                                 const struct ABTable *bmtable,
                                 const struct neighbor *neighbor,
                                 const struct SUn *U,
                                 const struct Fermion *s_x,
                                 const struct Fermion *s_y,
                                 void *rb[]);
size_t qx(omp_doc_A_conj)(struct Q(State) *state,
                                struct Fermion *r_x,
                                size_t size, size_t Ls,
                                const struct ABTable *axptable,
                                const struct ABTable *axmtable,
                                const struct Fermion *s_x);
size_t qx(omp_doc_A1xBxFx)(struct Q(State) *state,
                                 struct Fermion *r_x,
                                 size_t start, size_t size, size_t Ls,
                                 const struct ABiTable *aiptable,
                                 const struct ABiTable *aimtable,
                                 const struct ABTable *bptable,
                                 const struct ABTable *bmtable,
                                 const struct neighbor *neighbor,
                                 const struct SUn *U,
                                 const struct Fermion *s_y,
                                 void *rb[]);
size_t qx(omp_doc_A1xBx)(struct Q(State) *state,
                               struct Fermion *r_y,
                               size_t size, size_t Ls,
                               const struct ABTable *bptable,
                               const struct ABTable *bmtable,
                               const struct ABiTable *iatable_p,
                               const struct ABiTable *iatable_m,
                               const struct Fermion *b_y);
size_t qx(omp_doc_A_inverse)(struct Q(State) *state,
                                   struct Fermion *r,
                                   size_t size, size_t Ls,
                                   const struct ABiTable *iatable_p,
                                   const struct ABiTable *iatable_m,
                                   const struct Fermion *x);
size_t qx(omp_doc_A_conj_inverse)(struct Q(State) *state,
                                        struct Fermion *r,
                                        size_t size, size_t Ls,
                                        const struct ABiTable *iatable_p,
                                        const struct ABiTable *iatable_m,
                                        const struct Fermion *x);
size_t qx(omp_doc_ApF)(struct Q(State) *state,
                             struct Fermion *r_x,
                             size_t start, size_t size, size_t Ls,
                             const struct ABTable *aptable,
                             const struct ABTable *amtable,
                             const struct neighbor *neighbor,
                             const struct SUn *U,
                             const struct Fermion *s_x,
                             const struct Fermion *s_y,
                             void *rb[]);
size_t qx(omp_doc_A)(struct Q(State) *state,
                           struct Fermion *r_x,
                           size_t size, size_t Ls,
                           const struct ABTable *aptable,
                           const struct ABTable *amtable,
                           const struct Fermion *s_x);
size_t qx(omp_doc_K)(struct Q(State) *state,
			   struct Fermion *r_x,
			   size_t size, size_t Ls,
			   const struct KTable *ktable,
			   const struct Fermion *s_x);
size_t qx(omp_doc_K_conj)(struct Q(State) *state,
				struct Fermion *r_x,
				size_t size, size_t Ls,
				const struct KTable *ktable,
				const struct Fermion *s_x);
size_t qx(omp_doc_BA1F)(struct Q(State) *state,
                              struct Fermion *r_y,
                              size_t start, size_t size, size_t Ls,
                              const struct ABTable *bptable,
                              const struct ABTable *bmtable,
                              const struct ABiTable *iatable_p,
                              const struct ABiTable *iatable_m,
                              const struct neighbor *neighbor,
                              const struct SUn *U,
                              const struct Fermion *s_x,
                              void *rb[]);
size_t qx(omp_doc_BA1)(struct Q(State) *state,
                             struct Fermion *r_x,
                             size_t size, size_t Ls,
                             const struct ABTable *bptable,
                             const struct ABTable *bmtable,
                             const struct ABiTable *iatable_p,
                             const struct ABiTable *iatable_m,
                             const struct Fermion *s_x);
size_t qx(omp_doc_BA1K)(struct Q(State) *state,
			      struct Fermion *r_x,
			      size_t size, size_t Ls,
			      const struct ABTable *bptable,
			      const struct ABTable *bmtable,
			      const struct ABiTable *iatable_p,
			      const struct ABiTable *iatable_m,
			      const struct KTable *ktable,
			      const struct Fermion *s_x);
size_t qx(omp_doc_1mK1xA1xBxFx)(struct Q(State) *state,
				      struct Fermion *r_y,
				      size_t start, size_t size, size_t Ls,
				      const struct KTable *ktable,
				      const struct ABiTable *iatable_p,
				      const struct ABiTable *iatable_m,
				      const struct ABTable *bptable,
				      const struct ABTable *bmtable,
				      const struct neighbor *neighbor,
				      const struct SUn *U,
				      const struct Fermion *a_y,
				      const struct Fermion *b_x,
				      void *rb[]);
size_t qx(omp_doc_1mK1xA1xBxFx_norm)(struct Q(State) *state,
					   struct Fermion *r_y,
					   double *local_norm,
					   size_t start, size_t size, size_t Ls,
					   const struct KTable *ktable,
					   const struct ABiTable *iatable_p,
					   const struct ABiTable *iatable_m,
					   const struct ABTable *bptable,
					   const struct ABTable *bmtable,
					   const struct neighbor *neighbor,
					   const struct SUn *U,
					   const struct Fermion *a_y,
					   const struct Fermion *b_x,
					   void *rb[]);
void qx(omp_fermion2blas)(struct Q(State) *state, void *data, const struct Fermion *f, size_t size, size_t Ls);
void qx(omp_blas2fermion)(struct Q(State) *state, struct Fermion *f, size_t size, size_t Ls, const void *data);
size_t qx(omp_cg_xp)(struct Q(State) *state,
                           struct Fermion *x,
                           struct Fermion *p,
                           size_t size, size_t Ls,
                           double alpha,
                           double beta,
                           const struct Fermion *r);
size_t qx(omp_scg_madd)(struct Q(State) *state,
                              struct Fermion *xi_e,
                              struct VectorFermion *v_xi_e,
                              size_t size, size_t Ls, size_t count,
                              double a,
                              const double *ad,
                              const struct Fermion *pi_e);
size_t qx(omp_scg_xp)(struct Q(State) *state,
                            struct Fermion *xi_e,
                            struct Fermion *pi_e,
                            struct VectorFermion *v_xi_e,
                            struct VectorFermion *v_pi_e,
                            size_t size, size_t Ls, size_t count,
                            double a,
                            double b,
                            const double *ad,
                            const double *bdd,
                            const struct Fermion *rho_e);
size_t qx(omp_do_1mFx)(struct Q(State) *state,
                             struct Fermion *r_y,
                             size_t start, size_t size, size_t Ls,
                             const struct neighbor *neighbor,
                             const struct SUn *U,
                             const struct Fermion *a_y,
                             const struct Fermion *b_x,
                             void *rb[]);
size_t qx(omp_do_1mF)(struct Q(State) *state,
                            struct Fermion *r_y,
                            size_t start, size_t size, size_t Ls,
                            const struct neighbor *neighbor,
                            const struct SUn *U,
                            const struct Fermion *a_y,
                            const struct Fermion *b_x,
                            void *rb[]);
size_t qx(omp_do_1mKF)(struct Q(State) *state,
			     struct Fermion *r_y,
			     size_t start, size_t size, size_t Ls,
			     const struct KTable *ktable,
			     const struct neighbor *neighbor,
			     const struct SUn *U,
			     const struct Fermion *a_y,
			     const struct Fermion *b_x,
			     void *rb[]);
size_t qx(omp_doc_1mKF)(struct Q(State) *state,
			      struct Fermion *r_y,
			      size_t start, size_t size, size_t Ls,
			      const struct KTable *ktable,
			      const struct neighbor *neighbor,
			      const struct SUn *U,
			      const struct Fermion *a_y,
			      const struct Fermion *b_x,
			      void *rb[]);
void qx(omp_f_zero)(struct Q(State) *state,
                    struct Fermion *dst, 
                    size_t size, size_t Ls);
void qx(omp_f_copy)(struct Q(State) *state,
                    struct Fermion *dst, 
                    size_t size, size_t Ls,
                    const struct Fermion *src);
size_t qx(omp_f_add3)(struct Q(State) *state,
                            struct Fermion *r,
                            size_t size, size_t Ls,
                            const struct Fermion *a,
                            double s,
                            const struct Fermion *b);
size_t qx(omp_f_add2)(struct Q(State) *state,
                            struct Fermion *r,
                            size_t size, size_t Ls,
                            double s,
                            const struct Fermion *b);
size_t qx(omp_f_cadd2)(struct Q(State) *state,
                             struct Fermion *r,
                             size_t size, size_t Ls,
                             double sr, double si,
                             const struct Fermion *b);
size_t qx(omp_f_rmul1)(struct Q(State) *state,
                             struct Fermion *r,
                             size_t size, size_t Ls,
                             double s);
size_t qx(omp_f_add2x)(struct Q(State) *state,
                             struct Fermion *r,
                             size_t size, size_t Ls,
                             double s,
                             const struct Fermion *b);
void qx(omp_fv_zero)(struct Q(State) *state,
                     struct VectorFermion *vf,
                     size_t size, size_t Ls, size_t count);
void qx(omp_fv_copy)(struct Q(State) *state,
                     struct VectorFermion *vf,
                     size_t size, size_t Ls, size_t count,
                     const struct Fermion *f);
void qx(omp_fv_get)(struct Q(State) *state,
                    struct Fermion *f,
                    size_t size, size_t Ls, size_t count,
                    const struct VectorFermion *vf, size_t k);
void qx(omp_fv_put)(struct Q(State) *state,
                    struct VectorFermion *vf, size_t k,
                    size_t size, size_t Ls, size_t count,
                    const struct Fermion *f);
size_t qx(omp_vf_copy)(struct Q(State) *state, size_t size, size_t Ls, size_t len,
                             struct vFermion *fv, size_t fv_stride, size_t fv_begin,
                             const struct vFermion *gv, size_t gv_stride, size_t gv_begin);
size_t qx(omp_vf_put)(struct Q(State) *state, size_t size, size_t Ls,
                            struct vFermion *fv, size_t fv_stride, size_t fv_idx,
                            const struct Fermion *x);
size_t qx(omp_vf_get)(struct Q(State) *state, size_t size, size_t Ls,
                            struct Fermion *x,
                            const struct vFermion *fv, size_t fv_stride, size_t fv_idx);
size_t qx(omp_vf_dot_vz)(struct Q(State) *state, size_t size, size_t Ls,
                               struct Fermion *g,
                               const struct vFermion *fv,
                               size_t fv_stride, size_t fv_begin, size_t fv_len,
                               const double *v);
size_t qx(omp_vf_dot_mz)(struct Q(State) *state, size_t size, size_t Ls,
                               struct vFermion *gv,
                               size_t gv_stride, size_t gv_begin, size_t gv_len,
                               const struct vFermion *fv,
                               size_t fv_stride, size_t fv_begin, size_t fv_len,
                               const double *m, size_t ldm);
size_t qx(omp_do_vfH_dot_f)(struct Q(State) *state, size_t size, size_t Ls,
                                  double *c,
                                  const struct vFermion *fv,
                                  size_t fv_stride, size_t fv_begin, size_t fv_len,
                                  const struct Fermion *g);
size_t qx(omp_do_vfH_dot_vf)(struct Q(State) *state, size_t size, size_t Ls,
                                   double *c, size_t ldc,
                                   const struct vFermion *fv,
                                   size_t fv_stride, size_t fv_begin, size_t fv_len,
                                   const struct vFermion *gv,
                                   size_t gv_stride, size_t gv_begin, size_t gv_len);
size_t qx(omp_do_1mBA1F_norm)(struct Q(State) *state,
                                    struct Fermion *r_y,
                                    double *local_norm,
                                    size_t start, size_t size, size_t Ls,
                                    const struct ABTable *bptable,
                                    const struct ABTable *bmtable,
                                    const struct ABiTable *iatable_p,
                                    const struct ABiTable *iatable_m,
                                    const struct neighbor *neighbor,
                                    const struct SUn *U,
                                    const struct Fermion *a_y,
                                    const struct Fermion *b_x,
                                    void *rb[]);
size_t qx(omp_do_ApF_norm)(struct Q(State) *state,
                                 struct Fermion *r_x,
                                 double *local_norm,
                                 size_t start, size_t size, size_t Ls,
                                 const struct ABTable *aptable,
                                 const struct ABTable *amtable,
                                 const struct neighbor *neighbor,
                                 const struct SUn *U,
                                 const struct Fermion *s_x,
                                 const struct Fermion *s_y,
                                 void *rb[]);
size_t qx(omp_doc_1mBA1F_norm)(struct Q(State) *state,
                                     struct Fermion *r_y,
                                     double *local_norm,
                                     size_t start, size_t size, size_t Ls,
                                     const struct ABTable *bptable,
                                     const struct ABTable *bmtable,
                                     const struct ABiTable *iatable_p,
                                     const struct ABiTable *iatable_m,
                                     const struct neighbor *neighbor,
                                     const struct SUn *U,
                                     const struct Fermion *a_y,
                                     const struct Fermion *b_x,
                                     void *rb[]);
size_t qx(omp_doc_ApF_norm)(struct Q(State) *state,
                                  struct Fermion *r_x,
                                  double *local_norm,
                                  size_t start, size_t size, size_t Ls,
                                  const struct ABTable *aptable,
                                  const struct ABTable *amtable,
                                  const struct neighbor *neighbor,
                                  const struct SUn *U,
                                  const struct Fermion *s_x,
                                  const struct Fermion *s_y,
                                  void *rb[]);
size_t qx(omp_do_1mFx_norm)(struct Q(State) *state,
                                  struct Fermion *r_y,
                                  double *local_norm,
                                  size_t start, size_t size, size_t Ls,
                                  const struct neighbor *neighbor,
                                  const struct SUn *U,
                                  const struct Fermion *a_y,
                                  const struct Fermion *b_x,
                                  void *rb[]);
size_t qx(omp_do_1mKF_norm)(struct Q(State) *state,
                                  struct Fermion *r_y,
                                  double *local_norm,
                                  size_t start, size_t size, size_t Ls,
				  const struct KTable *ktable,
                                  const struct neighbor *neighbor,
                                  const struct SUn *U,
                                  const struct Fermion *a_y,
                                  const struct Fermion *b_x,
                                  void *rb[]);
size_t qx(omp_doc_1mKF_norm)(struct Q(State) *state,
				   struct Fermion *r_y,
				   double *local_norm,
				   size_t start, size_t size, size_t Ls,
				   const struct KTable *ktable,
				   const struct neighbor *neighbor,
				   const struct SUn *U,
				   const struct Fermion *a_y,
				   const struct Fermion *b_x,
				   void *rb[]);
size_t qx(omp_f_add2_norm)(struct Q(State) *state,
                                 struct Fermion *r,
                                 double *local_norm,
                                 size_t size, size_t Ls,
                                 double s,
                                 const struct Fermion *b);
size_t qx(omp_f_diff_norm)(struct Q(State) *state,
                                 double *s,
                                 size_t size, size_t Ls,
                                 const struct Fermion *a,
                                 const struct Fermion *b);
size_t qx(omp_f_norm)(struct Q(State) *state,
                            double *s,
                            size_t size, size_t Ls,
                            const struct Fermion *a);
size_t qx(omp_f_dot)(struct Q(State) *state,
			   double *v_r, double *v_i,
			   size_t size, size_t Ls,
			   const struct Fermion *a,
			   const struct Fermion *b);

/* apply operators to (half)fermions */
int qx(debugmesilly)(struct QX(Fermion)          *y,
                     struct Q(State)             *state,
                     const struct Q(Parameters)  *params,
                     const struct QX(Gauge)      *gauge,
                     const char                  *name,
                     const struct QX(Fermion)    *x);
