struct QX(Fermion);
struct QX(HalfFermion);
struct QX(HalfFermionMat);
struct QX(VectorFermion);
struct QX(Gauge);
struct qx(MxM_workspace);

#include "deflator-x.h"

/* CLOVER types */
struct QX(Fermion) {
  struct Q(State) *state;
  size_t size;
  struct FermionX *even;  
  struct FermionX *odd;
};

struct QX(HalfFermion) {
  struct Q(State) *state;
  size_t size;
  struct FermionX *even;  
};

struct QX(HalfFermionMat) {
  struct Q(State) *state;
  size_t mem_size;
  qx(defl_mat) m;
};

struct QX(Gauge) {
  struct Q(State) *state;
  size_t size;
  struct SUn *g_data;
  struct CloverX *ce_data;
  struct CloverX *co_data;
  struct CloverX *cox_data;
};

struct qx(MxM_workspace) {
    struct Q(State)        *state;
    const struct QX(Gauge) *gauge;
    struct FermionX         *tmp_e;
    struct FermionX         *tmp2_e;
    struct FermionX         *tmp_o;
    long long              *flops;
    long long              *sent;
    long long              *received;
};

/* allocation routines */
void *qx(allocate_eo)(struct Q(State) *state,
                      size_t *size, void **aligned_ptr,
                      size_t hdr_size, int even_count, int odd_count);
void *qx(step_even)(struct Q(State) *state, void *aligned_ptr);
void *qx(step_odd)(struct Q(State) *state, void *aligned_ptr);

/* data interface routines types */
void qx(x_import)(struct eo_lattice *eo,
                  struct FermionX *data, 
                  double (*reader)(const int pos[Q(DIM)],
                                   int color,
                                   int dirac, 
                                   int re_im,
                                   void *env),
                  void *env);
void qx(x_export)(struct eo_lattice *eo,
                  const struct FermionX *data, 
                  void (*writer)(const int pos[Q(DIM)],
                                 int color,
                                 int dirac, 
                                 int re_im,
                                 double value,
                                 void *env),
                  void *env);

/* Projections */
typedef size_t (*qx(Up_project))(struct ProjectedFermionX *r,
                                       size_t size,
                                       const struct up_pack *link,
                                       const struct SUn *U,
                                       const struct FermionX *f);
typedef size_t (*qx(Down_project))(struct ProjectedFermionX  *r,
                                         size_t size,
                                         const struct down_pack *link,
                                         const struct FermionX *f);
size_t qx(proj_g0plus)(struct ProjectedFermionX  *r,
                             size_t size,
                             const struct down_pack *link,
                             const struct FermionX *f);
size_t qx(proj_g1plus)(struct ProjectedFermionX  *r,
                             size_t size,
                             const struct down_pack *link,
                             const struct FermionX *f);
size_t qx(proj_g2plus)(struct ProjectedFermionX  *r,
                             size_t size,
                             const struct down_pack *link,
                             const struct FermionX *f);
size_t qx(proj_g3plus)(struct ProjectedFermionX  *r,
                             size_t size,
                             const struct down_pack *link,
                             const struct FermionX *f);
size_t qx(proj_g0minus)(struct ProjectedFermionX  *r,
                              size_t size,
                              const struct down_pack *link,
                              const struct FermionX *f);
size_t qx(proj_g1minus)(struct ProjectedFermionX  *r,
                              size_t size,
                              const struct down_pack *link,
                              const struct FermionX *f);
size_t qx(proj_g2minus)(struct ProjectedFermionX  *r,
                              size_t size,
                              const struct down_pack *link,
                              const struct FermionX *f);
size_t qx(proj_g3minus)(struct ProjectedFermionX  *r,
                              size_t size,
                              const struct down_pack *link,
                              const struct FermionX *f);
size_t qx(proj_Ucg0plus)(struct ProjectedFermionX  *r,
                               size_t size,
                               const struct up_pack *link,
                               const struct SUn *U,
                               const struct FermionX *f);
size_t qx(proj_Ucg1plus)(struct ProjectedFermionX  *r,
                               size_t size,
                               const struct up_pack *link,
                               const struct SUn *U,
                               const struct FermionX *f);
size_t qx(proj_Ucg2plus)(struct ProjectedFermionX  *r,
                               size_t size,
                               const struct up_pack *link,
                               const struct SUn *U,
                               const struct FermionX *f);
size_t qx(proj_Ucg3plus)(struct ProjectedFermionX  *r,
                               size_t size,
                               const struct up_pack *link,
                               const struct SUn *U,
                               const struct FermionX *f);
size_t qx(proj_Ucg0minus)(struct ProjectedFermionX  *r,
                                size_t size,
                                const struct up_pack *link,
                                const struct SUn *U,
                                const struct FermionX *f);
size_t qx(proj_Ucg1minus)(struct ProjectedFermionX  *r,
                                size_t size,
                                const struct up_pack *link,
                                const struct SUn *U,
                                const struct FermionX *f);
size_t qx(proj_Ucg2minus)(struct ProjectedFermionX  *r,
                                size_t size,
                                const struct up_pack *link,
                                const struct SUn *U,
                                const struct FermionX *f);
size_t qx(proj_Ucg3minus)(struct ProjectedFermionX *r,
                                size_t size,
                                const struct up_pack *link,
                                const struct SUn *U,
                                const struct FermionX *f);

/* projection tables */
/*  normal projection */
extern qx(Up_project) qx(up_project_n)[Q(DIM)];
extern qx(Down_project) qx(down_project_n)[Q(DIM)];
/*  conjugated projection */
extern qx(Up_project) qx(up_project_x)[Q(DIM)];
extern qx(Down_project) qx(down_project_x)[Q(DIM)];
/* compute projections on the boundary and fill the send buffers */
void qx(boundary)(struct eo_lattice *xy,
                  const qx(Up_project) up_proj[],
                  const qx(Down_project) down_proj[],
                  const struct SUn *U,
                  const struct FermionX *src_y,
                  long long *flops);

/* Backend controled structure sizes */
size_t qx(sizeof_fermion)(size_t volume);
size_t qx(sizeof_gauge)(size_t volume);
size_t qx(sizeof_clover)(size_t volume);
size_t qx(sizeof_vfermion)(size_t volume, size_t count);
size_t qx(strideof_vfermion)(size_t volume);

/* qa0 level data access routines */
void qx(put_fermion)(struct FermionX *data, size_t pos, const double r[]);
void qx(get_fermion)(double r[], const struct FermionX *data, size_t pos);
void qx(put_gauge)(struct SUn *ptr, size_t pos, const double r[]);
void qx(put_clover_lo)(struct CloverX *ptr, size_t pos, const double r[]);
void qx(put_clover_hi)(struct CloverX *ptr, size_t pos, const double r[]);
void qx(get_clover_lo)(double r[], const struct CloverX *ptr, size_t pos);
void qx(get_clover_hi)(double r[], const struct CloverX *ptr, size_t pos);
void qx(fermion2blas)(void *data, const struct FermionX *f, size_t size);
void qx(blas2fermion)(struct FermionX *f, size_t size, const void *data);

/* Linear algebra on fermions */
void qx(f_zero)(struct FermionX *dst, 
                size_t size);
void qx(f_copy)(struct FermionX *dst, 
                size_t size,
                const struct FermionX *src);
size_t qx(f_dot)(double *v_r, double *v_i,
                       size_t size,
                       const struct FermionX *a,
                       const struct FermionX *b);
size_t qx(f_add3)(struct FermionX *r,
                        size_t size,
                        const struct FermionX *a,
                        double s,
                        const struct FermionX *b);
size_t qx(f_add2)(struct FermionX *r,
                        size_t size,
                        double s,
                        const struct FermionX *b);
size_t qx(f_cadd2)(struct FermionX *r,
                         size_t size,
                         double zr, double zi,
                         const struct FermionX *b);
size_t qx(f_add2_norm)(struct FermionX *r,
                             double *local_norm,
                             size_t size,
                             double s,
                             const struct FermionX *b);
size_t qx(f_rmul1)(struct FermionX *r,
                         size_t size,
                         double s);
size_t qx(f_add2x)(struct FermionX *r,
                         size_t size,
                         double s,
                         const struct FermionX *b);
size_t qx(f_norm)(double *s,
                        size_t size,
                        const struct FermionX *a);
size_t qx(f_diff_norm)(double *s,
                             size_t size,
                             const struct FermionX *a,
                             const struct FermionX *b);

/* algebra for arrays of fermions */

// XXX ?
// size_t qx(fv_zero)(struct vFermion *dst, 
//                         size_t size, size_t len);

/* fv[fv_begin + (0 .. len-1)] = gv[gv_begin + (0 .. len-1)]
*/
size_t qx(vf_copy)(
        size_t size, size_t len,
        struct vFermion *fv, size_t fv_stride, size_t fv_begin,
        const struct vFermion *gv, size_t gv_stride, size_t gv_begin
        );
/*
 * set fv[idx] = x
*/
size_t qx(vf_put)(
        size_t size,
        struct vFermion *fv, size_t fv_stride, size_t fv_idx,
        const struct FermionX *x
        );

/*
 * read x = fv[idx]
*/
size_t qx(vf_get)(
        size_t size,
        struct FermionX *x,
        const struct vFermion *fv, size_t fv_stride, size_t fv_idx
        );

/*
*   g = fv[fv_begin + (0 .. f_vlen-1)] . v
*   v is a complex vector [fv_len] indexed as [re:0/im:1 + 2 * i]
*/
size_t qx(vf_dot_vz)(
        size_t size,
        struct FermionX *g,
        const struct vFermion *fv, size_t fv_stride, size_t fv_begin, size_t fv_len,
        const double *v
        );

/*
*   gv[gv_begin + (0 .. gv_len-1)] = fv[fv_begin + (0 .. f_len - 1)] . m
*   m is a complex matrix [fv_len*gv_len] indexed as [re:0/im:1 + 2 * (row + ldm * col) ]
*/
size_t qx(vf_dot_mz)(
        size_t size,
        struct vFermion *gv, size_t gv_stride, size_t gv_begin, size_t gv_len,
        const struct vFermion *fv, size_t fv_stride, size_t fv_begin, size_t fv_len,
        const double *m, size_t ldm
        );

/*  This does not include global reduction
 *  c[i] = herm(fv[fv_begin+i]) * g 
 *      for all i = (0 .. fv_len-1)
 *  c is complex vector as [re:0/im:1 + 2 * i]
 */
size_t qx(do_vfH_dot_f)(
        size_t size,
        double *c,
        const struct vFermion *fv, size_t fv_stride, size_t fv_begin, size_t fv_len,
        const struct FermionX *g);

/* This does not include global reduction
 * c[i,j] = herm(fv[fv_begin + i]) . g[gv_begin+j] 
 *      for all i = (0 .. fv_len-1), 
 *              j = (0 .. gv_len-1),
 * c is a complex matrix as [re:0/im:1 + 2 * (i + ldc * j)]
 */
size_t qx(do_vfH_dot_vf)(
        size_t size,
        double *c, size_t ldc,
        const struct vFermion *fv, size_t fv_stride, size_t fv_begin, size_t fv_len,
        const struct vFermion *gv, size_t gv_stride, size_t gv_begin, size_t gv_len
        );

/* basic matrices */
size_t qx(op_norm2)(double *global_norm,
                          const struct QX(Fermion) *psi,
                          struct Q(State) *state);
size_t qx(do_A)(struct FermionX *r_x,
                      size_t size,
                      const struct CloverX *C,
                      const struct FermionX *s_x);

/* basic A+B, A, B, and their combinations  */
size_t qx(do_ApB)(struct FermionX *r_x,
                        size_t start, size_t size,
                        const struct neighbor *neighbor,
                        const struct SUn *U,
                        const struct CloverX *C,
                        const struct FermionX *s_x,
                        const struct FermionX *s_y,
                        void *rb[]);
size_t qx(do_AxpBx)(struct FermionX *r_x,
                          size_t start, size_t size,
                          const struct neighbor *neighbor,
                          const struct SUn *U,
                          const struct CloverX *C,
                          const struct FermionX *s_x,
                          const struct FermionX *s_y,
                          void *rb[]);
size_t qx(do_CmB)(struct FermionX *r_x,
                        size_t start, size_t size,
                        const struct neighbor *neighbor,
                        const struct SUn *U,
                        const struct FermionX *s_x,
                        const struct FermionX *s_y,
                        void *rb[]);
size_t qx(do_AmB)(struct FermionX *r_x,
                        size_t start, size_t size,
                        const struct neighbor *neighbor,
                        const struct SUn *U,
                        const struct CloverX *C,
                        const struct FermionX *s_x,
                        const struct FermionX *s_y,
                        void *rb[]);
size_t qx(do_AmB_norm)(struct FermionX *r_x,
                             double *local_norm,
                             size_t start, size_t size,
                             const struct neighbor *neighbor,
                             const struct SUn *U,
                             const struct CloverX *C,
                             const struct FermionX *s_x,
                             const struct FermionX *s_y,
                             void *rb[]);
size_t qx(do_AxmBx)(struct FermionX *r_x,
                          size_t start, size_t size,
                          const struct neighbor *neighbor,
                          const struct SUn *U,
                          const struct CloverX *C,
                          const struct FermionX *s_x,
                          const struct FermionX *s_y,
                          void *rb[]);
size_t qx(do_AB)(struct FermionX *r_x,
                       size_t start, size_t size,
                       const struct neighbor *neighbor,
                       const struct SUn *U,
                       const struct CloverX *C,
                       const struct FermionX *s_y,
                       void *rb[]);
size_t qx(do_AxBx)(struct FermionX *r_x,
                         size_t start, size_t size,
                         const struct neighbor *neighbor,
                         const struct SUn *U,
                         const struct CloverX *C,
                         const struct FermionX *s_y,
                         void *rb[]);

/* even/odd level routines */
void qx(op_CmB)(struct FermionX *res_x,
                struct eo_lattice *r_x,
                const struct SUn *g_data,
                const struct FermionX *a_x,
                const struct FermionX *b_y,
                long long *flops,
                long long *sent,
                long long *received);
void qx(op_A)(struct FermionX *r_x,
              struct eo_lattice *xy,
              const struct CloverX *cx_data,
              const struct FermionX *s_x,
              long long *flops);
void qx(op_AB)(struct FermionX *r_x,
               struct eo_lattice *xy,
               const struct SUn *g_data,
               const struct CloverX *cx_data,
               const struct FermionX *s_y,
               long long *flops,
               long long *sent,
               long long *received);
void qx(op_AmB)(struct FermionX *r_x,
                struct eo_lattice *xy,
                const struct SUn *g_data,
                const struct CloverX *cx_data,
                const struct FermionX *s_x,
                const struct FermionX *s_y,
                long long *flops,
                long long *sent,
                long long *received);
void qx(op_AmB_norm)(struct FermionX *r_x,
                     double *global_norm,
                     struct eo_lattice *xy,
                     const struct SUn *g_data,
                     const struct CloverX *cx_data,
                     const struct FermionX *s_x,
                     const struct FermionX *s_y,
                     long long *flops,
                     long long *sent,
                     long long *received);
void qx(op_AxBx)(struct FermionX *r_x,
                 struct eo_lattice *xy,
                 const struct SUn *g_data,
                 const struct CloverX *cx_data,
                 const struct FermionX *s_y,
                 long long *flops,
                 long long *sent,
                 long long *received);
void qx(op_AxmBx)(struct FermionX *r_x,
                  struct eo_lattice *xy,
                  const struct SUn *g_data,
                  const struct CloverX *cx_data,
                  const struct FermionX *s_x,
                  const struct FermionX *s_y,
                  long long *flops,
                  long long *sent,
                  long long *received);
void qx(op_ApB)(struct FermionX *r_x,
                struct eo_lattice *xy,
                const struct SUn *U,
                const struct CloverX *C,
                const struct FermionX *a_x,
                const struct FermionX *a_y,
                long long *flops,
                long long *sent,
                long long *received);
void qx(op_AxpBx)(struct FermionX *r_x,
                  struct eo_lattice *xy,
                  const struct SUn *U,
                  const struct CloverX *C,
                  const struct FermionX *a_x,
                  const struct FermionX *a_y,
                  long long *flops,
                  long long *sent,
                  long long *received);
void qx(op_even_M)(struct FermionX *r_x,
                   struct Q(State) *state,
                   const struct QX(Gauge) *gauge,
                   const struct FermionX *a_x,
                   long long *flops,
                   long long *sent,
                   long long *received,
                   struct FermionX *tmp_y);
void qx(op_even_Mn)(struct FermionX *r_x,
                    double *global_norm,
                    struct Q(State) *state,
                    const struct QX(Gauge) *gauge,
                    const struct FermionX *a_x,
                    long long *flops,
                    long long *sent,
                    long long *received,
                    struct FermionX *tmp_y);
void qx(op_even_Mx)(struct FermionX *r_x,
                    struct Q(State) *state,
                    const struct QX(Gauge) *gauge,
                    const struct FermionX *a_x,
                    long long *flops,
                    long long *sent,
                    long long *received,
                    struct FermionX *tmp_y);

/* logging */
void qx(zprint)(struct Q(State) *state,
                const char *source,
                const char *fmt,
                ...);
/* parts of the CG solver */
void qx(cg_precondition)(struct FermionX *chi_e,
                         struct Q(State) *state,
                         const struct QX(Gauge) *gauge,
                         const struct FermionX *eta_e,
                         const struct FermionX *eta_o,
                         long long *flops,
                         long long *sent,
                         long long *received,
                         struct FermionX *t0_e,
                         struct FermionX *t0_o);
void qx(cg_inflate)(struct FermionX *psi_o,
                    struct Q(State) *state,
                    const struct QX(Gauge) *gauge,
                    const struct FermionX *eta_o,
                    const struct FermionX *psi_e,
                    long long *flops,
                    long long *sent,
                    long long *received,
                    struct FermionX *t_o);
double qx(cg_dirac_error)(const struct FermionX *psi_e,
                          const struct FermionX *psi_o,
                          struct Q(State) *state,
                          const struct QX(Gauge) *gauge,
                          const struct FermionX *eta_e,
                          const struct FermionX *eta_o,
                          long long *flops,
                          long long *sent,
                          long long *received,
                          struct FermionX *t0_e,
                          struct FermionX *t0_o);
void qx(cg_log)(double cg_res, const char *source, int iter,
                const struct FermionX *xi_e,
                struct Q(State) *state,
                const struct QX(Gauge) *gauge,
                const struct FermionX *chi_e,
                long long *flops,
                long long *sent,
                long long *received,
                unsigned int options,
                struct FermionX *t0_e,
                struct FermionX *t1_e,
                struct FermionX *t0_o,
                struct FermionX *t1_o);

void qx(cg_operator)(struct FermionX            *res_e,
                     const struct FermionX      *psi_e,
                     struct qx(MxM_workspace)   *ws);

CG_STATUS qx(cg_solver)(struct FermionX *psi_e,
                        const char *source,
                        int *out_iter,
                        double *out_epsilon,
                        struct Q(State) *state,
                        const struct QX(Gauge) *gauge,
                        const struct FermionX *chi_e,
                        struct QF(Deflator) *deflator,
                        int max_iter,
                        double epsilon,
                        unsigned options,
                        long long *flops,
                        long long *sent,
                        long long *received,
                        struct FermionX *rho_e,
                        struct FermionX *pi_e,
                        struct FermionX *zeta_e,
                        struct FermionX *t0_e,
                        struct FermionX *t1_e,
                        struct FermionX *t0_o,
                        struct FermionX *t1_o);

/*
 *  compute x <- x + alpha p
 *          p <- r + beta p
 */
size_t qx(cg_xp)(struct FermionX *x,
                       struct FermionX *p,
                       size_t size,
                       double alpha,
                       double beta,
                       const struct FermionX *r);
