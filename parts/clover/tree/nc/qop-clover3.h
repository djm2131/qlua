#ifndef QOP_3_CLOVER_1a978487255540d18dde15681e99166a
# define QOP_3_CLOVER_1a978487255540d18dde15681e99166a
  #define QOP_3_CLOVER_DIM 4
  #define QOP_3_CLOVER_FERMION_DIM 4
  #define QOP_3_CLOVER_PROJECTED_FERMION_DIM 2
  #define QOP_3_CLOVER_COLORS 3
  struct QOP_3_CLOVER_State;
  struct QOP_3_CLOVER_Config {
    int    self;                      /* this node QMP id */
    int    master_p;                  /* if != 0, do I/O from this node */
    int    rank;                      /* lattice rank, must be 4 */
    int   *lat;                       /* [rank] */
    int   *net;                       /* [rank] */
    int   *neighbor_up;               /* [rank], QMP ids of neighbors in up dirs */
    int   *neighbor_down;             /* [rank], QMP ids of neighbors in down dirs */
    void (*sublattice)(int lo[],      /*   [rank] */
                       int hi[],      /*   [rank] */
                       int node,      /*   any node QMP id */
                       void *env);    /*   lexical variables */
    void  *env;                       /* lexical variables for sublattice */
  };
  struct QOP_F3_CLOVER_Gauge;
  struct QOP_D3_CLOVER_Gauge;
  struct QOP_F3_CLOVER_Fermion;
  struct QOP_D3_CLOVER_Fermion;
  struct QOP_F3_CLOVER_HalfFermion;
  struct QOP_D3_CLOVER_HalfFermion;
  struct QOP_F3_CLOVER_Deflator;
  struct QOP_F3_CLOVER_HalfFermionMat;
  enum {
     QOP_CLOVER_LOG_NONE              = 0x00,
     QOP_CLOVER_LOG_CG_RESIDUAL       = 0x01,
     QOP_CLOVER_LOG_TRUE_RESIDUAL     = 0x02,
     QOP_CLOVER_FINAL_CG_RESIDUAL     = 0x04,
     QOP_CLOVER_FINAL_DIRAC_RESIDUAL  = 0x08,
     QOP_CLOVER_LOG_EIG_UPDATE1       = 0x10,
     QOP_CLOVER_LOG_EIG_POSTAMBLE     = 0x20,
     QOP_CLOVER_LOG_EVERYTHING        = 0x3f
  };
  const char *QOP_3_CLOVER_version(void);
  const char *QOP_3_CLOVER_signature(struct QOP_3_CLOVER_State *state);
  int QOP_3_CLOVER_parity(struct QOP_3_CLOVER_State *state);
  int QOP_3_CLOVER_performance(double *time_sec,
                                long long *flops,
                                long long *sent,
                                long long *receive,
                                struct QOP_3_CLOVER_State *state);
  int QOP_3_CLOVER_init(struct QOP_3_CLOVER_State **state_ptr,
                        struct QOP_3_CLOVER_Config *config);
  void QOP_3_CLOVER_fini(struct QOP_3_CLOVER_State **state_ptr);
  const char *QOP_3_CLOVER_error(struct QOP_3_CLOVER_State *state);
  int QOP_F3_CLOVER_import_gauge(struct QOP_F3_CLOVER_Gauge **gauge_ptr,
                                  struct QOP_3_CLOVER_State *state,
                                  double kappa,
                                  double c_sw,
                                  double (*U_reader)(int dir,
                                                     const int pos[4],
                                                     int a,
                                                     int b,
                                                     int re_im,
                                                     void *env),
                                  double (*F_reader)(int mu,
                                                     int nu,
                                                     const int pos[4],
                                                     int a,
                                                     int b,
                                                     int re_im,
                                                     void *env),
                                  void *env);
  int QOP_D3_CLOVER_import_gauge(struct QOP_D3_CLOVER_Gauge **gauge_ptr,
                                  struct QOP_3_CLOVER_State *state,
                                  double kappa,
                                  double c_sw,
                                  double (*U_reader)(int dir,
                                                     const int pos[4],
                                                     int a,
                                                     int b,
                                                     int re_im,
                                                     void *env),
                                  double (*F_reader)(int mu,
                                                     int nu,
                                                     const int pos[4],
                                                     int a,
                                                     int b,
                                                     int re_im,
                                                     void *env),
                                  void *env);
  int QOP_3_CLOVER_gauge_float_from_double(struct QOP_F3_CLOVER_Gauge **result,
                                                struct QOP_D3_CLOVER_Gauge *gauge_ptr);
  void QOP_F3_CLOVER_free_gauge(struct QOP_F3_CLOVER_Gauge **gauge_ptr);
  void QOP_D3_CLOVER_free_gauge(struct QOP_D3_CLOVER_Gauge **gauge_ptr);
  int QOP_F3_CLOVER_export_inv_clover(void (*writer)(const int pos[4],
                                                     int a, int i, int b, int j,
                                                     int re_im,
                                                     double value,
                                                     void *env),
                                      void *env,
                                      const struct QOP_F3_CLOVER_Gauge *gauge);
  int QOP_D3_CLOVER_export_inv_clover(void (*writer)(const int pos[4],
                                                     int a, int i, int b, int j,
                                                     int re_im,
                                                     double value,
                                                     void *env),
                                      void *env,
                                      const struct QOP_D3_CLOVER_Gauge *gauge);
  int QOP_F3_CLOVER_import_fermion(struct QOP_F3_CLOVER_Fermion **fermion_ptr,
                                    struct QOP_3_CLOVER_State *state,
                                    double (*reader)(const int pos[4],
                                                     int color,
                                                     int dirac,
                                                     int re_im,
                                                     void *env),
                                    void *env);
  int QOP_D3_CLOVER_import_fermion(struct QOP_D3_CLOVER_Fermion **fermion_ptr,
                                    struct QOP_3_CLOVER_State *state,
                                    double (*reader)(const int pos[4],
                                                     int color,
                                                     int dirac,
                                                     int re_im,
                                                     void *env),
                                    void *env);
  int QOP_F3_CLOVER_allocate_fermion(struct QOP_F3_CLOVER_Fermion **fermion_ptr,
                                   struct QOP_3_CLOVER_State *state);
  int QOP_D3_CLOVER_allocate_fermion(struct QOP_D3_CLOVER_Fermion **fermion_ptr,
                                   struct QOP_3_CLOVER_State *state);
  int QOP_F3_CLOVER_export_fermion(void (*writer)(const int pos[4],
                                                   int color,
                                                   int dirac,
                                                   int re_im,
                                                   double value,
                                                   void *env),
                                    void *env,
                                    const struct QOP_F3_CLOVER_Fermion *fermion);
  int QOP_D3_CLOVER_export_fermion(void (*writer)(const int pos[4],
                                                   int color,
                                                   int dirac,
                                                   int re_im,
                                                   double value,
                                                   void *env),
                                    void *env,
                                    const struct QOP_D3_CLOVER_Fermion *fermion);
  int QOP_F3_CLOVER_blas_from_half_fermion(float *data,
                                                int size,
                                                const struct QOP_F3_CLOVER_HalfFermion *hfermion);
  int QOP_D3_CLOVER_blas_from_half_fermion(double *data,
                                                int size,
                                                const struct QOP_D3_CLOVER_HalfFermion *hfermion);
  int QOP_F3_CLOVER_half_fermion_from_blas(struct QOP_F3_CLOVER_HalfFermion *hfermion,
                                                const float *data,
                                                int size);
  int QOP_D3_CLOVER_half_fermion_from_blas(struct QOP_D3_CLOVER_HalfFermion *hfermion,
                                                const double *data,
                                                int size);
  int QOP_3_CLOVER_half_fermion_size(struct QOP_3_CLOVER_State *state_ptr);
  void QOP_F3_CLOVER_free_fermion(struct QOP_F3_CLOVER_Fermion **fermion_ptr);
  void QOP_D3_CLOVER_free_fermion(struct QOP_D3_CLOVER_Fermion **fermion_ptr);
  int QOP_F3_CLOVER_import_half_fermion(struct QOP_F3_CLOVER_HalfFermion **hfermion_ptr,
                                         struct QOP_3_CLOVER_State *state,
                                         double (*reader)(const int pos[4],
                                                          int color,
                                                          int dirac,
                                                          int re_im,
                                                          void *env),
                                         void *env);
  int QOP_D3_CLOVER_import_half_fermion(struct QOP_D3_CLOVER_HalfFermion **hfermion_ptr,
                                         struct QOP_3_CLOVER_State *state,
                                         double (*reader)(const int pos[4],
                                                          int color,
                                                          int dirac,
                                                          int re_im,
                                                          void *env),
                                         void *env);
  int QOP_F3_CLOVER_allocate_half_fermion(struct QOP_F3_CLOVER_HalfFermion **hfermion_ptr,
                                           struct QOP_3_CLOVER_State *state);
  int QOP_D3_CLOVER_allocate_half_fermion(struct QOP_D3_CLOVER_HalfFermion **hfermion_ptr,
                                           struct QOP_3_CLOVER_State *state);
  int QOP_F3_CLOVER_export_half_fermion(void (*writer)(const int pos[4],
                                                        int color,
                                                        int dirac,
                                                        int re_im,
                                                        double value,
                                                        void *env),
                                        void *env,
                                        const struct QOP_F3_CLOVER_HalfFermion *hfermion);
  int QOP_D3_CLOVER_export_half_fermion(void (*writer)(const int pos[4],
                                                        int color,
                                                        int dirac,
                                                        int re_im,
                                                        double value,
                                                        void *env),
                                        void *env,
                                        const struct QOP_D3_CLOVER_HalfFermion *hfermion);
  void QOP_F3_CLOVER_free_half_fermion(struct QOP_F3_CLOVER_HalfFermion **hfermion_ptr);
  void QOP_D3_CLOVER_free_half_fermion(struct QOP_D3_CLOVER_HalfFermion **hfermion_ptr);
  int QOP_F3_CLOVER_D_operator(struct QOP_F3_CLOVER_Fermion *result,
                                const struct QOP_F3_CLOVER_Gauge *gauge,
                                const struct QOP_F3_CLOVER_Fermion *fermion);
  int QOP_D3_CLOVER_D_operator(struct QOP_D3_CLOVER_Fermion *result,
                                const struct QOP_D3_CLOVER_Gauge *gauge,
                                const struct QOP_D3_CLOVER_Fermion *fermion);
  int QOP_F3_CLOVER_D_operator_conjugated(struct QOP_F3_CLOVER_Fermion *result,
                                           const struct QOP_F3_CLOVER_Gauge *gauge,
                                           const struct QOP_F3_CLOVER_Fermion *fermion);
  int QOP_D3_CLOVER_D_operator_conjugated(struct QOP_D3_CLOVER_Fermion *result,
                                           const struct QOP_D3_CLOVER_Gauge *gauge,
                                           const struct QOP_D3_CLOVER_Fermion *fermion);
  int QOP_F3_CLOVER_M_operator(struct QOP_F3_CLOVER_HalfFermion *result,
                             const struct QOP_F3_CLOVER_Gauge *gauge,
                             const struct QOP_F3_CLOVER_HalfFermion *fermion);
  int QOP_D3_CLOVER_M_operator(struct QOP_D3_CLOVER_HalfFermion *result,
                             const struct QOP_D3_CLOVER_Gauge *gauge,
                             const struct QOP_D3_CLOVER_HalfFermion *fermion);
  int QOP_F3_CLOVER_M_operator_conjugated(struct QOP_F3_CLOVER_HalfFermion *result,
                                        const struct QOP_F3_CLOVER_Gauge *gauge,
                                        const struct QOP_F3_CLOVER_HalfFermion *fermion);
  int QOP_D3_CLOVER_M_operator_conjugated(struct QOP_D3_CLOVER_HalfFermion *result,
                                        const struct QOP_D3_CLOVER_Gauge *gauge,
                                        const struct QOP_D3_CLOVER_HalfFermion *fermion);
  int QOP_F3_CLOVER_create_deflator(struct QOP_F3_CLOVER_Deflator **defl_ptr,
                                 struct QOP_3_CLOVER_State *state,
                                 int nev, int vsize, double eps, int umax);
  int QOP_F3_CLOVER_create_deflator_inplace(struct QOP_F3_CLOVER_Deflator       **defl_ptr,
                                          const struct QOP_F3_CLOVER_Gauge    *gauge,
                                          struct QOP_F3_CLOVER_HalfFermionMat **hfm_ptr,
                                          int                               hfm_nev,
                                          int                               eigcg_vmax,
                                          int                               eigcg_nev,
                                          double                            eigcg_eps,
                                          int                               eigcg_umax);
  int QOP_F3_CLOVER_alloc_half_fermion_matrix(struct QOP_F3_CLOVER_HalfFermionMat **hfm_ptr,
                                               struct QOP_3_CLOVER_State *state,
                                               int ncol);
  int QOP_F3_CLOVER_blas_view_half_fermion_matrix(struct QOP_F3_CLOVER_HalfFermionMat *hfm_ptr,
                                                  int        *nrow_loc,
                                                  int        *ncol,
                                                  float      **blas_ptr,
                                                  int        *blas_ld);
  void QOP_F3_CLOVER_free_half_fermion_matrix(struct QOP_F3_CLOVER_HalfFermionMat **hfm_ptr);
  void QOP_F3_CLOVER_free_deflator(struct QOP_F3_CLOVER_Deflator **defl_ptr);
  int QOP_F3_CLOVER_deflator_eigcg_reset(struct QOP_F3_CLOVER_Deflator *defl_ptr);
  int QOP_F3_CLOVER_deflator_eigcg_stop(struct QOP_F3_CLOVER_Deflator *defl_ptr);
  int QOP_F3_CLOVER_deflator_eigcg_resume(struct QOP_F3_CLOVER_Deflator *defl_ptr);
  int QOP_F3_CLOVER_deflator_eigcg_is_stopped(struct QOP_F3_CLOVER_Deflator *defl_ptr);
  int QOP_F3_CLOVER_deflator_current_dim(struct QOP_F3_CLOVER_Deflator *defl_ptr);
  int QOP_F3_CLOVER_deflator_extract_vector(struct QOP_F3_CLOVER_HalfFermion *hfermion_ptr,
                                            const struct QOP_F3_CLOVER_Deflator *defl_ptr,
                                            int idx);
  int QOP_F3_CLOVER_deflator_start_load(struct QOP_F3_CLOVER_Deflator *defl_ptr);
  int QOP_F3_CLOVER_deflator_add_vector(const struct QOP_F3_CLOVER_Gauge *gauge,
                                      struct QOP_F3_CLOVER_Deflator *deflator,
                                      const struct QOP_F3_CLOVER_HalfFermion *hfermion);
  int QOP_F3_CLOVER_deflator_stop_load(struct QOP_F3_CLOVER_Deflator *defl_ptr);
  int QOP_F3_CLOVER_deflator_eigen(int n, double *eigen_values,
                                struct QOP_F3_CLOVER_Deflator *defl_ptr);
  int QOP_F3_CLOVER_D_CG(struct QOP_F3_CLOVER_Fermion *result,
                          int *out_iterations,
                          double *out_epsilon,
                          const struct QOP_F3_CLOVER_Fermion *chi_0,
                          const struct QOP_F3_CLOVER_Gauge *gauge,
                          const struct QOP_F3_CLOVER_Fermion *rhs,
                          int max_iteration,
                          double epsilon,
                          unsigned int options);
  int QOP_D3_CLOVER_D_CG(struct QOP_D3_CLOVER_Fermion *result,
                          int *out_iterations,
                          double *out_epsilon,
                          const struct QOP_D3_CLOVER_Fermion *x_0,
                          const struct QOP_D3_CLOVER_Gauge *gauge,
                          const struct QOP_D3_CLOVER_Fermion *rhs,
                          int max_iteration,
                          double epsilon,
                          unsigned int options);
  int QOP_F3_CLOVER_MxM_CG(struct QOP_F3_CLOVER_HalfFermion *result,
                            int *out_iterations,
                            double *out_epsilon,
                            const struct QOP_F3_CLOVER_HalfFermion *chi_0,
                            const struct QOP_F3_CLOVER_Gauge *gauge,
                            const struct QOP_F3_CLOVER_HalfFermion *rhs,
                            int max_iteration,
                            double epsilon,
                            unsigned int options);
  int QOP_D3_CLOVER_MxM_CG(struct QOP_D3_CLOVER_HalfFermion *result,
                            int *out_iterations,
                            double *out_epsilon,
                            const struct QOP_D3_CLOVER_HalfFermion *x_0,
                            const struct QOP_D3_CLOVER_Gauge *gauge,
                            const struct QOP_D3_CLOVER_HalfFermion *rhs,
                            int max_iteration,
                            double epsilon,
                            unsigned int options);
  int QOP_3_CLOVER_mixed_D_CG(struct QOP_D3_CLOVER_Fermion *result,
                             int *out_iterations,
                             double *out_epsilon,
                             const struct QOP_D3_CLOVER_Fermion *x_0,
                             const struct QOP_D3_CLOVER_Gauge *gauge,
                             const struct QOP_D3_CLOVER_Fermion *rhs,
                             int f_iterations,
                             double f_epsilon,
                             int max_iteration,
                             double epsilon,
                             unsigned int options);
  int QOP_3_CLOVER_deflated_mixed_D_CG(struct QOP_D3_CLOVER_Fermion *result,
                                     int *out_iterations,
                                     double *out_epsilon,
                                     const struct QOP_D3_CLOVER_Fermion *chi_0,
                                     const struct QOP_D3_CLOVER_Gauge *gauge,
                                     const struct QOP_D3_CLOVER_Fermion *rhs,
                                     struct QOP_F3_CLOVER_Deflator *deflator,
                                     int f_iterations,
                                     double f_epsilon,
                                     int max_iteration,
                                     double epsilon,
                                     unsigned int options);
  int QOP_F3_CLOVER_MxM_poly(struct QOP_F3_CLOVER_HalfFermion *result,
                           struct QOP_F3_CLOVER_HalfFermion *result_prev,
                           const struct QOP_F3_CLOVER_Gauge *gauge,
                           const struct QOP_F3_CLOVER_HalfFermion *psi,
                           int n,
                           const double a[/* n */],
                           const double b[/* n */],
                           const double c[/* n */]);
  int QOP_D3_CLOVER_MxM_poly(struct QOP_D3_CLOVER_HalfFermion *result,
                           struct QOP_D3_CLOVER_HalfFermion *result_prev,
                           const struct QOP_D3_CLOVER_Gauge *gauge,
                           const struct QOP_D3_CLOVER_HalfFermion *psi,
                           int n,
                           const double a[/* n */],
                           const double b[/* n */],
                           const double c[/* n */]);
  int QOP_3_CLOVER_poly_normalize(int n,
                                double a[/* n */],
                                double b[/* n */],
                                double c[/* n */],
                                double x0,
                                double tol);
  int QOP_F3_CLOVER_madd_fermion(struct QOP_F3_CLOVER_Fermion *r,
                                  const struct QOP_F3_CLOVER_Fermion *a,
                                  double alpha,
                                  const struct QOP_F3_CLOVER_Fermion *b);
  int QOP_D3_CLOVER_madd_fermion(struct QOP_D3_CLOVER_Fermion *r,
                                  const struct QOP_D3_CLOVER_Fermion *a,
                                  double alpha,
                                  const struct QOP_D3_CLOVER_Fermion *b);
  int QOP_F3_CLOVER_madd_half_fermion(struct QOP_F3_CLOVER_HalfFermion *r,
                                       const struct QOP_F3_CLOVER_HalfFermion *a,
                                       double alpha,
                                       const struct QOP_F3_CLOVER_HalfFermion *b);
  int QOP_D3_CLOVER_madd_half_fermion(struct QOP_D3_CLOVER_HalfFermion *r,
                                       const struct QOP_D3_CLOVER_HalfFermion *a,
                                       double alpha,
                                       const struct QOP_D3_CLOVER_HalfFermion *b);
  int QOP_F3_CLOVER_dot_fermion(double *r_re,
                                 double *r_im,
                                 const struct QOP_F3_CLOVER_Fermion *a,
                                 const struct QOP_F3_CLOVER_Fermion *b);
  int QOP_D3_CLOVER_dot_fermion(double *r_re,
                                 double *r_im,
                                 const struct QOP_D3_CLOVER_Fermion *a,
                                 const struct QOP_D3_CLOVER_Fermion *b);
  int QOP_F3_CLOVER_dot_half_fermion(double *r_re,
                                      double *r_im,
                                      const struct QOP_F3_CLOVER_HalfFermion *a,
                                      const struct QOP_F3_CLOVER_HalfFermion *b);
  int QOP_D3_CLOVER_dot_half_fermion(double *r_re,
                                      double *r_im,
                                      const struct QOP_D3_CLOVER_HalfFermion *a,
                                      const struct QOP_D3_CLOVER_HalfFermion *b);
  int QOP_F3_CLOVER_norm2_fermion(double *r,
                                   const struct QOP_F3_CLOVER_Fermion *a);
  int QOP_D3_CLOVER_norm2_fermion(double *r_re,
                                   const struct QOP_D3_CLOVER_Fermion *a);
  int QOP_F3_CLOVER_norm2_half_fermion(double *r_re,
                                        const struct QOP_F3_CLOVER_HalfFermion *a);
  int QOP_D3_CLOVER_norm2_half_fermion(double *r_re,
                                        const struct QOP_D3_CLOVER_HalfFermion *a);
# if defined(QOP_CLOVER_DEFAULT_PRECISION) && (QOP_CLOVER_DEFAULT_PRECISION == 'F')
   #define QOP_3_CLOVER_import_gauge QOP_3_CLOVER_import_gauge
   #define QOP_3_CLOVER_free_gauge QOP_F3_CLOVER_free_gauge
   #define QOP_3_CLOVER_Gauge QOP_F3_CLOVER_Gauge
   #define QOP_3_CLOVER_import_fermion QOP_F3_CLOVER_import_fermion
   #define QOP_3_CLOVER_export_fermion QOP_F3_CLOVER_export_fermion
   #define QOP_3_CLOVER_allocate_fermion QOP_F3_CLOVER_allocate_fermion
   #define QOP_3_CLOVER_blas_from_half_fermion QOP_F3_CLOVER_blas_from_half_fermion
   #define QOP_3_CLOVER_half_fermion_from_blas QOP_F3_CLOVER_half_fermion_from_blas
   #define QOP_3_CLOVER_free_fermion QOP_F3_CLOVER_free_fermion
   #define QOP_3_CLOVER_Fermion QOP_F3_CLOVER_Fermion
   #define QOP_3_CLOVER_import_half_fermion QOP_F3_CLOVER_import_half_fermion
   #define QOP_3_CLOVER_export_half_fermion QOP_F3_CLOVER_export_half_fermion
   #define QOP_3_CLOVER_allocate_half_fermion QOP_F3_CLOVER_allocate_half_fermion
   #define QOP_3_CLOVER_free_half_fermion QOP_F3_CLOVER_free_half_fermion
   #define QOP_3_CLOVER_HalfFermion QOP_F3_CLOVER_HalfFermion
   #define QOP_3_CLOVER_D_operator QOP_F3_CLOVER_D_operator
   #define QOP_3_CLOVER_D_operator_conjugated QOP_F3_CLOVER_D_operator_conjugated
   #define QOP_CLOVER_M_operator QOP_F3_CLOVER_M_operator
   #define QOP_CLOVER_M_operator_conjugated QOP_F3_CLOVER_M_operator_conjugated
   #define QOP_3_CLOVER_D_CG QOP_F3_CLOVER_D_CG
   #define QOP_3_CLOVER_MxM_CG QOP_F3_CLOVER_MxM_CG
   #define QOP_3_CLOVER_MxM_poly QOP_F3_CLOVER_MxM_poly
   #define QOP_3_CLOVER_madd_fermion QOP_F3_CLOVER_madd_fermion
   #define QOP_3_CLOVER_madd_half_fermion QOP_F3_CLOVER_madd_half_fermion
   #define QOP_3_CLOVER_dot_fermion QOP_F3_CLOVER_dot_fermion
   #define QOP_3_CLOVER_dot_half_fermion QOP_F3_CLOVER_dot_half_fermion
   #define QOP_3_CLOVER_norm2_fermion QOP_F3_CLOVER_norm2_fermion
   #define QOP_3_CLOVER_norm2_half_fermion QOP_F3_CLOVER_norm2_half_fermion
# endif
# if defined(QOP_CLOVER_DEFAULT_PRECISION) && (QOP_CLOVER_DEFAULT_PRECISION == 'D')
    #define QOP_3_CLOVER_import_gauge QOP_D3_CLOVER_import_gauge
    #define QOP_3_CLOVER_free_gauge QOP_D3_CLOVER_free_gauge
    #define QOP_3_CLOVER_Gauge QOP_D3_CLOVER_Gauge
    #define QOP_3_CLOVER_import_fermion QOP_D3_CLOVER_import_fermion
    #define QOP_3_CLOVER_export_fermion QOP_D3_CLOVER_export_fermion
    #define QOP_3_CLOVER_allocate_fermion QOP_D3_CLOVER_allocate_fermion
    #define QOP_3_CLOVER_blas_from_half_fermion QOP_D3_CLOVER_blas_from_half_fermion
    #define QOP_3_CLOVER_half_fermion_from_blas QOP_D3_CLOVER_half_fermion_from_blas
    #define QOP_3_CLOVER_free_fermion QOP_D3_CLOVER_free_fermion
    #define QOP_3_CLOVER_Fermion QOP_D3_CLOVER_Fermion
    #define QOP_3_CLOVER_import_half_fermion QOP_D3_CLOVER_import_half_fermion
    #define QOP_3_CLOVER_export_half_fermion QOP_D3_CLOVER_export_half_fermion
    #define QOP_3_CLOVER_allocate_half_fermion QOP_D3_CLOVER_allocate_half_fermion
    #define QOP_3_CLOVER_free_half_fermion QOP_D3_CLOVER_free_half_fermion
    #define QOP_3_CLOVER_HalfFermion QOP_D3_CLOVER_HalfFermion
    #define QOP_3_CLOVER_D_operator QOP_D3_CLOVER_D_operator
    #define QOP_3_CLOVER_D_operator_conjugated QOP_D3_CLOVER_D_operator_conjugated
    #define QOP_CLOVER_M_operator QOP_D3_CLOVER_M_operator
    #define QOP_CLOVER_M_operator_conjugated QOP_D3_CLOVER_M_operator_conjugated
    #define QOP_3_CLOVER_D_CG QOP_D3_CLOVER_D_CG
    #define QOP_3_CLOVER_MxM_CG QOP_D3_CLOVER_MxM_CG
    #define QOP_3_CLOVER_MxM_poly QOP_D3_CLOVER_MxM_poly
    #define QOP_3_CLOVER_madd_fermion QOP_D3_CLOVER_madd_fermion
    #define QOP_3_CLOVER_madd_half_fermion QOP_D3_CLOVER_madd_half_fermion
    #define QOP_3_CLOVER_dot_fermion QOP_D3_CLOVER_dot_fermion
    #define QOP_3_CLOVER_dot_half_fermion QOP_D3_CLOVER_dot_half_fermion
    #define QOP_3_CLOVER_norm2_fermion QOP_D3_CLOVER_norm2_fermion
    #define QOP_3_CLOVER_norm2_half_fermion QOP_D3_CLOVER_norm2_half_fermion
# endif
#endif /* !defined(QOP_3_CLOVER_1a978487255540d18dde15681e99166a) */
