#ifndef QOP_MDWF_0bd50d0caeec4311a0d7c183032c43c2
# define QOP_MDWF_0bd50d0caeec4311a0d7c183032c43c2
  #define QOP_MDWF_DIM 4
  #define QOP_MDWF_FERMION_DIM 4
  #define QOP_MDWF_PROJECTED_FERMION_DIM 2
  #define QOP_MDWF_COLORS 3
  struct QOP_MDWF_State;
  struct QOP_MDWF_Parameters;
  enum QOP_MDWF_eopc_type {
    QOP_MDWF_NONE        = 0,
    QOP_MDWF_EOPC2       = 1,
    QOP_MDWF_EOPC2PRIME  = 2,
    QOP_MDWF_PC_DEFAULT  = QOP_MDWF_EOPC2
  };
  struct QOP_MDWF_Config {
    int    self;                      /* this node QMP id */
    int    master_p;                  /* if != 0, do I/O from this node */
    int    rank;                      /* lattice rank, must be 4 */
    int   *lat;                       /* [rank] */
    int    ls;                        /* lattice extend in the flavor dimension */
    int   *net;                       /* [rank] */
    int   *neighbor_up;               /* [rank], QMP ids of neighbors in up dirs */
    int   *neighbor_down;             /* [rank], QMP ids of neighbors in down dirs */
    void (*sublattice)(int lo[],      /*   [rank] */
                       int hi[],      /*   [rank] */
                       int node,      /*   any node QMP id */
                       void *env);    /*   lexical variables */
    void  *env;                       /* lexical variables for sublattice */
    int   parity;                     /* note that if parity=1, even&odd vars mean odd&even, resp. */
    enum QOP_MDWF_eopc_type eopc_type;
  };
  struct QOP_F3_MDWF_Gauge;
  struct QOP_D3_MDWF_Gauge;
  struct QOP_F3_MDWF_Fermion;
  struct QOP_D3_MDWF_Fermion;
  struct QOP_F3_MDWF_HalfFermion;
  struct QOP_D3_MDWF_HalfFermion;
  struct QOP_F3_MDWF_VectorFermion;
  struct QOP_D3_MDWF_VectorFermion;
  struct QOP_F3_MDWF_Deflator;
  struct QOP_F3_MDWF_HalfFermionMat;
  enum {
     QOP_MDWF_LOG_NONE              = 0x00,
     QOP_MDWF_LOG_CG_RESIDUAL       = 0x01,
     QOP_MDWF_LOG_TRUE_RESIDUAL     = 0x02,
     QOP_MDWF_FINAL_CG_RESIDUAL     = 0x04,
     QOP_MDWF_FINAL_DIRAC_RESIDUAL  = 0x08,
     QOP_MDWF_LOG_EIG_UPDATE1       = 0x10,
     QOP_MDWF_LOG_EIG_POSTAMBLE     = 0x20,
     QOP_MDWF_LOG_EVERYTHING        = 0x3f
  };
  const char *QOP_MDWF_version(void);
  const char *QOP_MDWF_signature(struct QOP_MDWF_State *state);
  int QOP_MDWF_parity(struct QOP_MDWF_State *state);
  int QOP_MDWF_performance(double *time_sec,
                           long long *flops,
                           long long *sent,
                           long long *receive,
                           struct QOP_MDWF_State *state);
  int QOP_MDWF_init(struct QOP_MDWF_State **state_ptr,
                    struct QOP_MDWF_Config *config);
  void QOP_MDWF_fini(struct QOP_MDWF_State **state_ptr);
  const char *QOP_MDWF_error(struct QOP_MDWF_State *state);
  int QOP_MDWF_set_threads(struct QOP_MDWF_State *state,
                            int threads);
  int QOP_MDWF_set_generic(struct QOP_MDWF_Parameters **param_ptr,
                           struct QOP_MDWF_State *state,
                           const double b_5[],
                           const double c_5[],
                           double M_5,
                           double m);
  int QOP_MDWF_set_complex(struct QOP_MDWF_Parameters **param_ptr,
                           struct QOP_MDWF_State *state,
                           const double b_5_re[],
                           const double b_5_im[],
                           const double c_5_re[],
                           const double c_5_im[],
                           double M_5,
                           double m);
  int QOP_MDWF_set_Moebius(struct QOP_MDWF_Parameters **param_ptr,
                           struct QOP_MDWF_State *state,
                           const double b_5[],
                           double kappa,
                           double M_5,
                           double m);
  int QOP_MDWF_set_Shamir(struct QOP_MDWF_Parameters **param_ptr,
                          struct QOP_MDWF_State *state,
                          double a_5,
                          double M_5,
                          double m);
  int QOP_MDWF_set_Borichi(struct QOP_MDWF_Parameters **param_ptr,
                           struct QOP_MDWF_State *state,
                           double a_5,
                           double M_5,
                           double m);
  int QOP_MDWF_set_Chiu(struct QOP_MDWF_Parameters **param_ptr,
                        struct QOP_MDWF_State *state,
                        const double a_5[],
                        double M_5,
                        double m); 

  void QOP_MDWF_free_parameters(struct QOP_MDWF_Parameters **param_ptr);
  int QOP_F3_MDWF_import_gauge(struct QOP_F3_MDWF_Gauge **gauge_ptr,
                               struct QOP_MDWF_State *state,
                               void (*reader)(double *val_re,
                                              double *val_im,
                                              int dir,
                                              const int pos[4],
                                              int a,
                                              int b,
                                              void *env),
                               void *env);
  int QOP_D3_MDWF_import_gauge(struct QOP_D3_MDWF_Gauge **gauge_ptr,
                               struct QOP_MDWF_State *state,
                               void (*reader)(double *val_re,
                                              double *val_im,
                                              int dir,
                                              const int pos[4],
                                              int a,
                                              int b,
                                              void *env),
                               void *env);
  int QOP_MDWF_gauge_float_from_double(struct QOP_F3_MDWF_Gauge **result,
                                       struct QOP_D3_MDWF_Gauge *gauge_ptr);
  void QOP_F3_MDWF_free_gauge(struct QOP_F3_MDWF_Gauge **gauge_ptr);
  void QOP_D3_MDWF_free_gauge(struct QOP_D3_MDWF_Gauge **gauge_ptr);
  int QOP_F3_MDWF_import_fermion(struct QOP_F3_MDWF_Fermion **fermion_ptr,
                                 struct QOP_MDWF_State *state,
                                 void (*reader)(double *val_re,
                                                double *val_im,
                                                const int pos[5],
                                                int color,
                                                int dirac,
                                                void *env),
                                 void *env);
  int QOP_D3_MDWF_import_fermion(struct QOP_D3_MDWF_Fermion **fermion_ptr,
                                 struct QOP_MDWF_State *state,
                                 void (*reader)(double *val_re,
                                                double *val_im,
                                                const int pos[5],
                                                int color,
                                                int dirac,
                                                void *env),
                                 void *env);
  int QOP_F3_MDWF_allocate_fermion(struct QOP_F3_MDWF_Fermion **fermion_ptr,
                                   struct QOP_MDWF_State *state);
  int QOP_D3_MDWF_allocate_fermion(struct QOP_D3_MDWF_Fermion **fermion_ptr,
                                   struct QOP_MDWF_State *state);
  int QOP_F3_MDWF_export_fermion(void (*writer)(const int pos[5],
                                                int color,
                                                int dirac,
                                                double val_re,
                                                double val_im,
                                                void *env),
                                 void *env,
                                 const struct QOP_F3_MDWF_Fermion *fermion);
  int QOP_D3_MDWF_export_fermion(void (*writer)(const int pos[5],
                                                int color,
                                                int dirac,
                                                double val_re,
                                                double val_im,
                                                void *env),
                                 void *env,
                                 const struct QOP_D3_MDWF_Fermion *fermion);
  int QOP_F3_MDWF_import_4d_fermion(struct QOP_F3_MDWF_Fermion **fermion_ptr,
                                    struct QOP_MDWF_State *state,
                                    void (*reader)(double *val_re,
                                                   double *val_im,
                                                   const int pos[4],
                                                   int color,
                                                   int dirac,
                                                   void *env),
                                    void *env);
  int QOP_D3_MDWF_import_4d_fermion(struct QOP_D3_MDWF_Fermion **fermion_ptr,
                                    struct QOP_MDWF_State *state,
                                    void (*reader)(double *val_re,
                                                   double *val_im,
                                                   const int pos[4],
                                                   int color,
                                                   int dirac,
                                                   void *env),
                                    void *env);
  int QOP_F3_MDWF_export_4d_fermion(void (*writer)(const int pos[4],
                                                   int color,
                                                   int dirac,
                                                   double val_re,
                                                   double val_im,
                                                   void *env),
                                    void *env,
                                    const struct QOP_F3_MDWF_Fermion *fermion);
  int QOP_D3_MDWF_export_4d_fermion(void (*writer)(const int pos[4],
                                                   int color,
                                                   int dirac,
                                                   double val_re,
                                                   double val_im,
                                                   void *env),
                                    void *env,
                                    const struct QOP_D3_MDWF_Fermion *fermion);
  int QOP_F3_MDWF_midpoint_pseudo(void (*writer)(const int pos[4],
                                                 double value,
                                                 void *env),
                                  void *env,
                                  const struct QOP_F3_MDWF_Fermion *fermion);
  int QOP_D3_MDWF_midpoint_pseudo(void (*writer)(const int pos[4],
                                                 double value,
                                                 void *env),
                                  void *env,
                                  const struct QOP_D3_MDWF_Fermion *fermion);
  int QOP_F3_MDWF_axial_current(void (*writer)(const int pos[4],
                                               int dir,
                                               double value,
                                               void *env),
                                void *env,
                                const struct QOP_F3_MDWF_Fermion *fermion,
                                const struct QOP_F3_MDWF_Gauge *gauge);
  int QOP_D3_MDWF_axial_current(void (*writer)(const int pos[4],
                                               int dir,
                                               double value,
                                               void *env),
                                void *env,
                                const struct QOP_D3_MDWF_Fermion *fermion,
                                const struct QOP_D3_MDWF_Gauge *gauge);
  void QOP_F3_MDWF_free_fermion(struct QOP_F3_MDWF_Fermion **fermion_ptr);
  void QOP_D3_MDWF_free_fermion(struct QOP_D3_MDWF_Fermion **fermion_ptr);
  int QOP_F3_MDWF_import_half_fermion(struct QOP_F3_MDWF_HalfFermion **hfermion_ptr,
                                      struct QOP_MDWF_State *state,
                                      void (*reader)(double *val_re,
                                                     double *val_im,
                                                     const int pos[5],
                                                     int color,
                                                     int dirac,
                                                     void *env),
                                      void *env);
  int QOP_D3_MDWF_import_half_fermion(struct QOP_D3_MDWF_HalfFermion **hfermion_ptr,
                                      struct QOP_MDWF_State *state,
                                      void (*reader)(double *val_re,
                                                     double *val_im,
                                                     const int pos[5],
                                                     int color,
                                                     int dirac,
                                                     void *env),
                                      void *env);
  int QOP_F3_MDWF_allocate_half_fermion(struct QOP_F3_MDWF_HalfFermion **hfermion_ptr,
                                        struct QOP_MDWF_State *state);
  int QOP_D3_MDWF_allocate_half_fermion(struct QOP_D3_MDWF_HalfFermion **hfermion_ptr,
                                        struct QOP_MDWF_State *state);
  int QOP_F3_MDWF_export_half_fermion(void (*writer)(const int pos[5],
                                                     int color,
                                                     int dirac,
                                                     double val_re,
                                                     double val_im,
                                                     void *env),
                                     void *env,
                                     const struct QOP_F3_MDWF_HalfFermion *hfermion);
  int QOP_D3_MDWF_export_half_fermion(void (*writer)(const int pos[5],
                                                     int color,
                                                     int dirac,
                                                     double val_re,
                                                     double val_im,
                                                     void *env),
                                     void *env,
                                     const struct QOP_D3_MDWF_HalfFermion *hfermion);
  int QOP_F3_MDWF_blas_from_half_fermion(float *data,
                                         int size,
                                         const struct QOP_F3_MDWF_HalfFermion *hfermion);
  int QOP_D3_MDWF_blas_from_half_fermion(double *data,
                                         int size,
                                         const struct QOP_D3_MDWF_HalfFermion *hfermion);
  int QOP_F3_MDWF_half_fermion_from_blas(struct QOP_F3_MDWF_HalfFermion *hfermion,
                                         const float *data,
                                         int size);
  int QOP_D3_MDWF_half_fermion_from_blas(struct QOP_D3_MDWF_HalfFermion *hfermion,
                                         const double *data,
                                         int size);
  int QOP_MDWF_half_fermion_size(struct QOP_MDWF_State *state_ptr);
  void QOP_F3_MDWF_free_half_fermion(struct QOP_F3_MDWF_HalfFermion **hfermion_ptr);
  void QOP_D3_MDWF_free_half_fermion(struct QOP_D3_MDWF_HalfFermion **hfermion_ptr);
  int QOP_F3_MDWF_allocate_vector_fermion(struct QOP_F3_MDWF_VectorFermion **vf_ptr,
                                          struct QOP_MDWF_State *state,
                                          int n);
  int QOP_D3_MDWF_allocate_vector_fermion(struct QOP_D3_MDWF_VectorFermion **vf_ptr,
                                         struct QOP_MDWF_State *state,
                                         int n);
  void QOP_F3_MDWF_free_vector_fermion(struct QOP_F3_MDWF_VectorFermion **vf_ptr);
  void QOP_D3_MDWF_free_vector_fermion(struct QOP_D3_MDWF_VectorFermion **vf_ptr);
  int QOP_F3_MDWF_get_vector_fermion(struct QOP_F3_MDWF_HalfFermion *hf,
                                     const struct QOP_F3_MDWF_VectorFermion *vf,
                                     int index);
  int QOP_D3_MDWF_get_vector_fermion(struct QOP_D3_MDWF_HalfFermion *hf,
                                     const struct QOP_D3_MDWF_VectorFermion *vf,
                                     int index);
  int QOP_F3_MDWF_put_vector_fermion(struct QOP_F3_MDWF_VectorFermion *vf,
                                     int index,
                                     const struct QOP_F3_MDWF_HalfFermion *hf);
  int QOP_D3_MDWF_put_vector_fermion(struct QOP_D3_MDWF_VectorFermion *vf,
                                     int index,
                                     const struct QOP_D3_MDWF_HalfFermion *hf);
  int QOP_F3_MDWF_DDW_operator(struct QOP_F3_MDWF_Fermion *result,
                               const struct QOP_MDWF_Parameters *params,
                               const struct QOP_F3_MDWF_Gauge *gauge,
                               const struct QOP_F3_MDWF_Fermion *fermion);
  int QOP_D3_MDWF_DDW_operator(struct QOP_D3_MDWF_Fermion *result,
                               const struct QOP_MDWF_Parameters *params,
                               const struct QOP_D3_MDWF_Gauge *gauge,
                               const struct QOP_D3_MDWF_Fermion *fermion);
  int QOP_F3_MDWF_DDW_operator_conjugated(struct QOP_F3_MDWF_Fermion *result,
                                          const struct QOP_MDWF_Parameters *params,
                                          const struct QOP_F3_MDWF_Gauge *gauge,
                                          const struct QOP_F3_MDWF_Fermion *fermion);
  int QOP_D3_MDWF_DDW_operator_conjugated(struct QOP_D3_MDWF_Fermion *result,
                                          const struct QOP_MDWF_Parameters *params,
                                          const struct QOP_D3_MDWF_Gauge *gauge,
                                          const struct QOP_D3_MDWF_Fermion *fermion);
  int QOP_F3_MDWF_M_operator(struct QOP_F3_MDWF_HalfFermion *result,
                             const struct QOP_MDWF_Parameters *params,
                             const struct QOP_F3_MDWF_Gauge *gauge,
                             const struct QOP_F3_MDWF_HalfFermion *fermion);
  int QOP_D3_MDWF_M_operator(struct QOP_D3_MDWF_HalfFermion *result,
                             const struct QOP_MDWF_Parameters *params,
                             const struct QOP_D3_MDWF_Gauge *gauge,
                             const struct QOP_D3_MDWF_HalfFermion *fermion);
  int QOP_F3_MDWF_M_operator_conjugated(struct QOP_F3_MDWF_HalfFermion *result,
                                        const struct QOP_MDWF_Parameters *params,
                                        const struct QOP_F3_MDWF_Gauge *gauge,
                                        const struct QOP_F3_MDWF_HalfFermion *fermion);
  int QOP_D3_MDWF_M_operator_conjugated(struct QOP_D3_MDWF_HalfFermion *result,
                                        const struct QOP_MDWF_Parameters *params,
                                        const struct QOP_D3_MDWF_Gauge *gauge,
                                        const struct QOP_D3_MDWF_HalfFermion *fermion);
  int QOP_F3_MDWF_create_deflator(struct QOP_F3_MDWF_Deflator **defl_ptr,
                                  struct QOP_MDWF_State       *state,
                                  int nev, int vsize, double eps, int umax);
  int QOP_F3_MDWF_create_deflator_inplace(struct QOP_F3_MDWF_Deflator       **defl_ptr,
                                          const struct QOP_MDWF_Parameters  *params,
                                          const struct QOP_F3_MDWF_Gauge    *gauge,
                                          struct QOP_F3_MDWF_HalfFermionMat **hfm_ptr,
                                          int                               hfm_nev,
                                          int                               eigcg_vmax,
                                          int                               eigcg_nev,
                                          double                            eigcg_eps,
                                          int                               eigcg_umax);
  int QOP_F3_MDWF_alloc_half_fermion_matrix(struct QOP_F3_MDWF_HalfFermionMat **hfm_ptr,
                                             struct QOP_MDWF_State *state,
                                             int ncol);
  int QOP_F3_MDWF_blas_view_half_fermion_matrix(struct QOP_F3_MDWF_HalfFermionMat *hfm_ptr,
                                                int        *nrow_loc,
                                                int        *ncol,
                                                float      **blas_ptr,
                                                int        *blas_ld);
  void QOP_F3_MDWF_free_half_fermion_matrix(struct QOP_F3_MDWF_HalfFermionMat **hfm_ptr);
  void QOP_F3_MDWF_free_deflator(struct QOP_F3_MDWF_Deflator **defl_ptr);
  int QOP_F3_MDWF_deflator_eigcg_reset(struct QOP_F3_MDWF_Deflator *defl_ptr);
  int QOP_F3_MDWF_deflator_eigcg_stop(struct QOP_F3_MDWF_Deflator *defl_ptr);
  int QOP_F3_MDWF_deflator_eigcg_resume(struct QOP_F3_MDWF_Deflator *defl_ptr);
  int QOP_F3_MDWF_deflator_eigcg_is_stopped(struct QOP_F3_MDWF_Deflator *defl_ptr);
  int QOP_F3_MDWF_deflator_current_dim(struct QOP_F3_MDWF_Deflator *defl_ptr);
  int QOP_F3_MDWF_deflator_extract_vector(struct QOP_F3_MDWF_HalfFermion *hfermion_ptr,
                                          const struct QOP_F3_MDWF_Deflator *defl_ptr,
                                          int idx);
  int QOP_F3_MDWF_deflator_start_load(struct QOP_F3_MDWF_Deflator *defl_ptr);
  int QOP_F3_MDWF_deflator_add_vector(const struct QOP_MDWF_Parameters *params,
                                      const struct QOP_F3_MDWF_Gauge *gauge,
                                      struct QOP_F3_MDWF_Deflator *deflator,
                                      const struct QOP_F3_MDWF_HalfFermion *hfermion);
  int QOP_F3_MDWF_deflator_stop_load(struct QOP_F3_MDWF_Deflator *defl_ptr);
  int QOP_F3_MDWF_deflator_eigen(int n, double *eigen_values,
                                 struct QOP_F3_MDWF_Deflator *defl_ptr);
  int QOP_F3_MDWF_DDW_CG(struct QOP_F3_MDWF_Fermion *result,
                         int *out_iterations,
                         double *out_epsilon,
                         const struct QOP_MDWF_Parameters *params,
                         const struct QOP_F3_MDWF_Fermion *chi_0,
                         const struct QOP_F3_MDWF_Gauge *gauge,
                         const struct QOP_F3_MDWF_Fermion *rhs,
                         int max_iteration,
                         double epsilon,
                         unsigned int options);
  int QOP_D3_MDWF_DDW_CG(struct QOP_D3_MDWF_Fermion *result,
                         int *out_iterations,
                         double *out_epsilon,
                         const struct QOP_MDWF_Parameters *params,
                         const struct QOP_D3_MDWF_Fermion *x_0,
                         const struct QOP_D3_MDWF_Gauge *gauge,
                         const struct QOP_D3_MDWF_Fermion *rhs,
                         int max_iteration,
                         double epsilon,
                         unsigned int options);
  int QOP_MDWF_mixed_DDW_CG(struct QOP_D3_MDWF_Fermion *result,
                             int *out_iterations,
                             double *out_epsilon,
                             const struct QOP_MDWF_Parameters *params,
                             const struct QOP_D3_MDWF_Fermion *x_0,
                             const struct QOP_D3_MDWF_Gauge *gauge,
                             const struct QOP_D3_MDWF_Fermion *rhs,
                             int f_iterations,
                             double f_epsilon,
                             int max_iteration,
                             double epsilon,
                             unsigned int options);
  int QOP_F3_MDWF_DxD_CG(struct QOP_F3_MDWF_Fermion *psi,
                         int *out_iterations,
                         double *out_epsilon,
                         const struct QOP_MDWF_Parameters *params,
                         const struct QOP_F3_MDWF_Fermion *psi_0,
                         const struct QOP_F3_MDWF_Gauge *gauge,
                         const struct QOP_F3_MDWF_Fermion *rhs,
                         int max_iteration,
                         double epsilon,
                         unsigned int options);
  int QOP_D3_MDWF_DxD_CG(struct QOP_D3_MDWF_Fermion *psi,
                         int *out_iterations,
                         double *out_epsilon,
                         const struct QOP_MDWF_Parameters *params,
                         const struct QOP_D3_MDWF_Fermion *psi_0,
                         const struct QOP_D3_MDWF_Gauge *gauge,
                         const struct QOP_D3_MDWF_Fermion *rhs,
                         int max_iteration,
                         double epsilon,
                         unsigned int options);
  int QOP_F3_MDWF_MxM_CG(struct QOP_F3_MDWF_HalfFermion *result,
                         int *out_iterations,
                         double *out_epsilon,
                         const struct QOP_MDWF_Parameters *params,
                         const struct QOP_F3_MDWF_Gauge *gauge,
                         const struct QOP_F3_MDWF_HalfFermion *rhs,
                         int max_iteration,
                         double epsilon,
                         unsigned int options);
  int QOP_D3_MDWF_MxM_CG(struct QOP_D3_MDWF_HalfFermion *result,
                         int *out_iterations,
                         double *out_epsilon,
                         const struct QOP_MDWF_Parameters *params,
                         const struct QOP_D3_MDWF_Gauge *gauge,
                         const struct QOP_D3_MDWF_HalfFermion *rhs,
                         int max_iteration,
                         double epsilon,
                         unsigned int options);
  int QOP_F3_MDWF_MxM_SCG(struct QOP_F3_MDWF_VectorFermion *vector_result,
                          struct QOP_F3_MDWF_HalfFermion *scalar_result,
                          int *out_iterations,
                          double *out_epsilon,
                          const struct QOP_MDWF_Parameters *params,
                          const double shift[],
                          const struct QOP_F3_MDWF_Gauge *gauge,
                          const struct QOP_F3_MDWF_HalfFermion *rhs,
                          int max_iterations,
                          double min_epsilon,
                          unsigned int options);
  int QOP_D3_MDWF_MxM_SCG(struct QOP_D3_MDWF_VectorFermion *vector_result,
                          struct QOP_D3_MDWF_HalfFermion *scalar_result,
                          int *out_iterations,
                          double *out_epsilon,
                          const struct QOP_MDWF_Parameters *params,
                          const double shift[],
                          const struct QOP_D3_MDWF_Gauge *gauge,
                          const struct QOP_D3_MDWF_HalfFermion *rhs,
                          int max_iterations,
                          double min_epsilon,
                          unsigned int options);
  int QOP_MDWF_deflated_mixed_D_CG(struct QOP_D3_MDWF_Fermion *result,
                                   int *out_iterations,
                                   double *out_epsilon,
                                   const struct QOP_MDWF_Parameters *params,
                                   const struct QOP_D3_MDWF_Fermion *chi_0,
                                   const struct QOP_D3_MDWF_Gauge *gauge,
                                   const struct QOP_D3_MDWF_Fermion *rhs,
                                   struct QOP_F3_MDWF_Deflator *deflator,
                                   int f_iterations,
                                   double f_epsilon,
                                   int max_iteration,
                                   double epsilon,
                                   unsigned int options);
  int QOP_F3_MDWF_MxM_poly(struct QOP_F3_MDWF_HalfFermion *result,
                           struct QOP_F3_MDWF_HalfFermion *result_prev,
                           const struct QOP_MDWF_Parameters *params,
                           const struct QOP_F3_MDWF_Gauge *gauge,
                           const struct QOP_F3_MDWF_HalfFermion *psi,
                           int n,
                           const double a[/* n */],
                           const double b[/* n */],
                           const double c[/* n */]);
  int QOP_D3_MDWF_MxM_poly(struct QOP_D3_MDWF_HalfFermion *result,
                           struct QOP_D3_MDWF_HalfFermion *result_prev,
                           const struct QOP_MDWF_Parameters *params,
                           const struct QOP_D3_MDWF_Gauge *gauge,
                           const struct QOP_D3_MDWF_HalfFermion *psi,
                           int n,
                           const double a[/* n */],
                           const double b[/* n */],
                           const double c[/* n */]);
  int QOP_MDWF_poly_normalize(int n,
                              double a[/* n */],
                              double b[/* n */],
                              double c[/* n */],
                              double x0,
                              double tol);
  int QOP_F3_MDWF_madd_fermion(struct QOP_F3_MDWF_Fermion *r,
                               const struct QOP_F3_MDWF_Fermion *a,
                               double alpha,
                               const struct QOP_F3_MDWF_Fermion *b);
  int QOP_D3_MDWF_madd_fermion(struct QOP_D3_MDWF_Fermion *r,
                               const struct QOP_D3_MDWF_Fermion *a,
                               double alpha,
                               const struct QOP_D3_MDWF_Fermion *b);
  int QOP_F3_MDWF_madd_half_fermion(struct QOP_F3_MDWF_HalfFermion *r,
                                    const struct QOP_F3_MDWF_HalfFermion *a,
                                    double alpha,
                                    const struct QOP_F3_MDWF_HalfFermion *b);
  int QOP_D3_MDWF_madd_half_fermion(struct QOP_D3_MDWF_HalfFermion *r,
                                    const struct QOP_D3_MDWF_HalfFermion *a,
                                    double alpha,
                                    const struct QOP_D3_MDWF_HalfFermion *b);
  int QOP_F3_MDWF_dot_fermion(double *r_re,
                              double *r_im,
                              const struct QOP_F3_MDWF_Fermion *a,
                              const struct QOP_F3_MDWF_Fermion *b);
  int QOP_D3_MDWF_dot_fermion(double *r_re,
                              double *r_im,
                              const struct QOP_D3_MDWF_Fermion *a,
                              const struct QOP_D3_MDWF_Fermion *b);
  int QOP_F3_MDWF_dot_half_fermion(double *r_re,
                                   double *r_im,
                                   const struct QOP_F3_MDWF_HalfFermion *a,
                                   const struct QOP_F3_MDWF_HalfFermion *b);
  int QOP_D3_MDWF_dot_half_fermion(double *r_re,
                                   double *r_im,
                                   const struct QOP_D3_MDWF_HalfFermion *a,
                                   const struct QOP_D3_MDWF_HalfFermion *b);
  int QOP_F3_MDWF_norm2_fermion(double *r,
                                const struct QOP_F3_MDWF_Fermion *a);
  int QOP_D3_MDWF_norm2_fermion(double *r_re,
                                const struct QOP_D3_MDWF_Fermion *a);
  int QOP_F3_MDWF_norm2_half_fermion(double *r_re,
                                     const struct QOP_F3_MDWF_HalfFermion *a);
  int QOP_D3_MDWF_norm2_half_fermion(double *r_re,
                                     const struct QOP_D3_MDWF_HalfFermion *a);
  int QOP_F3_MDWF_debugmesilly(struct QOP_F3_MDWF_Fermion *y,
                              const struct QOP_MDWF_Parameters *params,
                              const struct QOP_F3_MDWF_Gauge *gauge,
                              const char *op_name, 
                              const struct QOP_F3_MDWF_Fermion *x);
  int QOP_D3_MDWF_debugmesilly(struct QOP_D3_MDWF_Fermion *y,
                              const struct QOP_MDWF_Parameters *params,
                              const struct QOP_D3_MDWF_Gauge *gauge,
                              const char *op_name, 
                              const struct QOP_D3_MDWF_Fermion *x);
# if defined(QOP_MDWF_DEFAULT_PRECISION) && (QOP_MDWF_DEFAULT_PRECISION == 'F')
   #define QOP_MDWF_import_gauge QOP_F3_MDWF_import_gauge
   #define QOP_MDWF_free_gauge QOP_F3_MDWF_free_gauge
   #define QOP_MDWF_Gauge QOP_F3_MDWF_Gauge
   #define QOP_MDWF_import_fermion QOP_F3_MDWF_import_fermion
   #define QOP_MDWF_import_4d_fermion QOP_F3_MDWF_import_4d_fermion
   #define QOP_MDWF_export_fermion QOP_F3_MDWF_export_fermion
   #define QOP_MDWF_export_4d_fermion QOP_F3_MDWF_export_4d_fermion
   #define QOP_MDWF_allocate_fermion QOP_F3_MDWF_allocate_fermion
   #define QOP_MDWF_midpoint_pseudo QOP_F3_MDWF_midpoint_pseudo
   #define QOP_MDWF_axial_current QOP_F3_MDWF_axial_current
   #define QOP_MDWF_free_fermion QOP_F3_MDWF_free_fermion
   #define QOP_MDWF_Fermion QOP_F3_MDWF_Fermion
   #define QOP_MDWF_import_half_fermion QOP_F3_MDWF_import_half_fermion
   #define QOP_MDWF_export_half_fermion QOP_F3_MDWF_export_half_fermion
   #define QOP_MDWF_allocate_half_fermion QOP_F3_MDWF_allocate_half_fermion
   #define QOP_MDWF_blas_from_half_fermion QOP_F3_MDWF_blas_from_half_fermion
   #define QOP_MDWF_half_fermion_from_blas QOP_F3_MDWF_half_fermion_from_blas
   #define QOP_MDWF_free_half_fermion QOP_F3_MDWF_free_half_fermion
   #define QOP_MDWF_HalfFermion QOP_F3_MDWF_HalfFermion
   #define QOP_MDWF_allocate_vector_fermion QOP_F3_MDWF_allocate_vector_fermion
   #define QOP_MDWF_free_vector_fermion QOP_F3_MDWF_free_vector_fermion
   #define QOP_MDWF_get_vector_fermion QOP_F3_MDWF_get_vector_fermion
   #define QOP_MDWF_put_vector_fermion QOP_F3_MDWF_put_vector_fermion
   #define QOP_MDWF_VectorFermion QOP_F3_MDWF_VectorFermion
   #define QOP_MDWF_DDW_operator QOP_F3_MDWF_DDW_operator
   #define QOP_MDWF_DDW_operator_conjugated QOP_F3_MDWF_DDW_operator_conjugated
   #define QOP_MDWF_M_operator QOP_F3_MDWF_M_operator
   #define QOP_MDWF_M_operator_conjugated QOP_F3_MDWF_M_operator_conjugated
   #define QOP_MDWF_DDW_CG QOP_F3_MDWF_DDW_CG
   #define QDP_MDWF_DxD_CG QOP_F3_MDWF_DxD_CG
   #define QOP_MDWF_MxM_CG QOP_F3_MDWF_MxM_CG
   #define QOP_MDWF_MxM_SCG QOP_F3_MDWF_MxM_SCG
   #define QOP_MDWF_MxM_poly QOP_F3_MDWF_MxM_poly
   #define QOP_MDWF_madd_fermion QOP_F3_MDWF_madd_fermion
   #define QOP_MDWF_madd_half_fermion QOP_F3_MDWF_madd_half_fermion
   #define QOP_MDWF_dot_fermion QOP_F3_MDWF_dot_fermion
   #define QOP_MDWF_dot_half_fermion QOP_F3_MDWF_dot_half_fermion
   #define QOP_MDWF_norm2_fermion QOP_F3_MDWF_norm2_fermion
   #define QOP_MDWF_norm2_half_fermion QOP_F3_MDWF_norm2_half_fermion
# endif
# if defined(QOP_MDWF_DEFAULT_PRECISION) && (QOP_MDWF_DEFAULT_PRECISION == 'D')
    #define QOP_MDWF_import_gauge QOP_D3_MDWF_import_gauge
    #define QOP_MDWF_free_gauge QOP_D3_MDWF_free_gauge
    #define QOP_MDWF_Gauge QOP_D3_MDWF_Gauge
    #define QOP_MDWF_import_fermion QOP_D3_MDWF_import_fermion
    #define QOP_MDWF_import_4d_fermion QOP_D3_MDWF_import_4d_fermion
    #define QOP_MDWF_export_fermion QOP_D3_MDWF_export_fermion
    #define QOP_MDWF_export_4d_fermion QOP_D3_MDWF_export_4d_fermion
    #define QOP_MDWF_allocate_fermion QOP_D3_MDWF_allocate_fermion
    #define QOP_MDWF_midpoint_pseudo QOP_D3_MDWF_midpoint_pseudo
    #define QOP_MDWF_axial_current QOP_D3_MDWF_axial_current
    #define QOP_MDWF_free_fermion QOP_D3_MDWF_free_fermion
    #define QOP_MDWF_Fermion QOP_D3_MDWF_Fermion
    #define QOP_MDWF_import_half_fermion QOP_D3_MDWF_import_half_fermion
    #define QOP_MDWF_export_half_fermion QOP_D3_MDWF_export_half_fermion
    #define QOP_MDWF_allocate_half_fermion QOP_D3_MDWF_allocate_half_fermion
    #define QOP_MDWF_blas_from_half_fermion QOP_D3_MDWF_blas_from_half_fermion
    #define QOP_MDWF_half_fermion_from_blas QOP_D3_MDWF_half_fermion_from_blas
    #define QOP_MDWF_free_half_fermion QOP_D3_MDWF_free_half_fermion
    #define QOP_MDWF_HalfFermion QOP_D3_MDWF_HalfFermion
    #define QOP_MDWF_allocate_vector_fermion QOP_D3_MDWF_allocate_vector_fermion
    #define QOP_MDWF_free_vector_fermion QOP_D3_MDWF_free_vector_fermion
    #define QOP_MDWF_get_vector_fermion QOP_D3_MDWF_get_vector_fermion
    #define QOP_MDWF_put_vector_fermion QOP_D3_MDWF_put_vector_fermion
    #define QOP_MDWF_VectorFermion QOP_D3_MDWF_VectorFermion
    #define QOP_MDWF_DDW_operator QOP_D3_MDWF_DDW_operator
    #define QOP_MDWF_DDW_operator_conjugated QOP_D3_MDWF_DDW_operator_conjugated
    #define QOP_MDWF_M_operator QOP_D3_MDWF_M_operator
    #define QOP_MDWF_M_operator_conjugated QOP_D3_MDWF_M_operator_conjugated
    #define QOP_MDWF_DDW_CG QOP_D3_MDWF_DDW_CG
    #define QDP_MDWF_DxD_CG QOP_D3_MDWF_DxD_CG
    #define QOP_MDWF_MxM_CG QOP_D3_MDWF_MxM_CG
    #define QOP_MDWF_MxM_SCG QOP_D3_MDWF_MxM_SCG
    #define QOP_MDWF_MxM_poly QOP_D3_MDWF_MxM_poly
    #define QOP_MDWF_madd_fermion QOP_D3_MDWF_madd_fermion
    #define QOP_MDWF_madd_half_fermion QOP_D3_MDWF_madd_half_fermion
    #define QOP_MDWF_dot_fermion QOP_D3_MDWF_dot_fermion
    #define QOP_MDWF_dot_half_fermion QOP_D3_MDWF_dot_half_fermion
    #define QOP_MDWF_norm2_fermion QOP_D3_MDWF_norm2_fermion
    #define QOP_MDWF_norm2_half_fermion QOP_D3_MDWF_norm2_half_fermion
# endif
#endif
