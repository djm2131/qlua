#define QOP_MDWF_DEFAULT_PRECISION 'F'
#include <assert.h>
#include <mdwf.h>
#include <math.h>
#include <qmp.h>

#if defined(HAVE_LAPACK)
#  include <lapack.h>
#elif defined(HAVE_GSL)
#  include <gsl/gsl_linalg.h>
#  include <gsl/gsl_sort_double.h>
#  include <gsl/gsl_sort_vector_double.h>
#else
#  error "no linear algebra library"
#endif

/* macros to reuse workspace */
#define eps_reortho 2e-8


/* TODO add flop counting */
/* TODO change latvec_c_copy(x, y) to (y, x) because it does y<-x */
/* TODO change solving in uspace from cholesky decomp to using evectors:
        we diag matrix anyway to compute e.values */

/* XXX uses WK: df->zwork */
int
qx(defl_solve_in_uspace)(
        struct QX(Deflator)      *df,
        struct Fermion         *x,
        struct Fermion         *b)
{
    long long fl = 0;

    assert(NULL != df &&
            NULL != df->state &&
            NULL != x &&
            NULL != b);
    qx(defl_vec) lv_x   = qx(defl_vec_view)(df->state, x);
    if (df->usize <= 0) {
      qx(defl_vec_zero)(df->state, lv_x);
        return 0;
    }    
    qx(defl_vec) lv_b   = qx(defl_vec_view)(df->state, b);
    qx(defl_mat) cur_U  = qx(defl_mat_submat_col)(df->U, 0, df->usize);
    fl += qx(defl_lmH_dot_lv)(df->state, df->usize, cur_U, lv_b, df->zwork);

#if defined(HAVE_LAPACK)
    long int usize  = df->usize,
             ONE    = 1,
             umax   = df->umax,
             info   = 0;
    char cU = 'U';
    zpotrs_(&cU, &usize, &ONE, df->C, &umax, df->zwork, &umax, &info, 1);
    assert(0 == info);
#elif defined(HAVE_GSL)

    gsl_matrix_complex_view gsl_C = gsl_matrix_complex_view_array_with_tda(
            (double *)df->C, df->usize, df->usize, df->umax);
    gsl_vector_complex_view gsl_zwork = gsl_vector_complex_view_array(
            (double *)df->zwork, df->usize);
    CHECK_GSL_STATUS(gsl_linalg_complex_cholesky_svx(
            &gsl_C.matrix, 
            &gsl_zwork.vector));
#else
#  error "no linear algebra library"
#endif

    fl += qx(defl_lm_dot_zv)(df->state, df->usize, cur_U, df->zwork, lv_x);

    df->state->flops += fl;
    return 0;
}

/* XXX uses WK: df->work_c_2, df->zwork */
int 
qx(defl_ortho_uspace)(qx(defl_vec) vec,
                      struct QX(Deflator) *df,
                      int u_lo, int u_hi)
{
    if (NULL == df || defl_vec_is_null(&vec))
        return q(set_error)(df->state, 0, "df_ortho_uspace(): null pointer(s)");
    
    long long fl = 0;
    int u_len = u_hi - u_lo;
    if (u_len <= 0)
        return 0; /* relax */

    qx(defl_mat) cur_U = qx(defl_mat_submat_col)(df->U, u_lo, u_len);
    qx(defl_vec) ws_vec = df->work_c_2;

    fl += qx(defl_lmH_dot_lv)(df->state, u_len, cur_U, 
                              vec, df->zwork);
    fl += qx(defl_lm_dot_zv)(df->state, u_len, cur_U,
                             df->zwork, ws_vec);
    fl += qx(defl_vec_axpy)(df->state, -1., ws_vec, vec);

    df->state->flops += fl;
    return 0;
}

/* orthogonalize wrt [u_lo: u_hi) vectors, normalize, inject into [u_pos]
   and compute H[u_lo:u_hi, vpos]
   XXX uses WK: (children: df->work_c_2, df->zwork)
       vector `vec' is orthogonal to U and |vec|=1 on return */
int
qx(defl_inject)(struct QX(Deflator) *df,
             struct qx(MxM_workspace) *ws,
             int u_lo, int u_hi, int u_pos,
             qx(defl_vec) vec, int n_reortho)
{
    long long fl = 0;
    double v_norm2_min = eps_reortho * eps_reortho;
    int i, u_len = u_hi - u_lo;

    /* reorthogonalize n_reortho times */
    for (i = 0 ; i < n_reortho; i++)
        qx(defl_ortho_uspace)(vec, df, u_lo, u_hi);

    double v_norm2;
    fl += qx(defl_vec_nrm2)(df->state, &v_norm2, vec);
    if (v_norm2 < v_norm2_min)
        return 0;

    /* normalize & insert the vector */
    fl += qx(defl_vec_scal)(df->state, 1. / sqrt(v_norm2), vec);
    fl += qx(defl_mat_insert_col)(df->state, df->U, u_pos, vec);

    qx(defl_vec) ws_vec = df->work_c_2;
    qx(defl_vec_linop)(ws_vec, vec, ws);

    if (0 < u_len) {
        /* compute and store (U^H . A . vec) */
        qx(defl_mat) cur_U = qx(defl_mat_submat_col)(df->U, u_lo, u_len);
        fl += qx(defl_lmH_dot_lv)(df->state, u_len, cur_U, ws_vec, 
                                  df->H + u_lo + u_pos * df->umax);
        for (i = u_lo ; i < u_hi ; i++) {
            doublecomplex *p1 = df->H + i + u_pos * df->umax,
                          *p2 = df->H + u_pos + i * df->umax;
            p2->r =  p1->r;
            p2->i = -p1->i;
        }
    }
    /* compute and store (vec^H . A . vec) */
    doublecomplex vAv;
#if 1
    fl += qx(defl_vec_dotu)(df->state, &vAv, vec, ws_vec);
#else
    qx(defl_mat) vec_m = qx(defl_mat_alloc)(df->state, 1);
    qx(defl_mat_insert_col)(df->state, vec_m, 0, vec);
    fl += qx(defl_lmH_dot_lv)(df->state, 1, vec_m, ws_vec, &vAv);
    qx(defl_mat_free)(df->state, &vec_m);
#endif
    df->H[u_pos * (df->umax + 1)].r = vAv.r;
    df->H[u_pos * (df->umax + 1)].i = 0.;

    df->state->flops += fl;
    /* finished adding one new vector */
    return 1;
}

/* orthogonalize, normalize, add 'vec' to U
   XXX uses WK: (children: df->work_c_2, df->zwork)
       vector `vec' is orthogonal to U and |vec|=1 on return */
int 
qx(defl_inject_back)(struct QX(Deflator) *df,
                     struct qx(MxM_workspace) *ws,
                     qx(defl_vec) vec, int n_reortho)
{
    if (df->usize < df->umax) {
        qx(defl_inject)(df, ws, 0, df->usize, df->usize, vec, n_reortho);
        df->usize ++;
        return 1;
    } else
        return 0;
}

/* use df_inject below : extract vector; in ject vector */
/* recompute H[:usize, :usize]; all vectors are NOT reorthogonalized 
   with respect to preceding vectors in U
   XXX uses WK: df->work_c_1 (children: df->work_c_2, df->zwork) */
int 
qx(defl_recalc_mat)(struct QX(Deflator) *df,
                    struct qx(MxM_workspace) *ws)
{
    long long fl = 0;
    int i, u_pos;

    qx(defl_vec) vec = df->work_c_1;

    for (i = 0, u_pos = 0 ; i < df->usize ; i++) {
      fl += qx(defl_mat_get_col)(df->state, df->U, i, vec);
        /*increment only if have an indep. vector */
        u_pos += qx(defl_inject)(df, ws, 0, u_pos, u_pos, vec, N_REORTHO_MAT_RECALC);
    }
  
    df->state->flops += fl;
    return u_pos;
}

/* recompute Cholesky factorization for U-space solving */
int
qx(defl_rebuild)(struct QX(Deflator) *df)
{
    int status;
    int i;

    /* compute Cholesky decomposition */
    memcpy(df->C, df->H, df->usize * df->umax * sizeof(df->C[0]));

#if defined(HAVE_LAPACK)
    long int usize  = df->usize,
           umax   = df->umax,
           info   = 0;
    char cU = 'U';
    zpotrf_(&cU, &usize, df->C, &umax, &info, 1);
    assert(0 == info);
#elif defined(HAVE_GSL)
    gsl_matrix_complex_view gsl_C = gsl_matrix_complex_view_array_with_tda(
            (double *)df->C, df->usize, df->usize, df->umax);
    /* transpose to convert matrix Fortran[row][col] -> C[col][row] */
    CHECK_GSL_STATUS(gsl_matrix_complex_transpose(&gsl_C.matrix));
    CHECK_GSL_STATUS(gsl_linalg_complex_cholesky_decomp(&gsl_C.matrix));
#else
#  error "no linear algebra library"
#endif

    /* broadcast Cholesky matrix from the master node for consistency */
    for (i = 0 ; i < df->usize; i++)
        QMP_broadcast((void *)(df->C + i * df->umax), 
                      df->usize * sizeof(df->C[0]));

    if (0 != (status = qx(defl_calc_evals)(df)))
        return status;

    return 0;
}

int 
qx(defl_calc_evals)(struct QX(Deflator) *df) 
{
  long int usize  = df->usize;
#if defined(HAVE_LAPACK)
  long int umax   = df->umax;
#endif
    /* matrix is destroyed by the eigenvalue algorithm; need to copy */
    /* FIXME only triangular part is destroyed, so no extra storage 
       is necessary; the full matrix can be restored after computing evals */
    memcpy(df->H_ev, df->H, df->usize * df->umax * sizeof(df->H[0]));
#if defined(HAVE_LAPACK)
    {
        int info   = 0;
        char cU = 'U',
             cN = 'N';
        zheev_(&cN, &cU, &usize, df->H_ev, &umax, df->hevals, 
               df->eig_zwork, &(df->eig_lwork), df->eig_rwork,
               &info, 1, 1);
        assert(0 == info);
    }
#elif defined(HAVE_GSL)
    {
        gsl_vector_view gsl_hevals = gsl_vector_view_array(df->hevals, usize);
        gsl_matrix_complex_view gsl_H_ev = gsl_matrix_complex_view_array_with_tda(
                (double *)df->H_ev, usize, usize, df->umax);
        /* transpose to convert matrix Fortran[row][col] -> C[col][row] */
        CHECK_GSL_STATUS(gsl_matrix_complex_transpose(&gsl_H_ev.matrix));

        CHECK_GSL_STATUS(gsl_eigen_herm(&gsl_H_ev.matrix, &gsl_hevals.vector,
                                        df->gsl_eig_wkspace));
        gsl_sort_vector(&gsl_hevals.vector);
    }
#if 0    /* HOTFIX/DEBUG : PRINT PROJECTED MATRIX */
    if (0 == QMP_get_node_number()) {
        printf("#qx(defl_calc_evals) Hmatr\n");
        for (int i = 0; i <usize ; i++) {
            for (int j = 0 ; j < usize ; j++) {
                printf("%e  %e\n", 
                        df->H[i * df->umax + j].r, 
                        df->H[i * df->umax + j].i);
            }
        }
        printf("#qx(defl_calc_evals) evals\n");
        for (int i = 0; i <usize ; i++) {
            printf("%e\n", df->hevals[i]);
        }
    }
#endif

#else
#  error "no linear algebra library"
#endif
    return 0;
}
