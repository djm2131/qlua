# ifndef QOP_TWISTED_DEFAULT_PRECISION
#  define QOP_TWISTED_DEFAULT_PRECISION 'D'
# endif

#ifndef MARK_21eedb1e5eff4d4ba0462475b43ae655
#define MARK_21eedb1e5eff4d4ba0462475b43ae655

# include "qop-twisted.h"
# include <stdlib.h>
# include <string.h>
# include <qmp.h>
# include <sys/time.h>

# define q(x) qop_|:Nc:|_twisted_##x
# define qf(x) qop_f|:Nc:|_twisted_##x
# define qd(x) qop_d|:Nc:|_twisted_##x
# define Q(x) QOP_|:Nc:|_TWISTED_##x
# define QF(x) QOP_F|:Nc:|_TWISTED_##x
# define QD(x) QOP_D|:Nc:|_TWISTED_##x

/* Cache size */
#define CACHE_LINE_SIZE 128
#define ALIGN(p,n) ((void *)((((ptrdiff_t)(p))+(n)+CACHE_LINE_SIZE-1) & \
                                                         ~(CACHE_LINE_SIZE-1)))


/* QCD types (qa0 controls these definitions) */
struct Twisted;
struct SUnF;
struct SUnD;
struct FermionF;
struct FermionD;
struct ProjectedFermionF;
struct ProjectedFermionD;

/* Internal types */
struct local {
  int lo[Q(DIM)];
  int hi[Q(DIM)];
  int dx[Q(DIM)];
};

/* structs neighbor and up_pack are defined by qa0 */
struct neighbor;
struct up_pack;
struct down_pack;

struct eo_lattice {
  struct Q(State) *state;                  /* back pointer to state */
  int              face_size;              /* 4-d size of the face */
  int              body_size;              /* 4-d size of the body */
  int              full_size;              /* face + body */
  int             *lx2v;                   /* 4-d layout 2 vector translation */
  int             *v2lx;                   /* only for init */
  int              Ls;                     /* Ls */
  struct local    *local;                  /* points to state.local */

  struct neighbor *neighbor;               /* neighbor data (body,face) */
  int              send_up_size[Q(DIM)];   /* 4-d send size in each up-dir */
  struct up_pack  *up_pack[Q(DIM)];        /* 4-d (U,f) for up-face packing */
  int              send_down_size[Q(DIM)]; /* 4-d send size in each down-dir */
  struct down_pack *down_pack[Q(DIM)];      /* 4-d (f) for down-face packing */
  int              receive_up_size[Q(DIM)]; /* 4-d (U,f) up receive size */
  int              receive_down_size[Q(DIM)]; /* 4-d (f) down receive size */

  int              real_size;              /* 0, 4 or 8 */ 
  int              h_valid;                /* is .handle valid? */
  QMP_msghandle_t  handle;                 /* global send&receive handle */
  QMP_msghandle_t  th[4*Q(DIM)];           /* transitody handles */
  int              th_count;               /* number of valid th[] */
  QMP_msgmem_t     mh[4*Q(DIM)];           /* memory handles for th[] */
  int              mh_count;               /* number of valid mh[] */
  QMP_mem_t       *mem[4*Q(DIM)];          /* memory for mh[] */
  int              mem_count;              /* number of valid mem[] */
  void            *send_up_buf[Q(DIM)];    /* pf up-bufs */
  void            *send_down_buf[Q(DIM)];  /* pf down-bufs */
  void            *receive_buf[2*Q(DIM)];  /* pf receive bufs (up[], down[]) */
  int              total_send;             /* bytes to send */
  int              total_receive;          /* bytes to receive */
};

struct Q(State) {
  const char        *version;         /* to get version string into app */
  int                used;            /* gc ref counter */
  int                saved;           /* gc internal counter */
  size_t             allocated;       /* currently allocated bytes */
  size_t             max_allocated;   /* maximum allocation */

  int                error_latched;   /* if 0, allow error recording */
  int                fatal_error;     /* if 0, allow reseting latch */
  const char        *error;           /* error string */

  int                real_size;       /* 0, 4 or 8 */ 
  struct eo_lattice  even;            /* even sublattice */
  struct eo_lattice  odd;             /* odd sublattice */

  struct timeval     t0, t1;          /* for timing */
  double             time_sec;        /* seconds in the last routine */
  long long          flops;           /* FLOP in the last routine */
  long long          sent;            /* bytes sent in the last routine */
  long long          received;        /* bytes received in the last routine */

  int                volume;          /* 4-d volume */
  int                lattice[Q(DIM)]; /* 4-d lattice size */
  struct local       local;           /* 4-d local sublattice */
  int                node;            /* local node id */
  int                neighbor_up[Q(DIM)];   /* up neighbors on the net */
  int                neighbor_down[Q(DIM)]; /* down neighbors on the net */
  int                network[Q(DIM)]; /* the network geometry */
  int                master_p;        /* are we the master? */
  int               *lx2v;            /* Sublattice 1-d -> 4-d translation */
  int               *v2lx;            /* Only for init */
};

typedef enum {
    CG_SUCCESS,
    CG_MAXITER,
    CG_EIGCONV,
    CG_ZEROMODE,
    CG_NOEMEM
} CG_STATUS;

/* debug printing */
extern int QDP_this_node;
extern int QDP_is_initialized(void);

/* Timing */
#define BEGIN_TIMING(s) do { gettimeofday(&((s)->t0), NULL); } while (0)
#define END_TIMING(s, f, snd, rcv) do { \
    gettimeofday(&((s)->t1), NULL); \
    (s)->time_sec = ((s)->t1.tv_sec - (s)->t0.tv_sec) \
      + 1e-6 * ((s)->t1.tv_usec - (s)->t0.tv_usec); \
    (s)->flops = (f); (s)->sent = (snd); (s)->received = (rcv); } while (0)

/* Argument checking */
#define DECLARE_STATE struct Q(State) *state = NULL
#define CHECK_ARG0(n) do { if ((n) == 0) return 1;      \
    state = (n)->state; } while (0)
#define CHECK_ARGn(n,f) do { if ((n) == 0)                              \
      return q(set_error)(state, 0, f "(): NULL argument");             \
    if ((n)->state != state)                                            \
      return q(set_error)(state, 0, f "(): geometry mismatch"); } while (0)
#define CHECK_POINTER(n,f) do { if ((n) == 0)                           \
      return q(set_error)(state, 0, f "(): NULL argument"); } while (0)

#endif /* !defined(MARK_21eedb1e5eff4d4ba0462475b43ae655) */


/* ugly but working solution */
#undef QOP_TWISTED_DEFAULT_PRECISION_SAVE__
#if   QOP_TWISTED_DEFAULT_PRECISION=='F'
# define QOP_TWISTED_DEFAULT_PRECISION_SAVE__ 'F'
#elif QOP_TWISTED_DEFAULT_PRECISION=='D'
# define QOP_TWISTED_DEFAULT_PRECISION_SAVE__ 'D'
#endif

# undef QOP_TWISTED_DEFAULT_PRECISION
# undef qx
# undef QX
# undef REAL
# undef SUn
# undef FermionX
# undef VectorFermionX
# undef ProjectedFermionX
# define QOP_TWISTED_DEFAULT_PRECISION 'D'
# define qx(x) qd(x)
# define QX(x) QD(x)
# define REAL double
# define SUn  SUnD
# define FermionX FermionD
# define VectorFermionX VectorFermionD
# define ProjectedFermionX ProjectedFermionD
# include "twisted-x.h"


# undef QOP_TWISTED_DEFAULT_PRECISION
# undef qx
# undef QX
# undef REAL
# undef SUn
# undef FermionX
# undef VectorFermionX
# undef ProjectedFermionX
# define QOP_TWISTED_DEFAULT_PRECISION 'F'
# define qx(x) qf(x)
# define QX(x) QF(x)
# define REAL float
# define SUn  SUnF
# define FermionX FermionF
# define VectorFermionX VectorFermionF
# define ProjectedFermionX ProjectedFermionF
# include "twisted-x.h"


# undef QOP_TWISTED_DEFAULT_PRECISION
# undef qx
# undef QX
# undef REAL
# undef SUn
# undef FermionX
# undef VectorFermionX
# undef ProjectedFermionX
#if   QOP_TWISTED_DEFAULT_PRECISION_SAVE__=='D'
# define QOP_TWISTED_DEFAULT_PRECISION 'D'
# define qx(x) qd(x)
# define QX(x) QD(x)
# define REAL double
# define SUn  SUnD
# define FermionX FermionD
# define VectorFermionX VectorFermionD
# define ProjectedFermionX ProjectedFermionD
#elif QOP_TWISTED_DEFAULT_PRECISION_SAVE__=='F'
# define QOP_TWISTED_DEFAULT_PRECISION 'F'
# define qx(x) qf(x)
# define QX(x) QF(x)
# define REAL float
# define SUn  SUnF
# define FermionX FermionF
# define VectorFermionX VectorFermionF
# define ProjectedFermionX ProjectedFermionF
#endif

/* mixed precision operations */
/* Fd = Fd + Ff */
size_t q(f_d_eq_dpf)(struct FermionD *dst,
                           size_t size,
                           const struct FermionD *src_d,
                           const struct FermionF *src_f);
/* Ff = Fd - Fd */
size_t q(f_f_eq_dmd_norm2)(struct FermionF *dst,
                                 double *local_norm,
                                 size_t size,
                                 const struct FermionD *src_a,
                                 const struct FermionD *src_b);

/* converting gauge from double down to float */
void q(g_f_eq_d)(struct SUnF *dst,
                 size_t size,
                 const struct SUnD *src);

/* the mixed solver */
int q(mixed_cg)(struct Q(State)             *state,
                const char                  *name,
                struct QD(Fermion)          *psi,
                int                         *out_iterations,
                double                      *out_epsilon,
                const struct QD(Fermion)    *psi_0,
                const struct QD(Gauge)      *gauge,
                const struct QD(Fermion)    *eta,
                struct QF(Deflator)         *deflator,
                int                          f_iter,
                double                       f_epsilon,
                int                          max_iterations,
                double                       min_epsilon,
                unsigned int                 options);

/* layout translation */
void q(l2v)(int x[Q(DIM)], const struct local *local, int p);
int q(v2l)(const int x[Q(DIM)], const struct local *local);

/* Implementation functions */
int q(set_error)(struct Q(State) *state, int fatal, const char *error);

int q(setup_comm)(struct Q(State) *state, int real_size);
int q(free_comm)(struct Q(State) *state);

void *q(malloc)(struct Q(State) *state, size_t bytes);
void *q(allocate_aligned)(struct Q(State) *state,
                          size_t *size, void **aligned_ptr,
                          size_t hdr_size, size_t bulk_size);
void q(free)(struct Q(State) *state, void *ptr, size_t bytes);
void q(cleanup_state)(struct Q(State) *state);

/* Backend controled structure sizes */
size_t q(sizeof_neighbor)(size_t volume);
size_t q(sizeof_up_pack)(size_t volume);
size_t q(sizeof_down_pack)(size_t volume);
size_t q(sizeof_twist)(void);

/* qa0 level data access routines */
size_t q(get_down_pack_f)(const struct down_pack *up, size_t p);
size_t q(get_up_pack_f)(const struct up_pack *up, size_t p);
void q(put_down_pack)(struct down_pack *down, size_t p, size_t f);
void q(get_down_pack)(size_t *f, const struct down_pack *up, size_t p);
void q(put_up_pack)(struct up_pack *up, size_t p, size_t f, size_t u);
void q(get_up_pack)(size_t *f, size_t *u, const struct up_pack *up, size_t p);
void q(put_neighbor)(struct neighbor *n, size_t p,
                     size_t m,
                     const size_t f_up[Q(DIM)], size_t u_up,
                     const size_t f_down[Q(DIM)], const size_t u_down[Q(DIM)]);
void q(get_neighbor)(size_t *m, size_t *f_up, size_t *u_up,
                     size_t *f_down, size_t *u_down,
                     const struct neighbor *n, size_t p);
void q(fix_neighbor_f_up)(struct neighbor *n, size_t p, size_t f_up, size_t d);
void q(fix_neighbor_f_down)(struct neighbor *n, size_t p, size_t f_down, size_t d);

void q(put_twisted)(struct Twisted *ptr, double x_re, double x_im, double y_re, double y_im);
void q(put_twisted_inv)(struct Twisted *ptr, double x_re, double x_im, double y_re, double y_im);

