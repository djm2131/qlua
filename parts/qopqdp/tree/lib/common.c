#include <stdlib.h>
#include <sys/time.h>
#include <qop_internal.h>

QOP_common_t QOP_common = {.inited=0};
extern QDP_Layout *QOP_layout_user;

static int
compare_sizes(int n1, int *v1, char *s1, int n2, int *v2, char *s2)
{
  int i, n, error=0;

  n = n1;
  if(n2>n1) n = n2;
  for(i=0; i<n; i++) {
    int t1=1, t2=1;
    if(i<n1) t1 = v1[i];
    if(i<n2) t2 = v2[i];
    if(t1!=t2) error = 1;
  }
  if(error) {
    if(QDP_this_node==0) {
      printf("QOP Warning: %s != %s\n", s1, s2);
      printf("%s =", s1);
      for(i=0; i<n1; i++) printf(" %i", v1[i]);
      printf("\n%s =", s2);
      for(i=0; i<n2; i++) printf(" %i", v2[i]);
      printf("\n");
      fflush(stdout);
    }
  }
  return error;
}

  /*********************/
  /*  Public routines  */
  /*********************/

QOP_status_t
QOP_init(QOP_layout_t *layout)
{
  QOP_status_t retval = QOP_SUCCESS;

  CHECK_NOT_INIT;
  QOP_common.inited = 1;
  QOP_common.verbosity = 0;
  QOP_common.proflevel = 0;
  QOP_common.we_inited_qdp = 0;
  QOP_common.ndim = layout->latdim;

  if(!QDP_is_initialized()) {
    QDP_initialize(NULL, NULL);
    QDP_set_default_layout(QOP_layout_user);
    QDP_set_latsize(layout->latdim, layout->latsize);
    QDP_create_layout();
    QOP_common.we_inited_qdp = 1;
  } else if (layout->latsize != NULL) {
    int error;
    int qdplatdim = QDP_ndim(); /* ugly, but I don't see how to improve it */
    int qdplatsize[qdplatdim];
    QDP_latsize(qdplatsize);
    error = compare_sizes(layout->latdim, layout->latsize, "QOP lattice",
			  qdplatdim, qdplatsize, "QDP lattice");
    if(error) retval = QOP_FAIL;
  }

  if( (layout->machdim>=0) && (QMP_logical_topology_is_declared()) ) {
    int error;
    int qmpndim;
    const int *qmpdims;
    qmpndim = QMP_get_logical_number_of_dimensions();
    qmpdims = QMP_get_logical_dimensions();
    error = compare_sizes(layout->machdim, layout->machsize, "QOP machsize",
			  qmpndim, (int *)qmpdims, "QMP topology");
    if(error) retval = QOP_FAIL;
  }

#ifdef _OPENMP
  QDP_set_block_size(1024*1024*1024);
#endif

  return retval;
}

QOP_status_t
QOP_finalize(void)
{
  CHECK_INIT;
  QOP_common.inited = 0;
  if(QOP_common.we_inited_qdp) {
    QDP_finalize();
  }
  return QOP_SUCCESS;
}

int
QOP_is_initialized(void)
{
  return QOP_common.inited;
}

static const char *vs = VERSION;

const char *
QOP_version_str(void)
{
  return vs;
}

int
QOP_version_int(void)
{
  int maj, min, bug;
  sscanf(vs, "%i.%i.%i", &maj, &min, &bug);
  return ((maj*1000)+min)*1000 + bug;
}

int
QOP_verbose(int level)
{
  int old = QOP_common.verbosity;
  QOP_common.verbosity = level;
  return old;
}

int
QOP_profcontrol(int level)
{
  int old = QOP_common.proflevel;
  QOP_common.proflevel = level;
  return old;
}

int
QOP_node_number_raw(int coords[])
{
  CHECK_INIT;
  return QDP_node_number(coords);
}

int
QOP_node_index_raw_V(int coords[], QOP_evenodd_t evenodd)
{
  CHECK_INIT;
  return QDP_index(coords);
}

int
QOP_node_index_raw_D(int coords[], QOP_evenodd_t evenodd)
{
  CHECK_INIT;
  return QDP_index(coords);
}

int
QOP_node_index_raw_G(int coords[], QOP_evenodd_t evenodd)
{
  CHECK_INIT;
  return QDP_index(coords);
}

int
QOP_node_index_raw_F(int coords[], QOP_evenodd_t evenodd)
{
  CHECK_INIT;
  return QDP_index(coords);
}

int
QOP_sites_on_node_raw_V(QDP_Lattice *lat, QOP_evenodd_t evenodd)
{
  return QDP_sites_on_node_L(lat);
}

int
QOP_sites_on_node_raw_D(QDP_Lattice *lat, QOP_evenodd_t evenodd)
{
  return QDP_sites_on_node_L(lat);
}

int
QOP_sites_on_node_raw_G(QDP_Lattice *lat, QOP_evenodd_t evenodd)
{
  return QDP_sites_on_node_L(lat);
}

int
QOP_sites_on_node_raw_F(QDP_Lattice *lat, QOP_evenodd_t evenodd)
{
  return QDP_sites_on_node_L(lat);
}

static int
sub32_func(QDP_Lattice *lat, int coords[], void *args)
{
  int nd = QDP_ndim_L(lat);
  int s=0, r=0;
  for(int i=nd-1; i>=0; i--) {
    s = 2*(s + (coords[i]%2));
    r += coords[i]/2;
  }
  return s + (r%2);
}

typedef struct {
  QDP_Lattice *lat;
  QDP_Subset *sub32;
} latsub32_t;
static latsub32_t *latsub32 = NULL;
static int nlatsub32 = 0, nlatsub32alloc = 0;

QDP_Subset *
QOP_get_sub32(QDP_Lattice *lat)
{
  for(int i=0; i<nlatsub32; i++) {
    if(latsub32[i].lat==lat) return latsub32[i].sub32;
  }
  int n = nlatsub32+1;
  if(nlatsub32alloc<n) {
    nlatsub32alloc = 2*n;
    latsub32 = realloc(latsub32, nlatsub32alloc*sizeof(latsub32_t));
  }
  QDP_Subset *s = QDP_create_subset_L(lat, sub32_func, NULL, 0, 32);
  latsub32[nlatsub32].lat = lat;
  latsub32[nlatsub32].sub32 = s;
  nlatsub32 = n;
  return s;
}

  /***********************/
  /*  Internal routines  */
  /***********************/

#if 0
double
QOP_time(void)
{
#if 0
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return tv.tv_sec + 1e-6*tv.tv_usec;
#else
  return QDP_time();
#endif
}
#endif
