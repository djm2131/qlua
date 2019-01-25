#define QDP_Precision 'D'
#define QDP_Nc 3
#include <qdp.h>
#include <qmp.h>

#define NDIM  4
#define NELEMS(x) (sizeof (x) / sizeof ((x)[0]))

/* 
extern int lattice[NDIM];
*/

#define printf0 if (QDP_this_node == 0) printf

/* gauge */
extern void create_Mvector(QDP_ColorMatrix *U[], int size);
extern void destroy_Mvector(QDP_ColorMatrix *U[], int size);

extern QLA_Real plaquette(QDP_ColorMatrix *U[]);
extern void clover(QDP_ColorMatrix *Cl[], QDP_ColorMatrix *U[]);
extern int read_gauge(QDP_ColorMatrix *U[], const char *name);
extern void unit_gauge(QDP_ColorMatrix *U[]);
extern void point_gauge(QDP_ColorMatrix *U[],
                        const int p[], int d, int a, int b, int ri);
extern void coord_gauge(QDP_ColorMatrix *U[]);
extern void dump_gauge(char *name, QDP_ColorMatrix *U[], int d, int u_p);

/* fermion */
extern void create_Dvector(QDP_DiracFermion *U[], int size);
extern void destroy_Dvector(QDP_DiracFermion *U[], int size);

extern void point_fermion(QDP_DiracFermion *f,
                          const int p[], int c, int d, int ri);
extern void dump_fermion(char *name, QDP_DiracFermion *f);

/* clover */
extern void
std_clover_op(QDP_DiracFermion *psi,
              QDP_ColorMatrix *U[],
              QDP_ColorMatrix *Cl[],
              double kappa,
              double c_sw,
              QDP_DiracFermion *phi);

