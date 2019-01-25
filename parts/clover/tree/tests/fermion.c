#include "clover-test.h"
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h> /* for sprintf only */


void
create_Dvector(QDP_DiracFermion *f[], int size)
{
    int i;

    for (i = 0; i < size; i++)
        f[i] = QDP_create_D();
}

void
destroy_Dvector(QDP_DiracFermion *f[], int size)
{
    int i;
    
    for (i = 0; i < size; i++) {
        QDP_destroy_D(f[i]); 
        f[i] = 0;
    }
}


void
point_fermion(QDP_DiracFermion *f,
              const int p[], int c, int d, int ri)
{
    int n, i;
    QLA_DiracFermion *qladf;

    /* zero source field */
    QDP_D_eq_zero(f, QDP_all);

    /* expose the internal array of QLA objects */
    qladf = QDP_expose_D(f);

    /* find the node number and site index of point */
    n = QDP_node_number(p);
    i = QDP_index(p);
    
    /* if the point is on this node set it */
    if(n==QDP_this_node) {
        QLA_Complex z, v;
        if (ri) {
            QLA_c_eq_r(v, 1.0);
            QLA_c_eq_ic(z, v);
        } else {
            QLA_c_eq_r(z, 1.0);
        }
        QLA_c_eq_c(QLA_elem_D(qladf[i], c, d), z);
    }
        
    /* "un-expose" the field */
    QDP_reset_D(f);
}    

void
dump_fermion(char *name, QDP_DiracFermion *src)
{
    int i, c, d, sites, j;
    int p[NDIM];
    int len = strlen(name);
    char *fn = malloc(len + 12);
    FILE *f;
    QLA_DiracFermion *qladf;
    QLA_Complex z;

    sprintf(fn, "%s.%d", name, QDP_this_node);
    f = fopen(fn, "wt");
    if (f == 0) {
        printf("NODE(%d): error opening %s\n", QDP_this_node, fn);
        goto end;
    }

    sites = QDP_numsites(QDP_this_node);
    qladf = QDP_expose_D(src);
    for (i = 0; i < sites; i++) {
        QDP_get_coords(p, QDP_this_node, i);
        for (c = 0; c < QDP_Nc; c++) {
            for (d = 0; d < 4; d++) { /* Spinor size, not NDIM */
                QLA_Real zr, zi;

                QLA_c_eq_c(z, QLA_elem_D(qladf[i], c, d));
                QLA_r_eq_Re_c(zr, z);
                QLA_r_eq_Im_c(zi, z);

                if (fabs(zr) + fabs(zi) > 1e-7) {
                    fprintf(f, "F[");
                    for (j = 0; j < NDIM; j++)
                        fprintf(f, " %2d", p[j]);
                    fprintf(f, " ; %d %d] = %20.8e %20.8e\n", c, d, zr, zi);
                }
            }
        }
    }
    QDP_reset_D(src);

    fclose(f);
end:
    free(fn);
}
