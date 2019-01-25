#include "clover-test.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h> /* for sprintf only */
#include <math.h>


void
create_Mvector(QDP_ColorMatrix *U[], int size)
{
    int i;

    for (i = 0; i < size; i++)
        U[i] = QDP_create_M();
}

void
destroy_Mvector(QDP_ColorMatrix *U[], int size)
{
    int i;
    
    for (i = 0; i < size; i++) {
        QDP_destroy_M(U[i]); 
        U[i] = 0;
    }
}

void
dump_gauge(char *name, QDP_ColorMatrix *U[], int d, int u_p)
{
    int i, a, b, mu, sites, j;
    int p[NDIM];
    int len = strlen(name);
    char *fn = malloc(len + 12);
    FILE *f;
    QLA_ColorMatrix *m;
    QLA_Complex z;

    sprintf(fn, "%s.%d", name, QDP_this_node);
    f = fopen(fn, "wt");
    if (f == 0) {
        printf("NODE(%d): error opening %s\n", QDP_this_node, fn);
        goto end;
    }

    sites = QDP_numsites(QDP_this_node);
    for (mu = 0; mu < d; mu++) {
        m = QDP_expose_M(U[mu]);
        for (i = 0; i < sites; i++) {
            QDP_get_coords(p, QDP_this_node, i);
            for (a = 0; a < QDP_Nc; a++) {
                for (b = 0; b < QDP_Nc; b++) {
                    QLA_Real zr, zi;
                    int sh;

                    QLA_c_eq_c(z, QLA_elem_M(m[i], a, b));
                    QLA_r_eq_Re_c(zr, z);
                    QLA_r_eq_Im_c(zi, z);

                    sh = 0;
                    if (u_p) {
                        if (a == b) {
                            if (fabs(zr - 1.0) > 1e-7)
                                sh = 1;
                            else if (fabs(zi) > 1e-7)
                                sh = 1;
                        } else if (fabs(zr) + fabs(zi) > 1e-7) {
                            sh = 1;
                        }
                    } else {
                        if (fabs(zr) + fabs(zi) > 1e-7) {
                            sh = 1;
                        }
                    }
                    if (sh == 1) {
                        fprintf(f, "U[%d]", mu);
                        for (j = 0; j < NDIM; j++)
                            fprintf(f, " %2d", p[j]);
                        fprintf(f, " ; %d %d] = %20.8e %20.8e\n",
                                a, b, zr, zi);
                    }
                }
            }
        }
        QDP_reset_M(U[mu]);
    }

    fclose(f);
end:
    free(fn);
}

int
read_gauge(QDP_ColorMatrix *link[], const char *name)
{
  QDP_String *id = QDP_string_create();
  QDP_Reader *reader;

  reader = QDP_open_read(id, (char *)name);
  if (reader == 0) {
    printf0("open_read(%s) failed\n", name);
    return 1;
  }
  printf0("---------------------------------------------------------------\n");
  printf0("file metadata(%s):\n", name);
  printf0("%s\n", QDP_string_ptr(id));
  printf0("---------------------------------------------------------------\n");
  if( QDP_vread_M(reader, id, link, NDIM) ) {
    printf0("vread_M(%s) failed\n", name);
    return 1;
  }
  printf0("---------------------------------------------------------------\n");
  printf0("record metadata(%s):\n", name);
  printf0("%s\n", QDP_string_ptr(id));
  printf0("---------------------------------------------------------------\n");
  QDP_close_read(reader);
  QDP_string_destroy(id);
  return 0;  
}

void
coord_gauge(QDP_ColorMatrix *U[])
{
    int d, sites, i, p[NDIM], a, b, j;
    QLA_Real v;
    QLA_Complex w;
    QLA_ColorMatrix *qu;

    sites = QDP_numsites(QDP_this_node);
    for (d = 0; d < NDIM; d++) {
        qu = QDP_expose_M(U[d]);
        for (i = 0; i < sites; i++) {
            QDP_get_coords(p, QDP_this_node, i);
            v = 0.0;
            for (j = 0; j < NDIM; j++)
                v = v * 10 + p[j];
            v = v * 10 + d;
            for (a = 0; a < QDP_Nc; a++) {
                for (b = 0; b < QDP_Nc; b++) {
                    QLA_Real z = v * 1000 + a * 100 + b * 10;
                    QLA_c_eq_r_plus_ir(w, z, z + 1);
                }
            }
        }
        QDP_reset_M(U[d]);
    }
}

void
unit_gauge(QDP_ColorMatrix *U[])
{
    int d;
    QLA_Complex w;

    QLA_c_eq_r(w, 1.0);
    for (d = 0; d < NDIM; d++) {
        QDP_M_eq_c(U[d], &w, QDP_all);
    }
}

void
point_gauge(QDP_ColorMatrix *U[],
            const int p[], int mu, int a, int b, int ri)
{
    int d, n, i;
    QLA_Complex w;
    QLA_ColorMatrix *qu;

    QLA_c_eq_r(w, 1.0);
    for (d = 0; d < NDIM; d++) {
        QDP_M_eq_c(U[d], &w, QDP_all);
    }
    qu = QDP_expose_M(U[mu]);
    n = QDP_node_number(p);
    i = QDP_index(p);
    if (n == QDP_this_node) {
        QLA_Complex z;
        if (ri) {
            QLA_Complex v;
            QLA_c_eq_r(v, 1.0);
            QLA_c_eq_ic(z, v);
        } else {
            QLA_c_eq_r(z, 1.0);
        }
        QLA_c_eq_c(QLA_elem_M(qu[i], a, b), z);
    }
    QDP_reset_M(U[mu]);
}

/****
 **       t[1]
 **  ^-----------X
 **  |           |
 **  | nu        | t[0]
 **  |     mu    |
 **  o----------->
 **
 **
 **     C               3 0
 **   D   B             2 1
 **     A
 **
 ** clover indices:   mu nu   gamma           wilson mu
 **  0                 0  1     -i [3]         0          [1]
 **  1                 0  2     -i [5]         1          [2]
 **  2                 0  3     -i [9]         2          [4]
 **  3                 1  2     -i [6]         3          [8]
 **  4                 1  3     -i [10]
 **  5                 2  3     -i [12]
 */

void
clover(QDP_ColorMatrix *cl[], QDP_ColorMatrix *U[])
{
    int mu, nu, i;
    QDP_ColorMatrix *t[6];

    create_Mvector(t, NELEMS(t));

    for (i = mu = 0; mu < NDIM; mu++) {
        for (nu = mu + 1; nu < NDIM; nu++, i++) {
            /* [0]: B */
            QDP_M_eq_sM(t[0], U[nu], QDP_neighbor[mu], QDP_forward, QDP_all);
            /* [1]: DA */
            QDP_M_eq_Ma_times_M(t[1], U[nu], U[mu], QDP_all);
            /* [2]: DAB */
            QDP_M_eq_M_times_M(t[2], t[1], t[0], QDP_all);
            /* [3]: DAB|nu */
            QDP_M_eq_sM(t[3], t[2], QDP_neighbor[nu], QDP_backward, QDP_all);
            /* [4]: DABC|nu = p1 */
            QDP_M_eq_M_times_Ma(t[4], t[3], U[mu], QDP_all);
            /* [1]: C */
            QDP_M_eq_sM(t[1], U[mu], QDP_neighbor[nu], QDP_forward, QDP_all);
            /* [5]: CDAB|nu */
            QDP_M_eq_Ma_times_M(t[5], U[mu], t[3], QDP_all);
            /* [2]: BC */
            QDP_M_eq_M_times_Ma(t[2], t[0], t[1], QDP_all);
            /* [3]: BCD */
            QDP_M_eq_M_times_Ma(t[3], t[2], U[nu], QDP_all);
            /* [4]: ABCD + DABC|nu */
            QDP_M_peq_M_times_M(t[4], U[mu], t[3], QDP_all);
            /* [5]: BCDA + CDAB|nu */
            QDP_M_peq_M_times_M(t[5], t[3], U[mu], QDP_all);
            /* [2]: BCDA|mu + CDAB|nu|mu */
            QDP_M_eq_sM(t[2], t[5], QDP_neighbor[mu], QDP_backward, QDP_all);
            /* [4]: clover */
            QDP_M_peq_M(t[4], t[2], QDP_all);
            
            QDP_M_eq_M(cl[i], t[4], QDP_all);
            QDP_M_meq_Ma(cl[i], t[4], QDP_all);
        }
    }

    destroy_Mvector(t, NELEMS(t));
    return;
}

QLA_Real
plaquette(QDP_ColorMatrix *link[])
{
  int mu, nu;
  QLA_Real plaq, total;
  QDP_ColorMatrix *t[4];

  total = 0;

  create_Mvector(t, NELEMS(t));

  for (mu = 0; mu < NDIM; mu++) {
    for (nu = mu + 1; nu < NDIM; nu++) {
      QDP_M_eq_sM(t[0], link[nu], QDP_neighbor[mu], QDP_forward, QDP_all);
      QDP_M_eq_sM(t[1], link[mu], QDP_neighbor[nu], QDP_forward, QDP_all);
      QDP_M_eq_Ma_times_M(t[2], link[nu], link[mu], QDP_all);
      QDP_M_eq_M_times_M(t[3], t[2], t[0], QDP_all);
      QDP_r_eq_re_M_dot_M(&plaq, t[1], t[3], QDP_all);
      total += plaq;
    }
  }

  destroy_Mvector(t, NELEMS(t));

  return total;
}
