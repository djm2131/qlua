#include <stdio.h>
#include <stdlib.h>

/* This is a test for the complex matrix inverse used by the clover code */

#define Q(x) x
#define qx(x) x
#define PROJECTED_FERMION_DIM 2
#define COLORS 3

#define X(m,a,i,b,j) (&((m)[2 * ((j) + Q(PROJECTED_FERMION_DIM)         \
                                 * ((b) + Q(COLORS)                     \
                                    * ((i) + Q(PROJECTED_FERMION_DIM)   \
                                       * (a))))]))


#define MDIM (PROJECTED_FERMION_DIM *PROJECTED_FERMION_DIM*COLORS*COLORS*2)

#define clFOR(a, i) for (a = 0; a < Q(COLORS); a++) \
        for (i = 0; i < Q(PROJECTED_FERMION_DIM); i++)

static void
project_r(int a, int i, int b, int j, double r[])
{
    int c, k;
    double xi, yi, xj, yj;

    xi = X(r, a, i, a, i)[0];
    yi = X(r, a, i, a, i)[1];
    xj = X(r, b, j, a, i)[0];
    yj = X(r, b, j, a, i)[1];
    for (c = a, k = i + 1; k < Q(PROJECTED_FERMION_DIM); k++) {
        double zi = X(r, a, i, c, k)[0];
        double ti = X(r, a, i, c, k)[1];
        double zj = X(r, b, j, c, k)[0];
        double tj = X(r, b, j, c, k)[1];
        X(r, b, j, c, k)[0] = xi * zj - yi * tj - xj * zi + yj * ti;
        X(r, b, j, c, k)[1] = xi * tj + yi * zj - xj * ti - yj * zi;
    }
    for (c = a + 1; c < Q(COLORS); c++) {
        for (k = 0; k < Q(PROJECTED_FERMION_DIM); k++) {
            double zi = X(r, a, i, c, k)[0];
            double ti = X(r, a, i, c, k)[1];
            double zj = X(r, b, j, c, k)[0];
            double tj = X(r, b, j, c, k)[1];
            X(r, b, j, c, k)[0] = xi * zj - yi * tj - xj * zi + yj * ti;
            X(r, b, j, c, k)[1] = xi * tj + yi * zj - xj * ti - yj * zi;
        }
    }
}

static void
project_v(int a, int i, int b, int j, double r[], double v[])
{
    int c, k;
    double xi, yi, xj, yj;

    xi = X(r, a, i, a, i)[0];
    yi = X(r, a, i, a, i)[1];
    xj = X(r, b, j, a, i)[0];
    yj = X(r, b, j, a, i)[1];
    clFOR(c, k) {
        double zi = X(v, a, i, c, k)[0];
        double ti = X(v, a, i, c, k)[1];
        double zj = X(v, b, j, c, k)[0];
        double tj = X(v, b, j, c, k)[1];
        X(v, b, j, c, k)[0] = xi * zj - yi * tj - xj * zi + yj * ti;
        X(v, b, j, c, k)[1] = xi * tj + yi * zj - xj * ti - yj * zi;
    }
}

static void
scale(int a, int i, int b, int j, double r[], double v[])
{
    int c, k;
    double x = X(r, b, j, a, i)[0];
    double y = X(r, b, j, a, i)[1];
    clFOR(c, k) {
        double z = X(v, a, i, c, k)[0];
        double t = X(v, a, i, c, k)[1];
        X(v, b, j, c, k)[0] -= x * z - y * t;
        X(v, b, j, c, k)[1] -= x * t + y * z;
    }
}

static void
cl_invert(double v[], double r[])
{
    int a, i, b, j, ax, ix;

    clFOR(a, i) {
        clFOR(b, j) {
            X(v, a, i, b, j)[0] = ((b == a) && (i == j))? 1.0: 0.0;
            X(v, a, i, b, j)[1] = 0;
        }
    }

    clFOR(a, i) {
        double x = X(r, a, i, a, i)[0];
        double y = X(r, a, i, a, i)[1];
        double d = x * x + y * y;
        int am = a;
        int im = i;
        for (ax = a, ix = i + 1; ix < Q(PROJECTED_FERMION_DIM); ix++) {
            double xx = X(r, ax, ix, a, i)[0];
            double yy = X(r, ax, ix, a, i)[1];
            double dd = xx * xx + yy * yy;
            if (dd > d) d = dd, am = ax, im = ix;
        }
        for (ax = a + 1; ax < Q(COLORS); ax++) {
            for (ix = 0; ix < Q(PROJECTED_FERMION_DIM); ix++) {
                double xx = X(r, ax, ix, a, i)[0];
                double yy = X(r, ax, ix, a, i)[1];
                double dd = xx * xx + yy * yy;
                if (dd > d) d = dd, am = ax, im = ix;
            }
        }
        if ((am != a) || (im != i)) {
            for (b = a, j = i; j < Q(PROJECTED_FERMION_DIM); j++) {
                double x = X(r, a, i, b, j)[0];
                double y = X(r, a, i, b, j)[1];
                X(r, a, i, b, j)[0] = X(r, am, im, b, j)[0];
                X(r, a, i, b, j)[1] = X(r, am, im, b, j)[1];
                X(r, am, im, b, j)[0] = x;
                X(r, am, im, b, j)[1] = y;
            }
            for (b = a + 1; b < Q(COLORS); b++) {
                for (j = 0; j < Q(PROJECTED_FERMION_DIM); j++) {
                    double x = X(r, a, i, b, j)[0];
                    double y = X(r, a, i, b, j)[1];
                    X(r, a, i, b, j)[0] = X(r, am, im, b, j)[0];
                    X(r, a, i, b, j)[1] = X(r, am, im, b, j)[1];
                    X(r, am, im, b, j)[0] = x;
                    X(r, am, im, b, j)[1] = y;
                }
            }
            clFOR(b, j) {
                double x = X(v, a, i, b, j)[0];
                double y = X(v, a, i, b, j)[1];
                X(v, a, i, b, j)[0] = X(v, am, im, b, j)[0];
                X(v, a, i, b, j)[1] = X(v, am, im, b, j)[1];
                X(v, am, im, b, j)[0] = x;
                X(v, am, im, b, j)[1] = y;
            }
        }
        for (b = a, j = i + 1; j < Q(PROJECTED_FERMION_DIM); j++) {
            project_r(a, i, b, j, r);
            project_v(a, i, b, j, r, v);
        }
        for (b = a + 1; b < Q(COLORS); b++) {
            for (j = 0; j < Q(PROJECTED_FERMION_DIM); j++) {
                project_r(a, i, b, j, r);
                project_v(a, i, b, j, r, v);
            }
        }
    }

    for (a = Q(COLORS); a--;) {
        for (i = Q(PROJECTED_FERMION_DIM); i--;) {
            double x = X(r, a, i, a, i)[0];
            double y = X(r, a, i, a, i)[1];
            double dd = 1/(x * x + y * y);
            x = x * dd;
            y = -y * dd;
            clFOR(b, j) {
                double z = X(v, a, i, b, j)[0];
                double t = X(v, a, i, b, j)[1];
                X(v, a, i, b, j)[0] = x * z - y * t;
                X(v, a, i, b, j)[1] = x * t + y * z;
            }
            for (b = 0; b < a; b++) {
                for (j = 0; j < Q(PROJECTED_FERMION_DIM); j++) {
                    scale(a, i, b, j, r, v);
                }
            }
            for (b = a, j = 0; j < i; j++) {
                scale(a, i, b, j, r, v);
            }
        }
    }
}

static void
mul(double v[], const double x[], const double y[])
{
    int a, i, b, j;
    int c, k;

    clFOR(a, i) {
        clFOR(b, j) {
            double vr = 0;
            double vi = 0;
            clFOR(c, k) {
                vr += X(x, a, i, c, k)[0] * X(y, c, k, b, j)[0];
                vr -= X(x, a, i, c, k)[1] * X(y, c, k, b, j)[1];
                vi += X(x, a, i, c, k)[0] * X(y, c, k, b, j)[1];
                vi += X(x, a, i, c, k)[1] * X(y, c, k, b, j)[0];
            }
            X(v, a, i, b, j)[0] = vr;
            X(v, a, i, b, j)[1] = vi;
        }
    }
}

int
main(int argc, char *argv[])
{
    double cl[MDIM];
    double v[MDIM];
    double ci[MDIM];
    int a, i, b, j;

    if (argc > 1) srandom(atoi(argv[1]));

    for (i = 0; i <  MDIM; i++)
        v[i] = cl[i] = random() / (double)0x7fffffff;

    qx(cl_invert)(ci, cl);
    
    mul(cl, ci, v);

    clFOR(a, i) {
        clFOR(b, j) {
            printf(" [%d,%d][%d,%d]  %15f %15f   %15f %15f\n",
                   a, i, b, j,
                   X(ci, a, i, b, j)[0],
                   X(ci, a, i, b, j)[1],
                   X(cl, a, i, b, j)[0],
                   X(cl, a, i, b, j)[1]);
        }
    }

    return 0;
}
