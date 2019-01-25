#ifndef SAMPLE_COMMON_H
#define SAMPLE_COMMON_H
extern double U_scale;
extern int lattice[5];
extern int network[4];
extern int this_node[4];
extern int neighbor_up[4];
extern int neighbor_down[4];
extern int primary_p;

extern double M;
extern double m_5;
extern double kappa;

extern int max_iterations;
extern double min_epsilon;
extern unsigned U_seed;
extern unsigned rhs_seed;
extern unsigned sol_seed;
extern unsigned options;

void read_gauge(double *v_re, double *v_im, int d, const int p[4], int a, int b, void *env);
void read_fermion(double *v_re, double *v_im, const int p[5], int c, int d, void *env);
void write_fermion(const int p[5], int c, int d, double v_re, double v_im, void *e);
int init_qmp(int argc, char *argv[], const char *name, char prec,
	     int *count, double **shift);
void fini_qmp(void);
void zprint(const char *who, const char *fmt, ...);
void get_sublattice(int lo[4], int hi[4], int node, void *env);
void report_performance(struct QOP_MDWF_State *state, char *name);
void report_time(struct QOP_MDWF_State *state, char *name);

#endif
