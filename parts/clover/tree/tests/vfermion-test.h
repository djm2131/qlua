/* propotypes for vfermion test harness */

void show_fermion(const char *name, struct Fermion *f, int size);
void show_vfermion(const char *n, struct vFermion *v, int s, int w,
                   struct Fermion *t);
void mk_fermion(struct Fermion *f, int size, int *data, int stride);
struct Fermion *new_fermion(int size);
struct vFermion *new_vfermion(int size, int width);

void construct_f(int esize, struct Fermion *f, double m);
void construct_vf(int esize, int width, struct vFermion *dst,
                  struct Fermion *t0, double m);

void construct_d(int len, double *d, double m);
