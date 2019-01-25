#include <clover.h>

void
qx(op_AmB_norm)(struct FermionX *r_x,
                double *local_norm,
                struct eo_lattice *xy,
                const struct SUn *U,
                const struct CloverX *C,
                const struct FermionX *a_x,
                const struct FermionX *a_y,
                long long *flops,
                long long *sent,
                long long *received)
{
    double n_body, n_face;

    qx(boundary)(xy, qx(up_project_n), qx(down_project_n), U, a_y, flops);

    if (xy->h_valid)
        QMP_start(xy->handle);

    *flops += qx(do_AmB_norm)(r_x, &n_body, 0, xy->body_size,
                              xy->neighbor, U, C, a_x, a_y, NULL);

    if (xy->h_valid)
        QMP_wait(xy->handle);

    *flops += qx(do_AmB_norm)(r_x, &n_face, xy->body_size, xy->face_size,
                              xy->neighbor, U, C, a_x, a_y, xy->receive_buf);
    *sent += xy->total_send;
    *received += xy->total_receive;
    *local_norm = n_body + n_face;
}

