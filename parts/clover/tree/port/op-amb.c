#include <clover.h>

void
qx(op_AmB)(struct FermionX *r_x,
           struct eo_lattice *xy,
           const struct SUn *U,
           const struct CloverX *C,
           const struct FermionX *a_x,
           const struct FermionX *a_y,
           long long *flops,
           long long *sent,
           long long *received)
{
    qx(boundary)(xy, qx(up_project_n), qx(down_project_n), U, a_y, flops);

    if (xy->h_valid)
        QMP_start(xy->handle);

    *flops += qx(do_AmB)(r_x, 0, xy->body_size,
                         xy->neighbor, U, C, a_x, a_y, NULL);

    if (xy->h_valid)
        QMP_wait(xy->handle);

    *flops += qx(do_AmB)(r_x, xy->body_size, xy->face_size,
                         xy->neighbor, U, C, a_x, a_y, xy->receive_buf);
    *sent += xy->total_send;
    *received += xy->total_receive;
}

