#include <clover.h>

void
qx(op_AxpBx)(struct FermionX *r_x,
             struct eo_lattice *xy,
             const struct SUn *U,
             const struct CloverX *C,
             const struct FermionX *s_x,
             const struct FermionX *s_y,
             long long *flops,
             long long *sent,
             long long *received)
{
    qx(boundary)(xy, qx(up_project_x), qx(down_project_x), U, s_y, flops);
    
    if (xy->h_valid)
        QMP_start(xy->handle);

    *flops += qx(do_AxpBx)(r_x, 0, xy->body_size,
                           xy->neighbor, U, C, s_x, s_y, NULL);

    if (xy->h_valid)
        QMP_wait(xy->handle);

    *flops += qx(do_AxpBx)(r_x, xy->body_size, xy->face_size,
                           xy->neighbor, U, C, s_x, s_y, xy->receive_buf);
    *sent += xy->total_send;
    *received += xy->total_receive;
}
