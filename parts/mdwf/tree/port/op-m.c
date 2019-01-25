#include <mdwf.h>

void
qx(op_pc2_M)(struct Fermion *r_x,
              struct Q(State) *state,
              const struct Q(Parameters) *params,
              const struct SUn *gauge,
              const struct Fermion *s_x,
              long long *flops,
              long long *sent,
              long long *received,
              struct Fermion *t_x,
              struct Fermion *t_y)
{
    qx(op_BA1K1)(t_x, state->lat_x, params, s_x,        /* reuse r_e; need t_e? */
                flops);
    qx(op_BA1F)(t_y, state->lat_y, params, gauge, t_x,
                flops, sent, received);
    qx(op_1mKF)(r_x, state->lat_x, params, gauge, s_x, t_y, 
                flops, sent, received);
}
void
qx(op_pc2p_M)(struct Fermion *r_x,
              struct Q(State) *state,
              const struct Q(Parameters) *params,
              const struct SUn *gauge,
              const struct Fermion *s_x,
              long long *flops,
              long long *sent,
              long long *received,
              struct Fermion *t_x,
              struct Fermion *t_y)
{
    qx(op_BA1)(t_x, state->lat_x, params, s_x,        /* reuse r_e; need t_e? */
                flops);
    qx(op_BA1F)(t_y, state->lat_y, params, gauge, t_x,
                flops, sent, received);
    qx(op_1mF)(r_x, state->lat_x, gauge, s_x, t_y,
                flops, sent, received);
}
void
qx(op_M)(struct Fermion *r_x,
         struct Q(State) *state,
         const struct Q(Parameters) *params,
         const struct SUn *gauge,
         const struct Fermion *s_x,
         long long *flops,
         long long *sent,
         long long *received,
         struct Fermion *t_x,
         struct Fermion *t_y)
{
    if (QOP_MDWF_EOPC2 == state->eopc_type)
        qx(op_pc2_M)(r_x, state, params, gauge, s_x, 
                flops, sent, received, t_x, t_y);
    else if (QOP_MDWF_EOPC2PRIME == state->eopc_type)
        qx(op_pc2p_M)(r_x, state, params, gauge, s_x, 
                flops, sent, received, t_x, t_y);
    else 
        q(set_error)(state, 1, "unknown eoprec");
}
