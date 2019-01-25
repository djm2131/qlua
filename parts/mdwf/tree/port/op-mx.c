#include <mdwf.h>

void
qx(op_pc2_Mx)(struct Fermion *r_x,
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
    qx(op_Kx)(t_x, state->lat_x, params, s_x,
                flops);
    qx(op_A1xBxFx)(t_y, state->lat_y, params, gauge, t_x, 
                flops, sent, received);
    qx(op_1mK1xA1xBxFx)(r_x, state->lat_x, params, gauge, s_x, t_y,
                flops, sent, received);
}
void
qx(op_pc2p_Mx)(struct Fermion *r_x,
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
    qx(op_A1xBxFx)(t_y, state->lat_y, params, gauge, s_x,
                flops, sent, received);
//  replace with fused function
//    qx(op_1mA1xBxFx)(r_x, state->lat_x, params, gauge, s_x, t_y,
//                flops, sent, received);
    qx(op_A1xBxFx)(t_x, state->lat_x, params, gauge, t_y,
                flops, sent, received);
    qx(omp_f_copy)(state, r_x, state->lat_x->full_size, state->Ls, s_x);
    state->flops += qx(omp_f_add2)(state, r_x, state->lat_x->full_size, state->Ls, -1., t_x);
// to here
}
void
qx(op_Mx)(struct Fermion *r_x,
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
        qx(op_pc2_Mx)(r_x, state, params, gauge, s_x, 
                flops, sent, received, t_x, t_y);
    else if (QOP_MDWF_EOPC2PRIME == state->eopc_type)
        qx(op_pc2p_Mx)(r_x, state, params, gauge, s_x, 
                flops, sent, received, t_x, t_y);
    else 
        q(set_error)(state, 1, "unknown eoprec");
}
