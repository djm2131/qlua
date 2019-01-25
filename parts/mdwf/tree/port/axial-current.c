#include <mdwf.h>

/* NOTE: This is probably true only for Shamir case */

int
QX(axial_current)(void                      (*writer)(const int pos[Q(DIM)],
                                                      int dir,
                                                      double value,
                                                      void *env),
                  void                       *env,
                  const struct QX(Fermion)   *fermion,
                  const struct QX(Gauge)     *gauge)
{
    DECLARE_STATE;
    long long flops = 0;
    long long sent = 0;
    long long received = 0;
    
    CHECK_ARG0(fermion);
    CHECK_ARGn(gauge, "axial_current");
    
    if (q(setup_comm)(state, sizeof (REAL)))
      return q(set_error)(state, 0,
                          "axial_current(): communication setup failed");
    
    BEGIN_TIMING(state);
    qx(op_axial_current)(writer, env,
                         state->lat_x, state->lat_y, gauge->data,
                         fermion->cb_x, fermion->cb_y,
                         &flops, &sent, &received, state->node);
    qx(op_axial_current)(writer, env,
                         state->lat_y, state->lat_x, gauge->data,
                         fermion->cb_y, fermion->cb_x,
                         &flops, &sent, &received, state->node);
    END_TIMING(state, flops, sent, received);

    return 0;
}    
