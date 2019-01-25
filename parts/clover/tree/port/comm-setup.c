#include <clover.h>

static int
eo_comm(struct eo_lattice *eo, struct Q(State) *state, int real_size)
{
    int d, k;
    int pfL = Q(COLORS) * Q(PROJECTED_FERMION_DIM) * 2 * real_size;
    int size;

    if (eo->real_size == real_size)
        return 0;

    /* This version bundles all communication together. Seems to work, but may
     * be suboptimal.
     */
  
    eo->total_send = 0;
    eo->total_receive = 0;
    for (d = 0, k = 0; d < Q(DIM); d++) {
        if (state->network[d] == 1) {
            continue;
        } else if (state->network[d] == 2) {
            int up_size;
            int down_size;

            up_size = pfL * eo->send_up_size[d];
            down_size = pfL * eo->send_down_size[d];
            size = up_size + down_size;
            eo->mem[k] = QMP_allocate_aligned_memory(size,
                                                     CACHE_LINE_SIZE,
                                                     QMP_MEM_DEFAULT);
            if (eo->mem[k] == NULL)
                return 1;
            eo->mem_count = k + 1;
            eo->send_up_buf[d] = QMP_get_memory_pointer(eo->mem[k]);
            eo->send_down_buf[d] = (char *)eo->send_up_buf[d] + up_size;
            eo->mh[k] = QMP_declare_msgmem(eo->send_up_buf[d], size);
            if (eo->mh[k] == NULL)
                return 1;
            eo->mh_count = k + 1;
            eo->th[k] = QMP_declare_send_to(eo->mh[k],            /* [d] +1 */
                                            state->neighbor_up[d],
                                            0);
            if (eo->th[k] == NULL)
                return 1;
            eo->th_count = k + 1;
            eo->total_send += size;
            k++;

            up_size = pfL * eo->receive_up_size[d];
            down_size = pfL * eo->receive_down_size[d];
            size = up_size + down_size;
            eo->mem[k] = QMP_allocate_aligned_memory(size,
                                                     CACHE_LINE_SIZE,
                                                     QMP_MEM_DEFAULT);
            if (eo->mem[k] == NULL)
                return 1;
            eo->mem_count = k + 1;
            eo->receive_buf[d + Q(DIM)] = QMP_get_memory_pointer(eo->mem[k]);
            eo->receive_buf[d] = (char *)eo->receive_buf[d + Q(DIM)] + down_size;
            eo->mh[k] = QMP_declare_msgmem(eo->receive_buf[d + Q(DIM)], size);
            if (eo->mh[k] == NULL)
                return 1;
            eo->mh_count = k + 1;
            eo->th[k] = QMP_declare_receive_from(eo->mh[k],      /* [d] -1 */
                                                 state->neighbor_down[d],
                                                 0);
            if (eo->th[k] == NULL)
                return 1;
            eo->th_count = k + 1;
            eo->total_receive += size;
            k++;
        } else { /* state->network[d] > 2 */
            size = eo->send_up_size[d] * pfL;
            eo->mem[k] = QMP_allocate_aligned_memory(size,
                                                     CACHE_LINE_SIZE,
                                                     QMP_MEM_DEFAULT);
            if (eo->mem[k] == NULL)
                return 1;
            eo->mem_count = k + 1;
            eo->send_up_buf[d] = QMP_get_memory_pointer(eo->mem[k]);
            eo->mh[k] = QMP_declare_msgmem(eo->send_up_buf[d], size);
            if (eo->mh[k] == NULL)
                return 1;
            eo->mh_count = k + 1;
            eo->th[k] = QMP_declare_send_to(eo->mh[k],           /* [d] +1 */
                                            state->neighbor_up[d],
                                            0);
            if (eo->th[k] == NULL)
                return 1;
            eo->th_count = k + 1;
            eo->total_send += size;
            k++;

            size = eo->send_down_size[d] * pfL;
            eo->mem[k] = QMP_allocate_aligned_memory(size,
                                                     CACHE_LINE_SIZE,
                                                     QMP_MEM_DEFAULT);
            if (eo->mem[k] == NULL)
                return 1;
            eo->mem_count = k + 1;
            eo->send_down_buf[d] = QMP_get_memory_pointer(eo->mem[k]);
            eo->mh[k] = QMP_declare_msgmem(eo->send_down_buf[d], size);
            if (eo->mh[k] == NULL)
                return 1;
            eo->mh_count = k + 1;
            eo->th[k] = QMP_declare_send_to(eo->mh[k], /* [d] -1 */
                                            state->neighbor_down[d],
                                            0);
            if (eo->th[k] == NULL)
                return 1;
            eo->th_count = k + 1;
            eo->total_send += size;
            k++;

            size = eo->receive_up_size[d] * pfL;
            eo->mem[k] = QMP_allocate_aligned_memory(size,
                                                     CACHE_LINE_SIZE,
                                                     QMP_MEM_DEFAULT);
            if (eo->mem[k] == NULL)
                return 1;
            eo->mem_count = k + 1;
            eo->receive_buf[d] = QMP_get_memory_pointer(eo->mem[k]);
            eo->mh[k] = QMP_declare_msgmem(eo->receive_buf[d], size);
            if (eo->mh[k] == NULL)
                return 1;
            eo->mh_count = k + 1;
            eo->th[k] = QMP_declare_receive_from(eo->mh[k],        /* [d] +1 */
                                                 state->neighbor_up[d],
                                                 0);
            if (eo->th[k] == NULL)
                return 1;
            eo->th_count = k + 1;
            eo->total_receive += size;
            k++;

            size = eo->receive_down_size[d] * pfL;
            eo->mem[k] = QMP_allocate_aligned_memory(size,
                                                     CACHE_LINE_SIZE,
                                                     QMP_MEM_DEFAULT);
            if (eo->mem[k] == NULL)
                return 1;
            eo->mem_count = k + 1;
            eo->receive_buf[d+Q(DIM)] = QMP_get_memory_pointer(eo->mem[k]);
            eo->mh[k] = QMP_declare_msgmem(eo->receive_buf[d+Q(DIM)], size);
            if (eo->mh[k] == NULL)
                return 1;
            eo->mh_count = k + 1;
            eo->th[k] = QMP_declare_receive_from(eo->mh[k],       /* [d] -1 */
                                                 state->neighbor_down[d],
                                                 0);
            if (eo->th[k] == NULL)
                return 1;
            eo->th_count = k + 1;
            eo->total_receive += size;
            k++;
        }
    }
    if (eo->th_count > 0) {
        eo->handle = QMP_declare_multiple(eo->th, eo->th_count);
        if (eo->handle == NULL)
            return 1;
        eo->th_count = 0;
        eo->h_valid = 1;
    }
    eo->real_size = real_size;
    return 0;
}


int
q(setup_comm)(struct Q(State) *state, int real_size)
{
  if (state->real_size == real_size)
    return 0;

  q(free_comm)(state);
  state->real_size = real_size;
  if (eo_comm(&state->even, state, real_size))
    return q(free_comm)(state);
  if (eo_comm(&state->odd, state, real_size))
    return q(free_comm)(state);

  return 0;
}
