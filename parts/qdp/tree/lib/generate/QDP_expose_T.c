#include "qdp_$lib_internal.h"

void *
QDP$PC_expose_$ABBR($QDPPCTYPE *dest)
{
  TGET;
  ONE {
    QDP_prepare_expose(&dest->dc);
    dest->dc.exposed++;
  }
  TBARRIER;
  return dest->data;
}

void
QDP$PC_reset_$ABBR($QDPPCTYPE *dest)
{
  TGET;
  ONE {
    if(!dest->dc.exposed) {
      fprintf(stderr,"error: trying to restore non-exposed data\n");
      QDP_abort(1);
    }
    dest->dc.exposed--;
  }
  TBARRIER;
}
