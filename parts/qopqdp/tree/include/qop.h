#ifndef _QOP_H
#define _QOP_H

#ifdef __cplusplus
extern "C" {
#endif

  // allow the user to specify QOP_Precision and/or QOP_PrecisionInt
  // default to single precision
#ifndef QOP_Precision
#  ifndef QOP_PrecisionInt
#    define QOP_PrecisionInt 1
#  endif
#  if QOP_PrecisionInt == 1
#    define QOP_Precision 'F'
#    define QOP_PrecisionLetter F
#  elif QOP_PrecisionInt == 2
#    define QOP_Precision 'D'
#    define QOP_PrecisionLetter D
#  else
#    error "bad QOP_PrecisionInt"
#  endif
#else
#  ifndef QOP_PrecisionInt
#    if QOP_Precision == 'F'
#      define QOP_PrecisionInt 1
#      define QOP_PrecisionLetter F
#    elif QOP_Precision == 'D'
#      define QOP_PrecisionInt 2
#      define QOP_PrecisionLetter D
#    else
#      error "bad QOP_Precision"
#    endif
#  else
#    if QOP_Precision == 'F'
#      if QOP_PrecisionInt != 1
#        error "inconsistent QOP_Precision='F' and QOP_PrecisionInt"
#      endif
#      define QOP_PrecisionLetter F
#    elif QOP_Precision == 'D'
#      if QOP_PrecisionInt != 2
#        error "inconsistent QOP_Precision='D' and QOP_PrecisionInt"
#      endif
#      define QOP_PrecisionLetter D
#    else
#      error "bad QOP_Precision"
#    endif
#  endif
#endif

  // allow the user to specify QOP_Colors and/or QOP_Nc
  // default to Nc=3
#ifndef QOP_Colors
#  ifndef QOP_Nc
#    define QOP_Nc 3
#  endif
#  if QOP_Nc == 1 || QOP_Nc == 2 || QOP_Nc == 3
#    define QOP_Colors QOP_Nc
#  elif QOP_Nc > 0
#    define QOP_Colors 'N'
#  else
#    error "bad QOP_Nc"
#  endif
#else
#  ifndef QOP_Nc
#    if QOP_Colors == 1 || QOP_Colors == 2 || QOP_Colors == 3
#      define QOP_Nc QOP_Colors
#    elif QOP_Colors == 'N'
#      define QOP_Nc 3
#    else
#      error "bad QOP_Colors"
#    endif
#  else
#    if QOP_Colors == 1 || QOP_Colors == 2 || QOP_Colors == 3
#      if QOP_Colors != QOP_Nc
#        error "inconsistent QOP_Colors and QOP_Nc"
#      endif
#    elif QOP_Colors == 'N'
#      if QOP_Nc <= 0
#        error "bad QOP_Nc"
#      endif
#    else
#      error "bad QOP_Colors"
#    endif
#  endif
#endif

#include <qop_int.h>

#if QOP_Precision == 'F'
#  include <qop_f.h>
#else
#  include <qop_d.h>
#endif

#if QOP_Colors == 1
#  include <qop_f1.h>
#  include <qop_d1.h>
#  include <qop_df1.h>
#elif QOP_Colors == 2
#  include <qop_f2.h>
#  include <qop_d2.h>
#  include <qop_df2.h>
#elif QOP_Colors == 3
#  include <qop_f3.h>
#  include <qop_d3.h>
#  include <qop_df3.h>
  //#  include <qop_mg.h>
#elif QOP_Colors == 'N'
#  include <qop_fn.h>
#  include <qop_dn.h>
#  include <qop_dfn.h>
#endif

#ifdef __cplusplus
}
#endif

#endif /* _QOP_H */
