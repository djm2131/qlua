#ifndef MARK_2DCAC914_635D_4D58_AA60_DC75CD13961F
#define MARK_2DCAC914_635D_4D58_AA60_DC75CD13961F

#ifndef QDP_Nc
#define QDP_Nc 'Z' /* Try to confuse QDP & QLA to warn about undefined Nc */
#endif
#define QDP_Precision 'D' /* DO NOT CHANGE THESE DEFAULTS */
#define QDP_Ns 4 /* fermion dimension, seems undefined in QDP */
#include <stdarg.h>
#include <stdio.h>
#include <math.h> // for M_PI (if there)

#define lua_c
#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "qdp.h"
#include "qla_types.h"
#include "qla.h"
#undef QDP_Nc

#define QDP_Nc 'Z'
#if USE_Nc2
#include "qdp_d2.h"
#include "qla_d2.h"
#endif
#if USE_Nc3
#include "qdp_d3.h"
#include "qla_d3.h"
#endif
#if USE_NcN
#include "qdp_dn.h"
#include "qla_dn.h"
#endif

#ifndef M_PI
#define M_PI 3.141592653589793238462643383279502884197
#endif /* !defined(M_PI) */

typedef enum {
    /* start with all type that have any of arithmetic operations defined */
    qReal,                    /*  0 */
    qComplex,                 /*  1 */
    qGamma,                   /*  2 */
    qMatReal,                 /*  3 */
    qMatComplex,              /*  4 */
    qVecReal,                 /*  5 */
    qVecComplex,              /*  6 */
    qSeqColVec2,              /*  7 */
    qSeqColVec3,              /*  8 */
    qSeqColVecN,              /*  9 */
    qSeqColMat2,              /* 10 */
    qSeqColMat3,              /* 11 */
    qSeqColMatN,              /* 12 */
    qSeqDirFerm2,             /* 13 */
    qSeqDirFerm3,             /* 14 */
    qSeqDirFermN,             /* 15 */
    qSeqDirProp2,             /* 16 */
    qSeqDirProp3,             /* 17 */
    qSeqDirPropN,             /* 18 */
    qLatInt,                  /* 19 */
    qLatReal,                 /* 20 */
    qLatComplex,              /* 21 */
    qLatColVec2,              /* 22 */
    qLatColVec3,              /* 23 */
    qLatColVecN,              /* 24 */
    qLatColMat2,              /* 25 */
    qLatColMat3,              /* 26 */
    qLatColMatN,              /* 27 */
    qLatDirFerm2,             /* 28 */
    qLatDirFerm3,             /* 29 */
    qLatDirFermN,             /* 30 */
    qLatDirProp2,             /* 31 */
    qLatDirProp3,             /* 32 */
    qLatDirPropN,             /* 33 */
    qOther,                   /* 34  no operations for this type */
    qArithTypeCount,          /* 35  number of types in arith dispatch tables */
    qLattice,                 /* 36 */
    qLatMulti,                /* 37 */
    qLatSubset,               /* 38 */
    qString,                  /* 39 */
    qTable,                   /* 40 */
    qVecInt,                  /* 41 */
    qSeqRandom,               /* 42 */
    qLatRandom,               /* 43 */
    qScatter,                 /* 44 */
    qGather,                  /* 45 */
    qReader,                  /* 46 */
    qWriter,                  /* 47 */
    qAffReader,               /* 48 */
    qAffWriter,               /* 49 */
    qHdf5Reader,              /* 50 */
    qHdf5Writer,              /* 51 */
    qClover,                  /* 52 */
    qCloverDeflator,          /* 53 */
    qCloverDeflatorState,     /* 54 */
    qTwisted,                 /* 55 */
    qTwistedDeflator,         /* 56 */
    qTwistedDeflatorState,    /* 57 */
    qMDWF,                    /* 58 */
    qMDWFDeflator,            /* 59 */
    qMDWFDeflatorState,       /* 60 */
    qQOPwmgState,             /* 61 */
    /* types for stack parsing */
    qOptInt,                  /* 62 */
    qOptBool,                 /* 63 */
    qOptArray,                /* 64 */
    qOptStruct,                /* 65 */
    qOptNone,                 /* 66 */
#ifdef HAS_QUDA
    qQudaGaugeParam,
    qQudaInvertParam,
    qQudaEigParam,
#endif /* defined(HAVE_QUDA) */
    qLatmat,
    /* ZZZ add types for other packages here */
    qNoType                   /*  */
} QLUA_Type;

typedef enum { /* simple arithmetic types */
    zReal,
    zLatInt,
    zLatReal,
    zNoType,
    zArithTypeCount, /* number of types in simple arith dispatch tables */
} QLUA_Ztype;

extern const char *progname;
extern const char *qcdlib;
extern const char *a_type_key;
extern int qlua_master_node;

double qlua_timeofday(void);
double qlua_nodetime(void);

void qlua_fillmeta(lua_State *L,
                   const luaL_Reg *table,
                   QLUA_Type t_id);
void qlua_metatable(lua_State *L,
                    const char *name,
                    const luaL_Reg *table,
                    QLUA_Type t_id);
void qlua_selftable(lua_State *L,
                    const luaL_Reg *table,
                    QLUA_Type t_id);
void qlua_latticetable(lua_State *L,
                       const luaL_Reg *table,
                       QLUA_Type t_id,
                       int Sidx);
int qlua_getlattice(lua_State *L, int idx); /* => [idx]'s lattice object */
int qlua_lookup(lua_State *L, int idx, const char *table);
int qlua_selflookup(lua_State *L, int idx, const char *key);
void *qlua_checkLatticeType(lua_State *L,
                            int idx,
                            QLUA_Type t_id,
                            const char *name);
void qlua_createLatticeTable(lua_State *L,
                             int Sidx,
                             const struct luaL_Reg *ft,
                             QLUA_Type t_id,
                             const char *name);
QLUA_Type qlua_qtype(lua_State *L, int idx);
QLUA_Ztype qlua_ztype(lua_State *L, int idx);
QLUA_Type qlua_atype(lua_State *L, int idx); /* non-arith types => qOther */
const char *qlua_ptype(lua_State *L, int idx);
void *qlua_malloc(lua_State *L, int size);
void qlua_qdp_memuse(lua_State *L, const char *name, int count);
void qlua_free(lua_State *L, void *ptr);

int qlua_index(lua_State *L, int idx, const char *name, int mv);  /* k or -1 */
int qlua_checkindex(lua_State *L, int idx, const char *name, int mv);
void qlua_checkindex2(lua_State *L, int idx, const char *nm, int *i0, int *i1);

int qlua_diracindex(lua_State *L, int idx);               /* d = 0 .. Nf - 1 */
int qlua_checkdiracindex(lua_State *L, int idx);
int qlua_colorindex(lua_State *L, int idx, int nc);       /* c = 0 .. Nc - 1 */
int qlua_checkcolorindex(lua_State *L, int idx, int nc);
int qlua_checkleftindex(lua_State *L, int idx, int nc);   /* a = 0 .. Nc - 1 */
int qlua_leftindex(lua_State *L, int idx, int nc);
int qlua_checkrightindex(lua_State *L, int idx, int nc);  /* b = 0 .. Nc - 1 */
int qlua_rightindex(lua_State *L, int idx, int nc);
int qlua_gammaindex(lua_State *L, int idx);                /* mu = 0..3 or 5 */
int qlua_checkgammaindex(lua_State *L, int idx);
int qlua_gammabinary(lua_State *L, int idx);                  /* n = 0 .. 15 */
int qlua_checkgammabinary(lua_State *L, int idx);

void qlua_get_complex_vector(lua_State *L, int idx, int dim, QLA_D_Complex cv[], const char *msg);

const char *qlua_checkstring(lua_State *L, int idx, const char *fmt, ...);
int qlua_checkint(lua_State *L, int idx, const char *fmt, ...);
double qlua_checknumber(lua_State *L, int idx, const char *fmt, ...);
void qlua_checktable(lua_State *L, int idx, const char *fmt, ...);
int *qlua_intarray(lua_State *L, int idx, int *out_dim);
double *qlua_numberarray(lua_State *L, int idx, int *out_dim);
int *qlua_checkintarray(lua_State *L, int idx, int dim, int *out_dim);
double *qlua_checknumberarray(lua_State *L, int idx, int dim, int *out_dim);
int qlua_checkcomplexarray(lua_State *L, int idx, int dim, QLA_D_Complex *a);

int qlua_checkopt_paramtable(lua_State *L, int idx);
int qlua_checkopt_table(lua_State *L, int idx);
int qlua_tabpushopt_key(lua_State *L, int idx, const char *key);
int qlua_tabpushopt_idx(lua_State *L, int idx, int subindex);

int qlua_push_key_bool(lua_State *L, int idx, const char *key, int val);
int qlua_push_key_number(lua_State *L, int idx, const char *key, double val);
int qlua_push_key_string(lua_State *L, int idx, const char *key, const char *val);
int qlua_push_key_object(lua_State *L, int idx, const char *key);

int qlua_tabkey(lua_State *L, int idx, const char *key);
int qlua_tabkey_present(lua_State *L, int idx, const char *key);
int qlua_tabkey_bool(lua_State *L, int idx, const char *key);
int qlua_tabkey_boolopt(lua_State *L, int idx, const char *key, int def);
int qlua_tabkey_int(lua_State *L, int idx, const char *key);
int qlua_tabkey_intopt(lua_State *L, int idx, const char *key, int def);
int qlua_tabidx_int(lua_State *L, int idx, int subidx);
double qlua_tabkey_double(lua_State *L, int idx, const char *key);
double qlua_tabkey_doubleopt(lua_State *L, int idx, const char *key, double def);
double qlua_tabidx_double(lua_State *L, int idx, int subidx);
void qlua_tabkey_complex(lua_State *L, int idx, const char *key, double *rptr, double *iptr, const char *msg);
void qlua_tabidx_complex(lua_State *L, int idx, int subidx, double *rptr, double *iptr, const char *msg);
const char *qlua_tabkey_string(lua_State *L, int idx, const char *key);
const char *qlua_tabidx_string(lua_State *L, int idx, int subidx);
const char *qlua_tabkey_stringopt(lua_State *L, int idx, const char *key, const char *def);
int qlua_tabidx_tableopt(lua_State *L, int idx, int subidx);
int qlua_tabkey_tableopt(lua_State *L, int idx, const char *key);

typedef int (*q_op)(lua_State *L);

typedef struct {
    q_op     *table;
    QLUA_Type ta;
    QLUA_Type tb;
    q_op      op;
} QLUA_Op2;

typedef struct {
    q_op      *table;
    QLUA_Ztype ta;
    QLUA_Ztype tb;
    q_op       op;
} QLUA_ZOp2;

extern q_op qlua_add_table[];
extern q_op qlua_sub_table[];
extern q_op qlua_mul_table[];
extern q_op qlua_div_table[];
extern q_op qlua_mod_table[];
extern q_op qlua_min_table[];
extern q_op qlua_max_table[];
extern q_op qlua_eq_table[];
extern q_op qlua_ne_table[];
extern q_op qlua_lt_table[];
extern q_op qlua_le_table[];
extern q_op qlua_gt_table[];
extern q_op qlua_ge_table[];

void qlua_reg_op2(const QLUA_Op2 *ops);
void qlua_reg_zop2(const QLUA_ZOp2 *ops);

int qlua_add(lua_State *L);
int qlua_sub(lua_State *L);
int qlua_mul(lua_State *L);
int qlua_div(lua_State *L);
int qlua_mod(lua_State *L);

void qlua_reg_dot(QLUA_Type ta, q_op op);

int qlua_badconstr(lua_State *L, const char *name);
int qlua_badindex(lua_State *L, const char *type);

void qlua_send_string(lua_State *L, int idx);
int qlua_receive_string(lua_State *L, char **str);

/* generic error reporter for __newindex in metatables */
int qlua_nowrite(lua_State *L);

/* missing piece of QMP: distribute values from one node to others */
void XMP_dist_int_array(int src_node, int count, int *value);
void XMP_dist_double_array(int src_node, int count, double *value);

#define QLUA_ASSERT(x) do qlua_assert(x, #x); while (0)
#define QLUA_ABORT(msg) do qlua_assert(0, msg); while (0)
void qlua_assert(int, const char *);

char *qlua_strdup(lua_State *L, const char *str);

/* qlua cleanup */
void qlua_fini(void);

/* strict memory management: collect garbage befor any QDP operation */
#define CALL_QDP(L) do lua_gc(L, LUA_GCCOLLECT, 0); while (0)

#endif /* !defined(MARK_2DCAC914_635D_4D58_AA60_DC75CD13961F) */
