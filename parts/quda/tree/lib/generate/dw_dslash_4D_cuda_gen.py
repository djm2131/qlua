# -*- coding: utf-8 -*-
import sys

### complex numbers ########################################################################

def complexify(a):
    return [complex(x) for x in a]

def complexToStr(c):
    def fltToString(a):
        if a == int(a): return `int(a)`
        else: return `a`
    
    def imToString(a):
        if a == 0: return "0i"
        elif a == -1: return "-i"
        elif a == 1: return "i"
        else: return fltToString(a)+"i"
    
    re = c.real
    im = c.imag
    if re == 0 and im == 0: return "0"
    elif re == 0: return imToString(im)
    elif im == 0: return fltToString(re)
    else:
        im_str = "-"+imToString(-im) if im < 0 else "+"+imToString(im)
        return fltToString(re)+im_str


### projector matrices ########################################################################

id = complexify([
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1
])

gamma1 = complexify([
    0, 0, 0, 1j,
    0, 0, 1j, 0,
    0, -1j, 0, 0,
    -1j, 0, 0, 0
])

gamma2 = complexify([
    0, 0, 0, 1,
    0, 0, -1, 0,
    0, -1, 0, 0,
    1, 0, 0, 0
])

gamma3 = complexify([
    0, 0, 1j, 0,
    0, 0, 0, -1j,
    -1j, 0, 0, 0,
    0, 1j, 0, 0
])

gamma4 = complexify([
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, -1, 0,
    0, 0, 0, -1
])

igamma5 = complexify([
    0, 0, 1j, 0,
    0, 0, 0, 1j,
    1j, 0, 0, 0,
    0, 1j, 0, 0
])

two_P_L = [ id[x] - igamma5[x]/1j for x in range(0,4*4) ]
two_P_R = [ id[x] + igamma5[x]/1j for x in range(0,4*4) ]

# for s1 in range(0,4) :
# for s2 in range (0,4): print "%8s" % two_P_L[s1*4+s2],
# print " ",
# for s2 in range (0,4): print "%8s" % two_P_R[s1*4+s2],
# print ""


def gplus(g1, g2):
    return [x+y for (x,y) in zip(g1,g2)]

def gminus(g1, g2):
    return [x-y for (x,y) in zip(g1,g2)]

def projectorToStr(p):
    out = ""
    for i in range(0, 4):
        for j in range(0,4):
            out += '%3s' % complexToStr(p[4*i+j])
        out += "\n"
    return out

projectors = [
    gminus(id,gamma1), gplus(id,gamma1),
    gminus(id,gamma2), gplus(id,gamma2),
    gminus(id,gamma3), gplus(id,gamma3),
    gminus(id,gamma4), gplus(id,gamma4),
]

### code generation ########################################################################

def indent(code, n=1):
    def indentline(line): return (n*" "+line if ( line and line.count("#", 0, 1) == 0) else line)
    return ''.join([indentline(line)+"\n" for line in code.splitlines()])

def block(code):
    return "{\n"+indent(code)+"}"

def sign(x):
    if x==1: return "+"
    elif x==-1: return "-"
    elif x==+2: return "+2*"
    elif x==-2: return "-2*"

def nthFloat4(n):
    return `(n/4)` + "." + ["x", "y", "z", "w"][n%4]

def nthFloat2(n):
    return `(n/2)` + "." + ["x", "y"][n%2]


def in_re(s, c): return "i"+`s`+`c`+"_re"
def in_im(s, c): return "i"+`s`+`c`+"_im"
def g_re(d, m, n): return ("g" if (d%2==0) else "gT")+`m`+`n`+"_re"
def g_im(d, m, n): return ("g" if (d%2==0) else "gT")+`m`+`n`+"_im"
def out_re(s, c): return "o"+`s`+`c`+"_re"
def out_im(s, c): return "o"+`s`+`c`+"_im"
def h1_re(h, c): return ["a","b"][h]+`c`+"_re"
def h1_im(h, c): return ["a","b"][h]+`c`+"_im"
def h2_re(h, c): return ["A","B"][h]+`c`+"_re"
def h2_im(h, c): return ["A","B"][h]+`c`+"_im"
def c_re(b, sm, cm, sn, cn): return "c"+`(sm+2*b)`+`cm`+"_"+`(sn+2*b)`+`cn`+"_re"
def c_im(b, sm, cm, sn, cn): return "c"+`(sm+2*b)`+`cm`+"_"+`(sn+2*b)`+`cn`+"_im"
def a_re(b, s, c): return "a"+`(s+2*b)`+`c`+"_re"
def a_im(b, s, c): return "a"+`(s+2*b)`+`c`+"_im"

def tmp_re(s, c): return "tmp"+`s`+`c`+"_re"
def tmp_im(s, c): return "tmp"+`s`+`c`+"_im"


def def_input_spinor():
    str = ""
    str += "// input spinor\n"
    str += "#ifdef SPINOR_DOUBLE\n"
    str += "#define spinorFloat double\n"
    for s in range(0,4):
        for c in range(0,3):
            i = 3*s+c
            str += "#define "+in_re(s,c)+" I"+nthFloat2(2*i+0)+"\n"
            str += "#define "+in_im(s,c)+" I"+nthFloat2(2*i+1)+"\n"
    str += "#define m5 m5_d\n"
    str += "#define mdwf_b5 mdwf_b5_d\n"
    str += "#define mdwf_c5 mdwf_c5_d\n"
    str += "#else\n"
    str += "#define spinorFloat float\n"
    for s in range(0,4):
        for c in range(0,3):
            i = 3*s+c
            str += "#define "+in_re(s,c)+" I"+nthFloat4(2*i+0)+"\n"
            str += "#define "+in_im(s,c)+" I"+nthFloat4(2*i+1)+"\n"
    str += "#define m5 m5_f\n"
    str += "#define mdwf_b5 mdwf_b5_f\n"
    str += "#define mdwf_c5 mdwf_c5_f\n"
    str += "#endif // SPINOR_DOUBLE\n\n"
    return str
# end def def_input_spinor


def def_gauge():
    str = "// gauge link\n"
    str += "#ifdef GAUGE_FLOAT2\n"
    for m in range(0,3):
        for n in range(0,3):
            i = 3*m+n
            str += "#define "+g_re(0,m,n)+" G"+nthFloat2(2*i+0)+"\n"
            str += "#define "+g_im(0,m,n)+" G"+nthFloat2(2*i+1)+"\n"

    str += "\n"
    str += "#else\n"
    for m in range(0,3):
        for n in range(0,3):
            i = 3*m+n
            str += "#define "+g_re(0,m,n)+" G"+nthFloat4(2*i+0)+"\n"
            str += "#define "+g_im(0,m,n)+" G"+nthFloat4(2*i+1)+"\n"

    str += "\n"
    str += "#endif // GAUGE_DOUBLE\n\n"
            
    str += "// conjugated gauge link\n"
    for m in range(0,3):
        for n in range(0,3):
            i = 3*m+n
            str += "#define "+g_re(1,m,n)+" (+"+g_re(0,n,m)+")\n"
            str += "#define "+g_im(1,m,n)+" (-"+g_im(0,n,m)+")\n"
    str += "\n"

    return str
# end def def_gauge


def def_clover():
    str = "// first chiral block of inverted clover term\n"
    str += "#ifdef CLOVER_DOUBLE\n"
    i = 0
    for m in range(0,6):
        s = m/3
        c = m%3
        str += "#define "+c_re(0,s,c,s,c)+" C"+nthFloat2(i)+"\n"
        i += 1
    for n in range(0,6):
        sn = n/3
        cn = n%3
        for m in range(n+1,6):
            sm = m/3
            cm = m%3
            str += "#define "+c_re(0,sm,cm,sn,cn)+" C"+nthFloat2(i)+"\n"
            str += "#define "+c_im(0,sm,cm,sn,cn)+" C"+nthFloat2(i+1)+"\n"
            i += 2
    str += "#else\n"
    i = 0
    for m in range(0,6):
        s = m/3
        c = m%3
        str += "#define "+c_re(0,s,c,s,c)+" C"+nthFloat4(i)+"\n"
        i += 1
    for n in range(0,6):
        sn = n/3
        cn = n%3
        for m in range(n+1,6):
            sm = m/3
            cm = m%3
            str += "#define "+c_re(0,sm,cm,sn,cn)+" C"+nthFloat4(i)+"\n"
            str += "#define "+c_im(0,sm,cm,sn,cn)+" C"+nthFloat4(i+1)+"\n"
            i += 2
    str += "#endif // CLOVER_DOUBLE\n\n"

    for n in range(0,6):
        sn = n/3
        cn = n%3
        for m in range(0,n):
            sm = m/3
            cm = m%3
            str += "#define "+c_re(0,sm,cm,sn,cn)+" (+"+c_re(0,sn,cn,sm,cm)+")\n"
            str += "#define "+c_im(0,sm,cm,sn,cn)+" (-"+c_im(0,sn,cn,sm,cm)+")\n"
    str += "\n"

    str += "// second chiral block of inverted clover term (reuses C0,...,C9)\n"
    for n in range(0,6):
        sn = n/3
        cn = n%3
        for m in range(0,6):
            sm = m/3
            cm = m%3
            str += "#define "+c_re(1,sm,cm,sn,cn)+" "+c_re(0,sm,cm,sn,cn)+"\n"
            if m != n: str += "#define "+c_im(1,sm,cm,sn,cn)+" "+c_im(0,sm,cm,sn,cn)+"\n"
    str += "\n"

    return str
# end def def_clover

def def_output_spinor():
    str = "// output spinor\n"
    for s in range(0,4):
        for c in range(0,3):
            i = 3*s+c
            if 2*i < sharedFloats:
                str += "#define "+out_re(s,c)+" s["+`(2*i+0)`+"*SHARED_STRIDE]\n"
            else:
                str += "VOLATILE spinorFloat "+out_re(s,c)+";\n"
            if 2*i+1 < sharedFloats:
                str += "#define "+out_im(s,c)+" s["+`(2*i+1)`+"*SHARED_STRIDE]\n"
            else:
                str += "VOLATILE spinorFloat "+out_im(s,c)+";\n"
    return str
# end def def_output_spinor


def prolog():
    if dslash:
        prolog_str= ("// *** CUDA DSLASH ***\n\n" if not dagger else "// *** CUDA DSLASH DAGGER ***\n\n")
        prolog_str+= "#define DSLASH_SHARED_FLOATS_PER_THREAD "+str(sharedFloats)+"\n\n"
    elif clover:
        prolog_str= ("// *** CUDA CLOVER ***\n\n")
        prolog_str+= "#define CLOVER_SHARED_FLOATS_PER_THREAD "+str(sharedFloats)+"\n\n"
    else:
        print "Undefined prolog"
        exit

    if domain_wall: prolog_str += "// NB! Don't trust any MULTI_GPU code\n"

    prolog_str+= (
"""
#if (CUDA_VERSION >= 4010)
#define VOLATILE
#else
#define VOLATILE volatile
#endif
""")

    prolog_str+= def_input_spinor()
    if dslash == True: 
        if dslash4 == True:
            prolog_str+= def_gauge()
    if clover == True: prolog_str+= def_clover()
    prolog_str+= def_output_spinor()

    prolog_str+= (
"""
#ifdef SPINOR_DOUBLE
#if (__COMPUTE_CAPABILITY__ >= 200)
#define SHARED_STRIDE 16 // to avoid bank conflicts on Fermi
#else
#define SHARED_STRIDE 8 // to avoid bank conflicts on G80 and GT200
#endif
#else
#if (__COMPUTE_CAPABILITY__ >= 200)
#define SHARED_STRIDE 32 // to avoid bank conflicts on Fermi
#else
#define SHARED_STRIDE 16 // to avoid bank conflicts on G80 and GT200
#endif
#endif
""")

    if sharedFloats > 0:
        prolog_str += (
"""
extern __shared__ char s_data[];
""")

        if dslash:
            prolog_str += (
"""
VOLATILE spinorFloat *s = (spinorFloat*)s_data + DSLASH_SHARED_FLOATS_PER_THREAD*SHARED_STRIDE*(threadIdx.x/SHARED_STRIDE)
+ (threadIdx.x % SHARED_STRIDE);
""")
        else:
            prolog_str += (
"""
VOLATILE spinorFloat *s = (spinorFloat*)s_data + CLOVER_SHARED_FLOATS_PER_THREAD*SHARED_STRIDE*(threadIdx.x/SHARED_STRIDE)
+ (threadIdx.x % SHARED_STRIDE);
""")


    if dslash:
        if dslash4 == True:
            prolog_str += "\n#include \"read_gauge.h\"\n"
        if not domain_wall:
            prolog_str += "#include \"read_clover.h\"\n"
        prolog_str += "#include \"io_spinor.h\"\n"
        if dslash4 == True:
            prolog_str += (
"""
#if (defined MULTI_GPU) && (DD_PREC==2) // half precision
int sp_norm_idx;
#endif // MULTI_GPU half precision
""")
        prolog_str += (
"""
int sid = ((blockIdx.y*blockDim.y + threadIdx.y)*gridDim.x + blockIdx.x)*blockDim.x + threadIdx.x;
if (sid >= param.threads*param.Ls) return;

int boundaryCrossing;
""")
        if domain_wall:
          if dslash4 == True:
            prolog_str+=(
"""
int X, x1, x2, x3, x4, xs;
""")
          else:
            prolog_str+=(
"""
int X, xs;
""")
        else:
          prolog_str+=(
"""
int X, x1, x2, x3, x4, xs;
""")
        if dslash4 == True:
            prolog_str += (
"""
#ifdef MULTI_GPU
int face_idx;
if (kernel_type == INTERIOR_KERNEL) {
#endif
""")
        prolog_str += (
"""
// Inline by hand for the moment and assume even dimensions
//coordsFromIndex(X, x1, x2, x3, x4, sid, param.parity);
""")
        if domain_wall:
          if dslash4 == True:
            prolog_str+=(
"""
boundaryCrossing = sid/X1h + sid/(X2*X1h) + sid/(X3*X2*X1h);

X = 2*sid + (boundaryCrossing + param.parity) % 2;
x1 = X % X1;
x2 = (X/X1) % X2;
x3 = (X/(X1*X2)) % X3;
x4 = (X/(X1*X2*X3)) % X4;
xs = X/(X1*X2*X3*X4);

""")
          else:
            prolog_str+=(
"""
boundaryCrossing = sid/X1h + sid/(X2*X1h) + sid/(X3*X2*X1h);

X = 2*sid + (boundaryCrossing + param.parity) % 2;
xs = X/(X1*X2*X3*X4);

""")

        else:
          prolog_str+=(
"""
X = 2*sid;
int aux1 = X / X1;
x1 = X - aux1 * X1;
int aux2 = aux1 / X2;
x2 = aux1 - aux2 * X2;
x4 = aux2 / X3;
x3 = aux2 - x4 * X3;
aux1 = (param.parity + x4 + x3 + x2) & 1;
x1 += aux1;
X += aux1;

""")

        out = ""
        for s in range(0,4):
            for c in range(0,3):
                out += out_re(s,c)+" = 0; "+out_im(s,c)+" = 0;\n"
        prolog_str+= indent(out)

        if dslash4 == True:
          prolog_str+= (
"""
#ifdef MULTI_GPU
} else { // exterior kernel

const int dim = static_cast<int>(kernel_type);
const int face_volume = (param.threads*param.Ls >> 1); // volume of one face
const int face_num = (sid >= face_volume); // is this thread updating face 0 or 1
face_idx = sid - face_num*face_volume; // index into the respective face

// ghostOffset is scaled to include body (includes stride) and number of FloatN arrays (SPINOR_HOP)
// face_idx not sid since faces are spin projected and share the same volume index (modulo UP/DOWN reading)
//sp_idx = face_idx + param.ghostOffset[dim];

#if (DD_PREC==2) // half precision
sp_norm_idx = sid + param.ghostNormOffset[static_cast<int>(kernel_type)];
#endif

const int dims[] = {X1, X2, X3, X4};
coordsFromDW4DFaceIndex<1>(sid, x1, x2, x3, x4, xs, face_idx, face_volume, dim, face_num, param.parity, dims);

boundaryCrossing = sid/X1h + sid/(X2*X1h) + sid/(X3*X2*X1h);

X = 2*sid + (boundaryCrossing + param.parity) % 2;

READ_INTERMEDIATE_SPINOR(INTERTEX, param.sp_stride, sid, sid);
""")
          out = ""
          for s in range(0,4):
            for c in range(0,3):
              out += out_re(s,c)+" = "+in_re(s,c)+"; "+out_im(s,c)+" = "+in_im(s,c)+";\n"
          prolog_str+= indent(out)
          prolog_str+= "}\n"
          prolog_str+= "#endif // MULTI_GPU\n"

          if domain_wall:
            if dslash4 == True:
              prolog_str += (
"""
// declare G## here and use ASSN below instead of READ
#ifdef GAUGE_FLOAT2
#if (DD_PREC==0) //temporal hack
double2 G0;
double2 G1;
double2 G2;
double2 G3;
double2 G4;
double2 G5;
double2 G6;
double2 G7;
double2 G8;
#else
float2 G0;
float2 G1;
float2 G2;
float2 G3;
float2 G4;
float2 G5;
float2 G6;
float2 G7;
float2 G8;
#endif
#else
float4 G0;
float4 G1;
float4 G2;
float4 G3;
float4 G4;
#endif

""")

          prolog_str+= "\n\n"

    else:
        prolog_str+=(
"""
#include "read_clover.h"
#include "io_spinor.h"

int sid = blockIdx.x*blockDim.x + threadIdx.x;
if (sid >= param.threads) return;

// read spinor from device memory
READ_SPINOR(SPINORTEX, param.sp_stride, sid, sid);

""")
    return prolog_str
# end def prolog


def gen(dir, pack_only=False):
    projIdx = dir if not dagger else dir + ( +1 if dir%2 == 0 else -1 )
    projStr = projectorToStr(projectors[projIdx])
    def proj(i,j):
        return projectors[projIdx][4*i+j]
    
    # if row(i) = (j, c), then the i'th row of the projector can be represented
    # as a multiple of the j'th row: row(i) = c row(j)
    def row(i):
        assert i==2 or i==3
        if proj(i,0) == 0j:
            return (1, proj(i,1))
        if proj(i,1) == 0j:
            return (0, proj(i,0))

# boundary = ["x1==X1m1", "x1==0", "x2==X2m1", "x2==0", "x3==X3m1", "x3==0", "x4==X4m1", "x4==0"]
# interior = ["x1<X1m1", "x1>0", "x2<X2m1", "x2>0", "x3<X3m1", "x3>0", "x4<X4m1", "x4>0"]
    boundary = ["x1==X1m1", "x1==0", "x2==X2m1", "x2==0", "x3==X3m1", "x3==0", "x4==X4m1", "x4==0"]
    interior = ["x1<X1m1", "x1>0", "x2<X2m1", "x2>0", "x3<X3m1", "x3>0", "x4<X4m1", "x4>0"]
    dim = ["X", "Y", "Z", "T"]

    # index of neighboring site when not on boundary
    sp_idx = ["X+1", "X-1", "X+X1", "X-X1", "X+X2X1", "X-X2X1", "X+X3X2X1", "X-X3X2X1"]

    # index of neighboring site (across boundary)
    sp_idx_wrap = ["X-X1m1", "X+X1m1", "X-X2X1mX1", "X+X2X1mX1", "X-X3X2X1mX2X1", "X+X3X2X1mX2X1",
                   "X-X4X3X2X1mX3X2X1", "X+X4X3X2X1mX3X2X1"]

    cond = ""
    cond += "#ifdef MULTI_GPU\n"
    cond += "if ( (kernel_type == INTERIOR_KERNEL && (!param.ghostDim["+`dir/2`+"] || "+interior[dir]+")) ||\n"
    cond += " (kernel_type == EXTERIOR_KERNEL_"+dim[dir/2]+" && "+boundary[dir]+") )\n"
    cond += "#endif\n"

    str = ""
    
    projName = "P"+`dir/2`+["-","+"][projIdx%2]
    str += "// Projector "+projName+"\n"
    for l in projStr.splitlines():
        str += "//"+l+"\n"
    str += "\n"

    str += "#ifdef MULTI_GPU\n"
    str += "const int sp_idx = (kernel_type == INTERIOR_KERNEL) ? ("+boundary[dir]+" ? "+sp_idx_wrap[dir]+" : "+sp_idx[dir]+") >> 1 :\n"
    str += " face_idx + param.ghostOffset[static_cast<int>(kernel_type)];\n"
    str += "#else\n"
    str += "const int sp_idx = ("+boundary[dir]+" ? "+sp_idx_wrap[dir]+" : "+sp_idx[dir]+") >> 1;\n"
    str += "#endif\n"

    str += "\n"
    if dir % 2 == 0:
        if domain_wall: str += "const int ga_idx = sid % Vh;\n"
        else: str += "const int ga_idx = sid;\n"
    else:
        str += "#ifdef MULTI_GPU\n"
        if domain_wall: str += "const int ga_idx = ((kernel_type == INTERIOR_KERNEL) ? sp_idx % Vh : Vh+(face_idx % ghostFace[static_cast<int>(kernel_type)]));\n"
        else: str += "const int ga_idx = ((kernel_type == INTERIOR_KERNEL) ? sp_idx : Vh+face_idx);\n"
        str += "#else\n"
        if domain_wall: str += "const int ga_idx = sp_idx % Vh;\n"
        else: str += "const int ga_idx = sp_idx;\n"
        str += "#endif\n"
    str += "\n"

    # scan the projector to determine which loads are required
    row_cnt = ([0,0,0,0])
    for h in range(0,4):
        for s in range(0,4):
            re = proj(h,s).real
            im = proj(h,s).imag
            if re != 0 or im != 0:
                row_cnt[h] += 1
    row_cnt[0] += row_cnt[1]
    row_cnt[2] += row_cnt[3]

    decl_half = ""
    for h in range(0, 2):
        for c in range(0, 3):
            decl_half += "spinorFloat "+h1_re(h,c)+", "+h1_im(h,c)+";\n";
    decl_half += "\n"

    load_spinor = "// read spinor from device memory\n"
    if row_cnt[0] == 0:
        load_spinor += "READ_SPINOR_DOWN(SPINORTEX, param.sp_stride, sp_idx, sp_idx);\n"
    elif row_cnt[2] == 0:
        load_spinor += "READ_SPINOR_UP(SPINORTEX, param.sp_stride, sp_idx, sp_idx);\n"
    else:
        load_spinor += "READ_SPINOR(SPINORTEX, param.sp_stride, sp_idx, sp_idx);\n"
    load_spinor += "\n"

    load_half = ""
    if domain_wall : 
        load_half += "const int sp_stride_pad = param.Ls*ghostFace[static_cast<int>(kernel_type)];\n"
    else :
        load_half += "const int sp_stride_pad = ghostFace[static_cast<int>(kernel_type)];\n" 
    #load_half += "#if (DD_PREC==2) // half precision\n"
    #load_half += "const int sp_norm_idx = sid + param.ghostNormOffset[static_cast<int>(kernel_type)];\n"
    #load_half += "#endif\n"

    if dir >= 6: load_half += "const int t_proj_scale = TPROJSCALE;\n"
    load_half += "\n"
    load_half += "// read half spinor from device memory\n"

# we have to use the same volume index for backwards and forwards gathers
# instead of using READ_UP_SPINOR and READ_DOWN_SPINOR, just use READ_HALF_SPINOR with the appropriate shift
    if (dir+1) % 2 == 0: load_half += "READ_HALF_SPINOR(SPINORTEX, sp_stride_pad, sp_idx, sp_norm_idx);\n\n"
    else: load_half += "READ_HALF_SPINOR(SPINORTEX, sp_stride_pad, sp_idx + (SPINOR_HOP/2)*sp_stride_pad, sp_norm_idx);\n\n"
    load_gauge = "// read gauge matrix from device memory\n"
    load_gauge += "ASSN_GAUGE_MATRIX(G, GAUGE"+`( dir%2)`+"TEX, "+`dir`+", ga_idx, ga_stride);\n\n"

    reconstruct_gauge = "// reconstruct gauge matrix\n"
    reconstruct_gauge += "RECONSTRUCT_GAUGE_MATRIX("+`dir`+");\n\n"

    project = "// project spinor into half spinors\n"
    for h in range(0, 2):
        for c in range(0, 3):
            strRe = ""
            strIm = ""
            for s in range(0, 4):
                re = proj(h,s).real
                im = proj(h,s).imag
                if re==0 and im==0: ()
                elif im==0:
                    strRe += sign(re)+in_re(s,c)
                    strIm += sign(re)+in_im(s,c)
                elif re==0:
                    strRe += sign(-im)+in_im(s,c)
                    strIm += sign(im)+in_re(s,c)
            if row_cnt[0] == 0: # projector defined on lower half only
                for s in range(0, 4):
                    re = proj(h+2,s).real
                    im = proj(h+2,s).imag
                    if re==0 and im==0: ()
                    elif im==0:
                        strRe += sign(re)+in_re(s,c)
                        strIm += sign(re)+in_im(s,c)
                    elif re==0:
                        strRe += sign(-im)+in_im(s,c)
                        strIm += sign(im)+in_re(s,c)
                
            project += h1_re(h,c)+" = "+strRe+";\n"
            project += h1_im(h,c)+" = "+strIm+";\n"

    copy_half = ""
    for h in range(0, 2):
        for c in range(0, 3):
            copy_half += h1_re(h,c)+" = "+("t_proj_scale*" if (dir >= 6) else "")+in_re(h,c)+"; "
            copy_half += h1_im(h,c)+" = "+("t_proj_scale*" if (dir >= 6) else "")+in_im(h,c)+";\n"
    copy_half += "\n"

    prep_half = ""
    prep_half += "#ifdef MULTI_GPU\n"
    prep_half += "if (kernel_type == INTERIOR_KERNEL) {\n"
    prep_half += "#endif\n"
    prep_half += "\n"
    prep_half += indent(load_spinor)
    prep_half += indent(project)
    prep_half += "\n"
    prep_half += "#ifdef MULTI_GPU\n"
    prep_half += "} else {\n"
    prep_half += "\n"
    prep_half += indent(load_half)
    prep_half += indent(copy_half)
    prep_half += "}\n"
    prep_half += "#endif // MULTI_GPU\n"
    prep_half += "\n"
    
    ident = "// identity gauge matrix\n"
    for m in range(0,3):
        for h in range(0,2):
            ident += "spinorFloat "+h2_re(h,m)+" = " + h1_re(h,m) + "; "
            ident += "spinorFloat "+h2_im(h,m)+" = " + h1_im(h,m) + ";\n"
    ident += "\n"
    
    mult = ""
    for m in range(0,3):
        mult += "// multiply row "+`m`+"\n"
        for h in range(0,2):
            re = "spinorFloat "+h2_re(h,m)+" = 0;\n"
            im = "spinorFloat "+h2_im(h,m)+" = 0;\n"
            for c in range(0,3):
                re += h2_re(h,m) + " += " + g_re(dir,m,c) + " * "+h1_re(h,c)+";\n"
                re += h2_re(h,m) + " -= " + g_im(dir,m,c) + " * "+h1_im(h,c)+";\n"
                im += h2_im(h,m) + " += " + g_re(dir,m,c) + " * "+h1_im(h,c)+";\n"
                im += h2_im(h,m) + " += " + g_im(dir,m,c) + " * "+h1_re(h,c)+";\n"
            mult += re + im
        mult += "\n"
    
    reconstruct = ""
    for m in range(0,3):

        for h in range(0,2):
            h_out = h
            if row_cnt[0] == 0: # projector defined on lower half only
                h_out = h+2
            reconstruct += out_re(h_out, m) + " += " + h2_re(h,m) + ";\n"
            reconstruct += out_im(h_out, m) + " += " + h2_im(h,m) + ";\n"
    
        for s in range(2,4):
            (h,c) = row(s)
            re = c.real
            im = c.imag
            if im == 0 and re == 0:
                ()
            elif im == 0:
                reconstruct += out_re(s, m) + " " + sign(re) + "= " + h2_re(h,m) + ";\n"
                reconstruct += out_im(s, m) + " " + sign(re) + "= " + h2_im(h,m) + ";\n"
            elif re == 0:
                reconstruct += out_re(s, m) + " " + sign(-im) + "= " + h2_im(h,m) + ";\n"
                reconstruct += out_im(s, m) + " " + sign(+im) + "= " + h2_re(h,m) + ";\n"
        
        if ( m < 2 ): reconstruct += "\n"

    if dir >= 6:
        str += "if (gauge_fixed && ga_idx < X4X3X2X1hmX3X2X1h)\n"
        str += block(decl_half + prep_half + ident + reconstruct)
        str += " else "
        str += block(load_gauge + decl_half + prep_half + reconstruct_gauge + mult + reconstruct)
    else:
        str += load_gauge + decl_half + prep_half + reconstruct_gauge + mult + reconstruct
    
    if pack_only:
        out = load_spinor + decl_half + project
        out = out.replace("sp_idx", "idx")
        return out
    else:
        return cond + block(str)+"\n\n"
# end def gen

def gen_dw():
    if dagger: lsign='-'; ledge = '0'; rsign='+'; redge='param.Ls-1'
    else: lsign='+'; ledge = 'param.Ls-1'; rsign='-'; redge='0'

    str = "\n\n"
    str += "// 5th dimension -- NB: not partitionable!\n"
    if normalDWF:
      str += "#ifdef MULTI_GPU\nif(kernel_type == INTERIOR_KERNEL)\n#endif\n"
    str += "{\n// 2 P_L = 2 P_- = ( ( +1, -1 ), ( -1, +1 ) )\n"
    str += "  {\n"
    str += "     int sp_idx = ( xs == %s ? X%s(param.Ls-1)*2*Vh : X%s2*Vh ) / 2;\n" % (ledge, rsign, lsign)
    str += "\n"
    str += "// read spinor from device memory\n"
    str += "     READ_SPINOR( SPINORTEX, param.sp_stride, sp_idx, sp_idx );\n"
    str += "\n"
    str += "     if ( xs != %s )\n" % ledge
    str += "     {\n"
    
    def proj(i,j):
        return two_P_L[4*i+j]
    
    # xs != 0:
    out_L = ""
    for s1 in range(0,4):
    #{
        for c in range(0,3):
            re_rhs, im_rhs = "", ""
            for s2 in range(0,4):
                re, im = proj(s1,s2).real, proj(s1,s2).imag
                if re != 0 :
                    re_rhs += sign(re) + in_re(s2,c)
                    im_rhs += sign(re) + in_im(s2,c)
                if im != 0 :
                    re_rhs += sign(-im) + in_im(s2,c)
                    im_rhs += sign(im) + in_re(s2,c)
            out_L += 3*" " + out_re(s1,c) + " += " + re_rhs + ";"
            out_L += 3*" " + out_im(s1,c) + " += " + im_rhs + ";\n"
        if s1 < 3 : out_L += "\n"
    #}
    
    str += out_L

    str += "    }\n"
    str += "    else\n"
    str += "    {\n"
    
    # xs == 0:
    str += out_L.replace(" += "," += -mferm*(").replace(";",");")

    str += "    } // end if ( xs != %s )\n" % ledge
    str += "  } // end P_L\n\n"
    str += " // 2 P_R = 2 P_+ = ( ( +1, +1 ), ( +1, +1 ) )\n"
    str += "  {\n"
    str += "    int sp_idx = ( xs == %s ? X%s(param.Ls-1)*2*Vh : X%s2*Vh ) / 2;\n" % (redge, lsign, rsign)
    str += "\n"
    str += "// read spinor from device memory\n"
    str += "    READ_SPINOR( SPINORTEX, param.sp_stride, sp_idx, sp_idx );\n"
    str += "\n"
    str += "    if ( xs != %s )\n" % redge
    str += "    {\n"
    
    # xs != Ls-1
    str += out_L.replace("-","+")

    str += "    }\n"
    str += "    else\n"
    str += "    {\n"

    # xs == Ls-1
    str += out_L.replace("-","+").replace(" += "," += -mferm*(").replace(";",");")

    str += "    } // end if ( xs != %s )\n" % redge
    str += "  } // end P_R\n\n"

    if dslash5:
      str += "  // MDWF Dslash_5 operator is given as follow\n"
      str += "  // Dslash4pre = [c_5(s)(P_+\delta_{s,s`+1} - mP_+\delta_{s,0}\delta_{s`,L_s-1}\n"
      str += "  //         + P_-\delta_{s,s`-1}-mP_-\delta_{s,L_s-1}\delta_{s`,0})\n"
      str += "  //         + b_5(s)\delta_{s,s`}]\delta_{x,x`}\n"
      str += "  // For Dslash4pre\n"
      str += "  // C_5 \equiv c_5(s)*0.5\n"
      str += "  // B_5 \equiv b_5(s)\n"
      str += "  // For Dslash5\n"
      str += "  // C_5 \equiv 0.5*{c_5(s)(4+M_5)-1}/{b_5(s)(4+M_5)+1}\n"
      str += "  // B_5 \equiv 1.0\n"
      str += "#ifdef MDWF_mode   // Check whether MDWF option is enabled\n" 
      str += "#if (MDWF_mode==1)\n"
      str += "  VOLATILE spinorFloat C_5;\n"
      str += "  VOLATILE spinorFloat B_5;\n"
      str += "  C_5 = (spinorFloat)mdwf_c5[xs]*0.5;\n"
      str += "  B_5 = (spinorFloat)mdwf_b5[xs];\n\n"
      str += "  READ_SPINOR( SPINORTEX, param.sp_stride, X/2, X/2 );\n"
      str += "  o00_re = C_5*o00_re + B_5*i00_re;\n" 
      str += "  o00_im = C_5*o00_im + B_5*i00_im;\n"
      str += "  o01_re = C_5*o01_re + B_5*i01_re;\n"
      str += "  o01_im = C_5*o01_im + B_5*i01_im;\n"
      str += "  o02_re = C_5*o02_re + B_5*i02_re;\n"
      str += "  o02_im = C_5*o02_im + B_5*i02_im;\n"
      str += "  o10_re = C_5*o10_re + B_5*i10_re;\n"
      str += "  o10_im = C_5*o10_im + B_5*i10_im;\n"
      str += "  o11_re = C_5*o11_re + B_5*i11_re;\n"
      str += "  o11_im = C_5*o11_im + B_5*i11_im;\n"
      str += "  o12_re = C_5*o12_re + B_5*i12_re;\n"
      str += "  o12_im = C_5*o12_im + B_5*i12_im;\n"
      str += "  o20_re = C_5*o20_re + B_5*i20_re;\n"
      str += "  o20_im = C_5*o20_im + B_5*i20_im;\n"
      str += "  o21_re = C_5*o21_re + B_5*i21_re;\n"
      str += "  o21_im = C_5*o21_im + B_5*i21_im;\n"
      str += "  o22_re = C_5*o22_re + B_5*i22_re;\n"
      str += "  o22_im = C_5*o22_im + B_5*i22_im;\n"
      str += "  o30_re = C_5*o30_re + B_5*i30_re;\n"
      str += "  o30_im = C_5*o30_im + B_5*i30_im;\n"
      str += "  o31_re = C_5*o31_re + B_5*i31_re;\n"
      str += "  o31_im = C_5*o31_im + B_5*i31_im;\n"
      str += "  o32_re = C_5*o32_re + B_5*i32_re;\n"
      str += "  o32_im = C_5*o32_im + B_5*i32_im;\n"
      str += "#elif (MDWF_mode==2)\n"
      str += "  VOLATILE spinorFloat C_5;\n"
      str += "  C_5 = (spinorFloat)(0.5*(mdwf_c5[xs]*(m5+4.0) - 1.0)/(mdwf_b5[xs]*(m5+4.0) + 1.0));\n\n"
      str += "  READ_SPINOR( SPINORTEX, param.sp_stride, X/2, X/2 );\n"
      str += "  o00_re = C_5*o00_re + i00_re;\n"
      str += "  o00_im = C_5*o00_im + i00_im;\n"
      str += "  o01_re = C_5*o01_re + i01_re;\n"
      str += "  o01_im = C_5*o01_im + i01_im;\n"
      str += "  o02_re = C_5*o02_re + i02_re;\n"
      str += "  o02_im = C_5*o02_im + i02_im;\n"
      str += "  o10_re = C_5*o10_re + i10_re;\n"
      str += "  o10_im = C_5*o10_im + i10_im;\n"
      str += "  o11_re = C_5*o11_re + i11_re;\n"
      str += "  o11_im = C_5*o11_im + i11_im;\n"
      str += "  o12_re = C_5*o12_re + i12_re;\n"
      str += "  o12_im = C_5*o12_im + i12_im;\n"
      str += "  o20_re = C_5*o20_re + i20_re;\n"
      str += "  o20_im = C_5*o20_im + i20_im;\n"
      str += "  o21_re = C_5*o21_re + i21_re;\n"
      str += "  o21_im = C_5*o21_im + i21_im;\n"
      str += "  o22_re = C_5*o22_re + i22_re;\n"
      str += "  o22_im = C_5*o22_im + i22_im;\n"
      str += "  o30_re = C_5*o30_re + i30_re;\n"
      str += "  o30_im = C_5*o30_im + i30_im;\n"
      str += "  o31_re = C_5*o31_re + i31_re;\n"
      str += "  o31_im = C_5*o31_im + i31_im;\n"
      str += "  o32_re = C_5*o32_re + i32_re;\n"
      str += "  o32_im = C_5*o32_im + i32_im;\n"
      str += "#endif  // select MDWF mode\n"
      str += "#endif  // check MDWF on/off\n"
    str += "} // end 5th dimension\n\n"

    return str
# end def gen_dw

def gen_dw_inv():

    str = "\n"
    str += "VOLATILE spinorFloat kappa;\n\n"
    str += "#ifdef MDWF_mode   // Check whether MDWF option is enabled\n" 
    str += "  kappa = (spinorFloat)(-(mdwf_c5[xs]*(4.0 + m5) - 1.0)/(mdwf_b5[xs]*(4.0 + m5) + 1.0));\n"
    str += "#else\n" 
    str += "  kappa = 2.0*a;\n"
    str += "#endif  // select MDWF mode\n\n"
    str += "// M5_inv operation -- NB: not partitionable!\n\n"
    str += "// In this part, we will do the following operation in parallel way.\n\n"
    str += "// w = M5inv * v\n"
    str += "// 'w' means output vector\n"
    str += "// 'v' means input vector\n"
    str += "{\n"
    str += "  int base_idx = sid%Vh;\n"
    str += "  int sp_idx;\n\n"
    str += "// let's assume the index,\n"
    str += "// s = output vector index,\n"
    str += "// s' = input vector index and\n"
    str += "// 'a'= kappa5\n"
    str += "\n"
    str += "  spinorFloat inv_d_n = 1.0 / ( 1.0 + pow(kappa,param.Ls)*mferm);\n"
    str += "  spinorFloat factorR;\n"
    str += "  spinorFloat factorL;\n"
    str += "\n"
    str += "  for(int s = 0; s < param.Ls; s++)\n  {\n"
    if dagger == True :  
        str += "    factorR = ( xs > s ? -inv_d_n*pow(kappa,param.Ls-xs+s)*mferm : inv_d_n*pow(kappa,s-xs))/2.0;\n\n"
    else : 
        str += "    factorR = ( xs < s ? -inv_d_n*pow(kappa,param.Ls-s+xs)*mferm : inv_d_n*pow(kappa,xs-s))/2.0;\n\n"
    str += "    sp_idx = base_idx + s*Vh;\n"
    str += "    // read spinor from device memory\n"
    str += "    READ_SPINOR( SPINORTEX, param.sp_stride, sp_idx, sp_idx );\n\n"
    str += "    o00_re += factorR*(i00_re + i20_re);\n" 
    str += "    o00_im += factorR*(i00_im + i20_im);\n"
    str += "    o20_re += factorR*(i00_re + i20_re);\n" 
    str += "    o20_im += factorR*(i00_im + i20_im);\n"
    str += "    o01_re += factorR*(i01_re + i21_re);\n" 
    str += "    o01_im += factorR*(i01_im + i21_im);\n"
    str += "    o21_re += factorR*(i01_re + i21_re);\n" 
    str += "    o21_im += factorR*(i01_im + i21_im);\n"
    str += "    o02_re += factorR*(i02_re + i22_re);\n" 
    str += "    o02_im += factorR*(i02_im + i22_im);\n"
    str += "    o22_re += factorR*(i02_re + i22_re);\n" 
    str += "    o22_im += factorR*(i02_im + i22_im);\n"
    str += "    o10_re += factorR*(i10_re + i30_re);\n" 
    str += "    o10_im += factorR*(i10_im + i30_im);\n"
    str += "    o30_re += factorR*(i10_re + i30_re);\n" 
    str += "    o30_im += factorR*(i10_im + i30_im);\n"
    str += "    o11_re += factorR*(i11_re + i31_re);\n" 
    str += "    o11_im += factorR*(i11_im + i31_im);\n"
    str += "    o31_re += factorR*(i11_re + i31_re);\n" 
    str += "    o31_im += factorR*(i11_im + i31_im);\n"
    str += "    o12_re += factorR*(i12_re + i32_re);\n" 
    str += "    o12_im += factorR*(i12_im + i32_im);\n"
    str += "    o32_re += factorR*(i12_re + i32_re);\n" 
    str += "    o32_im += factorR*(i12_im + i32_im);\n\n"
    
    if dagger == True :  
        str += "    factorL = ( xs < s ? -inv_d_n*pow(kappa,param.Ls-s+xs)*mferm : inv_d_n*pow(kappa,xs-s))/2.0;\n\n"
    else : 
        str += "    factorL = ( xs > s ? -inv_d_n*pow(kappa,param.Ls-xs+s)*mferm : inv_d_n*pow(kappa,s-xs))/2.0;\n\n"

    str += "    o00_re += factorL*(i00_re - i20_re);\n"
    str += "    o00_im += factorL*(i00_im - i20_im);\n"
    str += "    o01_re += factorL*(i01_re - i21_re);\n" 
    str += "    o01_im += factorL*(i01_im - i21_im);\n"
    str += "    o02_re += factorL*(i02_re - i22_re);\n" 
    str += "    o02_im += factorL*(i02_im - i22_im);\n"
    str += "    o10_re += factorL*(i10_re - i30_re);\n" 
    str += "    o10_im += factorL*(i10_im - i30_im);\n"
    str += "    o11_re += factorL*(i11_re - i31_re);\n" 
    str += "    o11_im += factorL*(i11_im - i31_im);\n"
    str += "    o12_re += factorL*(i12_re - i32_re);\n" 
    str += "    o12_im += factorL*(i12_im - i32_im);\n"
    str += "    o20_re += factorL*(i20_re - i00_re);\n" 
    str += "    o20_im += factorL*(i20_im - i00_im);\n"
    str += "    o21_re += factorL*(i21_re - i01_re);\n" 
    str += "    o21_im += factorL*(i21_im - i01_im);\n"
    str += "    o22_re += factorL*(i22_re - i02_re);\n" 
    str += "    o22_im += factorL*(i22_im - i02_im);\n"
    str += "    o30_re += factorL*(i30_re - i10_re);\n" 
    str += "    o30_im += factorL*(i30_im - i10_im);\n"
    str += "    o31_re += factorL*(i31_re - i11_re);\n" 
    str += "    o31_im += factorL*(i31_im - i11_im);\n"
    str += "    o32_re += factorL*(i32_re - i12_re);\n" 
    str += "    o32_im += factorL*(i32_im - i12_im);\n"
    str += "  }\n"
    str += "} // end of M5inv dimension\n\n"

    return str
# end def gen_dw_inv


def input_spinor(s,c,z):
    if dslash:
        if z==0: return out_re(s,c)
        else: return out_im(s,c)
    else:
        if z==0: return in_re(s,c)
        else: return in_im(s,c)

def to_chiral_basis(c):
    str = ""
    str += "spinorFloat "+a_re(0,0,c)+" = -"+input_spinor(1,c,0)+" - "+input_spinor(3,c,0)+";\n"
    str += "spinorFloat "+a_im(0,0,c)+" = -"+input_spinor(1,c,1)+" - "+input_spinor(3,c,1)+";\n"
    str += "spinorFloat "+a_re(0,1,c)+" = "+input_spinor(0,c,0)+" + "+input_spinor(2,c,0)+";\n"
    str += "spinorFloat "+a_im(0,1,c)+" = "+input_spinor(0,c,1)+" + "+input_spinor(2,c,1)+";\n"
    str += "spinorFloat "+a_re(0,2,c)+" = -"+input_spinor(1,c,0)+" + "+input_spinor(3,c,0)+";\n"
    str += "spinorFloat "+a_im(0,2,c)+" = -"+input_spinor(1,c,1)+" + "+input_spinor(3,c,1)+";\n"
    str += "spinorFloat "+a_re(0,3,c)+" = "+input_spinor(0,c,0)+" - "+input_spinor(2,c,0)+";\n"
    str += "spinorFloat "+a_im(0,3,c)+" = "+input_spinor(0,c,1)+" - "+input_spinor(2,c,1)+";\n"
    str += "\n"

    for s in range (0,4):
        str += out_re(s,c)+" = "+a_re(0,s,c)+"; "
        str += out_im(s,c)+" = "+a_im(0,s,c)+";\n"

    return block(str)+"\n\n"
# end def to_chiral_basis


def from_chiral_basis(c): # note: factor of 1/2 is included in clover term normalization
    str = ""
    str += "spinorFloat "+a_re(0,0,c)+" = "+out_re(1,c)+" + "+out_re(3,c)+";\n"
    str += "spinorFloat "+a_im(0,0,c)+" = "+out_im(1,c)+" + "+out_im(3,c)+";\n"
    str += "spinorFloat "+a_re(0,1,c)+" = -"+out_re(0,c)+" - "+out_re(2,c)+";\n"
    str += "spinorFloat "+a_im(0,1,c)+" = -"+out_im(0,c)+" - "+out_im(2,c)+";\n"
    str += "spinorFloat "+a_re(0,2,c)+" = "+out_re(1,c)+" - "+out_re(3,c)+";\n"
    str += "spinorFloat "+a_im(0,2,c)+" = "+out_im(1,c)+" - "+out_im(3,c)+";\n"
    str += "spinorFloat "+a_re(0,3,c)+" = -"+out_re(0,c)+" + "+out_re(2,c)+";\n"
    str += "spinorFloat "+a_im(0,3,c)+" = -"+out_im(0,c)+" + "+out_im(2,c)+";\n"
    str += "\n"

    for s in range (0,4):
        str += out_re(s,c)+" = "+a_re(0,s,c)+"; "
        str += out_im(s,c)+" = "+a_im(0,s,c)+";\n"

    return block(str)+"\n\n"
# end def from_chiral_basis


def clover_mult(chi):
    str = "READ_CLOVER(CLOVERTEX, "+`chi`+")\n\n"

    for s in range (0,2):
        for c in range (0,3):
            str += "spinorFloat "+a_re(chi,s,c)+" = 0; spinorFloat "+a_im(chi,s,c)+" = 0;\n"
    str += "\n"

    for sm in range (0,2):
        for cm in range (0,3):
            for sn in range (0,2):
                for cn in range (0,3):
                    str += a_re(chi,sm,cm)+" += "+c_re(chi,sm,cm,sn,cn)+" * "+out_re(2*chi+sn,cn)+";\n"
                    if (sn != sm) or (cn != cm):
                        str += a_re(chi,sm,cm)+" -= "+c_im(chi,sm,cm,sn,cn)+" * "+out_im(2*chi+sn,cn)+";\n"
                    #else: str += ";\n"
                    str += a_im(chi,sm,cm)+" += "+c_re(chi,sm,cm,sn,cn)+" * "+out_im(2*chi+sn,cn)+";\n"
                    if (sn != sm) or (cn != cm):
                        str += a_im(chi,sm,cm)+" += "+c_im(chi,sm,cm,sn,cn)+" * "+out_re(2*chi+sn,cn)+";\n"
                    #else: str += ";\n"
            str += "\n"

    for s in range (0,2):
        for c in range (0,3):
            str += out_re(2*chi+s,c)+" = "+a_re(chi,s,c)+"; "
            str += out_im(2*chi+s,c)+" = "+a_im(chi,s,c)+";\n"
    str += "\n"

    return block(str)+"\n\n"
# end def clover_mult


def apply_clover():
    if domain_wall: return ""
    str = ""
    if dslash: str += "#ifdef DSLASH_CLOVER\n\n"
    str += "// change to chiral basis\n"
    str += to_chiral_basis(0) + to_chiral_basis(1) + to_chiral_basis(2)
    str += "// apply first chiral block\n"
    str += clover_mult(0)
    str += "// apply second chiral block\n"
    str += clover_mult(1)
    str += "// change back from chiral basis\n"
    str += "// (note: required factor of 1/2 is included in clover term normalization)\n"
    str += from_chiral_basis(0) + from_chiral_basis(1) + from_chiral_basis(2)
    if dslash: str += "#endif // DSLASH_CLOVER\n\n"

    return str
# end def clover


def twisted_rotate(x):
    str = "// apply twisted mass rotation\n"

    for h in range(0, 4):
        for c in range(0, 3):
            strRe = ""
            strIm = ""
            for s in range(0, 4):
                # identity
                re = id[4*h+s].real
                im = id[4*h+s].imag
                if re==0 and im==0: ()
                elif im==0:
                    strRe += sign(re)+out_re(s,c)
                    strIm += sign(re)+out_im(s,c)
                elif re==0:
                    strRe += sign(-im)+out_im(s,c)
                    strIm += sign(im)+out_re(s,c)
                
                # sign(x)*i*mu*gamma_5
                re = igamma5[4*h+s].real
                im = igamma5[4*h+s].imag
                if re==0 and im==0: ()
                elif im==0:
                    strRe += sign(re*x)+out_re(s,c) + "*a"
                    strIm += sign(re*x)+out_im(s,c) + "*a"
                elif re==0:
                    strRe += sign(-im*x)+out_im(s,c) + "*a"
                    strIm += sign(im*x)+out_re(s,c) + "*a"

            str += "VOLATILE spinorFloat "+tmp_re(h,c)+" = " + strRe + ";\n"
            str += "VOLATILE spinorFloat "+tmp_im(h,c)+" = " + strIm + ";\n"
        str += "\n"
    
    return str+"\n"


def twisted():
    str = ""
    str += twisted_rotate(+1)

    str += "#ifndef DSLASH_XPAY\n"
    str += "//scale by b = 1/(1 + a*a) \n"
    for s in range(0,4):
        for c in range(0,3):
            str += out_re(s,c) + " = b*" + tmp_re(s,c) + ";\n"
            str += out_im(s,c) + " = b*" + tmp_im(s,c) + ";\n"
    str += "#else\n"
    for s in range(0,4):
        for c in range(0,3):
            str += out_re(s,c) + " = " + tmp_re(s,c) + ";\n"
            str += out_im(s,c) + " = " + tmp_im(s,c) + ";\n"
    str += "#endif // DSLASH_XPAY\n"
    str += "\n"

    return block(str)+"\n"
# end def twisted

def coeff():
  if dslash4:
    str = "coeff"
  elif dslash5:
    str = "coeff"
  else :
    str = "a" 
  return str
# end def coeff()

def ypax():
    str = ""
    str += "#ifdef SPINOR_DOUBLE\n"

    for s in range(0,4):
        for c in range(0,3):
            i = 3*s+c
            if twist == False:
                str += out_re(s,c) +" = "+out_re(s,c)+" + coeff*accum"+nthFloat2(2*i+0)+";\n"
                str += out_im(s,c) +" = "+out_im(s,c)+" + coeff*accum"+nthFloat2(2*i+1)+";\n"

    str += "#else\n"

    for s in range(0,4):
        for c in range(0,3):
            i = 3*s+c
            if twist == False:
                str += out_re(s,c) +" = "+out_re(s,c)+" + coeff*accum"+nthFloat4(2*i+0)+";\n"
                str += out_im(s,c) +" = "+out_im(s,c)+" + coeff*accum"+nthFloat4(2*i+1)+";\n"

    str += "#endif // SPINOR_DOUBLE\n"
    return str
# end def ypax

def xpay():
    str = ""
    str += "#ifdef DSLASH_XPAY\n"
    str += "READ_ACCUM(ACCUMTEX, param.sp_stride)\n"
    if dslash4:
      str += "VOLATILE spinorFloat coeff;\n\n"
      str += "#ifdef MDWF_mode\n"
      str += "coeff = (spinorFloat)(0.5*a/(mdwf_b5[xs]*(m5+4.0) + 1.0));\n"
      str += "#else\n"
      str += "coeff = a;\n"
      str += "#endif\n\n"
    elif dslash5:
      str += "VOLATILE spinorFloat coeff;\n\n"
      str += "#ifdef MDWF_mode\n"
      str += "coeff = (spinorFloat)(0.5/(mdwf_b5[xs]*(m5+4.0) + 1.0));\n"
      str += "coeff *= -coeff;\n"
      str += "#else\n"
      str += "coeff = a;\n"
      str += "#endif\n\n"
      str += "#ifdef YPAX\n"
      str += ypax()
      str += "#else\n"
    
    str += "#ifdef SPINOR_DOUBLE\n"

    for s in range(0,4):
        for c in range(0,3):
            i = 3*s+c
            if twist == False:
                str += out_re(s,c) +" = "+coeff()+"*"+out_re(s,c)+" + accum"+nthFloat2(2*i+0)+";\n"
                str += out_im(s,c) +" = "+coeff()+"*"+out_im(s,c)+" + accum"+nthFloat2(2*i+1)+";\n"
            else:
                str += out_re(s,c) +" = b*"+out_re(s,c)+" + accum"+nthFloat2(2*i+0)+";\n"
                str += out_im(s,c) +" = b*"+out_im(s,c)+" + accum"+nthFloat2(2*i+1)+";\n"

    str += "#else\n"

    for s in range(0,4):
        for c in range(0,3):
            i = 3*s+c
            if twist == False:
                str += out_re(s,c) +" = "+coeff()+"*"+out_re(s,c)+" + accum"+nthFloat4(2*i+0)+";\n"
                str += out_im(s,c) +" = "+coeff()+"*"+out_im(s,c)+" + accum"+nthFloat4(2*i+1)+";\n"
            else:
                str += out_re(s,c) +" = b*"+out_re(s,c)+" + accum"+nthFloat4(2*i+0)+";\n"
                str += out_im(s,c) +" = b*"+out_im(s,c)+" + accum"+nthFloat4(2*i+1)+";\n"

    str += "#endif // SPINOR_DOUBLE\n"
    if dslash5:
      str += "#endif // YPAX\n"
    str += "#endif // DSLASH_XPAY\n"

    return str
# end def xpay


def epilog():
    str = ""
    if dslash:
        if twist:
            str += "#ifdef MULTI_GPU\n"
        else:
            if domain_wall:
              if dslash4:
                str += "#if defined MULTI_GPU && defined DSLASH_XPAY\n"
            else:
                str += "#if defined MULTI_GPU && (defined DSLASH_XPAY || defined DSLASH_CLOVER)\n"
        if dslash4:
          str += (
"""
int incomplete = 0; // Have all 8 contributions been computed for this site?

switch(kernel_type) { // intentional fall-through
case INTERIOR_KERNEL:
incomplete = incomplete || (param.commDim[3] && (x4==0 || x4==X4m1));
case EXTERIOR_KERNEL_T:
incomplete = incomplete || (param.commDim[2] && (x3==0 || x3==X3m1));
case EXTERIOR_KERNEL_Z:
incomplete = incomplete || (param.commDim[1] && (x2==0 || x2==X2m1));
case EXTERIOR_KERNEL_Y:
incomplete = incomplete || (param.commDim[0] && (x1==0 || x1==X1m1));
}

""")
          str += "if (!incomplete)\n"
          str += "#endif // MULTI_GPU\n"
    
    str += block( "\n" + (twisted() if twist else apply_clover()) + xpay() )
    
    str += "\n\n"
    str += "// write spinor field back to device memory\n"
    str += "WRITE_SPINOR(param.sp_stride);\n\n"

    str += "// undefine to prevent warning when precision is changed\n"
    str += "#undef m5\n"
    str += "#undef mdwf_b5\n"
    str += "#undef mdwf_c5\n"
    str += "#undef spinorFloat\n"
    str += "#undef SHARED_STRIDE\n\n"

    if dslash:
      if dslash4 == True:
        for m in range(0,3):
          for n in range(0,3):
            i = 3*m+n
            str += "#undef "+g_re(0,m,n)+"\n"
            str += "#undef "+g_im(0,m,n)+"\n"
        str += "\n"

    for s in range(0,4):
        for c in range(0,3):
            i = 3*s+c
            str += "#undef "+in_re(s,c)+"\n"
            str += "#undef "+in_im(s,c)+"\n"
    str += "\n"

    if clover == True:
        for m in range(0,6):
            s = m/3
            c = m%3
            str += "#undef "+c_re(0,s,c,s,c)+"\n"
        for n in range(0,6):
            sn = n/3
            cn = n%3
            for m in range(n+1,6):
                sm = m/3
                cm = m%3
                str += "#undef "+c_re(0,sm,cm,sn,cn)+"\n"
                str += "#undef "+c_im(0,sm,cm,sn,cn)+"\n"
    str += "\n"

    for s in range(0,4):
        for c in range(0,3):
            i = 3*s+c
            if 2*i < sharedFloats:
                str += "#undef "+out_re(s,c)+"\n"
                if 2*i+1 < sharedFloats:
                    str += "#undef "+out_im(s,c)+"\n"
    str += "\n"

    str += "#undef VOLATILE\n"

    return str
# end def epilog


def pack_face(facenum):
    str = "\n"
    str += "switch(dim) {\n"
    for dim in range(0,4):
        str += "case "+`dim`+":\n"
        proj = gen(2*dim+facenum, pack_only=True)
        proj += "\n"
        proj += "// write half spinor back to device memory\n"
        proj += "WRITE_HALF_SPINOR(face_volume, face_idx);\n"
        str += indent(block(proj)+"\n"+"break;\n")
    str += "}\n\n"
    return str
# end def pack_face

def generate_pack():
    assert (sharedFloats == 0)
    str = ""
    str += def_input_spinor()
    str += "#include \"io_spinor.h\"\n\n"

    str += "if (face_num) "
    str += block(pack_face(1))
    str += " else "
    str += block(pack_face(0))

    str += "\n\n"
    str += "// undefine to prevent warning when precision is changed\n"
    str += "#undef spinorFloat\n"
    str += "#undef SHARED_STRIDE\n\n"

    for s in range(0,4):
        for c in range(0,3):
            i = 3*s+c
            str += "#undef "+in_re(s,c)+"\n"
            str += "#undef "+in_im(s,c)+"\n"
    str += "\n"

    return str
# end def generate_pack

def generate_dslash():
    r = prolog()
    for i in range(0,8) :
      r += gen( i )
    if domain_wall:
      r += gen_dw()
    r += epilog()
    return r

def generate_dslash4D():
    r = prolog()
    for i in range(0,8) :
      r += gen( i )
    r += epilog()
    return r

def generate_dslash5D():
    r = prolog()
    r += gen_dw()
    r += epilog()
    return r

def generate_dslash5D_inv():
    r = prolog()
    r += gen_dw_inv()
    r += epilog()
    return r

def generate_clover():
    return prolog() + epilog()


# To fit 192 threads/SM (single precision) with 16K shared memory, set sharedFloats to 19 or smaller

sharedFloats = 0
cloverSharedFloats = 0
if(len(sys.argv) > 1):
    if (sys.argv[1] == '--shared'):
        sharedFloats = int(sys.argv[2])
print "Shared floats set to " + str(sharedFloats);

# generate Domain Wall Dslash kernels
domain_wall = True
twist = False
clover = False
dslash4 = True
dslash5 = False
normalDWF = False

print sys.argv[0] + ": generating dw_dslash4_core.h";
dslash = True
dagger = False
f = open('dslash_core/dw_dslash4_core.h', 'w')
f.write(generate_dslash4D())
f.close()

print sys.argv[0] + ": generating dw_dslash4_dagger_core.h";
dslash = True
dagger = True
f = open('dslash_core/dw_dslash4_dagger_core.h', 'w')
f.write(generate_dslash4D())
f.close()

dslash4 = False
dslash5 = True

print sys.argv[0] + ": generating dw_dslash5_core.h";
dslash = True
dagger = False
f = open('dslash_core/dw_dslash5_core.h', 'w')
f.write(generate_dslash5D())
f.close()

print sys.argv[0] + ": generating dw_dslash5_dagger_core.h";
dslash = True
dagger = True
f = open('dslash_core/dw_dslash5_dagger_core.h', 'w')
f.write(generate_dslash5D())
f.close()

dslash5 = False

print sys.argv[0] + ": generating dw_dslash5inv_core.h";
dslash = True
dagger = False
f = open('dslash_core/dw_dslash5inv_core.h', 'w')
f.write(generate_dslash5D_inv())
f.close()

print sys.argv[0] + ": generating dw_dslash5inv_dagger_core.h";
dslash = True
dagger = True
f = open('dslash_core/dw_dslash5inv_dagger_core.h', 'w')
f.write(generate_dslash5D_inv())
f.close()

