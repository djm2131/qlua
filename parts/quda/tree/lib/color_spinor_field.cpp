#include <color_spinor_field.h>
#include <string.h>
#include <iostream>
#include <typeinfo>
#include <face_quda.h>

namespace quda {

  // forward declarations
  double normCpu(const cpuColorSpinorField &b);
  double normCuda(const cudaColorSpinorField &b);

  /*ColorSpinorField::ColorSpinorField() : init(false) {

    }*/

  ColorSpinorParam::ColorSpinorParam(const ColorSpinorField &field) {
    field.fill(*this);
  }

  ColorSpinorField::ColorSpinorField(const ColorSpinorParam &param) 
    : LatticeField(param), init(false), v(0), norm(0), even(0), odd(0), eigenvectors(0)
  {
    create(param.nDim, param.x, param.nColor, param.nSpin, param.twistFlavor, 
	   param.precision, param.pad, param.siteSubset, param.siteOrder, 
	   param.fieldOrder, param.gammaBasis, param.PCtype, param.eigv_dim);
  }

  ColorSpinorField::ColorSpinorField(const ColorSpinorField &field) 
    : LatticeField(field), init(false), v(0), norm(0), even(0), odd(0), eigenvectors(0)
  {
    create(field.nDim, field.x, field.nColor, field.nSpin, field.twistFlavor, 
	   field.precision, field.pad, field.siteSubset, field.siteOrder, 
	   field.fieldOrder, field.gammaBasis, field.PCtype, field.eigv_dim, field.eigv_id);
  }

  ColorSpinorField::~ColorSpinorField() {
    destroy();
  }

  static bool createSpinorGhost = true;
  void setGhostSpinor(bool value) { createSpinorGhost = value; }

  void ColorSpinorField::createGhostZone() {

    if (!createSpinorGhost) {
      total_length = length;
      total_norm_length = 2*stride;
      return;
    }

    if (getVerbosity() == QUDA_DEBUG_VERBOSE) 
      printfQuda("Precision = %d, Subset = %d\n", precision, siteSubset);

    // FIXME - The ghost zone is allocated before we know which
    // operator (and hence number of faces are needed), thus we
    // allocate a ghost zone large enough to cope with the maximum
    // number of faces.  All Wilson-like operators support only
    // involve the excahnge of one face so this is no problem.
    // However, for staggered fermions, we have either nFace=1 or 3,
    // thus we allocated using the latter.  This will artificially
    // raise the GPU memory requirements for naive staggered fermions.
    // One potential future solution may be to separate the ghost zone
    // memory allocation from the field itself, which has other
    // benefits (1. on multi-gpu machines with UVA, we can read the
    // ghost zone directly from the neighbouring field and 2.) we can
    // use a single contiguous buffer for the ghost zone and its norm
    // which will reduce latency for half precision and allow us to
    // enable GPU_COMMS support for half precision).
    int nFaceGhost = (nSpin == 1) ? 3 : 1;

    // For Wilson we have the number of effective faces since the
    // fields are spin projected.
    int num_faces = ((nSpin == 1) ? 2 : 1) * nFaceGhost;
    int num_norm_faces = 2*nFaceGhost;

    // calculate size of ghost zone required
    int ghostVolume = 0;
    //temporal hack
    int dims = nDim == 5 ? (nDim - 1) : nDim;
    int x5   = nDim == 5 ? x[4] : 1; ///includes DW  and non-degenerate TM ghosts
    for (int i=0; i<dims; i++) {
      ghostFace[i] = 0;
      if (commDimPartitioned(i)) {
	ghostFace[i] = 1;
	for (int j=0; j<dims; j++) {
	  if (i==j) continue;
	  ghostFace[i] *= x[j];
	}
	ghostFace[i] *= x5; ///temporal hack : extra dimension for DW ghosts
	if (i==0 && siteSubset != QUDA_FULL_SITE_SUBSET) ghostFace[i] /= 2;
	if (siteSubset == QUDA_FULL_SITE_SUBSET) ghostFace[i] /= 2;
	ghostVolume += ghostFace[i];
      }
      if(i==0){
	ghostOffset[i] = 0;
	ghostNormOffset[i] = 0;
      }else{
	ghostOffset[i] = ghostOffset[i-1] + num_faces*ghostFace[i-1];
	ghostNormOffset[i] = ghostNormOffset[i-1] + num_norm_faces*ghostFace[i-1];
      }

#ifdef MULTI_GPU
      if (getVerbosity() == QUDA_DEBUG_VERBOSE) 
	printfQuda("face %d = %6d commDimPartitioned = %6d ghostOffset = %6d ghostNormOffset = %6d\n", 
		   i, ghostFace[i], commDimPartitioned(i), ghostOffset[i], ghostNormOffset[i]);
#endif
    }//end of outmost for loop
    int ghostNormVolume = num_norm_faces * ghostVolume;
    ghostVolume *= num_faces;

    if (getVerbosity() == QUDA_DEBUG_VERBOSE) 
      printfQuda("Allocated ghost volume = %d, ghost norm volume %d\n", ghostVolume, ghostNormVolume);

    // ghost zones are calculated on c/b volumes
#ifdef MULTI_GPU
    ghost_length = ghostVolume*nColor*nSpin*2; 
    ghost_norm_length = (precision == QUDA_HALF_PRECISION) ? ghostNormVolume : 0;
#else
    ghost_length = 0;
    ghost_norm_length = 0;
#endif

    if (siteSubset == QUDA_FULL_SITE_SUBSET) {
      total_length = length + 2*ghost_length; // 2 ghost zones in a full field
      total_norm_length = 2*(stride + ghost_norm_length); // norm length = 2*stride
    } else {
      total_length = length + ghost_length;
      total_norm_length = (precision == QUDA_HALF_PRECISION) ? stride + ghost_norm_length : 0; // norm length = stride
    }

    if (precision != QUDA_HALF_PRECISION) total_norm_length = 0;

    if (getVerbosity() == QUDA_DEBUG_VERBOSE) {
      printfQuda("ghost length = %d, ghost norm length = %d\n", ghost_length, ghost_norm_length);
      printfQuda("total length = %d, total norm length = %d\n", total_length, total_norm_length);
    }

  } // createGhostZone

  void ColorSpinorField::create(int Ndim, const int *X, int Nc, int Ns, QudaTwistFlavorType Twistflavor, 
				QudaPrecision Prec, int Pad, QudaSiteSubset siteSubset, 
				QudaSiteOrder siteOrder, QudaFieldOrder fieldOrder, 
				QudaGammaBasis gammaBasis, QudaDWFPCType DWFPC, int evdim /*= 0*/, int evid /* = -1*/) {
    this->siteSubset = siteSubset;
    this->siteOrder = siteOrder;
    this->fieldOrder = fieldOrder;
    this->gammaBasis = gammaBasis;

    if (Ndim > QUDA_MAX_DIM){
      errorQuda("Number of dimensions nDim = %d too great", Ndim);
    }
    nDim = Ndim;
    nColor = Nc;
    nSpin = Ns;
    twistFlavor = Twistflavor;

    PCtype = DWFPC;

//! for eigCG:
    eigv_dim    = evdim;
    eigv_id     = (evdim == 0) ? -1: evid;

    precision = Prec;
    volume = 1;
    for (int d=0; d<nDim; d++) {
      x[d] = X[d];
      volume *= x[d];
    }
    volumeCB = siteSubset == QUDA_PARITY_SITE_SUBSET ? volume : volume/2;

   if((twistFlavor == QUDA_TWIST_NONDEG_DOUBLET || twistFlavor == QUDA_TWIST_DEG_DOUBLET) && x[4] != 2) errorQuda("Must be two flavors for non-degenerate twisted mass spinor (while provided with %d number of components)\n", x[4]);//two flavors

    pad = Pad;
    if (siteSubset == QUDA_FULL_SITE_SUBSET) {
      stride = volume/2 + pad; // padding is based on half volume
      length = 2*stride*nColor*nSpin*2;    
    } else {
      stride = volume + pad;
      length = stride*nColor*nSpin*2;
    }

    real_length = volume*nColor*nSpin*2; // physical length

    createGhostZone();

    bytes = total_length * precision; // includes pads and ghost zones
    bytes = (siteSubset == QUDA_FULL_SITE_SUBSET) ? 2*ALIGNMENT_ADJUST(bytes/2) : ALIGNMENT_ADJUST(bytes);

    norm_bytes = total_norm_length * sizeof(float);
    norm_bytes = (siteSubset == QUDA_FULL_SITE_SUBSET) ? 2*ALIGNMENT_ADJUST(norm_bytes/2) : ALIGNMENT_ADJUST(norm_bytes);

    init = true;

//! stuff for deflated solvers (eigenvector sets):
    if(evdim != 0){

      eigv_volume = volume;
      eigv_stride = stride;
      eigv_length = length;
      eigv_real_length = real_length;
//multi-gpu:
      eigv_total_length      = total_length;
      eigv_total_norm_length = total_norm_length;

      eigv_ghost_length      = ghost_length;
      eigv_ghost_norm_length = ghost_norm_length; 

      eigv_bytes       = bytes;
      eigv_norm_bytes  = norm_bytes; 
      
      volume *= evdim;
      stride *= evdim;
      length *= evdim;
      real_length *= evdim;
      
      total_length *= evdim;
      total_norm_length *= evdim;
//won't be used.
      ghost_length *= evdim;
      ghost_norm_length *= evdim;  

      bytes *= evdim;
      norm_bytes *= evdim;

    }else{

      eigv_volume = 0;
      eigv_stride = 0;
      eigv_length = 0;
      eigv_real_length = 0;

      eigv_total_length      = 0;
      eigv_total_norm_length = 0;

      eigv_ghost_length      = 0;
      eigv_ghost_norm_length = 0;

      eigv_bytes       = 0;
      eigv_norm_bytes  = 0;
 
    }

    clearGhostPointers();
    setTuningString();
  }

  void ColorSpinorField::setTuningString() {
    char vol_tmp[TuneKey::volume_n];
    int check;
    check = snprintf(vol_string, TuneKey::volume_n, "%d", x[0]);
    if (check < 0 || check >= TuneKey::volume_n) errorQuda("Error writing volume string");
    for (int d=1; d<nDim; d++) {
      strcpy(vol_tmp, vol_string);
      check = snprintf(vol_string, TuneKey::volume_n, "%sx%d", vol_tmp, x[d]);
      if (check < 0 || check >= TuneKey::volume_n) errorQuda("Error writing volume string");
    }

    int aux_string_n = TuneKey::aux_n / 2;
    char aux_tmp[aux_string_n];
    check = snprintf(aux_string, aux_string_n, "vol=%d,stride=%d,precision=%d", volume, stride, precision);
    if (check < 0 || check >= aux_string_n) errorQuda("Error writing aux string");

    if (twistFlavor != QUDA_TWIST_NO && twistFlavor != QUDA_TWIST_INVALID) {
      strcpy(aux_tmp, aux_string);
      check = snprintf(aux_string, aux_string_n, "%s,TwistFlavour=%d", aux_tmp, twistFlavor);
      if (check < 0 || check >= aux_string_n) errorQuda("Error writing aux string");
    }
  }

  void ColorSpinorField::destroy() {
    init = false;
  }

  ColorSpinorField& ColorSpinorField::operator=(const ColorSpinorField &src) {
    if (&src != this) {
      create(src.nDim, src.x, src.nColor, src.nSpin, src.twistFlavor, 
	     src.precision, src.pad, src.siteSubset, 
	     src.siteOrder, src.fieldOrder, src.gammaBasis, src.PCtype, src.eigv_dim, src.eigv_id);    
    }
    return *this;
  }

  // Resets the attributes of this field if param disagrees (and is defined)
  void ColorSpinorField::reset(const ColorSpinorParam &param) {

    if (param.nColor != 0) nColor = param.nColor;
    if (param.nSpin != 0) nSpin = param.nSpin;
    if (param.twistFlavor != QUDA_TWIST_INVALID) twistFlavor = param.twistFlavor;

    if (param.PCtype != QUDA_PC_INVALID) PCtype = param.PCtype;

    if (param.precision != QUDA_INVALID_PRECISION)  precision = param.precision;
    if (param.nDim != 0) nDim = param.nDim;

    if (param.eigv_dim  != 0 ){
      eigv_dim     = param.eigv_dim;
      eigv_id      = param.eigv_id;
    }
    else
    {
      eigv_dim     = 0;
      eigv_id      = -1;
    }

    volume = 1;
    for (int d=0; d<nDim; d++) {
      if (param.x[0] != 0) x[d] = param.x[d];
      volume *= x[d];
    }
    volumeCB = siteSubset == QUDA_PARITY_SITE_SUBSET ? volume : volume/2;

  if((twistFlavor == QUDA_TWIST_NONDEG_DOUBLET || twistFlavor == QUDA_TWIST_DEG_DOUBLET) && x[4] != 2) errorQuda("Must be two flavors for non-degenerate twisted mass spinor (provided with %d)\n", x[4]);

  
    if (param.pad != 0) pad = param.pad;

    if (param.siteSubset == QUDA_FULL_SITE_SUBSET){
      stride = volume/2 + pad;
      length = 2*stride*nColor*nSpin*2;
    } else if (param.siteSubset == QUDA_PARITY_SITE_SUBSET){
      stride = volume + pad;
      length = stride*nColor*nSpin*2;  
    } else {
      //errorQuda("SiteSubset not defined %d", param.siteSubset);
      //do nothing, not an error (can't remember why - need to document this sometime! )
    }

    if (param.siteSubset != QUDA_INVALID_SITE_SUBSET) siteSubset = param.siteSubset;
    if (param.siteOrder != QUDA_INVALID_SITE_ORDER) siteOrder = param.siteOrder;
    if (param.fieldOrder != QUDA_INVALID_FIELD_ORDER) fieldOrder = param.fieldOrder;
    if (param.gammaBasis != QUDA_INVALID_GAMMA_BASIS) gammaBasis = param.gammaBasis;

    createGhostZone();

    real_length = volume*nColor*nSpin*2;

    bytes = total_length * precision; // includes pads and ghost zones
    bytes = (siteSubset == QUDA_FULL_SITE_SUBSET) ? 2*ALIGNMENT_ADJUST(bytes/2) : ALIGNMENT_ADJUST(bytes);

    if (precision == QUDA_HALF_PRECISION) {
      norm_bytes = total_norm_length * sizeof(float);
      norm_bytes = (siteSubset == QUDA_FULL_SITE_SUBSET) ? 2*ALIGNMENT_ADJUST(norm_bytes/2) : ALIGNMENT_ADJUST(norm_bytes);
    } else {
      norm_bytes = 0;
    }

//! for deflated solvers:
    if(eigv_dim > 0)
    {
       if(eigv_id == -1)
       {
          eigv_volume            = volume;
          eigv_stride            = stride;
          eigv_length            = length;
          eigv_real_length       = real_length;

          eigv_total_length      = total_length;
          eigv_total_norm_length = total_norm_length;

          eigv_ghost_length      = ghost_length;
          eigv_ghost_norm_length = ghost_norm_length;

          eigv_bytes             = bytes;
          eigv_norm_bytes        = norm_bytes;

          volume            *= eigv_dim;
          stride            *= eigv_dim;
          length            *= eigv_dim;
          real_length       *= eigv_dim;

          total_length      *= eigv_dim;
          total_norm_length *= eigv_dim;
          ghost_length      *= eigv_dim;
          ghost_norm_length *= eigv_dim;
       }
       else if(eigv_id > -1)
       {
 // just to be safe...
          eigv_volume            = 0;
          eigv_stride            = 0;
          eigv_length            = 0;
          eigv_real_length       = 0;

          eigv_total_length      = 0;
          eigv_total_norm_length = 0;

          eigv_ghost_length      = 0;
          eigv_ghost_norm_length = 0;  

          eigv_bytes             = 0;
          eigv_norm_bytes        = 0; 
       }
       else
       {
          errorQuda("\nIncorrect eigenvector index.\n");
       }
    }

    if (!init) errorQuda("Shouldn't be resetting a non-inited field\n");

    if (getVerbosity() >= QUDA_DEBUG_VERBOSE) {
      printfQuda("\nPrinting out reset field\n");
      std::cout << *this << std::endl;
      printfQuda("\n");
    }

    setTuningString();
  }

  // Fills the param with the contents of this field
  void ColorSpinorField::fill(ColorSpinorParam &param) const {
    param.nColor = nColor;
    param.nSpin = nSpin;
    param.twistFlavor = twistFlavor;
    param.precision = precision;
    param.nDim = nDim;
    param.eigv_dim = eigv_dim;
    param.eigv_id = eigv_id;
    memcpy(param.x, x, QUDA_MAX_DIM*sizeof(int));
    param.pad = pad;
    param.siteSubset = siteSubset;
    param.siteOrder = siteOrder;
    param.fieldOrder = fieldOrder;
    param.gammaBasis = gammaBasis;
    param.PCtype = PCtype;
    param.create = QUDA_INVALID_FIELD_CREATE;
  }

  // For kernels with precision conversion built in
  void ColorSpinorField::checkField(const ColorSpinorField &a, const ColorSpinorField &b) {
    if (a.Length() != b.Length()) {
      errorQuda("checkSpinor: lengths do not match: %d %d", a.Length(), b.Length());
    }

    if (a.Ncolor() != b.Ncolor()) {
      errorQuda("checkSpinor: colors do not match: %d %d", a.Ncolor(), b.Ncolor());
    }

    if (a.Nspin() != b.Nspin()) {
      errorQuda("checkSpinor: spins do not match: %d %d", a.Nspin(), b.Nspin());
    }

    if (a.TwistFlavor() != b.TwistFlavor()) {
      errorQuda("checkSpinor: twist flavors do not match: %d %d", a.TwistFlavor(), b.TwistFlavor());
    }
  }

  // Set the ghost pointers to NULL.
  // This is a private initialisation routine. 
  void ColorSpinorField::clearGhostPointers() 
  {
    for(int dim=0; dim<QUDA_MAX_DIM; ++dim){
      ghost[dim] = NULL;
      ghostNorm[dim] = NULL;
    }
  }


  void* ColorSpinorField::Ghost(const int i) {
    if(siteSubset != QUDA_PARITY_SITE_SUBSET) errorQuda("Site Subset %d is not supported",siteSubset);
    return ghost[i];
  }
  
  const void* ColorSpinorField::Ghost(const int i) const {
    if(siteSubset != QUDA_PARITY_SITE_SUBSET) errorQuda("Site Subset %d is not supported",siteSubset);
    return ghost[i];
  }


  void* ColorSpinorField::GhostNorm(const int i){
    if(siteSubset != QUDA_PARITY_SITE_SUBSET) errorQuda("Site Subset %d is not supported",siteSubset);
    return ghostNorm[i];
  }

  const void* ColorSpinorField::GhostNorm(const int i) const{
    if(siteSubset != QUDA_PARITY_SITE_SUBSET) errorQuda("Site Subset %d is not supported",siteSubset);
    return ghostNorm[i];
  }

  double norm2(const ColorSpinorField &a) {

    double rtn = 0.0;
    if (typeid(a) == typeid(cudaColorSpinorField)) {
      rtn = normCuda(dynamic_cast<const cudaColorSpinorField&>(a));
    } else if (typeid(a) == typeid(cpuColorSpinorField)) {
      rtn = normCpu(dynamic_cast<const cpuColorSpinorField&>(a));
    } else {
      errorQuda("Unknown input ColorSpinorField %s", typeid(a).name());
    }

    return rtn;
  }

  std::ostream& operator<<(std::ostream &out, const ColorSpinorField &a) {
    out << "typedid = " << typeid(a).name() << std::endl;
    out << "nColor = " << a.nColor << std::endl;
    out << "nSpin = " << a.nSpin << std::endl;
    out << "twistFlavor = " << a.twistFlavor << std::endl;
    out << "nDim = " << a.nDim << std::endl;
    for (int d=0; d<a.nDim; d++) out << "x[" << d << "] = " << a.x[d] << std::endl;
    out << "volume = " << a.volume << std::endl;
    out << "precision = " << a.precision << std::endl;
    out << "pad = " << a.pad << std::endl;
    out << "stride = " << a.stride << std::endl;
    out << "real_length = " << a.real_length << std::endl;
    out << "length = " << a.length << std::endl;
    out << "ghost_length = " << a.ghost_length << std::endl;
    out << "total_length = " << a.total_length << std::endl;
    out << "ghost_norm_length = " << a.ghost_norm_length << std::endl;
    out << "total_norm_length = " << a.total_norm_length << std::endl;
    out << "bytes = " << a.bytes << std::endl;
    out << "norm_bytes = " << a.norm_bytes << std::endl;
    out << "siteSubset = " << a.siteSubset << std::endl;
    out << "siteOrder = " << a.siteOrder << std::endl;
    out << "fieldOrder = " << a.fieldOrder << std::endl;
    out << "gammaBasis = " << a.gammaBasis << std::endl;
    out << "eigDim = " << a.eigv_dim << std::endl;
    out << "eigID = " << a.eigv_id << std::endl;
    out << "eigVolume = " << a.eigv_volume << std::endl;
    out << "eigStride = " << a.eigv_stride << std::endl;
    out << "eigLength = " << a.eigv_length << std::endl;
    out << "PC type = " << a.PCtype << std::endl;
    return out;
  }

} // namespace quda
