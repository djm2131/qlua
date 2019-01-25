#include <dirac_quda.h>
#include <dslash_quda.h>
#include <blas_quda.h>

#include <iostream>

namespace quda {

  // FIXME: At the moment, it's unsafe for more than one Dirac operator to be active unless
  // they all have the same volume, etc. (used to initialize the various CUDA constants).

  Dirac::Dirac(const DiracParam &param) 
    : gauge(*(param.gauge)), kappa(param.kappa), mass(param.mass), matpcType(param.matpcType), 
      dagger(param.dagger), flops(0), tmp1(param.tmp1), tmp2(param.tmp2), tune(QUDA_TUNE_NO),
      profile("Dirac", false)
  {
    for (int i=0; i<4; i++) commDim[i] = param.commDim[i];
  }

  Dirac::Dirac(const Dirac &dirac) 
    : gauge(dirac.gauge), kappa(dirac.kappa), matpcType(dirac.matpcType), 
      dagger(dirac.dagger), flops(0), tmp1(dirac.tmp1), tmp2(dirac.tmp2), tune(QUDA_TUNE_NO),
      profile("Dirac", false)
  {
    for (int i=0; i<4; i++) commDim[i] = dirac.commDim[i];
  }

  Dirac::~Dirac() {   
    if (getVerbosity() > QUDA_VERBOSE) profile.Print();
  }

  Dirac& Dirac::operator=(const Dirac &dirac)
  {
    if(&dirac != this) {
      gauge = dirac.gauge;
      kappa = dirac.kappa;
      matpcType = dirac.matpcType;
      dagger = dirac.dagger;
      flops = 0;
      tmp1 = dirac.tmp1;
      tmp2 = dirac.tmp2;
      tune = dirac.tune;

      for (int i=0; i<4; i++) commDim[i] = dirac.commDim[i];

      profile = dirac.profile;
    }
    return *this;
  }

  bool Dirac::newTmp(cudaColorSpinorField **tmp, const cudaColorSpinorField &a) const {
    if (*tmp) return false;
    ColorSpinorParam param(a);
    param.create = QUDA_ZERO_FIELD_CREATE; // need to zero elements else padded region will be junk
    *tmp = new cudaColorSpinorField(a, param);
    return true;
  }

  void Dirac::deleteTmp(cudaColorSpinorField **a, const bool &reset) const {
    if (reset) {
      delete *a;
      *a = NULL;
    }
  }

#define flip(x) (x) = ((x) == QUDA_DAG_YES ? QUDA_DAG_NO : QUDA_DAG_YES)

  void Dirac::Mdag(cudaColorSpinorField &out, const cudaColorSpinorField &in) const
  {
    flip(dagger);
    M(out, in);
    flip(dagger);
  }

  void Dirac::MMdag(cudaColorSpinorField &out, const cudaColorSpinorField &in) const
  {
    flip(dagger);
    MdagM(out, in);
    flip(dagger);
  }

#undef flip

  void Dirac::checkParitySpinor(const cudaColorSpinorField &out, const cudaColorSpinorField &in) const
  {
    if ( (in.GammaBasis() != QUDA_UKQCD_GAMMA_BASIS || out.GammaBasis() != QUDA_UKQCD_GAMMA_BASIS) && 
	 in.Nspin() == 4) {
      errorQuda("CUDA Dirac operator requires UKQCD basis, out = %d, in = %d", 
		out.GammaBasis(), in.GammaBasis());
    }

    if (in.Precision() != out.Precision()) {
      errorQuda("Input precision %d and output spinor precision %d don't match in dslash_quda",
		in.Precision(), out.Precision());
    }

    if (in.Stride() != out.Stride()) {
      errorQuda("Input %d and output %d spinor strides don't match in dslash_quda", 
		in.Stride(), out.Stride());
    }

    if (in.SiteSubset() != QUDA_PARITY_SITE_SUBSET || out.SiteSubset() != QUDA_PARITY_SITE_SUBSET) {
      errorQuda("ColorSpinorFields are not single parity: in = %d, out = %d", 
		in.SiteSubset(), out.SiteSubset());
    }

    if (out.Ndim() != 5) {
      if ((out.Volume() != gauge.Volume() && out.SiteSubset() == QUDA_FULL_SITE_SUBSET) ||
	  (out.Volume() != gauge.VolumeCB() && out.SiteSubset() == QUDA_PARITY_SITE_SUBSET) ) {
	errorQuda("Spinor volume %d doesn't match gauge volume %d", out.Volume(), gauge.VolumeCB());
      }
    } else {
      // Domain wall fermions, compare 4d volumes not 5d
      if ((out.Volume()/out.X(4) != gauge.Volume() && out.SiteSubset() == QUDA_FULL_SITE_SUBSET) ||
	  (out.Volume()/out.X(4) != gauge.VolumeCB() && out.SiteSubset() == QUDA_PARITY_SITE_SUBSET) ) {
	errorQuda("Spinor volume %d doesn't match gauge volume %d", out.Volume(), gauge.VolumeCB());
      }
    }
  }

  void Dirac::checkFullSpinor(const cudaColorSpinorField &out, const cudaColorSpinorField &in) const
  {
    if (in.SiteSubset() != QUDA_FULL_SITE_SUBSET || out.SiteSubset() != QUDA_FULL_SITE_SUBSET) {
      errorQuda("ColorSpinorFields are not full fields: in = %d, out = %d", 
		in.SiteSubset(), out.SiteSubset());
    } 
  }

  void Dirac::checkSpinorAlias(const cudaColorSpinorField &a, const cudaColorSpinorField &b) const {
    if (a.V() == b.V()) errorQuda("Aliasing pointers");
  }

  // Dirac operator factory
  Dirac* Dirac::create(const DiracParam &param)
  {
    if (param.type == QUDA_WILSON_DIRAC) {
      if (getVerbosity() >= QUDA_VERBOSE) printfQuda("Creating a DiracWilson operator\n");
      return new DiracWilson(param);
    } else if (param.type == QUDA_WILSONPC_DIRAC) {
      if (getVerbosity() >= QUDA_VERBOSE) printfQuda("Creating a DiracWilsonPC operator\n");
      return new DiracWilsonPC(param);
    } else if (param.type == QUDA_CLOVER_DIRAC) {
      if (getVerbosity() >= QUDA_VERBOSE) printfQuda("Creating a DiracClover operator\n");
      return new DiracClover(param);
    } else if (param.type == QUDA_CLOVERPC_DIRAC) {
      if (getVerbosity() >= QUDA_VERBOSE) printfQuda("Creating a DiracCloverPC operator\n");
      return new DiracCloverPC(param);
    } else if (param.type == QUDA_DOMAIN_WALL_DIRAC) {
      if (getVerbosity() >= QUDA_VERBOSE) printfQuda("Creating a DiracDomainWall operator\n");
      return new DiracDomainWall(param);
    } else if (param.type == QUDA_DOMAIN_WALLPC_DIRAC) {
      if (getVerbosity() >= QUDA_VERBOSE) printfQuda("Creating a DiracDomainWallPC operator\n");
      return new DiracDomainWallPC(param);
    } else if (param.type == QUDA_DOMAIN_WALL_4DPC_DIRAC) {
      if (getVerbosity() >= QUDA_VERBOSE) printfQuda("Creating a DiracDomainWall4DPC operator\n");
      return new DiracDomainWall4DPC(param);
    } else if (param.type == QUDA_MOBIUS_DOMAIN_WALLPC_DIRAC) {
      if (getVerbosity() >= QUDA_VERBOSE) printfQuda("Creating a DiracMobiusDomainWallPC operator\n");
      return new DiracMobiusDomainWallPC(param);
    } else if (param.type == QUDA_STAGGERED_DIRAC) {
      if (getVerbosity() >= QUDA_VERBOSE) printfQuda("Creating a DiracStaggered operator\n");
      return new DiracStaggered(param);
    } else if (param.type == QUDA_STAGGEREDPC_DIRAC) {
      if (getVerbosity() >= QUDA_VERBOSE) printfQuda("Creating a DiracStaggeredPC operator\n");
      return new DiracStaggeredPC(param);    
    } else if (param.type == QUDA_ASQTAD_DIRAC) {
      if (getVerbosity() >= QUDA_VERBOSE) printfQuda("Creating a DiracImprovedStaggered operator\n");
      return new DiracImprovedStaggered(param);
    } else if (param.type == QUDA_ASQTADPC_DIRAC) {
      if (getVerbosity() >= QUDA_VERBOSE) printfQuda("Creating a DiracImprovedStaggeredPC operator\n");
      return new DiracImprovedStaggeredPC(param);    
    } else if (param.type == QUDA_TWISTED_CLOVER_DIRAC) {
      if (getVerbosity() >= QUDA_VERBOSE) printfQuda("Creating a DiracTwistedClover operator (%d flavor(s))\n", param.Ls);
      if (param.Ls == 1) {
	return new DiracTwistedClover(param, 4);
      } else { 
	errorQuda("Cannot create DiracTwistedClover operator for %d flavors\n", param.Ls);
      }
    } else if (param.type == QUDA_TWISTED_CLOVERPC_DIRAC) {
      if (getVerbosity() >= QUDA_VERBOSE) printfQuda("Creating a DiracTwistedCloverPC operator (%d flavor(s))\n", param.Ls);
      if (param.Ls == 1) {
	return new DiracTwistedCloverPC(param, 4);
      } else {
	errorQuda("Cannot create DiracTwistedCloverPC operator for %d flavors\n", param.Ls);
      }
    } else if (param.type == QUDA_TWISTED_MASS_DIRAC) {
      if (getVerbosity() >= QUDA_VERBOSE) printfQuda("Creating a DiracTwistedMass operator (%d flavor(s))\n", param.Ls);
        if (param.Ls == 1) return new DiracTwistedMass(param, 4);
        else return new DiracTwistedMass(param, 5);
    } else if (param.type == QUDA_TWISTED_MASSPC_DIRAC) {
        if (getVerbosity() >= QUDA_VERBOSE) printfQuda("Creating a DiracTwistedMassPC operator (%d flavor(s))\n", param.Ls);
        if (param.Ls == 1) return new DiracTwistedMassPC(param, 4);
        else return new DiracTwistedMassPC(param, 5);
    }

    return 0;
  }

} // namespace quda
