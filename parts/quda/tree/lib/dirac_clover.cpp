#include <iostream>
#include <dirac_quda.h>
#include <blas_quda.h>

namespace quda {

  namespace clover {
#include <dslash_init.cuh>
  }

  namespace asym_clover {
#include <dslash_init.cuh>
  }

  DiracClover::DiracClover(const DiracParam &param)
    : DiracWilson(param), clover(*(param.clover))
  {
    clover::initConstants(*param.gauge, profile);
    asym_clover::initConstants(*param.gauge, profile);
#ifdef DYNAMIC_CLOVER
    warningQuda("Dynamic clover generation/inversion is currently not supported for pure Wilson-Clover dslash.\n");
#endif
  }

  DiracClover::DiracClover(const DiracClover &dirac) 
    : DiracWilson(dirac), clover(dirac.clover)
  {
    clover::initConstants(dirac.gauge, profile);
    asym_clover::initConstants(dirac.gauge, profile);
#ifdef DYNAMIC_CLOVER
    warningQuda("Dynamic clover generation/inversion is currently not supported for pure Wilson-Clover dslash.\n");
#endif
  }

  DiracClover::~DiracClover() { }

  DiracClover& DiracClover::operator=(const DiracClover &dirac)
  {
    if (&dirac != this) {
      DiracWilson::operator=(dirac);
      clover = dirac.clover;
    }
    return *this;
  }

  void DiracClover::checkParitySpinor(const cudaColorSpinorField &out, const cudaColorSpinorField &in) const
  {
    Dirac::checkParitySpinor(out, in);

    if (out.Volume() != clover.VolumeCB()) {
      errorQuda("Parity spinor volume %d doesn't match clover checkboard volume %d",
		out.Volume(), clover.VolumeCB());
    }
  }

  /** Applies the operator (A + k D) */
  void DiracClover::DslashXpay(cudaColorSpinorField &out, const cudaColorSpinorField &in, 
			       const QudaParity parity, const cudaColorSpinorField &x,
			       const double &k) const
  {
    asym_clover::setFace(face1,face2); // FIXME: temporary hack maintain C linkage for dslashCuda

    checkParitySpinor(in, out);
    checkSpinorAlias(in, out);

    FullClover cs(clover);
    asymCloverDslashCuda(&out, gauge, cs, &in, parity, dagger, &x, k, commDim, profile);

    flops += 1872ll*in.Volume();
  }

  // Public method to apply the clover term only
  void DiracClover::Clover(cudaColorSpinorField &out, const cudaColorSpinorField &in, const QudaParity parity) const
  {
    checkParitySpinor(in, out);

    // regular clover term
    FullClover cs(clover);
    cloverCuda(&out, gauge, cs, &in, parity);

    flops += 504ll*in.Volume();
  }

  void DiracClover::M(cudaColorSpinorField &out, const cudaColorSpinorField &in) const
  {
    checkFullSpinor(out, in);
    DslashXpay(out.Odd(), in.Even(), QUDA_ODD_PARITY, in.Odd(), -kappa);
    DslashXpay(out.Even(), in.Odd(), QUDA_EVEN_PARITY, in.Even(), -kappa);
  }

  void DiracClover::MdagM(cudaColorSpinorField &out, const cudaColorSpinorField &in) const
  {
    checkFullSpinor(out, in);

    bool reset = newTmp(&tmp1, in);
    checkFullSpinor(*tmp1, in);

    M(*tmp1, in);
    Mdag(out, *tmp1);

    deleteTmp(&tmp1, reset);
  }

  void DiracClover::prepare(cudaColorSpinorField* &src, cudaColorSpinorField* &sol,
			    cudaColorSpinorField &x, cudaColorSpinorField &b, 
			    const QudaSolutionType solType) const
  {
    if (solType == QUDA_MATPC_SOLUTION || solType == QUDA_MATPCDAG_MATPC_SOLUTION) {
      errorQuda("Preconditioned solution requires a preconditioned solve_type");
    }

    src = &b;
    sol = &x;
  }

  void DiracClover::reconstruct(cudaColorSpinorField &x, const cudaColorSpinorField &b,
				const QudaSolutionType solType) const
  {
    // do nothing
  }

  DiracCloverPC::DiracCloverPC(const DiracParam &param) : 
    DiracClover(param)
  {
    // For the preconditioned operator, we need to check that the inverse of the clover term is present
    if (!clover.cloverInv) errorQuda("Clover inverse required for DiracCloverPC");
  }

  DiracCloverPC::DiracCloverPC(const DiracCloverPC &dirac) : DiracClover(dirac) { }

  DiracCloverPC::~DiracCloverPC() { }

  DiracCloverPC& DiracCloverPC::operator=(const DiracCloverPC &dirac)
  {
    if (&dirac != this) {
      DiracClover::operator=(dirac);
    }
    return *this;
  }

  // Public method
  void DiracCloverPC::CloverInv(cudaColorSpinorField &out, const cudaColorSpinorField &in, 
				const QudaParity parity) const
  {
    checkParitySpinor(in, out);

    // needs to be cloverinv
    FullClover cs(clover, true);
    cloverCuda(&out, gauge, cs, &in, parity);

    flops += 504ll*in.Volume();
  }

  // apply hopping term, then clover: (A_ee^-1 D_eo) or (A_oo^-1 D_oe),
  // and likewise for dagger: (A_ee^-1 D^dagger_eo) or (A_oo^-1 D^dagger_oe)
  // NOTE - this isn't Dslash dagger since order should be reversed!
  void DiracCloverPC::Dslash(cudaColorSpinorField &out, const cudaColorSpinorField &in, 
			     const QudaParity parity) const
  {
    clover::setFace(face1,face2); // FIXME: temporary hack maintain C linkage for dslashCuda

    checkParitySpinor(in, out);
    checkSpinorAlias(in, out);

    FullClover cs(clover, true);
    cloverDslashCuda(&out, gauge, cs, &in, parity, dagger, 0, 0.0, commDim, profile);

    flops += 1824ll*in.Volume();
  }

  // xpay version of the above
  void DiracCloverPC::DslashXpay(cudaColorSpinorField &out, const cudaColorSpinorField &in, 
				 const QudaParity parity, const cudaColorSpinorField &x,
				 const double &k) const
  {
    clover::setFace(face1,face2); // FIXME: temporary hack maintain C linkage for dslashCuda

    checkParitySpinor(in, out);
    checkSpinorAlias(in, out);

    FullClover cs(clover, true);
    cloverDslashCuda(&out, gauge, cs, &in, parity, dagger, &x, k, commDim, profile);

    flops += 1872ll*in.Volume();
  }

  // Apply the even-odd preconditioned clover-improved Dirac operator
  void DiracCloverPC::M(cudaColorSpinorField &out, const cudaColorSpinorField &in) const
  {
    double kappa2 = -kappa*kappa;
    bool reset1 = newTmp(&tmp1, in);

    if (matpcType == QUDA_MATPC_EVEN_EVEN_ASYMMETRIC) {
      // DiracCloverPC::Dslash applies A^{-1}Dslash
      Dslash(*tmp1, in, QUDA_ODD_PARITY);
      // DiracClover::DslashXpay applies (A - kappa^2 D)
      DiracClover::DslashXpay(out, *tmp1, QUDA_EVEN_PARITY, in, kappa2);
    } else if (matpcType == QUDA_MATPC_ODD_ODD_ASYMMETRIC) {
      // DiracCloverPC::Dslash applies A^{-1}Dslash
      Dslash(*tmp1, in, QUDA_EVEN_PARITY);
      // DiracClover::DslashXpay applies (A - kappa^2 D)
      DiracClover::DslashXpay(out, *tmp1, QUDA_ODD_PARITY, in, kappa2);
    } else if (!dagger) { // symmetric preconditioning
      if (matpcType == QUDA_MATPC_EVEN_EVEN) {
	Dslash(*tmp1, in, QUDA_ODD_PARITY);
	DslashXpay(out, *tmp1, QUDA_EVEN_PARITY, in, kappa2); 
      } else if (matpcType == QUDA_MATPC_ODD_ODD) {
	Dslash(*tmp1, in, QUDA_EVEN_PARITY);
	DslashXpay(out, *tmp1, QUDA_ODD_PARITY, in, kappa2); 
      } else {
	errorQuda("Invalid matpcType");
      }
    } else { // symmetric preconditioning, dagger
      if (matpcType == QUDA_MATPC_EVEN_EVEN) {
	CloverInv(out, in, QUDA_EVEN_PARITY); 
	Dslash(*tmp1, out, QUDA_ODD_PARITY);
	DiracWilson::DslashXpay(out, *tmp1, QUDA_EVEN_PARITY, in, kappa2); 
      } else if (matpcType == QUDA_MATPC_ODD_ODD) {
	CloverInv(out, in, QUDA_ODD_PARITY); 
	Dslash(*tmp1, out, QUDA_EVEN_PARITY);
	DiracWilson::DslashXpay(out, *tmp1, QUDA_ODD_PARITY, in, kappa2); 
      } else {
	errorQuda("MatPCType %d not valid for DiracCloverPC", matpcType);
      }
    }
  
    deleteTmp(&tmp1, reset1);
  }

  void DiracCloverPC::MdagM(cudaColorSpinorField &out, const cudaColorSpinorField &in) const
  {
    // need extra temporary because of symmetric preconditioning dagger
    // and for multi-gpu the input and output fields cannot alias
    bool reset = newTmp(&tmp2, in);
    M(*tmp2, in);
    Mdag(out, *tmp2);
    deleteTmp(&tmp2, reset);
  }

  void DiracCloverPC::prepare(cudaColorSpinorField* &src, cudaColorSpinorField* &sol, 
			      cudaColorSpinorField &x, cudaColorSpinorField &b, 
			      const QudaSolutionType solType) const
  {
    // we desire solution to preconditioned system
    if (solType == QUDA_MATPC_SOLUTION || solType == QUDA_MATPCDAG_MATPC_SOLUTION) {
      src = &b;
      sol = &x;
      return;
    }

    bool reset = newTmp(&tmp1, b.Even());
  
    // we desire solution to full system
    if (matpcType == QUDA_MATPC_EVEN_EVEN) {
      // src = A_ee^-1 (b_e + k D_eo A_oo^-1 b_o)
      src = &(x.Odd());
      CloverInv(*src, b.Odd(), QUDA_ODD_PARITY);
      DiracWilson::DslashXpay(*tmp1, *src, QUDA_EVEN_PARITY, b.Even(), kappa);
      CloverInv(*src, *tmp1, QUDA_EVEN_PARITY);
      sol = &(x.Even());
    } else if (matpcType == QUDA_MATPC_ODD_ODD) {
      // src = A_oo^-1 (b_o + k D_oe A_ee^-1 b_e)
      src = &(x.Even());
      CloverInv(*src, b.Even(), QUDA_EVEN_PARITY);
      DiracWilson::DslashXpay(*tmp1, *src, QUDA_ODD_PARITY, b.Odd(), kappa);
      CloverInv(*src, *tmp1, QUDA_ODD_PARITY);
      sol = &(x.Odd());
    } else if (matpcType == QUDA_MATPC_EVEN_EVEN_ASYMMETRIC) {
      // src = b_e + k D_eo A_oo^-1 b_o
      src = &(x.Odd());
      CloverInv(*tmp1, b.Odd(), QUDA_ODD_PARITY); // safe even when *tmp1 = b.odd
      DiracWilson::DslashXpay(*src, *tmp1, QUDA_EVEN_PARITY, b.Even(), kappa);
      sol = &(x.Even());
    } else if (matpcType == QUDA_MATPC_ODD_ODD_ASYMMETRIC) {
      // src = b_o + k D_oe A_ee^-1 b_e
      src = &(x.Even());
      CloverInv(*tmp1, b.Even(), QUDA_EVEN_PARITY); // safe even when *tmp1 = b.even
      DiracWilson::DslashXpay(*src, *tmp1, QUDA_ODD_PARITY, b.Odd(), kappa);
      sol = &(x.Odd());
    } else {
      errorQuda("MatPCType %d not valid for DiracCloverPC", matpcType);
    }

    // here we use final solution to store parity solution and parity source
    // b is now up for grabs if we want

    deleteTmp(&tmp1, reset);

  }

  void DiracCloverPC::reconstruct(cudaColorSpinorField &x, const cudaColorSpinorField &b,
				  const QudaSolutionType solType) const
  {
    if (solType == QUDA_MATPC_SOLUTION || solType == QUDA_MATPCDAG_MATPC_SOLUTION) {
      return;
    }

    checkFullSpinor(x, b);

    bool reset = newTmp(&tmp1, b.Even());

    // create full solution

    if (matpcType == QUDA_MATPC_EVEN_EVEN ||
	matpcType == QUDA_MATPC_EVEN_EVEN_ASYMMETRIC) {
      // x_o = A_oo^-1 (b_o + k D_oe x_e)
      DiracWilson::DslashXpay(*tmp1, x.Even(), QUDA_ODD_PARITY, b.Odd(), kappa);
      CloverInv(x.Odd(), *tmp1, QUDA_ODD_PARITY);
    } else if (matpcType == QUDA_MATPC_ODD_ODD ||
	       matpcType == QUDA_MATPC_ODD_ODD_ASYMMETRIC) {
      // x_e = A_ee^-1 (b_e + k D_eo x_o)
      DiracWilson::DslashXpay(*tmp1, x.Odd(), QUDA_EVEN_PARITY, b.Even(), kappa);
      CloverInv(x.Even(), *tmp1, QUDA_EVEN_PARITY);
    } else {
      errorQuda("MatPCType %d not valid for DiracCloverPC", matpcType);
    }

    deleteTmp(&tmp1, reset);

  }

} // namespace quda
