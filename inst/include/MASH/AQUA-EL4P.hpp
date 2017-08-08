////////////////////////////////////////////////////////////
//
//  MASH
//  AQUATIC ECOLOGY
//  Sean Wu
//  July 19, 2017
//
////////////////////////////////////////////////////////////

#ifndef _MASH_EL4P_HPP_
#define _MASH_EL4P_HPP_

#include <Rcpp.h>

namespace MASH {

// EL4P: struct to store the EL4P object for by genotype
struct EL4Pslot{
  EL4Pslot(const double &eggs_new,
    const double &L1_new,
    const double &L2_new,
    const double &L3_new,
    const double &L4_new,
    const double &P_new,
    const double &lambda_new,
    const int &genotype_new,
  );
  double eggs;    // eggs
  double L1;      // larval instar 1
  double L2;      // larval instar 2
  double L3;      // larval instar 3
  double L4;      // larval instar 4
  double lambda;  // emerging adults
  int genotype;   // genotype
};

// explicit constructor for EL4Pslot struct
inline EL4Pslot::EL4Pslot(const double &eggs_new,
  const double &L1_new,
  const double &L2_new,
  const double &L3_new,
  const double &L4_new,
  const double &P_new,
  const double &lambda_new,
  const int &genotype_new,
){
  eggs = eggs_new;
  L1 = L1_new;
  L2 = L2_new;
  L3 = L3_new;
  L4 = L4_new;
  P = P_new;
  lambda = lambda_new;
  genotype = genotype_new;
}

// EL4Pvector: store EL4Pslot structs by genotype
typedef std::vector<EL4Pslot> EL4Pvector;

// EL4P: EL4P class definition
class EL4P {
// public members
public:

  ///////////////////////////////////
  // EL4P Constructor
  ///////////////////////////////////

  // constructor defined below
  EL4P(const int &numGenotypes = 1);


  ///////////////////////////////////
  // EL4P daily difference equations
  ///////////////////////////////////

  // oneDay: run daily difference equations for EL4P pool
  void oneDay(){

    // total larval density in pool
    double D = 0;
    for(auto it = EL4Pvec.begin(); it != EL4Pvec.end(); it++){
      D += it->L1;
      D += it->L2;
      D += it->L3;
      D += it->L4;
    }

    // aquatic stage mortality
    double s1 = exp(-alpha);
    double s2 = exp(-(alpha + psi*D));

    // run daily difference equations for each genotype in EL4P pool
    for(int i=0; i<EL4Pvec.size(); i++){

      // initial larval sizes
      double L10 = EL4Pvec[i].L1;
      double L20 = EL4Pvec[i].L2;
      double L30 = EL4Pvec[i].L3;
      double L40 = EL4Pvec[i].L4;

      // difference equations
      EL4Pvec[i].lambda = s1*EL4Pvec[i].P; // P to lambda
      EL4Pvec[i].P = s2*p*L40; // L4 to P
      EL4Pvec[i].L4 = s2*(p*L30 + (1-p)*L40); // L3 to L4 (and L4 who do not advance)
      EL4Pvec[i].L3 = s2*(p*L20 + (1-p)*L30); // L2 to L3 (and L3 who do not advance)
      EL4Pvec[i].L2 = s2*(p*L10 + (1-p)*L20); // L1 to L2 (and L2 who do not advance)
      EL4Pvec[i].L1 = EL4Pvec[i].eggs + s2*(1-p)*L10; // eggs to L1 (and L1 who do not advance)
      EL4Pvec[i].eggs = 0.0;
    }

  };

  ///////////////////////////////////
  // Getters & Setters
  ///////////////////////////////////

  // return all genotypes
  Rcpp::List get_allGenotypes(){

  };

  // return a single genotype
  Rcpp::List get_genotypeIx(const int &ix){

  };

  // psi
  double get_psi(){

  };

  void set_psi(){

  };

  // alpha
  double get_alpha(){

  };

  void set_alpha(){

  };

  // p
  double get_p(){

  };

  void set_p(){

  };

  // number of genotypes
  int get_numGenotypes(){
    return(EL4P.size());
  };

// private members
private:

  EL4Pvector EL4Pvec; // vector of EL4Pslot objects (one for each genotype)
  double alpha; // density-dependent mortality independent of carrying capacity
  double psi; // density-dependent mortality dependent on carrying capacity
  double p; // expected fraction of cohort that advances to next life stage (1/p is expected time spent in stages L1,L2,L3,L4,P)

};

// inline definition of constructor to accept default argument values
inline EL4P::EL4P(const int &numGenotypes, const double &psi_new, const double &alpha_new, const double &p_new){

  EL4Pvec.reserve(numGenotypes);
  for(int i=0; i<numGenotypes; i++){
    EL4Pvec.push_back(EL4Pslot(0,0,0,0,0,0,0,i));
  }

  psi = psi_new;
  alpha = alpha_new;
  p = p_new;

}

}

#endif
