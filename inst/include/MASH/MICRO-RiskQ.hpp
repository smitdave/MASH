#ifndef _MASH_RISKQ_HPP_
#define _MASH_RISKQ_HPP_

#include <Rcpp.h>

namespace MASH {

// // OtherHost: struct to store information for non-human hosts
// inline struct OtherHost{
//   OtherHost(const double &w_init, const int &typeID_init);
//   double w;
//   int typeID;
// };
//
// inline OtherHost::OtherHost(const double &w_init, const int &typeID_init){
//   w = w_init;
//   typeID = typeID_init;
// }
//
// // OtherHostVector: store OtherHost structs
// inline typedef std::vector<OtherHost> OtherHostVector;

// RiskQ: Risk Queue class definition
class RiskQ {
// public members
public:

  ///////////////////////////////////
  // Risk Queue Constructor
  ///////////////////////////////////

  // constructor defined below
  RiskQ();


  ///////////////////////////////////
  // Getters & Setters
  ///////////////////////////////////

  int get_N(){
    return(N);
  };
  void set_N(const int &N_new){
    N = N_new;
  };

  std::vector<int> get_who(){
    return(who);
  };
  void push_who(const int &who_new){
    who.push_back(who_new);
  };

  std::vector<double> get_pTm(){
    return(pTm);
  };
  void push_pTm(const double &pTm_new){
    pTm.push_back(pTm_new);
  };

  std::vector<double> get_w(){
    return(w);
  };
  void push_w(const double &w_new){
    w.push_back(w_new);
  };


  ///////////////////////////////////
  // Human Hosts
  ///////////////////////////////////

  void add_HumanHost(const int &who_new, const double &pTm_new, const double &w_new){

    // chcek if new host is already in this RiskQ
    std::vector<int>::iterator it = find(who.begin(), who.end(), who_new);
    if(it != who.end()){
      // host is already in this RiskQ
      int ix = std::distance(who.begin(), it);
      who[ix] = who_new;
      pTm[ix] = pTm_new;
      w[ix] = w_new;
    } else {
      // host is new to this RiskQ
      N += 1;
      who.push_back(who_new);
      pTm.push_back(pTm_new);
      w.push_back(w_new);
    }


  };

  ///////////////////////////////////
  // Other Hosts
  ///////////////////////////////////

  int get_nOther(){
    return(nOther);
  };
  void set_nOther(const int &nOther_new){
    nOther = nOther_new;
  };

  // add_OtherHost:
  void add_OtherHost(const double &otherW_new, const int &typeID_new){
    nOther += 1;
    otherW.push_back(otherW_new);
    typeID.push_back(typeID_new);
  };

  // get_OtherHost
  Rcpp::List get_OtherHost(){
    return(
      Rcpp::List::create(
        Rcpp::Named("otherW") = otherW,
        Rcpp::Named("typeID") = typeID
      )
    );
  };

// private members
private:

  // RiskQ elements
  int N; // number of humans in the risk queue
  std::vector<int> who; // who: ix of humans in risk queue
  std::vector<double> pTm; // pTm: person time at risk
  std::vector<double> w; // w: biting weight on hosts

  // other hosts
  int nOther;
  std::vector<double> otherW;
  std::vector<int> typeID;
  // OtherHostVector OtherHosts; // vector of non-human hosts

};

// inline definition of constructor to accept default argument values
inline RiskQ::RiskQ(){

  N = 0;
  who.reserve(50);
  pTm.reserve(50);
  w.reserve(50);

  nOther = 0;
  otherW.reserve(5);
  typeID.reserve(5);
  // OtherHosts.reserve(5);
}

}

#endif
