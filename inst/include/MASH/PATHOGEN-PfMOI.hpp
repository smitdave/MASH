#ifndef _MASH_PFMOI_HPP_
#define _MASH_PFMOI_HPP_

#include <Rcpp.h>

namespace MASH {

// Human-stage PfMOI Object
class humanPfMOIcpp {
// public members
public:

  ///////////////////////////////////
  // Human Stage PfMOI Constructor
  ///////////////////////////////////

  humanPfMOIcpp(const int &PfID_init, const double &tInf_init = -1, const int &MOI_init = 0,
    const double &b_init = 0.55, const double &c_init = 0.15,
    const int &damID_init = -1, const int &sireID_init = -1,
    const bool &chemoprophylaxis_init = false);

  ///////////////////////////////////
  // Getters & Setters
  ///////////////////////////////////

  std::vector<int> get_PfID(){
    return(PfID);
  };
  void push_PfID(const int &PfID_new){
    PfID.push_back(PfID_new);
  };

  std::vector<double> get_tInf(){
    return(tInf);
  };
  void push_tInf(const double &tInf_new){
    tInf.push_back(tInf_new);
  };

  int get_MOI(){
    return(MOI);
  };
  void set_MOI(const int &MOI_new){
    MOI = MOI_new;
  };

  double get_b(){
    return(b);
  };
  void set_b(const double &b_new){
    b = b_new;
  };

  double get_c(){
    return(c);
  };
  void set_c(const double &c_new){
    c = c_new;
  };

  std::vector<int> get_damID(){
    return(damID);
  };
  void push_damID(const int &damID_new){
    damID.push_back(damID_new);
  };

  std::vector<int> get_sireID(){
    return(sireID);
  };
  void push_sireID(const int &sireID_new){
    sireID.push_back(sireID_new);
  };

  bool get_chemoprophylaxis(){
    return(chemoprophylaxis);
  };
  void set_chemoprophylaxis(const bool &chemoprophylaxis_new){
    chemoprophylaxis = chemoprophylaxis_new;
  };

  ///////////////////////////////////
  // Infection Dynamics
  ///////////////////////////////////

  // add a new infection
  void add_Infection(const int &PfID_new, const int &damID_new, const int &sireID_new){
    PfID.push_back(PfID_new);
    damID.push_back(damID_new);
    sireID.push_back(sireID_new);
    MOI += 1;
  };

  // completely clear the infection associated with index ix
  void clear_Infection(const int &PfID_ix){
    // find infection associated with this PfID
    auto it = std::find(PfID.begin(), PfID.end(), PfID_ix);
    size_t ix = std::distance(PfID.begin(), it);
    PfID.erase(PfID.begin()+ix);
    damID.erase(damID.begin()+ix);
    sireID.erase(sireID.begin()+ix);
    MOI -= 1;
  };

  ///////////////////////////////////
  // PfMOI History
  ///////////////////////////////////

  // history tracking
  void track_history(const double &tEvent, const std::string &event){
    events.push_back(event);
    eventT.push_back(tEvent);
    MOI_history.push_back(MOI);
  };

  // return history
  Rcpp::List get_history(){
    return(
      Rcpp::List::create(
        Rcpp::Named("events") = events,
        Rcpp::Named("eventT") = eventT,
        Rcpp::Named("MOI") = MOI_history;
      )
    );
  };

// private members
private:

  // PfMOI History
  std::vector<std::string> events;
  std::vector<double>      eventT;
  std::vector<int>         MOI_history;

  // PfMOI Parameters & State Variables
  std::vector<int> PfID; // pathogen ID
  std::vector<double> tInf; // time of infection (mosquito to human transmission)
  int MOI; // multiplicity of infection
  double b; // transmission efficiency: infected mosquito to human
  double c; // transmission efficiency: infected human to mosquito
  std::vector<int> damID; // female gametocyte mother
  std::vector<int> sireID; // male gametocyte father
  bool chemoprophylaxis;

};

// inline definition of constructor to accept default argument values
inline humanPfMOIcpp::humanPfMOIcpp(const int &PfID_init, const double &tInf_init,
  const int &MOI_init, const double &b_init, const double &c_init,
  const int &damID_init, const int &sireID_init,
  const bool &chemoprophylaxis_init){

    // set parameters and state variables
    PfID.push_back(PfID_init);
    tInf.push_back(tInf_init);
    b = b_init;
    c = c_init;
    damID.push_back(damID_init);
    sireID.push_back(sireID_init);
    infected = infected_init;
    chemoprophylaxis = chemoprophylaxis_init;

    // reserve memory for history
    events.reserve(50);
    events.push_back("init");
    eventT.reserve(50);
    eventT.push_back(-1);
    MOI_history.reserve(50);

  }

// Mosquito-stage PfMOI Object
class mosquitoPfSIcpp {
// public members
public:

  ///////////////////////////////////
  // Mosquito Stage PfMOI Constructor
  ///////////////////////////////////

  mosquitoPfSIcpp(const int &PfID_init, const double &tInf_init = -1,
    const int &MOI_init = 0;
    const int &damID_init = -1, const int &sireID_init = -1);

  ///////////////////////////////////
  // Getters & Setters
  ///////////////////////////////////

  int get_PfID(){
    return(PfID);
  };
  void push_PfID(const int &PfID_new){
    PfID.push_back(PfID_new);
  }

  double get_tInf(){
    return(tInf);
  };
  void push_tInf(const double &tInf_new){
    tInf.push_back(tInf_new);
  };

  double get_MOI(){
    return(MOI);
  };
  void set_MOI(const int &MOI_new){
    MOI = MOI_new;
  }

  int get_damID(){
    return(damID);
  };
  void push_damID(const int &damID_new){
    damID.push_back(damID_new);
  };

  int get_sireID(){
    return(sireID);
  };
  void push_sireID(const int &sireID_new){
    sireID.push_back(sireID_new);
  };

  ///////////////////////////////////
  // Infection Dynamics
  ///////////////////////////////////

  // return the clonal variant associated with given PfID
  Rcpp::List get_Infection(const int &PfID_ix){
    auto it = std::find(PfID.begin(), PfID.end(), PfID_ix);
    size_t ix = std::distance(PfID.begin(), it);
    return(
      Rcpp::List::create(
        Rcpp::Named("PfID") = PfID[ix],
        Rcpp::Named("damID") = damID[ix],
        Rcpp::Named("sireID") = sireID[ix]
      )
    );
  };

  // get_InfectionEIP: argument 'incubation' = tNow - EIP; only return infections that started at tNow - EIP in the past
  // because only those can possibly have passed the EIP and produced sporozoites.
  Rcpp::List get_InfectionEIP(const double &incubation){

    // find infections where tInf < tNow - EIP (incubation)
    std::vector<int> incubationIx;
    auto it = std::find_if(tInf.begin(), tInf.end(), [incubation](const double &infectionTime){
        return(infectionTime < incubation);
    });
    while(it != tInf.end()){
        incubationIx.emplace_back(std::distance(tInf.begin(), it));
        it = std::find_if(std::next(it), std::end(tInf), [incubation](const double &infectionTime){
            return(infectionTime < incubation);
        });
    }

    // export these infections that have passed the EIP
    std::vector<int> PfID_out(incubationIx.size());
    std::vector<int> damID_out(incubationIx.size());
    std::vector<int> sireID_out(incubationIx.size());
    std::transform(incubationIx.begin(), incubationIx.end(), std::back_inserter(PfID_out), [PfID](size_t ix){
      return(PfID[pos]);
    });
    std::transform(incubationIx.begin(), incubationIx.end(), std::back_inserter(damID_out), [damID](size_t ix){
      return(damID[pos]);
    });
    std::transform(incubationIx.begin(), incubationIx.end(), std::back_inserter(sireID_out), [sireID](size_t ix){
      return(sireID[pos]);
    });

    return(
      Rcpp::List::create(
        Rcpp::Named("PfID") = PfID_out,
        Rcpp::Named("damID") = damID_out,
        Rcpp::Named("sireID") = sireID_out
      )
    );
  };

// private members
private:

  // PfMOI Parameters & State Variables
  std::vector<int>              PfID;
  std::vector<double>           tInf;
  int                           MOI;
  std::vector<int>              damID;
  std::vector<int>              sireID;

};

// inline definition of constructor to accept default argument values
inline mosquitoPfSI::mosquitoPfSI(const int &PfID_init, const double &tInf_init,
  const int &damID_init, const int &sireID_init, const bool &infected_init){

    // set parameters and state variables
    PfID = PfID_init;
    tInf = tInf_init;
    damID = damID_init;
    sireID = sireID_init;
    infected = infected_init;

  }


}

#endif
