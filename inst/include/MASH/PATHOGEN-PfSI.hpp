#ifndef _MASH_PFSI_HPP_
#define _MASH_PFSI_HPP_

#include <Rcpp.h>

namespace MASH {

class humanPfSIcpp {
public:

  // constructor
  // humanPfSIcpp(const int &PfID_init, const double &tInf_init = -1,
  //   const double &b_init = 0.55, const double &c_init = 0.15,
  //   const int &damID_init = -1, const int &sireID_init = -1,
  //   const bool &infected_init = false, const bool &chemoprophylaxis_init = false){
  //
  //     // set parameters and state variables
  //     PfID = PfID_init;
  //     tInf = tInf_init;
  //     b = b_init;
  //     c = c_init;
  //     damID = damID_init;
  //     sireID = sireID_init;
  //     infected = infected_init;
  //     chemoprophylaxis = chemoprophylaxis_init;
  //
  //     // reserve memory for history
  //     events.reserve(50);
  //     events.push_back("init");
  //     eventT.reserve(50);
  //     eventT.push_back(-1);
  // }

  humanPfSIcpp(const int &PfID_init, const double &tInf_init = -1,
    const double &b_init = 0.55, const double &c_init = 0.15,
    const int &damID_init = -1, const int &sireID_init = -1,
    const bool &infected_init = false, const bool &chemoprophylaxis_init = false);

  // history tracking
  void track_history(const double &tEvent, const std::string &event){
    events.push_back(event);
    eventT.push_back(tEvent);
  };

  // return history
  Rcpp::List get_history(){
    return(
      Rcpp::List::create(
        Rcpp::Named("events") = events,
        Rcpp::Named("eventT") = eventT
      )
    );
  };

private:

  // PfSI History
  std::vector<std::string> events;
  std::vector<double>      eventT;

  // PfSI Parameters & State Variables
  int PfID; // pathogen ID
  double tInf; // time of infection (mosquito to human transmission)
  double b; // transmission efficiency: infected mosquito to human
  double c; // transmission efficiency: infected human to mosquito
  int damID; // female gametocyte mother
  int sireID; // male gametocyte father
  bool infected;
  bool chemoprophylaxis;

};

inline humanPfSIcpp::humanPfSIcpp(const int &PfID_init, const double &tInf_init,
  const double &b_init, const double &c_init,
  const int &damID_init, const int &sireID_init,
  const bool &infected_init, const bool &chemoprophylaxis_init){

    // set parameters and state variables
    PfID = PfID_init;
    tInf = tInf_init;
    b = b_init;
    c = c_init;
    damID = damID_init;
    sireID = sireID_init;
    infected = infected_init;
    chemoprophylaxis = chemoprophylaxis_init;

    // reserve memory for history
    events.reserve(50);
    events.push_back("init");
    eventT.reserve(50);
    eventT.push_back(-1);

  }

}

#endif
