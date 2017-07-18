// Generated by RcppR6 (0.2.4): do not edit by hand
#include <MASH.h>

// [[Rcpp::export]]
MASH::HumanEventQ HumanEventQ__ctor(int initQ) {
  return MASH::HumanEventQ(initQ);
}
// [[Rcpp::export]]
Rcpp::List HumanEventQ__firstEvent(MASH::RcppR6::RcppR6<MASH::HumanEventQ> obj_) {
  return obj_->firstEvent();
}
// [[Rcpp::export]]
int HumanEventQ__firstTime(MASH::RcppR6::RcppR6<MASH::HumanEventQ> obj_) {
  return obj_->firstTime();
}
// [[Rcpp::export]]
void HumanEventQ__rmFirstEventFromQ(MASH::RcppR6::RcppR6<MASH::HumanEventQ> obj_) {
  obj_->rmFirstEventFromQ();
}
// [[Rcpp::export]]
void HumanEventQ__rmTagFromQ(MASH::RcppR6::RcppR6<MASH::HumanEventQ> obj_, std::string tag) {
  obj_->rmTagFromQ(tag);
}
// [[Rcpp::export]]
int HumanEventQ__get_queueN(MASH::RcppR6::RcppR6<MASH::HumanEventQ> obj_) {
  return obj_->get_queueN();
}
// [[Rcpp::export]]
void HumanEventQ__addEvent2Q(MASH::RcppR6::RcppR6<MASH::HumanEventQ> obj_, Rcpp::List event) {
  obj_->addEvent2Q(event);
}
// [[Rcpp::export]]
Rcpp::List HumanEventQ__get_EventQ(MASH::RcppR6::RcppR6<MASH::HumanEventQ> obj_) {
  return obj_->get_EventQ();
}

// [[Rcpp::export]]
MASH::HistoryGeneric HistoryGeneric__ctor(int N) {
  return MASH::HistoryGeneric(N);
}
// [[Rcpp::export]]
void HistoryGeneric__track_history(MASH::RcppR6::RcppR6<MASH::HistoryGeneric> obj_, double tEvent, std::string event) {
  obj_->track_history(tEvent, event);
}
// [[Rcpp::export]]
Rcpp::List HistoryGeneric__get_history(MASH::RcppR6::RcppR6<MASH::HistoryGeneric> obj_) {
  return obj_->get_history();
}

// [[Rcpp::export]]
MASH::HistoryTravel HistoryTravel__ctor(int N) {
  return MASH::HistoryTravel(N);
}
// [[Rcpp::export]]
void HistoryTravel__track_travel(MASH::RcppR6::RcppR6<MASH::HistoryTravel> obj_, double tTravel, int locationH) {
  obj_->track_travel(tTravel, locationH);
}
// [[Rcpp::export]]
Rcpp::List HistoryTravel__get_travelHistory(MASH::RcppR6::RcppR6<MASH::HistoryTravel> obj_) {
  return obj_->get_travelHistory();
}

// [[Rcpp::export]]
MASH::humanPfSIcpp humanPfSIcpp__ctor(int PfID_init, double tInf_init, double b_init, double c_init, int damID_init, int sireID_init, bool infected_init, bool chemoprophylaxis_init) {
  return MASH::humanPfSIcpp(PfID_init, tInf_init, b_init, c_init, damID_init, sireID_init, infected_init, chemoprophylaxis_init);
}
// [[Rcpp::export]]
std::vector<int> humanPfSIcpp__get_PfID(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_) {
  return obj_->get_PfID();
}
// [[Rcpp::export]]
void humanPfSIcpp__push_PfID(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_, int PfID_new) {
  obj_->push_PfID(PfID_new);
}
// [[Rcpp::export]]
std::vector<double> humanPfSIcpp__get_tInf(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_) {
  return obj_->get_tInf();
}
// [[Rcpp::export]]
void humanPfSIcpp__push_tInf(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_, double tInf_new) {
  obj_->push_tInf(tInf_new);
}
// [[Rcpp::export]]
double humanPfSIcpp__get_b(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_) {
  return obj_->get_b();
}
// [[Rcpp::export]]
void humanPfSIcpp__set_b(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_, double b_new) {
  obj_->set_b(b_new);
}
// [[Rcpp::export]]
double humanPfSIcpp__get_c(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_) {
  return obj_->get_c();
}
// [[Rcpp::export]]
void humanPfSIcpp__set_c(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_, double c_new) {
  obj_->set_c(c_new);
}
// [[Rcpp::export]]
std::vector<int> humanPfSIcpp__get_damID(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_) {
  return obj_->get_damID();
}
// [[Rcpp::export]]
void humanPfSIcpp__push_damID(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_, int damID_new) {
  obj_->push_damID(damID_new);
}
// [[Rcpp::export]]
std::vector<int> humanPfSIcpp__get_sireID(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_) {
  return obj_->get_sireID();
}
// [[Rcpp::export]]
void humanPfSIcpp__push_sireID(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_, int sireID_new) {
  obj_->push_sireID(sireID_new);
}
// [[Rcpp::export]]
bool humanPfSIcpp__get_infected(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_) {
  return obj_->get_infected();
}
// [[Rcpp::export]]
void humanPfSIcpp__set_infected(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_, bool infected_new) {
  obj_->set_infected(infected_new);
}
// [[Rcpp::export]]
bool humanPfSIcpp__get_chemoprophylaxis(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_) {
  return obj_->get_chemoprophylaxis();
}
// [[Rcpp::export]]
void humanPfSIcpp__set_chemoprophylaxis(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_, bool chemoprophylaxis_new) {
  obj_->set_chemoprophylaxis(chemoprophylaxis_new);
}
// [[Rcpp::export]]
void humanPfSIcpp__track_history(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_, double tEvent, std::string event) {
  obj_->track_history(tEvent, event);
}
// [[Rcpp::export]]
Rcpp::List humanPfSIcpp__get_history(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_) {
  return obj_->get_history();
}

// [[Rcpp::export]]
MASH::RiskQ RiskQ__ctor() {
  return MASH::RiskQ();
}
// [[Rcpp::export]]
int RiskQ__get_N(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_) {
  return obj_->get_N();
}
// [[Rcpp::export]]
void RiskQ__set_N(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_, int N_new) {
  obj_->set_N(N_new);
}
// [[Rcpp::export]]
std::vector<int> RiskQ__get_who(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_) {
  return obj_->get_who();
}
// [[Rcpp::export]]
void RiskQ__push_who(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_, int who_new) {
  obj_->push_who(who_new);
}
// [[Rcpp::export]]
std::vector<double> RiskQ__get_pTm(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_) {
  return obj_->get_pTm();
}
// [[Rcpp::export]]
void RiskQ__push_pTm(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_, double pTm_new) {
  obj_->push_pTm(pTm_new);
}
// [[Rcpp::export]]
std::vector<double> RiskQ__get_w(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_) {
  return obj_->get_w();
}
// [[Rcpp::export]]
void RiskQ__push_w(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_, double w_new) {
  obj_->push_w(w_new);
}
// [[Rcpp::export]]
void RiskQ__add_HumanHost(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_, int who_new, double pTm_new, double w_new) {
  obj_->add_HumanHost(who_new, pTm_new, w_new);
}
// [[Rcpp::export]]
Rcpp::List RiskQ__get_HumanHost(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_) {
  return obj_->get_HumanHost();
}
// [[Rcpp::export]]
int RiskQ__get_nOther(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_) {
  return obj_->get_nOther();
}
// [[Rcpp::export]]
void RiskQ__set_nOther(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_, int nOther_new) {
  obj_->set_nOther(nOther_new);
}
// [[Rcpp::export]]
void RiskQ__add_OtherHost(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_, double otherW_new, int typeID_new) {
  obj_->add_OtherHost(otherW_new, typeID_new);
}
// [[Rcpp::export]]
Rcpp::List RiskQ__get_OtherHost(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_) {
  return obj_->get_OtherHost();
}

// [[Rcpp::export]]
MASH::ImagoQ ImagoQ__ctor() {
  return MASH::ImagoQ();
}
// [[Rcpp::export]]
void ImagoQ__clear_ImagoQ(MASH::RcppR6::RcppR6<MASH::ImagoQ> obj_) {
  obj_->clear_ImagoQ();
}
// [[Rcpp::export]]
void ImagoQ__add_ImagoQ(MASH::RcppR6::RcppR6<MASH::ImagoQ> obj_, int N_new, double tEmerge_new, int genotype_new, int damID_new, int sireID_new) {
  obj_->add_ImagoQ(N_new, tEmerge_new, genotype_new, damID_new, sireID_new);
}
// [[Rcpp::export]]
double ImagoQ__track_ImagoQ(MASH::RcppR6::RcppR6<MASH::ImagoQ> obj_, double time) {
  return obj_->track_ImagoQ(time);
}
// [[Rcpp::export]]
int ImagoQ__get_N(MASH::RcppR6::RcppR6<MASH::ImagoQ> obj_) {
  return obj_->get_N();
}
// [[Rcpp::export]]
void ImagoQ__set_N(MASH::RcppR6::RcppR6<MASH::ImagoQ> obj_, int N_new) {
  obj_->set_N(N_new);
}
// [[Rcpp::export]]
Rcpp::List ImagoQ__get_ImagoQ(MASH::RcppR6::RcppR6<MASH::ImagoQ> obj_) {
  return obj_->get_ImagoQ();
}


