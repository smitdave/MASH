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
MASH::humanPfSI humanPfSI__ctor(int PfID_init, double tInf_init, double b_init, double c_init, int damID_init, int sireID_init, bool infected_init, bool chemoprophylaxis_init) {
  return MASH::humanPfSI(PfID_init, tInf_init, b_init, c_init, damID_init, sireID_init, infected_init, chemoprophylaxis_init);
}
// [[Rcpp::export]]
std::vector<int> humanPfSI__get_PfID(MASH::RcppR6::RcppR6<MASH::humanPfSI> obj_) {
  return obj_->get_PfID();
}
// [[Rcpp::export]]
void humanPfSI__push_PfID(MASH::RcppR6::RcppR6<MASH::humanPfSI> obj_, int PfID_new) {
  obj_->push_PfID(PfID_new);
}
// [[Rcpp::export]]
int humanPfSI__back_PfID(MASH::RcppR6::RcppR6<MASH::humanPfSI> obj_) {
  return obj_->back_PfID();
}
// [[Rcpp::export]]
std::vector<double> humanPfSI__get_tInf(MASH::RcppR6::RcppR6<MASH::humanPfSI> obj_) {
  return obj_->get_tInf();
}
// [[Rcpp::export]]
void humanPfSI__push_tInf(MASH::RcppR6::RcppR6<MASH::humanPfSI> obj_, double tInf_new) {
  obj_->push_tInf(tInf_new);
}
// [[Rcpp::export]]
double humanPfSI__get_b(MASH::RcppR6::RcppR6<MASH::humanPfSI> obj_) {
  return obj_->get_b();
}
// [[Rcpp::export]]
void humanPfSI__set_b(MASH::RcppR6::RcppR6<MASH::humanPfSI> obj_, double b_new) {
  obj_->set_b(b_new);
}
// [[Rcpp::export]]
double humanPfSI__get_c(MASH::RcppR6::RcppR6<MASH::humanPfSI> obj_) {
  return obj_->get_c();
}
// [[Rcpp::export]]
void humanPfSI__set_c(MASH::RcppR6::RcppR6<MASH::humanPfSI> obj_, double c_new) {
  obj_->set_c(c_new);
}
// [[Rcpp::export]]
std::vector<int> humanPfSI__get_damID(MASH::RcppR6::RcppR6<MASH::humanPfSI> obj_) {
  return obj_->get_damID();
}
// [[Rcpp::export]]
void humanPfSI__push_damID(MASH::RcppR6::RcppR6<MASH::humanPfSI> obj_, int damID_new) {
  obj_->push_damID(damID_new);
}
// [[Rcpp::export]]
std::vector<int> humanPfSI__get_sireID(MASH::RcppR6::RcppR6<MASH::humanPfSI> obj_) {
  return obj_->get_sireID();
}
// [[Rcpp::export]]
void humanPfSI__push_sireID(MASH::RcppR6::RcppR6<MASH::humanPfSI> obj_, int sireID_new) {
  obj_->push_sireID(sireID_new);
}
// [[Rcpp::export]]
bool humanPfSI__get_infected(MASH::RcppR6::RcppR6<MASH::humanPfSI> obj_) {
  return obj_->get_infected();
}
// [[Rcpp::export]]
void humanPfSI__set_infected(MASH::RcppR6::RcppR6<MASH::humanPfSI> obj_, bool infected_new) {
  obj_->set_infected(infected_new);
}
// [[Rcpp::export]]
bool humanPfSI__get_chemoprophylaxis(MASH::RcppR6::RcppR6<MASH::humanPfSI> obj_) {
  return obj_->get_chemoprophylaxis();
}
// [[Rcpp::export]]
void humanPfSI__set_chemoprophylaxis(MASH::RcppR6::RcppR6<MASH::humanPfSI> obj_, bool chemoprophylaxis_new) {
  obj_->set_chemoprophylaxis(chemoprophylaxis_new);
}
// [[Rcpp::export]]
void humanPfSI__track_history(MASH::RcppR6::RcppR6<MASH::humanPfSI> obj_, double tEvent, std::string event) {
  obj_->track_history(tEvent, event);
}
// [[Rcpp::export]]
Rcpp::List humanPfSI__get_history(MASH::RcppR6::RcppR6<MASH::humanPfSI> obj_) {
  return obj_->get_history();
}

// [[Rcpp::export]]
MASH::mosquitoPfSI mosquitoPfSI__ctor(int PfID_init, double tInf_init, int damID_init, int sireID_init, bool infected_init) {
  return MASH::mosquitoPfSI(PfID_init, tInf_init, damID_init, sireID_init, infected_init);
}
// [[Rcpp::export]]
int mosquitoPfSI__get_PfID(MASH::RcppR6::RcppR6<MASH::mosquitoPfSI> obj_) {
  return obj_->get_PfID();
}
// [[Rcpp::export]]
void mosquitoPfSI__set_PfID(MASH::RcppR6::RcppR6<MASH::mosquitoPfSI> obj_, int PfID_new) {
  obj_->set_PfID(PfID_new);
}
// [[Rcpp::export]]
double mosquitoPfSI__get_tInf(MASH::RcppR6::RcppR6<MASH::mosquitoPfSI> obj_) {
  return obj_->get_tInf();
}
// [[Rcpp::export]]
void mosquitoPfSI__set_tInf(MASH::RcppR6::RcppR6<MASH::mosquitoPfSI> obj_, double tInf_new) {
  obj_->set_tInf(tInf_new);
}
// [[Rcpp::export]]
int mosquitoPfSI__get_damID(MASH::RcppR6::RcppR6<MASH::mosquitoPfSI> obj_) {
  return obj_->get_damID();
}
// [[Rcpp::export]]
void mosquitoPfSI__set_damID(MASH::RcppR6::RcppR6<MASH::mosquitoPfSI> obj_, int damID_new) {
  obj_->set_damID(damID_new);
}
// [[Rcpp::export]]
int mosquitoPfSI__get_sireID(MASH::RcppR6::RcppR6<MASH::mosquitoPfSI> obj_) {
  return obj_->get_sireID();
}
// [[Rcpp::export]]
void mosquitoPfSI__set_sireID(MASH::RcppR6::RcppR6<MASH::mosquitoPfSI> obj_, int sireID_new) {
  obj_->set_sireID(sireID_new);
}
// [[Rcpp::export]]
bool mosquitoPfSI__get_infected(MASH::RcppR6::RcppR6<MASH::mosquitoPfSI> obj_) {
  return obj_->get_infected();
}
// [[Rcpp::export]]
void mosquitoPfSI__set_infected(MASH::RcppR6::RcppR6<MASH::mosquitoPfSI> obj_, bool infected_new) {
  obj_->set_infected(infected_new);
}

// [[Rcpp::export]]
MASH::humanPfMOI humanPfMOI__ctor(int PfID_init, double tInf_init, int MOI_init, double b_init, double c_init, int damID_init, int sireID_init, bool chemoprophylaxis_init) {
  return MASH::humanPfMOI(PfID_init, tInf_init, MOI_init, b_init, c_init, damID_init, sireID_init, chemoprophylaxis_init);
}
// [[Rcpp::export]]
std::vector<int> humanPfMOI__get_PfID(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_) {
  return obj_->get_PfID();
}
// [[Rcpp::export]]
void humanPfMOI__push_PfID(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_, int PfID_new) {
  obj_->push_PfID(PfID_new);
}
// [[Rcpp::export]]
std::vector<double> humanPfMOI__get_tInf(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_) {
  return obj_->get_tInf();
}
// [[Rcpp::export]]
void humanPfMOI__push_tInf(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_, double tInf_new) {
  obj_->push_tInf(tInf_new);
}
// [[Rcpp::export]]
int humanPfMOI__get_MOI(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_) {
  return obj_->get_MOI();
}
// [[Rcpp::export]]
void humanPfMOI__set_MOI(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_, int MOI_new) {
  obj_->set_MOI(MOI_new);
}
// [[Rcpp::export]]
double humanPfMOI__get_b(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_) {
  return obj_->get_b();
}
// [[Rcpp::export]]
void humanPfMOI__set_b(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_, double b_new) {
  obj_->set_b(b_new);
}
// [[Rcpp::export]]
double humanPfMOI__get_c(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_) {
  return obj_->get_c();
}
// [[Rcpp::export]]
void humanPfMOI__set_c(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_, double c_new) {
  obj_->set_c(c_new);
}
// [[Rcpp::export]]
std::vector<int> humanPfMOI__get_damID(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_) {
  return obj_->get_damID();
}
// [[Rcpp::export]]
void humanPfMOI__push_damID(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_, int damID_new) {
  obj_->push_damID(damID_new);
}
// [[Rcpp::export]]
std::vector<int> humanPfMOI__get_sireID(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_) {
  return obj_->get_sireID();
}
// [[Rcpp::export]]
void humanPfMOI__push_sireID(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_, int sireID_new) {
  obj_->push_sireID(sireID_new);
}
// [[Rcpp::export]]
bool humanPfMOI__get_chemoprophylaxis(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_) {
  return obj_->get_chemoprophylaxis();
}
// [[Rcpp::export]]
void humanPfMOI__set_chemoprophylaxis(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_, bool chemoprophylaxis_new) {
  obj_->set_chemoprophylaxis(chemoprophylaxis_new);
}
// [[Rcpp::export]]
void humanPfMOI__add_Infection(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_, int PfID_new, int damID_new, int sireID_new) {
  obj_->add_Infection(PfID_new, damID_new, sireID_new);
}
// [[Rcpp::export]]
void humanPfMOI__clear_Infection(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_, int PfID_ix) {
  obj_->clear_Infection(PfID_ix);
}
// [[Rcpp::export]]
Rcpp::List humanPfMOI__get_Infection(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_) {
  return obj_->get_Infection();
}
// [[Rcpp::export]]
void humanPfMOI__track_history(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_, double tEvent, std::string event) {
  obj_->track_history(tEvent, event);
}
// [[Rcpp::export]]
Rcpp::List humanPfMOI__get_history(MASH::RcppR6::RcppR6<MASH::humanPfMOI> obj_) {
  return obj_->get_history();
}

// [[Rcpp::export]]
MASH::mosquitoPfMOI mosquitoPfMOI__ctor(int PfID_init, double tInf_init, int MOI_init, int damID_init, int sireID_init) {
  return MASH::mosquitoPfMOI(PfID_init, tInf_init, MOI_init, damID_init, sireID_init);
}
// [[Rcpp::export]]
std::vector<int> mosquitoPfMOI__get_PfID(MASH::RcppR6::RcppR6<MASH::mosquitoPfMOI> obj_) {
  return obj_->get_PfID();
}
// [[Rcpp::export]]
void mosquitoPfMOI__push_PfID(MASH::RcppR6::RcppR6<MASH::mosquitoPfMOI> obj_, int PfID_new) {
  obj_->push_PfID(PfID_new);
}
// [[Rcpp::export]]
std::vector<double> mosquitoPfMOI__get_tInf(MASH::RcppR6::RcppR6<MASH::mosquitoPfMOI> obj_) {
  return obj_->get_tInf();
}
// [[Rcpp::export]]
void mosquitoPfMOI__push_tInf(MASH::RcppR6::RcppR6<MASH::mosquitoPfMOI> obj_, double tInf_new) {
  obj_->push_tInf(tInf_new);
}
// [[Rcpp::export]]
int mosquitoPfMOI__get_MOI(MASH::RcppR6::RcppR6<MASH::mosquitoPfMOI> obj_) {
  return obj_->get_MOI();
}
// [[Rcpp::export]]
void mosquitoPfMOI__set_MOI(MASH::RcppR6::RcppR6<MASH::mosquitoPfMOI> obj_, int MOI_new) {
  obj_->set_MOI(MOI_new);
}
// [[Rcpp::export]]
std::vector<int> mosquitoPfMOI__get_damID(MASH::RcppR6::RcppR6<MASH::mosquitoPfMOI> obj_) {
  return obj_->get_damID();
}
// [[Rcpp::export]]
void mosquitoPfMOI__push_damID(MASH::RcppR6::RcppR6<MASH::mosquitoPfMOI> obj_, int damID_new) {
  obj_->push_damID(damID_new);
}
// [[Rcpp::export]]
std::vector<int> mosquitoPfMOI__get_sireID(MASH::RcppR6::RcppR6<MASH::mosquitoPfMOI> obj_) {
  return obj_->get_sireID();
}
// [[Rcpp::export]]
void mosquitoPfMOI__push_sireID(MASH::RcppR6::RcppR6<MASH::mosquitoPfMOI> obj_, int sireID_new) {
  obj_->push_sireID(sireID_new);
}
// [[Rcpp::export]]
void mosquitoPfMOI__add_Infection(MASH::RcppR6::RcppR6<MASH::mosquitoPfMOI> obj_, int PfID_new, double tInf_new, int damID_new, int sireID_new) {
  obj_->add_Infection(PfID_new, tInf_new, damID_new, sireID_new);
}
// [[Rcpp::export]]
Rcpp::List mosquitoPfMOI__get_Infection(MASH::RcppR6::RcppR6<MASH::mosquitoPfMOI> obj_, int PfID_ix) {
  return obj_->get_Infection(PfID_ix);
}
// [[Rcpp::export]]
Rcpp::List mosquitoPfMOI__get_InfectionIx(MASH::RcppR6::RcppR6<MASH::mosquitoPfMOI> obj_, int ix) {
  return obj_->get_InfectionIx(ix);
}
// [[Rcpp::export]]
Rcpp::List mosquitoPfMOI__get_InfectionEIP(MASH::RcppR6::RcppR6<MASH::mosquitoPfMOI> obj_, double incubation) {
  return obj_->get_InfectionEIP(incubation);
}
// [[Rcpp::export]]
std::vector<int> mosquitoPfMOI__which_EIP(MASH::RcppR6::RcppR6<MASH::mosquitoPfMOI> obj_, double incubation) {
  return obj_->which_EIP(incubation);
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
Rcpp::List RiskQ__get_HumanHostIx(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_, int ix) {
  return obj_->get_HumanHostIx(ix);
}
// [[Rcpp::export]]
void RiskQ__clear_HumanHost(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_) {
  obj_->clear_HumanHost();
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
void ImagoQ__clear_ImagoQTime(MASH::RcppR6::RcppR6<MASH::ImagoQ> obj_, double time) {
  obj_->clear_ImagoQTime(time);
}
// [[Rcpp::export]]
void ImagoQ__add_ImagoQ(MASH::RcppR6::RcppR6<MASH::ImagoQ> obj_, int N_new, double tEmerge_new, int genotype_new, std::string damID_new, std::string sireID_new) {
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
// [[Rcpp::export]]
Rcpp::List ImagoQ__get_ImagoQTime(MASH::RcppR6::RcppR6<MASH::ImagoQ> obj_, double tNow, bool clear) {
  return obj_->get_ImagoQTime(tNow, clear);
}

// [[Rcpp::export]]
MASH::EggQ EggQ__ctor() {
  return MASH::EggQ();
}
// [[Rcpp::export]]
void EggQ__clear_EggQ(MASH::RcppR6::RcppR6<MASH::EggQ> obj_) {
  obj_->clear_EggQ();
}
// [[Rcpp::export]]
void EggQ__clear_EggQTime(MASH::RcppR6::RcppR6<MASH::EggQ> obj_, double time) {
  obj_->clear_EggQTime(time);
}
// [[Rcpp::export]]
void EggQ__add_EggQ(MASH::RcppR6::RcppR6<MASH::EggQ> obj_, int N_new, double tOviposit_new, int genotype_new, std::string damID_new, std::string sireID_new) {
  obj_->add_EggQ(N_new, tOviposit_new, genotype_new, damID_new, sireID_new);
}
// [[Rcpp::export]]
double EggQ__track_EggQ(MASH::RcppR6::RcppR6<MASH::EggQ> obj_, double time) {
  return obj_->track_EggQ(time);
}
// [[Rcpp::export]]
int EggQ__get_N(MASH::RcppR6::RcppR6<MASH::EggQ> obj_) {
  return obj_->get_N();
}
// [[Rcpp::export]]
void EggQ__set_N(MASH::RcppR6::RcppR6<MASH::EggQ> obj_, int N_new) {
  obj_->set_N(N_new);
}
// [[Rcpp::export]]
Rcpp::List EggQ__get_EggQ(MASH::RcppR6::RcppR6<MASH::EggQ> obj_) {
  return obj_->get_EggQ();
}
// [[Rcpp::export]]
Rcpp::List EggQ__get_EggQTime(MASH::RcppR6::RcppR6<MASH::EggQ> obj_, double tNow, bool clear) {
  return obj_->get_EggQTime(tNow, clear);
}

// [[Rcpp::export]]
MASH::EL4P EL4P__ctor(int numGenotypes, double psi_new, double alpha_new, double p_new) {
  return MASH::EL4P(numGenotypes, psi_new, alpha_new, p_new);
}
// [[Rcpp::export]]
void EL4P__oneStep(MASH::RcppR6::RcppR6<MASH::EL4P> obj_) {
  obj_->oneStep();
}
// [[Rcpp::export]]
Rcpp::List EL4P__get_allGenotypes(MASH::RcppR6::RcppR6<MASH::EL4P> obj_) {
  return obj_->get_allGenotypes();
}
// [[Rcpp::export]]
Rcpp::List EL4P__get_genotypeIx(MASH::RcppR6::RcppR6<MASH::EL4P> obj_, int ix) {
  return obj_->get_genotypeIx(ix);
}
// [[Rcpp::export]]
double EL4P__get_psi(MASH::RcppR6::RcppR6<MASH::EL4P> obj_) {
  return obj_->get_psi();
}
// [[Rcpp::export]]
void EL4P__set_psi(MASH::RcppR6::RcppR6<MASH::EL4P> obj_, double psi_new) {
  obj_->set_psi(psi_new);
}
// [[Rcpp::export]]
double EL4P__get_alpha(MASH::RcppR6::RcppR6<MASH::EL4P> obj_) {
  return obj_->get_alpha();
}
// [[Rcpp::export]]
void EL4P__set_alpha(MASH::RcppR6::RcppR6<MASH::EL4P> obj_, double alpha_new) {
  obj_->set_alpha(alpha_new);
}
// [[Rcpp::export]]
double EL4P__get_p(MASH::RcppR6::RcppR6<MASH::EL4P> obj_) {
  return obj_->get_p();
}
// [[Rcpp::export]]
void EL4P__set_p(MASH::RcppR6::RcppR6<MASH::EL4P> obj_, double p_new) {
  obj_->set_p(p_new);
}
// [[Rcpp::export]]
int EL4P__get_numGenotypes(MASH::RcppR6::RcppR6<MASH::EL4P> obj_) {
  return obj_->get_numGenotypes();
}

// [[Rcpp::export]]
MASH::MosquitoFemaleHistory MosquitoFemaleHistory__ctor() {
  return MASH::MosquitoFemaleHistory();
}
// [[Rcpp::export]]
void MosquitoFemaleHistory__historyInit(MASH::RcppR6::RcppR6<MASH::MosquitoFemaleHistory> obj_, Rcpp::Environment privateEnv) {
  obj_->historyInit(privateEnv);
}
// [[Rcpp::export]]
void MosquitoFemaleHistory__historyTrack(MASH::RcppR6::RcppR6<MASH::MosquitoFemaleHistory> obj_, Rcpp::Environment privateEnv, bool alive) {
  obj_->historyTrack(privateEnv, alive);
}
// [[Rcpp::export]]
void MosquitoFemaleHistory__historyFeed(MASH::RcppR6::RcppR6<MASH::MosquitoFemaleHistory> obj_, Rcpp::Environment privateEnv) {
  obj_->historyFeed(privateEnv);
}
// [[Rcpp::export]]
void MosquitoFemaleHistory__calcBionomics(MASH::RcppR6::RcppR6<MASH::MosquitoFemaleHistory> obj_) {
  obj_->calcBionomics();
}
// [[Rcpp::export]]
Rcpp::List MosquitoFemaleHistory__exportHistory(MASH::RcppR6::RcppR6<MASH::MosquitoFemaleHistory> obj_) {
  return obj_->exportHistory();
}


