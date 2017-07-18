// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/MASH.h"
#include <Rcpp.h>

using namespace Rcpp;

// util_PfSIoneHistory
void util_PfSIoneHistory(const Rcpp::List& historyIxH, const NumericVector& timeBins, NumericMatrix timeSeries);
RcppExport SEXP _MASH_util_PfSIoneHistory(SEXP historyIxHSEXP, SEXP timeBinsSEXP, SEXP timeSeriesSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::List& >::type historyIxH(historyIxHSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type timeBins(timeBinsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type timeSeries(timeSeriesSEXP);
    util_PfSIoneHistory(historyIxH, timeBins, timeSeries);
    return R_NilValue;
END_RCPP
}
// HumanEventQ__ctor
MASH::HumanEventQ HumanEventQ__ctor(int initQ);
RcppExport SEXP _MASH_HumanEventQ__ctor(SEXP initQSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type initQ(initQSEXP);
    rcpp_result_gen = Rcpp::wrap(HumanEventQ__ctor(initQ));
    return rcpp_result_gen;
END_RCPP
}
// HumanEventQ__firstEvent
Rcpp::List HumanEventQ__firstEvent(MASH::RcppR6::RcppR6<MASH::HumanEventQ> obj_);
RcppExport SEXP _MASH_HumanEventQ__firstEvent(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::HumanEventQ> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(HumanEventQ__firstEvent(obj_));
    return rcpp_result_gen;
END_RCPP
}
// HumanEventQ__firstTime
int HumanEventQ__firstTime(MASH::RcppR6::RcppR6<MASH::HumanEventQ> obj_);
RcppExport SEXP _MASH_HumanEventQ__firstTime(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::HumanEventQ> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(HumanEventQ__firstTime(obj_));
    return rcpp_result_gen;
END_RCPP
}
// HumanEventQ__rmFirstEventFromQ
void HumanEventQ__rmFirstEventFromQ(MASH::RcppR6::RcppR6<MASH::HumanEventQ> obj_);
RcppExport SEXP _MASH_HumanEventQ__rmFirstEventFromQ(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::HumanEventQ> >::type obj_(obj_SEXP);
    HumanEventQ__rmFirstEventFromQ(obj_);
    return R_NilValue;
END_RCPP
}
// HumanEventQ__rmTagFromQ
void HumanEventQ__rmTagFromQ(MASH::RcppR6::RcppR6<MASH::HumanEventQ> obj_, std::string tag);
RcppExport SEXP _MASH_HumanEventQ__rmTagFromQ(SEXP obj_SEXP, SEXP tagSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::HumanEventQ> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< std::string >::type tag(tagSEXP);
    HumanEventQ__rmTagFromQ(obj_, tag);
    return R_NilValue;
END_RCPP
}
// HumanEventQ__get_queueN
int HumanEventQ__get_queueN(MASH::RcppR6::RcppR6<MASH::HumanEventQ> obj_);
RcppExport SEXP _MASH_HumanEventQ__get_queueN(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::HumanEventQ> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(HumanEventQ__get_queueN(obj_));
    return rcpp_result_gen;
END_RCPP
}
// HumanEventQ__addEvent2Q
void HumanEventQ__addEvent2Q(MASH::RcppR6::RcppR6<MASH::HumanEventQ> obj_, Rcpp::List event);
RcppExport SEXP _MASH_HumanEventQ__addEvent2Q(SEXP obj_SEXP, SEXP eventSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::HumanEventQ> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type event(eventSEXP);
    HumanEventQ__addEvent2Q(obj_, event);
    return R_NilValue;
END_RCPP
}
// HumanEventQ__get_EventQ
Rcpp::List HumanEventQ__get_EventQ(MASH::RcppR6::RcppR6<MASH::HumanEventQ> obj_);
RcppExport SEXP _MASH_HumanEventQ__get_EventQ(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::HumanEventQ> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(HumanEventQ__get_EventQ(obj_));
    return rcpp_result_gen;
END_RCPP
}
// HistoryGeneric__ctor
MASH::HistoryGeneric HistoryGeneric__ctor(int N);
RcppExport SEXP _MASH_HistoryGeneric__ctor(SEXP NSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    rcpp_result_gen = Rcpp::wrap(HistoryGeneric__ctor(N));
    return rcpp_result_gen;
END_RCPP
}
// HistoryGeneric__track_history
void HistoryGeneric__track_history(MASH::RcppR6::RcppR6<MASH::HistoryGeneric> obj_, double tEvent, std::string event);
RcppExport SEXP _MASH_HistoryGeneric__track_history(SEXP obj_SEXP, SEXP tEventSEXP, SEXP eventSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::HistoryGeneric> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< double >::type tEvent(tEventSEXP);
    Rcpp::traits::input_parameter< std::string >::type event(eventSEXP);
    HistoryGeneric__track_history(obj_, tEvent, event);
    return R_NilValue;
END_RCPP
}
// HistoryGeneric__get_history
Rcpp::List HistoryGeneric__get_history(MASH::RcppR6::RcppR6<MASH::HistoryGeneric> obj_);
RcppExport SEXP _MASH_HistoryGeneric__get_history(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::HistoryGeneric> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(HistoryGeneric__get_history(obj_));
    return rcpp_result_gen;
END_RCPP
}
// HistoryTravel__ctor
MASH::HistoryTravel HistoryTravel__ctor(int N);
RcppExport SEXP _MASH_HistoryTravel__ctor(SEXP NSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    rcpp_result_gen = Rcpp::wrap(HistoryTravel__ctor(N));
    return rcpp_result_gen;
END_RCPP
}
// HistoryTravel__track_travel
void HistoryTravel__track_travel(MASH::RcppR6::RcppR6<MASH::HistoryTravel> obj_, double tTravel, int locationH);
RcppExport SEXP _MASH_HistoryTravel__track_travel(SEXP obj_SEXP, SEXP tTravelSEXP, SEXP locationHSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::HistoryTravel> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< double >::type tTravel(tTravelSEXP);
    Rcpp::traits::input_parameter< int >::type locationH(locationHSEXP);
    HistoryTravel__track_travel(obj_, tTravel, locationH);
    return R_NilValue;
END_RCPP
}
// HistoryTravel__get_travelHistory
Rcpp::List HistoryTravel__get_travelHistory(MASH::RcppR6::RcppR6<MASH::HistoryTravel> obj_);
RcppExport SEXP _MASH_HistoryTravel__get_travelHistory(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::HistoryTravel> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(HistoryTravel__get_travelHistory(obj_));
    return rcpp_result_gen;
END_RCPP
}
// humanPfSIcpp__ctor
MASH::humanPfSIcpp humanPfSIcpp__ctor(int PfID_init, double tInf_init, double b_init, double c_init, int damID_init, int sireID_init, bool infected_init, bool chemoprophylaxis_init);
RcppExport SEXP _MASH_humanPfSIcpp__ctor(SEXP PfID_initSEXP, SEXP tInf_initSEXP, SEXP b_initSEXP, SEXP c_initSEXP, SEXP damID_initSEXP, SEXP sireID_initSEXP, SEXP infected_initSEXP, SEXP chemoprophylaxis_initSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type PfID_init(PfID_initSEXP);
    Rcpp::traits::input_parameter< double >::type tInf_init(tInf_initSEXP);
    Rcpp::traits::input_parameter< double >::type b_init(b_initSEXP);
    Rcpp::traits::input_parameter< double >::type c_init(c_initSEXP);
    Rcpp::traits::input_parameter< int >::type damID_init(damID_initSEXP);
    Rcpp::traits::input_parameter< int >::type sireID_init(sireID_initSEXP);
    Rcpp::traits::input_parameter< bool >::type infected_init(infected_initSEXP);
    Rcpp::traits::input_parameter< bool >::type chemoprophylaxis_init(chemoprophylaxis_initSEXP);
    rcpp_result_gen = Rcpp::wrap(humanPfSIcpp__ctor(PfID_init, tInf_init, b_init, c_init, damID_init, sireID_init, infected_init, chemoprophylaxis_init));
    return rcpp_result_gen;
END_RCPP
}
// humanPfSIcpp__get_PfID
std::vector<int> humanPfSIcpp__get_PfID(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_);
RcppExport SEXP _MASH_humanPfSIcpp__get_PfID(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(humanPfSIcpp__get_PfID(obj_));
    return rcpp_result_gen;
END_RCPP
}
// humanPfSIcpp__push_PfID
void humanPfSIcpp__push_PfID(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_, int PfID_new);
RcppExport SEXP _MASH_humanPfSIcpp__push_PfID(SEXP obj_SEXP, SEXP PfID_newSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< int >::type PfID_new(PfID_newSEXP);
    humanPfSIcpp__push_PfID(obj_, PfID_new);
    return R_NilValue;
END_RCPP
}
// humanPfSIcpp__get_tInf
std::vector<double> humanPfSIcpp__get_tInf(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_);
RcppExport SEXP _MASH_humanPfSIcpp__get_tInf(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(humanPfSIcpp__get_tInf(obj_));
    return rcpp_result_gen;
END_RCPP
}
// humanPfSIcpp__push_tInf
void humanPfSIcpp__push_tInf(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_, double tInf_new);
RcppExport SEXP _MASH_humanPfSIcpp__push_tInf(SEXP obj_SEXP, SEXP tInf_newSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< double >::type tInf_new(tInf_newSEXP);
    humanPfSIcpp__push_tInf(obj_, tInf_new);
    return R_NilValue;
END_RCPP
}
// humanPfSIcpp__get_b
double humanPfSIcpp__get_b(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_);
RcppExport SEXP _MASH_humanPfSIcpp__get_b(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(humanPfSIcpp__get_b(obj_));
    return rcpp_result_gen;
END_RCPP
}
// humanPfSIcpp__set_b
void humanPfSIcpp__set_b(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_, double b_new);
RcppExport SEXP _MASH_humanPfSIcpp__set_b(SEXP obj_SEXP, SEXP b_newSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< double >::type b_new(b_newSEXP);
    humanPfSIcpp__set_b(obj_, b_new);
    return R_NilValue;
END_RCPP
}
// humanPfSIcpp__get_c
double humanPfSIcpp__get_c(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_);
RcppExport SEXP _MASH_humanPfSIcpp__get_c(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(humanPfSIcpp__get_c(obj_));
    return rcpp_result_gen;
END_RCPP
}
// humanPfSIcpp__set_c
void humanPfSIcpp__set_c(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_, double c_new);
RcppExport SEXP _MASH_humanPfSIcpp__set_c(SEXP obj_SEXP, SEXP c_newSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< double >::type c_new(c_newSEXP);
    humanPfSIcpp__set_c(obj_, c_new);
    return R_NilValue;
END_RCPP
}
// humanPfSIcpp__get_damID
std::vector<int> humanPfSIcpp__get_damID(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_);
RcppExport SEXP _MASH_humanPfSIcpp__get_damID(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(humanPfSIcpp__get_damID(obj_));
    return rcpp_result_gen;
END_RCPP
}
// humanPfSIcpp__push_damID
void humanPfSIcpp__push_damID(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_, int damID_new);
RcppExport SEXP _MASH_humanPfSIcpp__push_damID(SEXP obj_SEXP, SEXP damID_newSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< int >::type damID_new(damID_newSEXP);
    humanPfSIcpp__push_damID(obj_, damID_new);
    return R_NilValue;
END_RCPP
}
// humanPfSIcpp__get_sireID
std::vector<int> humanPfSIcpp__get_sireID(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_);
RcppExport SEXP _MASH_humanPfSIcpp__get_sireID(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(humanPfSIcpp__get_sireID(obj_));
    return rcpp_result_gen;
END_RCPP
}
// humanPfSIcpp__push_sireID
void humanPfSIcpp__push_sireID(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_, int sireID_new);
RcppExport SEXP _MASH_humanPfSIcpp__push_sireID(SEXP obj_SEXP, SEXP sireID_newSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< int >::type sireID_new(sireID_newSEXP);
    humanPfSIcpp__push_sireID(obj_, sireID_new);
    return R_NilValue;
END_RCPP
}
// humanPfSIcpp__get_infected
bool humanPfSIcpp__get_infected(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_);
RcppExport SEXP _MASH_humanPfSIcpp__get_infected(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(humanPfSIcpp__get_infected(obj_));
    return rcpp_result_gen;
END_RCPP
}
// humanPfSIcpp__set_infected
void humanPfSIcpp__set_infected(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_, bool infected_new);
RcppExport SEXP _MASH_humanPfSIcpp__set_infected(SEXP obj_SEXP, SEXP infected_newSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< bool >::type infected_new(infected_newSEXP);
    humanPfSIcpp__set_infected(obj_, infected_new);
    return R_NilValue;
END_RCPP
}
// humanPfSIcpp__get_chemoprophylaxis
bool humanPfSIcpp__get_chemoprophylaxis(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_);
RcppExport SEXP _MASH_humanPfSIcpp__get_chemoprophylaxis(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(humanPfSIcpp__get_chemoprophylaxis(obj_));
    return rcpp_result_gen;
END_RCPP
}
// humanPfSIcpp__set_chemoprophylaxis
void humanPfSIcpp__set_chemoprophylaxis(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_, bool chemoprophylaxis_new);
RcppExport SEXP _MASH_humanPfSIcpp__set_chemoprophylaxis(SEXP obj_SEXP, SEXP chemoprophylaxis_newSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< bool >::type chemoprophylaxis_new(chemoprophylaxis_newSEXP);
    humanPfSIcpp__set_chemoprophylaxis(obj_, chemoprophylaxis_new);
    return R_NilValue;
END_RCPP
}
// humanPfSIcpp__track_history
void humanPfSIcpp__track_history(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_, double tEvent, std::string event);
RcppExport SEXP _MASH_humanPfSIcpp__track_history(SEXP obj_SEXP, SEXP tEventSEXP, SEXP eventSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< double >::type tEvent(tEventSEXP);
    Rcpp::traits::input_parameter< std::string >::type event(eventSEXP);
    humanPfSIcpp__track_history(obj_, tEvent, event);
    return R_NilValue;
END_RCPP
}
// humanPfSIcpp__get_history
Rcpp::List humanPfSIcpp__get_history(MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> obj_);
RcppExport SEXP _MASH_humanPfSIcpp__get_history(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::humanPfSIcpp> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(humanPfSIcpp__get_history(obj_));
    return rcpp_result_gen;
END_RCPP
}
// RiskQ__ctor
MASH::RiskQ RiskQ__ctor();
RcppExport SEXP _MASH_RiskQ__ctor() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(RiskQ__ctor());
    return rcpp_result_gen;
END_RCPP
}
// RiskQ__get_N
int RiskQ__get_N(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_);
RcppExport SEXP _MASH_RiskQ__get_N(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::RiskQ> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(RiskQ__get_N(obj_));
    return rcpp_result_gen;
END_RCPP
}
// RiskQ__set_N
void RiskQ__set_N(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_, int N_new);
RcppExport SEXP _MASH_RiskQ__set_N(SEXP obj_SEXP, SEXP N_newSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::RiskQ> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< int >::type N_new(N_newSEXP);
    RiskQ__set_N(obj_, N_new);
    return R_NilValue;
END_RCPP
}
// RiskQ__get_who
std::vector<int> RiskQ__get_who(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_);
RcppExport SEXP _MASH_RiskQ__get_who(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::RiskQ> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(RiskQ__get_who(obj_));
    return rcpp_result_gen;
END_RCPP
}
// RiskQ__push_who
void RiskQ__push_who(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_, int who_new);
RcppExport SEXP _MASH_RiskQ__push_who(SEXP obj_SEXP, SEXP who_newSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::RiskQ> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< int >::type who_new(who_newSEXP);
    RiskQ__push_who(obj_, who_new);
    return R_NilValue;
END_RCPP
}
// RiskQ__get_pTm
std::vector<double> RiskQ__get_pTm(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_);
RcppExport SEXP _MASH_RiskQ__get_pTm(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::RiskQ> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(RiskQ__get_pTm(obj_));
    return rcpp_result_gen;
END_RCPP
}
// RiskQ__push_pTm
void RiskQ__push_pTm(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_, double pTm_new);
RcppExport SEXP _MASH_RiskQ__push_pTm(SEXP obj_SEXP, SEXP pTm_newSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::RiskQ> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< double >::type pTm_new(pTm_newSEXP);
    RiskQ__push_pTm(obj_, pTm_new);
    return R_NilValue;
END_RCPP
}
// RiskQ__get_w
std::vector<double> RiskQ__get_w(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_);
RcppExport SEXP _MASH_RiskQ__get_w(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::RiskQ> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(RiskQ__get_w(obj_));
    return rcpp_result_gen;
END_RCPP
}
// RiskQ__push_w
void RiskQ__push_w(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_, double w_new);
RcppExport SEXP _MASH_RiskQ__push_w(SEXP obj_SEXP, SEXP w_newSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::RiskQ> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< double >::type w_new(w_newSEXP);
    RiskQ__push_w(obj_, w_new);
    return R_NilValue;
END_RCPP
}
// RiskQ__add_HumanHost
void RiskQ__add_HumanHost(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_, int who_new, double pTm_new, double w_new);
RcppExport SEXP _MASH_RiskQ__add_HumanHost(SEXP obj_SEXP, SEXP who_newSEXP, SEXP pTm_newSEXP, SEXP w_newSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::RiskQ> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< int >::type who_new(who_newSEXP);
    Rcpp::traits::input_parameter< double >::type pTm_new(pTm_newSEXP);
    Rcpp::traits::input_parameter< double >::type w_new(w_newSEXP);
    RiskQ__add_HumanHost(obj_, who_new, pTm_new, w_new);
    return R_NilValue;
END_RCPP
}
// RiskQ__get_nOther
int RiskQ__get_nOther(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_);
RcppExport SEXP _MASH_RiskQ__get_nOther(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::RiskQ> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(RiskQ__get_nOther(obj_));
    return rcpp_result_gen;
END_RCPP
}
// RiskQ__set_nOther
void RiskQ__set_nOther(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_, int nOther_new);
RcppExport SEXP _MASH_RiskQ__set_nOther(SEXP obj_SEXP, SEXP nOther_newSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::RiskQ> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< int >::type nOther_new(nOther_newSEXP);
    RiskQ__set_nOther(obj_, nOther_new);
    return R_NilValue;
END_RCPP
}
// RiskQ__add_OtherHost
void RiskQ__add_OtherHost(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_, double otherW_new, int typeID_new);
RcppExport SEXP _MASH_RiskQ__add_OtherHost(SEXP obj_SEXP, SEXP otherW_newSEXP, SEXP typeID_newSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::RiskQ> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< double >::type otherW_new(otherW_newSEXP);
    Rcpp::traits::input_parameter< int >::type typeID_new(typeID_newSEXP);
    RiskQ__add_OtherHost(obj_, otherW_new, typeID_new);
    return R_NilValue;
END_RCPP
}
// RiskQ__get_OtherHost
Rcpp::List RiskQ__get_OtherHost(MASH::RcppR6::RcppR6<MASH::RiskQ> obj_);
RcppExport SEXP _MASH_RiskQ__get_OtherHost(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MASH::RcppR6::RcppR6<MASH::RiskQ> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(RiskQ__get_OtherHost(obj_));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_MASH_util_PfSIoneHistory", (DL_FUNC) &_MASH_util_PfSIoneHistory, 3},
    {"_MASH_HumanEventQ__ctor", (DL_FUNC) &_MASH_HumanEventQ__ctor, 1},
    {"_MASH_HumanEventQ__firstEvent", (DL_FUNC) &_MASH_HumanEventQ__firstEvent, 1},
    {"_MASH_HumanEventQ__firstTime", (DL_FUNC) &_MASH_HumanEventQ__firstTime, 1},
    {"_MASH_HumanEventQ__rmFirstEventFromQ", (DL_FUNC) &_MASH_HumanEventQ__rmFirstEventFromQ, 1},
    {"_MASH_HumanEventQ__rmTagFromQ", (DL_FUNC) &_MASH_HumanEventQ__rmTagFromQ, 2},
    {"_MASH_HumanEventQ__get_queueN", (DL_FUNC) &_MASH_HumanEventQ__get_queueN, 1},
    {"_MASH_HumanEventQ__addEvent2Q", (DL_FUNC) &_MASH_HumanEventQ__addEvent2Q, 2},
    {"_MASH_HumanEventQ__get_EventQ", (DL_FUNC) &_MASH_HumanEventQ__get_EventQ, 1},
    {"_MASH_HistoryGeneric__ctor", (DL_FUNC) &_MASH_HistoryGeneric__ctor, 1},
    {"_MASH_HistoryGeneric__track_history", (DL_FUNC) &_MASH_HistoryGeneric__track_history, 3},
    {"_MASH_HistoryGeneric__get_history", (DL_FUNC) &_MASH_HistoryGeneric__get_history, 1},
    {"_MASH_HistoryTravel__ctor", (DL_FUNC) &_MASH_HistoryTravel__ctor, 1},
    {"_MASH_HistoryTravel__track_travel", (DL_FUNC) &_MASH_HistoryTravel__track_travel, 3},
    {"_MASH_HistoryTravel__get_travelHistory", (DL_FUNC) &_MASH_HistoryTravel__get_travelHistory, 1},
    {"_MASH_humanPfSIcpp__ctor", (DL_FUNC) &_MASH_humanPfSIcpp__ctor, 8},
    {"_MASH_humanPfSIcpp__get_PfID", (DL_FUNC) &_MASH_humanPfSIcpp__get_PfID, 1},
    {"_MASH_humanPfSIcpp__push_PfID", (DL_FUNC) &_MASH_humanPfSIcpp__push_PfID, 2},
    {"_MASH_humanPfSIcpp__get_tInf", (DL_FUNC) &_MASH_humanPfSIcpp__get_tInf, 1},
    {"_MASH_humanPfSIcpp__push_tInf", (DL_FUNC) &_MASH_humanPfSIcpp__push_tInf, 2},
    {"_MASH_humanPfSIcpp__get_b", (DL_FUNC) &_MASH_humanPfSIcpp__get_b, 1},
    {"_MASH_humanPfSIcpp__set_b", (DL_FUNC) &_MASH_humanPfSIcpp__set_b, 2},
    {"_MASH_humanPfSIcpp__get_c", (DL_FUNC) &_MASH_humanPfSIcpp__get_c, 1},
    {"_MASH_humanPfSIcpp__set_c", (DL_FUNC) &_MASH_humanPfSIcpp__set_c, 2},
    {"_MASH_humanPfSIcpp__get_damID", (DL_FUNC) &_MASH_humanPfSIcpp__get_damID, 1},
    {"_MASH_humanPfSIcpp__push_damID", (DL_FUNC) &_MASH_humanPfSIcpp__push_damID, 2},
    {"_MASH_humanPfSIcpp__get_sireID", (DL_FUNC) &_MASH_humanPfSIcpp__get_sireID, 1},
    {"_MASH_humanPfSIcpp__push_sireID", (DL_FUNC) &_MASH_humanPfSIcpp__push_sireID, 2},
    {"_MASH_humanPfSIcpp__get_infected", (DL_FUNC) &_MASH_humanPfSIcpp__get_infected, 1},
    {"_MASH_humanPfSIcpp__set_infected", (DL_FUNC) &_MASH_humanPfSIcpp__set_infected, 2},
    {"_MASH_humanPfSIcpp__get_chemoprophylaxis", (DL_FUNC) &_MASH_humanPfSIcpp__get_chemoprophylaxis, 1},
    {"_MASH_humanPfSIcpp__set_chemoprophylaxis", (DL_FUNC) &_MASH_humanPfSIcpp__set_chemoprophylaxis, 2},
    {"_MASH_humanPfSIcpp__track_history", (DL_FUNC) &_MASH_humanPfSIcpp__track_history, 3},
    {"_MASH_humanPfSIcpp__get_history", (DL_FUNC) &_MASH_humanPfSIcpp__get_history, 1},
    {"_MASH_RiskQ__ctor", (DL_FUNC) &_MASH_RiskQ__ctor, 0},
    {"_MASH_RiskQ__get_N", (DL_FUNC) &_MASH_RiskQ__get_N, 1},
    {"_MASH_RiskQ__set_N", (DL_FUNC) &_MASH_RiskQ__set_N, 2},
    {"_MASH_RiskQ__get_who", (DL_FUNC) &_MASH_RiskQ__get_who, 1},
    {"_MASH_RiskQ__push_who", (DL_FUNC) &_MASH_RiskQ__push_who, 2},
    {"_MASH_RiskQ__get_pTm", (DL_FUNC) &_MASH_RiskQ__get_pTm, 1},
    {"_MASH_RiskQ__push_pTm", (DL_FUNC) &_MASH_RiskQ__push_pTm, 2},
    {"_MASH_RiskQ__get_w", (DL_FUNC) &_MASH_RiskQ__get_w, 1},
    {"_MASH_RiskQ__push_w", (DL_FUNC) &_MASH_RiskQ__push_w, 2},
    {"_MASH_RiskQ__add_HumanHost", (DL_FUNC) &_MASH_RiskQ__add_HumanHost, 4},
    {"_MASH_RiskQ__get_nOther", (DL_FUNC) &_MASH_RiskQ__get_nOther, 1},
    {"_MASH_RiskQ__set_nOther", (DL_FUNC) &_MASH_RiskQ__set_nOther, 2},
    {"_MASH_RiskQ__add_OtherHost", (DL_FUNC) &_MASH_RiskQ__add_OtherHost, 3},
    {"_MASH_RiskQ__get_OtherHost", (DL_FUNC) &_MASH_RiskQ__get_OtherHost, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_MASH(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
