// Generated by RcppR6 (0.2.4): do not edit by hand
#ifndef _MASH_RCPPR6_PRE_HPP_
#define _MASH_RCPPR6_PRE_HPP_

#include <RcppCommon.h>


namespace MASH {
namespace RcppR6 {
template <typename T> class RcppR6;
}
}

namespace MASH { class HumanEventQ; }
namespace MASH { class HistoryGeneric; }
namespace MASH { class HistoryTravel; }
namespace MASH { class humanPfSIcpp; }
namespace MASH { class RiskQ; }

namespace Rcpp {
template <typename T> SEXP wrap(const MASH::RcppR6::RcppR6<T>&);
namespace traits {
template <typename T> class Exporter<MASH::RcppR6::RcppR6<T> >;
}

template <> SEXP wrap(const MASH::HumanEventQ&);
template <> MASH::HumanEventQ as(SEXP);
template <> SEXP wrap(const MASH::HistoryGeneric&);
template <> MASH::HistoryGeneric as(SEXP);
template <> SEXP wrap(const MASH::HistoryTravel&);
template <> MASH::HistoryTravel as(SEXP);
template <> SEXP wrap(const MASH::humanPfSIcpp&);
template <> MASH::humanPfSIcpp as(SEXP);
template <> SEXP wrap(const MASH::RiskQ&);
template <> MASH::RiskQ as(SEXP);
}

#endif
