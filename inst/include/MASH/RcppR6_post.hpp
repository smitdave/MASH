// Generated by RcppR6 (0.2.4): do not edit by hand
#ifndef _MASH_RCPPR6_POST_HPP_
#define _MASH_RCPPR6_POST_HPP_

#include <Rcpp.h>
#include <MASH/RcppR6_support.hpp>

namespace MASH {
namespace RcppR6 {
namespace traits {
template <> inline std::string   class_name_r<MASH::HumanEventQ >() {return "HumanEventQ";}
template <> inline std::string   package_name<MASH::HumanEventQ >() {return "MASH";}
template <> inline std::string generator_name<MASH::HumanEventQ >() {return ".R6_HumanEventQ";}
}
}
}

namespace Rcpp {
template <typename T>
SEXP wrap(const MASH::RcppR6::RcppR6<T>& x) {
  return x.to_R6();
}

namespace traits {
template <typename T>
class Exporter<MASH::RcppR6::RcppR6<T> > {
public:
  Exporter(SEXP x) : obj(MASH::RcppR6::RcppR6<T>(x)) {}
  inline MASH::RcppR6::RcppR6<T> get() { return obj; }
private:
  MASH::RcppR6::RcppR6<T> obj;
};
}

template <> inline SEXP wrap(const MASH::HumanEventQ& x) {
  return wrap(MASH::RcppR6::RcppR6<MASH::HumanEventQ>(x));
}
template <> inline MASH::HumanEventQ as(SEXP x) {
  return *(MASH::RcppR6::RcppR6<MASH::HumanEventQ>(x));
}
}

#endif