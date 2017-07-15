#ifndef _MASH_HIST_HPP_
#define _MASH_HIST_HPP_

#include <Rcpp.h>

namespace MASH {

// GenericHistory stores events as strings and times as double precision floats
class GenericHistory {
public:

  // constructor
  GenericHistory(const int &N = 100){
    events.reserve(N);
    events.push_back("init");
    eventT.reserve(N);
    eventT.push_back(-1);
  }

  void track_History

private:
  std::vector<std::string> events;
  std::vector<double>      eventT;
};

}


#endif
