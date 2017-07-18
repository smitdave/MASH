////////////////////////////////////////////////////////////
//
//  MASH
//  MOSQUITO Life History Structure
//  Sean Wu
//  July 18, 2017
//
////////////////////////////////////////////////////////////

#ifndef _MASH_MOSYHIST_HPP_
#define _MASH_MOSYHIST_HPP_

#include <Rcpp.h>

namespace MASH {

// MosquitoFemaleHistory: class to store female mosquito histories; \code{\link{MicroMosquitoFemale}} and \code{\link{MicroMosquito}}
class MosquitoFemaleHistory{
// public members
public:

  ///////////////////////////////////
  // History Constructor
  ///////////////////////////////////

  MosquitoFemaleHistory();


  ///////////////////////////////////
  // History Tracking
  ///////////////////////////////////

  // pass the private environment of the enclosing mosquito to the function
  void historyTrack(Rcpp::Environment privateEnv){

    // check if mosquito is alive
    Rcpp::Function isAlive = privateEnv["isAlive"];
    bool alive = isAlive();

    if(alive){
      stateH.push_back(privateEnv["state"]);  // state trajectory
      timeH.push_back(privateEnv["tNow"]);  // transition times
      ixH.push_back(privateEnv["ix"]);  // sites visited
      pSetH.push_back(privateEnv["inPointSet"]);  // point sets visited
    } else {
      stateH.push_back(privateEnv["stateNew"]); // state trajectory
      timeH.push_back(privateEnv["tNext"]); // transition times
      this->calcBionomics(); // track bionomics upon death
    }
  };

  // historyFeed: track feeding history
  void historyFeed(Rcpp::Environment privateEnv){

    int hostID = privateEnv["hostID"];
    if(hostID > 0){
      // human host
      feedAllH += 1;    // number of blood meals
      feedAllT.push_back(privateEnv["tNow"]);   // times of blood meals
      feedHumanH += 1;    // number of blood meals on human hosts
      feedHumanT.push_back(privateEnv["tNow"]);   // times of blood meals on human hosts
      feedIxH.push_back(privateEnv["hostID"]);    // ids of all blood hosts
      bmSizeH.push_back(privateEnv["bmSize"]);    // size of blood meal
      batchH.push_back(privateEnv["batch"]);    // size of egg batch
    } else {
      // non-human host
      feedAllH += 1;    // number of blood meals
      feedAllT.push_back(privateEnv["tNow"]);   // times of blood meals
      feedIxH.push_back(privateEnv["hostID"]);    // ids of all blood hosts
      bmSizeH.push_back(privateEnv["bmSize"]);    // size of blood meal
      batchH.push_back(privateEnv["batch"]);    // size of egg batch
    }

  };

  // calcBionomics: track mosquito bionomics upon death
  void calcBionomics(){
    if(stateH.back() != "D"){
      Rcpp::stop("mosquito not dead yet but bionomics are being calculated!");
    } else {
      if(batchH.size() != 0){
        int batchH_sum = std::accumulate(batchH.begin(), batchH.end(), 0);
        double batchH_mean = batchH_sum / batchH.size();
        bionomics["mBatch"] = batchH_mean;
        bionomics["tBatch"] = batchH_sum;
      } else {
        bionomics["mBatch"] = 0.0;
        bionomics["tBatch"] = 0;
      }
      bionomics["feedAllH"] =  feedAllH; // total number of bloodmeals
      bionomics["feedHumanH"] = feedHumanH; // number of human bloodmeals

      // intervals between bloodmeals
      std::vector<double> feedAllT_diff;
      std::adjacent_difference(feedAllT.begin(), feedAllT.end(), std::back_inserter(feedAllT_diff));
      feedAllT_diff.erase(feedAllT_diff.begin());
      bionomics["bmInt"] = feedAllT_diff;

      // intervals between human bloodmeals
      std::vector<double> feedHumanT_diff;
      std::adjacent_difference(feedHumanT.begin(), feedHumanT.end(), std::back_inserter(feedHumanT_diff));
      feedAllT_diff.erase(feedHumanT_diff.begin());
      bionomics["bmIntH"] = feedHumanT_diff;

      // lifespan
      bionomics["lifespan"] = timeH.back() - timeH.front();
    }
  };


  ///////////////////////////////////
  // History Export
  ///////////////////////////////////

  // exportHistory: export this mosquito history
  Rcpp::List exportHistory(){

    return(
      Rcpp::List::create(
        Rcpp::Named("stateH") = stateH,
        Rcpp::Named("timeH") = timeH,
        Rcpp::Named("ixH") = ixH,
        Rcpp::Named("pSetH") = pSetH,
        Rcpp::Named("feedAllH") = feedAllH,
        Rcpp::Named("feedAllT") = feedAllT,
        Rcpp::Named("feedHumanH") = feedHumanH,
        Rcpp::Named("feedHumanT") = feedHumanT,
        Rcpp::Named("batchH") = batchH
      )
    );

  };

  // exportBionomics: export this mosquito calculated bionomics
  Rcpp::List exportBionomics(){

    return(bionomics);

  };


// private members
private:

  // history objects
  std::vector<std::string> stateH;
  std::vector<double>      timeH;
  std::vector<int>         ixH;
  std::vector<std::string> pSetH;
  int                      feedAllH;
  std::vector<double>      feedAllT;
  int                      feedHumanH;
  std::vector<double>      feedHumanT;
  std::vector<int>         feedIxH;
  std::vector<int>         bmSizeH;
  std::vector<int>         batchH;

  // bionomics
  Rcpp::List               bionomics;

};

// inline definition of constructor to accept default argument values
inline MosquitoFemaleHistory::MosquitoFemaleHistory(){

  stateH.reserve(50);
  timeH.reserve(50);
  ixH.reserve(50);
  pSetH.reserve(50);
  feedAllH = 0;
  feedAllT.reserve(50);
  feedHumanH = 0;
  feedHumanT.reserve(50);
  feedIxH.reserve(50);
  bmSizeH.reserve(50);
  batchH.reserve(50);

}

}

#endif
