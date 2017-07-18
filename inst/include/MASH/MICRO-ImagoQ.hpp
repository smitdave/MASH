#ifndef _MASH_IMAGOQ_HPP_
#define _MASH_IMAGOQ_HPP_

#include <Rcpp.h>

namespace MASH {

// OtherHost: struct to store information for non-human hosts
struct ImagoSlot{
  ImagoSlot(const int &N_new, const double &tEmerge_new, const int &genotype_new, const int &damID_new, const int &sireID_new);
  int N;
  double tEmerge;
  int genotype;
  int damID;
  int sireID;
};

inline ImagoSlot::ImagoSlot(const int &N_new, const double &tEmerge_new, const int &genotype_new, const int &damID_new, const int &sireID_new){
  N = N_new;
  tEmerge = tEmerge_new;
  genotype = genotype_new;
  damID = damID_new;
  sireID = sireID_new;
}

// OtherHostVector: store OtherHost structs
typedef std::vector<ImagoSlot> ImagoQVector;

// ImagoQ: Imago Queue class definition
class ImagoQ {
// public members
public:

  ///////////////////////////////////
  // Imago Queue Constructor
  ///////////////////////////////////

  // constructor defined belowreturn(ix.N != 0);
  ImagoQ();


  ///////////////////////////////////
  // Queue Management
  ///////////////////////////////////

  // clear_ImagoQ: Clear out all populated slots (N>0) in an ImagoQ for the EL4P module of Aquatic Ecology
  void clear_ImagoQ(){

    // find all non-null slots
    std::vector<int> fullIx;
    auto it = std::find_if(ImagoQVec.begin(), ImagoQVec.end(), [](ImagoSlot ix){
      return(ix.N != 0);
    });
    while(it != ImagoQVec.end()){
      fullIx.emplace_back(std::distance(ImagoQVec.begin(), it));
      it = std::find_if(std::next(it), std::end(ImagoQVec), [](ImagoSlot ix){
        return(ix.N != 0);
      });
    }

    for(std::vector<int>::iterator it = fullIx.begin(); it != fullIx.end(); it++){
      ImagoQVec[int(*it)].N = 0;
      ImagoQVec[int(*it)].tEmerge = -1;
      ImagoQVec[int(*it)].genotype = -1;
      ImagoQVec[int(*it)].damID = -1;
      ImagoQVec[int(*it)].sireID = -1;
    }

  };

  // add_ImagoQ: Add emerging adults to the ImagoQ
  void add_ImagoQ(const int &N_new, const double &tEmerge_new, const int &genotype_new, const int &damID_new, const int &sireID_new){

    // find null slot
    auto it = std::find(ImagoQVec.begin(), ImagoQVec.end(), [](ImagoSlot ix){
      return(ix.N == 0);
    });

    // insert the new slot into ImagoQ
    if(it == ImagoQVec.end()){
      // there are no null slots
      ImagoQVec.push_back(ImagoSlot(N_new,tEmerge_new,genotype_new,damID_new,sireID_new));
    } else {
      // there is a null slot
      ImagoQVec[int(*it)].N = N_new;
      ImagoQVec[int(*it)].tEmerge = tEmerge_new;
      ImagoQVec[int(*it)].genotype = genotype_new;
      ImagoQVec[int(*it)].damID = damID_new;
      ImagoQVec[int(*it)].sireID = sireID_new;
    }

  };


  ///////////////////////////////////
  // Queue Tracking
  ///////////////////////////////////

  // track_ImagoQ: Return the total number of emerging adults in this ImagoQ whose tEmerge <= time
  double track_ImagoQ(const double &time){

    // find slots where tEmerge <= time
    std::vector<int> timeIx;
    auto it = std::find_if(ImagoQVec.begin(), ImagoQVec.end(), [time](ImagoSlot ix){
      return(ix.tEmerge <= time);
    });
    while(it != ImagoQVec.end()){
      timeIx.emplace_back(std::distance(ImagoQVec.begin(), it));
      it = std::find_if(std::next(it), std::end(ImagoQVec), [time](ImagoSlot ix){
        return(ix.tEmerge <= time);
      });
    }

    int totalAdults;
    for(auto it = timeIx.begin(); it != timeIx.end(); it++){
      totalAdults += ImagoQVec[*it].N;
    }

    return(totalAdults);
  };


  ///////////////////////////////////
  // Getters & Setters
  ///////////////////////////////////

  int get_N(){
    return(N);
  };
  void set_N(const int &N_new){
    N = N_new;
  };

  Rcpp::List get_ImagoQ(){
    return(Rcpp::wrap(ImagoQVec));
  };


// private members
private:

  ImagoQVector ImagoQVec;
  int N; // size of queue

};

// inline definition of constructor to accept default argument values
inline ImagoQ::ImagoQ(){

  N = 0;
  ImagoQVec.reserve(50);
  ImagoQVec.insert(ImagoQVec.end(), 10, ImagoSlot(0,0,0,0,0));

}

}

#endif
