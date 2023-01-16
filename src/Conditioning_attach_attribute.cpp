#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
S4 attach_attribute(S4 fisheries, NumericVector attribute, CharacterVector fl, CharacterVector st, CharacterVector attribute_name) {
  
  // clone fisheries object
  S4 fltsS4 = clone(fisheries);
  
  // Extract vector of fleets names
  CharacterVector fltsNames = fltsS4.slot("names");
  
  // Extract list of fleets
  List fltsList = fltsS4.slot(".Data");
  
  // Numbers of fleets
  int nf = fltsList.size();
  
  // Create LogicalVector
  int f_idx;
  
  // Match fleet name to vector
  for(int fi = 0; fi < nf; fi++) {
    if(fltsNames[fi] == fl[0]) {
      f_idx = fi;
    }
  }
  
  // Extract fleet
  S4 fltS4 = fltsList[f_idx];
  
  // Extract vector of catch names
  CharacterVector catsNames = fltS4.slot("names");
  
  // Extract data for stock catches
  List catsList = fltS4.slot(".Data");
  
  // Numbers of catches
  int ns = catsList.size();
  
  // Create LogicalVector
  int s_idx;
  
  // Match stock name to vector
  for(int si = 0; si < ns; si++) {
    if(catsNames[si] == st[0]) {
      s_idx = si;
    }
  }
  
  // Extract stock of interest
  S4 catS4 = catsList[s_idx];
  
  // Attach attribute
  if(attribute_name[0] == "quotashare"){
    catS4.attr("quotashare") = attribute;
  }
  
  // Re-assemble fisheries object
  catsList[s_idx]      = catS4;
  fltS4.slot(".Data")  = catsList;
  fltsList[f_idx]      = fltS4;
  fltsS4.slot(".Data") = fltsList;
  
  return fltsS4;
}