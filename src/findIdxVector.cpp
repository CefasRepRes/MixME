#include <Rcpp.h>

/* This file contains a short helper function to find the location of a named
 * element in a vector
 */

int findIdxVector(Rcpp::CharacterVector idxName,
                  Rcpp::CharacterVector vecNames) {
  
  int Check = -99;
  for (int idx = 0; idx < vecNames.size(); idx++) {
    if (idxName[0] == vecNames[idx]) {
      Check = idx;
    }
  } 
  return Check;
}