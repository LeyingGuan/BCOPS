#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector conformalscore(NumericVector ste, NumericVector s) {
  int n = ste.size(); 
  int m = s.size();
  NumericVector count(n);
  for(int i = 0; i < n; i++){
    for(int j = 0;j < m; j++){
      if(ste[i] >= s[j]){
        count[i]+=1;
      }
    }
  }
  return count;
}

