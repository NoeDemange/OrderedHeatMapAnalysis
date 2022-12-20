#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector cppARorder(NumericMatrix mat) {
  int nrow = mat.nrow();
  NumericVector Viar(nrow);
  for(int k = 0; k<nrow; k++){
    NumericVector IARk = 0; 
    NumericVector rep = mat(k,_);
    for(int i = 1; i<rep.length();i++){
      NumericVector nv;
      for(int posm = 0; posm<i-1;posm++){
        if(mat(k,posm)>mat(k,i)) nv.push_back(mat(k,posm));
      }
      NumericVector neg = mat(k,i)-nv;
      NumericVector iAR(1);
      iAR[0] = sum(neg);
      IARk[0]=IARk[0]+iAR[0];
    }
    Viar[k] = IARk[0];
  }
  return Viar;
}
// 
// Viar <- vector()
// for (k in 1 : nrow(MdMat3))  {
//   IARk <- 0
//   for (i in 2 : length(MdMat3[k,]))  {
//     iAR <- sum(MdMat3[k, i]-MdMat3[k,which(MdMat3[k,1:(i-1)]>MdMat3[k,i])])
//     IARk	<- IARk + iAR	
//   }
//   Viar <- append(Viar, IARk)
// }