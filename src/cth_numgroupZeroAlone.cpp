#include <Rcpp.h> //utilisation du package Rcpp pour faire le lien entre du C++ et du R
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector cth_numgroupZeroAlone(NumericVector cvo) {
  //Fonction qui permet de donner des numeros croissants aux clusters et pour chaque 0 de changer la valeur
  int nb = 1;
  NumericVector cvod(cvo.length());
  cvod[0] = nb;
  for(int i = 1; i<cvo.length(); i++){
    if(cvo[i]==0) nb++;
    else{
      if(cvo[i-1]!=cvo[i]) nb++;
    }
    cvod[i] = nb;
  }
  return cvod;
}
