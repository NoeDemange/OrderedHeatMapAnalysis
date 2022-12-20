#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericVector cppcorrNeighbor(NumericMatrix mat) {
  int nrow = mat.nrow();
  Environment base = Environment::namespace_env("base");//importer la fonction table du package base de R
  Function table = base["table"];
  NumericVector Vphi;
  for(int i=1; i<nrow-1; i++){
    NumericVector Vmati = mat(i,_);
    NumericVector Vmat1i = mat(i-1,_);
    NumericVector Vmati1 = mat(i+1,_);
    //calcul du coefficient Phi pour la ligne et la precedente
    NumericVector T1 = table(Vmat1i, Vmati);//calcul du tableau de contingence
    NumericVector Tab1 = T1/sum(T1);//calcul de la moyenne pour chaque case
    double a1 = Tab1[0];
    double b1 = Tab1[1];
    double c1 = Tab1[2];
    double d1 = Tab1[3];
    //calcul du coefficient Phi
    double Phi1 = (a1 - (a1 + b1) * (a1 + c1))/sqrt((a1 + b1) * (c1 + d1) * (a1 + c1) * (b1 + d1));
    //calcul du coefficient Phi pour la ligne et la suivante
    NumericVector T2 = table(Vmati, Vmati1);
    NumericVector Tab2 = T2/sum(T2);
    double a2 = Tab2[0];
    double b2 = Tab2[1];
    double c2 = Tab2[2];
    double d2 = Tab2[3];
    double Phi2 = (a2 - (a2 + b2) * (a2 + c2))/sqrt((a2 + b2) * (c2 + d2) * (a2 + c2) * (b2 + d2));
    //on garde le max des coefficients Phi pour la ligne sauf si l'un des deux est NAN
    if(Rcpp::traits::is_nan<REALSXP>(Phi1)){Vphi.push_back(Phi2);}
    else if (Rcpp::traits::is_nan<REALSXP>(Phi2)){Vphi.push_back(Phi1);}
    else if(Phi1>Phi2){Vphi.push_back(Phi1);}
    else{Vphi.push_back(Phi2);}
  }
  return Vphi;
}
