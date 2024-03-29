#include <Rcpp.h> //utilisation du package Rcpp pour faire le lien entre du C++ et du R
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector cth_numgroup(NumericVector cvo) {
  //Fonction qui permet de numeroter dans l'ordre croissant
  int nb = 1;
  NumericVector cvod(cvo.length()); //creation d'un vecteur de meme longueur que le vecteur cvo
  cvod[0] = nb;
  for(int i = 1; i<cvo.length(); i++){
    if(cvo[i-1]!=cvo[i]) nb++; //Si la valeur du vecteur est diff?rente de la precedente alors on augmente nb de 1
    cvod[i] = nb;
      }
  return cvod; //on retroune le vecteur
}
