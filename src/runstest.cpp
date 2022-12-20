#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector cpprunstest(NumericMatrix mat, std::string alternative) {
  int nrow = mat.nrow();
  
  NumericVector RunTV(nrow);
  Environment tseries = Environment::namespace_env("tseries");
  Function runs_test = tseries["runs.test"];
  Environment base = Environment::namespace_env("base");
  Function as_factor = base["as.factor"];
  for(int i=0; i<nrow; i++){
    NumericVector Vmat = mat(i,_);
    List RT = runs_test(as_factor(Vmat),alternative);
    RunTV[i] = RT["statistic"];
  }
  
  return RunTV;
}
