#include <Rcpp.h>
#include <unordered_set>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector greedy_rcpp(IntegerVector x, IntegerVector y, NumericVector w, 
    bool include_ties = false) {
  std::unordered_map<int, double> xseen;
  std::unordered_map<int, double> yseen;
  LogicalVector res(x.length());
  const int n = x.length();
  if (y.length() != n)
    throw std::runtime_error("Lengths of x and y do not match.");
  if (w.length() != n)
    throw std::runtime_error("Lengths of x and w do not match.");
  for (int i = 0; i < n; ++i) {
    const auto xindex = xseen.find(x[i]);
    const bool xok = (xindex == xseen.end()) || (include_ties && xindex->second <= w[i]);
    const auto yindex = yseen.find(y[i]);
    const bool yok = (yindex == yseen.end()) || (include_ties && yindex->second <= w[i]);
    if (xok && yok) {
      xseen[x[i]] = w[i];
      yseen[y[i]] = w[i];
      res[i] = true;
    } else {
      res[i] = false;
    } 
  }
  return res;
}

// [[Rcpp::export]]
LogicalVector greedy_nm_rcpp(IntegerVector x, IntegerVector y, NumericVector w, 
    int n = 1, int m = 1) {
  std::unordered_map<int, int> xseen;
  std::unordered_map<int, int> yseen;
  LogicalVector res(x.length());
  const int nrecords = x.length();
  if (y.length() != nrecords)
    throw std::runtime_error("Lengths of x and y do not match.");
  if (w.length() != nrecords)
    throw std::runtime_error("Lengths of x and w do not match.");
  for (int i = 0; i < nrecords; ++i) {
    const auto xindex = xseen.find(x[i]);
    const bool xok = (xindex == xseen.end()) || (xindex->second < n);
    const auto yindex = yseen.find(y[i]);
    const bool yok = (yindex == yseen.end()) || (yindex->second < m);
    if (xok && yok) {
      xseen[x[i]] = xseen[x[i]] + 1;
      yseen[y[i]] = yseen[x[i]] + 1;
      res[i] = true;
    } else {
      res[i] = false;
    } 
  }
  return res;
}

/*
LogicalVector greedy_rcpp(IntegerVector x, IntegerVector y) {
  std::unordered_set<int> xseen;
  std::unordered_set<int> yseen;
  LogicalVector res(x.length());
  int n = x.length();
  if (y.length() != n)
    throw std::runtime_error("Lengths of x and y do not match.");
  for (int i = 0; i < n; ++i) {
    if ((xseen.find(x[i]) == xseen.end()) && \
        (yseen.find(y[i]) == yseen.end())) {
      xseen.insert(x[i]);
      yseen.insert(y[i]);
      res[i] = true;
    } else res[i] = false;
  }
  return res;
}
*/
