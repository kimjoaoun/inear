#include <Rcpp.h>
#include <iomanip>
#include <OpenXLSX/OpenXLSX.h>

using namespace Rcpp;
using namespace std;
using namespace OpenXLSX;



// [[Rcpp::export]]

DataFrame df = DataFrame::create();


/*** R
timesTwo(42)
*/
