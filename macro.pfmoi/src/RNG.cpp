#include "RNG.hpp"


int rcategorical(const arma::Row<double>& probs){
    int x = 0;
    if(probs.size() == 1){
        return x;
    }
    double s = probs.at(0);
    double u = R::runif(0.,1.);
    while(u > s){
        x += 1;
        s += probs.at(x);
    }
    return x;
};


void rmhyper(int* destination, int const* source, int n, int k){
    int sum=0, x=0, y=0;

    if(n < 0 || k < 0){Rcpp::stop("Invalid parameters of distribution");}

    // total number of "balls"
    for(int sum_idx = 0, sum = 0; sum_idx < k; ++sum_idx){
        y = source[sum_idx];
        if(y < 0){Rcpp::stop("Cannot have a negative number of balls in an urn");}
        sum += y;
    }
    if(n > sum){Rcpp::stop("Distribution undefined for n > sum");}

    int gen_idx = 0;
    for(; gen_idx<k-1; ++gen_idx){
        // generate output by calling rhyper k-1 times
        y = source[gen_idx];
        x = (int)R::rhyper((double)y,(double)sum-y,(double)n);
        n -= x;
        sum -= y;
        destination[gen_idx] = x;
    }
    // get the last one
    destination[gen_idx] = n;
};
