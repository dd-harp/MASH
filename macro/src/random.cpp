// [[Rcpp::depends(BH)]]

#include <vector>
#include <sstream>

#include "boost/random/mersenne_twister.hpp"
#include "Rcpp.h"

using namespace boost;
using namespace Rcpp;
using namespace std;

const size_t MASK31 = (1 >> 31);

int twos_complement(size_t val) {
    return static_cast<int>(
        -(val & MASK31) + (val & ~MASK31)
    );
}


//' Generate Mersenne Twister seed states skipped ahead by powers of two.
//'
//' A Mersenne Twister random number generator can be skipped ahead
//' by large numbers of random numbers. This function uses the Boost
//' implementation of that algorithm to skip ahead its version of
//' mt19937, not mt19937_64.
//'
//' @param seed A single number, without too many 0 bits, to
//'        initialize the random number generator.
//' @param skip_size_power_two The jump will be two raised to this
//'        power. If it is 14 or greater, it will use the
//'        special algorithm.
//' @param skip_count How many times to skip forward. This is
//'        the number of simultaneous streams you want to use.
//' @return A matrix of size 624 by skip_count. Put the 624
//'         into the Mersenne seed in order to initialize it.
//'
//' @export
// [[Rcpp::export]]
IntegerMatrix
skip_mersenne_twister(int seed, int skip_size_power_two, int skip_count) {
    mt19937 generator(seed);

    vector<size_t> seed_buffer(generator.state_size);
    IntegerMatrix skipped_seeds(generator.state_size, skip_count);
    for (int skip_idx=0; skip_idx < skip_count; ++skip_idx) {
        if (skip_idx > 0) {
            generator.discard(1 << skip_size_power_two);
        } // else the first one doesn't need to be skipped.

        // Because Boost RNGs don't tell you their internal state,
        // but they will print it.
        stringstream string_buffer;
        string_buffer << generator;
        string_buffer.seekg(0);
        for (int read_int=0; read_int < generator.state_size; ++read_int) {
            string_buffer >> seed_buffer[read_int];
        }

        for (int state_idx=0; state_idx < generator.state_size; ++state_idx) {
            skipped_seeds(state_idx, skip_idx) = twos_complement(seed_buffer[state_idx]);
        }
    }

    return skipped_seeds;
}
