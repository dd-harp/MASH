// [[Rcpp::depends(BH)]]

#include <vector>
#include <sstream>

#include "boost/random/mersenne_twister.hpp"
#include "Rcpp.h"

using namespace boost;
using namespace Rcpp;
using namespace stl;


// [[Rcpp::export]]
NumericMatrix
skip_mersenne_twister(int seed, int skip_size_power_two, int skip_count) {
    mt19937<size_t> generator(seed);

    vector<size_t> seed_buffer(generator.state_size);
    NumericMatrix skipped_seeds(generator.state_size, skip_count);
    for (size_t skip_idx=0; skip_idx < skip_count; ++skip_idx) {
        if (skip_idx > 0) {
            generator.discard(1 << skip_size_power_two);
        } // else the first one doesn't need to be skipped.

        // Because Boost RNGs don't tell you their internal state.
        stringstream string_buffer;
        generator >> string_buffer;
        string_buffer.rewind();
        for (size_t read_int=0; read_int < generator.state_size; ++read_int) {
            string_buffer >> seed_buffer[read_int];
        }

        for (size_t state_idx=0; state_idx < generator.state_size; ++state_idx) {
            skipped_seeds(state_idx, skip_idx) = seed_buffer[state_idx];
        }
    }

    return skipped_seeds;
}
