/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  specialized random variate samplers not in base Rmath.h
 *
 *  Sean Wu (slwu89@berkeley.edu)
 *  July 2019
*/

#ifndef RNG_HPP
#define RNG_HPP

#include <RcppArmadillo.h>

// movement needs to sample a categorical distribution
// stupid simple inversion method (Devroye III.2)
int rcategorical(const arma::Row<double>& probs);

/* PDG requires a multivariate hypergeometric random number, which we adapt the algorithm from Agner Fog here (cite him!) */
// citation: Fog, A. "Non-uniform random number generators." (2005).
// destination: array to fill the drawn "balls"
// source: number of "balls" in each "urn"
// n: number of draws to take
// k: number of "urns"
void rmhyper(int* destination, int const* source, int n, int k);

#endif
