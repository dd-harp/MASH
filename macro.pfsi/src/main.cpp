/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  R interfacing code to run simulations
 *
 *  Sean Wu
 *  December 2018
*/

/* C++ includes */
#include <iostream>
#include <memory>

/* Rcpp includes */
#include <RcppArmadillo.h>

/* MACRO includes */
#include "Tile.hpp"


//' Run a MACRO :: PfSI (CoI) - RM (mosquito) Simulation
//'
//' Please note that this simulation routine uses the RNG exposed by R's C API, because of its known quality. Therefore to set the seed
//' prior to simulation, please use \code{link{set.seed}}.
//'
//' @param tmax a non-negative integer maximum simulation time (simulation runs from day 0 to tmax-1)
//' @param human_pars a list of constructor parameters for each human in the simulation
//' @param mosquito_pars a list of constructor parameters for the mosquito population
//' @param patch_pars a list of constructor parameters for each patch in the simulation
//' @param model_pars a list of parameter objects (see \code{\link{pars_obj}}) to initialize parameters object (see inst/include/Parameters.hpp for details)
//' @param log_streams a list giving the output file and name of each logging stream (please ensure that you have provided the right logging stream names for the model(s) you wish to run)
//' @param vaxx_events a list of vaccination events on the human population (for no vaccination events, pass an empty list, \code{list()})
//' @param verbose print a progress bar to console?
//'
//' @export
// [[Rcpp::export]]
void run_macro(
     const int            tmax,
     const Rcpp::List&    human_pars,
     const Rcpp::List&    mosquito_pars,
     const Rcpp::List&    patch_pars,
     const Rcpp::List&    model_pars,
     const Rcpp::List&    log_streams,
     const Rcpp::List&    vaxx_events,
     const bool           verbose = true
   )
{

  std::unique_ptr<tile> tileP = std::make_unique<tile>(human_pars,mosquito_pars,patch_pars,model_pars,log_streams,vaxx_events,verbose);

  tileP->simulation(tmax);

};
