// [[Rcpp::depends(BH)]]

#include <vector>

#include "armadillo"
#include "boost/property_map/property_map.hpp"
#include "Rcpp.h"

#include "markov_flow.h"

// We can't put movement_init into namespace dd_harp because Rcpp's
// wrapping functions don't pick up the namespace.
using namespace dd_harp;
using namespace Rcpp;

/*!
 * From R, this will look like
 *     movement <- movement_init(params)
 *     time_step <- 0.1
 *     moves <- movement_step(movement, time_step)
 *     person <- human_model(moves, time_step)
 */
// [[Rcpp::export]]
List movement_init(List parameters, List initial_location) {
    auto movement = new movement_machine{};
    std::map<std::string, movement_machine_parameter> cparameters;
    int human_count = as<int>(parameters["human_count"]);
    cparameters["human_count"] = human_count;
    int patch_count = as<int>(parameters["patch_count"]);
    cparameters["patch_count"] = patch_count;
    NumericVector flux_matrix = parameters["flow_probability"];
    // Make one copy of the matrix and then work with a reference to it.
    cparameters["flow_probability"] = arma::Mat<double>(patch_count, patch_count);
    arma::Mat<double>& flow_probability = get<arma::Mat<double>>(cparameters["flow_probability"]);
    for (int from_idx = 0; from_idx < patch_count; ++from_idx) {
        for (int to_idx = 0; to_idx < patch_count; ++to_idx) {
            flow_probability(to_idx, from_idx) = flux_matrix[from_idx * patch_count + to_idx];
        }
    }

    std::vector<std::vector<int>> initial_state{patch_count};
    NumericVector locations = initial_location["human_locations"];
    // Check locations length is same as humans.
    for (int place_idx = 0; place_idx < human_count; ++place_idx) {
        initial_state[locations[place_idx]].push_back(place_idx);
    }
    movement->init(cparameters, initial_state);

    XPtr<movement_machine> handle_ptr(movement);
    auto movement_object = List::create(
            Named("handle") = handle_ptr,
            Named("parameters") = parameters
    );
    movement_object.attr("class") = CharacterVector::create("MovementModel", "MovementModel");
    return movement_object;
}


// [[Rcpp::export]]
List movement_step(List module, NumericVector time_step) {
    auto movement_machine_handle = as<XPtr<movement_machine>>(module["handle"]);
    auto result = movement_machine_handle->step(as<double>(time_step));
    return List::create(
            Named("handle") = XPtr<const movement_machine_result>(result)
    );
}


/*!
 * This approach copies all data out of C++ into the R space because it's
 * the transition from R - C++ and back that takes all the time.
 *
 * @param movement_list - This gets modified in place, so that it now has
 *     a list of trajectories for each person, enumerated by the person id.
 */
// [[Rcpp::export]]
void convert_to_r_movement(List movement_list, Rcpp::IntegerVector human) {
    auto result = as<XPtr<const movement_machine_result>>(movement_list["handle"]);
    Rcpp::List moves;
    for (int person_idx = 0; person_idx < result->human_count(); ++person_idx) {
        auto sequence = result->movements_of_human(human[0]);
        // auto sequence = result->movements_of_human(0);
        Rcpp::NumericMatrix vector(sequence.size(),2);
        for (int i = 0; i < sequence.size(); ++i) {
            vector[i, 0] = std::get<0>(sequence[i]);
            vector[i, 1] = std::get<1>(sequence[i]);
        }
        moves.push_back(Rcpp::clone(vector));
    }
    movement_list.push_back(Rcpp::clone(moves));
    movement_list.names() = Rcpp::CharacterVector::create("handle","moves");
    // movement_list[CharacterVector::create("moves")] = moves;
}


/*!
 * When you ask for movements for a particular human using R, it translates
 * the sample trajectory into an R list for easier processing.
 *
 * @param movement_list
 * @param human
 * @return
 */
// [[Rcpp::export]]
NumericVector movements_of_human(List movement_list, IntegerVector human) {
    auto result = as<XPtr<const movement_machine_result>>(movement_list["handle"]);
    auto sequence = result->movements_of_human(human[0]);
    // auto vector = NumericVector(Dimension(sequence.size(), 2));
    Rcpp::NumericMatrix vector(sequence.size(),2);
    for (int i=0; i < sequence.size(); ++i) {
        vector[i, 0] = std::get<0>(sequence[i]);
        vector[i, 1] = std::get<1>(sequence[i]);
    }
    return vector;
}


/*!
 * We need a method that takes R simulation of movement and converts
 * it into a *subclass of* a C++ movement_machine_result.
 */
