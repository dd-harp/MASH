// [[Rcpp::depends(BH)]]

#include <vector>

#include "armadillo"
#include "boost/property_map/property_map.hpp"
#include "Rcpp.h"

#include "flux_movement.h"

// We can't put movement_init into namespace dd_harp because Rcpp's
// wrapping functions don't pick up the namespace.
using namespace dd_harp;
using namespace Rcpp;

//' Initialize Flux Movement module
//'
//' This function returns a flux movement module.
//'
//' @param parameters A list containing random_seed, random_stream,
//'        human_count, patch_count, and a flow_probability vector.
//' @param initial_location A vector saying in which patch each human is located.
//'
//' @examples
//' \dontrun{
//' movement <- movement_init(params)
//' time_step <- 0.1
//' moves <- movement_step(movement, time_step)
//' person <- human_model(moves, time_step)
//' }
//'
//' @export
// [[Rcpp::export]]
List flux_movement_init(List parameters, List initial_location) {
    auto movement = new flux_movement{};
    std::map<std::string, flux_movement_parameter> cparameters;
    int random_seed = as<int>(parameters["random_seed"]);
    cparameters["random_seed"] = random_seed;
    int random_stream = as<int>(parameters["random_stream"]);
    cparameters["random_stream"] = random_stream;
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

    int zero_based_indexing{-1};
    std::vector<std::vector<int>> initial_state{patch_count};
    NumericVector locations = initial_location["human_locations"];
    // Check locations length is same as humans.
    for (int place_idx = 0; place_idx < locations.size(); ++place_idx) {
        initial_state[locations[place_idx] + zero_based_indexing].push_back(place_idx);
    }
    movement->init(cparameters, initial_state);

    XPtr<flux_movement> handle_ptr(movement);
    auto movement_object = List::create(
            Named("handle") = handle_ptr,
            Named("parameters") = parameters
    );
    movement_object.attr("class") = CharacterVector::create("MovementModel", "MovementModel");
    return movement_object;
}


//' Time step for a flux movement module.
//'
//' @param module The flux movement list
//' @param time_step A floating point time.
//' @return A result list that is opaque.
//'
//' @export
// [[Rcpp::export]]
List flux_movement_step(List module, NumericVector time_step) {
    auto flux_movement_handle = as<XPtr<flux_movement>>(module["handle"]);
    auto result = flux_movement_handle->step(as<double>(time_step));
    return List::create(
            Named("handle") = XPtr<const flux_movement_result>(result)
    );
}


//' Convert movement result to R data
//'
//' The result of a movement object is in C++. This extracts
//' it into an R list. The result is added to the original
//' list that was passed in.
//'
//' @param movement The movement module object
//' @param human An integer vector of which human positions to extract.
//' @returns Movement list with a "moves" list that has human locations.
//'
//' @export
// [[Rcpp::export]]
List convert_to_r_movement(List movement, IntegerVector human) {
    using result_ptr = XPtr<const flux_movement_result>;
    result_ptr result = as<result_ptr>(movement["handle"]);
    Rcpp::List moves;
    for (int person_idx = 0; person_idx < human.size(); ++person_idx) {
        int human_idx = human[person_idx];
        if (human_idx > result->human_count()) {
            std::stringstream msg;
            msg << "Looking for a human past end of list " << human_idx
                << " in a list of length " << result->human_count();
            throw std::runtime_error(msg.str());
        }
        const int zero_based_indexing{-1};
        auto sequence = result->movements_of_human(human_idx + zero_based_indexing);
        Rcpp::NumericMatrix vector(sequence.size(),2);
        for (int i = 0; i < sequence.size(); ++i) {
            vector(i, 0) = std::get<0>(sequence[i]);  // patch_id
            vector(i, 1) = std::get<1>(sequence[i]);  // clock_time
        }
        moves.push_back(Rcpp::clone(vector));
    }
    Rcpp::List movement_list;
    movement_list["handle"] = result;
    movement_list["moves"] = moves;
    return movement_list;
}


//' Extract human movements from the result of a movement step.
//'
//' @param movement_list The movement result object.
//' @param human A list of the humans for whom you want the movements.
//' @return A vector that's two wide.
//'
//' @export
// [[Rcpp::export]]
NumericVector movements_of_human(List movement_list, IntegerVector human) {
    auto result = as<XPtr<const flux_movement_result>>(movement_list["handle"]);
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
 * it into a *subclass of* a C++ flux_movement_result.
 */
