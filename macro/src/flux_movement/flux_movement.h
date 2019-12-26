#ifndef SRC_MARKOV_FLOW_H
#define SRC_MARKOV_FLOW_H

#include <exception>
#include <map>
#include <sstream>
#include <vector>
#include <variant>

#include "armadillo"
#include "boost/random/binomial_distribution.hpp"
#include "boost/random/mersenne_twister.hpp"
#include "boost/random/uniform_real_distribution.hpp"


namespace dd_harp {

using patch_id = int;
using human_id = int;
using clock_time = double;
using movement_sequence = std::vector<std::tuple<patch_id,clock_time>>;
using patch_sequence = std::vector<std::tuple<human_id, bool, clock_time>>;


struct no_parameter {};

using flux_movement_parameter = std::variant<no_parameter, int, arma::Mat<double>>;

class flux_movement;  // Forward declaration for friending.

class flux_movement_result {
public:
    patch_id starting_patch(human_id query) const;

    size_t human_count() const;

    /*!
     * For the human, the movement sequence will be a
     * set of events with predetermined times.
     *
     * @param query - Which human's movements we want.
     * @return movement_sequence - The set of patches and times.
     */
    movement_sequence
    movements_of_human(human_id query) const;

    patch_sequence
    duration_in_patch(patch_id query) const;

    void allocate(human_id human_count, patch_id patch_count);
    //! Remove events without resizing the event queue storage.
    void clear();
    friend class flux_movement;
private:
    std::vector<movement_sequence> human_location;
    std::vector<patch_sequence> patch_state;
};


/*! Markovian movement of individuals.
 *
 * Individuals have ids and are assigned to patches.
 * Each patch specifies the rate of movement to other patches.
 * The rate for an individual to leave a patch is the sum of
 * the rates to all other patches. The rate for any individual
 * to leave is that times the number of people in the patch.
 */
class flux_movement {
    // The result is a buffer that is owned by the machine,
    // so that it won't churn memory. It is read-only to others.
    flux_movement_result result;

public:
    void init(
            const std::map<std::string, flux_movement_parameter>& parameters,
            const std::vector<std::vector<int>>& initial_state
            );

    const flux_movement_result*
    step(double time_step);

private:
    bool initialized{false};
    boost::mt19937 rng;
    arma::Mat<double> flow_cumulant;
    arma::umat flow_index;
    arma::Col<double> patch_rate_with_people;
    arma::uvec patch_index;
    double total_rate;
    int patch_count;
    int human_count;
    std::vector<std::vector<int>> human_location;
};

} // namespace dd_harp
#endif //SRC_MARKOV_FLOW_H
