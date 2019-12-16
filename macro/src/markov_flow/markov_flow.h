#ifndef SRC_MARKOV_FLOW_H
#define SRC_MARKOV_FLOW_H

#include <map>
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


struct no_parameter {};

using movement_machine_parameter = std::variant<no_parameter, int, arma::Row<double>>;


class movement_machine_result {
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

    movement_sequence
    duration_in_patch(human_id query) const;
};


class movement_machine {
    // The result is a buffer that is owned by the machine,
    // so that it won't churn memory. It is read-only to others.
    movement_machine_result result;

public:
    void init(
            const std::map<std::string, movement_machine_parameter>& parameters,
            const std::vector<std::vector<int>>& initial_state
            );

    const movement_machine_result*
    step(double time_step);

private:
    boost::mt19937 rng;
    arma::Row<double> flow_probability;
    int patch_count;
    int human_count;
};

std::tuple<arma::Row<double>, arma::uvec> prepare_rates(const arma::Row<double> rates);



template<typename RNG>
int choose_direction(const arma::Row<double>& cumulant, const arma::uvec& sorted_rates_index, RNG& rng) {
    double total_rate{cumulant[cumulant.n_elem - 1]};
    // This is how to do it.
    // https://core.ac.uk/download/pdf/11830398.pdf

    boost::random::uniform_real_distribution<double> chooser(0, total_rate);
    auto choice = chooser(rng);
    arma::uvec found_vec = find(cumulant > choice, 1);
    int chosen{0};
    if (found_vec.n_elem == 1) {
        chosen = sorted_rates_index[found_vec[0]];
    } else if (found_vec.n_elem == 0) {
        chosen = sorted_rates_index[sorted_rates_index.n_elem - 1];
    }
    return chosen;
}


} // namespace dd_harp
#endif //SRC_MARKOV_FLOW_H
