#ifndef MASH_MULTINOMIAL_H
#define MASH_MULTINOMIAL_H


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
//! p such that 2^(p-1) < n <= 2^p.
    int next_power_of_two(int n);


/*! Create a data structure to enable a multinomial draw from travel rates.
 *
 * This assumes a person is going to one of the given places.
 * It sorts the rates and then takes their cumulative sum.
 * Later, the choose_direction() function can draw from that cumulative
 * sum.
 *
 * A faster method is to form a binary search tree, embedded in a vector.
 * Thanh describes this in TREE-BASED SEARCH FOR STOCHASTIC SIMULATION ALGORITHM, 2011.
 * https://core.ac.uk/download/pdf/11830398.pdf
 *
 * @param rates These are rates per unit time.
 * @return
 */
    std::tuple<arma::Col<double>, arma::uvec> prepare_rates(const arma::Col<double> rates);

/*! Given a cumulative sum of an indexed vector, draw a random number to pick one index.
 *
 * @tparam RNG Random number generator.
 * @param cumulant A vector of cumulative sums, so it's nondecreasing.
 * @param sorted_rates_index The index into the original vector.
 * @param rng The random number generator object.
 * @return One integer, the one that was chosen.
 */
    template<typename RNG>
    int choose_direction(const arma::Col<double> &cumulant, const arma::uvec &sorted_rates_index, RNG &rng) {
        double total_rate{cumulant[cumulant.n_elem - 1]};
        boost::random::uniform_real_distribution<double> chooser(0, total_rate);
        auto choice = chooser(rng);
        arma::uvec found_vec = find(cumulant >= choice, 1);
        int chosen{0};
        if (found_vec.n_elem == 1) {
            chosen = sorted_rates_index[found_vec[0]];
        } else if (found_vec.n_elem == 0) {
            // Some random number generators will draw the upper bound
            // of the range you give. In that case, we return the last index allowed.
            chosen = sorted_rates_index[sorted_rates_index.n_elem - 1];
        } else {
            throw std::runtime_error("Cannot have more than one found value.");
        }
        return chosen;
    }

    std::tuple<arma::Col<double>, arma::uvec>
    build_binary_tree(const arma::Col<double> rates);

    int locate_in_binary_tree(const arma::Col<double> &tree, double choice);

    template<typename RNG>
    int sample_binary_tree(const arma::Col<double> &tree, const arma::uvec &sorted_rates_index, RNG &rng) {
        int leaf_count = (tree.n_elem + 1) / 2;
        if (leaf_count < sorted_rates_index.n_elem) {
            std::stringstream msg;
            msg << "The tree and its index should match: ";
            msg << tree.n_elem << " index " << sorted_rates_index.n_elem;
            throw std::runtime_error(msg.str());
        }
        double total_rate{tree[0]};
        boost::random::uniform_real_distribution<double> chooser(0, total_rate);
        auto choice = chooser(rng);
        if (choice >= total_rate) {
            // The RNG can return a number at or near the total rate. Avoid that.
            return sorted_rates_index[sorted_rates_index.n_elem - 1];
        }
        return sorted_rates_index[locate_in_binary_tree(tree, choice)];
    }

    std::tuple<arma::Mat<double>, arma::Mat<arma::uword>>
    build_multinomial_matrix(const arma::Mat<double> &flow_probability);

    void update_binary_tree(
            arma::Col<double> &tree,
            const std::vector<std::tuple<int, double>> &updates
    );
}
#endif //MASH_MULTINOMIAL_H
