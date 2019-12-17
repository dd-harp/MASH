#include <cassert>
#include <vector>

#include "boost/random/exponential_distribution.hpp"

#include "markov_flow.h"

namespace dd_harp {
/*!
 * The sampling trajectory of the movement machine
 * tells the human when to change biting weight on
 * a patch and where they have hazard of being
 * bitten.
 */
    patch_id movement_machine_result::starting_patch(human_id query) const { return 4; };

    size_t movement_machine_result::human_count() const { return 10; }

/*!
 * For the human, the movement sequence will be a
 * set of events with predetermined times.
 *
 * @param query - Which human's movements we want.
 * @return movement_sequence - The set of patches and times.
 */
    movement_sequence
    movement_machine_result::movements_of_human(human_id query) const {
        return {{3, 0.02},
                {4, 0.05},
                {3, 0.8}};
    }


    movement_sequence
    movement_machine_result::duration_in_patch(human_id query) const {
        return {{4, .77},
                {3, .23}};
    }


    void movement_machine::init(
            const std::map <std::string, movement_machine_parameter> &parameters,
            const std::vector<std::vector<int>> &initial_state
    ) {
        this->human_count = std::get<int>(parameters.at("human_count"));
        arma::Mat<double> flow_probability = std::get<arma::Mat<double>>(parameters.at("flow_probability"));
        this->patch_count = flow_probability.n_rows;
        this->flow_cumulant.zeros(this->patch_count, this->patch_count);
        this->flow_index.zeros(this->patch_count, this->patch_count);
        for (int row_idx=0; row_idx < this->patch_count; ++row_idx) {
            auto [row_cumulant, row_index] = prepare_rates(flow_probability.row(row_idx));
            this->flow_cumulant.row(row_idx) = row_cumulant;
            this->flow_index.row(row_idx) = row_index;
        }
        // Assign per-patch rate = (# people in patch) x (total movement rate from patch)
        // Calculate total rate over all.
        this->human_location = initial_state;
    }


    const movement_machine_result *
    movement_machine::step(double time_step) {
        double time_within_step{0};
        while (time_within_step < time_step) {
            double total_rate{10};  // Use calculated total rate over all.
            double dt = boost::random::exponential_distribution<double>(total_rate)(this->rng);
            // Choose among patches using the per-patch rate.
            // Pick a person.
            // Choose where they go using the multinomial choose_direction().
            // Calculate updates to per-patch rate and total due to moving person.
            time_within_step += dt;
        }

        return &result;
    }


    std::tuple <arma::Row<double>, arma::uvec> prepare_rates(const arma::Row<double> rates) {
        arma::uvec sorted_rates_index = arma::sort_index(rates);
        arma::Row<double> sorted_rates(rates.n_elem);
        for (int copy_sorted = 0; copy_sorted < rates.n_elem; ++copy_sorted) {
            sorted_rates[copy_sorted] = rates[sorted_rates_index[copy_sorted]];
        }
        arma::Row<double> cumulant(sorted_rates.n_elem);

        double total_rate{0};
        for (size_t add_idx = 0; add_idx < cumulant.n_elem; ++add_idx) {
            total_rate += sorted_rates[add_idx];
            cumulant[add_idx] = total_rate;
        }
        return {cumulant, sorted_rates_index};
    }

    /*! p, such that 2^p <= n.
     *
     * @param n
     * @return
     */
    int next_power_of_two(int n) {
        int i = 0;
        while (n != 0) {
            n >>= 1;
            ++i;
        }
        return i;
    }

    /*! Builds a binary tree for rates, embedded in an single vector.
     *
     * If the rates aren't a power of two, they are padded with zeroes.
     * The returned tree should have rate[n] = rate[2n] + rate[2n+1].
     *
     * @param rates
     * @return
     */
    std::tuple<arma::Row<double>, arma::uvec>
            build_binary_tree(const arma::Row<double> rates) {
        arma::uvec sorted_rates_index = arma::sort_index(rates);
        arma::Row<double> sorted_rates(rates.n_elem);
        for (int copy_sorted = 0; copy_sorted < rates.n_elem; ++copy_sorted) {
            sorted_rates[copy_sorted] = rates[sorted_rates_index[copy_sorted]];
        }
        int leaf_power{next_power_of_two(sorted_rates.n_elem)};
        int leaf_count{1 << leaf_power};
        int tree_count{2 * leaf_count - 1};
        assert(leaf_count >= sorted_rates.n_elem);
        assert(leaf_count / 2 < sorted_rates.n_elem);
        arma::Row<double> tree(tree_count);

        // The last leaf_count entries are leaves.
        for (int leaf_index = 0; leaf_index < sorted_rates.n_elem; ++leaf_index) {
            tree[leaf_count + leaf_index - 1] = sorted_rates[leaf_index];
        }
        // Some leaf values are zero because they are padding.
        for (int zero_leaves = sorted_rates.n_elem; zero_leaves < leaf_count; ++zero_leaves) {
            tree[leaf_count + zero_leaves - 1] = 0;
        }

        // Branches sum leaves in a binary tree.
        for (int tier_index = leaf_power - 1; tier_index >= 0; --tier_index) {
            int base{1 << tier_index};
            for (int branch_index = 0; branch_index < base; ++branch_index)
            {
                int n = base + branch_index;
                assert(n < leaf_count - 1);
                tree[n - 1] = tree[2 * n - 1] + tree[2 * n];
            }
        }
        return {tree, sorted_rates_index};
    }
}
