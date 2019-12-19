#include <cassert>
#include <unordered_set>
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
        return this->human_location[query];
    }


    patch_sequence
    movement_machine_result::duration_in_patch(patch_id query) const {
        return this->patch_state[query];
    }


    void movement_machine_result::allocate(human_id human_count, patch_id patch_count) {
        this->human_location.resize(human_count);
        this->patch_state.resize(patch_count);
    }

    void movement_machine::init(
            const std::map <std::string, movement_machine_parameter> &parameters,
            const std::vector<std::vector<int>> &initial_state
    ) {
        this->human_count = std::get<int>(parameters.at("human_count"));
        arma::Mat<double> flow_probability = std::get<arma::Mat<double>>(parameters.at("flow_probability"));
        this->patch_count = flow_probability.n_rows;
        if (this->patch_count < 1) {
            std::stringstream msg;
            msg << "Patch count should be one or more but is " << patch_count;
            throw std::runtime_error(msg.str());
        }
        // Calculate total rate over all.
        this->human_location = initial_state;
        auto [search_matrix, index_matrix] = build_multinomial_matrix(flow_probability);
        this->flow_cumulant = search_matrix;
        this->flow_index = index_matrix;
        arma::Col<double> patch_rate_with_people(patch_count);
        // Assign per-patch rate = (# people in patch) x (total movement rate from patch)
        for (int patch_rate_index = 0; patch_rate_index <  patch_count; ++patch_rate_index) {
            patch_rate_with_people[patch_rate_index] = (
                    flow_cumulant(0, patch_rate_index) * human_location[patch_rate_index].size()
                    );
        }
        auto [rate_with_people, patch_index] = build_binary_tree(patch_rate_with_people.t());
        this->patch_rate_with_people = rate_with_people.t();
        this->patch_index = patch_index;
        this->total_rate = arma::sum(patch_rate_with_people);
        if (this->total_rate <= 0) {
            std::stringstream msg;
            msg << "The rate for flow should be positive but is " << total_rate;
            throw std::runtime_error(msg.str());
        }
        this->result.allocate(this->human_count, this->patch_count);
        this->initialized = true;
    }


    const movement_machine_result *
    movement_machine::step(double time_step) {
        if (!this->initialized) {
            throw std::runtime_error("You must initialize the class before stepping.");
        }
        double time_within_step{0};
        while (time_within_step < time_step) {
            double dt = boost::random::exponential_distribution<double>(total_rate)(this->rng);
            // Choose among patches using the per-patch rate.
            arma::Row<double> binary_encoding = this->patch_rate_with_people.t();
            int source_patch = sample_binary_tree(binary_encoding, this->patch_index, this->rng);
            // Choose where they go using the multinomial choose_direction().
            int destination_patch = sample_binary_tree(
                   this->flow_cumulant.col(source_patch).t(),
                    this->flow_index.col(source_patch),
                    this->rng
                    );
            // Pick a person.
            int who_index = boost::random::uniform_int_distribution<int>(
                    0, this->human_location[source_patch].size() - 1)(this->rng);
            this->human_location[destination_patch].push_back(who_index);
            this->human_location[source_patch].erase(human_location[source_patch].begin() + who_index);
            // Calculate updates to per-patch rate and total due to moving person.
            std::vector<std::tuple<int, double>> updates = {
                    {destination_patch, flow_cumulant(0, destination_patch)},
                    {source_patch, -flow_cumulant(0, source_patch)}
            };
            // Update the binary tree with new rates. Faster to do both at once.
            // update_binary_tree(patch_rate_with_people, updates);
            total_rate += flow_cumulant(0, destination_patch) - flow_cumulant(0, source_patch);

            time_within_step += dt;
            // Record the change.
            this->result.human_location[who_index].push_back({destination_patch, time_within_step});
            this->result.patch_state[source_patch].push_back({who_index, false, time_within_step});
            this->result.patch_state[destination_patch].push_back({who_index, true, time_within_step});
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

    /*! p, such that 2^p <= n and 2^(p-1) < n
     *
     * @param n
     * @return
     */
    int next_power_of_two(int n) {
        int i{0};
        while ((1 << i) < n) {
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
        arma::Row<double> sorted_rates = rates.elem(sorted_rates_index).t();
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

        for (int walk_idx = leaf_count - 2; walk_idx >= 0; --walk_idx) {
            int n{2 * walk_idx + 1};
            tree[walk_idx] = tree[n] + tree[n + 1];
        }
        return {tree, sorted_rates_index};
    }


    std::tuple<arma::Mat<double>, arma::Mat<arma::uword>>
            build_multinomial_matrix(const arma::Mat<double>& flow_probability) {
        if (flow_probability.n_cols != flow_probability.n_rows) {
            std::stringstream msg;
            msg << "Expect the flow probability to be square but it is " <<
                flow_probability.n_cols << "x" << flow_probability.n_rows;
            throw std::runtime_error(msg.str());
        }
        int patch_count = flow_probability.n_cols;
        int leaf_count = (1 << next_power_of_two(patch_count));
        arma::Mat<double> tree_matrix(2 * leaf_count - 1, patch_count);
        arma::Mat<arma::uword> sorted_rates_index(patch_count, patch_count);
        for (int col_index = 0; col_index < patch_count; ++col_index) {
            auto [single_tree, single_index] = build_binary_tree(flow_probability.col(col_index).t());
            tree_matrix.col(col_index) = single_tree.t();
            sorted_rates_index.col(col_index) = single_index;
        }
        return {tree_matrix, sorted_rates_index};
    }


    int locate_in_binary_tree(const arma::Row<double>& tree, double choice) {
        if (tree.n_elem == 1) {
            // This changes the incoming invariant to guarantee we have
            // at least two levels.
            return 0;
        }
        int leaf_count = (tree.n_elem + 1) / 2;

        int n{0};
        while (n < leaf_count - 1) {
            if (choice < tree[2 * n + 1]) {
                n = 2 * n + 1;
            } else {
                choice -= tree[2 * n + 1];
                n = 2 * n + 2;
            }
        }

        return n - leaf_count + 1;
    }

    //! Given a binary tree, update changes to several rates.
    //! The updates are new values, not changes to values.
    void update_binary_tree(
            arma::Row<double>& tree,
            const std::vector<std::tuple<int, double>>& updates
            ) {
        int leaf_count = (tree.n_elem + 1) / 2;
        std::array<std::unordered_set<int>, 2> changes;
        int change_idx{0};
        for (auto [index, value]: updates) {
            int n{leaf_count + index - 1};
            tree[n] = value;
            changes[change_idx].emplace((n - 1) / 2);
        }
        while (!changes[change_idx].empty()) {
            for (auto index: changes[change_idx]) {
                int left{2 * index + 1};
                tree[index] = tree[left] + tree[left + 1];
                if (index > 0) {
                    changes[1 - change_idx].emplace((index - 1) / 2);
                }
            }
            changes[change_idx].clear();
            change_idx = 1 - change_idx;
        }
    }
}
