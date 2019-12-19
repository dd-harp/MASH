
#include <cassert>
#include <unordered_set>
#include <vector>

#include "boost/random/exponential_distribution.hpp"

#include "multinomial.h"

namespace dd_harp {

    std::tuple <arma::Col<double>, arma::uvec> prepare_rates(const arma::Col<double> rates) {
        arma::uvec sorted_rates_index = arma::sort_index(rates);
        arma::Col<double> sorted_rates(rates.n_elem);
        for (int copy_sorted = 0; copy_sorted < rates.n_elem; ++copy_sorted) {
            sorted_rates[copy_sorted] = rates[sorted_rates_index[copy_sorted]];
        }
        arma::Col<double> cumulant(sorted_rates.n_elem);

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
    std::tuple<arma::Col<double>, arma::uvec>
    build_binary_tree(const arma::Col<double> rates) {
        arma::uvec sorted_rates_index = arma::sort_index(rates);
        arma::Col<double> sorted_rates = rates.elem(sorted_rates_index);
        for (int copy_sorted = 0; copy_sorted < rates.n_elem; ++copy_sorted) {
            sorted_rates[copy_sorted] = rates[sorted_rates_index[copy_sorted]];
        }
        int leaf_power{next_power_of_two(sorted_rates.n_elem)};
        int leaf_count{1 << leaf_power};
        int tree_count{2 * leaf_count - 1};
        assert(leaf_count >= sorted_rates.n_elem);
        assert(leaf_count / 2 < sorted_rates.n_elem);
        arma::Col<double> tree(tree_count);

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
            auto [single_tree, single_index] = build_binary_tree(flow_probability.col(col_index));
            tree_matrix.col(col_index) = single_tree;
            sorted_rates_index.col(col_index) = single_index;
        }
        return {tree_matrix, sorted_rates_index};
    }


    int locate_in_binary_tree(const arma::Col<double>& tree, double choice) {
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
            arma::Col<double>& tree,
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