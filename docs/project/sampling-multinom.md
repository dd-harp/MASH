# Sampling Algorithms: Binary Tree Search

A fast method to do multinomial draws from travel rates is to form a binary search tree, embedded in a vector. Thanh describes this in:
TREE-BASED SEARCH FOR STOCHASTIC SIMULATION ALGORITHM, 2011. https://core.ac.uk/download/pdf/11830398.pdf

## Functions

The file `multinomial.h` contains several functions.

  * `prepare_rates`: the caller provides a vector of rates, and the function returns the sorted rates and their cumulative sum
  * `choose_direction`: using the output of `prepare_rates` and a Boost RNG object, choose one event and return its index
  * `build_binary_tree`: construct a binary tree of reaction propensities for sampling
  * `locate_in_binary_tree`: given a random value `r*a0` where `r ~ Unif(0,1)` and `a0` is the cumulative sum, sample an event stored in a binary tree via discrete binary search
  * `sample_binary_tree`: a convenient wrapper for `locate_in_binary_tree` taking the tree and its sorted rates index, and a Boost RNG object; it returns the index of the sampled event
  * `build_multinomial_matrix`: given a stochastic matrix with `n` rows and columns (cols sum to one), build `n` binary trees stored in an Armadillo matrix and their associated sorted rates.
  * `update_binary_tree`: if propensities change, update the tree

#### Build Multinomial Matrix

Given a stochastic matrix with `n` rows and columns (cols sum to one), build `n` binary trees stored in an Armadillo matrix and their associated sorted rates. This function is used to quickly setup the sampling trees for movement across the entire landscape.

```
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
```

#### Build the Binary Tree

The complete binary tree to hold `m` reaction propensities at the leaf level will have `2m - 1` elements in the array. The internal nodes above the leaves store the sums of their children. The children of a node at position `i` are found at `2i` amd `2i + 1`.

The function uses an array `tree` which stores those partial sums. `m` should be even to hold the tree in the array, and dummy reactions with propensity `0` are added as padding if that is not the case.

The input vector of rates is first sorted and stored in `sorted_rates`, and the ordering is stored in the vector `sorted_rates_index`.

Then `leaf_count` is calculated, which is `m` (accounting for additional padding). `tree_count`, the total size of the array needed to store the tree is also calculated. Then the tree may be constructed in the Armadillo column vector object `tree`.

Next there are two for loops which fill in the leaves and the zero padding. Finally the last for loop corresponds to **Algorithm 2** in Thanh et al. The tree and corresponding ordering used to build it is returned in a tuple object.

```
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
```

#### Locate in Binary Tree

Thanh et al. call this "Finding the next reaction firing" in **Algorithm 3**. If `a0` is the sum of all rates/propensities and `r` is distributed at Uniform(0,1) then it `r*a0` is the value we are looking for in the discrete search, so that over repeated sampling each leaf `j` will be selected with the proper probability `aj/a0`.

```
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
```

#### Update System State

After a tree is built we may want to update (replace) the values of some leaves. To update the cumulative sums (internal nodes) we bank on the fact that the parent of node `i` is located at `floor(i/2)`.

We pass the required updates as a vector of tuples, telling us what reaction now fires with what propensity.

```
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
```
