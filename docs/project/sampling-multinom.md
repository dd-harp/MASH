# Sampling Algorithms: Binary Tree Search

A fast method to do multinomial draws from travel rates is to form a binary search tree, embedded in a vector. Thanh describes this in:
TREE-BASED SEARCH FOR STOCHASTIC SIMULATION ALGORITHM, 2011. https://core.ac.uk/download/pdf/11830398.pdf

## Functions

#### Build the Binary Tree

```
std::tuple<arma::Col<double>, arma::uvec> build_binary_tree(const arma::Col<double> rates);
```

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
