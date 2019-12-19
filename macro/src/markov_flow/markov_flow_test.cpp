#include <cmath>
#include <map>
#include <sstream>
#include <string>
#include <tuple>
#include <variant>
#include <vector>

#include "armadillo"
#include "boost/math/distributions/normal.hpp"
#include "boost/property_map/property_map.hpp"
#include "gtest/gtest.h"

#include "markov_flow.h"

using namespace std;
using namespace boost;


TEST(MarkovFlowTest, SmokeTest) {
    map<string, double> parameters;
    parameters["human_count"] = 100;
    associative_property_map parameter_map{parameters};
    auto m = dd_harp::movement_machine{};
    // m.init(parameter_map);
    auto result = m.step(0.1);
}



TEST(MarkovFlowTest, VariantWorks) {
    using no_parameter = dd_harp::no_parameter;
    using parameter_type = variant<no_parameter, int, double>;
    std::map<string, parameter_type> parameters;
    parameters["human_count"] = 100;
    parameters["base_rate"] = 3.7;
    double base_rate_one = std::get<double>(parameters["base_rate"]);
    associative_property_map parameter_map{parameters};
    auto base_variant = get(parameter_map, "base_rate");
    double base_rate_two = get<double>(base_variant);
    double base_rate_three = get<double>(get(parameter_map, "base_rate"));
    auto missing = get(parameter_map, "blah");
    bool blah_missing = (missing.index() == 0);
    EXPECT_TRUE(blah_missing);
}


std::tuple<double, double> wilson_score_interval(double p, double n, double confidence) {
    double z = quantile(boost::math::normal(), 0.5 * (confidence + 1));
    double mid = p + z * z / (2 * n);
    double diff = z * std::sqrt((p * (1 - p) + z * z / (4 * n)) / n);
    double denominator = 1 + z * z / n;
    return {(mid - diff) / denominator, (mid + diff) / denominator};
}


// Test against values from a paper.
TEST(MarkovFlowTest, WilsonMatchesAPaperResult) {
    auto [low, high] = wilson_score_interval(.4667, 15, .95);
    double epsilon = 1e-4;
    EXPECT_LT(abs(low - .2481), epsilon);
    EXPECT_LT(abs(high - .6988), epsilon);
}


TEST(MarkovFlowTest, DrawMultinomial) {
    boost::mt19937 rng(234234243);
    arma::Row<double> given_rates = {0.01, 0.8, 0.19};
    arma::Row<double> rates = 10 * given_rates; // To ensure scaling to max works.
    auto [cumulant, sorted_rates_index] = dd_harp::prepare_rates(rates);
    arma::Row<double> histogram(rates.n_elem);
    histogram.zeros();

    int draw_cnt{1000000};
    for (int draw = 0; draw < draw_cnt ; ++draw) {
        int chosen = dd_harp::choose_direction(cumulant, sorted_rates_index, rng);
        histogram[chosen] += 1;
    }
    for (int check_idx = 0; check_idx < given_rates.n_elem; ++check_idx) {
        auto [low, high] = wilson_score_interval(given_rates[check_idx], draw_cnt, 0.95);
        EXPECT_GT(histogram[check_idx] / draw_cnt, low);
        EXPECT_LT(histogram[check_idx] / draw_cnt, high);
    }
}



TEST(BinaryTreeMultinomial, PowerOfTwo) {
    std::vector<std::tuple<int,int>> inout = {
            {0, 0}, {1, 0}, {2, 1}, {3, 2}, {4, 2},
            {5, 3}, {6, 3}, {7, 3}, {8, 3}, {9, 4}};
    for (auto [value, result]: inout) {
        EXPECT_EQ(result, dd_harp::next_power_of_two(value));
    }
}


TEST(BinaryTreeMultinomial, TreeFilled) {
    double epsilon{1e-5};
    double fill{0.32792342};
    arma::Row<double> given_rates(3);
    given_rates.fill(fill);
    auto [tree, sorted_rates_index] = dd_harp::build_binary_tree(given_rates);
    std::vector<double> expected = {3 * fill, 2 * fill, fill, fill, fill, fill, 0};
    for (int check_idx = 0; check_idx < expected.size(); ++check_idx) {
        EXPECT_FLOAT_EQ(tree[check_idx], expected[check_idx]);
    }
}


TEST(BinaryTreeMultinomial, TreeTotalIsCorrect) {
    double epsilon{1e-5};
    double fill{0.32792342};
    for (int n = 1; n < 9; ++n) {
        arma::Row<double> given_rates(n);
        given_rates.fill(fill);
        auto [tree, sorted_rates_index] = dd_harp::build_binary_tree(given_rates);
        EXPECT_LT(std::abs(tree[0] - n * fill), epsilon);
    }
}


TEST(BinaryTreeMultinomial, TreeSortingWorks) {
    arma::Row<double> rates1 = {1, 2, 3, 4, 5, 6};
    auto [cumulant1, sorted_rates_index1] = dd_harp::build_binary_tree(rates1);
    for (int order = 0; order < rates1.n_elem; ++order) {
        EXPECT_EQ(sorted_rates_index1[order], order);
    }

    arma::Row<double> rates2 = {5, 4, 3, 2, 1};
    auto [cumulant2, sorted_rates_index2] = dd_harp::build_binary_tree(rates2);
    for (int order = 0; order < rates2.n_elem; ++order) {
        EXPECT_EQ(sorted_rates_index2[rates2.n_elem - order - 1], order);
    }

}


TEST(BinaryTreeMultinomial, TreeIntegrity) {
    double epsilon{1e-5};
    double fill{0.21349780};
    for (int n = 1; n < 29; ++n) {
        arma::Row<double> given_rates(n);
        given_rates.fill(fill);
        auto [tree, sorted_rates_index] = dd_harp::build_binary_tree(given_rates);

        for (int i = 0; i < tree.n_elem; ++i) {
            int child = 2 * (i + 1) - 1;
            if (child < tree.n_elem) {
                EXPECT_FLOAT_EQ(tree[i], tree[child] + tree[child + 1]);
            }
        }
    }
}


//! Convert an Armadillo vector to a string so that it shows up in debugging window.
template<typename VECTOR>
std::string vector_to_string(const VECTOR& rates) {
    std::stringstream msg;
    for (int msg_idx = 0; msg_idx < rates.n_elem; ++msg_idx) {
        msg << rates[msg_idx];
        if (msg_idx + 1 < rates.n_elem) {
            msg << " ";
        }
    }
    return(msg.str());
}



TEST(BinaryTreeMultinomial, DeterministicDrawsWork) {
    for (int n = 3; n < 40; ++n) {
        arma::Row<double> rates{arma::sort(arma::Row<double>(n, arma::fill::randu))};
        std::string rate_string{vector_to_string(rates)};
        auto [tree, sorted_rates_index] = dd_harp::build_binary_tree(rates);
        EXPECT_TRUE(sorted_rates_index.is_sorted());
        std::string tree_string{vector_to_string(tree)};
        std::string sorted_string{vector_to_string(sorted_rates_index)};
        arma::Row<double> histogram(rates.n_elem);
        histogram.zeros();

        double total{0};
        for (int rate_idx = 0; rate_idx < rates.n_elem; ++rate_idx) {
            double arate = rates[rate_idx];
            int seq_max{6};
            // Walk through draws that we know are firmly within a
            // particular rate index.
            for (int seq_idx = 1; seq_idx < seq_max; ++seq_idx) {
                double choice{total + seq_idx * arate / seq_max};
                EXPECT_GT(choice, total);
                EXPECT_LT(choice, total + arate);
                int chosen = dd_harp::locate_in_binary_tree(tree, choice);
                EXPECT_EQ(chosen, rate_idx);
            }
            total += arate;
        }
    }
}


TEST(BinaryTreeMultinomial, DrawsMatchRates) {
    boost::mt19937 rng(2342349873);
    for (int n = 2; n < 33; ++n) {
        arma::Row<double> unit_rates(n, arma::fill::randu);
        // Give the rates a wide range.
        arma::Row<double> rates = exp10(-3 + 3 * unit_rates);
        std::string rate_string{vector_to_string(rates)};
        auto [cumulant, sorted_rates_index] = dd_harp::build_binary_tree(rates);
        std::string tree_string{vector_to_string(cumulant)};
        std::string sorted_string{vector_to_string(sorted_rates_index)};
        arma::Row<double> histogram(rates.n_elem);
        histogram.zeros();

        int draw_cnt{1000000};
        for (int draw = 0; draw < draw_cnt ; ++draw) {
            int chosen = dd_harp::sample_binary_tree(cumulant, sorted_rates_index, rng);
            histogram[chosen] += 1;
        }
        if (n == 1) {
            EXPECT_FLOAT_EQ(histogram[0], draw_cnt);
        } else {
            arma::Row<double> given_rates = rates / sum(rates);
            double epsilon{3e-4};
            for (int check_idx = 0; check_idx < given_rates.n_elem; ++check_idx) {
            auto [low, high] = wilson_score_interval(given_rates[check_idx], draw_cnt, 0.99);
                EXPECT_GT(histogram[check_idx] / draw_cnt, low - epsilon);
                EXPECT_LT(histogram[check_idx] / draw_cnt, high + epsilon);
            }
        }
    }
}

