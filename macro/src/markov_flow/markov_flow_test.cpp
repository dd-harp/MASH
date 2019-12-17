#include <map>
#include <string>
#include <tuple>
#include <variant>
#include <vector>

#include "armadillo"
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
    double epsilon = 1e-4;
    for (int check_idx = 0; check_idx < given_rates.n_elem; ++check_idx) {
        EXPECT_LT(abs(histogram[check_idx] / draw_cnt - given_rates[check_idx]), epsilon);
    }
}



TEST(MarkovFlowTest, PowerOfTwo) {
    std::vector<std::tuple<int,int>> inout = {
            {0, 0}, {1, 1}, {2, 2}, {3, 2}, {4, 3},
            {5, 3}, {6, 3}, {7, 3}, {8, 4}};
    for (auto [value, result]: inout) {
        EXPECT_EQ(result, dd_harp::next_power_of_two(value));
    }
}


TEST(MarkovFlowTest, BinaryTreeFilled) {
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


TEST(MarkovFlowTest, BinaryTreeTotalIsCorrect) {
    double epsilon{1e-5};
    double fill{0.32792342};
    for (int n = 1; n < 9; ++n) {
        arma::Row<double> given_rates(n);
        given_rates.fill(fill);
        auto [tree, sorted_rates_index] = dd_harp::build_binary_tree(given_rates);
        EXPECT_LT(std::abs(tree[0] - n * fill), epsilon);
    }
}

