#include <map>
#include <string>
#include <variant>

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
