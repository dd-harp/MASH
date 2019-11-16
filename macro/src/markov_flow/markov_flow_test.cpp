#include "gtest/gtest.h"

#include "markov_flow.h"


TEST(MarkovFlowTest, SmokeTest) {
    auto m = movement_machine{};
    m.init();
    auto result = m.step(0.1);
}
