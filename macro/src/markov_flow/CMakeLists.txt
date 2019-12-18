add_executable(markov_flow_test
        test_main.cpp
        markov_flow.cpp
        markov_flow_test.cpp
        markov_flow.h
        )

target_include_directories(markov_flow_test
        PRIVATE
        ${ARMADILLO_INCLUDE_DIRS}
        "${CMAKE_SOURCE_DIR}/markov_flow"
        )

target_link_libraries(
        markov_flow_test
        Boost::filesystem
        Boost::system
        Boost::program_options
        ${ARMADILLO_LIBRARIES}
        gtest_main
)

cmake_print_properties(
        TARGETS markov_flow_test
        PROPERTIES COMPILE_OPTIONS COMPILE_DEFINITIONS
        COMPILE_FEATURES COMPILE_FLAGS
        INCLUDE_DIRECTORIES
        INTERFACE_INCLUDE_DIRECTORIES
)
