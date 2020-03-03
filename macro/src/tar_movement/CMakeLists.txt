add_executable(flux_movement_test
        test_main.cpp
        flux_movement.cpp
        flux_movement_test.cpp
        flux_movement.h
        multinomial.cpp
        multinomial.h
        )

target_include_directories(flux_movement_test
        PRIVATE
        ${ARMADILLO_INCLUDE_DIRS}
        "${CMAKE_SOURCE_DIR}/markov_flow"
        )

target_link_libraries(
        flux_movement_test
        Boost::filesystem
        Boost::system
        Boost::program_options
        ${ARMADILLO_LIBRARIES}
        gtest_main
)

cmake_print_properties(
        TARGETS flux_movement_test
        PROPERTIES COMPILE_OPTIONS COMPILE_DEFINITIONS
        COMPILE_FEATURES COMPILE_FLAGS
        INCLUDE_DIRECTORIES
        INTERFACE_INCLUDE_DIRECTORIES
)
