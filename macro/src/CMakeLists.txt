# Tutorials about CMake:
# 1. cliutils.gitlab.io/modern-cmake
# 2. cmake.org has a tutorial.
# 3. CMake Cookbook, a book.
cmake_minimum_required(VERSION 3.1...3.15)

if(${CMAKE_VERSION} VERSION_LESS 3.12)
    cmake_policy(VERSION ${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION})
endif()

# Tell it where to find cmake macros and functions before
# doing includes.
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
message(STATUS "Cmake module path ${CMAKE_MODULE_PATH}")

# Record provenance by embedding the git version in a header file.
find_package(Git QUIET)
if(GIT_FOUND)
    execute_process(COMMAND ${GIT_EXECUTABLE} describe --tags --always
            OUTPUT_VARIABLE MASH_VERSION
            OUTPUT_STRIP_TRAILING_WHITESPACE
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
            )
    message(STATUS "Found version ${MASH_VERSION}")
else(GIT_FOUND)
    message(STATUS "Needs git in order to tag version")
    set(MASH_VERSION "0.1")
endif(GIT_FOUND)

# The version string is a tag like "v19.11.01-af98234"
string(REGEX MATCH "[0-9]+.[0-9]+" _numeric_version ${MASH_VERSION})
message(STATUS "Library version ${_numeric_version}")
project(mash
        VERSION ${_numeric_version}
        DESCRIPTION "Determine patches from a landscape"
        LANGUAGES CXX)

# Add compiler flags to enable C++17.
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)


# Check compiler compatibility with the CheckCXXCompilerFlag library.
include(CheckCXXCompilerFlag)
# Make prettier debug printing for CMake itself.
include(CMakePrintHelpers)

# Default to a debug.
# If you want to compile for debug, build with `cmake -DCMAKE_BUILD_TYPE=Release`.
set(default_build_type "Debug")
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    message(STATUS "Setting build type to '${default_build_type}' because none was specified.")
    set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE
            STRING "Choose the type of build." FORCE)
    # Set the possible values of build type for cmake-gui
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
            "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

# Using native architecture can speed up the code.
# Turn this off if you are using a cluster with heterogeneous processor architectures.
option(COMPILE_NATIVE "Compile for this exact processor" ON)
if(COMPILE_NATIVE)
    check_cxx_compiler_flag("-march=native" _march_native_works)
    set(_CXX_FLAGS)
    if(_march_native_works)
        message(STATUS "Adding -march=native to flags for vectorization")
        set(_CXX_FLAGS "-march=native")
    endif()
endif(COMPILE_NATIVE)


# Exporting makes functions in the library available
# for calling by code outside that library. On Windows,
# it helps to export everything.
set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)

####### GoogleTest Boilerplate BEGIN
# https://github.com/google/googletest/blob/master/googletest/README.md
# Download and unpack googletest at configure time
configure_file(CMakeLists.txt.in googletest-download/CMakeLists.txt)
execute_process(COMMAND ${CMAKE_COMMAND} -G "${CMAKE_GENERATOR}" .
        RESULT_VARIABLE result
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/googletest-download )
if(result)
    message(FATAL_ERROR "CMake step for googletest failed: ${result}")
endif()
execute_process(COMMAND ${CMAKE_COMMAND} --build .
        RESULT_VARIABLE result
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/googletest-download )
if(result)
    message(FATAL_ERROR "Build step for googletest failed: ${result}")
endif()

# Prevent overriding the parent project's compiler/linker
# settings on Windows
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

# Add googletest directly to our build. This defines
# the gtest and gtest_main targets.
add_subdirectory(${CMAKE_CURRENT_BINARY_DIR}/googletest-src
        ${CMAKE_CURRENT_BINARY_DIR}/googletest-build
        EXCLUDE_FROM_ALL)

# The gtest/gtest_main targets carry header search path
# dependencies automatically when using CMake 2.8.11 or
# later. Otherwise we have to add them here ourselves.
if (CMAKE_VERSION VERSION_LESS 2.8.11)
    include_directories("${gtest_SOURCE_DIR}/include")
endif()
####### GoogleTest Boilerplate END


#### Find libraries for the source code.

# Boost and its components. Minimum version specified.
find_package(Boost 1.65 REQUIRED
        COMPONENTS filesystem system program_options
        )

if ( NOT Boost_FOUND )
    message(STATUS "This project requires the Boost library, and will not be compiled.")
    return()
endif()

find_package(Armadillo REQUIRED)

#### Now build the targets.
add_subdirectory(flux_movement)
