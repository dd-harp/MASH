# Installation and Testing

The MASH modules in this directory can be built
either for R or for standalone use.

## Building for R with Rcpp


## Building for C++

The C++ code has tests that do not use R libraries.
These tests require

 * CMake 3.1 or later
 * Boost
 * C++ compilers
 * [recommended] Git

In order to compile on the Mac, we recommend following
https://thecoatlessprofessor.com/programming/cpp/r-compiler-tools-for-rcpp-on-macos/.

Once CMake is installed, compile with::

    cd src
    mkdir build
    cd build
    cmake ..
    cmake --build .

If you want to configure the build, you can either use
a graphical interface to CMake (ccmake or cmake-gui),
or you can add variables to the command line.

 * **Set Debug Build** - `cmake -DCMAKE_BUILD_TYPE=Debug ..`
 * **Change compiler** - `cmake -DCMAKE_CXX_COMPILER=/usr/bin/clang++ ..`

## Testing

Each module defines its own tests. Building the C++
builds these tests. Run them at the command line.

 * `/macro/src/markov_flow/markov_flow_test` - These
   tests use GoogleTest.

