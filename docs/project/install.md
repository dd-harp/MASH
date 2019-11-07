# How to Install MASH

## Overview

MASH will be a modular simulation package that
we can run from R. Some of the modules, maybe all, will
be written in C++. There is already a working version
of MASH without clear modularity. There is already
a prototype of the R and C++ design. Here, we would
like to decide how to install it.

The files we need to install can be grouped as

 - C++ code for each module.
 - C++ testing code.
 - C++ main function to call the modules.
 - RCpp code, in C++ and using R's C headers, which wraps C++ for R and reads R data from C++.
 - R code to wrap the modules.
 - R unit tests.
 - R main function to set up the model and run it.

## Requirements

 1. Install for Operating Systems

    a. Linux, required.

    b. Mac, required.

    c. Windows, optional.

 2. Required. There must be a way to make a new model, in a different,
    Github repository, and use it with these models.

 3. Optional. Running the simulation outside of R in pure C++.

 4. Optional. It should pass CRAN testing of packages.

 5. Required. We need to be able to run unit tests
    in both R and C++.


## Technical Challenges

What makes this package different.

 1. R packages aren't usually set up to run C++ tests.

 2. We want to share some headers among different
    packages. These are the headers that wrap the
    C++ module for R.

 3. We want to let someone write their own package
    and interoperate with the core MASH packages.

 4. Random number generation should share a stream
    across parts of the simulation.

## Possible Approaches

 1. Use an R-only package, including compiled code.
    Add C++ testing to this package, but run it separately
    from anything we run inside of R.

    a. This could use the default R build system.

    b. It could use a CMake-driven build system.

 2. Make a separate C++ repository and install it separately.
    Make an R package that relies on this C++ package.

    a. The R package could use default R build system.

    b. The R package could use a Cmake-driven build system.

    c. The R package could automatically download and
       install the C++ package.


### What Can Default R Package Installation Do?

R is good at making packages that include one or two
C or C++ files. It requires attention to do any more than that.

 1. R complains if there are extra files in the build
    directory, which can make adding C++ tests difficult.
    The solution is to use a .Rbuildexcludes file.
    This file can include whole subdirectories or just
    CMakeLists.txt.

 2. R builds files in the base directory, not subdirectories.
    This requires a snippet of code to tell the Makefile
    to look in subdirectories too. Or you can add all files
    to a list of OBJECTS, and that will be compiled.

 3. R defines its own flags. These seem to be the
    RelWithDebugInfo flags, meaning "-g -O2". That's not
    terrible.

 4. Sean says it is difficult to make a package with
    compiled code that passes all CRAN checks.

 5. Could require that GoogleTest be separately installed
    so that C++ testing could use GoogleTest from
    a main called by R. Then there would be no need for
    a separate C++ makefile.


#### PROS

Very little to do for the R package to work.

Could edit the C++ and run real data through it,
provided by R, without a separate build step in C++.

#### CONS
Would have a separate build system for C++
embedded inside it, which is unusual.
That build system could build a C++ library, but then
isn't it the same as having a dependency on a separate
library, because you still need to make a build system?



### How Would a Separate C++ Repository Work?

With CMake, we could write a build system that works
on Linux, Mac, and Windows. It would make a shared library
for R packages and run unit testing.

#### PROS
Would be a standalone library so that it could
be tuned and sped up using C++-only code and standard Linux tools.

Could optimize the C++ without having an R layer above it.

#### CONS

Would require more use of CMake.
