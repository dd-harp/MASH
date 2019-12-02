# Technology Choices

This code relies on a lot of other technology. Here we discuss
choices.


## Why We Use R and C++

Listing pros and cons of options.

 * *Julia* - Very fast, is at a 1.0 version, so it isn't beta.
   This is a great choice for simulation. Our audience uses R.
   We don't trust Julia's statistical support yet, compared with
   R's extensive support. It might be possible to replace C++
   with Julia, though, using JuliaCall. We lack the experience
   and history with this in order to trust it.

 * *Python* - Our team is good at this language, and it's friendly
   for software development. It has a decent set of statistics
   libraries. We don't consider Python so much more friendly than
   R that we will cause stress for our user base. Python also
   needs to be paired with C++ for simulation, or it suffers
   slow speed.

 * *Pure R* - Too slow for what we are doing, and we know this
   because our rewrite from R into C++ created tremendous speedup.

 * *R and C++* - This allows pure C++ implementations that can
   be called from R. Writing C++ excludes the talents of most
   of the group and of most collaborators, which is a problem.

Decision is for R and C++ unless we push for R and Julia
together.


## Random Number Generation

Requirements for Random Number Generation come from
a combination of practicality and, if I recall, a review paper
by L'Ecuyer.

 1. Must be able to set a seed for a run or set of runs
    and to get the same values with the same seed again.

 2. Must be able to set a seed during a unit test.

 3. Would like to share a single stream of random numbers
    between R and C++.

 4. Would like to share multiple sub-streams among
    parallel threads.

 5. Would like to restart from the previous RNG state,
    the whole state, not just the seeds. This way you can
    continue on a stream.

We would like to use tools that exist. Let's list options we know.

 * *R builtin* - R provides random number generation that's
   exported to C libraries. It's fine quality, and we are using
   it now. Using this guarantees that a seed created in R
   carries forward to C code.

 * *Boost RNG* - This is a standard. The boost generators
   don't skip ahead the Mersenne Twister, which is a standard generator.
   Has an R package that installs it for C++ inclusion.
   It will skip Mersenne twister using the fast method (milliseconds)
   if you ask for something more than 10^8 of a skip.
   It's called `discard_many` in the source code.

 * *PRAND* - Barash made this library that includes a lot
   of generators. It isn't commonly used, but it has MT19937,
   which is standard.

 * *GSL RNG* - Also a standard, has MT19937. Has an R package
   that installs it, which makes things easier. I don't see
   the skip-ahead in here.

 * *Mersenne Twister skipahead* - We could use something separate
   to do the skip-ahead. The original conference proceeding,
   by Haramoto, Matsumoto, L'Ecuyer in 2008, has code, but it
   doesn't compile with current libraries.
   http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/JUMP/index.html

 * *SPRNG* - The parallel random number generation library.
   This works for parallel or serial. It's trusted. It might
   be difficult to install? Would have to call it from R
   for internal R random numbers.

 * *Agner Fog's libraries* - high quality PRNG designed in C++
   with Monte Carlo simulation in mind; has an example of an R
   package which uses these RNGs to draw from an urn (hypergeometric
   distribution). Mathematically trustworthy https://www.agner.org/random/

 * *RNGStreams* - From 1999, for parallel streams. Has C++ library
   and is part of R in `parallel::nextRNGStream(seed)`. Isn't Mersenne
   Twister. http://statmath.wu.ac.at/software/RngStreams/

 * *randtools package* - This R package offers some different
   generators, including an MT19937 that uses a newer initialization.
   This might be needed if you want R and C++ to match what numbers
   they generate.
   
Advice we found:

 * Vigna says Mersenne Twister is awful: https://arxiv.org/abs/1910.06437

 * Fog mixes Mersenne Twister with WELL.

In conclusion, our safest bet is to be able to switch random
number generators. Because Mersenne Twister is in use for almost
everything, we should be able to initialize it consistently
through C++ and R for now.
