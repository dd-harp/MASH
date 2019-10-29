# Epics

Suggestions for epics. The tasks listed here might need to
be split into smaller tasks. The goal is less than three days,
and half a day is fine.


 1. Code is organized for TWICE modularity (almost done)

    a. Decide among finite state machine, subject-observer, publish-subscribe.

    b. Choose final names for all the parts.

 2. Packages and Testing are Set Up.

    a. Create a C++ package that is separate, builds using R's toolchain
       or another toolchain, runs testing in C++.

    b. Write the simplest version of human, mosquito, human movement
       in C++, such that R can pass in parameters and get back results.

    c. Write a user-friendly main loop in C++ that calls these modules.

    d. Create an Rcpp package that loads the C++ package, tests in R
       that it is loaded.

    e. Write a human movement module in R, making C++ code to run
       and read from the R module.

 3. We can review the main loop of MASH and which data modules share.

    a. Implement PfMOI.

    b. Implement Movement module.

    c. Implement Mosquito module for Ross-Macdonald.

    d. Present API for review and evaluation.

 4. pdgSim is running.

 5. Mosquito-BITES is running.
