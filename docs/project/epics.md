# Epics

Suggestions for epics. The tasks listed here might need to
be split into smaller tasks. The goal is less than three days,
and half a day is fine.

 2. Packages and Testing are Set Up. By the end of this, there
    will be a C++ installation and an R package installation.
    The R package will contain a main loop in R and three
    loadable TWICE modules, which are simple.

    a. Create a C++ package that is separate, builds using R's toolchain
       or a user-specified g++ or clang, that runs testing in C++.

    b. In this package,
       create 1) a flux module to implement movement, 2) a PfSI
       module to implement humans, and 3) a forced EIR model
       to implement mosquitoes. These are done when they have
       tests and documentation.

    c. Write a user-friendly main loop in C++ that calls these modules.
       This main takes command-line arguments, is documented and tested.

    d. Create an Rcpp package that loads the C++ package, tests in R
       that it is loaded.

    e. Write a human movement module in R, making C++ code to run
       and read from the R module.

    g. Plan with collaborators on what they need for
       usable installation (Docker or whatever they want).

    h. Plan how we will move pdgSim into the code. This
       is done when we have mapped out pdgSim's inputs and
       outputs.

 4. Deliver working MASH.

    a. Move pdgSim to MASH. (Break this into tasks.)

    b. Augment user-oriented documentation. (Break this into tasks.)