# Epics

Suggestions for epics. The tasks listed here might need to
be split into smaller tasks. The goal is less than three days,
and half a day is fine.


 1. Code is organized for TWICE modularity (almost done).
    We are done when we have a document describing TWICE
    that has a component diagram, a main loop, and a specification of
    function signatures in R and C++ for an example.

    a. Create a prototype of PfSI using a bare module API,
       meaning that other modules call functions of the module itself.
       Use this prototype to create an object-oriented R interface
       to the C++ module. Make basic logging and sampling in this
       module to see how it behaves.

    b. Choose whether we use Countable State Machines or bare modules
       and choose final names for all the parts. This is a presentation
       and meeting. The definition of done is a document with a
       diagram and labels, to serve as a roadmap.

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

 3. We can rerun the test for correctness of Ross-Macdonald.

    a. Implement Movement module for R-M.

    b. Implement Mosquito module for Ross-Macdonald.

    c. Present the API to the team, showing dependencies in order
       for calling modules. Ask for feedback.

    d. Generate data and rerun the test that Daniel did.
       Try to put this test into a unit test in R.

 4. Deliver working MASH.

    a. Move pdgSim to MASH. (Break this into tasks.)

    b. Augment user-oriented documentation. (Break this into tasks.)

 5. Mosquito-BITES is running in MASH.
