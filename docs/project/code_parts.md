# Parts of the Code


## MASH

MASH is all the stuff that isn't an implementation of a malaria-related model.

### Main loop


The MASH simulation iterates over four steps, simulating human health, human movement,
mosquito lifecycle, and bloodmeals. These four steps could be implemented so separately
that they are in different languages, different computer programs, or computed remotely.
That separate implementation creates risk for how we write and run the loop that calls
each of those steps.

While the initial loop has four steps, there are useful simulations that may run two
modules together or combine a different and larger set of modules. We should be able
to write different main loops.

The main loop becomes useful when we can compare the effect of substituting one
location module for another or one bloodmeal module for another. We will be able
to substitute modules when they expect the same kinds of inputs and outputs. For instance,
a bloodmeal module needs to know how many food sources are in a location
and how many have parasites. We have to encode that information in a way that all
comparable bloodmeal modules can read. You can make a bloodmeal module that uses
different information, but it will have a different contract with the location module
providing that information. These contracts are our most expert product.


### Observation

This simulation style will let us observe basic trajectories, no matter what
modules we run, because we insist that the API between modules exchange a particular
set of data structures. We can make a generic observer that always records
this information, or a subset of it.

That generic information doesn't include, for instance, detailed transitions for
each person. It's not relevant to other modules that a person is in a particular
compartment. Other modules will need to know probability of infection. If we
want to observe details about each person, the method that records these
details will need to collaborate with the specific human module.


### Parameter-handling


We should make it easy to work with parameters. A user has some main needs.
I'll list them here, in order, with possible ways to meet those needs beneath.

1. Find possible parameters
   a. Look in the documentation.
   b. Function to query possible parameters.
2. Know default values and construct with them.
   a. Defaults could be set as default parameters in a constructor.
   b. Defaults would be in the documentation.
3. Save and load parameters.
   a. This could be an R rdata file.
   b. Save as HDF.
4. Change them during the simulation.
5. Know which ones can be changed during the simulation
6. Let the simulation see which ones were just changed by the user.
7. Check that parameters are reasonable before running a simulation
   a. Run a few steps of the main loop.
   b. Check parameters against known types.

Parameters are currently in a list.

There are levels of checks that parameters are reasonable. One sure way
to check is to run a step or two of the main loop. That's great for smaller
simulations, but I have in mind that we will run this on the cluster against
cluster-sized data, so it helps to have another kind of check, where you see
that the names of parameters are OK and that values are the correct type.

Getting parameter types wrong can be insidious. A wrong type can be converted
automatically into something you didn't expect, but which still runs.


### Builders

There is a problem with making a system that is highly configurable.
There are too many choices you have to make in order to do anything.
We are going to address this problem by helping build MASH simulations.
We can call this part of the code builders.

Simple things should be simple, so we can have a single function that
constructs a Ross-Macdonald, for instance. We have a command that saves
and loads a description of a simulation.

Complicated things should be possible, so we should make ways to
display whether the main loop has all the information it needs to
run. Is it missing a movement module? Is it missing parameters for
one of the modules?

The builder collaborates with parameter-handling. The builders are
responsible for construction of simulations, including setting up
what their parameters are. They rely on parameter-handling.


### Protocol Handling

These are a part of the code because there are functions that convert from
one representation of events to another. For instance, code will wrap a set of events in
C++ so that R can read them.

We will, in the future, give modules a way to declare what protocols they
read and write so that we can verify that modules can talk with each other
and offer the user modules that will work together.


## MASH Modules


### Health Modules

These represent human health states.

#### Forced SI (Trivial)

A human is susceptible or infected. A susceptible person can recover. A person
becomes infected only if they receive an infectious bites. This module assumes
it will receive a list of infectious bites at the start of each time step.

Status: There are no interventions possible, and it will be done when it has interventions.

#### Ross-Macdonald ODE

Status: Not started

### Location Modules

#### Single Location (Trivial)

Everybody is in one location, all the time. This is a well-mixed system.

Status: Done except integration testing.


#### Simple Trip

Each person lives somewhere, but they travel to another place and, after
a waiting time, travel home.

This is in R.

Status: Done except integration testing.

#### Markov Flow

There is a transition rate from one location to another, and individuals
drift around according to those transitions rates, with no place to call home.

This is in C++.

Status: Done except integration testing.


### Bloodmeal Modules

#### Exponential Bites (Trivial)

There is a steady stream of bites with some biting rate.
Each location has a biting rate that is predetermined.

#### Poisson Bites

This is the one that takes input from the mosquito module.

Status: Not started.

### Mosquito Modules

#### Basic Mosquitoes

This would reflect the basic aquatic stage, as we did with the previous
version of MASH.

Status: This is started but only partially done.
