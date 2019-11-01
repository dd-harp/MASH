# Mix Modules with Different Time Steps

PfGEIST has a one-day time step. pdgSim has a 10-day
time step. We need to be able to have a main simulation
loop that loops PfGEIST ten times for each pdgSim call.

This will impact data structures in the API to pass
data from one module to another, if it needs to store
more than one step in the queue or if it needs to
include time in the API for retrieving data from another
module.
