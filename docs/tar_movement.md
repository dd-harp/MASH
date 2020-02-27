# TaR movement model notes

Assume `n` patches and `k` individuals.

Each individual needs:
  * vector of length `n` giving the length of duration of stay each time they take a trip
  * scalar giving the length of duration of time they stay home between trips

There are global parameters:
  * matrix `n x n` giving destinations when each individual leaves their home patch
