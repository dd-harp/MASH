# TaR movement model notes

Assume `n` patches and `k` individuals.

Each individual needs:
  * vector of length `n-1` giving the length of duration of stay each time they take a trip
    * Assume their home is `j`, then to return the correct value for some other place `i` do:
      ```
      if i < j
        return vec[i];
      else
        reutnr vec[i-1];
      end
      ```
  * scalar giving the length of duration of time they stay home between trips

There are global parameters:
  * matrix `n x n` giving destinations when each individual leaves their home patch
