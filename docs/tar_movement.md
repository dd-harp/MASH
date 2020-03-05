# TaR movement model notes

Assume `n` patches and `k` individuals.

Each individual needs:
  * vector of length `n-1` giving the length of duration of stay each time they take a trip
    * Assume their home is `j`, then to return the correct value for some other place `i` do:
      ```
      if i < j
        return vec[i];
      else
        return vec[i-1];
      end
      ```
  * scalar giving the length of duration of time they stay home between trips
  * int telling us where their home is

There are global parameters:
  * matrix `n x n` giving destinations when each individual leaves their home patch

## Design

There are two classes.
  * `tar_movement`: once initialized it hangs around for the duration of the simulation.
  * `tar_movement_result`: lives within `tar_movement`, which is its friend so `tar_movement` has full access to all members of `tar_movement_result` objects.

## `tar_movement`

The data members are described here:
  * 
