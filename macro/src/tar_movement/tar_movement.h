/* --------------------------------------------------------------------------------
#
#   TaR movement model
#   Sean Wu (slwu89@berkeley.edu)
#   March 2020
#
-------------------------------------------------------------------------------- */

#ifndef SRC_MARKOV_TAR_H
#define SRC_MARKOV_TAR_H

#include <exception>
#include <map>
#include <tuple>
#include <sstream>
#include <vector>
#include <variant>

#include "armadillo"
#include "boost/random/binomial_distribution.hpp"
#include "boost/random/mersenne_twister.hpp"
#include "boost/random/uniform_real_distribution.hpp"


namespace dd_harp {

using patch_id = int;
using human_id = int;
using clock_time = double;
using movement_sequence = std::vector<std::tuple<patch_id,clock_time>>;
using patch_sequence = std::vector<std::tuple<human_id, bool, clock_time>>;


/* --------------------------------------------------------------------------------
#   each person has a next-event list
-------------------------------------------------------------------------------- */

enum class event_type {take_trip, return_home};

using next_event =


/* --------------------------------------------------------------------------------
#   return class for TaR movement
-------------------------------------------------------------------------------- */

// This type is a sentinel that a variant has not been set.
struct no_parameter {};

using tar_movement_parameter = std::variant<no_parameter, int, double, arma::Mat<double>>;

class tar_movement;  // Forward declaration for friending.

class tar_movement_result {
public:
    patch_id starting_patch(human_id query) const;

    size_t human_count() const;

    /*!
     * For the human, the movement sequence will be a
     * set of events with predetermined times.
     *
     * @param query - Which human's movements we want.
     * @return movement_sequence - The set of patches and times.
     */
    movement_sequence movements_of_human(human_id query) const;

    patch_sequence duration_in_patch(patch_id query) const;

    void allocate(human_id human_count, patch_id patch_count);

    //! Remove events without resizing the event queue storage.
    void clear();

    friend class tar_movement;
private:
    std::vector<movement_sequence> human_events;  // p
    std::vector<patch_sequence>    patch_events;  // n
};



/* --------------------------------------------------------------------------------
#   class for the TaR movement
-------------------------------------------------------------------------------- */

class tar_movement {

public:
  void init(
    const std::map<std::string, tar_movement_parameter> &parameters,
    const std::vector<std::vector<int>> &initial_location
  );


private:

  /* essentials */
  bool                initialized{false};
  boost::mt19937      rng;

  /* everyone's next event */


  /* parameters of the simulation: n = number of patches, p = number of humans */
  arma::Mat<double>   move_probs; // (n,n)
  arma::Mat<double>   trip_duration; // (n-1,p)
  arma::Col<double>   home_duration; // (p)

  int                 p;
  int                 n;

  /* data structures to hold sorted arrays for sampling */


};

}


#endif
