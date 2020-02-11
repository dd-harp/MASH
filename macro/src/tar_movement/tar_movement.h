#ifndef SRC_MARKOV_TAR_H
#define SRC_MARKOV_TAR_H

#include <exception>
#include <map>
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
#   return class for TaR movement
-------------------------------------------------------------------------------- */

// This type is a sentinel that a variant has not been set.
struct no_parameter {};

using tar_movement_parameter = std::variant<no_parameter, int, double, arma::Mat<double>>;

class flux_movement;  // Forward declaration for friending.

class flux_movement_result {
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
    movement_sequence
    movements_of_human(human_id query) const;

    patch_sequence
    duration_in_patch(patch_id query) const;

    void allocate(human_id human_count, patch_id patch_count);
    //! Remove events without resizing the event queue storage.
    void clear();
    friend class flux_movement;
private:
    std::vector<movement_sequence> human_location;
    std::vector<patch_sequence> patch_state;
};



/* --------------------------------------------------------------------------------
#   class for the TaR movement
-------------------------------------------------------------------------------- */

class tar_movement {

public:


private:

};

}


#endif
