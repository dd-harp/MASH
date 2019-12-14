#include <vector>

#include "boost/property_map/property_map.hpp"

#include "markov_flow.h"
//#include "parameters.h"

namespace dd_harp {
/*!
 * The sampling trajectory of the movement machine
 * tells the human when to change biting weight on
 * a patch and where they have hazard of being
 * bitten.
 */
patch_id movement_machine_result::starting_patch(human_id query) const { return 4; };

size_t movement_machine_result::human_count() const { return 10; }

/*!
 * For the human, the movement sequence will be a
 * set of events with predetermined times.
 *
 * @param query - Which human's movements we want.
 * @return movement_sequence - The set of patches and times.
 */
movement_sequence
movement_machine_result::movements_of_human(human_id query) const {
    return {{3, 0.02},
            {4, 0.05},
            {3, 0.8}};
}


movement_sequence
movement_machine_result::duration_in_patch(human_id query) const {
    return {{4, .77},
            {3, .23}};
}


template<typename PARAMETERS>
void movement_machine::init(const PARAMETERS& parameters) {
    int human_count = dd_harp::get<int>(parameters, "human_count");
}


const movement_machine_result *
movement_machine::step(double time_step) { return &result; }

}
