#ifndef SRC_MARKOV_FLOW_H
#define SRC_MARKOV_FLOW_H

namespace dd_harp {

using patch_id = int;
using human_id = int;
using clock_time = double;
using movement_sequence = std::vector<std::tuple<patch_id,clock_time>>;


class movement_machine_result {
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

    movement_sequence
    duration_in_patch(human_id query) const;
};


class movement_machine {
    // The result is a buffer that is owned by the machine,
    // so that it won't churn memory. It is read-only to others.
    movement_machine_result result;
public:
    template<PARAMETERS>
    void init(const PARAMETERS& parameters);

    const movement_machine_result*
    step(double time_step);
};
} // namespace dd_harp

#endif //SRC_MARKOV_FLOW_H
