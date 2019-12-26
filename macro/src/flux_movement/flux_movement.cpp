#include <unordered_set>
#include <vector>

#include "boost/random/exponential_distribution.hpp"

#include "flux_movement.h"
#include "multinomial.h"


namespace dd_harp {
/*!
 * The sampling trajectory of the movement machine
 * tells the human when to change biting weight on
 * a patch and where they have hazard of being
 * bitten.
 */
    patch_id flux_movement_result::starting_patch(human_id query) const { return 4; };

    size_t flux_movement_result::human_count() const { return 10; }

/*!
 * For the human, the movement sequence will be a
 * set of events with predetermined times.
 *
 * @param query - Which human's movements we want.
 * @return movement_sequence - The set of patches and times.
 */
    movement_sequence
    flux_movement_result::movements_of_human(human_id query) const {
        return this->human_location[query];
    }


    patch_sequence
    flux_movement_result::duration_in_patch(patch_id query) const {
        return this->patch_state[query];
    }


    void flux_movement_result::allocate(human_id human_count, patch_id patch_count) {
        this->human_location.resize(human_count);
        this->patch_state.resize(patch_count);
    }

    void flux_movement_result::clear() {
        for (auto& human: this->human_location) {
            human.clear();
        }
        for (auto& patch: this->patch_state) {
            patch.clear();
        }
    }

    void flux_movement::init(
            const std::map<std::string, flux_movement_parameter> &parameters,
            const std::vector<std::vector<int>> &initial_state
    ) {
        this->human_count = std::get<int>(parameters.at("human_count"));
        arma::Mat<double> flow_probability = std::get<arma::Mat<double>>(parameters.at("flow_probability"));
        this->patch_count = flow_probability.n_rows;
        if (this->patch_count < 1) {
            std::stringstream msg;
            msg << "Patch count should be one or more but is " << patch_count;
            throw std::runtime_error(msg.str());
        }
        // Calculate total rate over all.
        this->human_location = initial_state;
        auto [search_matrix, index_matrix] = build_multinomial_matrix(flow_probability);
        this->flow_cumulant = search_matrix;
        this->flow_index = index_matrix;
        arma::Col<double> patch_rate_with_people(patch_count);
        // Assign per-patch rate = (# people in patch) x (total movement rate from patch)
        for (int patch_rate_index = 0; patch_rate_index <  patch_count; ++patch_rate_index) {
            patch_rate_with_people[patch_rate_index] = (
                    flow_cumulant(0, patch_rate_index) * human_location[patch_rate_index].size()
                    );
        }
        auto [rate_with_people, patch_index] = build_binary_tree(patch_rate_with_people);
        this->patch_rate_with_people = rate_with_people;
        this->patch_index = patch_index;
        this->total_rate = arma::sum(patch_rate_with_people);
        if (this->total_rate <= 0) {
            std::stringstream msg;
            msg << "The rate for flow should be positive but is " << total_rate;
            throw std::runtime_error(msg.str());
        }
        this->result.allocate(this->human_count, this->patch_count);
        this->initialized = true;
    }


    const flux_movement_result *
    flux_movement::step(double time_step) {
        if (!this->initialized) {
            throw std::runtime_error("You must initialize the class before stepping.");
        }
        this->result.clear();
        double time_within_step{0};
        while (time_within_step < time_step) {
            double dt = boost::random::exponential_distribution<double>(total_rate)(this->rng);
            // Choose among patches using the per-patch rate.
            int source_patch = sample_binary_tree(
                    this->patch_rate_with_people, this->patch_index, this->rng);
            // Choose where they go using the multinomial choose_direction().
            int destination_patch = sample_binary_tree(
                   this->flow_cumulant.col(source_patch),
                    this->flow_index.col(source_patch),
                    this->rng
                    );
            // Pick a person.
            int who_index = boost::random::uniform_int_distribution<int>(
                    0, this->human_location[source_patch].size() - 1)(this->rng);
            this->human_location[destination_patch].push_back(who_index);
            this->human_location[source_patch].erase(human_location[source_patch].begin() + who_index);
            // Calculate updates to per-patch rate and total due to moving person.
            std::vector<std::tuple<int, double>> updates = {
                    {destination_patch, human_location[destination_patch].size() * flow_cumulant(0, destination_patch)},
                    {source_patch, human_location[destination_patch].size() * flow_cumulant(0, source_patch)}
            };
            // Update the binary tree with new rates. Faster to do both at once.
            update_binary_tree(patch_rate_with_people, updates);
            total_rate = patch_rate_with_people[0];

            time_within_step += dt;
            // Record the change.
            this->result.human_location[who_index].push_back({destination_patch, time_within_step});
            this->result.patch_state[source_patch].push_back({who_index, false, time_within_step});
            this->result.patch_state[destination_patch].push_back({who_index, true, time_within_step});
        }

        return &result;
    }


}
