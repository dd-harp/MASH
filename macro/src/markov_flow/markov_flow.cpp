#include <vector>

#include "markov_flow.h"

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


    void movement_machine::init(
            const std::map <std::string, movement_machine_parameter> &parameters,
            const std::vector<std::vector<int>> &initial_state
    ) {
        this->human_count = std::get<int>(parameters.at("human_count"));
        arma::Mat<double> flow_probability = std::get<arma::Mat<double>>(parameters.at("flow_probability"));
        this->patch_count = flow_probability.n_rows;
        this->flow_cumulant.zeros(this->patch_count, this->patch_count);
        this->flow_index.zeros(this->patch_count, this->patch_count);
        for (int row_idx=0; row_idx < this->patch_count; ++row_idx) {
            auto [row_cumulant, row_index] = prepare_rates(flow_probability.row(row_idx));
            this->flow_cumulant.row(row_idx) = row_cumulant;
            this->flow_index.row(row_idx) = row_index;
        }
        this->human_location = initial_state;
    }


    const movement_machine_result *
    movement_machine::step(double time_step) {

        return &result;
    }


    std::tuple <arma::Row<double>, arma::uvec> prepare_rates(const arma::Row<double> rates) {
        arma::uvec sorted_rates_index = arma::sort_index(rates);
        arma::Row<double> sorted_rates(rates.n_elem);
        for (int copy_sorted = 0; copy_sorted < rates.n_elem; ++copy_sorted) {
            sorted_rates[copy_sorted] = rates[sorted_rates_index[copy_sorted]];
        }
        arma::Row<double> cumulant(sorted_rates.n_elem);

        double total_rate{0};
        for (size_t add_idx = 0; add_idx < cumulant.n_elem; ++add_idx) {
            total_rate += sorted_rates[add_idx];
            cumulant[add_idx] = total_rate;
        }
        return {cumulant, sorted_rates_index};
    }

}
