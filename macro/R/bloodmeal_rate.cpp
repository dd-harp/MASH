#include <limits>
#include "Rcpp.h"

using namespace Rcpp;


// [[Rcpp::export]]
NumericVector person_exposure(NumericVector when_array, IntegerVector event_array) {
    /* Assumes events are sorted, events run from time 0 to past time 10.
    */
    int day_cnt{10};
    long event_cnt = event_array.size();
    NumericVector person_days(day_cnt);
    if (when_array.size() != event_array.size()) {
        // Message how?
        person_days[0] = -1;
        person_days[1] = 2.0;
        return(person_days);
    }

    // We are recording person-years for day 0-1 at the start.
    int day_idx{0};
    double day_total{0.0}; // person-years for day_idx to day_idx+1 so far.
    double last_time{0.0}; // time of previous event.
    int last_count{0}; // state of system since last eveng (cadlag).
    for (long event_idx = 0; event_idx < event_cnt; ++event_idx) {
        double when = when_array[event_idx];
        int event = event_array[event_idx];

        while (day_idx + 1 < when && day_idx < day_cnt) {
            day_total += last_count * (day_idx + 1 - last_time);
            person_days[day_idx] = day_total;
            ++day_idx;
            day_total = 0.0;
            last_time = day_idx;
        }
        day_total += last_count * (when - last_time);
        last_time = when;
        if (event == 1) {
            last_count += 1;
        } else if (event == -1) {
            last_count -= 1;
            if (last_count < 0) {
                person_days[0] = -1;
                person_days[1] = 3.0;
                person_days[2] = when;
                return(person_days);
            }
        } else if (event == 2) {
            ; // don't care that infection status changed.
        } else {
            // Error message how?
            person_days[0] = -1;
            person_days[1] = event;
            return(person_days);
        }
    }
    // No more events. Record status quo until end of time period.
    while (day_idx < day_cnt) {
        day_total += last_count * (day_idx + 1 - last_time);
        person_days[day_idx] = day_total;
        ++day_idx;
        day_total = 0.0;
        last_time = day_idx;
    }

    return(person_days);
}



std::vector<double> running_exposure(NumericVector when_array, IntegerVector event_array) {
    /* Assumes events are sorted, events run from time 0 to past time 10.
    */
    int day_cnt{10};
    std::vector<double> person_days(day_cnt);
    if (when_array.size() != event_array.size()) {
        // Message how?
        person_days[0] = -1;
        person_days[1] = 2.0;
        return(person_days);
    }

    long event_cnt{when_array.size()};
    // We are recording person-years for day 0-1 at the start.
    int day_idx{0};
    double day_total{0.0}; // person-years for day_idx to day_idx+1 so far.
    double last_time{0.0}; // time of previous event.
    int last_count{0}; // state of system since last eveng (cadlag).
    for (long event_idx = 0; event_idx < event_cnt; ++event_idx) {
        double when = when_array[event_idx];
        int event = event_array[event_idx];

        while (day_idx + 1 < when && day_idx < day_cnt) {
            day_total += last_count * (day_idx + 1 - last_time);
            person_days[day_idx] = day_total;
            ++day_idx;
            day_total = 0.0;
            last_time = day_idx;
        }
        day_total += last_count * (when - last_time);
        last_time = when;
        if (event == 1) {
            last_count += 1;
        } else if (event == -1) {
            last_count -= 1;
            if (last_count < 0) {
                person_days[0] = -1;
                person_days[1] = 3.0;
                person_days[2] = when;
                return(person_days);
            }
        } else if (event == 2) {
            ; // don't care that infection status changed.
        } else {
            // Error message how?
            person_days[0] = -1;
            person_days[1] = event;
            return(person_days);
        }
    }
    // No more events. Record status quo until end of time period.
    while (day_idx < day_cnt) {
        day_total += last_count * (day_idx + 1 - last_time);
        person_days[day_idx] = day_total;
        ++day_idx;
        day_total = 0.0;
        last_time = day_idx;
    }

    return(person_days);
}


// [[Rcpp::export]]
Rcpp::List assign_bites(NumericVector bite_times, NumericVector when_array,
        IntegerVector event_array, IntegerVector who) {
    int day_cnt{10};
    long bite_cnt = bite_times.size();
    long event_cnt = event_array.size();
    Rcpp::NumericVector when(bite_cnt);
    Rcpp::NumericVector human(bite_cnt);
    Rcpp::NumericVector infectious(bite_cnt);

    // Precalculate person-days for each day because the times of bites
    // are specified as fractions of total person-days for each day, from 0 to 1.
    std::vector<double> daily_exposure = running_exposure(when_array, event_array);

    int day_idx{0};
    long bite_idx{0};
    long event_idx{0};

    double day_total{0.0}; // person-years for day_idx to day_idx+1 so far.
    double last_time{0.0}; // time of previous event.
    std::vector<long> humans;

    while (true) {

        double next_day = day_idx + 1;
        double next_event;
        if (event_idx < event_cnt) {
            next_event = when_array[event_idx];
        } else {
            next_event = std::numeric_limits<double>::infinity();
        }
        double next_bite;
        if (bite_idx < bite_cnt) {
            // A value of 1.2 is 20% of the person-days into the second day.
            double proportional_bite = (bite_times[bite_idx] - day_idx);
            double person_day_fraction = proportional_bite * daily_exposure[day_idx];
            next_bite = last_time + (person_day_fraction - day_total) / humans.size();
        } else {
            next_bite = std::numeric_limits<double>::infinity();
        }

        if (next_day <= next_event && next_day <= next_bite) {
            // next_day
            day_total = 0.0;
            last_time = day_idx + 1;
            ++day_idx;
            if (day_idx == day_cnt) {
                break;
            }
        } else if (next_bite <= next_day && next_bite <= next_event) {
            // next_bite
            Rcpp::NumericVector rands = Rcpp::runif(1);
            int choice = static_cast<int>(std::floor(rands[1] * humans.size()));
            int human_idx = humans[choice];
            when[next_bite] = bite_times[next_bite];
            human[next_bite] = human_idx;
            ++next_bite;
        } else {
            // next_event
            double event_time = when_array[event_idx];
            day_total += humans.size() * (event_time - last_time);
            last_time = event_time;
            long event = event_array[event_idx];
            if (event == 1) {
                humans.push_back(who[event_idx]);
            } else if (event == -1) {
                int to_remove = who[event_idx];
                auto found = std::find(humans.begin(), humans.end(), to_remove);
                if (found != humans.end()) {
                    humans.erase(found);
                } else {
                    ; // Wrong.
                }
            } else if (event == 2) {
                ; // don't care that infection status changed.
            } else {
                ; // Error message how?
            }
            ++next_event;
        }
    }

    return Rcpp::List::create(
        Rcpp::Named("when") = when,
        Rcpp::Named("who") = human,
        Rcpp::Named("infectious") = infectious
    );
}
