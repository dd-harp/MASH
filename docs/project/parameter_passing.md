# Passing Parameters Among R and C++ Objects

Our code has top-level modules, within the package. In
R, there will be a loop over the modules for time steps.
We generally need a way to pass initialization parameters
from R to C++ and from C++ to R.

I'd like the following to be true:

* The C++ code can compile on its own.
* Parameters could come from a file or from R.
* It's possible to update parameter values in the middle
  of a simulation.

There are three sections of code that coordinate
to make these things happen.

* The R code sets parameters and reads results.
* There is code that wraps C++ for R, using Rcpp,
  and it does translation in both directions.
* C++ code reads and validates parameters. It also
  sets outputs in such a way R can read them.

## Using Rcpp within the C++ module

The C++ module can assume that it always gets its parameters
from R. An example of that code comes from macro.pfsi.

```
patch::patch(
  const Rcpp::List& patch_pars,
  tile* tileP_
) {
 this->move = Rcpp::as<arma::Row<double>>(patch_pars["move"]);
}
```

This code uses a custom Rcpp form of the boost::any_cast. It's a
variable that doesn't have a type, and then you assert what
its type must be.

A drawback for this is that the code must always be called
from R. The C++ must be able to compile against Rcpp headers,
too.

## Use a Parameter Pack

This method makes a separate class to represent the parameters
and their type in C++. It leaves to R the challenge of filling
in those parameters.

```
struct Parameters {
    int human_count;
	arma::Mat<double> move;	

	void set_human_count(int count);
	void set_move(const arma::Mat<double>& move);

	bool human_was_set;
	bool move_was_set;
};
```

## Pass a Map of Variants

This method says that parameters will be stored in a C++
map data structure, which is known as a dictionary in other languages.
The trick is that this dictionary points to a typed variant,
which is a variable that holds only one of a few specific types.

```
struct no_parameter {};

using movement_machine_parameter = std::variant<no_parameter, int, arma::Mat<double>>;

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
    // Assign per-patch rate = (# people in patch) x (total movement rate from patch)
    // Calculate total rate over all.
    this->human_location = initial_state;
}
```

Drawbacks are that R has to figure out how to fill in the map.
This might involve copying the data. I don't know that
you can use std::move out of a map, but maybe you can.

An advantage is that, in the middle of a simulation, you could set
parameters on the map, and you would know which ones are set
automatically because they are the only ones in the map.


## Map of Any

We could do the same thing but use the Boost Any type.
This means there is no set of types to define in a variant
but that the code enforces type inside. I'm not sure
under what circumstances the code refuses to cast from any
to a particular type.

```
#include <boost/any.hpp>

using boost::any_cast;

bool is_arma_matrix(const boost::any& parameter) {
	try
    {
        any_cast<arma::Mat<double>>(parameter);
        return true;
    }
    catch(const boost::bad_any_cast &)
    {
        return false;
    }
}
```

That showed the syntax, but use is much like the map above.


## Boost Property Map

A Boost property map is a layer of abstraction over the map.
You can take a standard dictionary-style map and adapt it
with a property map. You can take an Rcpp-style list and
adapt it with a property map. The module that receives that map
would use the same get() function either way.

```
std::map<string,any> parameters;
parameters["human_cnt"] = 30;
boost::associative_property_map parameter_map(parameters);
module.init(parameter_map);
```
The property map is an adapter. It encloses whatever has the
parameter data so that the module can read it with
get() or set() commands.

A drawback is that the property map interface has two calls,
get() and set(). They do not give you a way to iterate through
the property map. This would mean that, every time step, you would
need to check for all the possible parameters in order to find
out whether any parameters were updated.

The challenge here is that it adds a layer of complexity on the
code to using a straight map.
The advantage is that you get optimally-concise code whether
you are in strict C++ or in R and C++.
