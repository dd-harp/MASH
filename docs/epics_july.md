% Epics for MASH
% Drew Dolgert
% 29 July 2019

# Overview

These are epics and stories for MASH that should result in an 0.1 release.
An epic is a set of work that a stakeholder would understand. The stories which
make up the epic may be more technical.
The subheadings below will be epics, and the lists below those will be stories.

This code has been difficult to write because it is so reconfigurable. The main loop knows only about the protocols of the modules. We have written modules and then not known how to test them within the main loop, given that we are writing the main loop. It's like knitting the sweater you're wearing.

I'm going to approach this by creating the lynchpin first, the bloodmeal module. This will mean that there is a sample of every protocol and that we can reasonably connect pieces together as soon as they run. Then move component by component. Finally, there is a clean-up epic to ensure it exactly meets 0.1 goals.


# Epics and Stories

IHME uses a points system:

* 1 = a todo
* 2 = a day or less
* 3 = 2-3 days
* 5 = a week

These tasks are almost all a day or less, so the points are less helpful. I'll give them 1/2 day, 1 day, or 2-3 day. These are uninterrupted work hours, so 3.5 days is about a week.


## Bloodmeal module

(3.5) This makes a bloodmeal module that assigns bites uniformly to humans. We make this module early because, if it exists, then we will have examples, already constructed of all of the protocol data formats. We won't have to make more testing-only code than we need.

1. (1/2) Make functions that create sample input datasets to the bloodmeal module. These will be data.table dataframes. This is done when the data matches the descriptions below.

2. (1/2) Write pseudocode for an algorithm to assign infectious bites to mosquitoes and infectious bites to humans. The product is a sketch of algorithmic steps. It includes suggestions for tests that it works.

3. (1/2) Write a function that separates people into places. This is done when it returns people organized by place in the format that the next step needs.

4. (1) Write a function that assigns bites to people in a place. This is where we make heterogenous biting, and we follow the previous code on how to do this. This is done when bites are assigned with the correct proportions, according to a unit test.

5. (1) Write the MASH module around those functions. In particular, write code to generate two outputs, one for humans and one for mosquitoes. This is done when inputs and outputs are correct for the MASH module.



## Health module gives correct input and output

(3) The health module is currently an S-I model without interventions. This epic won't add interventions yet. Instead, it will make the S-I model work in the main loop of MASH. That way, we can have a complete, running simulation with which to work while we add features. This epic is also partially done, so it's short.

1. (1) Create a trivial mosquito module, so that we can use the existing bloodmeal and have a running simulation. This is done when the bloodmeal and mosquito module can exchange data.

2. (1/2) Save output from all modules using an observer. This is done when data from modules is stored in an array in memory inside the observer in such a way that a unit test can check that data. We can save to disk later.

3. (1/2) Format the Health module output according to MASH protocol. This will be a time series of parasite presence in each person's blood, over the course of the time step. This is done when the output dataframe matches the known format.

4. (1) Modify the health module to accept bloodmeal output as its input, so that it gets new innoculations from the bloodmeal. This will require modifying the health module simulations so that infections are given as a list of times. This is done when the bloodmeal and health modules run, producing a stochastic curve of infection rates.


## Mosquito module model is implemented

(5.5) The mosquito model currently has code that will run a discrete-time model, so this epic will create the specific discrete-time model for the mosquito module. There are three descriptions of what the model will do, the MASH 0.1 document, the previous code, and the paper that Sean and Daniel wrote. The MASH 0.1 document constrains choices about how to implement a version of the previous model.

1. (1) Sketch the models from the previous Ross-Macdonald style simulation in MASH, as described in the code and in the paper draft written by Sean and Daniel. This is done when there are a written set of states, transitions, and rates for a discrete-time simulation.

2. (1/2) Read the code that Sean wrote for the MASH mosquito module. He had a design in mind, and he created code to make it easier to implement a specific mosquito module. The product of this is a design sketch for the new code to write, at the level of functions and classes.

3. (1/2) Ask Sean to review the written model. It doesn't take much time because it's a high-level description and he is familiar with the model. This is the highest-level way to include him in the process and a critical review. This is done when Sean has given his notes on the model.

4. (2-3) Create and test the core simulation model _that is simplified, if that's faster to write._ The core simulation model is code that runs one time step of the mosquito simulation, ignoring requirements from MASH about module structure. This is done when it passes a numerical test that the time step is converging to a good value.

5. (1/2) Write a module around the core simulation model, including its inputs and outputs. This is done when the module is of the correct S3 class, has all of its calls, and it passes unit tests for inputs and outputs. The output must be the dataframe that obeys the MASH protocol.


## Movement module accepts parameter input and gives correctly-formatted output

(2.5) The movement module currently accepts no input and assigns individuals to locations based on a simple trip model. It isn't completely set up as a module, because there isn't a good way to send parameters, and it doesn't send output in the correct format. This epic will make sure there is _some_ way to specify rates, even if it's awkward. But it will ensure that the output is in the correct format for the Bloodmeal.

The other big issue with the movement module is that it takes, as an argument, an all-to-all movement matrix. We would like to specify this as a distance matrix and a movement kernel. We won't address that here, though.

1. (1/2) Review the movement module code. Write a sketch of exactly what functions it needs in order to be a module.

2. (1) Create the MASH module code around the movement module, so that it is the correct S3 class and passes output. This is done when it has the right class type and functions defined. It must pass the data format the Bloodmeal needs.

3. (1) Add a way to input a travel matrix. Start with a very simple, explicit, input. This is done when there is an example of passing in a travel matrix.


## Add features to bring to MASH 0.1

(7) Things run at this point, but they don't necessarily do what we need yet, according to the 0.1 document. In particular, they will have remedial ways to set parameters. This epic creates a way to set and send parameters, and it does that in the context of an example for how to use MASH 0.1.

Pieces of this epic take surprisingly long times because they involve fussing with things. We may be able to start someone on a forest malaria model before it's done.

1. (1) Make a test that uses a single location, the S-I Health module, and the Mosquito module. This is done when there are graphs from that test.

2. (1/2) Create a model that looks like a forest model, but it has to do a lot of work to set parameters. This is done when there is a vignette with a graph of outputs from a forest malaria model.

3. (1) Make functions that build that model, so that those functions are easier for a user to call. This is done when the builder functions can be called from the same vignette to run the model.

4. (1) Test installation of MASH from a clean installation of R. This is done when MASH installs with dependencies on a Mac.

5. (1/2) Add interventions to the S-I module. This is done when there is an intervention parameter for the forest malaria simulation.

6. (2) Save and load parameters. This is done when it is possible to save a whole model's configuration to disk and load it again.

6. (1) Set random generators from the same seed. This uses R's parallel library, which has support for parallel streams. It's done when different R modules can use the same single seed for the whole simulation run.


# Protocols

The protocols define what data each module sends to another. A lot of the work in the steps above involves reading this data, reformatting it, and creating outputs in the given format. The data structures below help understand the work that has to be done.


## Health output to Bloodmeal

Each human has an integer ID. Their infection status is a floating-point number. That number can change at time points. Empty time points indicate no change in value.

ID   Start   Time1   Level1  Time2   Level2
---- ------- ------- ------- ------- -------
1    0.0     3.2     1.0
2    1.0     2.4     0.0     7.8     1.0
3    0.0


## Bloodmeal output to Health

This output indicates which individuals were inoculated by infectious mosquitoes, when they were inoculated, and some number to indicate a level of parasite. Humans not listed were not bitten.

ID  Time1  Level1  Time2  Level2
--- ------ ------- ------ -------
5   2.7    0.4
12  1.9    0.3     7.2    0.8

We won't use the level in this first round, but it should be in the protocol.

This is data we think of as crossing time steps because bites from last time step affect the current health time step, not bites from the same time step. We decided on this method in the TWICE document.


## Movement output to Bloodmeal

The first option is to list locations by human. Each human has an integer ID. Their location is an integer ID of a location. That location can change at time points. Empty time points indicate no change in value.

Human  Start  Time1  Location1  Time2  Location2
------ ------ ------ ---------- ------ ---------
1      17
2      23     2.7    38         4.9    23
3      10     9.8    12

The second option is to list humans by location. Let's look at a single location this way. This is sorted by location and lists every movement of a human.

Depart  Arrive  Human   Time
------- ------- ------- -----
1         2     5       0.5
1         52    7       1.2
1         39    2       8.4

This format is much closer to what the Bloodmeal module needs from the location module.


## Health to movement

We discussed having health outcomes affect movement, but that communication isn't in this version. We decided that care-seeking because of a health outcome would constrain an individual's future movement on the day they would get treatment.


## Mosquito output to Bloodmeal

The mosquito module generates infectious bites and bites that could infect a mosquito. In this case, should we allow a mosquito to receive two infectious bites? In general, don't disallow things in the protocol. Disallow them in the model's logic, so we'll permit it.


Location  Bite  Time
--------- ----- -----
1         0.7   0.05
1         0.0   0.07
2         0.6   0.03

A bite with level 0.0 is a bite from an uninfected mosquito. This mosquito can be infected by that bite.

Should this list say which human was bitten? We're treating all humans at a location equally, so we'll put assignment of bites into the Bloodmeal module. That's what it does. The bloodmeal uses a distribution to assign bites to people.


## Bloodmeal output to Mosquito

The bloodmeal has to tell the mosquito module how many bites were of infectious humans. We have lost track of which mosquito bit which human, but that's OK. The bite column is a measure of the parasite level of the bite.

Location  Bite  Time
--------- ----- -----
1         0.6   1.2
2         0.8   3.4

This is data we think of as crossing time steps because infectious bites from last time step affect the current time step, not more recent bites. We decided on this method in the TWICE document.
