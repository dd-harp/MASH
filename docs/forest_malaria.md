# Forest Malaria Project

The forest malaria project represents an opportunity to use the simulation software in service of exploring a new model of forest malaria transmission developed by David L Smith with Alec Georgoff.  The gist is that ``forest malaria'' represents a new mode or typology of malaria transmission which differs fundamentally from the mode of transmission in high burden countries in sub-Saharan Africa. In the latter case, people are most frequently at risk in their homes at night when they sleep; as such, IRS and LLINs appear to be effective control measures. In the case of forest malaria, on the other hand, people travel from their homes into the forest for days or weeks at a time. The forest represents a high transmission environment, in contrast to the home where there is relatively little transmission. Such a model for transmission is appropriate for locations in Southeast Asia or Central or Southern America.

The forest malaria model developed by this group involves defining one or more village patches and one or more forest patches, and then labeling a subset of villagers as people who travel periodically to the forest. The village may have low (or nonexistent) transmission while the forest may have high transmission.

The main research question is how high the transmission risk (or $R_0$) needs to be in order to sustain the appearance of endemic malaria in the village, given the frequency of forest travel and time spent at risk while in the forest.

We propose using MASH to explore some of these questions, and require certain features be included in one of the early releases:
  * Travel behavior must be attributed to individual hosts, rather than to home patches (specifically, the probability distribution representing destination choice) - this makes it possible to have truly heterogeneous travel behavior. For instance some people might go to one forest patch and other people might go to another
  * Age of infection model - one aspect of forest malaria worth exploring is whether old infections contribute to onward transmission and the parasite reservoir in the forest, such that people who travel more or less frequently might contribute more or less depending on the age of the parasites acquired in the previous forest trip
  * Ivermectin - I don't know the details of this, but Dave says he has a model for ivermectin which we could use to simulate using treatment as an intervention in a forest malaria setting. It would require an intervention schedule (ie, dosing people before they travel) and differentially killing mosquitoes who interact with people dosed with ivermectin
  * Travel ban - we could also simulate a government-imposed ban on entering the forest, and see whether that is enough to kill the parasite reservoir in the forest 