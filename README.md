# Kids momoply simulation
Simulation of the kid's version of monopoly with simplified rules. This is used to run simulations over several games with minor tweaks to see the outcomes of changing initial balance etc. This version of monopoly is much easier to create NPCs for since it doesn't relly involve any decisions. If you end up on an empty lot you have to buy it. 
The only decisions are from chance cards where you get to choose what lot to buy, but the general decision is to go to the first free of the most expensive one available.
The implementations are intended to be run by 2-4 NPCs rather than human players.

## Versions

### R

The R version is completely table (or tibble) driven and functional, in general without side effects etc. It is however sometimes overly complicated (and apparently not recommended) to create a game simulation with a data manipulation language...

### Python

The python version is object oriented and methods may wel have side effects

### C++

If there is ever a C++ version built it will be built around pointers to the player and board structs, something that will likely simplify some quirky parts of the R-implementation.

