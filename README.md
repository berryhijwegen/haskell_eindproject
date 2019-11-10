# Haskell Simulation
## Overview
This repository contains an simulation presented in a grid using Haskell.
When executing you will be prompted to specify some numbers which will be used in the simulation:
- Number of creatures to spawn
- Number of food items to spawn
- Grid size (One number, grid size will be num * num)
- Seed (Needed to keep code pure)

For example, a grid with 5 creatures, 5 food items and grid size of 5 could be:

||||||
|---|---|---|---|---|
| 0 | 2 | 1 | 0 | 1 |
| 0 | 0 | 0 | 0 | 2 |
| 1 | 0 | 1 | 0 | 0 |
| 0 | 2 | 0 | 2 | 0 |
| 1 | 0 | 0 | 0 | 2 |
||||||

0 = Nothing
1 = Creature
2 = Food

After the grid has been generated each step will be printed out to the console. This will contain info of all creatures and the actual grid.

## FAQ
**When does the simulation end?**
A simulation will end when all creatures die or all food items are gone.

**How do creatures die?**
Creatures die when they don't eat. Each step in the simulation their hp decreases with 10 hp.

**How do creatures not die?**
Creatures survive by eating. When they eat they regain 25 hp.

## Running the application
How to run:
- Run `stack build` to generate executables.
- Run `stack exec eindproject-exe` to execute application.
