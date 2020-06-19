# Implementation summary

## Week 1
### 21/5

- Added a num-contacts plot to keep track of the total number of contacts over time
- Implemented initial SEIR model, adding exposed people in-between susceptibles and infecteds

### 22/5

- Exposed people can now infect others
- Removed people now lose immunity after 30-35 days
- Updated contact counts to include exposeds
- Infecteds are now removed either with probability p-remove or after a certain number of ticks
- Infected people have a chance to become dead instead of removed
- Added sliders to control the range of infection, removal and immunity countdowns
- Temporarely removed the option for susceptibles to be aware of surrounding removeds
- Changed cyan indicator for lockdown into black outline

## Week 2
### 26/5

- Found sources that indicated an initial probability of infection of 20%

### 27/5

- Reintroduced option for susceptibles to be aware of surrounding removeds
- Implemented log-normal distribution for incubation period, mean and stdev as global variables
- Found sources that indicated an average probability of death of ~2%
- Adjusted initial probability of infection to 30% to account for other sources

### 28/5

- Implemented poisson distribution for loss of immunity countdown, mean to be varied in simulations
- Implemented normal distribution for recovery countdown, with absolute result to prevent negative durations
- Implemented imposed lockdown threshold with strictness level parameter, removed risk awareness and awareness neighbourhood systems
- Removed all contact weighing

### 29/5

- Created variables list
- Implemented control measures to reduce probability of transmission triggered after first lockdown, with effectiveness parameter
- Implemented an option to open the system, allowing susceptibles to be infected by outside sources depending on travel strictness when not into lockdown

## Week 3
### 1/6

- Implemented two separate start/end lockdown procedures to ensure stable percentages of quarantined population and help readability
- Impemented three age ranges, 0-29, 30-59, and 60+, assigned to people based on demographics of the UK population
- Implemented modified probability of death depending on age range, 0.6x for under 30 and 5.1x for over 60

### 2/6

- Created github repo
- Changed global variable probabilities from decimals to integers for ease of understanding
- Separated recurring processed into dedicated procedures

### 3/6

- Added a minimum value for duration of recovery equal to mean - stdev, unless that value is below 0, in which case the minimum is 1 day

### 4/6

- Implemented pareto distribution for variation of initial contact neighbourhood size, i.e. z-contact-ini, with alpha = 2
- Added two possible methods to ensure the minimum value for initial contact neighbourhood is equal to the value of z-contact-min
- Changed naming convention of exposeds into latents, as they are infectious but not yet symptomatic
- Created a separate countdown for infecteds that will die instead of recover, as the mean duration of infection is different between these two categories
- Created asymptomatic breed, not yet integrated in the model

### 5/6

- Implemented isolation for infected individuals, currently with isolation-strictness % of people isolating for iso-countdown-max days, and the rest isolating for half that time (applies after first lockdown)
- Changed already-locked? variable to currently-locked? for clarity
- Integrated asymptomatic infecteds in the model, who behave exactly like infecteds but with no isolation option

## Week 4
### 8/6

- Fixed the distribution of isolation duration for infected individuals
- Set up a test experiment for headless running and distributed processing
- Renamed infecteds to symptomatics and removeds to recovereds
- Removed possibility of death for asymptomatic infecteds
- Reduced infectiousness of asymptomatic infecteds by only counting 10% of the actual asymptomatic agents in the contact radius

### 9/6

- Set up a test0 experiment for testing with Condor cluster
- Started commenting model code

### 10/6

- Set up tests for different world sizes
- Finished commenting code
- Various naming and procedure fixes

### 11/6

- Decoupled the lockdown and control measures options, so that each can be switched on independently from the other
- Added a number of contacts variable to report during tests

### 12/6

- Added reporters for age of deads to use in tests
- Edited tests to find an ideal world size between 100x100 and 500x500

## Week 5
### 15/6

- Changed initial-inf and lockdown-threshold from absolute values to population percentages to help scaling during world size tests
- Changed initial infecteds from symptomatics to latents
- Renamed modify-p-infect? as control-measures? for clarity
- Decoupled isolation of symptomatics from lockdown, now isolation begins above a threshold and continues indefinitely. Note: fix occurence agents recover before the end of isolation period and continue isolating indefinitely (happens when lockdown off but isolation on)
- Added separate thresholds for lockdown, control measures, and isolation of symptomatics

### 16/6

- Created experiments for extremes validation

### 17/6

- Fixed contacts number bug where a spike in contacts happened before second lockdown
- Changed contact variable names from E for exposeds to L for latents

### 18/6

- Cleaned up and updated R visualisation script

### 19/6

- Implemented test and trace, but not a system to stop self-isolation yet