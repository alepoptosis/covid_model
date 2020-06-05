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