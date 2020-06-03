# Implementation summary

## Week 1
### 21/5

- Added a num-contacts plot to keep track of the total number of contacts over time
- Implemented exposeds (E), and passage from susceptible (S) to E and from E to infected (I)

### 22/5

- Exposed people can now infect others
- Removed people now lose immunity after 30-35 days
- Infecteds are now removed either with probability p-remove or after a certain number of ticks
- Infected people have a chance to become dead (D) instead of removed
- Changed cyan indicator for lockdown into black outline

## Week 2
### 27/5

- Implemented log-normal distribution for incubation period, mean and stdev as global variables

### 28/5

- Implemented poisson distribution for immunity period, mean to be varied in simulations
- Implemented normal distribution for recovery, with absolute result to prevent negative durations
- Implemented imposed lockdown threshold with strictness level parameter, removed risk-awareness system
- Removed all contact weighing

### 29/5

- Implemented control measures to reduce probability of transmission after first lockdown, with effectiveness parameter
- Implemented an option to open the system, which allows susceptibles to be infected by outside sources depending on travel strictness

## Week 3
### 1/6

- Implemented two separate start/end lockdown procedures to ensure strictness probability is enforced
- Impemented three age ranges, 0-29, 30-59, and 60+, assigned to people based on demographics of the UK population
- Implemented modified probability of death depending on age range

### 2/6

- Created github repo
- Changed global variable probabilities from decimals to integers for ease of understanding
- Separated recurring processed into dedicated procedures
