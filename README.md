# TO-DO list from code review

- ~~Assign asymptomatic prevalence by age range, like probability of death~~ Done following Chang et al., i.e. ~13% sym in children (<=18) and ~67% sym in adults (>18). We may have to add more references for that, and maybe even raise asym infectiousness to 30% like Chang et al. does
- ~~Add protection-compliance to determine what percentage of agents adopts personal protection~~
- ~~Reduce the risk of exposure from agents with personal protection, like for asymptomatics - this implies that PP needs to be extended to all breeds~~
- ~~Add an isolation-compliance-traced for traced contacts~~
- Ensure naming convention is consistent (e.g. test-coverage-sym instead of sym-test-coverage)
- ~~Turn isoltion-threshold into isolation-sym-threhsold to specify measure~~
- ~~Introduce separate isolation duration for contacts vs symptomatics~~
- ~~Find better name for imported-infection (maybe unusual-infection or extraneous-infection)~~
- ~~Expand countdown names (iso-countdown to isolation-countdown etc.)~~
- ~~Add a neighbours-met-per-day slider and make it so that each agent only interacts with that fixed percentage of their neighbourhood every tick~~
- ~~Test possible ways to only record contacts from x days before~~ 
- ~~^ Figure out what to do with the contact history once active cases dip below the threshold~~
- *Test using an external file to set age ranges, their prevalence, and relative asymptomatic percentages and risk of death* -> in progress, however this could be very difficult to do in a clean way (e.g. would have to read the number of lines in csv to figure out how many brackets there are etc.) and may be more trouble than it's worth
- ~~Move threshold calculation from setup-globals to modify-measures so that the threshold is updated as agents die (or could there be a way to only do this when agents die?)~~
- ~~Introduce an agentset called todays-contacts to slim up the process of counting and recording contacts, and exposing susceptibles. This would be a subset of the neighbours that an agent actually made contact with. The count of contacts would then just be the sum of agentset counts / 2 and would ensure that if an agent is only interacting with a percentage of its neighbourhood, that the agents chosen are consistent throughout a tick.~~
- ~~Find a name for the random duration of the period between incubation and development of symptoms~~
- ~~Change contact lists to agentsets~~
- ~~Instead of using a random p value and comparing it to the parameter value, use n-of where n is the number of agents calculated from the % compliance for that measure (e.g. lockdown or shielding)~~ (doesn't support floats, left old system in place)
- Explain to the user that the order of procedures in modify-measures is critical to ensure that lockdown and shielding don't release agents that are being isolated for other reasons (i.e. isolation of symptomatics and test and trace)
- ~~Make the isolate procedure less convoluted~~
- ~~Prevent the while loop of the normal distribution from looping infinitely~~
- ~~Ensure that symptomatics who finish isolation before recovering don't start a new course of isolation due to isolation of symptomatics~~
- ~~Make infectiousness of asymptomatics a global parameter~~
- ~~Make infectiousness of symptomatics with masks a global parameter~~ (Not necessary, it's regulated by protection strength)
- ~~Change wording of extraneous to exogenous~~ 
- Change reporters on infections and deaths to use the csv-data (change name to pop-data) to store and retrieve the counts
 
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

- Implemented full test and trace, applied to the asymptomatic or pre-symptomatic infecteds with probability test-coverage, and to symptomatics with probability symp-test-coverage, with contacts isolating in the same fashion as the isolate-symptomatics? procedure

## Week 6

### 23/6

- Changed visualisation to use number of cases against threshold to determine whether lockdown is on instead of number of isolated agents

### 26/6

- Implemented shelter in place system for agents in the 60+ range
- Designed experiments for testing different combinations of control measures
- Reintroduced count of locked agents as a reporter

## Week 7

### 29/6

- Renamed R script to base_vis
- Streamlined R script options so only first line has to be changed

### 30/6

- Created an R script to streamline visualisation of varying parameter experiments

### 1/7

- Changed wording of shelter-at-risk mechanism to shield-at-risk to be more coherent with UK terminology
- Added reporters of number of infecteds by age

### 2/7

- Edited script for visualisation of varying parameter experiments to support two varying parameters by using a heatmap instead of a line chart
- Changed the name of the mechanic describing the use of masks/social distancing/hand washing from control measures to personal protection

## Week 8

### 6/7

- Added code to create the classic breeds plot for vary-type experiments as well

### 7/7

- Added caption line for number of deaths in the first year and total number of infections (where available)

### 8/7

- Fixed wrong caption

### 10/7

- Added a switch to turn off loss of immunity for runs not involving that mechanic
- Fixed a bug where contacts would keep isolating indefinitely since the isolation check only kicked in above the threshold
- Restored Exposed breed instead of Latent, made them non-infectious
- Renamed shield-at-risk as shield-vulnerable
- Changed breed progression so that E become A right before the end of their incubation period, then they can either remain A until recovery, or become I after a few ticks and progress from there

## Week 9

- Added a visualisation for the log of cases vs time to analyse the effect of varying p-inf-init
- Fixed the random number generators to include floats, this will ensure that float parameters (especially those between 0 and 1) actually work instead of just effectively being 0

## Week 10

- Fixed a bug that did not exclude isolated susceptibles from travelling, now only susceptibles with z-contant higher than 0 can be infected by the travel mechanic
- Added and fixed comments
- Ensured all measures could operate independently of one another

## Week 11 onwards: write-up and minor code fixes

## Post-dissertation

### 11/9

- Added profiler button to test speed of different procedures
- Changed count-contacts system for a faster one that does not separate different types of contats, but simply counts all contacts per tick
- Changed system regulating agent neighbourhood: instead of checking agents within n-radius every time it's necessary, each agent is now assigned an agentset of "neighbours" at setup based on their radius, which is interrogated when necessary. Instead of z-contact-init and z-contact, now the agent has a simple boolean that flags them as isolating or not
- In modify-measures, number of cases is now only checked once at the beginning instead of at every measure
- Optimised logical checks for lockdown and isolation of symptomatics to skip code where possible
- Fixed shield-vulnerable mechanic, as it re-interrogated S agents over 60 at each tick instead of only when the threshold is first surpassed, leading to a change in which agents were shielding every tick. Now agents are only interrogated at first, and ALL over 60 are isolated, not just susceptibles
- Ensured all isolation checks depend on "isolated?" flag and not agent shape anymore, to prepare for a version of the model that can toggle off visual elements to speed up headless simulations (not fully implemented yet)
- Updated check-isolation to ensure 60+ agents who leave isolation for other reasons don't get released if shielding is in progress, not only if a lockdown is happening
- Updated variables list accordingly

## Refer to commits for further updates