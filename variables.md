# NETLOGO COVID-19 MODEL VARIABLES

Abbreviations: **G**lobals, **S**usceptibles, **E**xposeds, **A**symptomatics, symptomat**I**cs, **R**ecovereds

| name                     	  | access 	| description                                                               	|
|-----------------------------|---------|-------------------------------------------------------------------------------|
| min-radius                  | G       | minimum radius an agent can have                                              |
| daily-contacts              | G       | % of neighbours each agent makes contact with each tick                       |
| p-infect-base               | G       | basic probability of catching the virus upon contact                          |
| asym-prevalence             | G       | % of infections that does not develop symptoms                                |
| min-immunity-duration       | G       | minimum number of days a recovered agent will have immunity                   |
| recovery-mean               | G       | mean of the normal distribution for recovery countdowns                       |
| recovery-stdev              | G       | std deviation of the normal distribution for recovery countdowns              |
| death-mean                  | G       | mean of the normal distribution for death countdowns                          |
| death-stdev                 | G       | std deviation of the normal distribution for death countdowns                 |
| incubation-mean             | G       | mean of the log-normal distribution for incubation countdowns                 |
| incubation-stdev            | G       | std deviation of the log-normal distribution for incubation countdowns        |
| lockdown-threshold          | G       | % of population that needs to be I to trigger imposed lockdown                |
| lockdown-compliance         | G       | % of the population that complies with the lockdown measures                  |
| shield-threshold            | G       | % of population that needs to be I to trigger shielding of the vulnerable     |
| shield-compliance           | G       | % of at-risk population that complies with shielding measures                 |
| protections-threshold       | G       | % of population that needs to be I to trigger personal protections            |
| protections-strength        | G       | % reduction in probability of infections for agents adopting protections      |
| testtrace-threshold         | G       | % of population that needs to be I to trigger test and trace                  |
| test-coverage-sym           | G       | % of untested symptomatic agents that get tested each tick                    |
| test-coverage-asym          | G       | % of untested exposed and asymptomatic agents that get tested each tick       |
| contacts-traced             | G       | % of contacts of the tested agent that are asked to isolate                   |
| isolation-compliance-tested | G       | % of tested agents that isolate following a positive test result              |
| isolation-compliance-traced | G       | % of tested agents that isolate when being traced as contacts                 |
| isolation-sym-threshold     | G       | % of population that needs to be I to trigger isolation of symptomatics       |
| isolation-compliance-sym    | G       | % of symptomatic agents that complies with isolation of symptomatics          |
| isolation-duration-case     | G       | duration of isolation period for infected or symptomatic agents (TT or IS)    |
| isolation-duration-contact  | G       | duration of isolation period for contacts of tested agents                    |
| duration                    | G       | number of years to simulate                                                   |
| initial-infected            | G       | % of population that is exposed at the start of the simulation                |
| extraneous-infection        | G       | probability per tick to introduce an external infection into the system       |
| lose-immunity?              | G       | whether recovereds can lose post-infection immunity                           |
| visual-elements?            | G       | whether visual elements should be rendered (turn off when running headless)   |
| allow-extraneous-infections?| G       | whether external infections can occur                                         |
| imposed-lockdown?           | G       | whether to simulate imposed lockdown measures                                 |
| shield-vulnerable?          | G       | whether to simulate shielding of the vulnerable measures                      |
| personal-protections?       | G       | whether to simulate personal protections measures                             |
| test-and-trace?             | G       | whether to simulate test and trace measures                                   |
| isolate-symptomatics?       | G       | whether to simulate isolation of symptomatics measures                        |
| pop-size                 	  | G      	| number of agents in simulation                                            	|
| update-thresholds?          | G       | whether thresholds need to be updated due to the death of a number of agents  |
| lockdown-active?         	  | G      	| whether an imposed lockdown is currently in progress                      	|
| lockdown-threshold-num   	  | G      	| number of I agents to trigger lockdown                                    	|
| agents-at-risk           	  | G      	| set of agents over at-risk age (70)                                       	|
| shielding-active?        	  | G      	| whether shielding of vulnerable is currently in progress                  	|
| shield-threshold-num     	  | G      	| number of I agents to trigger shielding                                   	|
| protections-active?         | G      	| whether personal protections are currently in place                       	|
| protections-threshold-num   | G      	| number of I agents to trigger personal protections                        	|
| p-infect-adj             	  | G      	| p-infect after reduction of risk from protections                         	|
| testtrace-threshold-num  	  | G      	| number of I agents to trigger test and trace                              	|
| start-isolation?         	  | G      	| whether isolation of symptomatics has begun                               	|
| isolation-sym-threshold-num | G      	| number of I agents to trigger isolation of symptomatics                   	|
| num-contacts             	  | G      	| number of contacts between agents for current tick                        	|
| count-inf-0-39           	  | G      	| cumulative counts of infecteds for each age range                         	|
| count-inf-40-49          	  | G      	| cumulative counts of infecteds for each age range                         	|
| count-inf-50-59          	  | G      	| cumulative counts of infecteds for each age range                         	|
| count-inf-60-69          	  | G      	| cumulative counts of infecteds for each age range                         	|
| count-inf-70-79          	  | G      	| cumulative counts of infecteds for each age range                         	|
| count-inf-80plus         	  | G      	| cumulative counts of infecteds for each age range                         	|
| count-dead-0-39          	  | G      	| cumulative counts of dead agents for each age range                       	|
| count-dead-40-49         	  | G      	| cumulative counts of dead agents for each age range                       	|
| count-dead-50-59         	  | G      	| cumulative counts of dead agents for each age range                       	|
| count-dead-60-69         	  | G      	| cumulative counts of dead agents for each age range                       	|
| count-dead-70-79         	  | G      	| cumulative counts of dead agents for each age range                       	|
| count-dead-80plus        	  | G      	| cumulative counts of dead agents for each age range                       	|
| age                      	  | ALL    	| age range of the agent                                                    	|
| p-death                  	  | ALL    	| individual probability of death based on age range                        	|
| radius                   	  | ALL    	| contact radius of the agent                                               	|
| neighbours               	  | ALL    	| set containing the agent's contacts                                       	|
| staying-at-home?         	  | ALL    	| whether the agent is currently isolating, shielding or in lockdown        	|
| traced?                  	  | ALL    	| whether the agent was traced as a contact of a tested agent               	|
| asked-to-isolate?        	  | ALL    	| whether the agent was personally asked to isolate by IS or TT             	|
| comply-with-isolation?   	  | ALL    	| whether the agent decided to comply with an isolation request by IS or TT 	|
| isolation-countdown         | ALL    	| individual isolation countdown                                            	|
| using-protections?      	  | ALL    	| whether the agent is currently adopting personal protections                	|
| todays-contacts             | ALL     | set containing the neighbours contacted by the agent in the current tick      |
| p-infect                 	  | S      	| individual probability of catching the virus                              	|
| to-become-exposed?       	  | S      	| flag a S agent for exposure (E)                                           	|
| incubation-countdown     	  | E      	| individual incubation countdown                                           	|
| to-become-asymptomatic?  	  | E      	| flag an E agent to become asymptomatic (A)                                	|
| contact-list             	  | E A I  	| list of neighbours the agent came in contact with since exposure          	|
| tested?                  	  | E A I  	| whether the agent is aware of their infection status                      	|
| contacts-alerted?        	  | E A I  	| whether the agent's contacts have been instructed to isolate              	|
| presym-period               | E A     | number of days (1-3) in which agent is infectious but not (yet) symptomatic   |
| will-develop-sym?        	  | A      	| whether the agent will develop symptoms (become I) or not (stay A)        	|
| countdown                	  | A      	| multi-purpose countdown: symptoms if will-develop-sym?, removal if not    	|
| to-become-sym?           	  | A      	| flag an A agent to become symptomatic (I)                                 	|
| to-recover?              	  | A I    	| flag an agent (A or I) to recover (R)                                     	|
| will-die?                	  | I      	| whether the agent will die (become D) or not (become R)                   	|
| countdown                	  | I      	| multi-purpose countdown: death if will-die, recovery if not               	|
| to-die?                  	  | I      	| flag a I agent to die (D)                                                 	|
| immunity-countdown       	  | R      	| individual immunity countdown                                             	|
| to-become-susceptible?   	  | R      	| flag a R agent to lose immunity (S)                                       	|