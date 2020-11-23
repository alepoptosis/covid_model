extensions [
  profiler
  csv
  table
]

breed [susceptibles susceptible]    ;; can be infected (S)
breed [exposeds exposed]            ;; exposed but not infectious (E)
breed [symptomatics symptomatic]    ;; infectious and symptomatic (I)
breed [asymptomatics asymptomatic]  ;; infectious and asymptomatic (A)
breed [recovereds recovered]        ;; recovered and immune (R)

globals [
  csv-data                  ;; csv for population data
  pop-size                  ;; number of agents in simulation
  update-thresholds?        ;; whether thresholds need to be updated due to the death of a number of agents

  ;; lockdown globals
  lockdown-active?          ;; whether an imposed lockdown is in progress
  lockdown-threshold-num    ;; number of I agents to trigger lockdown

  ;; shielding globals
  agents-at-risk            ;; set of agents over 60
  shielding-active?         ;; whether shielding of vulnerable is in progress
  shield-threshold-num      ;; number of I agents to trigger shielding

  ;; personal protection globals
  protections-active?        ;; whether personal protections are in place
  protections-threshold-num  ;; number of I agents to trigger personal protections
  p-infect-adj               ;; p-infect after reduction of risk from protections

  ;; test and trace globals
  record-contacts?          ;; whether to start recording contacts
  testtrace-threshold-num   ;; number of I agents to trigger test and trace

  ;; isolation of symptomatics globals
  start-isolation?          ;; whether isolation of symptomatics has begun
  isolation-sym-threshold-num   ;; number of I agents to trigger isolation of symptomatics

  ;; reporters
  num-contacts              ;; number of contacts between agents for current tick
]

turtles-own [
  age                       ;; age range of the agent
  p-death                   ;; individual probability of death based on age range
  radius                    ;; contact radius of the agent
  neighbours                ;; set containing the agent's contact
  staying-at-home?          ;; whether the agent is currently isolating, shielding or in lockdown
  traced?                   ;; whether the agent was traced as a contact of a tested agent
  asked-to-isolate?         ;; whether the agent was already asked to isolate by IS or TT
  comply-with-isolation?    ;; whether the agent decided to comply with an isolation request by IS or TT
  isolation-countdown       ;; individual isolation countdown
  using-protections?        ;; whether the agent is currently adopting personal protections e.g. masks, hand-washing
  todays-contacts           ;; set containing the neighbours contacted by the agent in the current tick
]

susceptibles-own [
  p-infect                  ;; individual probability of catching the virus
  to-become-exposed?        ;; flag a S agent for exposure (E)
]

exposeds-own [
  incubation-countdown      ;; individual incubation countdown
  to-become-asymptomatic?   ;; flag an E agent to become asymptomatic (A)
  contact-list              ;; list of who the agent came in contact up to contact-history-length days prior
  tested?                   ;; whether the agent is aware of their infection status
  contacts-alerted?         ;; whether its contacts have been instructed to isolate
  presym-period             ;; number of days (1-3) in which agent is infectious but not (yet) symptomatic
]

asymptomatics-own [
  will-develop-sym?         ;; whether the agent will develop symptoms (become I) or not (stay A)
  countdown                 ;; multi-purpose countdown: symptoms if will-develop-sym?, removal if not
  to-become-sym?            ;; flag an A agent to become symptomatic (I)
  to-recover?               ;; flag an A agent to recover (R)
  contact-list              ;; list of who the agent came in contact up to contact-history-length days prior
  tested?                   ;; whether the agent is aware of their infection status
  contacts-alerted?         ;; whether its contacts have been instructed to isolate
  presym-period             ;; number of days (1-3) in which agent is infectious but not (yet) symptomatic
]

symptomatics-own [
  will-die?                 ;; whether the agent will die (become D) or not (become R)
  countdown                 ;; multi-purpose countdown: death if will-die, recovery if not
  to-die?                   ;; flag a I agent to die (D)
  to-recover?               ;; flag a I agent to recover (R)
  contact-list              ;; list of who the agent came in contact up to contact-history-length days prior
  tested?                   ;; whether the agent is aware of their infection status
  contacts-alerted?         ;; whether its contacts have been instructed to isolate
]

recovereds-own [
  immunity-countdown        ;; individual immunity countdown
  to-become-susceptible?    ;; flag a R agent to lose immunity (S)
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; PROFILER ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to profile
  setup                         ;; set up the model
  profiler:start                ;; start profiling
  repeat (duration * 365) [go]  ;; run one instance of the simulation
  profiler:stop                 ;; stop profiling
  print profiler:report         ;; print the results
  profiler:reset                ;; clear the data
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; SETUP ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  parse-csv
  setup-turtles
  setup-globals
  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; SETUP PROCEDURES ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-turtles
  set-default-shape turtles "person"
  ask patches [
    set pcolor white
    ;; place a susceptible on each patch
    sprout-susceptibles 1 [
      ;; setup turtle attributes
      set radius (pareto-dist min-radius 2)
      set staying-at-home? false
      set traced? false
      set asked-to-isolate? false
      set comply-with-isolation? false
      set isolation-countdown -1
      set using-protections? false
      set todays-contacts nobody

      ;; setup susceptible-specific attributes
      set-breed-susceptible
    ]
  ]

  ;; assign neigbours agentsets (can only be done once all agents are assigned a radius)
  ask turtles [
    set neighbours (other turtles in-radius radius with [radius >= distance myself])
  ]

  ;; assign age and relative death probability to all agents
  set-age-and-p-death

  ;; infect a number of agents equal to initial-infected
  set pop-size (count turtles)
  let to-infect round (initial-infected * pop-size / 100)
  ask n-of to-infect turtles [set-breed-exposed]
end

to setup-globals
  ;; pop-size is set in setup-turtles

  ;; lockdown globals
  set lockdown-active? false
  set lockdown-threshold-num (absolute-threshold lockdown-threshold)

  ;; shielding globals
  set agents-at-risk (turtles with [age = "60-69" or age = "70-79" or age = "80+"])
  set shielding-active? false
  set shield-threshold-num (absolute-threshold shield-threshold)

  ;; personal protection globals
  set protections-active? false
  set protections-threshold-num (absolute-threshold protections-threshold)
  set p-infect-adj ((1 - (protections-strength / 100)) * (p-infect-base / 100))

  ;; test and trace globals
  set testtrace-threshold-num (absolute-threshold testtrace-threshold)
  set record-contacts? false

  ;; isolation of symptomatics globals
  set isolation-sym-threshold-num (absolute-threshold isolation-sym-threshold)

  ;; tt & is globals
  set start-isolation? false

  ;; all measures globals
  set update-thresholds? false

  ;; num-contacts is reset at every tick in count-contacts
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; CSV PARAMETERS PARSING ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to parse-csv
  ;; Parse the csv into a table as key:value pairs.
  ;; The resulting data structure is a tree.

  ; close any files open from last run
  file-close-all
  file-open "pop-data.csv"

  ; create the destination table
  set csv-data table:make

  do-parsing
end

to do-parsing
  let total-percentage 0
  let header csv:from-row file-read-line

  while [ not file-at-end? ] [
    let row csv:from-row file-read-line
    let age-bracket item 0 row
    let population-percentage item 1 row
    let death-probability item 2 row

    ;; put age bracket parameters into the table
    put-age-bracket-data age-bracket "population-percentage" population-percentage
    put-age-bracket-data age-bracket "death-probability" death-probability

    set total-percentage (population-percentage + total-percentage)
  ]

  ; csv sanity check
  if total-percentage != 100 [
    error "Please ensure sum of percentages in 'pop-data.csv' is 100."
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; TABLE PROCEDURES ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to increase-age-bracket-counter [#age-bracket #counter-name]
  ; Increase the value of a counter for the specified age-bracket by 1.
  let age-bracket-data table:get csv-data #age-bracket
  let counter-value table:get-or-default age-bracket-data #counter-name 0
  set counter-value counter-value + 1
  put-age-bracket-data #age-bracket #counter-name counter-value
end

to-report get-age-bracket-data [#age-bracket #key]
  ; Report the value for the specified key of an age-bracket.
  ;
  ; If the specified key does not exist, it is created to avoid runtime errors.
  ; This should only happen when trying to retrieve a counter using the observer.

  let age-bracket-data-table table:get csv-data #age-bracket

  ifelse table:has-key? age-bracket-data-table #key [
    report table:get age-bracket-data-table #key
  ][; else
    ; avoid runtime errors if the counter has not been created yet
    put-age-bracket-data #age-bracket #key 0
    report 0
  ]
end

to put-age-bracket-data [#age-bracket #key #value]
  ; Put a table with key/value into the specified age-bracket.
  let age-bracket-table table:get-or-default csv-data #age-bracket table:make
  table:put age-bracket-table #key #value
  ; create, or overwrite, the age-bracket-table
  table:put csv-data #age-bracket age-bracket-table
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; GO ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  ifelse ticks < (duration * 365)
  [
    make-contact
    count-contacts                        ;; count the number of contacts between agents at each tick
    if test-and-trace? [record-contacts]  ;; record contacts of infected agents (E, A or I) and update their contact list
    expose-susceptibles                   ;; check whether non-isolating susceptibles become exposed to the virus
    progress-exposed                      ;; turn exposed agents that are almost the end of their incubation countdown into asymptomatic
    progress-asym                         ;; progress asymptomatic agents through their countdown (to either develop symptoms or recover)
    progress-sym                          ;; progress symptomatic agents through their countdown (to either die or recover)
    if lose-immunity? [lose-immunity]     ;; progress recovered agents through their immunity countdown (to return susceptible)
    update-breeds                         ;; change breeds of agents who moved onto a different stage during that tick
    modify-measures                       ;; update and implement control measures
    tick
  ] [
    stop
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; GO PROCEDURES ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to make-contact
  ;; establish who makes contact with each agent in this tick
  ask turtles with [not staying-at-home?] [
    let available-neighbours (neighbours with [not staying-at-home?])

    ;; prevents useless calculations if agents contacts everyone they can daily
    ifelse daily-contacts = 100 [
      set todays-contacts available-neighbours
    ] [ ;; else
      let n round (count available-neighbours * daily-contacts / 100)
      set todays-contacts (n-of n available-neighbours)
    ]
  ]
end

to count-contacts
  ;; count the number of contacts between agents at each tick
  ;; i.e. the total number of contacts divided by 2, since contacts are bilateral
  set num-contacts (round (sum [count todays-contacts] of turtles / 2))
end

to record-contacts
  ;; record contacts of infected agents (E, A or I) and update their contact list

  ;; ensure recording of contacts doesn't turn off once the threshold is first passed
  ;; this is because we are simulating manual contact tracing rather than one dependent
  ;; on tracking by an app, the government, or services (e.g. restaurants, pubs)
  if not record-contacts? and count symptomatics >= testtrace-threshold-num [
    set record-contacts? true
  ]

  ;; this system ensures an agent's contact history only goes back a certain number of days
  ;; (defined by contact-history-length)
  if record-contacts? [
    ask (turtle-set exposeds asymptomatics symptomatics) [
      ;; add today's contacts to the top of the contact-list
      set contact-list (fput todays-contacts contact-list)
      ;; if the addition means the list goes over the allowed length
      ;; remove the oldest entry, i.e. the last item of the list
      if length contact-list > contact-history-length [
        set contact-list (but-last contact-list)
      ]
    ]
  ]
end


to expose-susceptibles
  ;; check whether non-isolating susceptibles become exposed to the virus
  ask susceptibles with [not staying-at-home?] [

    let sym-contacts (todays-contacts with [breed = symptomatics and not staying-at-home?])
    let num-sym (count sym-contacts with [not using-protections?])
    ;; adjust number of symptomatics that use protections to account for their lower probability of transmission (determined by protections-strength)
    let num-sym-protections ((count sym-contacts - num-sym) * protections-strength / 100)
    ;; if the new number is between 0 and 1 (exclusive) set it to 1, as raising a number to a decimal lowers it
    if num-sym-protections > 0 and num-sym-protections < 1 [set num-sym-protections 1]

    ;; adjust number of asymptomatics to account for their lower probability of transmission (currently 10%)
    let num-asym ((count todays-contacts with [breed = asymptomatics and not staying-at-home? and not using-protections?]) * asym-infectiousness / 100)
    if num-asym > 0 and num-asym < 1 [set num-asym 1]

    ;; total number of infected contacts
    let total-inf (num-sym + num-sym-protections + num-asym)

    ;; if there are infected contacts, the probability of at least one of them causing infection
    ;; is 1 - the probability that none do (p-exposure)
    if total-inf != 0 [
      let p-exposure (1 - ((1 - p-infect) ^ total-inf))
      let p (random-float 100)
      if p < (p-exposure * 100) [
        set to-become-exposed? true
      ]
    ]
  ]

  ;; if infections originating outside the agent's normal social circle are permitted
  ;; each tick has a probability that one susceptible agent who is not isolating
  ;; will become exposed even if none of his contacts are infected
  if allow-exogenous-infections? [
    let candidates (susceptibles with [not staying-at-home?])
    if any? candidates [
      let p (random-float 100)
      if p < exogenous-infection [
        ask one-of candidates [set-breed-exposed]
      ]
    ]
  ]
end

to progress-exposed
  ;; turn exposed agents that are almost the end of their incubation countdown into asymptomatic
  ask exposeds [
    ifelse incubation-countdown <= presym-period
    [set to-become-asymptomatic? true]
    [set incubation-countdown (incubation-countdown - 1)]
  ]
end

to progress-asym
  ;; progress asymptomatic agents through their countdown
  ;; at the end of it, they either develop symptoms or recover
  ask asymptomatics [
    ifelse countdown <= 0
    [
      ifelse will-develop-sym? [
        set to-become-sym? true
      ] [ ;; else
        set to-recover? true
      ]
    ] [ ;; else
      set countdown (countdown - 1)
      ]
    ]
end

to progress-sym
  ;; progress symptomatic agents through their countdown
  ;; at the end of it, they either recover or die
  ask symptomatics [
    ifelse countdown <= 0
    [
      ifelse will-die? [
        set to-die? true
      ] [ ;; else
        set to-recover? true
      ]
    ] [ ;; else
      set countdown (countdown - 1)
    ]
  ]
end

to lose-immunity
  ;; progress recovered agents through their immunity countdown
  ;; at the end of it, they return susceptible
  ask recovereds [
    ifelse immunity-countdown <= 0 [
      set to-become-susceptible? true
    ] [ ;; else
      set immunity-countdown (immunity-countdown - 1)
    ]
  ]
end


to update-breeds
  ;; change breeds of agents who moved onto a different stage
  ask susceptibles with [to-become-exposed?] [
    set-breed-exposed
  ]

  ask exposeds with [to-become-asymptomatic?] [
    ; increase the counter of infected agents in the same age-bracket
    increase-age-bracket-counter age "infected"
    set-breed-asymptomatic
  ]

  ask asymptomatics with [to-become-sym?] [
    set-breed-symptomatic
  ]

  ask (turtle-set asymptomatics symptomatics) with [to-recover?] [
    set-breed-recovered
  ]

  let agents-to-die (symptomatics with [to-die?])

  ;; before agents marked for death are removed from the population
  ;; the model is notified that the threshold will have to be updated
  if any? agents-to-die [
    set update-thresholds? true
  ]

  ask agents-to-die [
    ; increase the counter of deceased agents in the same age-bracket
    increase-age-bracket-counter age "deceased"
    die
  ]

  ;; population size and tresholds are updated only if needed
  if update-thresholds? [
    set pop-size (count turtles)

    if imposed-lockdown? [set lockdown-threshold-num (absolute-threshold lockdown-threshold)]
    if shield-vulnerable? [set shield-threshold-num (absolute-threshold shield-threshold)]
    if personal-protections? [set protections-threshold-num (absolute-threshold protections-threshold)]
    if test-and-trace? [set testtrace-threshold-num (absolute-threshold testtrace-threshold)]
    if isolation-symptomatics? [set isolation-sym-threshold-num (absolute-threshold isolation-sym-threshold)]

    set update-thresholds? false
  ]

  if lose-immunity? [
    ask recovereds with [to-become-susceptible?] [
      set-breed-susceptible
    ]
  ]
end

to modify-measures
  ;; update control measures based on the current number of active cases
  let active-cases (count symptomatics)

  ;;;; IMPOSED LOCKDOWN
  ;; lockdown starts once active cases are past the threshold, if it wasn't already active
  ;; lockdown ends once active cases are below the threshold, if it was still active
  if imposed-lockdown? [
    ifelse active-cases >= lockdown-threshold-num [
      if not lockdown-active? [start-lockdown]
    ] [ ;; else
      if lockdown-active? [end-lockdown]
    ]
  ]

  ;;;; SHIELD VULNERABLE
  ;; shielding starts once active cases are past the threshold, if it wasn't already active
  ;; shielding ends once active cases are below the threshold, if it was still active
  if shield-vulnerable? [
    ifelse active-cases >= shield-threshold-num [
      if not shielding-active? [start-shielding]
    ] [ ;; else
      if shielding-active? [end-shielding]
    ]
  ]

  ;;;; PERSONAL PROTECTION
  ;; while active cases are past the threshold, p-infect is lowered for all susceptible agents
  ;; while active cases are below the threshold, p-infect returns to p-infect-base for all susceptible agents
  if personal-protections? [
    ifelse active-cases >= protections-threshold-num [
      if not protections-active? [start-protections]
    ] [ ;; else
      if protections-active? [end-protections]
    ]
  ]

  ;;;; TEST AND TRACE
  ;; while active cases are past the threshold, test and trace are carried out on infected agents
  if test-and-trace? [
    if active-cases >= testtrace-threshold-num [
      test
      trace
    ]
  ]

  ;;;; ISOLATE SYMPTOMATICS
  ;; once active cases are past the threshold, isolation of symptomatic agents is switched on
  ;; and remains active for the entirety of the simulation
  if isolation-symptomatics? and not start-isolation? [
    if active-cases >= isolation-sym-threshold-num [
      set start-isolation? true
    ]
  ]

  ;;;;; ISOLATION (IS and TT)
  ;; this section carries out isolation for both test and trace and isolation of symptomatics
  ;; test and trace doesn't check threshold to ensure agents finish their isolation
  ;; even if active cases dip below it
  if test-and-trace? or start-isolation? [
    update-isolation-status
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; SUPPORTING PROCEDURES ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; SETUP PROCEDURES

to set-age-and-p-death
  ;; Set the turtle parameters using data from the csv.

  let agents-to-update turtles

  ; set age related parameters
  foreach table:keys csv-data [ age-bracket ->
    ; get parameters for this age-bracket
    let age-bracket-table table:get csv-data age-bracket
    let population-percentage table:get age-bracket-table "population-percentage"
    let death-probability table:get age-bracket-table "death-probability"

    ; create agents subset
    let number-of-agents (population-percentage * count turtles) / 100
    let updated-agents (n-of number-of-agents agents-to-update)

    ; set agents' values
    ask updated-agents [set age age-bracket]
    ask updated-agents [set p-death death-probability]

    ; remove them from the temp agentset
    set agents-to-update agents-to-update with [not member? self updated-agents]
  ]

  ;; ensure we assigned the age bracket to all agents
  if count agents-to-update != 0 [
    error "An error occurred while settings the age-bracket for agents."
  ]
end

;;;;; BREED SETTING PROCEDURES

to check-outline
  ;; ensures agents retain their outline when changing breed
  if staying-at-home? [
    set shape "person-outline"
  ]
end

to set-breed-susceptible
  set breed susceptibles
  set p-infect (p-infect-base / 100)
  set to-become-exposed? false
  set asked-to-isolate? false
  if visual-elements? [
    set color green
    check-outline
  ]
end

to set-breed-exposed
  set breed exposeds
  set incubation-countdown (log-normal incubation-mean incubation-stdev 0)
  set to-become-asymptomatic? false
  set contact-list []
  set tested? false
  set contacts-alerted? false
  set presym-period (random 3 + 1)
  if visual-elements? [
    set color yellow
    check-outline
  ]
end

to set-breed-asymptomatic
  set breed asymptomatics
  check-symptoms               ;; sets will-develop-sym? and countdown
  set to-become-sym? false
  set to-recover? false
  ;; contact-list carries over from exposeds
  ;; tested? carries over from exposeds
  if visual-elements? [
    set color violet
    check-outline
  ]
end

to check-symptoms
  ;; check whether the agent will remain asymptomatic or develop symptoms
  ;; and assign a value to the countdown accordingly
  let p random-float 100
  ;; adjust asymptomatic prevalence based on age
  let asym-prevalence (actual-asym-prevalence age)
  ifelse p < asym-prevalence [
    set will-develop-sym? false
    set countdown (normal-dist recovery-mean recovery-stdev) ;; recovery countdown
  ] [ ;; else
    set will-develop-sym? true
    set countdown presym-period                              ;; symptoms countdown
  ]
end

to set-breed-symptomatic
  set breed symptomatics
  check-death                  ;; sets will-die? and countdown
  set to-die? false
  set to-recover? false
  ;; contact-list carries over from asymptomatic
  ;; tested? carries over from asymptomatic
  if visual-elements? [
    set color red
    check-outline
  ]
end

to check-death
  ;; check whether the agent will recover or die
  ;; and assign a value to the countdown accordingly
  let p random-float 100
  ifelse p <= p-death [
    set will-die? true
    set countdown (normal-dist death-mean death-stdev) ;; death countdown
  ] [ ;; else
    set will-die? false
    set countdown (normal-dist recovery-mean recovery-stdev) ;; recovery countdown
  ]
end

to set-breed-recovered
  set breed recovereds
  if lose-immunity? [
    set immunity-countdown (log-normal 1 0.5 min-immunity-duration)
    ]
  set to-become-susceptible? false
  if visual-elements? [
    set color grey
    check-outline
  ]
end

;;;;; ISOLATION AND RELEASE PROCEDURES

to isolate-agent
  set staying-at-home? true
  if visual-elements? [set shape "person-outline"]
end

to release-agent
  ;; makes agents stop isolating

  ;; agents can stop isolating only if the following are true:
  ;; - lockdown is not active
  ;; - they are not at risk or, if they are, shielding is not active
  ;; - they are not required to isolate because of other measures

  if not lockdown-active? [
    if not shielding-active? or not member? self agents-at-risk [
      if isolation-countdown <= 0 [
        set staying-at-home? false
        if visual-elements? [set shape "person"]
      ]
    ]
  ]
end

to start-lockdown
  ;; ask all agents to go into lockdown
  ;; and ensure only a percentage (lockdown-compliance) does so
  set lockdown-active? true

  ask turtles [
    let p (random-float 100)
    if p < lockdown-compliance [
      isolate-agent
    ]
  ]
end

to end-lockdown
  ;; stop the lockdown measure and allow agents to be released
  set lockdown-active? false

  ask turtles [
    release-agent
  ]
end

to start-shielding
  ;; ask all agents at risk to start shielding
  ;; and ensure only a percentage (shield-compliance) does so
  set shielding-active? true

  ask agents-at-risk [
    let p (random-float 100)
    if p < shield-compliance [
      isolate-agent
    ]
  ]
end

to end-shielding
  ;; stop the shielding measure and allow agents at risk to be released

  set shielding-active? false

  ask agents-at-risk [
    release-agent
  ]
end

to start-protections
  ;; protections-compliance % of agents start using protections
  ;; and susceptibles have their p-infect lowered to p-infect-adj
  ask turtles [
    let p (random-float 100)
    if p < protections-compliance [
      set using-protections? true
      if breed = susceptibles [
        set p-infect p-infect-adj
      ]
    ]
  ]
  set protections-active? true
end

to end-protections
  ;; all agents stop using protections
  ;; and susceptibles return to base p-infect
  ask turtles [
    set using-protections? false
    if breed = susceptibles [
      set p-infect (p-infect-base / 100)
    ]
  ]
  set protections-active? false
end

to test
  ;; test symptomatics and exposed/asymptomatics at their respective rates
  ask (turtle-set exposeds asymptomatics) with [not tested?] [
    let p (random-float 100)
    if p < test-coverage-asym [
      set tested? true
    ]
  ]

  ask symptomatics with [not tested?] [
    let p (random-float 100)
    if p < test-coverage-sym [
      set tested? true
    ]
  ]
end

to trace
  ;; identifies contacts of tested agents and flag them as traced

  ;; only a subset of agents contained in contact-list are flagged as traced.
  ;; this allows to simulate different scenarios where only a percentage of
  ;; contacts, set through the interface, are traced

  ask (turtle-set exposeds asymptomatics symptomatics) [
    ;; flagging once contacts are alerted ensures each tested
    ;; agent attempts to reach its contacts only once
    if tested? and not contacts-alerted? [
      ;; the contact-list made of contact-history-length sublists
      ;; is turned into a single agentset
      let contacts-agentset nobody
      foreach contact-list [
        day ->
        set contacts-agentset (turtle-set day contacts-agentset)
      ]
      if contacts-agentset != nobody [
        ;; only a percentage (contacts-traced) of contacts is actually flagged
        let n round (count contacts-agentset * contacts-traced / 100)
        ask n-of n contacts-agentset [set traced? true]
      ]
      set contacts-alerted? true
    ]
  ]
end

to update-isolation-status
  ;; updates the isolation status for agents in two steps:
  ;;
  ;; 1. agents who have not yet been asked to isolate, are now asked if they will comply
  ;;    with the request. Depending on the reason why they have been asked to isolate,
  ;;    they will respond with different probabilities (e.g. tested positive,
  ;;    traced contact, etc.).
  ;;
  ;; 2. all agents who have agreed to comply with the request to isolate, will update
  ;;    their isolation countdown. This will make them isolate, or stop isolating.

  let agents-to-update nobody

  ;; agents who have been tested, or traced as contacts
  if test-and-trace? [
    let tested-agents (turtle-set exposeds asymptomatics symptomatics) with [tested?]
    let traced-agents (turtles with [traced?])

    ask-agents-to-isolate tested-agents isolation-compliance-tested
    ask-agents-to-isolate traced-agents isolation-compliance-traced

    set agents-to-update (turtle-set tested-agents with [comply-with-isolation?] agents-to-update)
    set agents-to-update (turtle-set traced-agents with [comply-with-isolation?] agents-to-update)
  ]

  ;; agents who comply with "isolation of symptomatics"
  if isolation-symptomatics? [
    ask-agents-to-isolate symptomatics isolation-compliance-sym
    set agents-to-update (turtle-set symptomatics with [comply-with-isolation?] agents-to-update)
  ]

  ;; agents who have recovered but may still have an active isolation countdown
  ;; because they have recovered before the end of their isolation
  set agents-to-update (turtle-set recovereds with [isolation-countdown >= 0] agents-to-update)

  ask agents-to-update [update-isolation-countdown]
end

to ask-agents-to-isolate [#agents #isolation-compliance-likelihood]
  ;; asks agents who have not yet been asked if they will comply with the request to isolate
  ;; the response is based on the likelihood of isolating for each group (breed)

  ask #agents with [not asked-to-isolate?] [
    let p (random-float 100)

    ;; compare p to the correct compliance and mark them compliant if they pass
    if p < #isolation-compliance-likelihood [
      set comply-with-isolation? true
    ]

    ;; flag them as having been asked
    set asked-to-isolate? true
  ]
end

to update-isolation-countdown
  ;; updates the isolation countdown and isolation status for an agent.
  ;;
  ;; countdown = -1: the agent was not isolating, so ask them to isolate.
  ;; The isolation period is set depending on the reason why the agent was asked
  ;; to isolate, e.g. traced contact from "test and trace" measure, or symptomatic.
  ;;
  ;; countdown = 0: allow the agent to be released and reset their status.
  ;;
  ;; countdown > 0: decrease the countdown.

  if isolation-countdown = -1 [
    isolate-agent

    ifelse traced? [
      set isolation-countdown isolation-duration-contact
    ] [ ;; else they're a case (e.g. symptomatic, or tested agent)
      set isolation-countdown isolation-duration-case
    ]
  ]

  ;; then it starts being checked, and when it hits 0, the agent is released
  ifelse isolation-countdown = 0 [
    release-agent
    set isolation-countdown -1
    set traced? false
    set comply-with-isolation? false
  ] [ ;; else
    set isolation-countdown (isolation-countdown - 1)
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; REPORTERS ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report actual-asym-prevalence [#age]
  ;; set asymptomatic proportion based on age (Chang et al., 2020)
  let x 0
  ifelse #age = "0-18" [
    set x 87               ;; in children, ~13% cases are symptomatic
  ] [ ;; else
    set x 33               ;; in adults, ~67% of cases are symptomatic
  ]
  report x
end

to-report absolute-threshold [#per]
  ;; turn percentage-based thresholds into absolute numbers
  report round (#per * pop-size / 100)
end

to-report pareto-dist [#min #alpha] ;;? should this one use a while loop like lognormal to establish a minimum?
  ;; report value from a pareto distribution with minimum #min and shape #alpha
  let x (random 100 + 1)
  let num (#alpha * (#min ^ #alpha))
  let den (x ^ (#alpha + 1))
  report round ((num / den) + #min)
end

to-report log-normal [#mu #sigma #shift]
  ;; report value from a log-normal distribution with mean #mu and stdev #sigma (#shift used for immunity)
  ;  let z (random-normal #mu #sigma)                      ;; this was the original formula I thought was correct
  ;  let x (exp (#mu + (#sigma * z)))                      ;; but only works if mean and stdev are of the normal dist
  report round ((exp random-normal #mu #sigma + #shift))   ;; this works if the mean and stdev are of the log-normal dist (comment as needed)
end

to-report normal-dist [#mu #sigma]
  ;; report value from a normal distribution with mean #mu and stdev #sigma
  let x 0
  let counter 100                                             ;; counter prevents infinite loops
  while [x <= 0 and counter > 0] [                            ;; ensures value is resampled until it's not negative
    set x round (random-normal #mu #sigma)
    set counter (counter - 1)
  ]
  ifelse counter > 0 [
    report x
  ] [ ;; else
    report #mu
  ]
end

to-report count_staying-at-home
  report count turtles with [staying-at-home?]
end
@#$#@#$#@
GRAPHICS-WINDOW
18
20
1026
1029
-1
-1
10.0
1
10
1
1
1
0
0
0
1
0
99
0
99
0
0
1
ticks
30.0

BUTTON
20
540
83
573
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
99
541
162
574
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
174
541
249
574
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
444
542
515
575
NIL
profile
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
1245
240
1390
273
visual-elements?
visual-elements?
1
1
-1000

SLIDER
610
45
782
78
min-radius
min-radius
0
100
2.0
1
1
NIL
HORIZONTAL

SLIDER
1245
101
1418
134
initial-infected
initial-infected
0
100
0.1
1
1
% of pop
HORIZONTAL

SLIDER
1245
60
1417
93
duration
duration
0
10
1.0
0.5
1
years
HORIZONTAL

SLIDER
610
170
782
203
p-infect-base
p-infect-base
0
100
10.0
1
1
%
HORIZONTAL

TEXTBOX
634
140
784
158
Pathogen parameters
11
0.0
1

TEXTBOX
1292
36
1442
54
Simulation options
11
0.0
1

TEXTBOX
634
25
784
43
Population parameters
11
0.0
1

TEXTBOX
945
20
1095
38
Control measures parameters
11
0.0
1

SLIDER
605
485
777
518
death-mean
death-mean
0
50
16.0
1
1
days
HORIZONTAL

SLIDER
605
525
777
558
death-stdev
death-stdev
0
50
8.21
1
1
days
HORIZONTAL

SLIDER
605
390
777
423
recovery-mean
recovery-mean
0
50
20.5
1
1
days
HORIZONTAL

SLIDER
605
430
777
463
recovery-stdev
recovery-stdev
0
50
6.7
1
1
days
HORIZONTAL

TEXTBOX
630
357
780
375
Countdown parameters
11
0.0
1

SLIDER
605
580
778
613
incubation-mean
incubation-mean
0
50
1.6
1
1
log-days
HORIZONTAL

SLIDER
605
620
779
653
incubation-stdev
incubation-stdev
0
50
1.4
1
1
log-days
HORIZONTAL

PLOT
17
586
292
749
daily contacts
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -955883 true "" ";plot num-contacts"

SWITCH
1245
345
1407
378
imposed-lockdown?
imposed-lockdown?
1
1
-1000

SLIDER
914
50
1112
83
lockdown-threshold
lockdown-threshold
0
100
4.0
1
1
% of pop is I
HORIZONTAL

SLIDER
914
90
1156
123
lockdown-compliance
lockdown-compliance
0
100
75.0
1
1
% compliance
HORIZONTAL

SWITCH
1245
384
1408
417
shield-vulnerable?
shield-vulnerable?
1
1
-1000

SLIDER
914
145
1111
178
shield-threshold
shield-threshold
0
100
3.0
1
1
% pop is I
HORIZONTAL

SLIDER
914
185
1136
218
shield-compliance
shield-compliance
0
100
50.0
1
1
% compliance
HORIZONTAL

SWITCH
1245
200
1389
233
lose-immunity?
lose-immunity?
1
1
-1000

SWITCH
1245
421
1417
454
personal-protections?
personal-protections?
1
1
-1000

SLIDER
915
280
1147
313
protections-strength
protections-strength
0
100
50.0
1
1
% reduction
HORIZONTAL

SLIDER
915
240
1157
273
protections-threshold
protections-threshold
0
100
1.0
1
1
% of pop is I
HORIZONTAL

SWITCH
1245
460
1408
493
test-and-trace?
test-and-trace?
1
1
-1000

SLIDER
915
370
1111
403
testtrace-threshold
testtrace-threshold
0
100
0.0
1
1
% of pop is I
HORIZONTAL

SLIDER
915
450
1112
483
test-coverage-sym
test-coverage-sym
0
100
100.0
1
1
% of I
HORIZONTAL

SLIDER
915
490
1114
523
test-coverage-asym
test-coverage-asym
0
100
100.0
1
1
% of pop
HORIZONTAL

SLIDER
915
530
1132
563
contacts-traced
contacts-traced
0
100
100.0
1
1
% of contacts
HORIZONTAL

SWITCH
1245
500
1427
533
isolation-symptomatics?
isolation-symptomatics?
1
1
-1000

SLIDER
915
710
1115
743
isolation-compliance-sym
isolation-compliance-sym
0
100
100.0
1
1
% of I
HORIZONTAL

SLIDER
915
670
1135
703
isolation-sym-threshold
isolation-sym-threshold
0
100
0.0
1
1
% of pop is I
HORIZONTAL

SLIDER
1245
140
1440
173
exogenous-infection
exogenous-infection
0
100
1.0
1
1
% prob/tick
HORIZONTAL

SWITCH
1245
280
1430
313
allow-exogenous-infections?
allow-exogenous-infections?
0
1
-1000

SLIDER
915
570
1155
603
isolation-compliance-tested
isolation-compliance-tested
0
100
100.0
1
1
% of tested
HORIZONTAL

SLIDER
610
215
785
248
min-immunity-duration
min-immunity-duration
0
365
180.0
1
1
days
HORIZONTAL

SLIDER
1230
635
1430
668
isolation-duration-case
isolation-duration-case
0
100
7.0
1
1
days
HORIZONTAL

SLIDER
916
320
1168
353
protections-compliance
protections-compliance
0
100
100.0
1
1
% compliance
HORIZONTAL

SLIDER
915
610
1155
643
isolation-compliance-traced
isolation-compliance-traced
0
100
100.0
1
1
% of contacts
HORIZONTAL

SLIDER
1230
675
1447
708
isolation-duration-contact
isolation-duration-contact
0
100
14.0
1
1
days
HORIZONTAL

SLIDER
610
85
805
118
daily-contacts
daily-contacts
0
100
50.0
10
1
% of neighbours
HORIZONTAL

SLIDER
915
410
1117
443
contact-history-length
contact-history-length
0
30
7.0
1
1
days
HORIZONTAL

SLIDER
610
255
787
288
asym-infectiousness
asym-infectiousness
0
100
30.0
1
1
%
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This model simulates the spread of COVID-19 in a population of varying size, as well as the effects of five non-pharmaceutical interventions that are commonly implemented as part of the effort to contain the pandemic. These are

- Imposed lockdown
- Shielding of vulnerable population
- Personal protections such as masks, hand-washing, and social distancing
- Test, trace, and isolate system
- Isolation of symptomatic cases

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

person-outline
false
0
Polygon -16777216 true false 105 90 105 195 75 285 105 315 150 300 150 225 150 300 195 315 225 285 195 195 195 90
Polygon -16777216 true false 195 75 255 150 225 195 150 105
Polygon -16777216 true false 105 75 45 150 75 195 150 105
Circle -16777216 true false 96 -9 108
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="no-controls" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count susceptibles</metric>
    <metric>count exposeds</metric>
    <metric>count symptomatics</metric>
    <metric>count asymptomatics</metric>
    <metric>count recovereds</metric>
    <metric>count_staying-at-home</metric>
    <metric>num-contacts</metric>
    <metric>lockdown-active?</metric>
    <metric>shielding-active?</metric>
    <metric>protections-active?</metric>
    <metric>get-age-bracket-data "0-18" "infected"</metric>
    <metric>get-age-bracket-data "19-39" "infected"</metric>
    <metric>get-age-bracket-data "40-49" "infected"</metric>
    <metric>get-age-bracket-data "50-59" "infected"</metric>
    <metric>get-age-bracket-data "60-69" "infected"</metric>
    <metric>get-age-bracket-data "70-79" "infected"</metric>
    <metric>get-age-bracket-data "80+" "infected"</metric>
    <metric>get-age-bracket-data "0-18" "deceased"</metric>
    <metric>get-age-bracket-data "19-39" "deceased"</metric>
    <metric>get-age-bracket-data "40-49" "deceased"</metric>
    <metric>get-age-bracket-data "50-59" "deceased"</metric>
    <metric>get-age-bracket-data "60-69" "deceased"</metric>
    <metric>get-age-bracket-data "70-79" "deceased"</metric>
    <metric>get-age-bracket-data "80+" "deceased"</metric>
    <enumeratedValueSet variable="duration">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-infected">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-exogenous-infections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exogenous-infection">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visual-elements?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-radius">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-contacts">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-infect-base">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asym-infectiousness">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lose-immunity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-immunity-duration">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-mean">
      <value value="20.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-stdev">
      <value value="6.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-mean">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-stdev">
      <value value="8.21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="incubation-mean">
      <value value="1.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="incubation-stdev">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imposed-lockdown?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lockdown-threshold">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lockdown-compliance">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shield-vulnerable?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shield-threshold">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shield-compliance">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="personal-protections?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protections-threshold">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protections-strength">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protections-compliance">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-and-trace?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testtrace-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contact-history-length">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-coverage-sym">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-coverage-asym">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contacts-traced">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-compliance-tested">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-compliance-traced">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-symptomatics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-sym-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-compliance-sym">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-duration-case">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-duration-contact">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-pxcor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-pycor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pxcor">
      <value value="99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pycor">
      <value value="99"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="vary-lockdown-compliance-0100" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count susceptibles</metric>
    <metric>count exposeds</metric>
    <metric>count symptomatics</metric>
    <metric>count asymptomatics</metric>
    <metric>count recovereds</metric>
    <metric>count_staying-at-home</metric>
    <metric>num-contacts</metric>
    <metric>lockdown-active?</metric>
    <metric>shielding-active?</metric>
    <metric>protections-active?</metric>
    <metric>get-age-bracket-data "0-18" "infected"</metric>
    <metric>get-age-bracket-data "19-39" "infected"</metric>
    <metric>get-age-bracket-data "40-49" "infected"</metric>
    <metric>get-age-bracket-data "50-59" "infected"</metric>
    <metric>get-age-bracket-data "60-69" "infected"</metric>
    <metric>get-age-bracket-data "70-79" "infected"</metric>
    <metric>get-age-bracket-data "80+" "infected"</metric>
    <metric>get-age-bracket-data "0-18" "deceased"</metric>
    <metric>get-age-bracket-data "19-39" "deceased"</metric>
    <metric>get-age-bracket-data "40-49" "deceased"</metric>
    <metric>get-age-bracket-data "50-59" "deceased"</metric>
    <metric>get-age-bracket-data "60-69" "deceased"</metric>
    <metric>get-age-bracket-data "70-79" "deceased"</metric>
    <metric>get-age-bracket-data "80+" "deceased"</metric>
    <enumeratedValueSet variable="duration">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-infected">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-exogenous-infections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exogenous-infection">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visual-elements?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-radius">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-contacts">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-infect-base">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asym-infectiousness">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lose-immunity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-immunity-duration">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-mean">
      <value value="20.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-stdev">
      <value value="6.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-mean">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-stdev">
      <value value="8.21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="incubation-mean">
      <value value="1.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="incubation-stdev">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imposed-lockdown?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lockdown-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lockdown-compliance">
      <value value="0"/>
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shield-vulnerable?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shield-threshold">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shield-compliance">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="personal-protections?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protections-threshold">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protections-strength">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protections-compliance">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-and-trace?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testtrace-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contact-history-length">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-coverage-sym">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-coverage-asym">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contacts-traced">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-compliance-tested">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-compliance-traced">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-symptomatics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-sym-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-compliance-sym">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-duration-case">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-duration-contact">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-pxcor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-pycor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pxcor">
      <value value="99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pycor">
      <value value="99"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="vary-shield-compliance-0100" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count susceptibles</metric>
    <metric>count exposeds</metric>
    <metric>count symptomatics</metric>
    <metric>count asymptomatics</metric>
    <metric>count recovereds</metric>
    <metric>count_staying-at-home</metric>
    <metric>num-contacts</metric>
    <metric>lockdown-active?</metric>
    <metric>shielding-active?</metric>
    <metric>protections-active?</metric>
    <metric>get-age-bracket-data "0-18" "infected"</metric>
    <metric>get-age-bracket-data "19-39" "infected"</metric>
    <metric>get-age-bracket-data "40-49" "infected"</metric>
    <metric>get-age-bracket-data "50-59" "infected"</metric>
    <metric>get-age-bracket-data "60-69" "infected"</metric>
    <metric>get-age-bracket-data "70-79" "infected"</metric>
    <metric>get-age-bracket-data "80+" "infected"</metric>
    <metric>get-age-bracket-data "0-18" "deceased"</metric>
    <metric>get-age-bracket-data "19-39" "deceased"</metric>
    <metric>get-age-bracket-data "40-49" "deceased"</metric>
    <metric>get-age-bracket-data "50-59" "deceased"</metric>
    <metric>get-age-bracket-data "60-69" "deceased"</metric>
    <metric>get-age-bracket-data "70-79" "deceased"</metric>
    <metric>get-age-bracket-data "80+" "deceased"</metric>
    <enumeratedValueSet variable="duration">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-infected">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-exogenous-infections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exogenous-infection">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visual-elements?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-radius">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-contacts">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-infect-base">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asym-infectiousness">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lose-immunity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-immunity-duration">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-mean">
      <value value="20.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-stdev">
      <value value="6.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-mean">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-stdev">
      <value value="8.21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="incubation-mean">
      <value value="1.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="incubation-stdev">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imposed-lockdown?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lockdown-threshold">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lockdown-compliance">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shield-vulnerable?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shield-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shield-compliance">
      <value value="0"/>
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="personal-protections?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protections-threshold">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protections-strength">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protections-compliance">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-and-trace?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testtrace-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contact-history-length">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-coverage-sym">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-coverage-asym">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contacts-traced">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-compliance-tested">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-compliance-traced">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-symptomatics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-sym-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-compliance-sym">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-duration-case">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-duration-contact">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-pxcor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-pycor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pxcor">
      <value value="99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pycor">
      <value value="99"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="vary-protections-strength-0100" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count susceptibles</metric>
    <metric>count exposeds</metric>
    <metric>count symptomatics</metric>
    <metric>count asymptomatics</metric>
    <metric>count recovereds</metric>
    <metric>count_staying-at-home</metric>
    <metric>num-contacts</metric>
    <metric>lockdown-active?</metric>
    <metric>shielding-active?</metric>
    <metric>protections-active?</metric>
    <metric>get-age-bracket-data "0-18" "infected"</metric>
    <metric>get-age-bracket-data "19-39" "infected"</metric>
    <metric>get-age-bracket-data "40-49" "infected"</metric>
    <metric>get-age-bracket-data "50-59" "infected"</metric>
    <metric>get-age-bracket-data "60-69" "infected"</metric>
    <metric>get-age-bracket-data "70-79" "infected"</metric>
    <metric>get-age-bracket-data "80+" "infected"</metric>
    <metric>get-age-bracket-data "0-18" "deceased"</metric>
    <metric>get-age-bracket-data "19-39" "deceased"</metric>
    <metric>get-age-bracket-data "40-49" "deceased"</metric>
    <metric>get-age-bracket-data "50-59" "deceased"</metric>
    <metric>get-age-bracket-data "60-69" "deceased"</metric>
    <metric>get-age-bracket-data "70-79" "deceased"</metric>
    <metric>get-age-bracket-data "80+" "deceased"</metric>
    <enumeratedValueSet variable="duration">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-infected">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-exogenous-infections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exogenous-infection">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visual-elements?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-radius">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-contacts">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-infect-base">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asym-infectiousness">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lose-immunity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-immunity-duration">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-mean">
      <value value="20.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-stdev">
      <value value="6.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-mean">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-stdev">
      <value value="8.21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="incubation-mean">
      <value value="1.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="incubation-stdev">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imposed-lockdown?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lockdown-threshold">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lockdown-compliance">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shield-vulnerable?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shield-threshold">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shield-compliance">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="personal-protections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protections-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protections-strength">
      <value value="0"/>
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protections-compliance">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-and-trace?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testtrace-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contact-history-length">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-coverage-sym">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-coverage-asym">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contacts-traced">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-compliance-tested">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-compliance-traced">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-symptomatics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-sym-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-compliance-sym">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-duration-case">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-duration-contact">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-pxcor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-pycor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pxcor">
      <value value="99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pycor">
      <value value="99"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="vary-protections-compliance-0100" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count susceptibles</metric>
    <metric>count exposeds</metric>
    <metric>count symptomatics</metric>
    <metric>count asymptomatics</metric>
    <metric>count recovereds</metric>
    <metric>count_staying-at-home</metric>
    <metric>num-contacts</metric>
    <metric>lockdown-active?</metric>
    <metric>shielding-active?</metric>
    <metric>protections-active?</metric>
    <metric>get-age-bracket-data "0-18" "infected"</metric>
    <metric>get-age-bracket-data "19-39" "infected"</metric>
    <metric>get-age-bracket-data "40-49" "infected"</metric>
    <metric>get-age-bracket-data "50-59" "infected"</metric>
    <metric>get-age-bracket-data "60-69" "infected"</metric>
    <metric>get-age-bracket-data "70-79" "infected"</metric>
    <metric>get-age-bracket-data "80+" "infected"</metric>
    <metric>get-age-bracket-data "0-18" "deceased"</metric>
    <metric>get-age-bracket-data "19-39" "deceased"</metric>
    <metric>get-age-bracket-data "40-49" "deceased"</metric>
    <metric>get-age-bracket-data "50-59" "deceased"</metric>
    <metric>get-age-bracket-data "60-69" "deceased"</metric>
    <metric>get-age-bracket-data "70-79" "deceased"</metric>
    <metric>get-age-bracket-data "80+" "deceased"</metric>
    <enumeratedValueSet variable="duration">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-infected">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-exogenous-infections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exogenous-infection">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visual-elements?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-radius">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-contacts">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-infect-base">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asym-infectiousness">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lose-immunity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-immunity-duration">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-mean">
      <value value="20.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-stdev">
      <value value="6.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-mean">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-stdev">
      <value value="8.21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="incubation-mean">
      <value value="1.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="incubation-stdev">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imposed-lockdown?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lockdown-threshold">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lockdown-compliance">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shield-vulnerable?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shield-threshold">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shield-compliance">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="personal-protections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protections-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protections-strength">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protections-compliance">
      <value value="0"/>
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-and-trace?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testtrace-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contact-history-length">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-coverage-sym">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-coverage-asym">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contacts-traced">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-compliance-tested">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-compliance-traced">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-symptomatics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-sym-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-compliance-sym">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-duration-case">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-duration-contact">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-pxcor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-pycor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pxcor">
      <value value="99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pycor">
      <value value="99"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="vary-test-coverage-combo-0100" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count susceptibles</metric>
    <metric>count exposeds</metric>
    <metric>count symptomatics</metric>
    <metric>count asymptomatics</metric>
    <metric>count recovereds</metric>
    <metric>count_staying-at-home</metric>
    <metric>num-contacts</metric>
    <metric>lockdown-active?</metric>
    <metric>shielding-active?</metric>
    <metric>protections-active?</metric>
    <metric>get-age-bracket-data "0-18" "infected"</metric>
    <metric>get-age-bracket-data "19-39" "infected"</metric>
    <metric>get-age-bracket-data "40-49" "infected"</metric>
    <metric>get-age-bracket-data "50-59" "infected"</metric>
    <metric>get-age-bracket-data "60-69" "infected"</metric>
    <metric>get-age-bracket-data "70-79" "infected"</metric>
    <metric>get-age-bracket-data "80+" "infected"</metric>
    <metric>get-age-bracket-data "0-18" "deceased"</metric>
    <metric>get-age-bracket-data "19-39" "deceased"</metric>
    <metric>get-age-bracket-data "40-49" "deceased"</metric>
    <metric>get-age-bracket-data "50-59" "deceased"</metric>
    <metric>get-age-bracket-data "60-69" "deceased"</metric>
    <metric>get-age-bracket-data "70-79" "deceased"</metric>
    <metric>get-age-bracket-data "80+" "deceased"</metric>
    <enumeratedValueSet variable="duration">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-infected">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-exogenous-infections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exogenous-infection">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visual-elements?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-radius">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-contacts">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-infect-base">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asym-infectiousness">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lose-immunity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-immunity-duration">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-mean">
      <value value="20.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-stdev">
      <value value="6.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-mean">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-stdev">
      <value value="8.21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="incubation-mean">
      <value value="1.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="incubation-stdev">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imposed-lockdown?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lockdown-threshold">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lockdown-compliance">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shield-vulnerable?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shield-threshold">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shield-compliance">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="personal-protections?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protections-threshold">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protections-strength">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protections-compliance">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-and-trace?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testtrace-threshold">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contact-history-length">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-coverage-sym">
      <value value="0"/>
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-coverage-asym">
      <value value="0"/>
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contacts-traced">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-compliance-tested">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-compliance-traced">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-symptomatics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-sym-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-compliance-sym">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-duration-case">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-duration-contact">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-pxcor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-pycor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pxcor">
      <value value="99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pycor">
      <value value="99"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
