extensions [profiler]

breed [susceptibles susceptible]    ;; can be infected (S)
breed [exposeds exposed]            ;; exposed but not infectious (E)
breed [symptomatics symptomatic]    ;; infectious and symptomatic (I)
breed [asymptomatics asymptomatic]  ;; infectious and asymptomatic (A)
breed [recovereds recovered]        ;; recovered and immune (R)

globals [
  pop-size                  ;; number of agents in simulation

  ;; lockdown globals
  lockdown-active?          ;; whether an imposed lockdown is in progress
  lockdown-threshold-num    ;; number of I agents to trigger lockdown

  ;; shielding globals
  agents-at-risk            ;; set of agents over 70
  shielding-active?         ;; whether shielding of vulnerable is in progress
  shield-threshold-num      ;; number of I agents to trigger shielding

  ;; personal protection globals
  protections-active?        ;; whether personal protections are in place
  protections-threshold-num  ;; number of I agents to trigger personal protections
  p-infect-adj              ;; p-infect after reduction of risk from protections

  ;; test and trace globals
  testtrace-threshold-num   ;; number of I agents to trigger test and trace

  ;; isolation of symptomatics globals
  start-isolation?          ;; whether isolation of symptomatics has begun
  isolation-threshold-num   ;; number of I agents to trigger isolation of symptomatics

  ;; reporters
  num-contacts              ;; number of contacts between agents for current tick

  count-inf-0-39            ;; cumulative counts of infecteds for each age range
  count-inf-40-49
  count-inf-50-59
  count-inf-60-69
  count-inf-70-79
  count-inf-80plus

  count-dead-0-39           ;; cumulative counts of dead agents for each age range
  count-dead-40-49
  count-dead-50-59
  count-dead-60-69
  count-dead-70-79
  count-dead-80plus
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
  iso-countdown             ;; individual isolation countdown
  counted?                  ;; whether the agent was already counted in daily contacts
]

susceptibles-own [
  p-infect                  ;; individual probability of catching the virus
  to-become-exposed?        ;; flag a S agent for exposure (E)
]

exposeds-own [
  inc-countdown             ;; individual incubation countdown
  to-become-asymptomatic?   ;; flag an E agent to become asymptomatic (A)
  contact-list              ;; list of neighbours the agent came in contact with since exposure
  tested?                   ;; whether the agent is aware of their infection status
  contacts-alerted?         ;; whether its contacts have been instructed to isolate
]

asymptomatics-own [
  will-develop-sym?         ;; whether the agent will develop symptoms (become I) or not (stay A)
  countdown                 ;; multi-purpose countdown: symptoms if will-develop-sym?, removal if not
  to-become-sym?            ;; flag an A agent to become symptomatic (I)
  to-recover?               ;; flag an A agent to recover (R)
  contact-list              ;; list of agents contacted since exposure
  tested?                   ;; whether the agent is aware of their infection status
  contacts-alerted?         ;; whether its contacts have been instructed to isolate
]

symptomatics-own [
  will-die?                 ;; whether the agent will die (become D) or not (become R)
  countdown                 ;; multi-purpose countdown: death if will-die, recovery if not
  to-die?                   ;; flag a I agent to die (D)
  to-recover?               ;; flag a I agent to recover (R)
  contact-list              ;; list of agents contacted since exposure
  tested?                   ;; whether the agent is aware of their infection status
  contacts-alerted?         ;; whether its contacts have been instructed to isolate
]

recovereds-own [
  imm-countdown             ;; individual immunity countdown
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
      set-age
      set p-death (actual-p-death age)
      set radius (pareto-dist min-radius 2)
      set staying-at-home? false
      set traced? false
      set asked-to-isolate? false
      set comply-with-isolation? false
      set iso-countdown -1
      set counted? false

      ;; setup S-specific attributes
      set-breed-susceptible
    ]
  ]

  ;; assign neigbours agentsets (can only be done once all agents have a radius)
  ask turtles [
    set neighbours (other turtles in-radius radius with [radius >= distance myself])
  ]

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
  set agents-at-risk (turtles with [age = "70-79" or age = "80+"])
  set shielding-active? false
  set shield-threshold-num (absolute-threshold shield-threshold)

  ;; personal protection globals
  set protections-active? false
  set protections-threshold-num (absolute-threshold protections-threshold)
  set p-infect-adj ((1 - (protections-strength / 100)) * (base-p-infect / 100))

  ;; test and trace globals
  set testtrace-threshold-num (absolute-threshold testtrace-threshold)
  set isolation-threshold-num (absolute-threshold isolation-threshold)
  set start-isolation? false

  ;; reporters

  ;; num-contacts is reset at every tick in count-contacts

  set count-inf-0-39 0
  set count-inf-40-49 0
  set count-inf-50-59 0
  set count-inf-60-69 0
  set count-inf-70-79 0
  set count-inf-80plus 0

  set count-dead-0-39 0
  set count-dead-40-49 0
  set count-dead-50-59 0
  set count-dead-60-69 0
  set count-dead-70-79 0
  set count-dead-80plus 0
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; GO ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  ifelse ticks < (duration * 365)
  [
    count-contacts                        ;; count the number of contacts between agents at each tick
    if test-and-trace? [record-contacts]  ;; record contacts of infected agents (E, A or I) and update their contact list
    expose-susceptibles                   ;; check whether non-isolating susceptibles become exposed to the virus
    progress-exposed                      ;; turn exposed agents that are almost the end of their incubation countdown into asymptomatic
    progress-asym                         ;; progress asymptomatic agents through their countdown (to either develop symptoms or recover)
    progress-sym                          ;; progress symptomatic agents through their countdown (to either die or recover)
    if lose-immunity? [lose-immunity]     ;; progress recovered agents through their immunity countdown (to return susceptible)
    update-breeds                         ;; change breeds of agents who moved onto a different stage
    modify-measures                       ;; update control measures based on the new number of active cases
    tick
  ] [
    stop
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; GO PROCEDURES ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to count-contacts
  ;; count the number of contacts between agents at each tick
  set num-contacts 0          ;; reset daily number of contacts

  ;; each alive, non-isolating agent is flagged as counted and the
  ;; number of non-counted, non isolating and alive neighbours they have
  ;; is added to the number of total contacts of the day
  ask turtles with [not staying-at-home?] [
    set counted? true
    let these-contacts (count neighbours with [not staying-at-home? and not counted?])
    set num-contacts (num-contacts + these-contacts)
  ]

  ;; reset counted flag for all agents
  ask turtles [set counted? false]
end

to record-contacts
  ;; record contacts of infected agents (E, A or I) and update their contact list
  if count symptomatics >= testtrace-threshold-num [
    ask (turtle-set exposeds asymptomatics symptomatics) [
      ;; only check for new contacts if contact-list doesn't already include all neighbours
      if length contact-list < count neighbours [
        let contacts [self] of neighbours with [not staying-at-home?]
        foreach contacts [
          contact ->
          ;; prevents duplicate contacts
          if not member? contact contact-list [
            set contact-list lput contact contact-list
          ]
        ]
      ]
    ]
  ]
end

to expose-susceptibles
  ;; check whether non-isolating susceptibles become exposed to the virus
  ask susceptibles with [not staying-at-home?] [
    let num-sym (count neighbours with [breed = symptomatics and not staying-at-home?])

    ;; adjust number of asymptomatics to account for their lower probability of transmission (currently 10%)
    let num-asym ((count neighbours with [breed = asymptomatics and not staying-at-home?]) * 0.1)
    ;; if the new number is between 0 and 1 (exclusive) set it to 1, as raising a number to a decimal lowers it
    if num-asym > 0 and num-asym < 1 [set num-asym 1]

    let total-inf (num-sym + num-asym)

    ;; probability of becoming exposed is only calculated if the total number of infecteds isn't 0
    if total-inf != 0 [
      let p-exposure (1 - ((1 - p-infect) ^ total-inf))
      let p (random-float 100)
      if p < (p-exposure * 100) [
        set to-become-exposed? true
      ]
    ]
  ]

  ;; if imported infections are allowed, there is a imported-infection probability that
  ;; one non-isolating susceptible will become exposed
  if allow-imported-infections? [
    let candidates (susceptibles with [not staying-at-home?])
    if any? candidates [
      let p (random-float 100)
      if p < imported-infection [
        ask one-of candidates [set-breed-exposed]
      ]
    ]
  ]
end

to progress-exposed
  ;; turn exposed agents that are almost the end of their incubation countdown into asymptomatic
  ask exposeds [
    ifelse inc-countdown <= (random 3 + 1)
    [set to-become-asymptomatic? true]
    [set inc-countdown (inc-countdown - 1)]
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
    ifelse imm-countdown <= 0 [
      set to-become-susceptible? true
    ] [ ;; else
      set imm-countdown (imm-countdown - 1)
    ]
  ]
end


to update-breeds
  ;; change breeds of agents who moved onto a different stage
  ask susceptibles with [to-become-exposed?] [
    set-breed-exposed
  ]

  ask exposeds with [to-become-asymptomatic?] [
    add-inf-count
    set-breed-asymptomatic
  ]

  ask asymptomatics with [to-become-sym?] [
    set-breed-symptomatic
  ]

  ask (turtle-set asymptomatics symptomatics) with [to-recover?] [
    set-breed-recovered
  ]

  ask symptomatics with [to-die?] [
    add-dead-count
    die
  ]

  if lose-immunity? [
    ask recovereds with [to-become-susceptible?] [
      set-breed-susceptible
    ]
  ]
end

to modify-measures
  ;; update control measures based on the new number of active cases
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
  ;; while active cases are below the threshold, p-infect returns to base-p-infect for all susceptible agents
  if personal-protections? [
    ifelse active-cases >= protections-threshold-num [
      if not protections-active? [start-protection]
    ] [ ;; else
      if protections-active? [end-protection]
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
  if isolate-symptomatics? and not start-isolation? [
    if active-cases >= isolation-threshold-num [
      set start-isolation? true
    ]
  ]

  ;;;;; ISOLATION (IS and TT)
  ;; this section carries out isolation for both test and trace and isolation of symptomatics
  ;; test and trace doesn't check threshold to ensure agents finish their isolation even if
  ;; active cases dip below it
  if test-and-trace? or start-isolation? [
    isolate
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; SUPPORTING PROCEDURES ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-age
  ;; sets an agent's age based on UK population data
  let p random-float 100
  if p < 50 [set age "0-39"]                     ;; 50% (UK census 2019)
  if p >= 50 and p < 63 [set age "40-49"]        ;; 13%
  if p >= 63 and p < 76 [set age "50-59"]        ;; 13%
  if p >= 76 and p < 87 [set age "60-69"]        ;; 11%
  if p >= 87 and p < 95 [set age "70-79"]        ;; 8%
  if p >= 95 [set age "80+"]                     ;; 5%
end

to check-outline
  ;; ensures agents retain their outline when changing breed
  if staying-at-home? [
    set shape "person-outline"
  ]
end

to set-breed-susceptible
  set breed susceptibles
  set p-infect (base-p-infect / 100)
  set to-become-exposed? false
  if visual-elements? [
    set color green
    check-outline
  ]
end

to set-breed-exposed
  set breed exposeds
  set inc-countdown (log-normal incubation-mean incubation-stdev 0)
  set to-become-asymptomatic? false
  set contact-list []
  set tested? false
  set contacts-alerted? false
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
  ifelse p < asym-prevalence [
    set will-develop-sym? false
    set countdown (normal-dist recovery-mean recovery-stdev) ;; recovery countdown
  ] [ ;; else
    set will-develop-sym? true
    set countdown (random 3 + 1)                             ;; symptoms countdown
  ]
end

to add-inf-count
  ;; keeps a running count of infections per age range
  if age = "0-39"  [set count-inf-0-39 (count-inf-0-39 + 1)]
  if age = "40-49" [set count-inf-40-49 (count-inf-40-49 + 1)]
  if age = "50-59" [set count-inf-50-59 (count-inf-50-59 + 1)]
  if age = "60-69" [set count-inf-60-69 (count-inf-60-69 + 1)]
  if age = "70-79" [set count-inf-70-79 (count-inf-70-79 + 1)]
  if age = "80+"   [set count-inf-80plus (count-inf-80plus + 1)]
end

to add-dead-count
  ;; keeps a running count of deaths per age range
  if age = "0-39"  [set count-dead-0-39 (count-dead-0-39 + 1)]
  if age = "40-49" [set count-dead-40-49 (count-dead-40-49 + 1)]
  if age = "50-59" [set count-dead-50-59 (count-dead-50-59 + 1)]
  if age = "60-69" [set count-dead-60-69 (count-dead-60-69 + 1)]
  if age = "70-79" [set count-dead-70-79 (count-dead-70-79 + 1)]
  if age = "80+"   [set count-dead-80plus (count-dead-80plus + 1)]
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
    set imm-countdown (log-normal 1 0.5 min-immunity-duration)
    ]
  set to-become-susceptible? false
  if visual-elements? [
    set color grey
    check-outline
  ]
end

to isolate-agent
  set staying-at-home? true
  if visual-elements? [set shape "person-outline"]
end

to release-agent
    set staying-at-home? false
    if visual-elements? [set shape "person"]
end

to start-lockdown
  ;; ask all agents to go into lockdown
  ;; and ensure only a percentage (lockdown-compliance) does so
  ask turtles [
    let p (random-float 100)
    if p < lockdown-compliance [
      isolate-agent
    ]
  ]
  set lockdown-active? true
end

to end-lockdown
  ;; ask all agents to exit lockdown
  ;; unless they are at risk and shielding is still active
  ask turtles [
    if not shielding-active? or not member? self agents-at-risk [
      release-agent
    ]
  ]
  set lockdown-active? false
end

to start-shielding
  ;; ask all agents at risk to start shielding
  ;; and ensure only a percentage (shield-compliance) does so
  ask agents-at-risk [
    let p (random-float 100)
    if p < shield-compliance [
      isolate-agent
    ]
  ]
  set shielding-active? true
end

to end-shielding
  ;; ask all agents at risk to  stop shielding
  ;; unless a lockdown is currently in progress
  if not lockdown-active? [
    ask agents-at-risk [
      release-agent
    ]
  ]
  set shielding-active? false
end

to start-protection
  ;; give all susceptible agents the adjusted p-infect
  ask susceptibles [
    set p-infect p-infect-adj
  ]
  set protections-active? true
end

to end-protection
  ;; give all susceptible agents the base p-infect
  ask susceptibles [
    set p-infect (base-p-infect / 100)
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
  ;; identify contacts of tested agents and flag them as traced
  ask (turtle-set exposeds asymptomatics symptomatics) [
    ;; flagging once contacts are alerted ensures each tested
    ;; agent attempts to reach its contacts only once
    if tested? and not contacts-alerted? [
      foreach contact-list [
        ;; if the contact is not dead, flag them as traced
        ;; with probability contacts-traced
        contact -> if contact != nobody [
          let p (random-float 100)
          if p < contacts-traced [
            ask contact [set traced? true]
          ]
        ]
      ]
      set contacts-alerted? true
    ]
  ]
end

to isolate
  ;; isolate tested agents and their traced contacts
  let agents-to-check nobody ;; agents for whom isolation has to progress

  if test-and-trace? [
    ;; add agents who have been tested
    let tested-agents (turtle-set exposeds asymptomatics symptomatics) with [tested?]
    ask-agents-to-isolate tested-agents
    set agents-to-check (turtle-set tested-agents with [comply-with-isolation?] agents-to-check)

    ;; add agents who have been traced as contacts (those reached always isolate)
    let traced-agents (turtles with [traced?])
    set agents-to-check (turtle-set agents-to-check traced-agents)
  ]

  if isolate-symptomatics? [
    ;; add agents who comply with the isolation of symptomatics
    ask-agents-to-isolate symptomatics
    set agents-to-check (symptomatics with [comply-with-isolation?])
  ]

  ;; add agents who have recovered but may still have an active isolation countdown
  ;; because they have recovered before the end of their isolation
  set agents-to-check (turtle-set recovereds with [iso-countdown >= 0] agents-to-check)

  ask agents-to-check [update-isolation-countdown]
end

to ask-agents-to-isolate [agents]
  ;; ask a set of agents to isolate either due to the test and trace or
  ;; isolation of symptomatic measures
  ask agents with [not asked-to-isolate?] [
    let p (random-float 100)

    ;; agents who tested positive might have a different chance of
    ;; complying with isolation, so each case has different probabilities
    ifelse tested?
    [
      if p < isolation-compliance-tested [
        set comply-with-isolation? true
      ]
    ] [ ;; else they are just symptomatics
      if p < isolation-compliance-sym [
        set comply-with-isolation? true
      ]
    ]
    set asked-to-isolate? true
  ]
end

to update-isolation-countdown
  ;; set or decrease the countdown, or release the agent at the end of it
  if iso-countdown = -1 [
    isolate-agent
    set iso-countdown isolation-duration
  ]

  ifelse iso-countdown = 0 [
    if not lockdown-active? [
      if not shielding-active? or not member? self agents-at-risk [
        release-agent
        set iso-countdown -1
        set traced? false
      ]
    ]
  ] [ ;; else
    set iso-countdown (iso-countdown - 1)
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; REPORTERS ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report actual-p-death [#age]
  ;; return adjusted probability of death based on age range
  let p 0
  if #age = "0-39"  [set p 0.2]   ;; based on worldometer 24/9/2020
  if #age = "40-49" [set p 0.4]
  if #age = "50-59" [set p 1.3]
  if #age = "60-69" [set p 3.6]
  if #age = "70-79" [set p 8]
  if #age = "80+"   [set p 14.8]
  report p
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
  while [x <= 0] [                            ;; ensures value is resampled until it's not negative
    set x round (random-normal #mu #sigma)
  ]
  report x
end

to-report count-locked
  report count turtles with [staying-at-home?]
end
@#$#@#$#@
GRAPHICS-WINDOW
18
20
526
529
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
49
0
49
0
0
1
ticks
30.0

BUTTON
19
540
82
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
0
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
3.0
0.5
1
years
HORIZONTAL

SLIDER
610
170
782
203
base-p-infect
base-p-infect
0
100
10.0
1
1
%
HORIZONTAL

SLIDER
610
210
782
243
asym-prevalence
asym-prevalence
0
100
60.0
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
430
777
463
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
470
777
503
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
335
777
368
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
375
777
408
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
302
780
320
Countdown parameters
11
0.0
1

SLIDER
605
525
778
558
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
565
779
598
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
% adherance
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
% adherance
HORIZONTAL

SWITCH
1245
200
1389
233
lose-immunity?
lose-immunity?
0
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
914
285
1146
318
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
914
245
1156
278
protections-threshold
protections-threshold
0
100
2.0
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
914
345
1110
378
testtrace-threshold
testtrace-threshold
0
100
1.0
1
1
% of pop is I
HORIZONTAL

SLIDER
914
385
1111
418
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
914
425
1113
458
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
914
465
1131
498
contacts-traced
contacts-traced
0
100
0.0
1
1
% of contacts
HORIZONTAL

SWITCH
1245
500
1410
533
isolate-symptomatics?
isolate-symptomatics?
1
1
-1000

SLIDER
914
605
1114
638
isolation-compliance-sym
isolation-compliance-sym
0
100
10.0
1
1
% of I
HORIZONTAL

SLIDER
914
565
1114
598
isolation-threshold
isolation-threshold
0
100
5.0
1
1
% of pop is I
HORIZONTAL

SLIDER
1245
140
1430
173
imported-infection
imported-infection
0
100
1.0
1
1
% prob/tick
HORIZONTAL

SWITCH
1244
281
1441
314
allow-imported-infections?
allow-imported-infections?
1
1
-1000

SLIDER
914
505
1114
538
isolation-compliance-tested
isolation-compliance-tested
0
100
80.0
1
1
% of I
HORIZONTAL

SLIDER
610
250
785
283
min-immunity-duration
min-immunity-duration
0
365
365.0
1
1
days
HORIZONTAL

SLIDER
914
665
1114
698
isolation-duration
isolation-duration
0
100
14.0
1
1
days
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

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
