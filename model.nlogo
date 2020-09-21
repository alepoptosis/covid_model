extensions [profiler]

breed [susceptibles susceptible]    ;; can be infected (S)
breed [exposeds exposed]            ;; exposed but not infectious (E)
breed [symptomatics symptomatic]    ;; infectious and symptomatic (I)
breed [asymptomatics asymptomatic]  ;; infectious and asymptomatic (A)
breed [recovereds recovered]        ;; recovered and immune (R)
breed [deads dead]                  ;; removed from population (D)

globals [
  pop-size                  ;; number of agents in simulation
  num-contacts              ;; number of contacts between agents for current tick
  currently-locked?         ;; whether an imposed lockdown is in progress
  lockdown-threshold-num    ;; number of I agents to trigger lockdown
  currently-shielding?      ;; whether shielding of vulnerable is in progress
  shield-threshold-num      ;; number of I agents to trigger shielding
  at-risk-agents            ;; set of agents at risk
  currently-protecting?     ;; whether personal protections are in place
  protection-threshold-num  ;; number of I agents to trigger personal protections
  p-infect-adj              ;; p-infect after reduction of risk from protections
  testtrace-threshold-num   ;; number of I agents to trigger test and trace
  isolation-threshold-num   ;; number of I agents to trigger isolation of symptomatics
  start-isolation?          ;; whether isolation of symptomatics has begun

  count-dead-0-29           ;; running total of dead agents with age 0-29
  count-dead-30-59          ;; running total of dead agents with age 30-59
  count-dead-60plus         ;; running total of dead agents with age 60+
  count-dead-at-risk        ;; running total of dead agents with underlying conditions

  count-infected-0-29       ;; running total of I and A with age 0-29
  count-infected-30-59      ;; running total of I and A with age 30-59
  count-infected-60+        ;; running total of I and A with age 60+
  count-infected-at-risk    ;; running total of I and A agents with underlying conditions
]

turtles-own [
  age                       ;; age range of the agent (0-29, 30-59, 60+)
  at-risk?                  ;; whether the agent has an aggravating condition
  radius                    ;; contact radius of the agent
  neighbours                ;; agentset of the agent's contact
  isolating?                ;; whether the agent is currently isolating
  counted?                  ;; whether the agent was already counted in daily contacts
  traced?                   ;; whether the agent was traced as a contact of a tested agent
  iso-countdown             ;; individual isolation countdown
]

susceptibles-own [
  p-infect                  ;; individual probability of catching the virus
  to-become-exposed?        ;; flags a S agent for exposure (E)
]

exposeds-own [
  inc-countdown             ;; individual incubation countdown
  to-become-asymptomatic?   ;; flags an E agent to become asymptomatic (A)
  contact-list              ;; list of agents contacted since exposure
  tested?                   ;; whether the agent is aware of their infection status
  contacts-alerted?         ;; whether its contacts have been instructed to isolate
]

asymptomatics-own [
  will-develop-sym?         ;; whether the agent will develop symptoms (become I) or not (stay A)
  countdown                 ;; multi-purpose countdown: symptoms if will-develop-sym?, removal if not - TEST
  to-become-sym?            ;; flags an A agent to become symptomatic (I)
  to-recover?               ;; flags an A agent to recover (R)
  contact-list              ;; list of agents contacted since exposure
  tested?                   ;; whether the agent is aware of their infection status
  contacts-alerted?         ;; whether its contacts have been instructed to isolate
]

symptomatics-own [
  will-die?                 ;; whether the agent will die (become D) or not (become R)
  countdown                 ;; multi-purpose countdown: death if will-die, recovery if not - TEST
  to-die?                   ;; flags a I agent to die (D)
  to-recover?               ;; flags a I agent to recover (R)
  contact-list              ;; list of agents contacted since exposure
  tested?                   ;; whether the agent is aware of their infection status
  contacts-alerted?         ;; whether its contacts have been instructed to isolate
  asked-to-isolate?         ;; whether the agent was already asked to isolate by any measure
]

recovereds-own [
  imm-countdown             ;; individual immunity countdown
  to-become-susceptible?    ;; flags a R agent to lose immunity (S)
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; PROFILER ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to profile
  setup                         ;; sets up the model
  profiler:start                ;; starts profiling
  repeat (duration * 365) [go]  ;; runs one instance of the simulation
  profiler:stop                 ;; stops profiling
  print profiler:report         ;; prints the results
  profiler:reset                ;; clears the data
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
    sprout-susceptibles 1 [         ;; places a susceptible on each patch
      ;;;;; setup turtle attributes
      set-age
      set-at-risk
      set radius (pareto-dist min-radius 2)
      set isolating? false
      set counted? false
      set traced? false
      set iso-countdown -1

      ;;;;; setup S-specific attributes
      set-breed-susceptible
    ]
  ]

  ;; assigns neigbours agentsets (can only be done after all agents have a radius)
  ask turtles [
    set neighbours (other turtles in-radius radius with [radius >= distance myself])
  ]

  set pop-size (count turtles)
  let to-infect round (initial-infected * pop-size / 100)
  ask n-of to-infect turtles [set-breed-exposed]
end

to setup-globals
  ;; pop-size is set in setup-turtles
  ;; num-contacts is reset at every tick in count-contacts

  ;; lockdown globals
  set currently-locked? false
  set lockdown-threshold-num (absolute-threshold lockdown-threshold)

  ;; shielding globals
  set currently-shielding? false
  set shield-threshold-num (absolute-threshold shield-threshold)
  set at-risk-agents (turtles with [age = "60+" or at-risk?])

  ;; personal protection globals
  set currently-protecting? false
  set protection-threshold-num (absolute-threshold protection-threshold)
  set p-infect-adj ((1 - (protection-strength / 100)) * (base-p-infect / 100))

  ;; test and trace globals
  set testtrace-threshold-num (absolute-threshold testtrace-threshold)
  set isolation-threshold-num (absolute-threshold isolation-threshold)
  set start-isolation? false

  ;; reporters
  set count-dead-0-29 0
  set count-dead-30-59 0
  set count-dead-60plus 0
  set count-dead-at-risk 0

  set count-infected-0-29 0
  set count-infected-30-59 0
  set count-infected-60+ 0
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; GO ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  ifelse ticks < (duration * 365)
  [
    count-contacts
    if test-and-trace? [record-contacts] ;;? should I check threshold here already?
    expose-susceptibles
    become-infectious
    progress-asym
    progress-sym
    if lose-immunity? [lose-immunity]
    update-breeds
    modify-measures
    tick
  ] [
    stop
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; GO PROCEDURES ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to count-contacts
  ;; counts the number of contacts between agents at each tick
  set num-contacts 0          ;; resets daily number of contacts

  ;; each alive, non-isolating agent is flagged as counted and the
  ;; number of non-counted, non isolating and alive neighbours they have
  ;; is added to the number of total contacts of the day
  ask turtles with [not isolating? and breed != deads] [
    set counted? true
    let these-contacts (count neighbours with [not isolating? and not counted? and breed != deads])
    set num-contacts (num-contacts + these-contacts)
  ]

  ;; resets counted flag for agents
  ask turtles [set counted? false]
end

to record-contacts ;;? wondering if this can be merged with count-contacts without getting too messy
  if count symptomatics >= testtrace-threshold-num [
    ask (turtle-set exposeds asymptomatics symptomatics) [
      if length contact-list < count neighbours [
        let contacts [self] of neighbours with [not isolating? and breed != deads]
        foreach contacts [
          contact ->
          if not member? contact contact-list [
            set contact-list lput contact contact-list
          ]
        ]
      ]
    ]
  ]
end

to expose-susceptibles
  ask susceptibles with [not isolating?] [
    let num-sym (count neighbours with [breed = symptomatics and not isolating?])

    let num-asym ((count neighbours with [breed = asymptomatics and not isolating?]) * 0.1)
    if num-asym != 0 and num-asym < 1 [set num-asym 1]

    let total-inf (num-sym + num-asym)

    if total-inf != 0 [
      let p-exposure (1 - ((1 - p-infect) ^ total-inf))
      let p (random-float 100)
      if p < (p-exposure * 100) [
        set to-become-exposed? true
      ]
    ]
  ]

  if allow-travel? [
    let travellers (susceptibles with [not isolating?])    ;; group all S agents not isolating
    if any? travellers [                                   ;; if the group is not empty
      let p (random-float 100)                             ;; there is a chance that one agent
      if p < travel-frequency [                            ;; from this group becomes exposed from travelling
        ask one-of travellers [set-breed-exposed]
      ]
    ]
  ]
end

to become-infectious
  ask exposeds [
    ifelse inc-countdown <= (random 3 + 1)
    [set to-become-asymptomatic? true]
    [set inc-countdown (inc-countdown - 1)]
  ]
end

to progress-asym
  ask asymptomatics [
    ifelse countdown <= 0
    [
      ifelse will-develop-sym? [
        set to-become-sym? true
      ] [
        set to-recover? true
      ]
    ] [ ;; else
      set countdown (countdown - 1)
      ]
    ]
end

to progress-sym
  ask symptomatics [
    ifelse countdown <= 0
    [
      ifelse will-die? [
        set to-die? true
      ] [
        set to-recover? true
      ]
    ] [ ;; else
      set countdown (countdown - 1)
    ]
  ]
end

to lose-immunity
  ask recovereds [
    ifelse imm-countdown <= 0 [
      set to-become-susceptible? true
    ] [ ;; else
      set imm-countdown (imm-countdown - 1)
    ]
  ]
end


to update-breeds
  ask susceptibles with [to-become-exposed?] [
    set-breed-exposed
  ]

  ask exposeds with [to-become-asymptomatic?] [
    set-breed-asymptomatic
  ]

  ask asymptomatics with [to-become-sym?] [
    set-breed-symptomatic
  ]

  ask (turtle-set asymptomatics symptomatics) with [to-recover?] [
    set-breed-recovered
  ]

  ask symptomatics with [to-die?] [ ;;? leave as is or add a dead-add-count to mirror inf-add-count?
    if age = "60+" [set count-dead-60plus (count-dead-60plus + 1)]
    if age = "30-59" [set count-dead-30-59 (count-dead-30-59 + 1)]
    if age = "0-29" [set count-dead-0-29 (count-dead-0-29 + 1)]
    if at-risk? [set count-dead-at-risk (count-dead-at-risk + 1)]
    die
  ]

  ask recovereds with [to-become-susceptible?] [
    set-breed-susceptible
  ]
end

to modify-measures

  let active-cases (count symptomatics)

  ;;;; IMPOSED LOCKDOWN
  if imposed-lockdown? [
    ifelse active-cases >= lockdown-threshold-num [
      if not currently-locked? [start-lockdown]
    ] [ ;; else
      if currently-locked? [end-lockdown]
    ]
  ]

  ;;;; SHIELD VULNERABLE
  if shield-vulnerable? [
    ifelse active-cases >= shield-threshold-num [
      if not currently-shielding? [start-shielding]
    ] [ ;; else
      if currently-shielding? [end-shielding]
    ]
  ]

  ;;;; PERSONAL PROTECTION
  if personal-protection? [
    ifelse active-cases >= protection-threshold-num [
      if not currently-protecting? [
        start-protection
      ]
    ] [ ;; else
      if currently-protecting? [
        end-protection
      ]
    ]
  ]

  ;;;; TEST AND TRACE
  if test-and-trace? [
    if active-cases >= testtrace-threshold-num [
      test
      trace
    ]
    isolate
  ]

  ;;;; ISOLATE SYMPTOMATICS
  if isolate-symptomatics? [
    if start-isolation? [isolate-symptomatics]
    if not start-isolation? and active-cases > isolation-threshold-num [
      set start-isolation? true
    ]
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; SUPPORTING PROCEDURES ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-age
  let p random-float 100
  if p <= 40 [set age "30-59"]                ;; 40% (30-59)
  if p > 40 and p <= 77 [set age "0-29"]      ;; 37% (0-29)
  if p > 77 [set age "60+"]                   ;; 23% (60+)
end

to set-at-risk
  let p random-float 100
  ifelse p < percentage-at-risk [
    set at-risk? true
  ] [ ;; else
    set at-risk? false
  ]
end

to set-breed-susceptible
  set breed susceptibles
  if visual-elements? [set color green]
  set p-infect (base-p-infect / 100)
  set to-become-exposed? false
  if visual-elements? [check-outline]
end

to set-breed-exposed
  set breed exposeds
  if visual-elements? [set color yellow]
  set inc-countdown (log-normal incubation-mean incubation-stdev)
  set to-become-asymptomatic? false
  set contact-list []
  set tested? false
  set contacts-alerted? false
  if visual-elements? [check-outline]
end

to set-breed-asymptomatic
  set breed asymptomatics
  if visual-elements? [set color violet]
  check-symptoms ;; sets will-develop-sym? and countdown
  add-inf-count
  set to-become-sym? false
  set to-recover? false
  ;; contact-list carries over from exposeds
  ;; tested? carries over from exposeds
  if visual-elements? [check-outline]
end

to check-symptoms
  let p random-float 100
  ifelse p < asym-prevalence [
    set will-develop-sym? false
    set countdown (normal-dist recovery-mean recovery-stdev) ;; recovery countdown
  ] [ ;; else
    set will-develop-sym? true
    set countdown (random 3 + 1) ;; symptoms countdown
  ]
end

to add-inf-count
  ;; keeps a running count of infections per age range
  if age = "0-29" [set count-infected-0-29 (count-infected-0-29 + 1)]
  if age = "30-59" [set count-infected-30-59 (count-infected-30-59 + 1)]
  if age = "60+" [set count-infected-60+ (count-infected-60+ + 1)]
  if at-risk? [set count-infected-at-risk (count-infected-at-risk + 1)]
end

to set-breed-symptomatic
  set breed symptomatics
  if visual-elements? [set color red]
  check-death
  set to-die? false
  set to-recover? false
  set asked-to-isolate? false
  ;; contact-list carries over from asymptomatic
  ;; tested? carries over from asymptomatic
  if visual-elements? [check-outline]
end

to check-death
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
  if visual-elements? [set color grey]
  if lose-immunity? [set imm-countdown 365]
  set to-become-susceptible? false
  if visual-elements? [check-outline]
end

to check-outline
  if isolating? [
    set shape "person-outline"
  ]
end

to set-breed-dead
  set breed deads
  if visual-elements? [set color black]
end

to isolate-agent
  set isolating? true
  if visual-elements? [set shape "person-outline"]
end

to release-agent
    set isolating? false
    if visual-elements? [set shape "person"]
end

to start-lockdown
  ask turtles with [breed != deads] [
    let p (random-float 100)
    if p < lockdown-strictness [
      isolate-agent
    ]
  ]
  set currently-locked? true
end

to end-lockdown ;; this can be made better
  ask turtles [
    ifelse currently-shielding? [
      if not member? self at-risk-agents [
        release-agent
      ]
    ] [ ;; else
      release-agent
    ]
  ]
  set currently-locked? false
end

to start-shielding
  ask at-risk-agents [
    let p (random-float 100)
    if p < shield-adherance [
      isolate-agent
    ]
  ]
  set currently-shielding? true
end

to end-shielding
  if not currently-locked? [
    ask at-risk-agents [
      release-agent
    ]
  ]
  set currently-shielding? false
end

to start-protection
  ask susceptibles [
    set p-infect p-infect-adj
  ]
  set currently-protecting? true
end

to end-protection
  ask susceptibles [
    set p-infect (base-p-infect / 100)
  ]
  set currently-protecting? false
end

to test
  ask (turtle-set exposeds asymptomatics) with [not tested?] [
    let p (random-float 100)
    if p < asym-test-coverage [
      set tested? true
    ]
  ]

  ask symptomatics with [not tested?] [
    let p (random-float 100)
    if p < sym-test-coverage [
      set tested? true
    ]
  ]
end

to trace
  ask (turtle-set exposeds asymptomatics symptomatics) [
    if tested? and not contacts-alerted? [
      foreach contact-list [
        ;; if the contact is not dead, flag them as traced
        contact -> if contact != nobody [
          let p (random-float 100)
          if p < contacts-reached [
            ask contact [set traced? true]
          ]
        ]
      ]
      set contacts-alerted? true
    ]
  ]
end

to isolate
  ifelse isolate-symptomatics? [
    ask (turtle-set exposeds asymptomatics) with [tested?] [
      check-isolation
    ]
  ] [ ;; else
    ask (turtle-set exposeds asymptomatics symptomatics) with [tested?] [
      check-isolation
    ]
  ]

  ask turtles with [traced?] [
    check-isolation
  ]
end

to check-isolation
  if iso-countdown = -1 [
    isolate-agent
    set iso-countdown 14
  ]

  ifelse iso-countdown = 0 [
    ;; ADD IF NO LOCKDOWN/SHIELDING IS IN PROGRESS HERE
    release-agent
    set iso-countdown -1
    set traced? false
  ] [ ;; else
    set iso-countdown (iso-countdown - 1)
  ]
end

to isolate-symptomatics
  ask symptomatics with [not asked-to-isolate?] [
    let p (random-float 100)
    if p < isolation-strictness [
      check-isolation
    ]
    set asked-to-isolate? true
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; REPORTERS ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report absolute-threshold [#per]
  report round (#per * pop-size / 100)
end

to-report pareto-dist [#min #alpha]                   ;; reports value from a pareto distribution with minimum #min and shape #alpha
  let x (random 100 + 1)
  let num (#alpha * (#min ^ #alpha))
  let den (x ^ (#alpha + 1))
  report round ((num / den) + #min)
;  let y round (num / den)                            ;; version with true minimum instead of plus minimum
;  ifelse y < #min                                    ;; as the version above always sums the minimum to all results
;  [report #min]                                      ;; while this simply reports the minimum if the results falls under it
;  [report y]
end

to-report log-normal [#mu #sigma]                     ;; reports value from a log-normal distribution with mean #mu and stdev #sigma
  ;  let z (random-normal #mu #sigma)                 ;; this was the original formula I thought was correct
  ;  let x (exp (#mu + (#sigma * z)))                 ;; but only works if mean and stdev are of the normal dist
  report round (exp random-normal #mu #sigma)         ;; this works if the mean and stdev are of the log-normal dist (comment as needed)
end

to-report normal-dist [#mu #sigma]                    ;; reports value from a normal distribution with mean #mu and stdev #sigma
  let x round (random-normal #mu #sigma)              ;; draw a value x from the normal distribution
  let min_days (precision (#mu - #sigma) 0)           ;; let the minimum number be mean - stdev
  ifelse x >= min_days                                ;; if the resulting value is equal to or above the minimum
  [report x]                                          ;; then it can be reported as is
  [                                                   ;; otherwise, if it's below the minimum
    ifelse min_days > 0                               ;; and the minimum is above 0 (i.e. valid)
    [report min_days]                                 ;; the value reported is the minimum
    [report 1]                                        ;; otherwise, if the minimum happens to be negative, 1 is reported
  ]
end

to-report count-locked
  report count turtles with [isolating?]
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
1267
179
1412
212
visual-elements?
visual-elements?
0
1
-1000

SLIDER
601
31
773
64
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
1250
81
1423
114
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
1250
40
1422
73
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
896
36
1068
69
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
895
125
1067
158
asym-prevalence
asym-prevalence
0
100
60.0
1
1
%
HORIZONTAL

SLIDER
894
83
1066
116
p-death
p-death
0
100
2.5
1
1
%
HORIZONTAL

TEXTBOX
920
14
1070
32
Pathogen parameters
11
0.0
1

TEXTBOX
1297
16
1447
34
Simulation options
11
0.0
1

TEXTBOX
626
11
776
29
Population parameters
11
0.0
1

TEXTBOX
617
133
767
151
Control measures parameters
11
0.0
1

SLIDER
602
72
774
105
percentage-at-risk
percentage-at-risk
0
100
29.0
1
1
%
HORIZONTAL

SLIDER
888
405
1060
438
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
888
445
1060
478
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
889
313
1061
346
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
889
351
1061
384
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
912
279
1062
297
Countdown parameters
11
0.0
1

SLIDER
888
493
1061
526
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
888
532
1062
565
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
1256
279
1418
312
imposed-lockdown?
imposed-lockdown?
1
1
-1000

SLIDER
590
166
788
199
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
590
209
788
242
lockdown-strictness
lockdown-strictness
0
100
75.0
1
1
% adherance
HORIZONTAL

SWITCH
1256
318
1419
351
shield-vulnerable?
shield-vulnerable?
1
1
-1000

SLIDER
590
267
787
300
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
590
306
787
339
shield-adherance
shield-adherance
0
100
50.0
1
1
% adherance
HORIZONTAL

SWITCH
1267
139
1411
172
lose-immunity?
lose-immunity?
0
1
-1000

SWITCH
1256
355
1420
388
personal-protection?
personal-protection?
1
1
-1000

SLIDER
590
398
786
431
protection-strength
protection-strength
0
100
50.0
1
1
% reduction
HORIZONTAL

SLIDER
590
359
786
392
protection-threshold
protection-threshold
0
100
2.0
1
1
% of pop is I
HORIZONTAL

SWITCH
1256
394
1419
427
test-and-trace?
test-and-trace?
1
1
-1000

SLIDER
590
452
786
485
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
589
492
786
525
sym-test-coverage
sym-test-coverage
0
100
100.0
1
1
% of I
HORIZONTAL

SLIDER
588
532
787
565
asym-test-coverage
asym-test-coverage
0
100
100.0
1
1
% of pop
HORIZONTAL

SLIDER
588
571
787
604
contacts-reached
contacts-reached
0
100
50.0
1
1
% of contacts
HORIZONTAL

SWITCH
1255
436
1420
469
isolate-symptomatics?
isolate-symptomatics?
0
1
-1000

SLIDER
586
668
786
701
isolation-strictness
isolation-strictness
0
100
100.0
1
1
% of I
HORIZONTAL

SLIDER
586
629
786
662
isolation-threshold
isolation-threshold
0
100
0.0
1
1
% of pop is I
HORIZONTAL

SLIDER
586
719
784
752
travel-frequency
travel-frequency
0
100
1.0
1
1
% ticks
HORIZONTAL

SWITCH
1266
220
1412
253
allow-travel?
allow-travel?
0
1
-1000

TEXTBOX
889
586
1039
698
STILL MISSING:\n\nimmunity-mean and iso-countdown-max/mean-iso reduction (need to figure out what distribution to use or if poisson was ok)\n\n
11
0.0
1

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
0
@#$#@#$#@
