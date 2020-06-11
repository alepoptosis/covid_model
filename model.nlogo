globals [
  SS-contacts
  SE-contacts
  SI-contacts
  SR-contacts
  SA-contacts
  EE-contacts
  EI-contacts
  ER-contacts
  EA-contacts
  II-contacts
  IR-contacts
  IA-contacts
  RR-contacts
  RA-contacts
  AA-contacts

  first-lockdown?
  currently-locked?
  num-contacts
]

breed [susceptibles susceptible]    ;; can be infected (S)
breed [latents latent]              ;; infectious but pre-symptomatic (L)
breed [symptomatics symptomatic]    ;; infectious and symptomatic (I)
breed [asymptomatics asymptomatic]  ;; infectious and asymptomatic (A)
breed [recovereds recovered]        ;; recovered and immune (R)
breed [deads dead]                  ;; removed from population (D)

turtles-own [
  z-contact-init            ;; base radius of contact neighbourhood
  z-contact                 ;; individual radius of contact neighbourhood
  age                       ;; age range of the person (0-29, 30-59, 60+)
]

susceptibles-own [
  to-become-latent?         ;; flags a S for exposure
  p-infect                  ;; individual transmission probability
]

latents-own [
  to-become-infected?       ;; flags a L for beginning of infection
  inc-countdown             ;; individual incubation countdown
]

symptomatics-own [
  will-die?                 ;; whether the infected will die or recover
  to-remove?                ;; flags an I for removal (recovery or death)
  rec-countdown             ;; individual recovery countdown
  death-countdown           ;; individual death countdown
  iso-countdown             ;; idnividual isolation countdown
]

asymptomatics-own [
  to-remove?                ;; flags an A for recovery
  rec-countdown             ;; individual recovery countdown
]

recovereds-own [
  to-become-susceptible?    ;; flags a R for loss of immunity
  imm-countdown             ;; individual loss of immunity countdown
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  setup-globals
  setup-turtles
  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; SETUP PROCEDURES ;;;;;;;;;;;;;;;;;

to setup-globals
  set SS-contacts 0
  set SE-contacts 0
  set SI-contacts 0
  set SR-contacts 0
  set SA-contacts 0
  set EE-contacts 0
  set EI-contacts 0
  set ER-contacts 0
  set EA-contacts 0
  set II-contacts 0
  set IR-contacts 0
  set IA-contacts 0
  set RR-contacts 0
  set RA-contacts 0
  set AA-contacts 0

  set first-lockdown? false
  set currently-locked? false
end

to setup-turtles
  set-default-shape turtles "person"

  ;; creates a susceptible on each patch and initialises first variables
  ask patches [
    set pcolor white
    sprout-susceptibles 1 [
      set breed susceptibles
      set color green
      set to-become-latent? false
      set p-infect p-infect-init / 100
      set z-contact-init (pareto-dist z-contact-min 2)
      set z-contact z-contact-init
      set-age
    ]
  ]
  ;; randomly infects initial-inf susceptibles
  ask turtles-on (n-of initial-inf patches) [set-breed-symptomatic]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; GO ;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  ifelse ticks < (duration * 365)
  ;  and (count symptomatics + count latents) > 0  ;; uncomment to stop simulation when virus stops circulating
  [
    count-contacts            ;; updates the number of contacts made
    expose-susceptibles       ;; turns S into L if they had contact with an I, A or L based on p-infect, and checks if they have travelled
    infect-latents            ;; turns L into I after inc-countdown ticks
    remove-infecteds          ;; turns I into R after rec-countdown ticks or D after death-countdown ticks
    lose-immunity             ;; turns R back into S after imm-countdown ticks
    update-breeds             ;; updates breeds as necessary
    modify-lockdown           ;; modifies the lockdown depending on the new number of S, and isolates S for iso-countdown ticks
    tick                      ;; goes to next day
  ] [
    stop
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; GO PROCEDURES ;;;;;;;;;;;;;;;;;;;

to count-contacts
  let SS-tick 0
  let SE-tick 0
  let SI-tick 0
  let SR-tick 0
  let SA-tick 0
  let EE-tick 0
  let EI-tick 0
  let ER-tick 0
  let EA-tick 0
  let II-tick 0
  let IR-tick 0
  let IA-tick 0
  let RR-tick 0
  let RA-tick 0
  let AA-tick 0

  ask susceptibles [
    set SS-tick (SS-tick + ((count other susceptibles in-radius z-contact with [z-contact >= distance myself])))
    set SE-tick (SE-tick + ((count latents in-radius z-contact)))
    set SI-tick (SI-tick + ((count symptomatics in-radius z-contact)))
    set SR-tick (SR-tick + ((count recovereds in-radius z-contact)))
    set SA-tick (SA-tick + ((count asymptomatics in-radius z-contact)))

  ]
  set SS-tick (SS-tick / 2)

  ask latents [
    set EE-tick (EE-tick + ((count other latents in-radius z-contact with [z-contact >= distance myself])))
    set EI-tick (EI-tick + ((count symptomatics in-radius z-contact)))
    set ER-tick (ER-tick + ((count recovereds in-radius z-contact)))
    set EA-tick (EA-tick + ((count asymptomatics in-radius z-contact)))
  ]
  set EE-tick (EE-tick / 2)

  ask symptomatics [
    set II-tick (II-tick + (count other symptomatics in-radius z-contact with [z-contact >= distance myself]))
    set IR-tick (IR-tick + (count recovereds in-radius z-contact))
    set IA-tick (IA-tick + (count asymptomatics in-radius z-contact))
  ]
  set II-tick (II-tick / 2)

  ask recovereds [
    set RR-tick (RR-tick + (count other recovereds in-radius z-contact with [z-contact >= distance myself]))
    set RA-tick (RA-tick + (count asymptomatics in-radius z-contact))
  ]
  set RR-tick (RR-tick / 2)

  ask asymptomatics [
    set AA-tick (AA-tick + (count other asymptomatics in-radius z-contact with [z-contact >= distance myself]))
  ]
  set AA-tick (AA-tick / 2)

  set num-contacts (
    SS-tick + SE-tick + SI-tick + SR-tick + SA-tick +
    EE-tick + EI-tick + ER-tick + EA-tick +
    II-tick + IR-tick + IA-tick +
    RR-tick + RA-tick +
    AA-tick
  )

  set SS-contacts (SS-contacts + SS-tick)
  set SE-contacts (SE-contacts + SE-tick)
  set SI-contacts (SI-contacts + SI-tick)
  set SR-contacts (SR-contacts + SR-tick)
  set SA-contacts (SA-contacts + SA-tick)
  set EE-contacts (EE-contacts + EE-tick)
  set EI-contacts (EI-contacts + EI-tick)
  set ER-contacts (ER-contacts + ER-tick)
  set EA-contacts (EA-contacts + EA-tick)
  set II-contacts (II-contacts + II-tick)
  set IR-contacts (IR-contacts + IR-tick)
  set IA-contacts (IA-contacts + IA-tick)
  set RR-contacts (RR-contacts + RR-tick)
  set RA-contacts (RA-contacts + RA-tick)
  set AA-contacts (AA-contacts + AA-tick)
end

to expose-susceptibles
  ask susceptibles [

    ;; the number of infected contacts is the number of S + L in z-contact radius who are not isolating
    let infected-contacts (
      (count symptomatics in-radius z-contact with [z-contact >= distance myself])
      + (count latents in-radius z-contact with [z-contact >= distance myself])
    )

    ;; A are counted separately to account for their lower probability of transmission (currently 10%)
    ;; if the new number is between 0 and 1 it is set to 1, as raising a number to a decimal lowers it
    let infected-asymptomatics (count asymptomatics in-radius z-contact with [z-contact >= distance myself]) * 0.1
    if infected-asymptomatics < 1 and infected-asymptomatics != 0
    [set infected-asymptomatics 1]

    ;; total number of infecteds after lowered impact of A
    let total-infecteds (infected-contacts + infected-asymptomatics)

    ;; if the option is on and the first lockdown has happened,
    ;; or, if lockdowns are not happening, the number of infecteds is past the threshold
    ;; lower probability of transmissions through measures such as the use of masks, 2 metre distancing, etc.
    if modify-p-infect? and (first-lockdown? or (count symptomatics) > lockdown-threshold) [
      set p-infect (1 - (protection-strength / 100)) * (p-infect-init / 100)
    ]

    ;; the probability of at least one contact causing infection is 1 - the probability that none do
    let infection-prob 1 - ((1 - p-infect) ^ total-infecteds)

    ;; if the S fails the check, it is flagged to become L
    let p (random 100 + 1)
    if p <= (infection-prob * 100) [set to-become-latent? true]
  ]

  ;; if the system is open, there is a chance for a S to become L even if their contacts are S
  if not closed-system? [check-travel]
end

to infect-latents    ;; infects L that have reached the end of their incubation countdown
  ask latents [
    ifelse inc-countdown = 0
    [set to-become-infected? true]
    [set inc-countdown (inc-countdown - 1)]
  ]
end

to remove-infecteds      ;; removes infecteds that have reached the end of their countdown
  ask symptomatics [     ;; for symptomatics (I), this is either the death or recovery countdown
    ifelse will-die?
    [
      ifelse death-countdown = 0
      [set to-remove? true]
      [set death-countdown (death-countdown - 1)]
    ]
    [
      ifelse rec-countdown = 0
      [set to-remove? true]
      [set rec-countdown (rec-countdown - 1)]
    ]
  ]

  ask asymptomatics [    ;; for asymptomatics (A), it can only be the recovery countdown
    ifelse rec-countdown = 0
    [set to-remove? true]
    [set rec-countdown (rec-countdown - 1)]
  ]
end

to lose-immunity    ;; makes susceptible the R that have reached the end of their immunity period
  ask recovereds [
    ifelse imm-countdown = 0
    [set to-become-susceptible? true]
    [set imm-countdown (imm-countdown - 1)]
  ]
end

to update-breeds

  ask susceptibles with [to-become-latent? = true] [set-breed-latent]

  ask latents with [to-become-infected? = true] [
    let p (random 100 + 1)
    ifelse p <= asym-infections    ;; decides whether the infection will be symptomatic or asymptomatic
    [set-breed-asymptomatic]
    [set-breed-symptomatic]
  ]

  ask symptomatics with [to-remove? = true] [
    ifelse will-die?               ;; decides whether the symptomatic infected will die or recover
    [set-breed-dead]
    [set-breed-recovered]
  ]

  ask asymptomatics with [to-remove? = true] [set-breed-recovered]

  ask recovereds with [to-become-susceptible? = true] [set-breed-susceptible]
end

to modify-lockdown
  if imposed-lockdown? [
    ifelse (count symptomatics) > lockdown-threshold
    [start-lockdown]
    [end-lockdown]
  ]

  ;; if a lockdown has already occurred and the option is on, isolates S with probability isolation-strictness
  if isolate-symptomatics? and first-lockdown? [isolate-symptomatics]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; SUPPORTING PROCEDURES ;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;; SETUP SUPPORT ;;;;;;;;;;;;;;;;;;;;;

to set-age
  let p (random 100) + 1
  if p <= 40 [set age "30-59"]                ;; 40% (30-59)
  if p > 40 and p <= 77 [set age "0-29"]      ;; 37% (0-29)
  if p > 77 [set age "60+"]                   ;; 23% (60+)
end

;;;;;;;;;;;;;;;;;;; GO SUPPORT ;;;;;;;;;;;;;;;;;;;;;;

to check-travel
  if (count symptomatics) < lockdown-threshold [              ;; if people can travel and lockdown is not active
    let p (random 100 + 1)                                    ;; one person gets randomly infected per tick depending on travel strictness
    if p >= (travel-strictness) [                             ;; 1% chance if 100% strictness, 100% chance if 0% strictness
      ask susceptibles-on (n-of 1 patches) [set-breed-latent]
    ]
  ]
end

;; change a turtle's breed and set associated variables

to set-breed-susceptible
  set breed susceptibles
  set color green
  set to-become-latent? false
  set p-infect p-infect-init / 100
  check-outline
end

to set-breed-latent
  set breed latents
  set color yellow
  set to-become-infected? false
  set inc-countdown (log-normal incubation-mean incubation-stdev)
  check-outline
end

to set-breed-asymptomatic
  set breed asymptomatics
  set color violet
  set to-remove? false
  set rec-countdown (normal-dist recovery-mean recovery-stdev)
  check-outline
end

to set-breed-symptomatic    ;; also used in setup-turtles
  set breed symptomatics
  set color red
  set to-remove? false
  set iso-countdown (rev-poisson iso-countdown-max mean-iso-reduction)
  check-death
  check-outline
end

to set-breed-recovered
  set breed recovereds
  set color 8
  set to-become-susceptible? false
  set imm-countdown (poisson-dist immunity-mean)
  check-outline
end

to set-breed-dead
  set breed deads
  set color black
end

to start-lockdown                                         ;; triggers possibility of self-isolation for non-dead agents
  let alives turtles with [not member? self deads]        ;; groups non-dead turtles
  if not currently-locked? [                              ;; if the lockdown was not on in the previous tick
    ask alives [
      let p (random 100 + 1)                              ;; checks whether the agent will isolate
      if p <= (lockdown-strictness) [isolate]             ;; if yes, z-contact is set to 0
      set currently-locked? true                          ;; lockdown is flagged as currently happening
      set first-lockdown? true                            ;; and the first lockdown is flagged as occurred
    ]                                                     ;; otherwise, the turtle maintains z-contact-init
  ]
end

to end-lockdown                                           ;; end lockdown by returning all alive turtles to initial z-contact
  let alives turtles with [not member? self deads]        ;; groups non-dead turtles
  if currently-locked? [                                  ;; if lockdown is currently happening
    ask alives [not-isolate]                              ;; set z-contact to z-contact init for all alive turtles
    set currently-locked? false                           ;; and flag lockdown as not currently happening
  ]
end

to isolate-symptomatics
  ask symptomatics [                                      ;; for each symptomatic
    ifelse iso-countdown <= 0                             ;; if their isolation period is over
    [
      ifelse currently-locked?                            ;; but the lockdown is still on
      [isolate]                                           ;; keep isolating
      [not-isolate]                                       ;; otherwise, return to initial z-contact
    ]                                                     ;; (note: not sure if this could be done simply as "if not currently-locked? [not-isolate]")
    [                                                     ;; if the isolation period is not over
      isolate                                             ;; keep isolating
      set iso-countdown (iso-countdown - 1)               ;; and lower countdown by 1 day
    ]
  ]
end

to check-outline    ;; ensures turtles maintain correct shape when changing breed
  ifelse z-contact = 0
  [set shape "person-outline"]
  [set shape "person"]
end

to check-death                          ;; checks whether an infected will die or recover and assigns correct countdown
  let p (random 100 + 1)
  let p-death-here (actual-p-death age)
  ifelse (p <= p-death-here)
  [set will-die? true]                  ;; if agent fails the check, it's flagged as will-die
  [set will-die? false]
  ifelse will-die?                      ;; those that will die receive a death countdown, others receive a recovery one
  [set death-countdown
    (normal-dist death-mean death-stdev)]
  [set rec-countdown
    (normal-dist recovery-mean recovery-stdev)]
end

to isolate        ;; sets z-contact and shape for self-isolation
  set z-contact 0
  set shape "person-outline"
end

to not-isolate    ;; returns turtle to default z-contact and shape
  set z-contact z-contact-init
  set shape "person"
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; REPORTERS ;;;;;;;;;;;;;;;;;;;;;

to-report log-normal [#mu #sigma]                     ;; reports value from a log-normal distribution with mean #mu and stdev #sigma
  ;  let z (random-normal #mu #sigma)                 ;; this was the original formula I thought was correct
  ;  let x (exp (#mu + (#sigma * z)))                 ;; but only works if mean and stdev are of the normal dist
  report round (exp random-normal #mu #sigma)         ;; this works if the mean and stdev are of the log-normal dist (comment as needed)
end

to-report normal-dist [#mu #sigma]                    ;; reports value from a normal distribution with mean #mu and stdev #sigma
  let x round (random-normal #mu #sigma)              ;; draw a value x from the normal distribution
  let min_days (precision (#mu - #sigma) 0)           ;; let the minimum number be mean - stdev
  ifelse x > min_days                                 ;; if the resulting value is above the minimum
  [report round x]                                    ;; then it can be reported
  [                                                   ;; otherwise, if it's below the minimum
    ifelse min_days > 0                               ;; and the minimum is positive (i.e. valid)
    [report min_days]                                 ;; the value reported is the minimum
    [report 1]                                        ;; otherwise, if the minimum happens to be negative, 1 is reported
  ]
end

to-report poisson-dist [#mu]                          ;; reports value from a poisson distribution with mean #mu
  report round (random-poisson #mu)
end

to-report rev-poisson [#maxv #meanr]                  ;; reports value from a sort of "reverse" poisson distribution
  let x (round (random-poisson #meanr))               ;; where #maxv is the maximum value that we want the value to be
  report (#maxv - x)                                  ;; and x is derived from a poisson dist with mean #meanr ("mean reduction")
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

to-report actual-p-death [#age]                       ;; returns probability of death adjusted for age range
  let p 0
  if #age = "0-29" [
    set p (p-death * 0.6) / (100 - p-death + (p-death * 0.6)) * 100
  ]
  if #age = "30-59" [
    set p p-death
  ]
  if #age = "60+" [
    set p (p-death * 5.1) / (100 - p-death + (p-death * 5.1)) * 100
  ]
  report p
end

to-report count.lockdown    ;; reporter for experiments
  report count turtles with [shape = "person-outline"]
end
@#$#@#$#@
GRAPHICS-WINDOW
513
34
1121
643
-1
-1
12.0
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
1
1
1
ticks
30.0

BUTTON
1162
307
1232
342
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
1252
305
1323
341
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
1

SLIDER
14
40
228
73
p-infect-init
p-infect-init
0
100
30.0
1
1
%
HORIZONTAL

SLIDER
1202
408
1395
441
initial-inf
initial-inf
0
100
10.0
1
1
NIL
HORIZONTAL

PLOT
19
447
462
747
Simulation populations
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"susceptible" 1.0 0 -10899396 true "" "plot count susceptibles"
"latents" 1.0 0 -1184463 true "" "plot count latents"
"infected" 1.0 0 -2674135 true "" "plot count symptomatics"
"recovereds" 1.0 0 -3026479 true "" "plot count recovereds"
"dead" 1.0 0 -16777216 true "" "plot count deads"
"lockdown" 1.0 0 -11221820 true "" "plot count turtles with [shape = \"person-outline\"]"
"asymptomatics" 1.0 0 -8630108 true "" "plot count asymptomatics"

SLIDER
14
115
186
148
z-contact-min
z-contact-min
0
71
2.0
1
1
radius
HORIZONTAL

SLIDER
15
271
217
304
lockdown-strictness
lockdown-strictness
0
100
80.0
1
1
%
HORIZONTAL

PLOT
1143
35
1438
277
contacts
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
"contacts" 1.0 0 -16777216 true "" "plot num-contacts"

TEXTBOX
62
14
212
32
infection parameters
11
0.0
1

TEXTBOX
1251
376
1401
394
model options
11
0.0
1

BUTTON
1344
305
1423
342
go once
go\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
14
78
186
111
p-death
p-death
0
100
2.5
1.0
1
%
HORIZONTAL

SLIDER
303
56
475
89
incubation-mean
incubation-mean
0
10
1.6
0.1
1
NIL
HORIZONTAL

SLIDER
302
98
474
131
incubation-stdev
incubation-stdev
0
10
0.4
0.1
1
NIL
HORIZONTAL

SLIDER
301
144
473
177
recovery-mean
recovery-mean
0
50
20.5
0.1
1
NIL
HORIZONTAL

SLIDER
300
184
472
217
recovery-stdev
recovery-stdev
0
10
6.7
0.1
1
NIL
HORIZONTAL

SLIDER
1202
446
1374
479
duration
duration
0
10
3.0
1
1
years
HORIZONTAL

SLIDER
300
304
472
337
immunity-mean
immunity-mean
0
365 * 3
365.0
1
1
days
HORIZONTAL

TEXTBOX
358
33
508
51
countdowns
11
0.0
1

SLIDER
15
228
240
261
lockdown-threshold
lockdown-threshold
0
10000
100.0
100
1
infecteds
HORIZONTAL

SWITCH
1203
486
1365
519
imposed-lockdown?
imposed-lockdown?
0
1
-1000

TEXTBOX
53
204
203
222
lockdown parameters
11
0.0
1

SWITCH
1204
523
1350
556
modify-p-infect?
modify-p-infect?
0
1
-1000

SLIDER
14
311
222
344
protection-strength
protection-strength
0
100
60.0
1
1
%
HORIZONTAL

SWITCH
1204
562
1344
595
closed-system?
closed-system?
1
1
-1000

SLIDER
15
352
199
385
travel-strictness
travel-strictness
0
100
40.0
1
1
%
HORIZONTAL

MONITOR
605
658
671
703
latents
count latents
0
1
11

MONITOR
680
657
768
702
symptomatics
count symptomatics
0
1
11

MONITOR
513
658
593
703
susceptibles
count susceptibles
0
1
11

MONITOR
775
656
850
701
recovereds
count recovereds
0
1
11

MONITOR
857
657
914
702
deads
count deads
0
1
11

MONITOR
930
657
1023
702
% in lockdown
(count turtles with [shape = \"person-outline\"]) / \ncount turtles with [not member? self deads] \n* 100
0
1
11

MONITOR
1115
657
1177
702
% 30-59
count turtles with [age = \"30-59\"] /\ncount turtles * 100
1
1
11

MONITOR
1191
657
1248
702
% 60+
count turtles with [age = \"60+\"] /\ncount turtles * 100
1
1
11

MONITOR
1040
658
1097
703
% 0-29
count turtles with [age = \"0-29\"] /\ncount turtles * 100
1
1
11

MONITOR
1261
658
1361
703
superspreaders
count turtles with [z-contact-init = (max [z-contact-init] of turtles)]
0
1
11

SLIDER
300
225
472
258
death-mean
death-mean
0
50
16.0
1.0
1
NIL
HORIZONTAL

SLIDER
300
265
472
298
death-stdev
death-stdev
0
10
8.21
1.0
1
NIL
HORIZONTAL

SLIDER
14
154
214
187
asym-infections
asym-infections
0
100
60.0
1.0
1
%
HORIZONTAL

SLIDER
299
343
480
376
iso-countdown-max
iso-countdown-max
0
50
14.0
1
1
days
HORIZONTAL

SWITCH
1204
600
1381
633
isolate-symptomatics?
isolate-symptomatics?
0
1
-1000

SLIDER
15
393
187
426
isolation-strictness
isolation-strictness
0
100
50.0
1
1
%
HORIZONTAL

MONITOR
681
709
770
754
asymptomatic
count asymptomatics
0
1
11

SLIDER
297
381
483
414
mean-iso-reduction
mean-iso-reduction
0
10
3.0
1
1
days
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

## HOW TO USE IT
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
11
Polygon -16777216 true false 195 75 255 150 225 195 150 105
Polygon -16777216 true false 105 75 45 150 75 195 150 105
Polygon -16777216 true false 105 75 105 195 75 285 105 315 135 300 150 255 165 300 195 315 225 285 195 195 195 75
Circle -16777216 true false 103 -2 92
Circle -8630108 true true 110 5 80
Polygon -8630108 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -8630108 true true 127 79 172 94
Polygon -8630108 true true 105 90 60 150 75 180 135 105
Polygon -8630108 true true 195 90 240 150 225 180 165 105

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

transmitter
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105
Line -955883 false 210 30 285 15
Line -955883 false 210 60 285 75
Line -955883 false 90 30 15 15
Line -955883 false 90 60 15 75

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
  <experiment name="test0" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count susceptibles</metric>
    <metric>count latents</metric>
    <metric>count symptomatics</metric>
    <metric>count asymptomatics</metric>
    <metric>count recovereds</metric>
    <metric>count deads</metric>
    <metric>count.lockdown</metric>
    <metric>num-contacts</metric>
    <enumeratedValueSet variable="initial-inf">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="incubation-mean">
      <value value="1.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imposed-lockdown?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-stdev">
      <value value="8.21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="closed-system?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-stdev">
      <value value="6.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolate-symptomatics?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="immunity-mean">
      <value value="365"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asym-infections">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-p-infect?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="iso-countdown-max">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-infect-init">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protection-strength">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="travel-strictness">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lockdown-threshold">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-strictness">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="incubation-stdev">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-mean">
      <value value="20.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-death">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-mean">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-iso-reduction">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-contact-min">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lockdown-strictness">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-pxcor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-pycor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pxcor">
      <value value="49"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pycor">
      <value value="49"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test1" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count susceptibles</metric>
    <metric>count latents</metric>
    <metric>count symptomatics</metric>
    <metric>count asymptomatics</metric>
    <metric>count recovereds</metric>
    <metric>count deads</metric>
    <metric>count.lockdown</metric>
    <metric>num-contacts</metric>
    <enumeratedValueSet variable="initial-inf">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="incubation-mean">
      <value value="1.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imposed-lockdown?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-stdev">
      <value value="8.21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="closed-system?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-stdev">
      <value value="6.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolate-symptomatics?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="immunity-mean">
      <value value="365"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asym-infections">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-p-infect?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="iso-countdown-max">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-infect-init">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protection-strength">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="travel-strictness">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lockdown-threshold">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-strictness">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="incubation-stdev">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-mean">
      <value value="20.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-death">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-mean">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-iso-reduction">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-contact-min">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lockdown-strictness">
      <value value="80"/>
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
  <experiment name="test2" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count susceptibles</metric>
    <metric>count latents</metric>
    <metric>count symptomatics</metric>
    <metric>count asymptomatics</metric>
    <metric>count recovereds</metric>
    <metric>count deads</metric>
    <metric>count.lockdown</metric>
    <metric>num-contacts</metric>
    <enumeratedValueSet variable="initial-inf">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="incubation-mean">
      <value value="1.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imposed-lockdown?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-stdev">
      <value value="8.21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="closed-system?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-stdev">
      <value value="6.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolate-symptomatics?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="immunity-mean">
      <value value="365"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asym-infections">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-p-infect?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="iso-countdown-max">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-infect-init">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protection-strength">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="travel-strictness">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lockdown-threshold">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="isolation-strictness">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="incubation-stdev">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-mean">
      <value value="20.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-death">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-mean">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-iso-reduction">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-contact-min">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lockdown-strictness">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-pxcor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-pycor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pxcor">
      <value value="499"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pycor">
      <value value="499"/>
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
0
@#$#@#$#@
